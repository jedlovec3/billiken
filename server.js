import { Hono } from "hono";
import { execFile } from "child_process";
import { promises as fs } from "fs";
import { join } from "path";
import { readdirSync, statSync } from "fs";

const app = new Hono();
const PORT = process.env.PORT || 3000;

// CORS middleware — allow Lovable frontend and local dev to call the API
app.use("*", async (c, next) => {
  await next();
  c.header("Access-Control-Allow-Origin", "*");
  c.header("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
  c.header("Access-Control-Allow-Headers", "Content-Type");
});
app.options("*", (c) => c.text("", 204));

let packagesReady = true;

// Mutex to prevent concurrent R script execution
let rScriptRunning = false;
let rScriptStartedAt = null;
let lastSimulationResult = null; // { status, completedAt, error? }

// Ensure required directories exist
async function ensureDirectories() {
  const dirs = ["data/raw", "data/processed", "data/compare_picks", "output"];
  for (const dir of dirs) {
    try {
      await fs.mkdir(dir, { recursive: true });
    } catch (e) {
      // Directory may already exist
    }
  }
}

// Run R script with args (non-blocking)
function runRScript(scriptPath, args = []) {
  return new Promise((resolve, reject) => {
    execFile("Rscript", [scriptPath, ...args], {
      cwd: process.cwd(),
      timeout: 1800000, // 30 minutes
      maxBuffer: 10 * 1024 * 1024, // 10 MB
      env: { ...process.env, OPENBLAS_NUM_THREADS: "1" },
    }, (error, stdout, stderr) => {
      if (error) {
        reject(
          new Error(
            `R script failed:\n${error.message}\n\nSTDOUT:\n${stdout}\n\nSTDERR:\n${stderr}`
          )
        );
      } else {
        resolve(stdout);
      }
    });
  });
}

// Find latest file in directory
function getLatestFile(dirPath) {
  try {
    const files = readdirSync(dirPath)
      .map((f) => ({
        name: f,
        path: join(dirPath, f),
        time: statSync(join(dirPath, f)).mtimeMs,
      }))
      .sort((a, b) => b.time - a.time);

    return files.length > 0 ? files[0].path : null;
  } catch (e) {
    return null;
  }
}

// Parse CSV text into an array of objects (header row → keys).
//
// Type coercion rules:
//   * Empty cell (or literal NA / NaN)            → null
//   * Numeric-looking cell                         → number
//   * Anything else                                → string
//
// Without the empty-string guard, `Number("") === 0` would silently
// rewrite blank string cells (e.g. an empty `shed_names` or a missing
// `age_2026`) into the integer 0, which breaks any frontend that does
// `row.shed_names.split("|")` or treats `age_2026` as a real age.
function csvToJson(csv) {
  const lines = csv.trim().split("\n");
  if (lines.length < 2) return [];
  const headers = lines[0].split(",").map((h) => h.trim().replace(/^"|"$/g, ""));
  return lines.slice(1).map((line) => {
    const vals = line.split(",").map((v) => v.trim().replace(/^"|"$/g, ""));
    const obj = {};
    headers.forEach((h, i) => {
      const raw = vals[i];
      if (raw === undefined || raw === "" || raw === "NA" || raw === "NaN") {
        obj[h] = null;
      } else {
        const num = Number(raw);
        obj[h] = Number.isNaN(num) ? raw : num;
      }
    });
    return obj;
  });
}


// POST /run_projections
app.post("/run_projections", async (c) => {
  if (!packagesReady) {
    return c.json(
      { error: "R packages still installing, please retry" },
      { status: 503 }
    );
  }

  if (rScriptRunning) {
    return c.json(
      { error: "An R script is already running", startedAt: rScriptStartedAt },
      { status: 409 }
    );
  }

  rScriptRunning = true;
  rScriptStartedAt = new Date().toISOString();

  try {
    await ensureDirectories();

    console.log("Running prefreeze_update.R...");
    await runRScript("scripts/prefreeze_update.R");

    return c.json({ status: "projections_updated" });

  } catch (error) {

    console.error("Projection update error:", error);

    return c.json(
      { error: error.message, status: "failed" },
      { status: 500 }
    );
  } finally {
    rScriptRunning = false;
    rScriptStartedAt = null;
  }
});

// POST /run_pick_sim
app.post("/run_pick_sim", async (c) => {
  if (rScriptRunning) {
    return c.json(
      { error: "An R script is already running", startedAt: rScriptStartedAt },
      { status: 409 }
    );
  }

  rScriptRunning = true;
  rScriptStartedAt = new Date().toISOString();

  try {
    const body = await c.req.json();

    const compareArgs = ["--n_sims=100"];

    if (body.players && Array.isArray(body.players)) {
      compareArgs.push(`--players="${body.players.join(",")}"`);
    }

    if (body.team) {
      compareArgs.push(`--team="${body.team}"`);
    }

    if (body.round) {
      compareArgs.push(`--round=${body.round}`);
    }

    if (body.pick) {
      compareArgs.push(`--pick=${body.pick}`);
    }

    console.log("Running compare_draft_picks.R...");

    await runRScript("scripts/compare_draft_picks.R", compareArgs);

    return c.json({ status: "simulation_complete" });

  } catch (error) {

    console.error("Simulation error:", error);

    return c.json(
      { error: error.message },
      { status: 500 }
    );
  } finally {
    rScriptRunning = false;
    rScriptStartedAt = null;
  }
});

// POST /run_simulation
// Kicks off update_draft.R + compare_draft_picks.R in the background and
// returns immediately so callers (n8n, Lovable) don't time out.
app.post("/run_simulation", async (c) => {
  if (!packagesReady) {
    return c.json(
      { error: "R packages still installing, please retry in a moment" },
      { status: 503 }
    );
  }

  if (rScriptRunning) {
    return c.json(
      { error: "An R script is already running", startedAt: rScriptStartedAt },
      { status: 409 }
    );
  }

  rScriptRunning = true;
  rScriptStartedAt = new Date().toISOString();
  lastSimulationResult = null;

  // Fire-and-forget: run in background
  (async () => {
    try {
      await ensureDirectories();

      console.log("Running update_draft.R (pull draft sheet)...");
      await runRScript("scripts/update_draft.R");

      console.log("Running compare_draft_picks.R --n_sims=20...");
      await runRScript("scripts/compare_draft_picks.R", ["--n_sims=20"]);

      lastSimulationResult = { status: "complete", completedAt: new Date().toISOString() };
      console.log("Simulation complete.");
    } catch (error) {
      console.error("Simulation error:", error);
      lastSimulationResult = { status: "failed", error: error.message, completedAt: new Date().toISOString() };
    } finally {
      rScriptRunning = false;
      rScriptStartedAt = null;
    }
  })();

  // Return immediately
  return c.json({ status: "running", startedAt: rScriptStartedAt });
});

// GET /simulation_status
app.get("/simulation_status", (c) => {
  if (rScriptRunning) {
    return c.json({ status: "running", startedAt: rScriptStartedAt });
  }
  if (lastSimulationResult) {
    return c.json(lastSimulationResult);
  }
  return c.json({ status: "idle" });
});

// GET /projections
app.get("/projections", async (c) => {
  try {
    const filePath = join("data", "processed", "projections_prefreeze.csv");
    const content = await fs.readFile(filePath, "utf-8");

    c.header("Content-Type", "text/csv");
    c.header(
      "Content-Disposition",
      "attachment; filename=projections_prefreeze.csv"
    );
    return c.text(content);
  } catch (error) {
    console.error("Projections error:", error);
    return c.json(
      { error: "Projections file not found" },
      { status: 404 }
    );
  }
});

// GET /draft_results
app.get("/draft_results", async (c) => {
  try {
    const filePath = join("data", "raw", "draft_latest.csv");
    const content = await fs.readFile(filePath, "utf-8");

    c.header("Content-Type", "text/csv");
    c.header("Content-Disposition", "attachment; filename=draft_latest.csv");
    return c.text(content);
  } catch (error) {
    console.error("Draft results error:", error);
    return c.json(
      { error: "Draft results file not found" },
      { status: 404 }
    );
  }
});

// GET /pick_comparisons — returns latest comparison as JSON (for Lovable)
app.get("/pick_comparisons", async (c) => {
  try {
    const latestFile = getLatestFile("data/compare_picks");

    if (!latestFile) {
      return c.json(
        { error: "No comparison files found" },
        { status: 404 }
      );
    }

    const content = await fs.readFile(latestFile, "utf-8");
    const rows = csvToJson(content);
    const fileName = latestFile.split("/").pop();

    return c.json({ file: fileName, results: rows });
  } catch (error) {
    console.error("Pick comparisons error:", error);
    return c.json(
      { error: error.message },
      { status: 500 }
    );
  }
});

// ============================================================
// In-season endpoints
// ============================================================

// POST /run_inseason_update — trigger daily in-season pipeline
app.post("/run_inseason_update", async (c) => {
  if (!packagesReady) {
    return c.json(
      { error: "R packages still installing, please retry" },
      { status: 503 }
    );
  }

  if (rScriptRunning) {
    return c.json(
      { error: "An R script is already running", startedAt: rScriptStartedAt },
      { status: 409 }
    );
  }

  rScriptRunning = true;
  rScriptStartedAt = new Date().toISOString();

  try {
    await ensureDirectories();
    console.log("Running inseason_update.R...");
    await runRScript("scripts/inseason_update.R");
    return c.json({ status: "inseason_update_complete" });
  } catch (error) {
    console.error("In-season update error:", error);
    return c.json(
      { error: error.message, status: "failed" },
      { status: 500 }
    );
  } finally {
    rScriptRunning = false;
    rScriptStartedAt = null;
  }
});

// GET /inseason_standings — projected end-of-season standings as JSON
// Query params:
//   view         "all" | "active" | "prorated" (default "prorated")
//                  - all:      every rostered player at full ROS
//                  - active:   active-slot players only (no bench/IL/minors)
//                  - prorated: full ROS for stashed players, fill-in stats
//                              scaled by (1 - f) where f = stashed player's
//                              expected playing-time fraction
//   active_only  "true" → backwards-compatible alias for view=active.
function resolveStandingsView(c) {
  const rawView = String(c.req.query("view") || "").toLowerCase();
  const legacyActive = String(c.req.query("active_only") || "")
    .toLowerCase() === "true";

  let view;
  if (rawView === "all" || rawView === "all_rostered") {
    view = "all";
  } else if (rawView === "active" || rawView === "active_only") {
    view = "active";
  } else if (rawView === "prorated") {
    view = "prorated";
  } else if (legacyActive) {
    view = "active";
  } else {
    view = "prorated"; // new default
  }

  const fileMap = {
    all:      "inseason_projected_standings.csv",
    active:   "inseason_projected_standings_active.csv",
    prorated: "inseason_projected_standings_prorated.csv",
  };
  const labelMap = {
    all:      "all_rostered",
    active:   "active_only",
    prorated: "prorated",
  };
  return { view, fileName: fileMap[view], label: labelMap[view] };
}

app.get("/inseason_standings", async (c) => {
  const { view, fileName, label } = resolveStandingsView(c);

  try {
    const filePath = join("data", "processed", fileName);
    const content = await fs.readFile(filePath, "utf-8");
    const rows = csvToJson(content);

    // Also read status file for metadata
    let status = null;
    try {
      const statusContent = await fs.readFile(
        join("data", "processed", "inseason_status.json"),
        "utf-8"
      );
      status = JSON.parse(statusContent);
    } catch (_) {
      // Status file may not exist yet
    }

    return c.json({
      standings: rows,
      view: label,
      status,
    });
  } catch (error) {
    // If no standings file yet, return status only
    try {
      const statusContent = await fs.readFile(
        join("data", "processed", "inseason_status.json"),
        "utf-8"
      );
      return c.json(
        { error: "No standings data yet", status: JSON.parse(statusContent) },
        { status: 404 }
      );
    } catch (_) {
      return c.json(
        { error: "No in-season data available. Run /run_inseason_update first." },
        { status: 404 }
      );
    }
  }
});

// GET /inseason_team/:team — player-level ROS projections for a team
app.get("/inseason_team/:team", async (c) => {
  try {
    const teamParam = decodeURIComponent(c.req.param("team")).toLowerCase();
    const filePath = join("data", "processed", "inseason_team_details.csv");
    const content = await fs.readFile(filePath, "utf-8");
    const allRows = csvToJson(content);

    const teamRows = allRows.filter(
      (r) => r.team_name && r.team_name.toLowerCase().includes(teamParam)
    );

    if (teamRows.length === 0) {
      return c.json(
        { error: `No data for team matching '${teamParam}'` },
        { status: 404 }
      );
    }

    return c.json({ team: teamRows[0].team_name, players: teamRows });
  } catch (error) {
    return c.json(
      { error: "Team details not available. Run /run_inseason_update first." },
      { status: 404 }
    );
  }
});

// GET /inseason_free_agents — ranked free agents (players not on any roster)
// Query params:
//   type     hitter|pitcher|all (default all)
//   position optional eligibility filter (e.g. "C", "1B", "OF", "SP", "RP")
//   limit    max rows to return (default 50, 0 = no limit)
app.get("/inseason_free_agents", async (c) => {
  try {
    const filePath = join(
      "data", "processed", "inseason_free_agents.csv"
    );
    const content = await fs.readFile(filePath, "utf-8");
    const rows = csvToJson(content);

    const type = (c.req.query("type") || "all").toLowerCase();
    const position = c.req.query("position");
    const limitRaw = c.req.query("limit");
    const limit = limitRaw === undefined ? 50 : Number(limitRaw);

    let filtered = rows;
    if (type === "hitter" || type === "pitcher") {
      filtered = filtered.filter((r) => r.player_type === type);
    }
    if (position) {
      const needle = String(position).toUpperCase();
      filtered = filtered.filter((r) => {
        if (!r.positions) return false;
        const tokens = String(r.positions)
          .toUpperCase()
          .split("|")
          .map((s) => s.trim());
        return tokens.includes(needle);
      });
    }

    // Preserve the precomputed ranks from the CSV. Rewrite rank_by_type when
    // the user filters by position so the dashboard displays 1..N within the
    // visible list.
    filtered = filtered.map((r, i) => ({ ...r, rank_filtered: i + 1 }));

    if (Number.isFinite(limit) && limit > 0) {
      filtered = filtered.slice(0, limit);
    }

    // Attach status metadata (same pattern as /inseason_standings)
    let status = null;
    try {
      const statusContent = await fs.readFile(
        join("data", "processed", "inseason_status.json"),
        "utf-8"
      );
      status = JSON.parse(statusContent);
    } catch (_) {
      // status is optional
    }

    return c.json({ free_agents: filtered, count: filtered.length, status });
  } catch (error) {
    try {
      const statusContent = await fs.readFile(
        join("data", "processed", "inseason_status.json"),
        "utf-8"
      );
      return c.json(
        { error: "No free-agent data yet", status: JSON.parse(statusContent) },
        { status: 404 }
      );
    } catch (_) {
      return c.json(
        {
          error:
            "No in-season free-agent data available. Run /run_inseason_update first.",
        },
        { status: 404 }
      );
    }
  }
});

// GET /inseason_free_agents/:player — single player lookup
app.get("/inseason_free_agents/:player", async (c) => {
  try {
    const playerParam = decodeURIComponent(c.req.param("player")).toLowerCase();
    const filePath = join(
      "data", "processed", "inseason_free_agents.csv"
    );
    const content = await fs.readFile(filePath, "utf-8");
    const rows = csvToJson(content);

    const match = rows.find(
      (r) => r.Name && String(r.Name).toLowerCase() === playerParam
    );
    if (!match) {
      return c.json(
        { error: `No free-agent match for '${playerParam}'` },
        { status: 404 }
      );
    }
    return c.json({ player: match });
  } catch (error) {
    return c.json(
      {
        error:
          "Free-agent data not available. Run /run_inseason_update first.",
      },
      { status: 404 }
    );
  }
});

// GET /inseason_pt_benchmarks — league-wide playing-time benchmarks used
// by the prorated standings view. Returns the raw CSV rows plus a
// convenience `hitters` map and `pitchers` map.
app.get("/inseason_pt_benchmarks", async (c) => {
  try {
    const filePath = join(
      "data", "processed", "inseason_pt_benchmarks.csv"
    );
    const content = await fs.readFile(filePath, "utf-8");
    const rows = csvToJson(content);

    const hitters = {};
    const pitchers = {};
    for (const r of rows) {
      if (r.role === "hitter") hitters[r.position] = r.benchmark;
      else if (r.role === "pitcher") pitchers[r.position] = r.benchmark;
    }
    return c.json({ rows, hitters, pitchers });
  } catch (error) {
    return c.json(
      {
        error:
          "Benchmarks not available. Run /run_inseason_update first.",
      },
      { status: 404 }
    );
  }
});

// GET /inseason_status — pipeline health check
app.get("/inseason_status", async (c) => {
  try {
    const content = await fs.readFile(
      join("data", "processed", "inseason_status.json"),
      "utf-8"
    );
    return c.json(JSON.parse(content));
  } catch (_) {
    return c.json({ status: "never_run" });
  }
});

// ============================================================
// Trade-analysis endpoints (Phase 1 + Phase 3 of trade tooling)
// ============================================================

// Read a CSV from data/processed and return parsed rows or 404.
async function readProcessedCsv(fileName) {
  const filePath = join("data", "processed", fileName);
  const content = await fs.readFile(filePath, "utf-8");
  return csvToJson(content);
}

// GET /team_assets — every rostered player across the league with contract,
// salary, value, and surplus columns. Backed by
// data/processed/team_assets.csv (built by scripts/build_team_assets.R).
app.get("/team_assets", async (c) => {
  try {
    const rows = await readProcessedCsv("team_assets.csv");
    return c.json({ team_assets: rows, count: rows.length });
  } catch (error) {
    return c.json(
      {
        error:
          "team_assets.csv not available. Run scripts/build_team_assets.R or wait for the next daily refresh.",
      },
      { status: 404 }
    );
  }
});

// GET /team_assets/:team — rows filtered to one Billiken team. Match is
// case-insensitive substring on `billikenTeam`, mirroring the
// /inseason_team/:team behavior.
app.get("/team_assets/:team", async (c) => {
  try {
    const teamParam = decodeURIComponent(c.req.param("team")).toLowerCase();
    const rows = await readProcessedCsv("team_assets.csv");
    const teamRows = rows.filter(
      (r) =>
        r.billikenTeam &&
        String(r.billikenTeam).toLowerCase().includes(teamParam)
    );

    if (teamRows.length === 0) {
      return c.json(
        { error: `No team_assets rows for team matching '${teamParam}'` },
        { status: 404 }
      );
    }

    return c.json({
      team: teamRows[0].billikenTeam,
      players: teamRows,
      count: teamRows.length,
    });
  } catch (error) {
    return c.json(
      {
        error:
          "team_assets.csv not available. Run scripts/build_team_assets.R first.",
      },
      { status: 404 }
    );
  }
});

// GET /team_posture — competitive posture (contender/bubble/mid/rebuild)
// for each of the 10 Billiken teams, plus gap-to-3rd, projected finish,
// next-year keeper cap, and short priority_buy / priority_sell labels.
// Backed by data/processed/team_posture.csv
// (built by scripts/team_posture.R).
app.get("/team_posture", async (c) => {
  try {
    const rows = await readProcessedCsv("team_posture.csv");
    return c.json({ team_posture: rows, count: rows.length });
  } catch (error) {
    return c.json(
      {
        error:
          "team_posture.csv not available. Run scripts/team_posture.R or wait for the next daily refresh.",
      },
      { status: 404 }
    );
  }
});

// GET /team_keeper_pressure — per-team keeper-cap shedding analysis.
// For each Billiken team: how many keeper-worthy players (positive
// future_value) sit past the projected next-year keeper cap, and the
// pipe-separated names of those players. Backed by
// data/processed/team_keeper_pressure.csv (built by scripts/team_posture.R
// after Phase 2 multi-year values are attached to team_assets.csv).
app.get("/team_keeper_pressure", async (c) => {
  try {
    const rows = await readProcessedCsv("team_keeper_pressure.csv");
    return c.json({ team_keeper_pressure: rows, count: rows.length });
  } catch (error) {
    return c.json(
      {
        error:
          "team_keeper_pressure.csv not available. Run scripts/team_posture.R or wait for the next daily refresh.",
      },
      { status: 404 }
    );
  }
});

// GET /draft_pick_values — next-season draft pick valuation per team and
// round. Round-1 picks 1-7 are lottery-weighted (so the expected_overall_pick
// is a probability-weighted blend); picks 8-10 are deterministic
// (prior top-3 in reverse standings). Rounds 2+ are deterministic
// reverse standings.
//
// Backed by data/processed/draft_pick_values.csv
// (built by scripts/value_draft_picks.R).
//
// Optional query param: ?team=<substring> filters to one Billiken team
// (case-insensitive substring match on billikenTeam).
app.get("/draft_pick_values", async (c) => {
  try {
    const rows = await readProcessedCsv("draft_pick_values.csv");
    const teamParam = c.req.query("team");
    if (teamParam) {
      const needle = decodeURIComponent(teamParam).toLowerCase();
      const teamRows = rows.filter(
        (r) =>
          r.billikenTeam &&
          String(r.billikenTeam).toLowerCase().includes(needle)
      );
      if (teamRows.length === 0) {
        return c.json(
          { error: `No draft_pick_values rows for team matching '${needle}'` },
          { status: 404 }
        );
      }
      return c.json({
        team: teamRows[0].billikenTeam,
        picks: teamRows,
        count: teamRows.length,
      });
    }
    return c.json({ draft_pick_values: rows, count: rows.length });
  } catch (error) {
    return c.json(
      {
        error:
          "draft_pick_values.csv not available. Run scripts/value_draft_picks.R or wait for the next daily refresh.",
      },
      { status: 404 }
    );
  }
});

// Health check
app.get("/health", (c) => {
  return c.json({ status: "ok", packagesReady });
});

// Initialize directories
await ensureDirectories();

// Start server immediately (non-blocking)
Bun.serve({
  port: PORT,
  fetch: app.fetch,
  idleTimeout: 255, // seconds — R scripts can take a few minutes
});

console.log(`Billiken API server running on port ${PORT}`);

