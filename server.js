import { Hono } from "hono";
import { execFile } from "child_process";
import { promises as fs } from "fs";
import { join } from "path";
import { readdirSync, statSync } from "fs";
import {
  defaultTradeHorizon,
  findTeamPosture,
  filterTradeTargetsByPosture,
  postureWeights,
  resolveEffectivePosture,
} from "./lib/trade_posture.js";

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

// Read the pipeline status file. Always returns a non-null object so callers
// can safely do `data.status.last_updated` without null-checking. When the
// file is missing or unparseable we return a `{status: "never_run",
// last_updated: null}` shape; the frontend can still read `last_updated`
// (it'll be `null`) without crashing.
async function readStatusJsonSafe() {
  try {
    const content = await fs.readFile(
      join("data", "processed", "inseason_status.json"),
      "utf-8"
    );
    const parsed = JSON.parse(content);
    if (parsed && typeof parsed === "object") {
      if (parsed.last_updated === undefined) parsed.last_updated = null;
      return parsed;
    }
  } catch (_) {
    // fall through to default
  }
  return { status: "never_run", last_updated: null };
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
function parseCsvRow(line) {
  const out = [];
  let cur = "";
  let inQuotes = false;

  for (let i = 0; i < line.length; i++) {
    const ch = line[i];

    if (inQuotes) {
      if (ch === "\"") {
        if (line[i + 1] === "\"") {
          cur += "\"";
          i++;
        } else {
          inQuotes = false;
        }
      } else {
        cur += ch;
      }
    } else {
      if (ch === "\"") {
        inQuotes = true;
      } else if (ch === ",") {
        out.push(cur);
        cur = "";
      } else {
        cur += ch;
      }
    }
  }

  out.push(cur);
  return out.map((v) => v.trim());
}
function csvToJson(csv) {
  const lines = csv.replace(/\r\n/g, "\n").trim().split("\n");
  if (lines.length < 2) return [];
  const headers = parseCsvRow(lines[0]);
  return lines.slice(1).map((line) => {
    const vals = parseCsvRow(line);
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

function asFiniteNumber(value) {
  if (value === null || value === undefined || value === "") return null;
  const n = typeof value === "number" ? value : Number(value);
  return Number.isFinite(n) ? n : null;
}

function resolveDashboardValue(row) {
  const explicitValue = asFiniteNumber(row.dashboard_value_2026);
  const explicitSource = row.dashboard_value_source
    ? String(row.dashboard_value_source)
    : null;
  if (explicitValue !== null) {
    return {
      value: explicitValue,
      source:
        explicitSource ||
        (asFiniteNumber(row.fg_ros_auction_dollars) !== null
          ? "fangraphs_ros_auction"
          : asFiniteNumber(row.fg_auction_dollars) !== null
          ? "fangraphs_fullseason_auction"
          : "sgpar_standings_value"),
    };
  }

  const ros = asFiniteNumber(row.fg_ros_auction_dollars);
  if (ros !== null) {
    return { value: ros, source: "fangraphs_ros_auction" };
  }

  const full = asFiniteNumber(row.fg_auction_dollars);
  if (full !== null) {
    return { value: full, source: "fangraphs_fullseason_auction" };
  }

  const model = asFiniteNumber(row.dollar_value_2026);
  if (model !== null) {
    return { value: model, source: "sgpar_standings_value" };
  }

  return { value: null, source: null };
}

function futureValueBreakdown(row) {
  return [2027, 2028, 2029, 2030].map((year) => ({
    season: year,
    projection_value: asFiniteNumber(row[`dollar_value_${year}`]),
    prospect_value: asFiniteNumber(row[`prospect_value_${year}`]) ?? 0,
    selected_source: row[`future_selected_source_${year}`] || null,
    selected_value: asFiniteNumber(row[`future_selected_value_${year}`]),
    salary: asFiniteNumber(row[`salary_${year}`]),
    net_value: asFiniteNumber(row[`future_net_${year}`]) ?? 0,
    discounted_value: asFiniteNumber(row[`future_discounted_${year}`]) ?? 0,
  }));
}

function shapeTeamAssetRow(row) {
  const resolved = resolveDashboardValue(row);
  return {
    ...row,
    dashboard_value_2026: resolved.value,
    dashboard_value_source: resolved.source,
    future_value_breakdown: futureValueBreakdown(row),
    value: resolved.value,
    value_source: resolved.source,
  };
}

// GET /team_assets — every rostered player across the league with contract,
// salary, value, and surplus columns. Backed by
// data/processed/team_assets.csv (built by scripts/build_team_assets.R).
app.get("/team_assets", async (c) => {
  try {
    const rows = (await readProcessedCsv("team_assets.csv")).map(shapeTeamAssetRow);
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
    const rows = (await readProcessedCsv("team_assets.csv")).map(shapeTeamAssetRow);
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

// GET /prospect_values — consensus prospect rankings and future values.
// Backed by data/processed/prospect_values.csv.
app.get("/prospect_values", async (c) => {
  try {
    const rows = await readProcessedCsv("prospect_values.csv");
    return c.json({ prospect_values: rows, count: rows.length });
  } catch (error) {
    return c.json(
      {
        error:
          "prospect_values.csv not available. Run scripts/build_prospect_values.R or wait for the next daily refresh.",
      },
      { status: 404 }
    );
  }
});

function weightedAssetValue(asset, weights) {
  const wn = asFiniteNumber(asset.win_now_value) ?? 0;
  const fut = asFiniteNumber(asset.future_value) ?? 0;
  return weights.w_win_now * wn + weights.w_future * fut;
}

function sortTradeTargets(trades, horizon) {
  const h = (horizon || "balanced").toLowerCase();
  const sorted = [...trades];
  if (h === "win_now") {
    sorted.sort(
      (a, b) =>
        (asFiniteNumber(b.my_win_now_delta) ?? 0) -
        (asFiniteNumber(a.my_win_now_delta) ?? 0)
    );
  } else if (h === "future") {
    sorted.sort(
      (a, b) =>
        (asFiniteNumber(b.my_future_delta) ?? 0) -
        (asFiniteNumber(a.my_future_delta) ?? 0)
    );
  } else {
    sorted.sort(
      (a, b) =>
        (asFiniteNumber(b.my_value_delta) ?? 0) -
        (asFiniteNumber(a.my_value_delta) ?? 0)
    );
  }
  return sorted;
}

// GET /trade_targets/:my_team — ranked two-team trade suggestions where
// :my_team is the side initiating the trade. Optional query params:
//   partner=<substring>    filter to trades against one partner team
//   horizon=win_now|future|balanced (default balanced)
//   stance=auto|contender|bubble|mid|rebuild (default auto)
//
// Backed by data/processed/trade_targets.csv (built by
// scripts/trade_recommendations.R).
app.get("/trade_targets/:my_team", async (c) => {
  try {
    const myTeamRaw = decodeURIComponent(c.req.param("my_team")).toLowerCase();
    const [rows, postureRows] = await Promise.all([
      readProcessedCsv("trade_targets.csv"),
      readProcessedCsv("team_posture.csv"),
    ]);

    const teamRows = rows.filter(
      (r) => r.my_team && String(r.my_team).toLowerCase().includes(myTeamRaw)
    );
    const postureRow = findTeamPosture(postureRows, myTeamRaw);
    const matchedTeam = postureRow?.billikenTeam ?? teamRows[0]?.my_team ?? null;
    const actualPosture =
      postureRow?.posture ??
      teamRows[0]?.my_actual_posture ??
      teamRows[0]?.my_posture ??
      "mid";

    let postureResolution;
    try {
      postureResolution = resolveEffectivePosture(
        actualPosture,
        c.req.query("stance")
      );
    } catch (error) {
      return c.json({ error: error.message }, { status: 400 });
    }

    let filtered = filterTradeTargetsByPosture(
      teamRows,
      postureResolution.effectivePosture
    );

    const partnerParam = c.req.query("partner");
    if (partnerParam) {
      const needle = decodeURIComponent(partnerParam).toLowerCase();
      filtered = filtered.filter(
        (r) =>
          r.partner_team &&
          String(r.partner_team).toLowerCase().includes(needle)
      );
    }

    const horizon = defaultTradeHorizon(
      postureResolution.effectivePosture,
      c.req.query("horizon")
    );

    const trades = sortTradeTargets(filtered, horizon);

    return c.json({
      my_team: matchedTeam,
      my_actual_posture: postureResolution.actualPosture,
      my_effective_posture: postureResolution.effectivePosture,
      my_override_applied: postureResolution.overrideApplied,
      my_weights: postureWeights(postureResolution.effectivePosture),
      horizon,
      trades,
      count: trades.length,
      hint:
        trades.length === 0
          ? "No automated trade matches for this filter. Try another partner or use POST /evaluate_trade for a custom offer."
          : null,
    });
  } catch (error) {
    return c.json(
      {
        error:
          "trade_targets.csv not available. Run scripts/trade_recommendations.R or wait for the next daily refresh.",
      },
      { status: 404 }
    );
  }
});

// POST /evaluate_trade — posture-weighted value for a manual two-team trade.
// Body JSON:
//   { my_team, partner_team, my_asset_ids: [], partner_asset_ids: [],
//     my_posture_override?: "auto"|"contender"|"bubble"|"mid"|"rebuild" }
// Asset ids are player Names or pick ids like pick_2027_R02.
app.post("/evaluate_trade", async (c) => {
  try {
    const body = await c.req.json();
    const myTeam = body.my_team;
    const partnerTeam = body.partner_team;
    const myIds = Array.isArray(body.my_asset_ids) ? body.my_asset_ids : [];
    const partnerIds = Array.isArray(body.partner_asset_ids)
      ? body.partner_asset_ids
      : [];

    if (!myTeam || !partnerTeam) {
      return c.json(
        { error: "my_team and partner_team are required" },
        { status: 400 }
      );
    }

    const [assets, postureRows, pickRows] = await Promise.all([
      readProcessedCsv("team_assets.csv"),
      readProcessedCsv("team_posture.csv"),
      readProcessedCsv("draft_pick_values.csv").catch(() => []),
    ]);

    const myPostureRow = postureRows.find(
      (r) =>
        r.billikenTeam &&
        String(r.billikenTeam).toLowerCase().includes(String(myTeam).toLowerCase())
    );
    const partnerPostureRow = postureRows.find(
      (r) =>
        r.billikenTeam &&
        String(r.billikenTeam)
          .toLowerCase()
          .includes(String(partnerTeam).toLowerCase())
    );

    if (!myPostureRow || !partnerPostureRow) {
      return c.json({ error: "Could not resolve team posture for both sides" }, {
        status: 400,
      });
    }

    let myPostureResolution;
    try {
      myPostureResolution = resolveEffectivePosture(
        myPostureRow.posture,
        body.my_posture_override
      );
    } catch (error) {
      return c.json({ error: error.message }, { status: 400 });
    }

    const partnerPostureResolution = resolveEffectivePosture(
      partnerPostureRow.posture
    );
    const myWeights = postureWeights(myPostureResolution.effectivePosture);
    const partnerWeights = postureWeights(partnerPostureRow.posture);

    const nextYear = new Date().getFullYear() + 1;
    const pickAssets = pickRows
      .filter((p) => Number(p.season) === nextYear)
      .map((p) => ({
        billikenTeam: p.billikenTeam,
        asset_id: `pick_${p.season}_R${String(p.round).padStart(2, "0")}`,
        Name: `pick_${p.season}_R${String(p.round).padStart(2, "0")}`,
        win_now_value: 0,
        future_value: asFiniteNumber(p.expected_dollar_value) ?? 0,
        pick_value: asFiniteNumber(p.expected_dollar_value) ?? 0,
        prospect_value: 0,
        future_projection_source: p.curve_source ?? null,
        asset_type: "pick",
      }));

    const universe = [
      ...assets.map((a) => ({
        ...a,
        asset_id: a.Name,
        asset_type:
          (asFiniteNumber(a.prospect_value) ?? 0) > 0 ? "prospect" : "player",
      })),
      ...pickAssets,
    ];

    const resolveAssets = (teamNeedle, ids) =>
      ids.map((id) => {
        const row = universe.find(
          (a) =>
            a.billikenTeam &&
            String(a.billikenTeam).toLowerCase().includes(String(teamNeedle).toLowerCase()) &&
            (a.asset_id === id || a.Name === id)
        );
        return row || null;
      });

    const myGive = resolveAssets(myTeam, myIds).filter(Boolean);
    const partnerGive = resolveAssets(partnerTeam, partnerIds).filter(Boolean);

    const sumSide = (rows, weights) => {
      let winNow = 0;
      let future = 0;
      let total = 0;
      for (const a of rows) {
        const wn = asFiniteNumber(a.win_now_value) ?? 0;
        const fut = asFiniteNumber(a.future_value) ?? 0;
        winNow += wn;
        future += fut;
        total += weightedAssetValue(a, weights);
      }
      return { win_now: winNow, future, weighted_total: total, count: rows.length };
    };

    const myGiveVals = sumSide(myGive, myWeights);
    const partnerGiveVals = sumSide(partnerGive, partnerWeights);

    const myReceiveVals = sumSide(partnerGive, myWeights);
    const partnerReceiveVals = sumSide(myGive, partnerWeights);

    const my_net =
      myReceiveVals.weighted_total - myGiveVals.weighted_total;
    const partner_net =
      partnerReceiveVals.weighted_total - partnerGiveVals.weighted_total;

    const my_win_now_net = myReceiveVals.win_now - myGiveVals.win_now;
    const my_future_net = myReceiveVals.future - myGiveVals.future;
    const partner_win_now_net =
      partnerReceiveVals.win_now - partnerGiveVals.win_now;
    const partner_future_net =
      partnerReceiveVals.future - partnerGiveVals.future;

    return c.json({
      my_team: myPostureRow.billikenTeam,
      partner_team: partnerPostureRow.billikenTeam,
      my_posture: myPostureResolution.effectivePosture,
      my_actual_posture: myPostureResolution.actualPosture,
      my_effective_posture: myPostureResolution.effectivePosture,
      my_override_applied: myPostureResolution.overrideApplied,
      partner_posture: partnerPostureResolution.effectivePosture,
      partner_actual_posture: partnerPostureResolution.actualPosture,
      partner_effective_posture: partnerPostureResolution.effectivePosture,
      my_weights: myWeights,
      partner_weights: partnerWeights,
      my_give: myGiveVals,
      my_receive: myReceiveVals,
      partner_give: partnerGiveVals,
      partner_receive: partnerReceiveVals,
      my_net,
      partner_net,
      my_win_now_net,
      my_future_net,
      partner_win_now_net,
      partner_future_net,
      guidance:
        myPostureResolution.effectivePosture === "rebuild"
          ? "Rebuild: prioritize my_future_net > 0; send expiring vets for partner_win_now_net > 0."
          : myPostureResolution.effectivePosture === "contender" ||
            myPostureResolution.effectivePosture === "bubble"
          ? "Contender: prioritize my_win_now_net > 0."
          : null,
      unresolved_my_ids: myIds.filter(
        (id) => !myGive.some((a) => a.asset_id === id || a.Name === id)
      ),
      unresolved_partner_ids: partnerIds.filter(
        (id) => !partnerGive.some((a) => a.asset_id === id || a.Name === id)
      ),
    });
  } catch (error) {
    return c.json(
      {
        error:
          error.message ||
          "evaluate_trade failed. Ensure team_assets.csv exists.",
      },
      { status: 500 }
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
