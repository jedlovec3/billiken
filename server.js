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

// Parse CSV text into an array of objects (header row → keys)
function csvToJson(csv) {
  const lines = csv.trim().split("\n");
  if (lines.length < 2) return [];
  const headers = lines[0].split(",").map((h) => h.trim().replace(/^"|"$/g, ""));
  return lines.slice(1).map((line) => {
    const vals = line.split(",").map((v) => v.trim().replace(/^"|"$/g, ""));
    const obj = {};
    headers.forEach((h, i) => {
      const num = Number(vals[i]);
      obj[h] = isNaN(num) ? vals[i] : num;
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
app.get("/inseason_standings", async (c) => {
  try {
    const filePath = join("data", "processed", "inseason_projected_standings.csv");
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

    return c.json({ standings: rows, status });
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
});

console.log(`Billiken API server running on port ${PORT}`);

