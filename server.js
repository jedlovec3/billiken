import { Hono } from "hono";
import { execSync } from "child_process";
import { promises as fs } from "fs";
import { join } from "path";
import { readdirSync, statSync } from "fs";

const app = new Hono();
const PORT = process.env.PORT || 3000;

let packagesReady = false;

// Ensure required directories exist
async function ensureDirectories() {
  const dirs = ["data", "output"];
  for (const dir of dirs) {
    try {
      await fs.mkdir(dir, { recursive: true });
    } catch (e) {
      // Directory may already exist
    }
  }
}

// Run R script with args
function runRScript(scriptPath, args = []) {
  return new Promise((resolve, reject) => {
    try {
      const cmd = ["Rscript", scriptPath, ...args].join(" ");
      const output = execSync(cmd, {
        encoding: "utf-8",
        stdio: ["pipe", "pipe", "pipe"],
        cwd: process.cwd(),
        timeout: 600000, // 10 minutes
      });
      resolve(output);
    } catch (error) {
      reject(new Error(`R script failed: ${error.message}`));
    }
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

// POST /run_simulation
app.post("/run_simulation", async (c) => {
  if (!packagesReady) {
    return c.json(
      { error: "R packages still installing, please retry in a moment" },
      { status: 503 }
    );
  }

  try {
    await ensureDirectories();

    const body = await c.req.json();

    // Save draft state
    const draftStatePath = join("data", "draft_state.json");
    await fs.writeFile(draftStatePath, JSON.stringify(body, null, 2));

    console.log("Running prefreeze_update.R...");
    await runRScript("scripts/prefreeze_update.R");

    console.log("Running compare_draft_picks.R...");
    const compareArgs = ["--n_sims=200"];

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

    await runRScript("scripts/compare_draft_picks.R", compareArgs);

    return c.json({ status: "complete" });
  } catch (error) {
    console.error("Simulation error:", error);
    return c.json(
      { error: error.message, status: "failed" },
      { status: 500 }
    );
  }
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

// GET /pick_comparisons
app.get("/pick_comparisons", async (c) => {
  try {
    const latestFile = getLatestFile("output");

    if (!latestFile) {
      return c.json(
        { error: "No comparison files found" },
        { status: 404 }
      );
    }

    const content = await fs.readFile(latestFile, "utf-8");
    const fileName = latestFile.split("/").pop();

    c.header("Content-Type", "text/csv");
    c.header("Content-Disposition", `attachment; filename=${fileName}`);
    return c.text(content);
  } catch (error) {
    console.error("Pick comparisons error:", error);
    return c.json(
      { error: error.message },
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
});

console.log(`Billiken API server running on port ${PORT}`);

// Install packages in background (non-blocking)
installPackagesAsync();
