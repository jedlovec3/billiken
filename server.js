import { Hono } from 'hono';
import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';

const app = new Hono();
const PORT = process.env.PORT || 3000;

// Ensure required directories exist
const dataDir = './data';
const outputDir = './output';
[dataDir, outputDir].forEach(dir => {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
});

// Health check
app.get('/health', (c) => {
  return c.json({ status: 'ok' });
});

// POST /run_simulation
app.post('/run_simulation', async (c) => {
  try {
    const body = await c.req.json();
    
    // Save draft state
    const draftStatePath = path.join(dataDir, 'draft_state.json');
    fs.writeFileSync(draftStatePath, JSON.stringify(body, null, 2));
    
    // Execute R scripts
    try {
      execSync('Rscript scripts/prefreeze_update.R', { stdio: 'inherit' });
      execSync('Rscript scripts/compare_draft_picks.R --n_sims=200', { stdio: 'inherit' });
    } catch (error) {
      return c.json({ 
        status: 'error', 
        message: `R script execution failed: ${error.message}` 
      }, 500);
    }
    
    return c.json({ status: 'complete' });
  } catch (error) {
    return c.json({ 
      status: 'error', 
      message: error.message 
    }, 400);
  }
});

// GET /projections
app.get('/projections', (c) => {
  try {
    const filePath = path.join(process.cwd(), 'projections_prefreeze.csv');
    if (!fs.existsSync(filePath)) {
      return c.json({ error: 'File not found' }, 404);
    }
    const content = fs.readFileSync(filePath, 'utf-8');
    return c.text(content, 200, { 'Content-Type': 'text/csv' });
  } catch (error) {
    return c.json({ error: error.message }, 500);
  }
});

// GET /draft_results
app.get('/draft_results', (c) => {
  try {
    const filePath = path.join(process.cwd(), 'draft_latest.csv');
    if (!fs.existsSync(filePath)) {
      return c.json({ error: 'File not found' }, 404);
    }
    const content = fs.readFileSync(filePath, 'utf-8');
    return c.text(content, 200, { 'Content-Type': 'text/csv' });
  } catch (error) {
    return c.json({ error: error.message }, 500);
  }
});

// GET /pick_comparisons
app.get('/pick_comparisons', (c) => {
  try {
    const files = fs.readdirSync(outputDir)
      .filter(f => f.endsWith('.csv'))
      .sort()
      .reverse();
    
    if (files.length === 0) {
      return c.json({ error: 'No comparison files found' }, 404);
    }
    
    const latestFile = files[0];
    const filePath = path.join(outputDir, latestFile);
    const content = fs.readFileSync(filePath, 'utf-8');
    return c.text(content, 200, { 'Content-Type': 'text/csv' });
  } catch (error) {
    return c.json({ error: error.message }, 500);
  }
});

Bun.serve({
  port: PORT,
  fetch: app.fetch,
});

console.log(`Server running on port ${PORT}`);
