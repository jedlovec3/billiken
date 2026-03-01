# Billiken League Fantasy Baseball Analysis

## Overview
Web applications for the Billiken League fantasy baseball analysis. The active app is a Python Flask trade scenarios dashboard. A legacy R Shiny draft assistant also exists but is not the active workflow.

## Architecture

### Active: Trade Scenarios Dashboard (Flask)
- **Entry point**: `app.py` — Flask server, runs on port 5000
- **Template**: `templates/index.html` — Single-page app with dropdown + sortable table
- **Scenario discovery**: scans `data/scenarios/` for subfolders with `latest.txt`, excludes `_baseline`
- **Data loading**: reads `latest.txt` → `basename()` → `<timestamp>/delta_summary.csv`
- **API endpoints**:
  - `GET /` — serves the dashboard HTML
  - `GET /api/scenario/<name>` — returns delta_summary rows as JSON, sorted by Δ Pts desc
- **No external dependencies** beyond Flask (stdlib csv, os modules for data reading)

### Inactive: Draft Assistant (R Shiny)
- `DraftAssistant/server.R`, `DraftAssistant/ui.R`
- `TradeScenarios/app.R` — legacy R Shiny version of the trade scenarios dashboard (kept for reference)
- `run_app.R` — legacy R launcher (kept but not used by active workflow)
- R packages stored in `r-packages/` (project-local, gitignored)

## Workflow
- **Start application**: `python app.py`
- Port: 5000, host: 0.0.0.0
- Deployment target: vm (always-on; required for stable HTTP serving)

## Data Files
- `data/scenarios/<name>/latest.txt` — absolute Mac path; use `os.path.basename()` to extract timestamp folder
- `data/scenarios/<name>/<timestamp>/delta_summary.csv` — columns: team, baseline_avg_pts, scenario_avg_pts, delta_avg_pts, delta_avg_rank, delta_wins, delta_top_3, delta_avg_hit_pts, delta_avg_pit_pts
- `data/scenarios/_baseline/` — excluded from scenario list
- `scenarios/<name>.csv` — trade definition CSVs
- `scripts/` — R data update and analysis scripts

## Configuration / Secrets
- **BILLIKEN_SHEET_ID** (secret) — Google Sheets ID, used by DraftAssistant only (not the active Flask app)
