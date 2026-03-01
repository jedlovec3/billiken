# Billiken League Fantasy Baseball Analysis

## Overview
R Shiny web applications for the Billiken League fantasy baseball analysis. Includes a draft assistant and a standalone trade scenarios dashboard.

## Architecture
- **Trade Scenarios Dashboard** (`TradeScenarios/app.R`) — currently active workflow
  - Dropdown to select a trade scenario; displays simulated delta impact on projected standings
  - Reads `data/scenarios/<name>/latest.txt` → `<timestamp>/delta_summary.csv`
  - Excludes `_baseline` folder; handles absolute Mac paths in `latest.txt` via `basename()`
  - Uses only base R + shiny + DT (no dplyr/readr; avoids heavy compilation dependencies)
- **Draft Assistant** (`DraftAssistant/`) — kept but not the active workflow
  - `DraftAssistant/server.R` - Server logic, data loading, analytics
  - `DraftAssistant/ui.R` - User interface definition
- **Run script**: `run_app.R` - Launches the active Shiny app on port 5000

## Key Dependencies (TradeScenarios)
- **R 4.5** runtime
- **shiny** - Web application framework (no C/C++ code itself)
- **DT** - Interactive data tables
- **httpuv** - HTTP server for shiny (requires compilation with zlib headers)
- Base R only for data loading (`read.csv`) and manipulation

## Package Management
**IMPORTANT**: Packages are stored in `r-packages/` inside the project directory (persists across Replit environment resets). Do NOT use `/home/runner/R/...` — that path is wiped on environment reset.

### First-time or after package loss
1. `run_app.R` auto-detects missing packages and installs them on startup
2. For compiled packages needing system libs (e.g. `httpuv` needs zlib): the Makevars file at `r-packages/.Makevars` is auto-written with zlib compile flags via `pkg-config`
3. If install hangs or produces empty directories (from killed installs), use `R CMD INSTALL --no-byte-compile --no-staged-install` for each package:
   ```bash
   rm -rf r-packages/00LOCK-*
   curl -sfL -o /tmp/pkg.tar.gz 'https://packagemanager.posit.co/cran/__linux__/noble/latest/src/contrib/<pkg>_<ver>.tar.gz'
   RENV_CONFIG_AUTOLOADER_ENABLED=FALSE R CMD INSTALL --no-byte-compile --no-staged-install --library=r-packages /tmp/pkg.tar.gz
   ```
4. Clean stale locks before any install: `rm -rf r-packages/00LOCK-*`
5. Verify install: `ls r-packages/<pkgname>/DESCRIPTION`

renv is disabled via `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE` (R 4.5.2 vs lockfile R 4.4.3 mismatch).

## Configuration
- **BILLIKEN_SHEET_ID** (secret) - Google Sheets ID for draft data
- **BILLIKEN_TAB_FROZEN_ROSTERS** (optional, default: "FrozenRosters")
- **BILLIKEN_TAB_DRAFT** (optional, default: "Draft")
- **BILLIKEN_TAB_SALARIES** (optional, default: "Salaries")
- **BILLIKEN_TAB_POSITIONS** (optional, default: "Positions")
- **BILLIKEN_PROJECTIONS_YEAR** (optional, default: "2025")

## Data Files
- `DraftAssistant/hitter_projections_YYYY.csv` - FanGraphs hitter projections
- `DraftAssistant/pitcher_projections_YYYY.csv` - FanGraphs pitcher projections
- `data/scenarios/<name>/latest.txt` - Points to latest simulation run folder (absolute path; use basename())
- `data/scenarios/<name>/<timestamp>/delta_summary.csv` - Simulation results; columns: team, baseline_*, scenario_*, delta_*
- `scenarios/<name>.csv` - Trade definition CSV files
- `scripts/` - Data update and analysis scripts
- `r-packages/` - Local R library (gitignored; persists in Replit filesystem)

## Workflow
- **Start application**: `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript run_app.R`
- Port: 5000
