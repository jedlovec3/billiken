# Billiken League Fantasy Baseball Analysis

## Overview
R Shiny web applications for the Billiken League fantasy baseball analysis. Includes a draft assistant and a standalone trade scenarios dashboard.

## Architecture
- **Trade Scenarios Dashboard** (`TradeScenarios/app.R`) — currently active workflow
  - Dropdown to select a trade scenario; displays simulated delta impact on projected standings
  - Reads `data/scenarios/<name>/latest.txt` → `<timestamp>/delta_summary.csv`
  - Excludes `_baseline` folder; handles absolute Mac paths in `latest.txt` via `basename()`
- **Draft Assistant** (`DraftAssistant/`) — kept but not the active workflow
  - `DraftAssistant/server.R` - Server logic, data loading, analytics
  - `DraftAssistant/ui.R` - User interface definition
- **Run script**: `run_app.R` - Launches the active Shiny app on port 5000

## Key Dependencies
- **R 4.5** runtime
- **shiny** - Web application framework
- **tidyverse** - Data manipulation (dplyr, tidyr, stringr, ggplot2, etc.)
- **googlesheets4** - Google Sheets integration (reads draft/roster data)
- **fuzzyjoin** / **stringdist** - Fuzzy player name matching
- **DT** - Interactive data tables
- **stringi** - String processing (requires libicu74)

## Package Management
Packages are installed to `/home/runner/R/x86_64-pc-linux-gnu-library/4.5` (bypassing renv due to R version mismatch between lockfile R 4.4.3 and available R 4.5.2).

renv is disabled at startup via `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE` in the workflow command.

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

## Workflow
- **Start application**: `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript run_app.R`
- Port: 5000
