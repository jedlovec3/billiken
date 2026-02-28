# Billiken League Fantasy Baseball Analysis

## Overview
R Shiny web application for the Billiken League fantasy baseball draft assistant. Provides player rankings, projected standings, and draft analysis tools.

## Architecture
- **Frontend/Backend**: Single R Shiny app (`DraftAssistant/`)
  - `DraftAssistant/server.R` - Server logic, data loading, analytics
  - `DraftAssistant/ui.R` - User interface definition
- **Run script**: `run_app.R` - Launches Shiny on port 5000

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
- `data/` - Processed analysis data
- `scripts/` - Data update and analysis scripts

## Workflow
- **Start application**: `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript run_app.R`
- Port: 5000
