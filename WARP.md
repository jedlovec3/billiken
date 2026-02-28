# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Project overview

Billiken is an R/RStudio project for Billiken League fantasy baseball analysis. The repository is organized around:

- A Shiny-based draft assistant app in `DraftAssistant/` (primary interactive tool)
- R Markdown analysis notebooks in the repo root (e.g., `Pre_Freeze_Rankings_2025.Rmd`, `Draft_Rankings_2025.Rmd`, `InSeason_Rankings_2025.Rmd`, `Simulate_Draft_2025.Rmd`)
- Generated HTML notebook artifacts (`*.nb.html`) and CSV data exports (projections, rosters, standings)

The RStudio project file `billiken.Rproj` anchors the working directory at the repo root.

## Running the Shiny Draft Assistant

The main interactive entry point is the Shiny app under `DraftAssistant/`, which uses `ui.R` and `server.R`.

### Start the app from an R session

From the project root (e.g., by opening `billiken.Rproj` in RStudio or setting the working directory to the repo root), run:

```r path=null start=null
shiny::runApp("DraftAssistant")
```

This will launch the "Billiken League Draft Assistant" Shiny application, which exposes:

- Filters for team (`input$team`) and position (`input$pos`)
- A `players` table (from the `par` data frame) showing projected value and surplus value
- A `projected_standings` table with team-level category points

### Start the app from the shell

From the shell in the repo root, you can start the app without opening RStudio:

```sh path=null start=null
R -q -e "shiny::runApp('DraftAssistant')"
```

This uses the same `ui.R`/`server.R` pair under `DraftAssistant/`.

### Shiny app dependencies and assumptions

Key R packages used by the app (via `DraftAssistant/server.R` and `DraftAssistant/ui.R`) include:

- `shiny`
- `tidyverse` (dplyr, readr, ggplot2, etc.)
- `googlesheets4`
- `fuzzyjoin`
- `DT`
- `stringi` (used via `stringi::stri_trans_general` in notebooks; the app expects ASCII-normalized names consistent with the notebooks)

The app assumes that:

- Projection CSVs such as `hitter_projections_2025.csv` and `pitcher_projections_2025.csv` exist in the repo root and match the FanGraphs Depth Charts schema used in the notebooks.
- The Billiken League Google Sheet referenced in `server.R` is reachable anonymously; the app calls `gs4_deauth()` and then `googlesheets4::read_sheet()` against multiple tabs (rosters, draft, salaries, positions).
- Column names and shapes of both the Google Sheets and the projection CSVs remain consistent with what `server.R` expects (e.g., player name columns, team codes, and stat columns like `HR`, `R`, `RBI`, `SB`, `AVG`, `IP`, `W`, `SV`, `SO`, `ERA`, `WHIP`).

If you change any of these data sources, you will likely need to update both `DraftAssistant/server.R` and the R Markdown notebooks that share the same logic.

## R Markdown analysis notebooks

The root-level `.Rmd` files implement season-specific analyses for different phases of the league year. They all follow the same broad pattern:

1. Load libraries (`tidyverse`, `googlesheets4`, `fuzzyjoin`, and related packages)
2. Pull league data from the shared Billiken Google Sheet (rosters, draft history, salaries, positions)
3. Load local FanGraphs Depth Charts projections from CSVs in the repo root
4. Clean and normalize player names (e.g., using `stringi::stri_trans_general` and various `gsub` operations)
5. Join projections to league rosters via `fuzzyjoin::stringdist_left_join` to tolerate small name mismatches
6. Compute team totals by category (batting and pitching)
7. Rank teams within each category and convert those ranks into roto points
8. Fit models (GLMs and linear models) that map underlying stats to roto points
9. Use those models to derive per-player `point_value`, replacement levels by position, and surplus value

Examples include:

- `Pre_Freeze_Rankings_2025.Rmd`: pre-freeze roster evaluation and projected standings
- `Draft_Rankings_2025.Rmd`: draft-centric player and team valuations
- `InSeason_Rankings_2025.Rmd`: in-season standings and updated projections
- `Simulate_Draft_2025.Rmd`: draft simulation and impact analysis

### Rendering notebooks

The notebooks are configured with `output: html_notebook` and are typically run interactively in RStudio. From the command line, you can render an individual notebook from the repo root with:

```sh path=null start=null
Rscript -e "rmarkdown::render('Pre_Freeze_Rankings_2025.Rmd')"
```

Replace `Pre_Freeze_Rankings_2025.Rmd` with any other `.Rmd` file you want to render.

## Scripts directory

The `scripts/` directory contains standalone R scripts for data fetching, analysis, and optimization. These scripts are designed to be run from the command line and produce intermediate data files in `data/raw/` and `data/processed/`.

### SGP (Standings Gained Points) calculation pipeline

The SGP pipeline calculates player values based on historical standings data and optimizes roster assignments. Run these scripts in order:

1. **`draft_day_update.R`** - Fetches current league state and projections
   ```sh
   Rscript scripts/draft_day_update.R
   ```
   - Reads Google Sheets (rosters, draft, salaries) and writes to `data/raw/*_latest.csv`
   - Downloads FanGraphs Depth Charts projections via `download_fangraphs_projections.R`
   - Fetches ESPN positional eligibility via `fetch_espn_positions.R`
   - Requires environment variables: `BILLIKEN_SHEET_ID`, `FANGRAPHS_EMAIL`, `FANGRAPHS_PASSWORD`, `ESPN_LEAGUE_ID`

2. **`fetch_espn_standings.R`** - Fetches historical ESPN standings
   ```sh
   Rscript scripts/fetch_espn_standings.R
   ```
   - Fetches last 5 seasons of standings from ESPN Fantasy API
   - Outputs: `data/raw/standings_history_latest.csv` with team stats by season
   - Requires: `ESPN_LEAGUE_ID`, optionally `ESPN_S2` and `SWID` for private leagues

3. **`standings_gained_points.R`** - Calculates category unit values
   ```sh
   Rscript scripts/standings_gained_points.R
   ```
   - Fits logit models to historical standings to derive category point values
   - Calculates marginal unit values for each roto category (R, HR, RBI, SB, AVG, W, SV, SO, ERA, WHIP)
   - Outputs: `data/processed/category_unit_values.csv`, `category_value_scaling.csv`

4. **`calculate_player_sgp.R`** - Calculates player-level SGP
   ```sh
   Rscript scripts/calculate_player_sgp.R
   ```
   - Joins FanGraphs projections with ESPN positions
   - Calculates SGP for each player using category unit values
   - Handles two-way players (e.g., Shohei Ohtani)
   - Outputs: `data/processed/player_sgp.csv`
   - Note: Name normalization removes Jr./Sr. suffixes to match variations (e.g., "Luis Robert Jr." = "Luis Robert"), but takes first match when multiple players share a normalized name (e.g., Luis Garcia vs Luis Garcia Jr. on different teams)

5. **`optimize_rosters_sgp.R`** - Optimizes roster assignments
   ```sh
   Rscript scripts/optimize_rosters_sgp.R
   ```
   - Uses integer linear programming (lpSolve) to optimize 230 roster slots across 10 teams
   - Roster structure: 2C, 1-1B, 1-2B, 1-3B, 1-SS, 5-OF, 1-MI, 1-CI, 1-Util, 9-P per team
   - Outputs: `data/processed/optimal_rosters_sgp.csv`, `replacement_levels_sgp.csv`

### Draft simulation

6. **`simulate_draft.R`** - Runs the default draft simulation (no scenarios)
   ```sh
   Rscript scripts/simulate_draft.R
   ```
   - Implementation lives in `scripts/draft_simulation_lib.R` (`run_simulations()`, `summarize_simulations()`)
   - Uses `sgpar` (standings gained points above replacement) to rank players, with configurable percentage randomness (default 10%)
   - Loads keepers from `data/raw/keepers.csv` if present; otherwise falls back to `data/processed/simulated_keepers.csv`
   - Loads draft order from `data/raw/draft_latest.csv`
   - Prints a summary to the console; does **not** write output files by default

7. **`run_trade_scenario.R`** - Compare baseline vs a hypothetical trade scenario
   ```sh
   Rscript scripts/run_trade_scenario.R \
     --trades=scenarios/my_trade.csv \
     --scenario=my_trade \
     --n_sims=200 \
     --randomness=0.10 \
     --seed=42
   ```
   - Writes outputs under `data/scenarios/<scenario>/<timestamp>/`:
     - `scenario/standings_all.csv`, `scenario/standings_summary.csv`
     - `delta_summary.csv` (scenario minus baseline)
     - `baseline_path.txt` (path to the baseline used)
   - Baseline caching:
     - By default, baselines are cached under `data/scenarios/_baseline/<baseline_id>/` and reused across scenario runs.
     - If you run with `--baseline_cache=false`, then the baseline is stored alongside the scenario output instead: `data/scenarios/<scenario>/<timestamp>/baseline/`

### Other scripts

- **`download_fangraphs_projections.R`** - Downloads projections (called by `draft_day_update.R`)
- **`fetch_espn_positions.R`** - Fetches positional eligibility from ESPN API
- **`fangraphs_login.R`** - Authentication helper for FanGraphs (sourced by download script)
- **`draft_simulation_lib.R`** - Shared draft simulation functions (`run_simulations()`, `summarize_simulations()`)
- **`simulate_keepers.R`** - Simulates keeper selections (supports optional trade overlays; outputs under the provided `output_dir`)
- **`trade_utils.R`** - Helpers for reading/applying trade scenario CSVs (player moves + pick trades)
- **`paths.R`** - Optional helper for resolving project-root-relative paths
- **`prefreeze_update.R`** - Updates preseason roster data
- **`update_current_rosters.R`** - Updates current roster state

All scripts respect the `BILLIKEN_PROJECTIONS_YEAR` environment variable (defaults to current year) and filter projections to NL-only teams.

## Architecture notes

### Data flow and modeling

Across both the Shiny app (`DraftAssistant/server.R`) and the main R Markdown notebooks (e.g., `Pre_Freeze_Rankings_2025.Rmd`), the core data flow is:

1. **League state ingestion**
   - Google Sheets tabs provide rosters (`PreFreezeRosters`, `FrozenRosters`), draft picks (`Draft`), salaries (`Salaries`), and positional eligibility (`Positions`).
   - These are read via `googlesheets4::read_sheet()` with explicit `col_types` and then cleaned/normalized (e.g., title-casing team names, coercing salary/contract fields to numeric).

2. **Projection ingestion**
   - Hitter and pitcher projections are read from CSVs (e.g., `hitter_projections_2025.csv`, `pitcher_projections_2025.csv`).
   - Projections are restricted to NL (and `NA`) teams and normalized (ASCII-only names, basic filtering of non-player rows).

3. **Joining projections to league rosters**
   - `stringdist_left_join()` is used to match projection names to league rosters, allowing for minor spelling differences.
   - Joined data is grouped by `billikenTeam` to compute team-level category totals.

4. **Category points and models**
   - Teams are ranked in each roto category; those ranks are converted into percentile-style scores and then roto points.
   - GLMs (`glm(..., family = 'binomial')`) estimate smooth relationships between counting/ratio stats and percentile scores.
   - Linear models (`lm`) translate underlying stats into roto points per category and define baseline stats (e.g., baseline AVG, ERA, WHIP) for marginal impact calculations.

5. **Player-level valuation**
   - Player `point_value` is computed by applying those models at the player level.
   - Positional eligibility (C/1B/2B/3B/SS/OF/CI/MI/DH/P) is merged in from the `Positions` tab and used to define position-specific replacement levels.
   - Replacement-level players for each position are identified (e.g., the 21st catcher, 16th 1B, etc.), and each player's value above replacement (`par`) is calculated.
   - Dollar values (`projected$`) and surplus values (`surplus`) are derived from `par` and current salaries.

The Shiny app inlines a cleaned-up, production-hardened variant of this pipeline and keeps the key derived tables (`projected_players`, `projected_standings`, `par`) in memory for interactive use.

### Shiny UI/server contract

The Shiny app is split into:

- `DraftAssistant/ui.R`: defines the layout with:
  - `selectInput('team', ...)` with values for each Billiken team plus `Available`
  - `selectInput('pos', ...)` covering `All`, `Hitters`, `P`, and specific positions (C, 1B, 2B, 3B, SS, OF, DH, CI, MI)
  - A main `DT::DTOutput('players')` table and a sidebar `DT::DTOutput('projected_standings')`

- `DraftAssistant/server.R`: precomputes data frames and defines:
  - A `selected_players()` reactive that filters the `par` table based on `input$team` and `input$pos`
  - `output$players` as a paginated, scrollable DT table backed by `selected_players()`
  - `output$projected_standings` as a DT table backed by `projected_standings`

When making changes, keep the UI inputs and server logic in sync: modifying team names or position labels in `ui.R` without updating the corresponding filters in `server.R` will break filtering behavior.

### Season/version handling

Data files and notebooks are named by year and sometimes by date suffixes (e.g., `*_2025.csv`, `*_2026.csv`, `*_ros_250731.csv`). The live Shiny app currently targets the `2025` projection CSVs in the repo root.

If you roll the system forward to a new season:

- Add updated projection CSVs with the new season year
- Update both `DraftAssistant/server.R` and any season-specific notebooks so that they read from the new files and, if necessary, new Google Sheet tabs

## Testing and linting

There is no explicit automated testing or linting configuration checked into this repository:

- No `tests/` or `testthat`-style structure is present; the `test/` directory contains the stock Shiny example app and is not wired into any test runner.
- There are no Makefiles, CI scripts, or R-specific linting configs.

In practice, changes are validated by:

- Running the Shiny app (`shiny::runApp('DraftAssistant')`) and manually verifying player tables and projected standings
- Rendering the relevant R Markdown notebooks and checking that all chunks run successfully and produce coherent standings and valuations.
