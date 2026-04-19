# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Project overview

Billiken is a fantasy baseball analysis and draft simulation system for the Billiken League. The repository is organized around:

- **R pipeline** (`scripts/`) — data fetching, SGP (Standings Gained Points) calculation, roster optimization, draft simulation, and trade scenario analysis
- **In-season monitor** (`scripts/inseason_update.R`) — daily pipeline that projects end-of-season roto standings by combining ESPN YTD stats with FanGraphs ROS projections. See `docs/INSEASON_MONITOR.md` for full details.
- **Bun/Hono API server** (`server.js`) — deployed on Railway, exposes REST endpoints that trigger R scripts for projections, draft pick comparisons, and in-season standings updates
- **Lovable dashboards** — external frontends: one for draft simulation results, one for in-season standings monitoring (both call the Railway API)
- **n8n automation** — runs daily data updates and triggers updates when the draft Google Sheet changes
- **Shiny draft assistant** (`DraftAssistant/`) — legacy interactive tool
- **Flask trade scenario viewer** (`app.py` + `templates/index.html`) — standalone trade scenario comparison UI
- **R Markdown notebooks** — season-specific analysis (root-level `.Rmd` files)

The RStudio project file `billiken.Rproj` anchors the working directory at the repo root.

## API Server (Railway deployment)

`server.js` is a Bun/Hono HTTP server deployed on Railway. It shells out to R scripts to run projections and draft simulations.

### Endpoints

- `POST /run_projections` — runs `scripts/prefreeze_update.R` to refresh all data
- `POST /run_pick_sim` — runs `scripts/compare_draft_picks.R` with `--players`, `--team`, `--round`, `--pick`, `--n_sims` args
- `POST /run_simulation` — runs both `prefreeze_update.R` then `compare_draft_picks.R` (similar to `/run_pick_sim` but with data refresh)
- `GET /projections` — serves `data/processed/projections_prefreeze.csv`
- `GET /draft_results` — serves `data/raw/draft_latest.csv`
- `GET /pick_comparisons` — serves the most recently modified file from `output/`
- `POST /run_inseason_update` — runs `scripts/inseason_update.R` (daily in-season pipeline)
- `GET /inseason_standings` — serves projected end-of-season standings as JSON
- `GET /inseason_team/:team` — serves player-level ROS projections for a specific team
- `GET /inseason_free_agents` — serves ranked best-available players (not on any Billiken roster). Query params: `type=hitter|pitcher|all`, `position=C|1B|2B|3B|SS|OF|DH|SP|RP`, `limit` (default 50)
- `GET /inseason_free_agents/:player` — single free-agent lookup by name
- `GET /inseason_status` — pipeline health/status check
- `GET /health` — health check

### Running locally

```sh
bun server.js
```

Requires Bun runtime. The server listens on `PORT` env var (default 3000).

### Docker / Railway deployment

The `Dockerfile` builds from `rocker/r-ver:4.4.1`, installs Bun + system libs, restores R packages via `renv::restore()`, and runs `bun server.js`. Railway deploys this image.

### Environment variables (API server)

The R scripts called by the server require these env vars (set in Railway):

- `BILLIKEN_SHEET_ID` — Google Sheet ID for league data
- `FANGRAPHS_EMAIL`, `FANGRAPHS_PASSWORD` — FanGraphs login credentials
- `FANGRAPHS_COOKIE` — FanGraphs session cookie (set by `fangraphs_login.R`)
- `ESPN_LEAGUE_ID`, `ESPN_SEASON`, `ESPN_SCORING_PERIOD_ID` — ESPN Fantasy API
- `ESPN_S2`, `SWID` — ESPN auth cookies (for private leagues)
- `BILLIKEN_PROJECTIONS_YEAR` — projection year (default: current year)
- `PORT` — server port (default: 3000)

### Known issues

- `package.json` has `"start": "node server.js"` but the code uses `Bun.serve()` — these are mismatched
- `runRScript()` uses `execSync` inside a Promise, blocking the event loop during long R runs
- No authentication on endpoints — anyone with the Railway URL can trigger simulations
- POST endpoints return `{status: "complete"}` without the actual results

## n8n Integration

n8n is used for workflow automation:

- **Daily update** — triggers `POST /run_projections` to refresh data from Google Sheets, FanGraphs, and ESPN
- **Draft sheet trigger** — watches the Billiken Google Sheet for draft changes and triggers `POST /run_pick_sim` or `/run_simulation`

n8n calls the Railway-hosted API endpoints.

## Flask Trade Scenario Viewer

`app.py` is a Flask app that serves the trade scenario comparison UI (`templates/index.html`). It reads scenario results from `data/scenarios/` and trade definitions from `scenarios/`.

### Endpoints

- `GET /` — renders the scenario viewer page
- `GET /api/scenario/<name>` — returns delta summary JSON for a scenario
- `POST /api/scenarios` — saves a new trade scenario CSV
- `GET /api/scenarios/<name>/definition` — returns the trade definition rows
- `PUT /api/scenarios/<name>` — updates an existing scenario definition

### Running locally

```sh
python app.py
```

Listens on port 5000 (and 65535). Requires Flask (`pip install flask`).

## Running the Shiny Draft Assistant

The Shiny app under `DraftAssistant/` is the legacy interactive tool.

### Start the app

```sh
R -q -e "shiny::runApp('DraftAssistant')"
```

### Shiny app dependencies

Key R packages: `shiny`, `tidyverse`, `googlesheets4`, `fuzzyjoin`, `DT`, `stringi`.

The app reads projection CSVs from the repo root and league data from the Billiken Google Sheet (de-authed/public access).

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

### Draft pick comparison

8. **`compare_draft_picks.R`** - Compare candidates for a specific draft pick via simulation
   ```sh
   Rscript scripts/compare_draft_picks.R \
     --players="Bryce Harper,Bo Bichette" \
     --team="Blue Socks" \
     --n_sims=100
   ```
   - Auto-detects next open pick for the team, or use `--round` and `--pick` to override
   - If `--players` is omitted, auto-selects top N available players by sgpar
   - Outputs comparison CSV under `data/compare_picks/`
   - This is the script called by the Railway API's `/run_pick_sim` and `/run_simulation` endpoints

### Other scripts

- **`download_fangraphs_projections.R`** - Downloads projections (called by `draft_day_update.R`)
- **`download_fangraphs_auction_values.R`** - Downloads FanGraphs auction calculator dollar values
- **`fetch_espn_positions.R`** - Fetches positional eligibility from ESPN API
- **`fetch_espn_standings.R`** - Fetches historical standings from ESPN API
- **`fangraphs_login.R`** - Authentication helper for FanGraphs (sourced by download script)
- **`draft_simulation_lib.R`** - Shared draft simulation functions (`run_simulations()`, `summarize_simulations()`)
- **`calculate_player_value.R`** - Calculates SGPAR and dollar values per player; outputs `data/processed/projected_player_value.csv`
- **`simulate_keepers.R`** - Simulates keeper selections (supports optional trade overlays; outputs under the provided `output_dir`)
- **`trade_utils.R`** - Helpers for reading/applying trade scenario CSVs (player moves + pick trades)
- **`paths.R`** - Optional helper for resolving project-root-relative paths
- **`prefreeze_update.R`** - Orchestrates the full data refresh pipeline (called by `server.js`); note: hardcodes `setwd("/app")` for Docker
- **`update_current_rosters.R`** - Builds preseason rosters via bipartite matching on ESPN position eligibility
- **`install_packages.R`** - Installs required R packages (redundant with renv)

### Dead code (api/ directory)

The `api/` directory contains `run_projections.R`, `run_pick_sim.R`, and `run_simulation.R`. These are thin wrappers that `server.js` does not use — it calls scripts directly. They can be removed.

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

Data files and notebooks are named by year and sometimes by date suffixes (e.g., `*_2025.csv`, `*_2026.csv`). The scripts pipeline reads projections from `data/raw/` (e.g., `hitter_projections_2026.csv`) keyed by `BILLIKEN_PROJECTIONS_YEAR`. The Shiny app reads from the repo root.

If you roll the system forward to a new season:

- Set `BILLIKEN_PROJECTIONS_YEAR` to the new year
- Add updated projection CSVs
- Update both `DraftAssistant/server.R` and any season-specific notebooks

## GitHub Actions

`.github/workflows/run-trade-scenario.yml` triggers when `scenarios/*.csv` files are pushed to `main`. It runs `scripts/run_trade_scenario.R` for each changed scenario file. Note: this workflow does not commit results back, and may overlap with n8n-driven automation.

## Testing and linting

There is no explicit automated testing or linting configuration:

- No `tests/` or `testthat`-style structure is present; the `test/` directory contains a stock Shiny example app.
- No Makefiles, CI scripts, or R-specific linting configs.

In practice, changes are validated by:

- Running the Shiny app (`shiny::runApp('DraftAssistant')`) and manually verifying player tables and projected standings
- Rendering the relevant R Markdown notebooks and checking that all chunks run successfully
- Triggering the API endpoints on Railway and checking the output CSVs

## Other artifacts

- `main.py` and `pyproject.toml` — Replit scaffolding artifacts; not part of the active system
- `run_app.R` — alternative app launcher that installs packages and runs a `TradeScenarios` Shiny app on port 5000
- `renv.lock` + `renv/` — R package dependency management; `renv::restore()` is run in the Dockerfile
