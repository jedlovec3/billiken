# In-Season Standings Monitor

A daily-updating system that projects end-of-season roto standings by combining ESPN season-to-date stats with FanGraphs rest-of-season projections for each team's current roster.

## Architecture

```
Daily Cron (Railway) → POST /run_inseason_update
                            ↓
              inseason_update.R orchestrates:
                ├─ fetch_espn_standings(2026)    → YTD team stats
                ├─ fetch_espn_rosters()          → current fantasy rosters
                ├─ download_ros_projections()    → FanGraphs ROS per-player stats
                ├─ join rosters + projections    → per-team ROS stats
                ├─ YTD + ROS = projected totals  → end-of-season projections
                └─ rank + score                  → projected roto standings
                            ↓
              CSV + JSON outputs in data/processed/
                            ↓
              GET /inseason_standings (JSON API)
                            ↓
              Lovable dashboard renders standings
```

### Tool stack

- **Railway** (`billiken-production.up.railway.app`) — hosts the Bun/Hono API server + R runtime. Auto-deploys from `main` branch. Runs the daily cron.
- **Lovable** — frontend dashboard. Calls the Railway API to display projected standings, team drill-downs, and error alerts.
- **GitHub** (`jedlovec3/billiken`) — source control. Pushes to `main` trigger Railway redeploy.

## New scripts (in-season pipeline)

### `scripts/fetch_espn_rosters.R`

Fetches current fantasy rosters from the ESPN v3 API using the `mRoster` view. Makes two API calls: `mTeam` for team names and `mRoster` for roster entries (the mRoster view alone doesn't return team names).

- **Output:** `data/raw/espn_rosters_latest.csv` + timestamped snapshot
- **Columns:** `team_id, team_name, player_id, player_name, pro_team_id, default_position_id, lineup_slot_id, lineup_slot`
- **Env vars:** `ESPN_LEAGUE_ID`, `ESPN_SEASON`, `ESPN_S2`, `SWID`
- **Run standalone:** `Rscript scripts/fetch_espn_rosters.R`

### `scripts/download_ros_projections.R`

Downloads FanGraphs Rest-of-Season Depth Charts projections (`rfangraphsdc` projection type — note the `r` prefix vs `fangraphsdc` for full-season).

- **Output:** `data/raw/ros_hitter_projections_{year}.csv`, `data/raw/ros_pitcher_projections_{year}.csv`
- **Env vars:** `FANGRAPHS_COOKIE` (required), `FANGRAPHS_USER` + `FANGRAPHS_PASS` (optional, for login refresh)
- **Run standalone:** `Rscript scripts/download_ros_projections.R`

### `scripts/inseason_free_agents.R`

Computes "best available" free agents from the FanGraphs ROS projections (NL‑only) after removing every player on any Billiken roster. Applies the same SGP formulas as `scripts/calculate_player_sgp.R` but with ROS stats, using unit values from `data/processed/category_unit_values.csv` + `category_value_scaling.csv`. Falls back to a per‑stat z‑score composite when those files are missing.

Exports two functions consumed by `inseason_update.R`:

- `compute_inseason_free_agents(ros_hitters, ros_pitchers, rostered_names_normalized, positions_path, normalize_fn)` returns a combined tibble (see schema under "Free agents" below).
- `score_rostered_players(roster_hitters, roster_pitchers)` attaches `sgp_total`/`sgp_hitting`/`sgp_pitching` to the rostered‑player frames so the team details CSV can be sorted/surfaced consistently with the free‑agent rankings.

Positional eligibility comes from `data/raw/positions_latest.csv` (produced by `scripts/fetch_espn_positions.R`). If the file is absent the `positions` column is `NA` and the dashboard position filter becomes a no‑op.

### `scripts/inseason_update.R`

The core orchestrator. Runs all 9 steps of the projection pipeline:

1. **Fetch ESPN standings** — current-year season-to-date team stats via `fetch_espn_standings()`. Sources `fetch_espn_standings.R` into a local environment to prevent its auto-run block from firing (it would otherwise fetch 5 years of historical data).
2. **Fetch ESPN rosters** — which players are on which fantasy teams.
3. **Download FanGraphs ROS projections** — per-player projected remaining stats. Filters to NL-only teams.
4. **Join rosters to projections** — matches roster player names to FanGraphs names using `normalize_name()` (strips Jr./Sr./III/II, converts accented chars) + exact match first, then `stringdist_left_join()` (max distance 2) for fuzzy fallback.
   - **4b — Score rostered players (ROS SGP)**: calls `score_rostered_players()` so `roster_hitters`/`roster_pitchers` carry per‑player `sgp_total`.
   - **4c — Compute free agents**: calls `compute_inseason_free_agents()` to rank every FanGraphs ROS player not on any roster.
5. **Aggregate ROS stats by team** — sums per-player projections for each fantasy team.
6. **Combine YTD + ROS** — counting stats are added directly. Rate stats are recomputed from components:
   - `AVG = (H_ytd + H_ros) / (AB_ytd + AB_ros)`
   - `ERA = (ER_ytd + ER_ros) * 9 / (IP_ytd + IP_ros)`
   - `WHIP = (BB_ytd + BB_ros + HA_ytd + HA_ros) / (IP_ytd + IP_ros)`
7. **Rank and score** — ranks teams 1–10 in each of 10 categories (R, HR, RBI, SB, AVG, W, SV, SO, ERA, WHIP). Roto points = `N_TEAMS + 1 - rank`. Total points = sum across categories.
8. **Output standings + team details** — writes:
   - `data/processed/inseason_projected_standings.csv` — team-level projections using **all** rostered players (bench + IL + minors included)
   - `data/processed/inseason_projected_standings_active.csv` — same schema, but computed only from players in **active** ESPN lineup slots (excludes bench, IL, minors). Use this view to avoid double-counting the ROS stats of stashed players that would displace an active player upon return.
   - `data/processed/inseason_team_details.csv` — player-level ROS projection breakdown by team, including `sgp_total` (per‑player ROS SGP) and `roster_status` (`active` / `bench` / `IL` / `minors`)
9. **Output free agents + status** — writes:
   - `data/processed/inseason_free_agents.csv` — ranked free‑agent pool with ROS SGP and position eligibility
   - `data/processed/inseason_status.json` — pipeline status (`success`/`error`, `last_updated`, `warnings`, `error_message`, `sgp_source` = `unit_values`\|`fallback`, `n_free_agents`)

**Run locally:** `Rscript scripts/inseason_update.R`

**Error handling:** The entire pipeline is wrapped in `tryCatch()`. On failure, `inseason_status.json` is written with `status: "error"` and the error message, then the script exits with code 1.

## API endpoints (in-season)

All served by `server.js` (Bun/Hono on Railway).

| Method | Path | Description |
|--------|------|-------------|
| `POST` | `/run_inseason_update` | Triggers `scripts/inseason_update.R`. Returns 409 if an R script is already running. |
| `GET` | `/inseason_standings` | Returns `{ standings: [...], view, status: {...} }` from the CSV + status JSON. Query param `active_only=true` serves the active-slot-only projection (default `false` = all rostered). |
| `GET` | `/inseason_team/:team` | Returns `{ team, players: [...] }` filtered by team name (case-insensitive substring match). |
| `GET` | `/inseason_free_agents` | Returns `{ free_agents: [...], count, status }`. Query params: `type` (`hitter`\|`pitcher`\|`all`, default `all`), `position` (`C`, `1B`, `2B`, `3B`, `SS`, `OF`, `DH`, `SP`, `RP`), `limit` (default 50, 0 = no limit). |
| `GET` | `/inseason_free_agents/:player` | Single-player lookup by exact name (case-insensitive). |
| `GET` | `/inseason_status` | Returns the pipeline status JSON (last_updated, status, warnings, errors, sgp_source, n_free_agents). |

CORS is enabled for all origins (`Access-Control-Allow-Origin: *`) so the Lovable frontend can call the API.

## Environment variables (Railway)

These must be set in the Railway dashboard under your service's Variables tab:

| Variable | Description |
|----------|-------------|
| `ESPN_LEAGUE_ID` | `14845` |
| `ESPN_SEASON` | Current season year (e.g. `2026`) |
| `ESPN_S2` | ESPN auth cookie (for private leagues) |
| `SWID` | ESPN auth cookie (for private leagues) |
| `FANGRAPHS_USER` | FanGraphs login email |
| `FANGRAPHS_PASS` | FanGraphs login password |
| `FANGRAPHS_COOKIE` | FanGraphs session cookie |
| `BILLIKEN_PROJECTIONS_YEAR` | Current season year (e.g. `2026`) |
| `PORT` | `3000` |

**Note:** ESPN cookies (`ESPN_S2`, `SWID`) expire periodically. If the pipeline starts failing with ESPN auth errors, re-extract these from your browser. The same applies to `FANGRAPHS_COOKIE`.

## Daily cron

The in-season update runs daily at 6 AM Central via Railway cron. The cron service calls `POST /run_inseason_update` on the Railway-hosted API.

**To set up or modify the cron:** In the Railway project, the cron service is configured with schedule `0 12 * * *` (12:00 UTC = 6:00 AM Central). It uses `curlimages/curl:latest` with start command `curl -X POST https://billiken-production.up.railway.app/run_inseason_update`.

**Alternative:** You can also add cron directly in `server.js` using `setTimeout` scheduling (see Option B in the plan). This avoids a separate Railway service.

## Free agents

`data/processed/inseason_free_agents.csv` is the backing file for the "Best Available" view. One row per FanGraphs ROS-projected NL player that is NOT currently on a Billiken roster, sorted by `sgp_total` (higher = better).

Columns:

- `rank_overall` — rank across all free agents (hitters + pitchers) by `sgp_total`.
- `rank_by_type` — rank within the player's type (hitter or pitcher).
- `player_type` — `hitter` or `pitcher`.
- `Name`, `Team` — FanGraphs player name and MLB team abbreviation.
- `positions` — pipe-separated eligibility (e.g. `1B|OF|DH`) sourced from `data/raw/positions_latest.csv`. The server.js position filter splits on `|`. NA when positions are unknown.
- Hitter stats: `AB`, `H`, `R`, `HR`, `RBI`, `SB`, `AVG`.
- Pitcher stats: `IP`, `W`, `SV`, `SO`, `ERA`, `WHIP`.
- Per-category SGP components: `sgp_R`, `sgp_HR`, `sgp_RBI`, `sgp_SB`, `sgp_AVG`, `sgp_W`, `sgp_SV`, `sgp_SO`, `sgp_ERA`, `sgp_WHIP`.
- Summary SGP: `sgp_hitting`, `sgp_pitching`, `sgp_total`.

### SGP source

Unit values come from `data/processed/category_unit_values.csv` + `category_value_scaling.csv`, which are refreshed by the pre‑freeze pipeline (`standings_gained_points.R`) not the in‑season one. In practice the values change very slowly across seasons, so using the last‑committed version mid‑season is fine. If those files are missing on Railway at runtime, the free‑agent ranker falls back to a per‑stat z‑score composite and `status.sgp_source` in `inseason_status.json` flips from `unit_values` to `fallback` (surfaced as a dashboard warning).

## Transaction recommendations (planned)

The free‑agent feature is stage 1 of a broader goal: recommend concrete drop/add moves that improve a team's projected standings. The current output already contains everything that recommendation engine will need:

- `inseason_team_details.csv` carries per-rostered-player `sgp_total` (ROS SGP).
- `inseason_free_agents.csv` carries per-FA `sgp_total` + position eligibility.

Planned flow for the next phase:

1. **First-order score**: for each eligible swap (drop rostered X, add FA Y where Y's positions intersect X's lineup slot), compute `delta_sgp = fa.sgp_total - roster.sgp_total`. Sort descending per team.
2. **Second-order score**: recompute projected standings with the swap applied (rebuild category totals, re-rank, diff total roto points). Refactor steps 5–7 of `inseason_update.R` into a reusable `project_standings()` helper when that work starts.
3. **New artifacts**: `data/processed/inseason_recommendations.csv` and `GET /inseason_recommendations/:team` returning top-N drop/add moves for a given fantasy team.

## Lovable dashboard

The Lovable app is a standalone web app that calls the Railway API. It displays:

- **Standings table** — all 10 teams sorted by projected finish, with category breakdowns and roto points. Blue Socks row is highlighted.
- **Error alerting** — red banner if `status.status === "error"`, yellow banner for warnings, stale-data warning if data is >36 hours old.
- **Team drill-down** — click a team to see its player-level ROS projections (hitters and pitchers separately) and their individual `sgp_total`.
- **Best Available tab** — ranked free agents with a Hitters/Pitchers segmented control and an optional position filter. Defaults to top 50 by `sgp_total`.
- **Last-updated timestamp** — from `status.last_updated`.

To modify the Lovable app, open it in the Lovable editor and prompt changes in natural language. The API contract is:
- `GET /inseason_standings?active_only=true|false` → `{ standings: [{team_name, projected_finish, total_pts, proj_R, pts_R, rank_R, ...}], view: "all_rostered"|"active_only", status: {last_updated, status, data_date, error_message?, warnings?, sgp_source?, n_free_agents?} }`. Default is `active_only=false` (all rostered players). Flip to `true` for an active-slot-only view that excludes bench, IL, and minors.
- `GET /inseason_team/{name}` → `{ team, players: [{team_name, player_name, lineup_slot, roster_status, player_type, AB?, H?, R?, HR?, …, sgp_total?}] }` — `roster_status` is `active`/`bench`/`IL`/`minors`; use it to badge or filter rows on the team drill-down.
- `GET /inseason_free_agents?type=hitter|pitcher&position=<pos>&limit=50` → `{ free_agents: [{rank_overall, rank_by_type, player_type, Name, Team, positions, AB?, R?, HR?, …, IP?, W?, …, sgp_total}], count, status }`

## Local development

### Run the pipeline locally

```sh
# From the repo root (requires .Renviron with ESPN/FanGraphs env vars)
Rscript scripts/inseason_update.R
```

### Run individual steps

```sh
Rscript scripts/fetch_espn_rosters.R
Rscript scripts/download_ros_projections.R
```

### Test the API locally

```sh
bun server.js
# In another terminal:
curl -X POST http://localhost:3000/run_inseason_update
curl http://localhost:3000/inseason_standings
curl http://localhost:3000/inseason_team/Blue%20Socks
```

### Deploy

Push to `main` — Railway auto-deploys:

```sh
git add -A && git commit -m "your message" && git push origin main
```

The Docker build takes ~5-10 minutes (R package installation). Monitor deploy logs in the Railway dashboard.

## Known limitations and future work

### Current limitations
- **Two projection views, neither perfect** — the pipeline now emits both an "all rostered" projection (which double-counts the ROS stats of stashed players and the bench guys they would displace on return) and an "active-slot only" projection (which gives zero credit for bench/IL/minors contributions). The Lovable dashboard exposes these via the `active_only` toggle. FanGraphs ROS already accounts for expected playing time, so both views are biased in opposite directions. A future refinement can model the drop-when-activated behavior explicitly (see "Planned next steps").
- **Name matching is fuzzy** — player name matching between ESPN rosters and FanGraphs projections uses normalized names + Levenshtein distance ≤ 2. Most players match, but some edge cases may be missed. Check `n_hitters_matched` and `n_pitchers_matched` in the standings output for coverage.
- **No roster slot optimization** — the projection sums all rostered players' stats rather than optimizing lineup decisions (e.g. picking the best 5 OF from 7 eligible players).
- **FanGraphs cookie expiry** — the `FANGRAPHS_COOKIE` and login credentials need periodic refresh.

### Planned next steps
- **Transaction recommendations** — recommend drops, adds, and trades based on projected standings impact (the original goal beyond v1). See "Transaction recommendations (planned)" above for the design hook.
- **Waiver wire analysis** — compare available free agents to current roster players. (v1 of this shipped as the `/inseason_free_agents` endpoint + Best Available tab.)
- **Trade scenario modeling** — simulate the standings impact of proposed trades (can reuse existing `run_trade_scenario.R` patterns).
- **Historical tracking** — store daily snapshots of projected standings to show trends over time.
