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

### `scripts/inseason_update.R`

The core orchestrator. Runs all 8 steps of the projection pipeline:

1. **Fetch ESPN standings** — current-year season-to-date team stats via `fetch_espn_standings()`. Sources `fetch_espn_standings.R` into a local environment to prevent its auto-run block from firing (it would otherwise fetch 5 years of historical data).
2. **Fetch ESPN rosters** — which players are on which fantasy teams.
3. **Download FanGraphs ROS projections** — per-player projected remaining stats. Filters to NL-only teams.
4. **Join rosters to projections** — matches roster player names to FanGraphs names using `normalize_name()` (strips Jr./Sr./III/II, converts accented chars) + exact match first, then `stringdist_left_join()` (max distance 2) for fuzzy fallback.
5. **Aggregate ROS stats by team** — sums per-player projections for each fantasy team.
6. **Combine YTD + ROS** — counting stats are added directly. Rate stats are recomputed from components:
   - `AVG = (H_ytd + H_ros) / (AB_ytd + AB_ros)`
   - `ERA = (ER_ytd + ER_ros) * 9 / (IP_ytd + IP_ros)`
   - `WHIP = (BB_ytd + BB_ros + HA_ytd + HA_ros) / (IP_ytd + IP_ros)`
7. **Rank and score** — ranks teams 1–10 in each of 10 categories (R, HR, RBI, SB, AVG, W, SV, SO, ERA, WHIP). Roto points = `N_TEAMS + 1 - rank`. Total points = sum across categories.
8. **Output** — writes three files:
   - `data/processed/inseason_projected_standings.csv` — team-level projections with ranks and roto points
   - `data/processed/inseason_team_details.csv` — player-level ROS projection breakdown by team
   - `data/processed/inseason_status.json` — pipeline status (`success`/`error`, `last_updated`, `warnings`, `error_message`)

**Run locally:** `Rscript scripts/inseason_update.R`

**Error handling:** The entire pipeline is wrapped in `tryCatch()`. On failure, `inseason_status.json` is written with `status: "error"` and the error message, then the script exits with code 1.

## API endpoints (in-season)

All served by `server.js` (Bun/Hono on Railway).

| Method | Path | Description |
|--------|------|-------------|
| `POST` | `/run_inseason_update` | Triggers `scripts/inseason_update.R`. Returns 409 if an R script is already running. |
| `GET` | `/inseason_standings` | Returns `{ standings: [...], status: {...} }` from the CSV + status JSON. |
| `GET` | `/inseason_team/:team` | Returns `{ team, players: [...] }` filtered by team name (case-insensitive substring match). |
| `GET` | `/inseason_status` | Returns the pipeline status JSON (last_updated, status, warnings, errors). |

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

## Lovable dashboard

The Lovable app is a standalone web app that calls the Railway API. It displays:

- **Standings table** — all 10 teams sorted by projected finish, with category breakdowns and roto points. Blue Socks row is highlighted.
- **Error alerting** — red banner if `status.status === "error"`, yellow banner for warnings, stale-data warning if data is >36 hours old.
- **Team drill-down** — click a team to see its player-level ROS projections (hitters and pitchers separately).
- **Last-updated timestamp** — from `status.last_updated`.

To modify the Lovable app, open it in the Lovable editor and prompt changes in natural language. The API contract is:
- `GET /inseason_standings` → `{ standings: [{team_name, projected_finish, total_pts, proj_R, pts_R, rank_R, ...}], status: {last_updated, status, data_date, error_message?, warnings?} }`
- `GET /inseason_team/{name}` → `{ team, players: [{team_name, player_name, lineup_slot, player_type, AB?, H?, R?, HR?, ...}] }`

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
- **All rostered players are projected** — bench and IL players are included in ROS projections. FanGraphs ROS projections already account for expected playing time (including injury adjustments), so this is a reasonable approximation. A refinement would be to only project starting lineup contributions.
- **Name matching is fuzzy** — player name matching between ESPN rosters and FanGraphs projections uses normalized names + Levenshtein distance ≤ 2. Most players match, but some edge cases may be missed. Check `n_hitters_matched` and `n_pitchers_matched` in the standings output for coverage.
- **No roster slot optimization** — the projection sums all rostered players' stats rather than optimizing lineup decisions (e.g. picking the best 5 OF from 7 eligible players).
- **FanGraphs cookie expiry** — the `FANGRAPHS_COOKIE` and login credentials need periodic refresh.

### Planned next steps
- **Transaction recommendations** — recommend drops, adds, and trades based on projected standings impact (the original goal beyond v1).
- **Waiver wire analysis** — compare available free agents to current roster players.
- **Trade scenario modeling** — simulate the standings impact of proposed trades (can reuse existing `run_trade_scenario.R` patterns).
- **Historical tracking** — store daily snapshots of projected standings to show trends over time.
