# Trade Lab Prospect and Pick Valuation Design

Date: 2026-06-16

## Goal

Improve the Trade Lab for a rebuilding Billiken team by valuing future assets
more realistically. The first implementation should:

- Fix the FanGraphs ROS auction-value handoff so 2026 win-now value uses the
  intended rest-of-season source.
- Add repeatable prospect valuation using FanGraphs The Board and MLB/MiLB
  prospect rankings.
- Use ETA to discount prospect value by timing.
- Use FanGraphs ZiPS future-season projections when available.
- Let trade recommendations target draft picks and prospects, not only
  partner rostered players.
- Keep all outputs compatible with the Railway service and Lovable dashboard.

## Source Inputs

### FanGraphs Prospect Source

Use `https://www.fangraphs.com/prospects/the-board` as the FanGraphs prospect
source. The page exposes the key fields needed by the model: overall rank,
organization rank, organization, position, level, ETA, future value grade, risk,
and trend.

FanGraphs marks direct data export as members-only. The fetcher should prefer
structured data when accessible with the configured FanGraphs cookie, but it
must cache snapshots and fail softly so the daily Railway job is not brittle.

### MLB/MiLB Prospect Source

Use `https://www.mlb.com/milb/prospects` as the MLB Pipeline source. The page
exposes Top 100 rank, player name, position, team, level, ETA, age, height and
weight, bats, and throws.

This source should be the minimum viable prospect source because it is public
and stable enough to support repeatable refreshes.

### FanGraphs Future Projections

FanGraphs links 3-year ZiPS projections for 2027 and 2028. The existing
projection endpoint shape works for these:

- `type=zipsp1` for ZiPS 2027
- `type=zipsp2` for ZiPS 2028
- `stats=bat` and `stats=pit`

Add a downloader that writes:

- `data/raw/future_hitter_projections_2027.csv`
- `data/raw/future_pitcher_projections_2027.csv`
- `data/raw/future_hitter_projections_2028.csv`
- `data/raw/future_pitcher_projections_2028.csv`

If future projections are unavailable for a player, use the prospect ranking
model instead.

## New Pipeline Scripts

### `scripts/download_prospect_rankings.R`

Responsibilities:

- Fetch MLB/MiLB Top 100 prospect rankings.
- Fetch FanGraphs The Board prospect rankings when accessible.
- Normalize common fields:
  - `Name`
  - `source`
  - `source_rank`
  - `mlb_org`
  - `position`
  - `level`
  - `eta`
  - `age`
  - `fg_fv`
  - `fg_risk`
- Write source snapshots:
  - `data/raw/prospects_mlb_latest.csv`
  - `data/raw/prospects_fangraphs_latest.csv`
  - timestamped equivalents.
- Write a status JSON if a source fails, but do not fail the whole in-season
  pipeline when at least one prospect source succeeds.

### `scripts/download_future_projections.R`

Responsibilities:

- Fetch ZiPS 2027 and 2028 projections from FanGraphs for hitters and pitchers.
- Use the same request conventions as existing FanGraphs projection scripts.
- Standardize `Name`, `Team`, `playerid`, MLBAM ID if present, and core 5x5
  stats.
- Write raw future projection CSVs.
- Fail softly from the daily pipeline and surface warnings in
  `inseason_status.json`.

### `scripts/build_prospect_values.R`

Responsibilities:

- Read the raw prospect ranking files.
- Build a consensus prospect table.
- Join FanGraphs future projections when available.
- Convert rankings/FV/ETA into future dollar value streams.
- Write `data/processed/prospect_values.csv`.

Output columns:

- `Name`
- `name_normalized`
- `mlb_org`
- `position`
- `level`
- `eta`
- `age`
- `fg_rank`
- `fg_fv`
- `fg_risk`
- `mlb_rank`
- `consensus_rank`
- `future_projection_source`
- `prospect_value_2027`
- `prospect_value_2028`
- `prospect_value_2029`
- `prospect_value`
- `prospect_value_source`

## Valuation Model

### Consensus Rank

Use a stable weighted blend:

- If both FanGraphs and MLB ranks are available, average normalized percentile
  ranks with FanGraphs weighted slightly higher because FV and risk are useful.
- If only one source is available, use that source.
- If FanGraphs FV is available without a top-100 rank, map FV to a rank band.

The first version can use transparent heuristics rather than a trained model.
All source columns should be retained so the dashboard can explain why a player
has value.

### ETA Timing Discount

ETA should affect when the value arrives. A similarly ranked 2026 ETA prospect
is worth more than a 2028 ETA prospect because the value is closer and less
uncertain.

Default ETA multipliers:

- 2026: `1.00`
- 2027: `0.75`
- 2028: `0.55`
- 2029 or later: `0.35`
- missing ETA: `0.50`

The multiplier applies to ranking-derived prospect value. For players with
actual FanGraphs future projections, use projected dollar value for the relevant
year and apply only a light uncertainty discount if needed.

### Base Prospect Value

Use a curve shaped like the current draft-pick curve but tuned for prospects:

- elite top-5 prospects: high future asset value
- top-25 prospects: meaningful rebuild targets
- top-100 prospects: positive but increasingly modest value
- non-top-100 FV prospects: FV-band value

The implementation should put coefficients at the top of
`build_prospect_values.R` so they can be tuned without changing downstream
code.

### Future Projection Override

For a prospect with ZiPS 2027 or 2028 projections, use the projected stat line
to derive fantasy dollar value with the existing category-unit valuation inputs
when those files are available. If the valuation inputs are missing, retain the
raw future projection columns and use projection presence as a confidence signal
that raises the ETA/rank-derived value.

Recommended first-pass hierarchy:

1. FanGraphs ZiPS future projection dollar value when category-unit valuation
   inputs are available.
2. FanGraphs FV + ETA value if available.
3. MLB rank + ETA value if available.
4. Heuristic fallback from age, level, position, and ETA.

## Existing Pipeline Changes

### Fix ROS Auction Values

`scripts/download_fangraphs_auction_values.R` should respect:

- `FANGRAPHS_AUCTION_PROJ`
- `FANGRAPHS_AUCTION_OUTFILE`

When `inseason_update.R` sets `FANGRAPHS_AUCTION_PROJ=rfangraphsdc`, the
auction downloader should write `data/raw/auction_values_ros_<year>.csv`, which
`build_team_assets.R` already expects.

### Add Prospect Values to Team Assets

`scripts/build_team_assets.R` should join `data/processed/prospect_values.csv`
by normalized player name. It should add:

- `prospect_value`
- `prospect_value_2027`
- `prospect_value_2028`
- `prospect_value_2029`
- `consensus_rank`
- `eta`
- `prospect_value_source`

Future value should become:

`discounted_surplus + prospect_value - drop_penalty_liability`

The drop penalty should finally match the existing docs: extended contracts with
future penalty exposure should receive a liability haircut when the receiving
team would be forced to keep or eat the contract.

### Add Picks and Prospects as Targets

`scripts/trade_recommendations.R` should allow rebuilders to target:

- players with high `future_value`
- prospects
- next-year draft picks

Do not limit acquisition targets to `asset_type == "player"`. Keep players,
picks, and prospects in a shared asset universe with:

- `asset_type`
- `asset_id`
- `asset_label`
- `win_now_value`
- `future_value`
- `prospect_value`
- `pick_value`

For rebuild posture, sort target candidates primarily by `future_value`, then
`prospect_value`, then pick value.

## API Contract

Existing endpoints can carry most of the new fields without a new route because
the server serializes CSV rows. Add one optional endpoint for inspection:

- `GET /prospect_values`

Response:

```json
{
  "prospect_values": [],
  "count": 0
}
```

Existing endpoints should include the new fields when their backing CSVs have
them:

- `GET /team_assets`
- `GET /team_assets/:team`
- `GET /trade_targets/:my_team`
- `POST /evaluate_trade`

## Railway Handoff

After implementation is merged to `main`, the user should:

1. Confirm Railway variables:
   - `FANGRAPHS_COOKIE`
   - `FANGRAPHS_USER`
   - `FANGRAPHS_PASS`
   - `BILLIKEN_PROJECTIONS_YEAR=2026`
2. Redeploy from `main`.
3. Trigger:

```sh
curl -X POST https://billiken-production.up.railway.app/run_inseason_update
```

4. Check:

```sh
curl https://billiken-production.up.railway.app/inseason_status
curl https://billiken-production.up.railway.app/prospect_values
curl "https://billiken-production.up.railway.app/trade_targets/Blue%20Socks?horizon=future"
```

If the prospect sources fail but standings update succeeds, the dashboard should
show a warning rather than failing the whole pipeline.

## Lovable Handoff Prompt

Use this prompt in Lovable after the API deploys:

```text
Update the Trade Lab dashboard to support future-asset valuation for a rebuild.

API base remains the Railway service. Existing endpoints now include these
optional fields on team assets and trade suggestions:
prospect_value, prospect_value_2027, prospect_value_2028,
prospect_value_2029, consensus_rank, eta, prospect_value_source,
asset_type, pick_value, pick_value_source, future_projection_source.

Please make these UI updates:
1. Add a "Future" or "Rebuild" horizon view that calls
   /trade_targets/{my_team}?horizon=future and sorts by my_future_delta.
2. In suggested trades, support targets whose asset_type is "pick" or
   "prospect", not only rostered players.
3. Add small chips for ETA, prospect rank, source, and future value wherever
   prospect fields are present.
4. On team roster/player tables, show prospect_value and consensus_rank when
   available, with empty values hidden rather than rendered as zero.
5. In the manual trade evaluator, allow selecting draft picks and prospect
   assets from either side and show my_future_net and partner_future_net
   prominently.
6. Keep the existing win-now dashboard behavior intact for standings and best
   available players.
```

## Testing Strategy

Add lightweight tests for pure logic and integration contracts:

- Prospect rank consensus calculation.
- ETA discount application.
- Future projection downloader URL construction and file naming.
- ROS auction downloader respects `FANGRAPHS_AUCTION_PROJ` and
  `FANGRAPHS_AUCTION_OUTFILE`.
- Trade target selection includes pick/prospect targets for rebuild posture.
- Server CSV parsing and `/prospect_values` response shape.

Full live network tests should not run by default. Use cached fixtures for CI
and local verification. Live source refresh remains an on-demand/manual
validation step because FanGraphs and MLB can change page shape.

## Non-Goals

- Do not build a perfect historical prospect success model in this pass.
- Do not require Lovable changes before the backend remains backward
  compatible.
- Do not make Railway fail the daily standings pipeline solely because one
  prospect source is unavailable.
- Do not support draft picks beyond next season because league rules only allow
  next-season pick trades.
