# Trade Lab — Plan & Progress

Living doc for the "Trade Possibilities" Lovable tab and its backend. Phases
deliver in dependency order; each one is shippable on its own.

Treat this file as the source of truth when picking up in a new session.
Update it whenever a phase moves, a knob is tuned, or an open question gets
resolved.

## Problem statement
Build a Lovable tab that helps me identify realistic trade targets across the
10-team Billiken league. For each potential partner, the tab should:

1. Show that team's competitive posture (contender / bubble / mid / rebuild)
   given current standings and projected finish.
2. Surface the players on each roster I'd care about, annotated with contract
   years, salary, and surplus value.
3. Translate draft picks (and eventually prospects) onto the same value scale
   as rostered players.
4. Recommend offers that improve my team relative to my own posture while
   plausibly satisfying the partner's posture.

League rules driving the design live in `docs/LEAGUE_RULES.md`. The salient
ones for trade math:

* 10 teams, NL-only, 23 active spots, 10-cat 5x5 roto.
* Prizes 50/30/10 for top-3; the top-3 cliff is the dominant target.
* Keeper allotments: 10 / 11 / 12 / 15 for prior 1st / 2nd / 3rd / everyone
  else. Minimum keepers per GM = 7.
* Contracts: drafted at LABR price for 2 guaranteed years, then 1 option
  offseason. `keepers.csv` uses the literal codes `1`, `2`, `opt`, or a
  four-digit end-year. Salary is flat in years 1 and 2; extending past
  `opt` costs +$5 per added year. Trades do NOT change contracts.
* Drop penalty: dropping a player on a contract extension costs $5 per year
  remaining on next year's cap. Drives the haircut applied to long extensions
  from a partner's POV.
* Pick trades: only next season's picks are tradeable.
* Round 1 lottery: picks 1–7 randomized among prior bottom-7 via card draw;
  prior top-3 are locked into picks 8/9/10 (3rd→8, 2nd→9, 1st→10). Cards =
  `finish - 3`, so worst finisher gets 7 cards (best odds).
* Salary cap: $270 per GM at the draft.

## Phase status

### Phase 1 — Unified roster + contract dataset — ✅ shipped
**Script:** `scripts/build_team_assets.R`
**Output:** `data/processed/team_assets.csv`
**API:** `GET /team_assets`, `GET /team_assets/:team`

One row per rostered player joining ESPN rosters + keeper contracts + salaries
+ FanGraphs projections + ESPN positions. Single source of truth for every
later phase.

Key columns: `billikenTeam`, `Name`, `player_type`, `positions`, `salary_2026`,
`contract_code`, `contract_status` (`year1`/`year2`/`opt`/`extended`),
`contract_end`, `years_remaining`, `is_expiring_after_2026`,
`next_offseason_decision`, `ros_sgp_2026`, `sgpar_full_2026`,
`dollar_value_2026`, `surplus_2026`.

Contract-expiration logic was fixed to only count `opt` (and `extended &&
contract_end == CURRENT_YEAR`) as truly expiring. Year-2 players are NOT
expiring — they auto-roll to `opt` next offseason. Year-1 and year-2 contracts
both reflect the last keepable year (with the free `opt` year included) in
`contract_end` and `years_remaining`.

### Phase 2 — Multi-year player value — ✅ shipped
**Script:** `scripts/build_team_assets.R` (extended) +
`scripts/fetch_player_birthdates.R` (cache helper)
**Outputs:** extra columns on `team_assets.csv`; `data/processed/player_birthdates.csv`

Each player carries a value stream across 2026 → 2030 plus three derived
aggregates:

* `win_now_value` — in-season production value. **As of v1.1** this is the
  FanGraphs ROS auction-calculator dollar value (`fg_ros_auction_dollars`)
  when available, falling back to the legacy SGP surplus
  (`dollar_value_2026 - salary_2026`, also exposed as `win_now_surplus_sgp`)
  for the ~30% of rostered players FanGraphs has no ROS price for. Salary is
  intentionally NOT subtracted in the FG ROS path: in-season, the current-
  year salary is sunk for whichever team paid it in March; the receiving
  team's win-now gain is the raw rest-of-season auction value of the
  player. This switch fixes the prior over-valuation of closers (Saves is
  a tiny category in our scoring and the SGP standings-value model gave
  RPs implausibly high \$/yr) — closers drop ~3-4x while elite hitters
  and SP stay roughly comparable. The legacy SGP-surplus column is
  retained on every row as `win_now_surplus_sgp` for side-by-side checks.
* `future_value`  — `γ * surplus_2027 + γ² * surplus_2028 + ...` with default
  γ = 0.7. Includes the drop-penalty haircut where applicable. Per-year
  surplus uses the SGP-aged full-season values; switching the future-year
  baseline to FG full-season auction values is a candidate v2 iteration.
* `total_value`   — `win_now_value + future_value`.
* `dashboard_value_2026` / `dashboard_value_source` — the same value the
  Lovable dashboard renders as "Value". Coalesces ROS → FG full-season →
  SGP standings, with the chosen source tagged so the UI can label it.

Aging curve: hold flat ages 25–30, 5%/yr decay 31–33, 10%/yr after 33.
Birthdates come from `player_birthdates.csv`, populated once via the MLB
Stats API by `fetch_player_birthdates.R` and reused on subsequent runs.

Salary path follows the constitution:
* `1` → flat 2026 + 2027 (year2), then `opt` in 2028.
* `2` → flat 2026 (year2), `opt` in 2027.
* `opt` → assume GM extends iff projected discounted surplus > 0; salary
  bumps by +$5 per added year.
* `<year>` → locked at current salary through `<year>`.

### Phase 3 — Team competitive posture — ✅ shipped
**Script:** `scripts/team_posture.R`
**Outputs:** `data/processed/team_posture.csv`,
`data/processed/team_keeper_pressure.csv`
**API:** `GET /team_posture`, `GET /team_keeper_pressure`

`team_posture.csv` has one row per Billiken team with `posture` ∈
{`contender`, `bubble`, `mid`, `rebuild`}, plus `proj_finish`, `proj_total_pts`,
`gap_to_third`, `gap_to_first`, `playoff_prob_top3` (deterministic flag for
now, Monte Carlo TBD), `keeper_cap`, counts of `n_with_positive_surplus`,
`n_extended`, `n_year1or2`, `n_expiring_2026`, and short `priority_buy` /
`priority_sell` label strings.

`team_keeper_pressure.csv` ranks each team's players by `future_value` desc,
flags everyone past the keeper cap as "shed", and surfaces a pipe-separated
list of borderline names for the dashboard card.

### Phase 4 — Draft-pick valuation — ✅ v1 shipped, v2 deferred
**Script:** `scripts/value_draft_picks.R`
**Output:** `data/processed/draft_pick_values.csv`
**API:** `GET /draft_pick_values` (optional `?team=<substring>`)

One row per `(billikenTeam, round)` for next season (`NEXT_YEAR = CURRENT_YEAR
+ 1`). Round 1 picks 8–10 are deterministic (top-3 reverse standings); R1
picks 1–7 are lottery-weighted via 20k-sim Monte Carlo on `cards = finish - 3`
(worst finisher gets 7 cards, 4th gets 1). Rounds 2+ are deterministic reverse
standings.

Placeholder valuation curve (v1):

```
v(p) = 0.5 + 39.5 * exp(-0.04 * (p - 1))
```

Anchors: pick 1 ≈ \$40, pick 50 ≈ \$6, pick 100 ≈ \$1.25, deep picks tail to
\$0.5. `expected_dollar_value` applies this curve to `expected_overall_pick`.

Phase 4 v2 (deferred): replace placeholder curve with one fit to historical
drafts. Plan is in `scripts/value_draft_picks.R` header + the plan doc. Will
pull historical drafts from the Billiken Google Sheet (`Draft 'XX` tabs back
to 2013), compute per-pick SGPAR/dollar surplus realized, and smooth (LOESS
or monotone spline). Output schema is intentionally stable so the matchmaker
doesn't care which curve produced it.

### Phase 5 — Trade matchmaker — ✅ v1.1 shipped (FG ROS values)
**Script:** `scripts/trade_recommendations.R`
**Output:** `data/processed/trade_targets.csv`
**API:** `GET /trade_targets/:my_team?partner=<team>&horizon=win_now|future|balanced`

For each pair `(myTeam, otherTeam)`:

1. Score each of `otherTeam`'s players by my-side fit using posture weights
   (contender 1.0 win-now / 0.3 future, bubble 0.8/0.5, mid 0.6/0.7,
   rebuild 0.2/1.0). Targets are partner *players* only in v1 — picks come
   only from my side as offer currency.
2. For each top-`TOP_N_CANDIDATE_TARGETS=20` candidate, propose minimal
   greedy offers from my tradeable assets (rostered players + next-season
   picks) such that `partner.posture_weighted_value(incoming) ≥ outgoing +
   TRADE_PREMIUM` and `my.posture_weighted_value(incoming) > outgoing`.
3. Pathology guard: assets are pre-filtered with
   `MIN_ASSET_V_TO_PARTNER = 0.5` so the matchmaker can't "dump" underwater
   contracts on the partner. Without this filter, year2/extended players
   whose `v_to_partner` is *less negative* than their `v_to_me` get selected
   first because their `give_arb` is positive even though no real GM would
   accept them.
4. Keep top `TOP_N_PER_PARTNER=5` trades per `(my_team, partner_team)` by
   `my_value_delta`.

Knobs (top of `trade_recommendations.R`): `TRADE_PREMIUM=1.0`,
`MAX_OFFER_SIZE=4`, `MIN_TARGET_VALUE_TO_ME=3.0`,
`MIN_ASSET_V_TO_PARTNER=0.5`, `MIN_OFFER_SIZE=1`.

**v1.1 update:** `MIN_OFFER_SIZE=1` and a floored `need = max(target.v_to_partner + TRADE_PREMIUM, TRADE_PREMIUM)` guard against "free target" trades that
emerged once `win_now_value` switched to FG ROS. When a partner has a star
on a heavily underwater extension their `v_to_partner` can go negative;
without the guard the greedy would propose acquiring them for zero assets.

Two-team trades only in v1. Three-team is a future search-routine change;
the data model is already 3-team-ready.

### Phase 6 — Lovable Trade Lab tab — ⏳ next up
**Backend endpoints:** all current `/team_*` and `/draft_pick_values` plus
the future `/trade_targets/...`.

Planned layout:

1. Header: my team's posture card.
2. League posture grid: 10 cards, color-coded, clickable to filter.
3. Partner roster panel: `team_assets` table sorted by `surplus`, expandable
   to show year-by-year value.
4. My offer builder: drag/select my assets, show running value deltas.
5. Suggested trades panel: top N rows from `trade_targets.csv` for the
   selected partner, one-click "load into builder".

Until the Trade Lab tab ships, the data is still useful — the existing
in-season dashboard can surface posture and contract chips as a stepping
stone.

## Data files (Trade Lab)
| File | Producer | Shape |
|------|----------|-------|
| `data/processed/team_assets.csv`         | `build_team_assets.R` | one row per rostered player; contract, salary, multi-year value, surplus |
| `data/processed/team_posture.csv`        | `team_posture.R`      | one row per Billiken team; posture + projected standings facts |
| `data/processed/team_keeper_pressure.csv`| `team_posture.R`      | one row per team; keeper-cap shed analysis with names |
| `data/processed/draft_pick_values.csv`   | `value_draft_picks.R` | one row per `(team, round)` for next season; expected pick + \$ value |
| `data/processed/trade_targets.csv`       | `trade_recommendations.R` | one row per suggested trade; my team, partner, target, offer, deltas |
| `data/processed/player_birthdates.csv`   | `fetch_player_birthdates.R` | cached MLB Stats API birthdate lookup keyed by name + team |

All five downstream trade artifacts (`team_assets`, `team_posture`,
`team_keeper_pressure`, `draft_pick_values`, `trade_targets`) are rebuilt
every daily run via Step 10 of `scripts/inseason_update.R`.

## API surface (Trade Lab)
| Method | Path | Returns |
|--------|------|---------|
| `GET` | `/team_assets`              | All rostered players league-wide |
| `GET` | `/team_assets/:team`        | One team's players (substring match on `billikenTeam`) |
| `GET` | `/team_posture`             | All 10 team-posture rows |
| `GET` | `/team_keeper_pressure`     | All 10 keeper-pressure rows |
| `GET` | `/draft_pick_values`        | Full next-season pick valuation table |
| `GET` | `/draft_pick_values?team=X` | Same, filtered to one team |
| `GET` | `/trade_targets/:my_team`   | Ranked trade suggestions where `:my_team` initiates |
| `GET` | `/trade_targets/:my_team?partner=X` | Same, filtered to one partner team |

Empty CSV cells deserialize to JSON `null` (not `0`) — the in-server
`csvToJson` was hardened so the Lovable frontend can safely
`String(x).split("|")` on optional fields without first-row breakage. The
parser is now also quote-aware (handles RFC-4180 quoted fields with
embedded commas), which fixed a regression where `proposed_offer` rows
containing `"... overall, $X.Y)"` were shifting every downstream column
right by one position and causing the Lovable frontend to drop entire
trade rows.

## Open knobs (defaults baked in, easy to tune)
| Knob | Default | Where | What it does |
|------|---------|-------|--------------|
| Discount factor γ          | `0.7`              | `build_team_assets.R` | Higher = future-friendly; lower = win-now-friendly |
| Aging breakpoints          | 30 / 33            | `build_team_assets.R` | 5%/yr decay 31–33, 10%/yr 34+ |
| Drop-penalty trigger       | `years_remaining ≥ 2` | `build_team_assets.R` | Can lower to ≥1 if needed |
| Posture thresholds         | gap-to-3rd cutoffs | `team_posture.R`      | Splits `contender`/`bubble`/`mid`/`rebuild` |
| Placeholder pick curve     | `0.5 + 39.5*e^{-0.04(p-1)}` | `value_draft_picks.R` | Phase 4 v2 will replace this with historical fit |
| Lottery sims               | `20000`            | `value_draft_picks.R` | Determines smoothness of expected R1 pick |
| Trade premium              | `1.0`              | `trade_recommendations.R` | How much above v_to_partner the offer must clear to be "acceptable" |
| Max offer size             | `4`                | `trade_recommendations.R` | Greedy stops adding assets once we hit this many |
| Min target v_to_me         | `3.0`              | `trade_recommendations.R` | Don't waste rows on barely-worth-it targets |
| Min asset v_to_partner     | `0.5`              | `trade_recommendations.R` | Drops underwater contracts from the offer pool (dump-asset guard) |
| Min offer size             | `1`                | `trade_recommendations.R` | Every accepted trade must include at least one outgoing asset (free-target guard) |
| Value source preference    | FG ROS > FG full > SGP | `build_team_assets.R` | Coalesce order for `win_now_value` and `dashboard_value_2026` |

## Pending work / next session pickup
1. **Custom offer builder (top next-up).** Add the drag/select UI on my side
   so I can build my own offers against a selected partner, with running
   my-side and partner-side value deltas. The existing suggested trades panel
   should remain as the one-click starting point.
2. **Phase 6 — Lovable Trade Lab tab polish.** Iterate the current tab
   against `/trade_targets/...`, `/team_*`, and `/draft_pick_values`, now
   that the initial visualization exists.
3. **Phase 4 v2 — historical pick curve.** Pull `Draft 'XX` Google Sheet
   tabs (2013–present), match to historical FG/SGPAR, smooth, replace the
   placeholder curve. Output schema stays the same.
4. **Trade matchmaker iteration.** Possible v2 ideas once Lovable is live:
   server-side horizon re-ranking (currently `?horizon=` is informational),
   3-team chains, ILP offer selection, prospect inclusion.
5. **`/inseason_pt_benchmarks` hitter regression.** All hitter rows return
   `null` with `pool_size=0`. Root-cause in `compute_pt_benchmarks()` in
   `scripts/inseason_proration.R`. Non-blocking; pitcher benchmarks are fine.

## Conventions
* All R scripts read/write paths relative to the repo root and respect
  `BILLIKEN_PROJECTIONS_YEAR` (defaults to current year).
* `data/processed/` is `.gitignore`d — those CSVs are regenerated by the
  pipeline, not stored in git. The R scripts that produce them are tracked.
* Commits include a `Co-Authored-By: Oz <oz-agent@warp.dev>` trailer.
* When you change a contract/salary rule or knob default, update both this
  doc and `docs/LEAGUE_RULES.md`.
