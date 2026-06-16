# Trade Lab Prospect and Pick Valuation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add repeatable prospect/future-asset valuation to the Billiken Trade Lab and make rebuild recommendations target picks and prospects.

**Architecture:** Put pure valuation logic in a reusable R helper, add fetch/build scripts that produce cached CSV artifacts, join prospect values into `team_assets.csv`, and extend the existing trade asset universe so picks and prospects can be acquisition targets. Keep Railway/Lovable contracts additive and backward-compatible.

**Tech Stack:** R scripts with `tidyverse`, `httr2`, `jsonlite`; Hono/Bun API in `server.js`; standalone `Rscript` tests with fixtures.

---

### Task 1: Prospect Valuation Helpers

**Files:**
- Create: `scripts/prospect_value_utils.R`
- Create: `tests/trade_lab_future_assets_test.R`

- [ ] Write failing tests for ETA discount, rank blending, FV fallback, and prospect value hierarchy.
- [ ] Run: `RENV_CONFIG_AUTOLOADER_ENABLED=false Rscript tests/trade_lab_future_assets_test.R`
- [ ] Implement `eta_multiplier()`, `prospect_rank_value()`, `fv_to_rank()`, `risk_multiplier()`, `build_consensus_prospect_values()`.
- [ ] Rerun the test and confirm it passes.

### Task 2: FanGraphs Auction Output Fix

**Files:**
- Modify: `scripts/download_fangraphs_auction_values.R`
- Test: `tests/trade_lab_future_assets_test.R`

- [ ] Add a failing test that `auction_output_path(2026, "rfangraphsdc", "")` returns `data/raw/auction_values_ros_2026.csv`.
- [ ] Implement `auction_projection_type()` and `auction_output_path()`.
- [ ] Update the request query to use `FANGRAPHS_AUCTION_PROJ`.
- [ ] Update output path logic to respect `FANGRAPHS_AUCTION_OUTFILE`.
- [ ] Rerun the test and parse check.

### Task 3: Future Projection Downloader

**Files:**
- Create: `scripts/download_future_projections.R`
- Test: `tests/trade_lab_future_assets_test.R`

- [ ] Add a failing test for `future_projection_specs(2026)` returning 2027/2028 hitter and pitcher specs with `zipsp1`/`zipsp2`.
- [ ] Implement the spec helper and downloader using the existing FanGraphs projection request pattern.
- [ ] Ensure the script writes `future_hitter_projections_2027.csv`, `future_pitcher_projections_2027.csv`, `future_hitter_projections_2028.csv`, and `future_pitcher_projections_2028.csv`.
- [ ] Rerun tests and parse check.

### Task 4: Prospect Rankings Downloader

**Files:**
- Create: `scripts/download_prospect_rankings.R`
- Test: `tests/trade_lab_future_assets_test.R`

- [ ] Add a failing test for parsing representative MLB embedded prospect payload HTML into a tibble with `Name`, `source_rank`, `mlb_org`, `position`, `level`, and `eta`.
- [ ] Implement `extract_mlb_prospects_from_html()` with HTML entity unescaping and JSON extraction.
- [ ] Implement a FanGraphs CSV/env fallback fetch path that writes a warning when unavailable.
- [ ] Write latest and timestamped raw CSV snapshots.
- [ ] Rerun tests and parse check.

### Task 5: Build Prospect Values

**Files:**
- Create: `scripts/build_prospect_values.R`
- Test: `tests/trade_lab_future_assets_test.R`

- [ ] Add a failing test for building `prospect_values.csv` from MLB-only and FanGraphs+MLB fixture rows.
- [ ] Implement file reads, future projection joins, consensus valuation, and empty-output safety.
- [ ] Write `data/processed/prospect_values.csv`.
- [ ] Rerun tests and parse check.

### Task 6: Team Assets Integration

**Files:**
- Modify: `scripts/build_team_assets.R`
- Test: `tests/trade_lab_future_assets_test.R`

- [ ] Add a failing test for future value calculation including prospect value and subtracting drop-penalty liability.
- [ ] Implement pure helper `calculate_future_asset_value()` in `prospect_value_utils.R`.
- [ ] Join `prospect_values.csv` into `team_assets.csv` by normalized name.
- [ ] Add prospect columns to final output.
- [ ] Rerun tests and parse check.

### Task 7: Trade Matchmaker Future Targets

**Files:**
- Modify: `scripts/trade_recommendations.R`
- Test: `tests/trade_lab_future_assets_test.R`

- [ ] Add a failing test that rebuild target selection can return a `pick` target and a high-value prospect target.
- [ ] Move target selection logic into a pure helper in `prospect_value_utils.R`.
- [ ] Extend `trade_recommendations.R` asset universe with `asset_type`, `prospect_value`, `pick_value`, and labels.
- [ ] Allow rebuild target selection across players, picks, and prospects.
- [ ] Rerun tests and parse check.

### Task 8: Pipeline and API Contract

**Files:**
- Modify: `scripts/inseason_update.R`
- Modify: `server.js`
- Modify: `docs/TRADE_LAB.md`
- Modify: `docs/INSEASON_MONITOR.md`

- [ ] Add prospect/future scripts to Step 10 before `build_team_assets.R`.
- [ ] Add `GET /prospect_values`.
- [ ] Include new generated files in docs and status warnings.
- [ ] Run `node --check server.js`.
- [ ] Run full R parse check for all modified scripts.

### Task 9: Final Verification

**Files:**
- All modified files.

- [ ] Run: `RENV_CONFIG_AUTOLOADER_ENABLED=false Rscript tests/trade_lab_future_assets_test.R`
- [ ] Run: `node --check server.js`
- [ ] Run: `RENV_CONFIG_AUTOLOADER_ENABLED=false Rscript -e 'invisible(lapply(c("scripts/prospect_value_utils.R","scripts/download_prospect_rankings.R","scripts/download_future_projections.R","scripts/download_fangraphs_auction_values.R","scripts/build_prospect_values.R","scripts/build_team_assets.R","scripts/trade_recommendations.R","scripts/inseason_update.R"), parse)); cat("R parse ok\n")'`
- [ ] Run: `git diff --check`
- [ ] Summarize Railway and Lovable handoff commands.
