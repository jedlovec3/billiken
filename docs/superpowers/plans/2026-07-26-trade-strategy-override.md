# Trade Strategy Override Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let the Trade Lab evaluate custom offers and generated suggestions using a user-selected strategy while retaining the standings-derived posture as the default and as visible context.

**Architecture:** Add a small JavaScript posture helper used by the Railway API to validate overrides and resolve effective weights. Expand the R recommendation pipeline to generate a separate suggestion set for each possible initiating-team strategy while continuing to value the partner using its actual standings posture. Keep existing response fields for backward compatibility and add explicit actual/effective posture fields.

**Tech Stack:** Bun, Hono, JavaScript, R, tidyverse, CSV pipeline artifacts

---

### Task 1: Test and Add Backend Posture Resolution

**Files:**
- Create: `lib/trade_posture.js`
- Create: `tests/trade_posture_test.js`
- Modify: `package.json`

- [ ] **Step 1: Write a failing Node test for valid, auto, and invalid posture overrides**

Create tests that require `resolveEffectivePosture()` to preserve the actual posture for an omitted or `auto` override, accept the four known strategies case-insensitively, and reject unknown values.

- [ ] **Step 2: Run the test and verify it fails because the helper does not exist**

Run: `node --test tests/trade_posture_test.js`
Expected: FAIL with a missing-module error for `lib/trade_posture.js`.

- [ ] **Step 3: Implement the posture helper**

Export `POSTURE_WEIGHTS`, `resolveEffectivePosture(actualPosture, override)`, and `postureWeights(posture)`. The resolver returns `{ actualPosture, effectivePosture, overrideApplied }` and throws a descriptive error for unknown explicit strategies.

- [ ] **Step 4: Run the test and verify it passes**

Run: `node --test tests/trade_posture_test.js`
Expected: all posture-resolution tests PASS.

### Task 2: Test and Add Pipeline Strategy Expansion

**Files:**
- Modify: `scripts/prospect_value_utils.R`
- Modify: `scripts/trade_recommendations.R`
- Modify: `tests/trade_lab_future_assets_test.R`

- [ ] **Step 1: Write a failing R test for effective strategy expansion**

Require `build_effective_team_weights()` to produce four rows per team, preserve `actual_posture`, expose `effective_posture`, and use the rebuild weights `0.0/1.0`.

- [ ] **Step 2: Run the R test and verify it fails because the helper does not exist**

Run: `RENV_CONFIG_AUTOLOADER_ENABLED=false Rscript tests/trade_lab_future_assets_test.R`
Expected: FAIL because `build_effective_team_weights()` is unavailable.

- [ ] **Step 3: Implement strategy expansion and use it in recommendation generation**

Add the pure helper to `prospect_value_utils.R`. In `trade_recommendations.R`, loop through all effective strategies for the initiating team, leave partner weights tied to the partner's actual posture, and write `my_actual_posture`, `my_effective_posture`, and backward-compatible `my_posture`.

- [ ] **Step 4: Keep top suggestions distinct by effective strategy**

Group and rank output by `(my_team, my_effective_posture, partner_team)` so each strategy receives its own top suggestions.

- [ ] **Step 5: Run the R test and verify it passes**

Run: `RENV_CONFIG_AUTOLOADER_ENABLED=false Rscript tests/trade_lab_future_assets_test.R`
Expected: all tests PASS.

### Task 3: Wire Strategy Overrides into Railway API

**Files:**
- Modify: `server.js`

- [ ] **Step 1: Use the posture helper for manual trade evaluation**

Accept optional `my_posture_override`, return HTTP 400 for an invalid value, calculate the initiating team's weighted values using the effective posture, and return `my_actual_posture`, `my_effective_posture`, `my_override_applied`, and `my_weights`. Preserve `my_posture` as the effective posture for compatibility.

- [ ] **Step 2: Filter generated suggestions by selected strategy**

Accept `stance=auto|contender|bubble|mid|rebuild` on `GET /trade_targets/:my_team`. Resolve `auto` to the team's actual posture, filter rows by `my_effective_posture`, and return actual/effective posture metadata.

- [ ] **Step 3: Verify server syntax and helper tests**

Run: `node --check server.js`
Expected: exit 0.

Run: `node --test tests/trade_posture_test.js`
Expected: all tests PASS.

### Task 4: Document the Lovable API Contract

**Files:**
- Modify: `docs/TRADE_LAB.md`
- Modify: `docs/INSEASON_MONITOR.md`

- [ ] **Step 1: Document the strategy parameter and response fields**

Add the `stance` query parameter, `my_posture_override` request field, actual/effective posture response fields, and the rule that partner posture remains standings-derived.

- [ ] **Step 2: Document deployment behavior**

State that no Railway variable is required and that one in-season update must run after deployment to rebuild stance-specific suggestions.

### Task 5: Full Verification

**Files:**
- Verify all modified files

- [ ] **Step 1: Run JavaScript tests and syntax checks**

Run: `node --test tests/trade_posture_test.js && node --check server.js`
Expected: exit 0 with all tests passing.

- [ ] **Step 2: Run R tests and parse checks**

Run: `RENV_CONFIG_AUTOLOADER_ENABLED=false Rscript tests/trade_lab_future_assets_test.R`
Expected: all tests PASS.

Run: `RENV_CONFIG_AUTOLOADER_ENABLED=false Rscript -e 'invisible(lapply(c("scripts/prospect_value_utils.R","scripts/trade_recommendations.R"), parse)); cat("R parse ok\n")'`
Expected: `R parse ok`.

- [ ] **Step 3: Inspect the final diff**

Run: `git diff --check && git status --short`
Expected: no whitespace errors; only intended files plus the pre-existing untracked recreation document are listed.
