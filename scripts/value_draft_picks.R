# value_draft_picks.R
#
# Phase 4 (v1, placeholder curve) of the trade-analysis tooling.
#
# For each Billiken team, projects next season's draft picks and assigns
# a placeholder dollar value to each pick by overall pick number. Output
# is `data/processed/draft_pick_values.csv` with one row per (team,
# round) for the upcoming season.
#
# How pick slots are assigned (per the constitution):
#
#   * Round 1, picks 8/9/10:
#       Deterministic. The prior 1st/2nd/3rd-place teams get picks
#       10/9/8 of round 1 in reverse standings order (3rd -> 8, 2nd ->
#       9, 1st -> 10).
#
#   * Round 1, picks 1-7:
#       Lottery. The bottom-7 finishers each get a number of "cards"
#       equal to (finish - 3), so 4th place gets 1 card, 5th gets 2,
#       ..., 10th gets 7 cards (28 cards total). The worst finisher
#       gets the most cards (best lottery odds). Cards are drawn one
#       at a time; the first card whose owner has not yet been awarded
#       a pick wins the next slot. We Monte-Carlo this to get each
#       team's expected R1 pick number.
#
#       Note: the constitution lists card buckets for finishes 4-11
#       (an artifact of past league sizes), with cards = finish - 3.
#       In a 10-team year only 4-10 get cards. We use the same
#       formula, total 28 cards.
#
#   * Rounds 2+:
#       Deterministic reverse standings. With N=10 teams, the team
#       finishing last (10th) picks first in every round, the prior
#       1st-place team picks last. Overall pick number for round R
#       (R >= 2) and finish F is:
#           overall_pick = (R - 1) * N + (N + 1 - F)
#
# Placeholder valuation curve (v1):
#
#   v(p) = c + a * exp(-b * (p - 1))
#
# with anchors:
#   v(1)   = ~$40   (peak draft target)
#   v(50)  = ~$6
#   v(100) = ~$1.25
#   v(230) = ~$0.5  (replacement-level asymptote)
#
# Phase 4 v2 will replace this curve with a historical-draft-derived
# curve. The output schema is intentionally stable so the matchmaker
# can treat picks as just another asset regardless of which curve is
# in use.
#
# Run:
#   Rscript scripts/value_draft_picks.R

suppressPackageStartupMessages({
  library(tidyverse)
})

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

CURRENT_YEAR <- as.integer(Sys.getenv("BILLIKEN_PROJECTIONS_YEAR",
                                      unset = format(Sys.Date(), "%Y")))
NEXT_YEAR <- CURRENT_YEAR + 1L
N_TEAMS   <- 10L
N_ROUNDS  <- 23L
N_SIMS    <- 20000L  # lottery Monte Carlo iterations
SET_SEED  <- 42L

# Placeholder curve parameters.
PICK_VALUE_A <- 39.5
PICK_VALUE_B <- 0.04
PICK_VALUE_C <- 0.5

placeholder_pick_value <- function(overall_pick) {
  PICK_VALUE_C + PICK_VALUE_A * exp(-PICK_VALUE_B * (overall_pick - 1))
}

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

repo_root <- if (file.exists("billiken.Rproj")) {
  getwd()
} else if (file.exists("../billiken.Rproj")) {
  normalizePath("..")
} else {
  getwd()
}

resolve_path <- function(p) file.path(repo_root, p)

posture_path <- resolve_path("data/processed/team_posture.csv")
if (!file.exists(posture_path)) {
  stop(sprintf("Missing %s; run scripts/team_posture.R first.", posture_path),
       call. = FALSE)
}
posture <- read_csv(posture_path, show_col_types = FALSE)

# ---------------------------------------------------------------------------
# Lottery Monte Carlo
# ---------------------------------------------------------------------------

# Returns a tibble with `finish` (4..10) and the expected R1 pick number
# (1..7) for that finishing position based on the card-draw lottery.
simulate_lottery <- function(n_sims = N_SIMS, seed = SET_SEED) {
  set.seed(seed)

  finishes <- 4:10
  # cards = finish - 3, so worst finisher (10) gets 7 cards (best odds) and
  # 4th gets 1 card. 1+2+3+4+5+6+7 = 28 cards total.
  cards_per_finish <- finishes - 3L
  deck <- rep(finishes, times = cards_per_finish)
  n_lottery_picks <- length(finishes)

  # pick_counts[i, j] = number of times finish (3+i) won lottery pick j.
  pick_counts <- matrix(
    0L,
    nrow = length(finishes),
    ncol = n_lottery_picks,
    dimnames = list(as.character(finishes), seq_len(n_lottery_picks))
  )

  for (sim in seq_len(n_sims)) {
    shuffled <- sample(deck)
    seen <- integer(0)
    pick_idx <- 1L
    for (card in shuffled) {
      if (!card %in% seen) {
        seen <- c(seen, card)
        pick_counts[as.character(card), pick_idx] <-
          pick_counts[as.character(card), pick_idx] + 1L
        pick_idx <- pick_idx + 1L
        if (pick_idx > n_lottery_picks) break
      }
    }
  }

  pick_probs <- pick_counts / n_sims

  # Per-finish expected pick number.
  expected_pick <- as.numeric(
    pick_probs %*% seq_len(n_lottery_picks)
  )

  tibble(
    finish        = finishes,
    expected_pick = expected_pick,
    # Also stash the per-slot probabilities as a list-column for callers
    # that want full granularity (e.g. lottery-aware Phase 5 search).
    pick_probs    = lapply(seq_len(nrow(pick_probs)),
                           function(i) pick_probs[i, ])
  )
}

lottery <- simulate_lottery()

# ---------------------------------------------------------------------------
# Build the per-team-per-round pick table
# ---------------------------------------------------------------------------

team_finish <- posture %>%
  transmute(
    billikenTeam,
    team_name,
    proj_finish = as.integer(proj_finish)
  )

# For each (team, round) compute expected_overall_pick and lottery flag.
build_rows <- function() {
  rows <- list()

  for (i in seq_len(nrow(team_finish))) {
    team <- team_finish[i, ]
    finish <- team$proj_finish

    for (rnd in seq_len(N_ROUNDS)) {
      if (rnd == 1L) {
        if (finish <= 3L) {
          # Top 3 picks 8, 9, 10 of round 1 in reverse standings order.
          pick_in_round <- as.numeric(N_TEAMS + 1L - finish)  # 1->10, 2->9, 3->8
          overall_pick  <- pick_in_round
          lottery_weighted <- FALSE
        } else {
          row_idx <- which(lottery$finish == finish)
          if (length(row_idx) == 0) {
            pick_in_round <- NA_real_
          } else {
            pick_in_round <- lottery$expected_pick[row_idx]
          }
          overall_pick     <- pick_in_round
          lottery_weighted <- TRUE
        }
      } else {
        # Rounds 2+: reverse standings, last finisher picks first.
        pick_in_round <- as.numeric(N_TEAMS + 1L - finish)
        overall_pick  <- (rnd - 1L) * N_TEAMS + pick_in_round
        lottery_weighted <- FALSE
      }

      rows[[length(rows) + 1L]] <- tibble(
        billikenTeam        = team$billikenTeam,
        team_name           = team$team_name,
        season              = NEXT_YEAR,
        round               = as.integer(rnd),
        expected_pick_in_round = pick_in_round,
        expected_overall_pick  = overall_pick,
        lottery_weighted    = lottery_weighted,
        expected_dollar_value = placeholder_pick_value(overall_pick),
        curve_source        = "placeholder_v1"
      )
    }
  }

  bind_rows(rows)
}

picks <- build_rows() %>%
  arrange(billikenTeam, round)

# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------

out_path <- resolve_path("data/processed/draft_pick_values.csv")
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write_csv(picks, out_path)

# ---------------------------------------------------------------------------
# Console summary
# ---------------------------------------------------------------------------

message(sprintf("Wrote %s", out_path))
message(sprintf("  rows: %d (%d teams x %d rounds)",
                nrow(picks), N_TEAMS, N_ROUNDS))

message("\nLottery expected picks for finishes 4-10:")
lottery %>%
  transmute(
    line = sprintf("  finish %2d -> expected R1 pick %.2f",
                   finish, expected_pick)
  ) %>%
  pull(line) %>%
  walk(message)

message("\nFirst-round expected picks per team:")
picks %>%
  filter(round == 1L) %>%
  arrange(expected_overall_pick) %>%
  transmute(
    line = sprintf("  pick %5.2f  $%5.2f  %-22s  (lottery=%s)",
                   expected_overall_pick,
                   expected_dollar_value,
                   team_name,
                   lottery_weighted)
  ) %>%
  pull(line) %>%
  walk(message)

message("\nPlaceholder curve sample:")
sample_picks <- c(1, 5, 10, 20, 50, 100, 150, 230)
for (p in sample_picks) {
  message(sprintf("  pick %3d -> $%5.2f", p, placeholder_pick_value(p)))
}
