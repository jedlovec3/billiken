# team_posture.R
#
# Phase 3 of the trade-analysis tooling. Reads the projected end-of-season
# standings produced by `scripts/inseason_update.R` and classifies each
# Billiken team into a "posture" bucket that drives the trade matchmaker
# (contender vs bubble vs mid vs rebuild).
#
# Output: data/processed/team_posture.csv with one row per team and the
# fields required by the planned `/team_posture` API + Lovable cards.
#
# Run:
#   Rscript scripts/team_posture.R

suppressPackageStartupMessages({
  library(tidyverse)
})

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

CURRENT_YEAR <- as.integer(Sys.getenv("BILLIKEN_PROJECTIONS_YEAR",
                                      unset = format(Sys.Date(), "%Y")))

# Maximum point gap from the projected #3 team that still qualifies a
# 4th/5th-place team as a "bubble" contender (i.e. close enough to plausibly
# push for the top 3 with a trade).
BUBBLE_MAX_GAP <- 15

# Keeper allotments per the constitution (\u00a74.2): 1st=10, 2nd=11, 3rd=12,
# everyone else = 15.
keeper_cap_for_finish <- function(finish) {
  case_when(
    finish == 1L ~ 10L,
    finish == 2L ~ 11L,
    finish == 3L ~ 12L,
    TRUE         ~ 15L
  )
}

# Posture classification.
#
# - contender: projected top 3 (in the money this year)
# - bubble:    projected 4th/5th and within BUBBLE_MAX_GAP of 3rd
# - rebuild:   projected bottom 3 (8th-10th)
# - mid:       everyone else (typically 5th\u20137th, not close enough to push)
classify_posture <- function(proj_finish, gap_to_third) {
  case_when(
    proj_finish <= 3L                                                   ~ "contender",
    proj_finish %in% c(4L, 5L) & gap_to_third <= BUBBLE_MAX_GAP        ~ "bubble",
    proj_finish >= 8L                                                   ~ "rebuild",
    TRUE                                                                ~ "mid"
  )
}

# Short prose labels surfaced on the Lovable posture card. Kept terse so
# they fit on a small chip / sub-label.
priority_buy_label <- function(posture) {
  case_when(
    posture == "contender" ~ "expiring 2026 contracts; win-now bats/arms",
    posture == "bubble"    ~ "expiring 2026 contracts if pushing for top 3",
    posture == "mid"       ~ "value bargains; bounce-back candidates",
    posture == "rebuild"   ~ "prospects; year-1/2 contracts; long-term cheap deals; 2027 picks",
    TRUE                   ~ NA_character_
  )
}

priority_sell_label <- function(posture) {
  case_when(
    posture == "contender" ~ "prospects; 2027 picks; year-1/2 contracts",
    posture == "bubble"    ~ "expiring 2026 contracts if resetting",
    posture == "mid"       ~ "veterans on hot streaks at peak value",
    posture == "rebuild"   ~ "expiring 2026 contracts; veterans",
    TRUE                   ~ NA_character_
  )
}

normalize_team <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    str_to_upper()
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

standings_path <- resolve_path("data/processed/inseason_projected_standings.csv")
if (!file.exists(standings_path)) {
  stop(sprintf("Missing %s; run scripts/inseason_update.R first.", standings_path),
       call. = FALSE)
}
standings <- read_csv(standings_path, show_col_types = FALSE)

# Optional: team_assets.csv lets us compute next-year keeper-cap pressure
# (how many keepers a contender would have to shed). Surplus_2026 is a
# reasonable v1 stand-in for "would be kept" pending Phase 2's multi-year
# value model.
team_assets_path <- resolve_path("data/processed/team_assets.csv")
team_assets <- if (file.exists(team_assets_path)) {
  read_csv(team_assets_path, show_col_types = FALSE)
} else {
  tibble()
}

# ---------------------------------------------------------------------------
# Compute posture
# ---------------------------------------------------------------------------

# Pull the projected #3 team's total pts to anchor the contention gap.
top3_pts <- standings %>%
  filter(projected_finish == 3) %>%
  pull(total_pts) %>%
  first()

top1_pts <- standings %>%
  filter(projected_finish == 1) %>%
  pull(total_pts) %>%
  first()

if (length(top3_pts) == 0 || is.na(top3_pts)) {
  warning("Could not determine projected #3 total_pts; gap_to_third will be NA.")
  top3_pts <- NA_real_
}

posture <- standings %>%
  transmute(
    team_id,
    team_name      = str_squish(team_name),
    billikenTeam   = normalize_team(team_name),
    proj_finish    = as.integer(projected_finish),
    proj_total_pts = total_pts,
    gap_to_third   = top3_pts - total_pts,
    gap_to_first   = top1_pts - total_pts
  ) %>%
  mutate(
    keeper_cap          = keeper_cap_for_finish(proj_finish),
    keepers_to_shed_min = pmax(0L, 23L - keeper_cap), # naive floor; refined below
    posture             = classify_posture(proj_finish, gap_to_third),
    # Deterministic flag for v1; Phase 3 v2 will replace with a Monte Carlo
    # probability of finishing top 3.
    playoff_prob_top3   = ifelse(proj_finish <= 3L, 1.0, 0.0),
    priority_buy        = priority_buy_label(posture),
    priority_sell       = priority_sell_label(posture)
  )

# ---------------------------------------------------------------------------
# Keeper-cap pressure: how many keepers each team would have to drop
# next offseason to fit under their cap.
#
# v1 proxy: count the number of rostered players with positive
# `surplus_2026` (i.e. dollar value > salary). Phase 2 will replace this
# with multi-year `future_value` once that's available.
# ---------------------------------------------------------------------------

if (nrow(team_assets) > 0) {
  worth_keeping <- team_assets %>%
    mutate(billikenTeam = normalize_team(billikenTeam)) %>%
    group_by(billikenTeam) %>%
    summarise(
      n_with_positive_surplus = sum(surplus_2026 > 0, na.rm = TRUE),
      n_extended              = sum(contract_status == "extended", na.rm = TRUE),
      n_year1or2              = sum(contract_status %in% c("year1", "year2"),
                                    na.rm = TRUE),
      n_expiring_2026         = sum(is_expiring_after_2026, na.rm = TRUE),
      .groups = "drop"
    )

  posture <- posture %>%
    left_join(worth_keeping, by = "billikenTeam") %>%
    mutate(
      keepers_above_cap = pmax(0L,
                               replace_na(n_with_positive_surplus, 0L) - keeper_cap)
    ) %>%
    select(-keepers_to_shed_min)
} else {
  message("team_assets.csv not found; skipping keeper-pressure metrics.")
  posture <- posture %>%
    mutate(
      n_with_positive_surplus = NA_integer_,
      n_extended              = NA_integer_,
      n_year1or2              = NA_integer_,
      n_expiring_2026         = NA_integer_,
      keepers_above_cap       = NA_integer_
    ) %>%
    select(-keepers_to_shed_min)
}

# ---------------------------------------------------------------------------
# Final shape
# ---------------------------------------------------------------------------

team_posture <- posture %>%
  select(
    team_id,
    team_name,
    billikenTeam,
    posture,
    proj_finish,
    proj_total_pts,
    gap_to_third,
    gap_to_first,
    playoff_prob_top3,
    keeper_cap,
    keepers_above_cap,
    n_with_positive_surplus,
    n_extended,
    n_year1or2,
    n_expiring_2026,
    priority_buy,
    priority_sell
  ) %>%
  arrange(proj_finish)

out_path <- resolve_path("data/processed/team_posture.csv")
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write_csv(team_posture, out_path)

# ---------------------------------------------------------------------------
# Console summary
# ---------------------------------------------------------------------------

message(sprintf("Wrote %s", out_path))
message(sprintf("Posture distribution:"))
team_posture %>%
  count(posture) %>%
  pwalk(~ message(sprintf("  %-9s : %d", ..1, ..2)))

message("\nPer-team summary:")
team_posture %>%
  transmute(
    line = sprintf(
      "  %2d. %-22s  %-9s  pts=%5.1f  gap3=%+5.1f  cap=%2d  abv=%2d  exp=%2d",
      proj_finish, team_name, posture, proj_total_pts,
      gap_to_third, keeper_cap,
      replace_na(keepers_above_cap, 0L),
      replace_na(n_expiring_2026, 0L)
    )
  ) %>%
  pull(line) %>%
  walk(message)
