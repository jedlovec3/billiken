#!/usr/bin/env Rscript
# scripts/inseason_update.R
# Daily in-season update: project end-of-season roto standings.
#
# Combines:
#   1. ESPN season-to-date team stats (from current-year standings)
#   2. ESPN current fantasy rosters (which players on which teams)
#   3. FanGraphs rest-of-season projections (per player)
#
# Outputs:
#   data/processed/inseason_projected_standings.csv
#   data/processed/inseason_team_details.csv
#   data/processed/inseason_status.json

# --- Working directory ---
if (dir.exists("/app") && file.exists("/app/billiken.Rproj")) {
  setwd("/app")
} else if (file.exists("billiken.Rproj")) {
  # Already in project root
} else if (file.exists("scripts/paths.R")) {
  source("scripts/paths.R")
  setwd(find_project_root())
}

if (file.exists("renv/activate.R")) source("renv/activate.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringi)
  library(fuzzyjoin)
  library(jsonlite)
})

# =====================================================================
# Helpers
# =====================================================================

write_status <- function(status, error_message = NULL, warnings = NULL,
                         data_date = Sys.Date()) {
  out <- list(
    last_updated  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status        = status,
    data_date     = as.character(data_date)
  )
  if (!is.null(error_message)) out$error_message <- error_message
  if (!is.null(warnings) && length(warnings) > 0) out$warnings <- warnings
  dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
  write_json(out, "data/processed/inseason_status.json",
             auto_unbox = TRUE, pretty = TRUE)
}

normalize_name <- function(name) {
  name %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all(" Jr\\.?$", "") %>%
    str_replace_all(" Sr\\.?$", "") %>%
    str_replace_all(" III$",    "") %>%
    str_replace_all(" II$",     "") %>%
    str_trim()
}

# NL teams (Billiken league is NL-only)
NL_TEAMS <- c("ATL","LAD","SDP","ARI","NYM","PHI","MIL","STL",
              "CHC","SFG","CIN","COL","PIT","MIA","WSN", NA)

# =====================================================================
# Parameters
# =====================================================================
current_year <- as.integer(format(Sys.Date(), "%Y"))
N_TEAMS      <- 10
pipeline_warnings <- character(0)

# =====================================================================
# Source function definitions (local envs prevent auto-run blocks)
# =====================================================================
message("Loading function definitions...")

# fetch_espn_standings.R has an auto-run guard that checks for "R_GlobalEnv".
# Sourcing into a named env prevents that guard from firing.
.standings_env <- new.env(parent = globalenv())
source("scripts/fetch_espn_standings.R", local = .standings_env)
fetch_espn_standings <- .standings_env$fetch_espn_standings

source("scripts/fetch_espn_rosters.R")
source("scripts/download_ros_projections.R")

# =====================================================================
# Main pipeline (wrapped in error handler)
# =====================================================================
tryCatch({

  # ================================================================
  # STEP 1 — Fetch current-year ESPN standings (YTD stats)
  # ================================================================
  message("\n=== Step 1: Fetching ESPN standings for ", current_year, " ===")
  ytd_standings <- fetch_espn_standings(current_year, verbose = TRUE)

  message(sprintf("Got standings for %d teams", nrow(ytd_standings)))

  # Validate required columns
  required_cols <- c("team_name", "team_id",
                     "R", "HR", "RBI", "SB", "H", "AB",
                     "W", "SV", "SO", "IP", "ER", "BB", "HA")
  missing <- setdiff(required_cols, names(ytd_standings))
  if (length(missing) > 0) {
    stop(sprintf("ESPN standings missing columns: %s",
                 paste(missing, collapse = ", ")))
  }

  # ================================================================
  # STEP 2 — Fetch current ESPN rosters
  # ================================================================
  message("\n=== Step 2: Fetching ESPN rosters ===")
  rosters <- fetch_espn_rosters(season = current_year, verbose = TRUE)
  message(sprintf("Got %d roster entries across %d teams",
                  nrow(rosters), n_distinct(rosters$team_name)))

  # ================================================================
  # STEP 3 — Download FanGraphs ROS projections
  # ================================================================
  message("\n=== Step 3: Downloading FanGraphs ROS projections ===")
  ros <- download_ros_projections(projection_year = as.character(current_year))

  ros_hitters  <- ros$hitters
  ros_pitchers <- ros$pitchers

  # Filter to NL teams
  if ("Team" %in% names(ros_hitters)) {
    ros_hitters <- ros_hitters %>% filter(Team %in% NL_TEAMS)
  }
  if ("Team" %in% names(ros_pitchers)) {
    ros_pitchers <- ros_pitchers %>% filter(Team %in% NL_TEAMS)
  }

  message(sprintf("ROS projections: %d hitters, %d pitchers (NL only)",
                  nrow(ros_hitters), nrow(ros_pitchers)))

  # Normalize names for matching
  ros_hitters <- ros_hitters %>%
    mutate(name_normalized = normalize_name(Name))
  ros_pitchers <- ros_pitchers %>%
    mutate(name_normalized = normalize_name(Name))

  # Standardise strikeout column (FanGraphs uses K; ESPN uses SO)
  if ("K" %in% names(ros_pitchers) && !"SO" %in% names(ros_pitchers)) {
    ros_pitchers <- ros_pitchers %>% rename(SO = K)
  }

  # Ensure ER exists for pitchers (compute from ERA*IP/9 if missing)
  if (!"ER" %in% names(ros_pitchers) &&
      "ERA" %in% names(ros_pitchers) && "IP" %in% names(ros_pitchers)) {
    ros_pitchers <- ros_pitchers %>% mutate(ER = ERA * IP / 9)
  }

  # Rename pitcher H -> HA (hits allowed) to avoid confusion with hitter H
  if (!"HA" %in% names(ros_pitchers) && "H" %in% names(ros_pitchers)) {
    ros_pitchers <- ros_pitchers %>% rename(HA = H)
  }

  # ================================================================
  # STEP 4 — Join rosters to ROS projections
  # ================================================================
  message("\n=== Step 4: Joining rosters to projections ===")

  roster_norm <- rosters %>%
    mutate(name_normalized = normalize_name(player_name))

  # --- Hitters ---
  hitter_proj_cols <- intersect(
    c("name_normalized", "Name", "Team", "AB", "H", "R", "HR", "RBI", "SB"),
    names(ros_hitters)
  )

  hitter_exact <- roster_norm %>%
    inner_join(
      ros_hitters %>% select(all_of(hitter_proj_cols)),
      by = "name_normalized",
      relationship = "many-to-many"
    ) %>%
    group_by(team_name, player_name) %>%
    slice(1) %>%
    ungroup()

  hitter_unmatched <- roster_norm %>%
    anti_join(ros_hitters, by = "name_normalized")

  if (nrow(hitter_unmatched) > 0 && nrow(ros_hitters) > 0) {
    hitter_fuzzy <- hitter_unmatched %>%
      stringdist_left_join(
        ros_hitters %>% select(all_of(hitter_proj_cols)),
        by = "name_normalized",
        max_dist = 2,
        distance_col = "dist"
      ) %>%
      filter(!is.na(Name)) %>%
      group_by(team_name, player_name) %>%
      slice_min(dist, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(-dist) %>%
      rename(name_normalized = name_normalized.x) %>%
      select(-name_normalized.y)

    roster_hitters <- bind_rows(hitter_exact, hitter_fuzzy)
  } else {
    roster_hitters <- hitter_exact
  }

  message(sprintf("Matched %d roster entries to hitter projections",
                  nrow(roster_hitters)))

  # --- Pitchers ---
  pitcher_proj_cols <- intersect(
    c("name_normalized", "Name", "Team", "IP", "W", "SV", "SO", "ER", "BB", "HA"),
    names(ros_pitchers)
  )

  pitcher_exact <- roster_norm %>%
    inner_join(
      ros_pitchers %>% select(all_of(pitcher_proj_cols)),
      by = "name_normalized",
      relationship = "many-to-many"
    ) %>%
    group_by(team_name, player_name) %>%
    slice(1) %>%
    ungroup()

  pitcher_unmatched <- roster_norm %>%
    anti_join(ros_pitchers, by = "name_normalized")

  if (nrow(pitcher_unmatched) > 0 && nrow(ros_pitchers) > 0) {
    pitcher_fuzzy <- pitcher_unmatched %>%
      stringdist_left_join(
        ros_pitchers %>% select(all_of(pitcher_proj_cols)),
        by = "name_normalized",
        max_dist = 2,
        distance_col = "dist"
      ) %>%
      filter(!is.na(Name)) %>%
      group_by(team_name, player_name) %>%
      slice_min(dist, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(-dist) %>%
      rename(name_normalized = name_normalized.x) %>%
      select(-name_normalized.y)

    roster_pitchers <- bind_rows(pitcher_exact, pitcher_fuzzy)
  } else {
    roster_pitchers <- pitcher_exact
  }

  message(sprintf("Matched %d roster entries to pitcher projections",
                  nrow(roster_pitchers)))

  # Warn about teams with low match counts
  hitter_counts <- roster_hitters %>% count(team_name, name = "n_h")
  pitcher_counts <- roster_pitchers %>% count(team_name, name = "n_p")
  low_match <- hitter_counts %>%
    full_join(pitcher_counts, by = "team_name") %>%
    replace_na(list(n_h = 0, n_p = 0)) %>%
    filter(n_h < 5 | n_p < 3)
  if (nrow(low_match) > 0) {
    for (i in seq_len(nrow(low_match))) {
      w <- sprintf("Low match count for %s: %d hitters, %d pitchers",
                   low_match$team_name[i], low_match$n_h[i], low_match$n_p[i])
      message("WARNING: ", w)
      pipeline_warnings <- c(pipeline_warnings, w)
    }
  }

  # ================================================================
  # STEP 5 — Aggregate ROS stats by fantasy team
  # ================================================================
  message("\n=== Step 5: Aggregating ROS stats by team ===")

  hitter_ros_by_team <- roster_hitters %>%
    group_by(team_id, team_name) %>%
    summarize(
      ros_AB    = sum(AB, na.rm = TRUE),
      ros_H_bat = sum(H, na.rm = TRUE),
      ros_R     = sum(R, na.rm = TRUE),
      ros_HR    = sum(HR, na.rm = TRUE),
      ros_RBI   = sum(RBI, na.rm = TRUE),
      ros_SB    = sum(SB, na.rm = TRUE),
      n_hitters_matched = n(),
      .groups = "drop"
    )

  pitcher_ros_by_team <- roster_pitchers %>%
    group_by(team_id, team_name) %>%
    summarize(
      ros_IP = sum(IP, na.rm = TRUE),
      ros_W  = sum(W,  na.rm = TRUE),
      ros_SV = sum(SV, na.rm = TRUE),
      ros_SO = sum(SO, na.rm = TRUE),
      ros_ER = sum(ER, na.rm = TRUE),
      ros_BB = sum(BB, na.rm = TRUE),
      ros_HA = sum(HA, na.rm = TRUE),
      n_pitchers_matched = n(),
      .groups = "drop"
    )

  # ================================================================
  # STEP 6 — Combine YTD + ROS = projected end-of-season totals
  # ================================================================
  message("\n=== Step 6: Computing projected end-of-season totals ===")

  projected <- ytd_standings %>%
    left_join(hitter_ros_by_team,  by = c("team_id", "team_name")) %>%
    left_join(pitcher_ros_by_team, by = c("team_id", "team_name")) %>%
    mutate(across(starts_with("ros_"), ~replace_na(.x, 0))) %>%
    mutate(across(c(n_hitters_matched, n_pitchers_matched),
                  ~replace_na(.x, 0L))) %>%
    mutate(
      # Counting stats: simple addition
      proj_R   = R   + ros_R,
      proj_HR  = HR  + ros_HR,
      proj_RBI = RBI + ros_RBI,
      proj_SB  = SB  + ros_SB,
      proj_W   = W   + ros_W,
      proj_SV  = SV  + ros_SV,
      proj_SO  = SO  + ros_SO,

      # Rate stats: recompute from components
      proj_H_total  = H  + ros_H_bat,
      proj_AB_total = AB + ros_AB,
      proj_AVG = if_else(proj_AB_total > 0,
                         proj_H_total / proj_AB_total, 0),

      proj_ER_total = ER + ros_ER,
      proj_IP_total = IP + ros_IP,
      proj_ERA = if_else(proj_IP_total > 0,
                         proj_ER_total * 9 / proj_IP_total, 99.99),

      proj_BB_total = BB + ros_BB,
      proj_HA_total = HA + ros_HA,
      proj_WHIP = if_else(proj_IP_total > 0,
                          (proj_BB_total + proj_HA_total) / proj_IP_total,
                          9.99)
    )

  # ================================================================
  # STEP 7 — Rank teams and compute roto points
  # ================================================================
  message("\n=== Step 7: Ranking teams and computing roto points ===")

  higher_better <- c("R", "HR", "RBI", "SB", "AVG", "W", "SV", "SO")
  lower_better  <- c("ERA", "WHIP")
  categories    <- c(higher_better, lower_better)

  for (cat in categories) {
    proj_col <- paste0("proj_", cat)
    rank_col <- paste0("rank_", cat)
    pts_col  <- paste0("pts_",  cat)

    if (cat %in% higher_better) {
      projected <- projected %>%
        mutate(
          !!rank_col := rank(-get(proj_col), ties.method = "average"),
          !!pts_col  := N_TEAMS + 1 - get(rank_col)
        )
    } else {
      projected <- projected %>%
        mutate(
          !!rank_col := rank(get(proj_col), ties.method = "average"),
          !!pts_col  := N_TEAMS + 1 - get(rank_col)
        )
    }
  }

  pts_cols <- paste0("pts_", categories)
  projected <- projected %>%
    mutate(
      total_pts        = rowSums(across(all_of(pts_cols))),
      projected_finish = rank(-total_pts, ties.method = "min")
    ) %>%
    arrange(projected_finish)

  # ================================================================
  # STEP 8 — Output
  # ================================================================
  message("\n=== Step 8: Writing output files ===")
  dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

  # Standings
  standings_out <- projected %>%
    select(
      team_id, team_name, projected_finish, total_pts,
      starts_with("proj_"), starts_with("rank_"), starts_with("pts_"),
      n_hitters_matched, n_pitchers_matched
    )
  write_csv(standings_out,
            "data/processed/inseason_projected_standings.csv")
  message("Wrote data/processed/inseason_projected_standings.csv")

  # Player-level detail
  detail_hitters <- roster_hitters %>%
    select(team_id, team_name, player_name, lineup_slot,
           any_of(c("AB", "H", "R", "HR", "RBI", "SB"))) %>%
    mutate(player_type = "hitter")

  detail_pitchers <- roster_pitchers %>%
    select(team_id, team_name, player_name, lineup_slot,
           any_of(c("IP", "W", "SV", "SO", "ER", "BB", "HA"))) %>%
    mutate(player_type = "pitcher")

  team_detail <- bind_rows(detail_hitters, detail_pitchers) %>%
    arrange(team_name, player_type, desc(coalesce(
      if ("HR" %in% names(.)) HR else NULL,
      if ("SO" %in% names(.)) SO else NULL,
      0
    )))

  write_csv(team_detail, "data/processed/inseason_team_details.csv")
  message("Wrote data/processed/inseason_team_details.csv")

  # Status
  write_status("success",
               warnings = if (length(pipeline_warnings) > 0)
                 pipeline_warnings else NULL)

  # --- Summary ---
  message("\n=== In-season update complete! ===")
  message("\nProjected Standings:")
  projected %>%
    select(projected_finish, team_name, total_pts) %>%
    pwalk(~ message(sprintf("  %2d. %-25s %.1f pts", ..1, ..2, ..3)))

}, error = function(e) {
  message("\n!!! In-season update FAILED: ", e$message)
  write_status("error", error_message = e$message)
  quit(save = "no", status = 1)
})
