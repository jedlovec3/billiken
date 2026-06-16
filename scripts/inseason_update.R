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
#   data/processed/inseason_projected_standings.csv          (all rostered)
#   data/processed/inseason_projected_standings_active.csv   (active-slot only)
#   data/processed/inseason_projected_standings_prorated.csv (playing-time prorated)
#   data/processed/inseason_team_details.csv
#   data/processed/inseason_pt_benchmarks.csv
#   data/processed/inseason_pairings.csv                     (per-team stash/fill-in audit)
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
if (requireNamespace("renv", quietly = TRUE)) renv::restore(prompt = FALSE)

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
                         data_date = Sys.Date(),
                         extras = list()) {
  out <- list(
    last_updated  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status        = status,
    data_date     = as.character(data_date)
  )
  if (!is.null(error_message)) out$error_message <- error_message
  if (!is.null(warnings) && length(warnings) > 0) out$warnings <- warnings
  for (nm in names(extras)) out[[nm]] <- extras[[nm]]
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

# ESPN lineup slots that count as "active" (anything else is bench/IL/minors).
ACTIVE_LINEUP_SLOTS <- c(
  "C", "1B", "2B", "3B", "SS",
  "LF", "CF", "RF", "OF",
  "UTIL", "DH",
  "P", "SP", "RP",
  "IF", "MI", "CI"
)

# Classify each roster entry for the active-only projected-standings view.
classify_roster_status <- function(slot) {
  case_when(
    is.na(slot)                   ~ "unknown",
    slot == "IL"                  ~ "IL",
    slot == "BE"                  ~ "bench",
    slot %in% ACTIVE_LINEUP_SLOTS ~ "active",
    TRUE                          ~ "minors"   # e.g. SLOT_17
  )
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
source("scripts/inseason_free_agents.R")
source("scripts/inseason_proration.R")

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
  rosters <- rosters %>%
    mutate(roster_status = classify_roster_status(lineup_slot))
  message(sprintf("Got %d roster entries across %d teams",
                  nrow(rosters), n_distinct(rosters$team_name)))

  status_counts <- rosters %>% count(roster_status)
  message("Roster status breakdown: ",
          paste(status_counts$roster_status, status_counts$n,
                sep = "=", collapse = ", "))

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

  # Pull FanGraphs auction-calculator dollars in ROS mode so downstream
  # trade/dashboard views can use a market-style ROS value signal instead of
  # only the internal standings-value model.
  message("\n=== Step 3b: Downloading FanGraphs ROS auction dollars ===")
  old_auction_proj <- Sys.getenv("FANGRAPHS_AUCTION_PROJ", unset = "")
  old_auction_out  <- Sys.getenv("FANGRAPHS_AUCTION_OUTFILE", unset = "")
  tryCatch({
    Sys.setenv(FANGRAPHS_AUCTION_PROJ = "rfangraphsdc")
    Sys.unsetenv("FANGRAPHS_AUCTION_OUTFILE")
    source("scripts/download_fangraphs_auction_values.R")
    download_fangraphs_auction_values(projections_year = as.character(current_year))
  }, error = function(e) {
    w <- sprintf("ROS auction-dollar refresh failed: %s", e$message)
    message("WARNING: ", w)
    pipeline_warnings <<- c(pipeline_warnings, w)
  }, finally = {
    if (nzchar(old_auction_proj)) {
      Sys.setenv(FANGRAPHS_AUCTION_PROJ = old_auction_proj)
    } else {
      Sys.unsetenv("FANGRAPHS_AUCTION_PROJ")
    }
    if (nzchar(old_auction_out)) {
      Sys.setenv(FANGRAPHS_AUCTION_OUTFILE = old_auction_out)
    } else {
      Sys.unsetenv("FANGRAPHS_AUCTION_OUTFILE")
    }
  })

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
  # PA is kept so the prorated pipeline can compute pt_fraction = PA / benchmark.
  hitter_proj_cols <- intersect(
    c("name_normalized", "Name", "Team",
      "AB", "H", "R", "HR", "RBI", "SB",
      "PA"),
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
  # ERA + WHIP feed the team-detail rate-stat columns. GS + G feed SP/RP
  # classification (GS/G >= 0.5 → SP) for the prorated standings view.
  pitcher_proj_cols <- intersect(
    c("name_normalized", "Name", "Team",
      "IP", "W", "SV", "SO", "ER", "BB", "HA",
      "ERA", "WHIP",
      "GS", "G"),
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
  # STEP 4b — Score rostered players (ROS SGP) for team detail + recs
  # ================================================================
  message("\n=== Step 4b: Scoring rostered players (ROS SGP) ===")

  rostered_scores <- score_rostered_players(roster_hitters, roster_pitchers)
  roster_hitters  <- rostered_scores$hitters
  roster_pitchers <- rostered_scores$pitchers
  sgp_source <- rostered_scores$source
  message(sprintf("SGP source: %s", sgp_source))

  # ================================================================
  # STEP 4c — Compute free-agent rankings
  # ================================================================
  message("\n=== Step 4c: Computing free-agent rankings ===")

  fa_result <- compute_inseason_free_agents(
    ros_hitters              = ros_hitters,
    ros_pitchers             = ros_pitchers,
    rostered_names_normalized = unique(roster_norm$name_normalized),
    positions_path           = "data/raw/positions_latest.csv",
    normalize_fn             = normalize_name
  )
  free_agents <- fa_result$free_agents
  message(sprintf("Identified %d free agents (%d hitters, %d pitchers)",
                  fa_result$n_free_agents,
                  sum(free_agents$player_type == "hitter"),
                  sum(free_agents$player_type == "pitcher")))

  if (fa_result$source == "fallback") {
    w <- "category_unit_values.csv missing; free-agent ranking is using z-score fallback."
    message("WARNING: ", w)
    pipeline_warnings <- c(pipeline_warnings, w)
  }

  # ================================================================
  # STEPS 4d-4e — Compute benchmarks, attach pt_fraction, build pairings.
  # The whole prorated path is wrapped in its own tryCatch: if it fails the
  # legacy "all" and "active" views still ship.
  # ================================================================
  prorated_ok <- TRUE
  pt_benchmarks <- list(hitters = tibble(position = character(0),
                                         benchmark_pa = numeric(0),
                                         pool_size = integer(0)),
                        sp_benchmark = NA_real_,
                        rp_benchmark = NA_real_)
  hitter_pairs  <- tibble(team_id = integer(0), team_name = character(0),
                          stashed_player = character(0), stashed_pt = numeric(0),
                          fill_in_player = character(0), f = numeric(0))
  pitcher_pairs <- hitter_pairs

  tryCatch({
    message("\n=== Step 4d: Computing playing-time benchmarks ===")
    pt_benchmarks <- compute_pt_benchmarks(ros_hitters, ros_pitchers)
    message(sprintf("Hitter benchmarks: %s",
                    paste(sprintf("%s=%.0f PA (n=%d)",
                                  pt_benchmarks$hitters$position,
                                  pt_benchmarks$hitters$benchmark_pa,
                                  pt_benchmarks$hitters$pool_size),
                          collapse = ", ")))
    message(sprintf("Pitcher benchmarks: SP=%.1f IP, RP=%.1f IP",
                    pt_benchmarks$sp_benchmark, pt_benchmarks$rp_benchmark))

    roster_hitters <- attach_hitter_eligibility(
      roster_hitters,
      positions_path = "data/raw/positions_latest.csv",
      benchmarks     = pt_benchmarks,
      normalize_fn   = normalize_name
    )
    roster_pitchers <- attach_pitcher_role(roster_pitchers, pt_benchmarks)

    message("\n=== Step 4e: Pairing stashed players to active fill-ins ===")
    hitter_pairs  <- build_hitter_pairings(roster_hitters)
    pitcher_pairs <- build_pitcher_pairings(roster_pitchers)
    message(sprintf("Built %d hitter pairings, %d pitcher pairings across %d teams",
                    nrow(hitter_pairs), nrow(pitcher_pairs),
                    n_distinct(c(hitter_pairs$team_id, pitcher_pairs$team_id))))
  }, error = function(e) {
    prorated_ok <<- FALSE
    w <- sprintf("Prorated view setup failed: %s", e$message)
    message("WARNING: ", w)
    pipeline_warnings <<- c(pipeline_warnings, w)
  })

  # ================================================================
  # STEPS 5-7 — Aggregate, combine YTD + ROS, rank teams
  # Wrapped in a helper so we can compute three views:
  #   * all rostered players  (legacy default)
  #   * active-slot players   (excludes bench, IL, minors)
  #   * prorated              (full ROS for stashed; (1-f) ROS for fill-ins)
  # ================================================================
  message("\n=== Steps 5-7: Projecting standings (all + active + prorated) ===")

  project_standings <- function(rh, rp, ytd) {
    hitter_ros_by_team <- rh %>%
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

    pitcher_ros_by_team <- rp %>%
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

    projected <- ytd %>%
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
    projected %>%
      mutate(
        total_pts        = rowSums(across(all_of(pts_cols))),
        projected_finish = rank(-total_pts, ties.method = "min")
      ) %>%
      arrange(projected_finish)
  }

  # View 1: all rostered players (legacy behavior)
  projected <- project_standings(roster_hitters, roster_pitchers, ytd_standings)

  # View 2: active-slot players only (excludes bench, IL, minors)
  roster_hitters_active  <- roster_hitters  %>% filter(roster_status == "active")
  roster_pitchers_active <- roster_pitchers %>% filter(roster_status == "active")
  projected_active <- project_standings(
    roster_hitters_active, roster_pitchers_active, ytd_standings
  )

  # View 3: prorated. Scale fill-in counting stats by (1 - f) so a stashed
  # player and the active fill-in occupying their slot don't double-count.
  # If 4d/4e failed, fall back to the all-rostered projection so the prorated
  # CSV still has data and the dashboard never goes dark.
  projected_prorated <- projected
  if (prorated_ok) {
    tryCatch({
      prorated_input <- apply_prorations(
        roster_hitters, roster_pitchers, hitter_pairs, pitcher_pairs
      )
      projected_prorated <- project_standings(
        prorated_input$hitters, prorated_input$pitchers, ytd_standings
      )
    }, error = function(e) {
      prorated_ok <<- FALSE
      w <- sprintf("Prorated standings projection failed: %s", e$message)
      message("WARNING: ", w)
      pipeline_warnings <<- c(pipeline_warnings, w)
    })
  }

  # ================================================================
  # STEP 8 — Output
  # ================================================================
  message("\n=== Step 8: Writing output files ===")
  dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

  # Standings — "all rostered" view (legacy default)
  standings_cols <- function(df) {
    df %>% select(
      team_id, team_name, projected_finish, total_pts,
      starts_with("proj_"), starts_with("rank_"), starts_with("pts_"),
      n_hitters_matched, n_pitchers_matched
    )
  }

  write_csv(standings_cols(projected),
            "data/processed/inseason_projected_standings.csv")
  message("Wrote data/processed/inseason_projected_standings.csv")

  write_csv(standings_cols(projected_active),
            "data/processed/inseason_projected_standings_active.csv")
  message("Wrote data/processed/inseason_projected_standings_active.csv")

  write_csv(standings_cols(projected_prorated),
            "data/processed/inseason_projected_standings_prorated.csv")
  message("Wrote data/processed/inseason_projected_standings_prorated.csv")

  # Benchmarks audit
  bm_out <- bind_rows(
    pt_benchmarks$hitters %>% mutate(role = "hitter") %>%
      transmute(role, position, benchmark = benchmark_pa,
                unit = "PA", pool_size),
    tibble(role = "pitcher", position = "SP",
           benchmark = pt_benchmarks$sp_benchmark,
           unit = "IP", pool_size = NA_integer_),
    tibble(role = "pitcher", position = "RP",
           benchmark = pt_benchmarks$rp_benchmark,
           unit = "IP", pool_size = NA_integer_)
  )
  write_csv(bm_out, "data/processed/inseason_pt_benchmarks.csv")
  message("Wrote data/processed/inseason_pt_benchmarks.csv")

  # Pairings audit
  pairings_out <- bind_rows(
    hitter_pairs  %>% mutate(player_type = "hitter"),
    pitcher_pairs %>% mutate(player_type = "pitcher")
  )
  write_csv(pairings_out, "data/processed/inseason_pairings.csv")
  message("Wrote data/processed/inseason_pairings.csv")

  # ================================================================
  # Build per-player displacement_role + effective_share for the
  # team-detail CSV so the Lovable drill-down can label each row.
  # ================================================================
  hitter_disp <- bind_rows(
    hitter_pairs %>% transmute(team_id, player_name = stashed_player,
                               displacement_role = paste0("stashed_by:", fill_in_player),
                               effective_share   = 1.0),
    hitter_pairs %>% transmute(team_id, player_name = fill_in_player,
                               displacement_role = paste0("displaces:", stashed_player),
                               effective_share   = 1 - f)
  )
  pitcher_disp <- bind_rows(
    pitcher_pairs %>% transmute(team_id, player_name = stashed_player,
                                displacement_role = paste0("stashed_by:", fill_in_player),
                                effective_share   = 1.0),
    pitcher_pairs %>% transmute(team_id, player_name = fill_in_player,
                                displacement_role = paste0("displaces:", stashed_player),
                                effective_share   = 1 - f)
  )

  # Player-level detail (always includes every rostered player plus
  # roster_status, pt_fraction, and displacement_role so the dashboard can
  # filter/highlight bench/IL/minors and label each row's prorated share).
  detail_hitters <- roster_hitters %>%
    select(team_id, team_name, player_name, lineup_slot, roster_status,
           any_of(c("AB", "H", "R", "HR", "RBI", "SB", "PA")),
           any_of(c("primary_position", "pt_benchmark", "pt_fraction")),
           any_of(c("sgp_total", "sgp_hitting", "sgp_pitching"))) %>%
    mutate(player_type = "hitter") %>%
    left_join(hitter_disp, by = c("team_id", "player_name")) %>%
    mutate(
      displacement_role = coalesce(displacement_role, ""),
      effective_share   = coalesce(effective_share, 1.0)
    )

  detail_pitchers <- roster_pitchers %>%
    select(team_id, team_name, player_name, lineup_slot, roster_status,
           any_of(c("IP", "W", "SV", "SO", "ER", "BB", "HA", "ERA", "WHIP")),
           any_of(c("pitcher_role", "pt_benchmark", "pt_fraction")),
           any_of(c("sgp_total", "sgp_hitting", "sgp_pitching"))) %>%
    mutate(player_type = "pitcher") %>%
    left_join(pitcher_disp, by = c("team_id", "player_name")) %>%
    mutate(
      displacement_role = coalesce(displacement_role, ""),
      effective_share   = coalesce(effective_share, 1.0)
    )

  team_detail <- bind_rows(detail_hitters, detail_pitchers) %>%
    arrange(team_name, player_type, desc(coalesce(
      if ("sgp_total" %in% names(.)) sgp_total else NULL,
      if ("HR" %in% names(.)) HR else NULL,
      if ("SO" %in% names(.)) SO else NULL,
      0
    )))

  write_csv(team_detail, "data/processed/inseason_team_details.csv")
  message("Wrote data/processed/inseason_team_details.csv")

  # ================================================================
  # STEP 9 — Write free-agent rankings
  # ================================================================
  write_csv(free_agents, "data/processed/inseason_free_agents.csv")
  message("Wrote data/processed/inseason_free_agents.csv")

  # ================================================================
  # STEP 10 — Build trade-analysis artifacts (Phase 1 + Phase 3)
  # ================================================================
  # Run team_assets and team_posture as side-effect scripts. Wrap each in
  # its own tryCatch so a failure here is surfaced as a pipeline warning
  # without breaking the standings / free-agent endpoints that downstream
  # consumers already depend on.
  message("\n=== Step 10: Building trade-analysis artifacts ===")

  run_trade_artifact <- function(label, script_path, runner = NULL) {
    tryCatch({
      message(sprintf("Running %s...", script_path))
      artifact_env <- new.env(parent = globalenv())
      source(script_path, local = artifact_env)
      if (!is.null(runner)) {
        artifact_env[[runner]]()
      }
    }, error = function(e) {
      w <- sprintf("%s failed: %s", label, e$message)
      message("WARNING: ", w)
      pipeline_warnings <<- c(pipeline_warnings, w)
    })
  }

  # Future/prospect artifacts are additive. They may warn if a public source
  # changes shape or FanGraphs auth expires, but standings/free-agent outputs
  # should remain available.
  run_trade_artifact("download_future_projections",
                     "scripts/download_future_projections.R",
                     "download_future_projections")
  run_trade_artifact("download_prospect_rankings",
                     "scripts/download_prospect_rankings.R",
                     "download_prospect_rankings")
  run_trade_artifact("build_prospect_values",
                     "scripts/build_prospect_values.R")
  run_trade_artifact("build_team_assets", "scripts/build_team_assets.R")
  run_trade_artifact("team_posture",      "scripts/team_posture.R")
  # value_draft_picks depends on team_posture.csv produced by team_posture.R
  run_trade_artifact("value_draft_picks", "scripts/value_draft_picks.R")
  # trade_recommendations depends on all three CSVs above.
  run_trade_artifact("trade_recommendations",
                     "scripts/trade_recommendations.R")

  n_trade_targets <- 0L
  trade_targets_path <- "data/processed/trade_targets.csv"
  if (file.exists(trade_targets_path)) {
    n_trade_targets <- nrow(
      read_csv(trade_targets_path, show_col_types = FALSE)
    )
  }

  # Status
  write_status("success",
               warnings = if (length(pipeline_warnings) > 0)
                 pipeline_warnings else NULL,
               extras = list(
                 sgp_source       = sgp_source,
                 n_free_agents    = fa_result$n_free_agents,
                 sp_benchmark_ip  = pt_benchmarks$sp_benchmark,
                 rp_benchmark_ip  = pt_benchmarks$rp_benchmark,
                 n_hitter_pairings  = nrow(hitter_pairs),
                 n_pitcher_pairings = nrow(pitcher_pairs),
                 n_trade_targets    = n_trade_targets
               ))

  # --- Summary ---
  message("\n=== In-season update complete! ===")
  message("\nProjected Standings (all rostered players):")
  projected %>%
    select(projected_finish, team_name, total_pts) %>%
    pwalk(~ message(sprintf("  %2d. %-25s %.1f pts", ..1, ..2, ..3)))

  message("\nProjected Standings (active-slot players only):")
  projected_active %>%
    select(projected_finish, team_name, total_pts) %>%
    pwalk(~ message(sprintf("  %2d. %-25s %.1f pts", ..1, ..2, ..3)))

  message("\nProjected Standings (prorated):")
  projected_prorated %>%
    select(projected_finish, team_name, total_pts) %>%
    pwalk(~ message(sprintf("  %2d. %-25s %.1f pts", ..1, ..2, ..3)))

}, error = function(e) {
  message("\n!!! In-season update FAILED: ", e$message)
  write_status("error", error_message = e$message)
  quit(save = "no", status = 1)
})
