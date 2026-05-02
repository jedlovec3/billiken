# scripts/inseason_proration.R
#
# Helpers that implement the "playing-time ratio" prorated standings view
# (Option 2 from the design plan).
#
# The core idea: a stashed (bench / IL / minors) player and the active fill-in
# occupying their roster spot can't both fully contribute over the rest of the
# season. FanGraphs ROS already encodes how much each player will produce,
# scaled by their expected return time. We compute a per-player on-roster
# fraction `f = ros_pt / position_benchmark` (capped at 1.0), pair each stashed
# player with the lowest-value active player they could displace (respecting
# Billiken slot eligibility), then prorate the fill-in's stats by `(1 - f)`.
#
# Public entry points:
#   compute_pt_benchmarks(ros_hitters, ros_pitchers)
#   attach_hitter_eligibility(roster_hitters, positions_path, benchmarks)
#   attach_pitcher_role(roster_pitchers, benchmarks)
#   build_hitter_pairings(roster_hitters)
#   build_pitcher_pairings(roster_pitchers)
#   apply_prorations(roster_hitters, roster_pitchers, hitter_pairs, pitcher_pairs)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(stringi)
  library(stringr)
  library(tibble)
})

# --- Hitter slot precedence -------------------------------------------------
# Lower number = scarcer position. Used to pick a single primary position
# for the f benchmark when a player has multi-position eligibility.
HITTER_PRIMARY_PRECEDENCE <- c(
  "C" = 1, "SS" = 2, "2B" = 3, "3B" = 4, "1B" = 5, "OF" = 6, "DH" = 7
)

# Map ESPN lineup_slot label to a Billiken hitter slot category. Returns
# NA for non-hitter or non-active slots (BE, IL, P, SP, RP, etc.).
billiken_hitter_slot <- function(lineup_slot) {
  case_when(
    lineup_slot == "C"                       ~ "C",
    lineup_slot == "1B"                      ~ "1B",
    lineup_slot == "2B"                      ~ "2B",
    lineup_slot == "3B"                      ~ "3B",
    lineup_slot == "SS"                      ~ "SS",
    lineup_slot %in% c("LF","CF","RF","OF")  ~ "OF",
    lineup_slot %in% c("UTIL","DH")          ~ "UTIL",
    lineup_slot == "MI"                      ~ "MI",
    lineup_slot == "CI"                      ~ "CI",
    lineup_slot == "IF"                      ~ "MI",
    TRUE                                     ~ NA_character_
  )
}

# --- Benchmarks -------------------------------------------------------------

# Compute league-wide ROS playing-time benchmarks.
#
# Hitters: median PA among the top-K NL hitters at each primary position,
#   where K matches the number of NL starters at that position (15 catchers,
#   15 first basemen, ..., 45 outfielders).
# Pitchers: median IP among the top 75 SPs (5 per team x 15 NL teams) and
#   the top 120 RPs (8 per team x 15 NL teams). SP/RP split by GS/G >= 0.5.
#
# Returns a list:
#   $hitters       tibble(position, benchmark_pa, pool_size)
#   $sp_benchmark  scalar median IP for SPs (NA_real_ if unavailable)
#   $rp_benchmark  scalar median IP for RPs (NA_real_ if unavailable)
compute_pt_benchmarks <- function(ros_hitters, ros_pitchers) {
  # --- Hitter benchmarks
  hitter_bm <- tibble(position = character(0),
                      benchmark_pa = numeric(0),
                      pool_size = integer(0))

  if ("PA" %in% names(ros_hitters) && "Pos" %in% names(ros_hitters)) {
    position_starter_counts <- c(
      "C" = 15, "1B" = 15, "2B" = 15, "3B" = 15, "SS" = 15,
      "OF" = 45, "DH" = 7
    )

    pick_primary <- function(pos_str) {
      if (is.na(pos_str) || pos_str == "") return(NA_character_)
      parts <- strsplit(pos_str, "/", fixed = TRUE)[[1]]
      parts <- ifelse(parts %in% c("LF","CF","RF"), "OF", parts)
      parts <- intersect(parts, names(HITTER_PRIMARY_PRECEDENCE))
      if (length(parts) == 0) return("OF")
      parts[which.min(HITTER_PRIMARY_PRECEDENCE[parts])]
    }

    hitters_tagged <- ros_hitters %>%
      mutate(primary_position = vapply(Pos, pick_primary, character(1)))

    hitter_bm <- map_dfr(names(position_starter_counts), function(pos) {
      K <- position_starter_counts[[pos]]
      pool <- hitters_tagged %>%
        filter(primary_position == pos, !is.na(PA)) %>%
        arrange(desc(PA)) %>%
        slice_head(n = K)
      tibble(
        position     = pos,
        benchmark_pa = if (nrow(pool) > 0) median(pool$PA) else NA_real_,
        pool_size    = nrow(pool)
      )
    })
  }

  # --- Pitcher benchmarks
  sp_bm <- NA_real_
  rp_bm <- NA_real_
  if (all(c("GS","G","IP") %in% names(ros_pitchers))) {
    ros_p <- ros_pitchers %>%
      mutate(
        gs_ratio     = if_else(!is.na(G) & G > 0, GS / G, 0),
        pitcher_role = if_else(gs_ratio >= 0.5, "SP", "RP")
      )
    sp_pool <- ros_p %>% filter(pitcher_role == "SP", !is.na(IP)) %>%
      arrange(desc(IP)) %>% slice_head(n = 75)
    rp_pool <- ros_p %>% filter(pitcher_role == "RP", !is.na(IP)) %>%
      arrange(desc(IP)) %>% slice_head(n = 120)
    if (nrow(sp_pool) > 0) sp_bm <- median(sp_pool$IP)
    if (nrow(rp_pool) > 0) rp_bm <- median(rp_pool$IP)
  }

  list(
    hitters      = hitter_bm,
    sp_benchmark = sp_bm,
    rp_benchmark = rp_bm
  )
}

# --- Eligibility & pt_fraction ---------------------------------------------

# Attach Billiken positional eligibility, primary position, can_fill_* flags,
# pt_benchmark, pt_fraction, and billiken_slot to roster_hitters.
attach_hitter_eligibility <- function(roster_hitters, positions_path, benchmarks,
                                      normalize_fn = NULL) {
  rh <- roster_hitters
  if (nrow(rh) == 0) return(rh)

  # Default: nothing eligible (filled in below from positions_latest.csv +
  # FanGraphs Pos fallback).
  rh$elig_C  <- FALSE
  rh$elig_1B <- FALSE
  rh$elig_2B <- FALSE
  rh$elig_3B <- FALSE
  rh$elig_SS <- FALSE
  rh$elig_OF <- FALSE
  rh$elig_DH <- FALSE

  if (!is.null(positions_path) && file.exists(positions_path)) {
    pos_df <- read_csv(positions_path, show_col_types = FALSE)
    if (!is.null(normalize_fn)) {
      pos_df$name_normalized <- normalize_fn(pos_df$PLAYER)
    } else {
      pos_df$name_normalized <- pos_df$PLAYER
    }

    pos_lookup <- pos_df %>%
      transmute(
        name_normalized,
        elig_C  = !is.na(C)  & C  == 1,
        elig_1B = !is.na(`1B`) & `1B` == 1,
        elig_2B = !is.na(`2B`) & `2B` == 1,
        elig_3B = !is.na(`3B`) & `3B` == 1,
        elig_SS = !is.na(SS) & SS == 1,
        elig_OF = (!is.na(LF) & LF == 1) |
                  (!is.na(CF) & CF == 1) |
                  (!is.na(RF) & RF == 1),
        elig_DH = !is.na(DH) & DH == 1
      ) %>%
      group_by(name_normalized) %>%
      summarize(across(everything(), any), .groups = "drop")

    rh <- rh %>%
      select(-starts_with("elig_")) %>%
      left_join(pos_lookup, by = "name_normalized") %>%
      mutate(across(starts_with("elig_"), ~replace_na(.x, FALSE)))
  }

  # Fallback for players missing from positions_latest: parse FanGraphs Pos.
  no_elig <- !rh$elig_C & !rh$elig_1B & !rh$elig_2B & !rh$elig_3B &
             !rh$elig_SS & !rh$elig_OF & !rh$elig_DH
  if (any(no_elig) && "Pos" %in% names(rh)) {
    fb_idx <- which(no_elig)
    for (i in fb_idx) {
      pos_str <- rh$Pos[i]
      if (is.na(pos_str) || pos_str == "") next
      parts <- strsplit(pos_str, "/", fixed = TRUE)[[1]]
      if ("C"  %in% parts) rh$elig_C[i]  <- TRUE
      if ("1B" %in% parts) rh$elig_1B[i] <- TRUE
      if ("2B" %in% parts) rh$elig_2B[i] <- TRUE
      if ("3B" %in% parts) rh$elig_3B[i] <- TRUE
      if ("SS" %in% parts) rh$elig_SS[i] <- TRUE
      if (any(c("LF","CF","RF","OF") %in% parts)) rh$elig_OF[i] <- TRUE
      if ("DH" %in% parts) rh$elig_DH[i] <- TRUE
    }
  }

  # Final fallback: still no eligibility -> permissive UTIL-only.
  no_elig <- !rh$elig_C & !rh$elig_1B & !rh$elig_2B & !rh$elig_3B &
             !rh$elig_SS & !rh$elig_OF & !rh$elig_DH
  rh$elig_DH[no_elig] <- TRUE

  # Billiken slot fillability flags (which dashboard slots can this player fill).
  rh <- rh %>%
    mutate(
      can_fill_C    = elig_C,
      can_fill_1B   = elig_1B,
      can_fill_2B   = elig_2B,
      can_fill_3B   = elig_3B,
      can_fill_SS   = elig_SS,
      can_fill_OF   = elig_OF,
      can_fill_MI   = elig_2B | elig_SS,
      can_fill_CI   = elig_1B | elig_3B,
      can_fill_UTIL = elig_C | elig_1B | elig_2B | elig_3B |
                      elig_SS | elig_OF | elig_DH
    )

  # Primary position via scarcity precedence.
  primary_for_row <- function(elig_C, elig_SS, elig_2B, elig_3B,
                              elig_1B, elig_OF, elig_DH) {
    cands <- character(0)
    if (elig_C)  cands <- c(cands, "C")
    if (elig_SS) cands <- c(cands, "SS")
    if (elig_2B) cands <- c(cands, "2B")
    if (elig_3B) cands <- c(cands, "3B")
    if (elig_1B) cands <- c(cands, "1B")
    if (elig_OF) cands <- c(cands, "OF")
    if (elig_DH) cands <- c(cands, "DH")
    if (length(cands) == 0) return("DH")
    cands[which.min(HITTER_PRIMARY_PRECEDENCE[cands])]
  }
  rh$primary_position <- pmap_chr(
    list(rh$elig_C, rh$elig_SS, rh$elig_2B, rh$elig_3B,
         rh$elig_1B, rh$elig_OF, rh$elig_DH),
    primary_for_row
  )

  # pt_benchmark + pt_fraction (PA / benchmark, capped to [0, 1]).
  bm <- benchmarks$hitters
  bm_lookup <- if (nrow(bm) > 0) setNames(bm$benchmark_pa, bm$position)
               else setNames(numeric(0), character(0))
  rh$pt_benchmark <- bm_lookup[rh$primary_position]
  pa_col <- if ("PA" %in% names(rh)) rh$PA else rep(NA_real_, nrow(rh))
  rh$pt_fraction <- pmin(1, pmax(0, pa_col / rh$pt_benchmark))
  rh$pt_fraction[is.na(rh$pt_fraction)] <- 0

  # Billiken slot the player currently occupies (if active).
  rh$billiken_slot <- billiken_hitter_slot(rh$lineup_slot)

  rh
}

# Attach pitcher role + pt_benchmark + pt_fraction to roster_pitchers.
attach_pitcher_role <- function(roster_pitchers, benchmarks) {
  rp <- roster_pitchers
  if (nrow(rp) == 0) return(rp)

  if (all(c("GS","G") %in% names(rp))) {
    rp <- rp %>%
      mutate(
        gs_ratio     = if_else(!is.na(G) & G > 0, GS / G, 0),
        pitcher_role = if_else(gs_ratio >= 0.5, "SP", "RP")
      )
  } else {
    # No GS/G data: fall back to ESPN lineup_slot, default unknown to RP.
    rp <- rp %>%
      mutate(
        gs_ratio = NA_real_,
        pitcher_role = case_when(
          lineup_slot == "SP" ~ "SP",
          lineup_slot == "RP" ~ "RP",
          TRUE ~ "RP"
        )
      )
  }

  # Borderline starter-shaped IP: any "RP" projecting >= 80 IP is really an SP.
  if ("IP" %in% names(rp)) {
    rp <- rp %>%
      mutate(pitcher_role = if_else(pitcher_role == "RP" & !is.na(IP) & IP >= 80,
                                    "SP", pitcher_role))
  }

  rp$pt_benchmark <- if_else(rp$pitcher_role == "SP",
                             benchmarks$sp_benchmark,
                             benchmarks$rp_benchmark)
  ip_col <- if ("IP" %in% names(rp)) rp$IP else rep(NA_real_, nrow(rp))
  rp$pt_fraction <- pmin(1, pmax(0, ip_col / rp$pt_benchmark))
  rp$pt_fraction[is.na(rp$pt_fraction)] <- 0

  rp
}

# --- Pairings ---------------------------------------------------------------

# Greedy slot-eligibility-aware pairing of stashed hitters with active fill-ins.
# Returns: tibble(team_id, team_name, stashed_player, stashed_pt,
#                 fill_in_player, f).
build_hitter_pairings <- function(roster_hitters) {
  empty <- tibble(
    team_id = integer(0), team_name = character(0),
    stashed_player = character(0), stashed_pt = numeric(0),
    fill_in_player = character(0), f = numeric(0)
  )
  if (nrow(roster_hitters) == 0) return(empty)

  pairs_list <- list()
  for (tid in unique(roster_hitters$team_id)) {
    team_h <- roster_hitters %>% filter(team_id == tid)
    stashed <- team_h %>%
      filter(roster_status %in% c("bench", "IL", "minors")) %>%
      arrange(desc(pt_fraction))
    available <- team_h %>%
      filter(roster_status == "active", !is.na(billiken_slot)) %>%
      arrange(coalesce(sgp_total, 0))
    if (nrow(stashed) == 0 || nrow(available) == 0) next

    used <- rep(FALSE, nrow(available))
    team_pairs <- list()
    for (i in seq_len(nrow(stashed))) {
      s <- stashed[i, ]
      slot_flags <- c(
        C    = isTRUE(s$can_fill_C),
        `1B` = isTRUE(s$can_fill_1B),
        `2B` = isTRUE(s$can_fill_2B),
        `3B` = isTRUE(s$can_fill_3B),
        SS   = isTRUE(s$can_fill_SS),
        OF   = isTRUE(s$can_fill_OF),
        MI   = isTRUE(s$can_fill_MI),
        CI   = isTRUE(s$can_fill_CI),
        UTIL = isTRUE(s$can_fill_UTIL)
      )
      eligible_slots <- names(slot_flags)[slot_flags]
      cand_idx <- which(!used & available$billiken_slot %in% eligible_slots)
      if (length(cand_idx) == 0) next
      pick <- cand_idx[1]   # already sorted ascending by sgp_total
      used[pick] <- TRUE
      team_pairs[[length(team_pairs) + 1]] <- tibble(
        team_id        = tid,
        team_name      = s$team_name,
        stashed_player = s$player_name,
        stashed_pt     = s$pt_fraction,
        fill_in_player = available$player_name[pick],
        f              = s$pt_fraction
      )
    }
    if (length(team_pairs) > 0) {
      pairs_list[[length(pairs_list) + 1]] <- bind_rows(team_pairs)
    }
  }
  if (length(pairs_list) == 0) return(empty)
  bind_rows(pairs_list)
}

# Greedy single-pool pairing of stashed pitchers with active fill-ins. SP/RP
# share a pool because all nine Billiken pitcher slots are interchangeable.
# The proration factor uses the *stashed* player's f, computed against their
# own role's benchmark.
build_pitcher_pairings <- function(roster_pitchers) {
  empty <- tibble(
    team_id = integer(0), team_name = character(0),
    stashed_player = character(0), stashed_pt = numeric(0),
    fill_in_player = character(0), f = numeric(0)
  )
  if (nrow(roster_pitchers) == 0) return(empty)

  pairs_list <- list()
  for (tid in unique(roster_pitchers$team_id)) {
    team_p <- roster_pitchers %>% filter(team_id == tid)
    stashed <- team_p %>%
      filter(roster_status %in% c("bench", "IL", "minors")) %>%
      arrange(desc(pt_fraction))
    available <- team_p %>%
      filter(roster_status == "active") %>%
      arrange(coalesce(sgp_total, 0))
    if (nrow(stashed) == 0 || nrow(available) == 0) next

    n_pairs <- min(nrow(stashed), nrow(available))
    pairs_list[[length(pairs_list) + 1]] <- tibble(
      team_id        = tid,
      team_name      = stashed$team_name[seq_len(n_pairs)],
      stashed_player = stashed$player_name[seq_len(n_pairs)],
      stashed_pt     = stashed$pt_fraction[seq_len(n_pairs)],
      fill_in_player = available$player_name[seq_len(n_pairs)],
      f              = stashed$pt_fraction[seq_len(n_pairs)]
    )
  }
  if (length(pairs_list) == 0) return(empty)
  bind_rows(pairs_list)
}

# --- Apply prorations ------------------------------------------------------

# Scale each fill-in's counting stats by (1 - f). Stashed players keep full
# ROS (FanGraphs already accounts for delayed return); non-fill-in actives
# are unchanged.
apply_prorations <- function(roster_hitters, roster_pitchers,
                             hitter_pairs, pitcher_pairs) {
  scale_h <- roster_hitters %>%
    select(team_id, player_name) %>%
    left_join(
      hitter_pairs %>% select(team_id, fill_in_player, f),
      by = c("team_id", "player_name" = "fill_in_player")
    ) %>%
    mutate(scale = 1 - coalesce(f, 0)) %>%
    pull(scale)

  scale_p <- roster_pitchers %>%
    select(team_id, player_name) %>%
    left_join(
      pitcher_pairs %>% select(team_id, fill_in_player, f),
      by = c("team_id", "player_name" = "fill_in_player")
    ) %>%
    mutate(scale = 1 - coalesce(f, 0)) %>%
    pull(scale)

  rh_adj <- roster_hitters %>%
    mutate(across(any_of(c("AB","H","R","HR","RBI","SB")),
                  ~.x * scale_h))
  rp_adj <- roster_pitchers %>%
    mutate(across(any_of(c("IP","W","SV","SO","ER","BB","HA")),
                  ~.x * scale_p))

  list(hitters = rh_adj, pitchers = rp_adj)
}
