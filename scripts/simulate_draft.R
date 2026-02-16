# simulate_draft.R
# Simulate the Billiken League fantasy baseball draft
# Run multiple simulations to evaluate team projections and draft strategies

# Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
})

# ============================================================================
# CONFIGURATION
# ============================================================================

# Salary cap per team
SALARY_CAP <- 270

# Default salary for unpriced players
DEFAULT_SALARY <- 1

# Number of teams
N_TEAMS <- 10

# Team names
TEAM_NAMES <- c(
  "Blue Socks", "Free At Last", "Melonheads", "Free Birds", 
  "Westside Marauders", "Louisville Sluggers", "Hoosiers", 
  "Erie Lakers", "National Pastime", "Big Red Machine"
)

# Roster structure (no DH)
# 9P, 2C, 1 1B, 1 2B, 1 3B, 1 SS, 5 OF, 1 CI, 1 MI, 1 Util = 23 slots
ROSTER_POSITIONS <- c(
  rep("p", 9),
  rep("c", 2),
  "1b", "2b", "3b", "ss",
  rep("of", 5),
  "ci", "mi", "util"
)

# Replacement levels by position (for slot assignment priority - lower = scarcer)
REPLACEMENT_LEVELS <- tibble(
  pos = c("c", "1b", "2b", "3b", "ss", "of", "ci", "mi", "util", "p"),
  repl_level = c(1.2, 3.0, 3.2, 2.4, 3.6, 2.3, 2.6, 3.2, 3.7, 3.1)
)

# ============================================================================
# DATA LOADING FUNCTIONS
# ============================================================================

#' Load keepers - uses frozen rosters if available, otherwise projected keepers
load_keepers <- function() {
  keepers_path <- "../data/raw/keepers.csv"
  projected_path <- "../data/processed/projected_keepers.csv"
  
  # Check if keepers.csv exists and has all teams
  if (file.exists(keepers_path)) {
    keepers <- read_csv(keepers_path, show_col_types = FALSE)
    
    # Check if all teams have at least one keeper
    teams_with_keepers <- keepers %>% 
      filter(!is.na(billikenTeam)) %>% 
      distinct(billikenTeam) %>% 
      pull()
    
    if (length(teams_with_keepers) == N_TEAMS) {
      message("Using frozen rosters from keepers.csv")
      return(keepers %>% 
               filter(!is.na(Name)) %>%
               mutate(across(c("salary"), ~ifelse(is.na(.), DEFAULT_SALARY, .))))
    }
  }
  
  # Fall back to projected keepers
  message("Using projected keepers from projected_keepers.csv")
  keepers <- read_csv(projected_path, show_col_types = FALSE) %>% 
    filter(!is.na(Name)) %>%
    mutate(across(c("salary"), ~ifelse(is.na(.), DEFAULT_SALARY, .)))
  
  return(keepers)
}

#' Load draft order and completed picks from draft_latest.csv
load_draft_order <- function() {
  draft <- read_csv("../data/raw/draft_latest.csv", show_col_types = FALSE)
  
  # Clean up the draft data
  draft_order <- draft %>%
    rename(player = `Player...1`, billikenTeam = Team, salary = Salary) %>%
    mutate(
      salary = as.numeric(salary),
      salary = ifelse(is.na(salary), DEFAULT_SALARY, salary)
    ) %>%
    filter(!is.na(billikenTeam)) %>%
    select(player, Round, Pick, billikenTeam, salary)
  
  return(draft_order)
}

#' Load draft eligible players pool
load_draft_pool <- function() {
  pool <- read_csv("../data/processed/projected_draft_eligible.csv", show_col_types = FALSE)
  
  # Ensure position columns exist and are numeric
  pos_cols <- c("p_c", "p_1b", "p_2b", "p_3b", "p_ss", "p_of", "p_ci", "p_mi")
  for (col in pos_cols) {
    if (!col %in% names(pool)) {
      pool[[col]] <- 0
    }
    pool[[col]] <- replace_na(pool[[col]], 0)
  }
  
  # Add pitcher flag based on IP
  pool <- pool %>%
    mutate(p_p = ifelse(!is.na(IP) & IP > 0, 1, 0))
  
  # Add util eligibility for all hitters
  pool <- pool %>%
    mutate(p_util = ifelse(p_p == 0, 1, 0))
  
  return(pool)
}

#' Load salaries for players not in keeper list
load_salaries <- function() {
  salaries_path <- "../data/raw/salaries_latest.csv"
  
  if (!file.exists(salaries_path)) {
    return(tibble(Name = character(), salary = numeric()))
  }
  
  salaries <- read_csv(salaries_path, show_col_types = FALSE)
  
  # Handle case where Player column might be all NA or file is empty
  if (!"Player" %in% names(salaries) || all(is.na(salaries$Player))) {
    return(tibble(Name = character(), salary = numeric()))
  }
  
  salaries <- salaries %>%
    filter(!is.na(Player)) %>%
    rename(Name = Player) %>%
    mutate(salary = as.numeric(gsub("\\$", "", Salary))) %>%
    select(Name, salary)
  
  return(salaries)
}

#' Load position eligibility
load_positions <- function() {
  positions <- read_csv("../data/raw/positions_latest.csv", show_col_types = FALSE) %>%
    mutate(
      p_of = case_when(RF == 1 ~ 1, CF == 1 ~ 1, LF == 1 ~ 1, .default = 0),
      p_ci = case_when(`1B` == 1 ~ 1, `3B` == 1 ~ 1, .default = 0),
      p_mi = case_when(`2B` == 1 ~ 1, SS == 1 ~ 1, .default = 0)
    ) %>%
    rename(Name = PLAYER, p_c = C, p_1b = `1B`, p_2b = `2B`, p_3b = `3B`, p_ss = SS) %>%
    select(Name, p_c, p_1b, p_2b, p_3b, p_ss, p_of, p_ci, p_mi)
  
  return(positions)
}

# ============================================================================
# ROSTER MANAGEMENT FUNCTIONS
# ============================================================================

#' Create empty rosters for all teams
create_empty_rosters <- function() {
  # Create a tibble with one row per team x position slot
  rosters <- expand_grid(team = TEAM_NAMES, pos = ROSTER_POSITIONS) %>%
    group_by(team) %>%
    mutate(slot_id = row_number()) %>%
    ungroup() %>%
    mutate(
      player = NA_character_,
      salary = NA_real_,
      # Position eligibility flags for the slot
      p_c = ifelse(pos == "c", 1, 0),
      p_1b = ifelse(pos == "1b", 1, 0),
      p_2b = ifelse(pos == "2b", 1, 0),
      p_3b = ifelse(pos == "3b", 1, 0),
      p_ss = ifelse(pos == "ss", 1, 0),
      p_of = ifelse(pos == "of", 1, 0),
      p_ci = ifelse(pos == "ci", 1, 0),
      p_mi = ifelse(pos == "mi", 1, 0),
      p_util = ifelse(pos == "util", 1, 0),
      p_p = ifelse(pos == "p", 1, 0)
    )
  
  return(rosters)
}

#' Get positions a player is eligible for
get_player_positions <- function(player_row) {
  positions <- c()
  
  if (!is.null(player_row$p_p) && !is.na(player_row$p_p) && player_row$p_p == 1) {
    return(c("p"))  # Pitchers can only play pitcher
  }
  
  if (!is.null(player_row$p_c) && !is.na(player_row$p_c) && player_row$p_c == 1) positions <- c(positions, "c")
  if (!is.null(player_row$p_1b) && !is.na(player_row$p_1b) && player_row$p_1b == 1) positions <- c(positions, "1b", "ci")
  if (!is.null(player_row$p_2b) && !is.na(player_row$p_2b) && player_row$p_2b == 1) positions <- c(positions, "2b", "mi")
  if (!is.null(player_row$p_3b) && !is.na(player_row$p_3b) && player_row$p_3b == 1) positions <- c(positions, "3b", "ci")
  if (!is.null(player_row$p_ss) && !is.na(player_row$p_ss) && player_row$p_ss == 1) positions <- c(positions, "ss", "mi")
  if (!is.null(player_row$p_of) && !is.na(player_row$p_of) && player_row$p_of == 1) positions <- c(positions, "of")
  if (!is.null(player_row$p_ci) && !is.na(player_row$p_ci) && player_row$p_ci == 1) positions <- c(positions, "ci")
  if (!is.null(player_row$p_mi) && !is.na(player_row$p_mi) && player_row$p_mi == 1) positions <- c(positions, "mi")
  
  # All hitters can play utility
  if (length(positions) > 0 || (!is.null(player_row$p_util) && !is.na(player_row$p_util) && player_row$p_util == 1)) {
    positions <- c(positions, "util")
  }
  
  return(unique(positions))
}

#' Check if a player can be assigned to a roster slot
can_fill_slot <- function(player_row, slot_pos) {
  player_positions <- get_player_positions(player_row)
  return(slot_pos %in% player_positions)
}

#' Calculate team's current salary
calculate_team_salary <- function(rosters, team_name) {
  team_roster <- rosters %>%
    filter(team == team_name, !is.na(player))
  
  return(sum(team_roster$salary, na.rm = TRUE))
}

#' Find the best slot for a player on a team's roster
#' Returns the slot_id or NA if no slot available
find_best_slot <- function(rosters, team_name, player_row) {
  team_roster <- rosters %>%
    filter(team == team_name)
  
  # Get open slots
  open_slots <- team_roster %>%
    filter(is.na(player))
  
  if (nrow(open_slots) == 0) return(NA_integer_)
  
  # Get positions this player can fill
  player_positions <- get_player_positions(player_row)
  
  if (length(player_positions) == 0) return(NA_integer_)
  
  # Find eligible open slots, prioritizing by replacement level (lower = scarcer)
  eligible_slots <- open_slots %>%
    filter(pos %in% player_positions) %>%
    left_join(REPLACEMENT_LEVELS, by = "pos") %>%
    arrange(repl_level) %>%  # Assign to scarcest position first
    slice(1)
  
  if (nrow(eligible_slots) == 0) return(NA_integer_)
  
  return(eligible_slots$slot_id[1])
}

#' Assign a player to a team's roster
#' Returns updated rosters tibble
assign_player <- function(rosters, team_name, player_name, player_salary, player_row) {
  slot_id <- find_best_slot(rosters, team_name, player_row)
  
  if (is.na(slot_id)) {
    # warning(paste("Could not assign", player_name, "to", team_name, "- no eligible slot"))
    return(rosters)
  }
  
  rosters <- rosters %>%
    mutate(
      player = ifelse(team == team_name & slot_id == !!slot_id, player_name, player),
      salary = ifelse(team == team_name & slot_id == !!slot_id, player_salary, salary)
    )
  
  return(rosters)
}

#' Check if a team can add a player (salary cap check)
can_add_player <- function(rosters, team_name, player_salary) {
  current_salary <- calculate_team_salary(rosters, team_name)
  return((current_salary + player_salary) <= SALARY_CAP)
}

#' Check if team has any open roster slots
has_open_slots <- function(rosters, team_name) {
  open_count <- rosters %>%
    filter(team == team_name, is.na(player)) %>%
    nrow()
  
  return(open_count > 0)
}

# ============================================================================
# DRAFT SIMULATION FUNCTIONS
# ============================================================================

#' Fill rosters with keepers
fill_keepers <- function(rosters, keepers, player_pool) {
  for (i in 1:nrow(keepers)) {
    player_name <- keepers$Name[i]
    team_name <- keepers$billikenTeam[i]
    player_salary <- keepers$salary[i]
    
    if (is.na(team_name)) next
    
    # Find player in pool for position eligibility
    player_row <- player_pool %>% filter(Name == player_name)
    
    if (nrow(player_row) == 0) {
      # Try fuzzy match
      player_row <- keepers[i, ]
    }
    
    rosters <- assign_player(rosters, team_name, player_name, player_salary, player_row)
  }
  
  return(rosters)
}

#' Fill rosters with already-drafted players
fill_drafted_players <- function(rosters, draft_order, player_pool) {
  drafted <- draft_order %>%
    filter(!is.na(player) & player != "")
  
  if (nrow(drafted) == 0) return(rosters)
  
  for (i in 1:nrow(drafted)) {
    player_name <- drafted$player[i]
    team_name <- drafted$billikenTeam[i]
    player_salary <- drafted$salary[i]
    
    if (is.na(player_name) || player_name == "") next
    
    # Find player in pool for position eligibility
    player_row <- player_pool %>% filter(Name == player_name)
    
    if (nrow(player_row) == 0) next
    
    rosters <- assign_player(rosters, team_name, player_name, player_salary, player_row)
  }
  
  return(rosters)
}

#' Get the next team to pick
get_next_pick <- function(draft_order) {
  next_pick <- draft_order %>%
    filter(is.na(player) | player == "") %>%
    arrange(Round, Pick) %>%
    slice(1)
  
  if (nrow(next_pick) == 0) {
    return(list(team = NA, round = NA, pick = NA, idx = NA))
  }
  
  # Find the row index in original draft_order
  idx <- which(draft_order$Round == next_pick$Round[1] & 
                 draft_order$Pick == next_pick$Pick[1] &
                 (is.na(draft_order$player) | draft_order$player == ""))[1]
  
  return(list(
    team = next_pick$billikenTeam[1],
    round = next_pick$Round[1],
    pick = next_pick$Pick[1],
    idx = idx
  ))
}

#' Get available players for a team
get_available_players <- function(rosters, player_pool, team_name) {
  # Get players not on any roster
  rostered_players <- rosters %>%
    filter(!is.na(player)) %>%
    pull(player) %>%
    unique()
  
  available <- player_pool %>%
    filter(!Name %in% rostered_players)
  
  # Get team's open slots
  team_roster <- rosters %>%
    filter(team == team_name)
  
  open_slots <- team_roster %>%
    filter(is.na(player)) %>%
    pull(pos) %>%
    unique()
  
  if (length(open_slots) == 0) return(tibble())
  
  # Filter to players who can fill at least one open slot
  eligible_players <- available %>%
    rowwise() %>%
    mutate(
      can_fill = {
        positions <- get_player_positions(cur_data())
        any(positions %in% open_slots)
      }
    ) %>%
    ungroup() %>%
    filter(can_fill) %>%
    select(-can_fill)
  
  return(eligible_players)
}

#' Make a single draft pick
#' Returns list with updated rosters, draft_order, player_pool, and selected player name
make_pick <- function(rosters, draft_order, player_pool, randomness_sd = 2) {
  # Get next pick info
  next_pick <- get_next_pick(draft_order)
  
  if (is.na(next_pick$team)) {
    return(list(rosters = rosters, draft_order = draft_order, player_pool = player_pool, picked = NA))
  }
  
  team_name <- next_pick$team
  
  # Check if team has open slots
  if (!has_open_slots(rosters, team_name)) {
    draft_order$player[next_pick$idx] <- "pass"
    return(list(rosters = rosters, draft_order = draft_order, player_pool = player_pool, picked = "pass"))
  }
  
  # Get eligible players
  eligible <- get_available_players(rosters, player_pool, team_name)
  
  if (nrow(eligible) == 0) {
    draft_order$player[next_pick$idx] <- "pass"
    return(list(rosters = rosters, draft_order = draft_order, player_pool = player_pool, picked = "pass"))
  }
  
  # Filter by salary cap
  current_salary <- calculate_team_salary(rosters, team_name)
  remaining_cap <- SALARY_CAP - current_salary
  
  # Filter players who can fit under cap
  eligible <- eligible %>%
    mutate(player_salary = ifelse(is.na(salary), DEFAULT_SALARY, salary)) %>%
    filter(player_salary <= remaining_cap)
  
  if (nrow(eligible) == 0) {
    draft_order$player[next_pick$idx] <- "pass"
    return(list(rosters = rosters, draft_order = draft_order, player_pool = player_pool, picked = "pass"))
  }
  
  # Add randomness and select best player
  eligible <- eligible %>%
    mutate(
      rand = rnorm(n(), mean = 0, sd = randomness_sd),
      rand_ev = ev + rand
    ) %>%
    arrange(desc(rand_ev))
  
  selected_player <- eligible %>% slice(1)
  player_name <- selected_player$Name[1]
  player_salary <- ifelse(is.na(selected_player$salary[1]), DEFAULT_SALARY, selected_player$salary[1])
  
  # Assign player to roster
  rosters <- assign_player(rosters, team_name, player_name, player_salary, selected_player)
  
  # Update draft order
  draft_order$player[next_pick$idx] <- player_name
  draft_order$salary[next_pick$idx] <- player_salary
  
  return(list(rosters = rosters, draft_order = draft_order, player_pool = player_pool, picked = player_name))
}

#' Run one complete draft simulation
simulate_draft <- function(keepers, draft_order_template, player_pool, randomness_sd = 2) {
  # Create fresh rosters
  rosters <- create_empty_rosters()
  
  # Make a copy of draft order for this simulation
  draft_order <- draft_order_template
  
  # Fill keepers
  rosters <- fill_keepers(rosters, keepers, player_pool)
  
  # Fill already-drafted players
  rosters <- fill_drafted_players(rosters, draft_order, player_pool)
  
  # Simulate remaining picks
  max_picks <- nrow(draft_order)
  picks_made <- 0
  
  while (picks_made < max_picks) {
    result <- make_pick(rosters, draft_order, player_pool, randomness_sd)
    rosters <- result$rosters
    draft_order <- result$draft_order
    
    if (is.na(result$picked)) break  # No more picks available
    
    picks_made <- picks_made + 1
  }
  
  return(list(rosters = rosters, draft_order = draft_order))
}

# ============================================================================
# STANDINGS CALCULATION
# ============================================================================

#' Calculate projected standings from rosters
calculate_standings <- function(rosters, player_pool, sim_num = 1) {
  # Get rostered players with their stats
  rostered <- rosters %>%
    filter(!is.na(player)) %>%
    select(team, player, salary)
  
  # Join with player pool to get stats
  roster_stats <- rostered %>%
    left_join(player_pool %>% select(Name, PA, AB, H, HR, R, RBI, SB, AVG, IP, W, SV, SO, ER, ERA, WHIP, point_value, par, ev),
              by = c("player" = "Name"))
  
  # Compute team totals
  team_totals <- roster_stats %>%
    group_by(team) %>%
    summarise(
      n = n(),
      total_salary = sum(salary, na.rm = TRUE),
      point_value = sum(point_value, na.rm = TRUE),
      par = sum(par, na.rm = TRUE),
      ev = sum(ev, na.rm = TRUE),
      PA = sum(PA, na.rm = TRUE),
      AB = sum(AB, na.rm = TRUE),
      H = sum(H, na.rm = TRUE),
      HR = sum(HR, na.rm = TRUE),
      R = sum(R, na.rm = TRUE),
      RBI = sum(RBI, na.rm = TRUE),
      SB = sum(SB, na.rm = TRUE),
      AVG = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
      IP = sum(IP, na.rm = TRUE),
      W = sum(W, na.rm = TRUE),
      SV = sum(SV, na.rm = TRUE),
      SO = sum(SO, na.rm = TRUE),
      ER = sum(ER, na.rm = TRUE),
      ERA = sum(ER, na.rm = TRUE) * 9 / sum(IP, na.rm = TRUE),
      WHIP = (sum(H[!is.na(IP) & IP > 0], na.rm = TRUE) + sum(IP, na.rm = TRUE) * 0.3) / sum(IP, na.rm = TRUE),  # Approximate WHIP
      .groups = "drop"
    )
  
  # Calculate roto points (rank in each category)
  standings <- team_totals %>%
    mutate(
      hr_pts = N_TEAMS + 1 - dense_rank(desc(HR)),
      r_pts = N_TEAMS + 1 - dense_rank(desc(R)),
      rbi_pts = N_TEAMS + 1 - dense_rank(desc(RBI)),
      sb_pts = N_TEAMS + 1 - dense_rank(desc(SB)),
      avg_pts = N_TEAMS + 1 - dense_rank(desc(AVG)),
      w_pts = N_TEAMS + 1 - dense_rank(desc(W)),
      sv_pts = N_TEAMS + 1 - dense_rank(desc(SV)),
      so_pts = N_TEAMS + 1 - dense_rank(desc(SO)),
      era_pts = N_TEAMS + 1 - dense_rank(ERA),
      whip_pts = N_TEAMS + 1 - dense_rank(WHIP)
    ) %>%
    mutate(
      hit_pts = hr_pts + r_pts + rbi_pts + sb_pts + avg_pts,
      pit_pts = w_pts + sv_pts + so_pts + era_pts + whip_pts,
      total_pts = hit_pts + pit_pts,
      rank = dense_rank(desc(total_pts)),
      sim_num = sim_num
    ) %>%
    arrange(desc(total_pts))
  
  return(standings)
}

# ============================================================================
# MAIN SIMULATION
# ============================================================================

#' Run multiple draft simulations
run_simulations <- function(n_sims = 20, randomness_sd = 2, verbose = TRUE) {
  # Load data
  if (verbose) message("Loading data...")
  keepers <- load_keepers()
  draft_order_template <- load_draft_order()
  player_pool <- load_draft_pool()
  
  # Add any missing salaries (handle case where salaries is empty)
  salaries <- load_salaries()
  if (nrow(salaries) > 0) {
    player_pool <- player_pool %>%
      left_join(salaries %>% rename(salary_lookup = salary), by = "Name") %>%
      mutate(salary = coalesce(salary, salary_lookup, DEFAULT_SALARY)) %>%
      select(-salary_lookup)
  } else {
    player_pool <- player_pool %>%
      mutate(salary = coalesce(salary, DEFAULT_SALARY))
  }
  
  # Run simulations
  all_standings <- tibble()
  
  for (i in 1:n_sims) {
    if (verbose) message(sprintf("Running simulation %d/%d...", i, n_sims))
    
    suppressWarnings({
      result <- simulate_draft(keepers, draft_order_template, player_pool, randomness_sd)
      standings <- calculate_standings(result$rosters, player_pool, sim_num = i)
    })
    
    all_standings <- bind_rows(all_standings, standings)
  }
  
  return(all_standings)
}

#' Summarize simulation results
summarize_simulations <- function(all_standings) {
  summary <- all_standings %>%
    group_by(team) %>%
    summarise(
      n_sims = n(),
      wins = sum(rank == 1),
      top_3 = sum(rank <= 3),
      avg_rank = mean(rank),
      avg_pts = mean(total_pts),
      avg_hit_pts = mean(hit_pts),
      avg_pit_pts = mean(pit_pts),
      min_pts = min(total_pts),
      max_pts = max(total_pts),
      .groups = "drop"
    ) %>%
    arrange(avg_rank)
  
  return(summary)
}

# ============================================================================
# RUN SIMULATIONS
# ============================================================================

# Run the simulations
message("\n=== Billiken League Draft Simulation ===\n")

all_standings <- run_simulations(n_sims = 20, randomness_sd = 2)

# Summarize results
summary <- summarize_simulations(all_standings)

message("\n=== Simulation Results ===\n")
print(summary, n = N_TEAMS)

# Blue Socks specific analysis
message("\n=== Blue Socks Analysis ===\n")
blue_socks <- all_standings %>%
  filter(team == "Blue Socks")

message(sprintf("Average rank: %.2f", mean(blue_socks$rank)))
message(sprintf("Wins: %d/%d (%.1f%%)", sum(blue_socks$rank == 1), nrow(blue_socks), 100 * mean(blue_socks$rank == 1)))
message(sprintf("Top 3 finishes: %d/%d (%.1f%%)", sum(blue_socks$rank <= 3), nrow(blue_socks), 100 * mean(blue_socks$rank <= 3)))
message(sprintf("Average total points: %.1f (range: %.1f - %.1f)", 
                mean(blue_socks$total_pts), min(blue_socks$total_pts), max(blue_socks$total_pts)))

message("\nDone!")
