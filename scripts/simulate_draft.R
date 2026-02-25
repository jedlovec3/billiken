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

# Team names (uppercase to match preseason_rosters.csv)
TEAM_NAMES <- c(
  "BLUE SOCKS", "FREE AT LAST", "MELONHEADS", "FREE BIRDS", 
  "WESTSIDE MARAUDERS", "LOUISVILLE SLUGGERS", "HOOSIERS", 
  "ERIE LAKERS", "NATIONAL PASTIME", "BIG RED MACHINE"
)

# Replacement levels by position (for slot assignment priority - lower = scarcer)
REPLACEMENT_LEVELS <- tibble(
  pos = c("C", "1B", "2B", "3B", "SS", "OF", "CI", "MI", "Util", "P"),
  repl_level = c(1.2, 3.0, 3.2, 2.4, 3.6, 2.3, 2.6, 3.2, 3.7, 3.1)
)

# ============================================================================
# DATA LOADING FUNCTIONS
# ============================================================================

#' Load player projections with roster assignments and salaries
load_draft_pool <- function() {
  # Load projected player values (projections + position eligibility + sgpar)
  player_value <- read_csv("../data/processed/projected_player_value.csv", show_col_types = FALSE)
  
  # Load preseason rosters (keepers + drafted players with billikenTeam, contract, salary)
  rosters_raw <- read_csv("../data/processed/preseason_rosters.csv", show_col_types = FALSE)
  
  # Filter to actual rostered players (non-NA Player)
  rosters <- rosters_raw %>%
    filter(!is.na(Player)) %>%
    rename(Name = Player, contract = Contract, salary = Salary) %>%
    mutate(salary = as.numeric(gsub("\\$", "", salary))) %>%
    select(Name, billikenTeam, contract, salary)
  
  # Join roster info to player values (only if there are rostered players)
  if (nrow(rosters) > 0) {
    pool <- player_value %>%
      left_join(rosters, by = "Name")
  } else {
    # No keepers yet - add empty columns
    pool <- player_value %>%
      mutate(billikenTeam = NA_character_, contract = NA_character_, salary = NA_real_)
  }
  
  # For players NOT on a roster (billikenTeam is NA), get salary from salaries_latest.csv
  salaries <- load_salaries()
  
  if (nrow(salaries) > 0) {
    pool <- pool %>%
      left_join(salaries %>% rename(lookup_salary = salary), by = "Name") %>%
      mutate(
        # For non-rostered players: contract = "1", salary from lookup or default to $1
        contract = ifelse(is.na(billikenTeam), "1", contract),
        salary = ifelse(is.na(billikenTeam), coalesce(lookup_salary, DEFAULT_SALARY), salary)
      ) %>%
      select(-lookup_salary)
  } else {
    pool <- pool %>%
      mutate(
        contract = ifelse(is.na(billikenTeam), "1", contract),
        salary = ifelse(is.na(billikenTeam), DEFAULT_SALARY, salary)
      )
  }
  
  # Add pitcher flag based on IP for position eligibility
  pool <- pool %>%
    mutate(p_p = ifelse(!is.na(IP) & IP > 0, 1, 0))
  
  # Add util eligibility for all hitters
  pool <- pool %>%
    mutate(p_util = ifelse(p_p == 0, 1, 0))
  
  return(pool)
}

#' Load salaries for non-rostered players
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

#' Load preseason rosters (already has keeper and drafted player assignments)
load_rosters <- function() {
  rosters <- read_csv("../data/processed/preseason_rosters.csv", show_col_types = FALSE) %>%
    rename(team = billikenTeam, pos = Position, player = Player, contract = Contract, salary = Salary) %>%
    mutate(
      # Ensure player column is character type even when all NA
      player = as.character(player),
      salary = as.numeric(gsub("\\$", "", salary))
    ) %>%
    group_by(team) %>%
    mutate(slot_id = row_number()) %>%
    ungroup()
  
  return(rosters)
}

#' Load simulated keepers (used when actual keepers haven't been filled in yet)
load_simulated_keepers <- function() {
  keepers_path <- "../data/processed/simulated_keepers.csv"
  
  if (!file.exists(keepers_path)) {
    return(tibble())
  }
  
  keepers <- read_csv(keepers_path, show_col_types = FALSE) %>%
    filter(!is.na(Name) & !is.na(billikenTeam)) %>%
    select(Name, billikenTeam, slot, contract, salary)
  
  return(keepers)
}

#' Load draft order from draft_latest.csv
load_draft_order <- function() {
  draft <- read_csv("../data/raw/draft_latest.csv", show_col_types = FALSE)
  
  # Clean up the draft data
  # Uppercase team names to match preseason_rosters format
  draft_order <- draft %>%
    rename(player = Player, billikenTeam = Team, salary = Salary) %>%
    mutate(
      billikenTeam = toupper(billikenTeam),
      salary = as.numeric(salary),
      salary = ifelse(is.na(salary), DEFAULT_SALARY, salary)
    ) %>%
    filter(!is.na(billikenTeam)) %>%
    select(player, Round, Pick, billikenTeam, salary)
  
  return(draft_order)
}

#' Get positions a player is eligible for
get_player_positions <- function(player_row) {
  positions <- c()
  
  # Check if pitcher
  if (!is.null(player_row$p_p) && !is.na(player_row$p_p) && player_row$p_p == 1) {
    return(c("P"))
  }
  if (!is.null(player_row$p_sp) && !is.na(player_row$p_sp) && player_row$p_sp == 1) {
    return(c("P"))
  }
  if (!is.null(player_row$p_rp) && !is.na(player_row$p_rp) && player_row$p_rp == 1) {
    return(c("P"))
  }
  
  if (!is.null(player_row$p_c) && !is.na(player_row$p_c) && player_row$p_c == 1) positions <- c(positions, "C")
  if (!is.null(player_row$p_1b) && !is.na(player_row$p_1b) && player_row$p_1b == 1) positions <- c(positions, "1B", "CI")
  if (!is.null(player_row$p_2b) && !is.na(player_row$p_2b) && player_row$p_2b == 1) positions <- c(positions, "2B", "MI")
  if (!is.null(player_row$p_3b) && !is.na(player_row$p_3b) && player_row$p_3b == 1) positions <- c(positions, "3B", "CI")
  if (!is.null(player_row$p_ss) && !is.na(player_row$p_ss) && player_row$p_ss == 1) positions <- c(positions, "SS", "MI")
  if (!is.null(player_row$p_of) && !is.na(player_row$p_of) && player_row$p_of == 1) positions <- c(positions, "OF")
  if (!is.null(player_row$p_ci) && !is.na(player_row$p_ci) && player_row$p_ci == 1) positions <- c(positions, "CI")
  if (!is.null(player_row$p_mi) && !is.na(player_row$p_mi) && player_row$p_mi == 1) positions <- c(positions, "MI")
  if (!is.null(player_row$p_dh) && !is.na(player_row$p_dh) && player_row$p_dh == 1) positions <- c(positions, "Util")
  
  # All hitters can play utility
  if (length(positions) > 0) {
    positions <- c(positions, "Util")
  }
  
  return(unique(positions))
}

#' Calculate team's current salary
calculate_team_salary <- function(rosters, team_name) {
  team_roster <- rosters %>%
    filter(team == team_name, !is.na(player))
  
  return(sum(team_roster$salary, na.rm = TRUE))
}

#' Find the best slot for a player on a team's roster
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
    arrange(repl_level) %>%
    slice(1)
  
  if (nrow(eligible_slots) == 0) return(NA_integer_)
  
  return(eligible_slots$slot_id[1])
}

#' Assign a player to a team's roster
assign_player <- function(rosters, team_name, player_name, player_salary, player_contract, player_row) {
  slot_id <- find_best_slot(rosters, team_name, player_row)
  
  if (is.na(slot_id)) {
    return(rosters)
  }
  
  rosters <- rosters %>%
    mutate(
      player = ifelse(team == team_name & slot_id == !!slot_id, player_name, player),
      salary = ifelse(team == team_name & slot_id == !!slot_id, player_salary, salary),
      contract = ifelse(team == team_name & slot_id == !!slot_id, player_contract, contract)
    )
  
  return(rosters)
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

#' Fill simulated keepers into rosters (when actual keepers haven't been set)
fill_simulated_keepers <- function(rosters, simulated_keepers, player_pool) {
  # Check if rosters already have keepers filled in
  filled_count <- rosters %>%
    filter(!is.na(player)) %>%
    nrow()
  
  if (filled_count > 0) {
    # Keepers already filled in, don't use simulated
    return(rosters)
  }
  
  if (nrow(simulated_keepers) == 0) {
    return(rosters)
  }
  
  for (i in 1:nrow(simulated_keepers)) {
    player_name <- simulated_keepers$Name[i]
    # Uppercase team name to match preseason_rosters format
    team_name <- toupper(simulated_keepers$billikenTeam[i])
    player_salary <- simulated_keepers$salary[i]
    player_contract <- as.character(simulated_keepers$contract[i])
    
    if (is.na(team_name)) next
    
    # Find player in pool for position eligibility
    player_row <- player_pool %>% filter(Name == player_name)
    
    if (nrow(player_row) == 0) next
    
    rosters <- assign_player(rosters, team_name, player_name, player_salary, player_contract, player_row)
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
  # Get players not on any roster (billikenTeam is NA in the pool)
  # Also exclude anyone already assigned in this simulation's rosters
  rostered_players <- rosters %>%
    filter(!is.na(player)) %>%
    pull(player) %>%
    unique()
  
  available <- player_pool %>%
    filter(is.na(billikenTeam)) %>%
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
        positions <- get_player_positions(pick(everything()))
        any(positions %in% open_slots)
      }
    ) %>%
    ungroup() %>%
    filter(can_fill) %>%
    select(-can_fill)
  
  return(eligible_players)
}

#' Make a single draft pick using sgpar with percentage randomness
make_pick <- function(rosters, draft_order, player_pool, randomness_pct = 0.10) {
  next_pick <- get_next_pick(draft_order)
  
  if (is.na(next_pick$team)) {
    return(list(rosters = rosters, draft_order = draft_order, picked = NA))
  }
  
  team_name <- next_pick$team
  
  # Check if team has open slots
  if (!has_open_slots(rosters, team_name)) {
    draft_order$player[next_pick$idx] <- "pass"
    return(list(rosters = rosters, draft_order = draft_order, picked = "pass"))
  }
  
  # Get eligible players
  eligible <- get_available_players(rosters, player_pool, team_name)
  
  if (nrow(eligible) == 0) {
    draft_order$player[next_pick$idx] <- "pass"
    return(list(rosters = rosters, draft_order = draft_order, picked = "pass"))
  }
  
  # Filter by salary cap
  current_salary <- calculate_team_salary(rosters, team_name)
  remaining_cap <- SALARY_CAP - current_salary
  
  eligible <- eligible %>%
    mutate(player_salary = ifelse(is.na(salary), DEFAULT_SALARY, salary)) %>%
    filter(player_salary <= remaining_cap)
  
  if (nrow(eligible) == 0) {
    draft_order$player[next_pick$idx] <- "pass"
    return(list(rosters = rosters, draft_order = draft_order, picked = "pass"))
  }
  
  # Add percentage randomness to sgpar and select best player
  # rand_sgpar = sgpar * (1 + uniform(-randomness_pct, +randomness_pct))
  eligible <- eligible %>%
    mutate(
      rand_factor = runif(n(), min = 1 - randomness_pct, max = 1 + randomness_pct),
      rand_sgpar = sgpar * rand_factor
    ) %>%
    arrange(desc(rand_sgpar))
  
  selected_player <- eligible %>% slice(1)
  player_name <- selected_player$Name[1]
  player_salary <- ifelse(is.na(selected_player$salary[1]), DEFAULT_SALARY, selected_player$salary[1])
  player_contract <- ifelse(is.na(selected_player$contract[1]), "1", selected_player$contract[1])
  
  # Assign player to roster
  rosters <- assign_player(rosters, team_name, player_name, player_salary, player_contract, selected_player)
  
  # Update draft order
  draft_order$player[next_pick$idx] <- player_name
  draft_order$salary[next_pick$idx] <- player_salary
  
  return(list(rosters = rosters, draft_order = draft_order, picked = player_name))
}

#' Run one complete draft simulation
simulate_draft <- function(rosters_template, draft_order_template, player_pool, randomness_pct = 0.10) {
  # Make copies for this simulation
  rosters <- rosters_template
  draft_order <- draft_order_template
  
  # Simulate remaining picks
  max_picks <- nrow(draft_order)
  picks_made <- 0
  
  while (picks_made < max_picks) {
    result <- make_pick(rosters, draft_order, player_pool, randomness_pct)
    rosters <- result$rosters
    draft_order <- result$draft_order
    
    if (is.na(result$picked)) break
    
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
  # Derive ER and WH (walks + hits) from ERA, WHIP, and IP:
  #   ER = ERA * IP / 9
  #   WH = WHIP * IP
  roster_stats <- rostered %>%
    left_join(player_pool %>% 
                select(Name, PA, AB, H, HR, R, RBI, SB, AVG, IP, W, SV, SO, ERA, WHIP, sgpar) %>%
                mutate(
                  ER = ifelse(!is.na(IP) & IP > 0, ERA * IP / 9, NA_real_),
                  WH = ifelse(!is.na(IP) & IP > 0, WHIP * IP, NA_real_)
                ),
              by = c("player" = "Name"))
  
  # Compute team totals
  team_totals <- roster_stats %>%
    group_by(team) %>%
    summarise(
      n = n(),
      total_salary = sum(salary, na.rm = TRUE),
      sgpar = sum(sgpar, na.rm = TRUE),
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
      WH = sum(WH, na.rm = TRUE),
      ERA = sum(ER, na.rm = TRUE) * 9 / sum(IP, na.rm = TRUE),
      WHIP = sum(WH, na.rm = TRUE) / sum(IP, na.rm = TRUE),
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
run_simulations <- function(n_sims = 20, randomness_pct = 0.10, verbose = TRUE) {
  # Load data
  if (verbose) message("Loading data...")
  player_pool <- load_draft_pool()
  rosters_template <- load_rosters()
  simulated_keepers <- load_simulated_keepers()
  draft_order_template <- load_draft_order()
  
  # Fill simulated keepers if actual keepers haven't been set
  rosters_template <- fill_simulated_keepers(rosters_template, simulated_keepers, player_pool)
  
  # Run simulations
  all_standings <- tibble()
  
  for (i in 1:n_sims) {
    if (verbose) message(sprintf("Running simulation %d/%d...", i, n_sims))
    
    suppressWarnings({
      result <- simulate_draft(rosters_template, draft_order_template, player_pool, randomness_pct)
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
      avg_sgpar = mean(sgpar),
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

all_standings <- run_simulations(n_sims = 20, randomness_pct = 0.10)

# Summarize results
summary <- summarize_simulations(all_standings)

message("\n=== Simulation Results ===\n")
print(summary, n = N_TEAMS)

# Blue Socks specific analysis
message("\n=== Blue Socks Analysis ===\n")
blue_socks <- all_standings %>%
  filter(team == "BLUE SOCKS")

message(sprintf("Average rank: %.2f", mean(blue_socks$rank)))
message(sprintf("Wins: %d/%d (%.1f%%)", sum(blue_socks$rank == 1), nrow(blue_socks), 100 * mean(blue_socks$rank == 1)))
message(sprintf("Top 3 finishes: %d/%d (%.1f%%)", sum(blue_socks$rank <= 3), nrow(blue_socks), 100 * mean(blue_socks$rank <= 3)))
message(sprintf("Average total points: %.1f (range: %.1f - %.1f)", 
                mean(blue_socks$total_pts), min(blue_socks$total_pts), max(blue_socks$total_pts)))

message("\nDone!")
