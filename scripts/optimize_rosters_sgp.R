# scripts/optimize_rosters_sgp.R
# Optimize roster assignments across 10 teams to maximize total SGP
# Uses integer linear programming to fill 230 roster spots optimally

suppressPackageStartupMessages({
  library(tidyverse)
  library(lpSolve)
})

projections_year <- Sys.getenv("BILLIKEN_PROJECTIONS_YEAR", unset = "2026")

message("Loading player data...")

# Load player SGP data
players <- read_csv(
  "data/processed/player_sgp.csv",
  show_col_types = FALSE
) %>%
  filter(!is.na(sgp_total)) %>%
  # Ensure position flags exist
  mutate(across(starts_with("p_"), ~replace_na(., 0)))

# Separate hitters and pitchers
hitters <- players %>%
  filter(player_type %in% c("hitter", "two-way")) %>%
  mutate(is_pitcher = FALSE)

pitchers <- players %>%
  filter(player_type %in% c("pitcher", "two-way")) %>%
  mutate(is_pitcher = TRUE,
         # Pitchers not eligible for hitter positions
         p_c = 0, p_1b = 0, p_2b = 0, p_3b = 0, p_ss = 0, 
         p_of = 0, p_ci = 0, p_mi = 0)

# Combine all players
all_players <- bind_rows(hitters, pitchers) %>%
  distinct(Name, .keep_all = TRUE) %>%
  mutate(player_id = row_number())

message(sprintf("Total players: %d (Hitters: %d, Pitchers: %d)",
                nrow(all_players), 
                sum(!all_players$is_pitcher),
                sum(all_players$is_pitcher)))

# --- Define roster structure ---
# 10 teams, each with:
# 2 C, 1 1B, 1 2B, 1 3B, 1 SS, 5 OF, 1 MI, 1 CI, 1 Util, 9 P = 23 players
n_teams <- 10

slot_structure <- c(
  C = 2,
  `1B` = 1,
  `2B` = 1,
  `3B` = 1,
  SS = 1,
  OF = 5,
  MI = 1,
  CI = 1,
  Util = 1,
  P = 9
)

# Create slot vector
slots <- rep(names(slot_structure), slot_structure * n_teams)
n_slots <- length(slots)

message(sprintf("\nRoster structure: %d slots across %d teams", n_slots, n_teams))
message("Slots per team: ", paste(names(slot_structure), slot_structure, sep = "=", collapse = ", "))

# --- Build eligibility matrix ---
# eligibility[i, s] = 1 if player i can fill slot s
n_players <- nrow(all_players)
eligibility <- matrix(0, nrow = n_players, ncol = n_slots)

message("\nBuilding eligibility matrix...")

for (s in 1:n_slots) {
  slot_type <- slots[s]
  
  eligibility[, s] <- case_when(
    slot_type == "C" ~ all_players$p_c,
    slot_type == "1B" ~ all_players$p_1b,
    slot_type == "2B" ~ all_players$p_2b,
    slot_type == "3B" ~ all_players$p_3b,
    slot_type == "SS" ~ all_players$p_ss,
    slot_type == "OF" ~ all_players$p_of,
    slot_type == "MI" ~ all_players$p_mi,
    slot_type == "CI" ~ all_players$p_ci,
    slot_type == "Util" ~ as.integer(!all_players$is_pitcher),
    slot_type == "P" ~ as.integer(all_players$is_pitcher),
    TRUE ~ 0
  )
}

# Check for unfillable slots
slots_with_eligible <- colSums(eligibility)
if (any(slots_with_eligible == 0)) {
  warning(sprintf("%d slots have no eligible players!", sum(slots_with_eligible == 0)))
}

# --- Set up linear programming problem ---
# Decision variables: x[i,s] = 1 if player i is assigned to slot s, 0 otherwise
# Objective: maximize sum of point_value[i] * x[i,s]
# Constraints:
#   1. Each player assigned to at most one slot: sum_s x[i,s] <= 1 for all i
#   2. Each slot filled by exactly one player: sum_i x[i,s] = 1 for all s
#   3. Eligibility: x[i,s] = 0 if player i not eligible for slot s

n_vars <- n_players * n_slots

message(sprintf("\nSetting up LP problem with %d variables...", n_vars))

# Objective: maximize total SGP
# We'll replicate each player's sgp_total for each slot
obj <- rep(all_players$sgp_total, times = n_slots)

# Apply eligibility by setting objective to very negative for ineligible assignments
obj_penalized <- obj
for (s in 1:n_slots) {
  col_indices <- (s - 1) * n_players + (1:n_players)
  ineligible <- which(eligibility[, s] == 0)
  obj_penalized[col_indices[ineligible]] <- -1e8
}

# Build constraint matrix
# Variables are ordered as: player 1 in all slots, player 2 in all slots, etc.
# So variable index for player i in slot s is: (s-1) * n_players + i

# Constraint 1: Each player in at most one slot (n_players constraints)
# For player i: sum over all s of x[i,s] <= 1
player_constraints <- matrix(0, nrow = n_players, ncol = n_vars)
for (i in 1:n_players) {
  for (s in 1:n_slots) {
    var_idx <- (s - 1) * n_players + i
    player_constraints[i, var_idx] <- 1
  }
}

# Constraint 2: Each slot filled by exactly one player (n_slots constraints)
# For slot s: sum over all i of x[i,s] = 1
slot_constraints <- matrix(0, nrow = n_slots, ncol = n_vars)
for (s in 1:n_slots) {
  var_indices <- (s - 1) * n_players + (1:n_players)
  slot_constraints[s, var_indices] <- 1
}

# Combine all constraints
constraint_matrix <- rbind(player_constraints, slot_constraints)
constraint_dir <- c(rep("<=", n_players), rep("=", n_slots))
constraint_rhs <- c(rep(1, n_players), rep(1, n_slots))

message(sprintf("Constraint matrix: %d constraints x %d variables", 
                nrow(constraint_matrix), ncol(constraint_matrix)))

# --- Solve optimization problem ---
message("\nSolving integer linear program...")
message("This may take several minutes...")

start_time <- Sys.time()

result <- lp(
  direction = "max",
  objective.in = obj_penalized,
  const.mat = constraint_matrix,
  const.dir = constraint_dir,
  const.rhs = constraint_rhs,
  all.bin = TRUE  # Binary variables only
)

end_time <- Sys.time()
elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

if (result$status != 0) {
  stop(sprintf("Optimization failed with status %d", result$status))
}

message(sprintf("✓ Optimization successful! (%.1f seconds)", elapsed))
message(sprintf("Total SGP: %.1f", result$objval))

# --- Extract solution ---
message("\nExtracting optimal roster assignments...")

solution <- result$solution
assignments <- which(solution == 1)

# Convert flat indices back to (player, slot) pairs
player_indices <- ((assignments - 1) %% n_players) + 1
slot_indices <- ((assignments - 1) %/% n_players) + 1

optimal_rosters <- tibble(
  player_id = player_indices,
  slot_id = slot_indices,
  Name = all_players$Name[player_indices],
  Team = all_players$Team[player_indices],
  sgp_total = all_players$sgp_total[player_indices],
  slot_type = slots[slot_indices]
)

message(sprintf("Assigned %d players to %d slots", 
                length(unique(optimal_rosters$Name)), nrow(optimal_rosters)))

# --- Assign to fantasy teams ---
# Distribute evenly within each position (round-robin by value)
optimal_rosters <- optimal_rosters %>%
  group_by(slot_type) %>%
  arrange(desc(sgp_total)) %>%
  mutate(fantasy_team = ((row_number() - 1) %% n_teams) + 1) %>%
  ungroup() %>%
  arrange(fantasy_team, slot_type, desc(sgp_total))

# --- Summary statistics ---
message("\n=== Optimization Results ===\n")

# By position
message("Position Summary:")
pos_summary <- optimal_rosters %>%
  group_by(slot_type) %>%
  summarise(
    count = n(),
    total_sgp = round(sum(sgp_total), 1),
    avg_sgp = round(mean(sgp_total), 2),
    min_sgp = round(min(sgp_total), 2),
    max_sgp = round(max(sgp_total), 2)
  ) %>%
  arrange(desc(avg_sgp))

print(pos_summary)

# By team
message("\nTeam Summary:")
team_summary <- optimal_rosters %>%
  group_by(fantasy_team) %>%
  summarise(
    players = n(),
    total_sgp = round(sum(sgp_total), 1),
    avg_sgp = round(mean(sgp_total), 2)
  ) %>%
  arrange(desc(total_sgp))

print(team_summary)

# Top players
message("\nTop 10 Players Drafted:")
optimal_rosters %>%
  arrange(desc(sgp_total)) %>%
  head(10) %>%
  select(Name, Team, slot_type, sgp_total, fantasy_team) %>%
  print()

# --- Calculate true replacement levels ---
message("\n=== Calculating Replacement Levels ===\n")

drafted_names <- optimal_rosters$Name

replacement_levels <- tibble()

# Helper function to get best undrafted at position
get_replacement <- function(position_name, filter_expr) {
  all_players %>%
    filter({{filter_expr}}, !Name %in% drafted_names) %>%
    arrange(desc(sgp_total)) %>%
    slice(1) %>%
    mutate(position = position_name) %>%
    select(position, Name, Team, sgp_total)
}

# Calculate for each position
replacement_levels <- bind_rows(
  get_replacement("C", p_c == 1),
  get_replacement("1B", p_1b == 1),
  get_replacement("2B", p_2b == 1),
  get_replacement("3B", p_3b == 1),
  get_replacement("SS", p_ss == 1),
  get_replacement("OF", p_of == 1),
  get_replacement("MI", p_mi == 1),
  get_replacement("CI", p_ci == 1),
  get_replacement("Util", !is_pitcher),
  get_replacement("P", is_pitcher)
)

message("Best Undrafted Player at Each Position:")
print(replacement_levels)

# --- Save results ---
write_csv(optimal_rosters, "data/processed/optimal_rosters_sgp.csv")
write_csv(replacement_levels, "data/processed/replacement_levels_sgp.csv")
write_csv(pos_summary, "data/processed/position_summary_sgp.csv")
write_csv(team_summary, "data/processed/team_summary_sgp.csv")

message("\n✓ Results saved to data/processed/")
message("  - optimal_rosters_sgp.csv (all assignments)")
message("  - replacement_levels_sgp.csv (best undrafted by position)")
message("  - position_summary_sgp.csv")
message("  - team_summary_sgp.csv")
