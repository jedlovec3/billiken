# scripts/calculate_player_value.r
# Calculate SGP Above Replacement (SGPAR) and standings-based $ values.
#
# Inputs:
# - data/processed/player_sgp.csv
# - data/processed/replacement_levels_sgp.csv
# - data/processed/optimal_rosters_sgp.csv
#
# Output:
# - data/processed/projected_player_value.csv

suppressPackageStartupMessages({
  library(tidyverse)
})

options(digits = 15, scipen = 999)

# --- Parameters ---
N_TEAMS <- 10
BUDGET_PER_TEAM <- 270
CATEGORIES <- c("R", "RBI", "HR", "SB", "AVG", "W", "SV", "SO", "ERA", "WHIP")
DOLLARS_PER_CATEGORY <- (N_TEAMS * BUDGET_PER_TEAM) / length(CATEGORIES)  # 270

# --- Load data ---
message("Loading inputs...")
players <- read_csv("data/processed/player_sgp.csv", show_col_types = FALSE) %>%
  mutate(across(starts_with("p_"), ~replace_na(.x, 0)))

replacement_levels <- read_csv("data/processed/replacement_levels_sgp.csv", show_col_types = FALSE)
optimal_rosters <- read_csv("data/processed/optimal_rosters_sgp.csv", show_col_types = FALSE)

# --- Determine replacement level by player's eligible positions ---
# Notes:
# - We treat Util as eligible for all hitters (and two-way players), consistent with roster construction.
# - We treat P as eligible for all pitchers (and two-way players).
message("Determining replacement level per player (min repl_sgp_total among eligible positions)...")

player_positions_long <- players %>%
  mutate(
    is_hitter = player_type %in% c("hitter", "two-way"),
    is_pitcher = player_type %in% c("pitcher", "two-way"),
    pos_C = as.integer(p_c == 1),
    pos_1B = as.integer(p_1b == 1),
    pos_2B = as.integer(p_2b == 1),
    pos_3B = as.integer(p_3b == 1),
    pos_SS = as.integer(p_ss == 1),
    pos_OF = as.integer(p_of == 1),
    pos_MI = as.integer(p_mi == 1),
    pos_CI = as.integer(p_ci == 1),
    pos_Util = as.integer(is_hitter),
    pos_P = as.integer(is_pitcher)
  ) %>%
  select(Name, Team, player_type, starts_with("pos_")) %>%
  pivot_longer(
    cols = starts_with("pos_"),
    names_to = "position",
    values_to = "eligible"
  ) %>%
  filter(eligible == 1) %>%
  mutate(position = str_remove(position, "^pos_"))

# Join to replacement levels and choose the eligible position with the lowest replacement level
player_replacement <- player_positions_long %>%
  inner_join(replacement_levels, by = "position") %>%
  group_by(Name, Team) %>%
  slice_min(repl_sgp_total, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    Name,
    Team,
    replacement_position = position,
    replacement_level_sgp = repl_sgp_total,
    repl_sgp_total,
    repl_sgp_R,
    repl_sgp_HR,
    repl_sgp_RBI,
    repl_sgp_SB,
    repl_sgp_AVG,
    repl_sgp_W,
    repl_sgp_SV,
    repl_sgp_SO,
    repl_sgp_ERA,
    repl_sgp_WHIP
  )

players_ar <- players %>%
  left_join(player_replacement, by = c("Name", "Team")) %>%
  mutate(
    # Category SGPAR (SGP Above Replacement)
    sgpar_R = sgp_R - repl_sgp_R,
    sgpar_HR = sgp_HR - repl_sgp_HR,
    sgpar_RBI = sgp_RBI - repl_sgp_RBI,
    sgpar_SB = sgp_SB - repl_sgp_SB,
    sgpar_AVG = sgp_AVG - repl_sgp_AVG,
    sgpar_W = sgp_W - repl_sgp_W,
    sgpar_SV = sgp_SV - repl_sgp_SV,
    sgpar_SO = sgp_SO - repl_sgp_SO,
    sgpar_ERA = sgp_ERA - repl_sgp_ERA,
    sgpar_WHIP = sgp_WHIP - repl_sgp_WHIP,
    # Totals
    sgpar_hitting = sgpar_R + sgpar_HR + sgpar_RBI + sgpar_SB + sgpar_AVG,
    sgpar_pitching = sgpar_W + sgpar_SV + sgpar_SO + sgpar_ERA + sgpar_WHIP,
    sgpar_total = sgpar_hitting + sgpar_pitching,
    sgpar = sgpar_total
  )

if (any(is.na(players_ar$replacement_level_sgp))) {
  warning("Some players did not receive a replacement level; check position flags / replacement_levels_sgp.csv.")
}

# --- Compute $ per SGPAR from optimized rosters ---
# Use the 230 players on the optimized rosters to set $/SGPAR in each category.
message("Computing category $ per SGPAR from optimal rosters...")

rostered_players <- optimal_rosters %>%
  select(Name, Team) %>%
  distinct() %>%
  inner_join(players_ar, by = c("Name", "Team"))

if (nrow(rostered_players) != nrow(optimal_rosters %>% distinct(Name, Team))) {
  warning("Some players in optimal_rosters_sgp.csv did not match player_sgp.csv by (Name, Team).")
}

sgpar_cols <- paste0("sgpar_", CATEGORIES)

sgpar_totals <- rostered_players %>%
  summarise(across(all_of(sgpar_cols), ~sum(.x, na.rm = TRUE)))

sgpar_totals_vec <- unlist(sgpar_totals, use.names = TRUE)

if (any(sgpar_totals_vec == 0)) {
  stop("At least one category has total SGPAR of 0 across the optimized rosters; cannot compute $ per SGPAR.")
}

dollars_per_sgpar <- DOLLARS_PER_CATEGORY / sgpar_totals_vec
names(dollars_per_sgpar) <- str_remove(names(dollars_per_sgpar), "^sgpar_")

dollars_per_sgpar_tbl <- tibble(
  category = names(dollars_per_sgpar),
  dollars_allocated = DOLLARS_PER_CATEGORY,
  total_sgpar = as.numeric(sgpar_totals_vec[paste0("sgpar_", category)]),
  dollars_per_sgpar = as.numeric(dollars_per_sgpar)
) %>%
  arrange(match(category, CATEGORIES))

message("$/SGPAR by category (from optimized rosters):")
print(dollars_per_sgpar_tbl)

# --- Apply $ per SGPAR to all players ---
message("Calculating player $ values...")

players_value <- players_ar %>%
  mutate(
    `$_R` = sgpar_R * dollars_per_sgpar["R"],
    `$_RBI` = sgpar_RBI * dollars_per_sgpar["RBI"],
    `$_HR` = sgpar_HR * dollars_per_sgpar["HR"],
    `$_SB` = sgpar_SB * dollars_per_sgpar["SB"],
    `$_AVG` = sgpar_AVG * dollars_per_sgpar["AVG"],
    `$_W` = sgpar_W * dollars_per_sgpar["W"],
    `$_SV` = sgpar_SV * dollars_per_sgpar["SV"],
    `$_SO` = sgpar_SO * dollars_per_sgpar["SO"],
    `$_ERA` = sgpar_ERA * dollars_per_sgpar["ERA"],
    `$_WHIP` = sgpar_WHIP * dollars_per_sgpar["WHIP"]
  ) %>%
  mutate(
    `$_cat` = rowSums(across(all_of(paste0("$_", CATEGORIES))), na.rm = TRUE),
    standings_value = `$_cat`
  ) %>%
  arrange(desc(standings_value))

# --- Export ---
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(players_value, "data/processed/projected_player_value.csv")
message("✓ Exported projected player values to data/processed/projected_player_value.csv")

invisible(players_value)
