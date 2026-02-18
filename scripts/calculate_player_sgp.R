# scripts/calculate_player_sgp.R
# Calculate Standings Gained Points (SGP) for individual players
# using unit values derived from historical standings.

suppressPackageStartupMessages({
  library(tidyverse)
  library(fuzzyjoin)
})

# --- Parameters ---
projections_year <- Sys.getenv("BILLIKEN_PROJECTIONS_YEAR", unset = "2026")

# NL teams filter (league is NL-only)
NL_TEAMS <- c('ATL','LAD','SDP','ARI','NYM','PHI','MIL','STL','CHC','SFG','CIN','COL','PIT','MIA','WSN', NA)

# --- Load Unit Values ---
message("Loading unit values from standings analysis...")

unit_values <- read_csv("data/processed/category_unit_values.csv", show_col_types = FALSE)
category_scaling <- read_csv("data/processed/category_value_scaling.csv", show_col_types = FALSE)

# Extract unit values into named list for easy access
uv <- setNames(unit_values$`Unit Value`, unit_values$Category)

# Extract replacement levels for rate stats
repl_AVG <- category_scaling %>% filter(Category == "AVG") %>% pull(`Replacement Value`)
repl_ERA <- category_scaling %>% filter(Category == "ERA") %>% pull(`Replacement Value`)
repl_WHIP <- category_scaling %>% filter(Category == "WHIP") %>% pull(`Replacement Value`)

message(sprintf("Unit values loaded for %d categories", length(uv)))
message(sprintf("Rate stat replacement levels: AVG=%.4f, ERA=%.2f, WHIP=%.2f", 
                repl_AVG, repl_ERA, repl_WHIP))

# --- Load Projections ---
message(sprintf("\nLoading %s projections...", projections_year))

hitter_projections <- read_csv(paste0("data/raw/hitter_projections_", projections_year, ".csv"), 
                                show_col_types = FALSE) %>% 
  mutate(Name = stringi::stri_trans_general(Name, "Latin-ASCII")) %>%
  filter(Team %in% NL_TEAMS) %>%
  mutate(player_type = "hitter")

pitcher_projections <- read_csv(paste0("data/raw/pitcher_projections_", projections_year, ".csv"), 
                                 show_col_types = FALSE) %>%
  mutate(Name = stringi::stri_trans_general(Name, "Latin-ASCII")) %>%
  filter(Team %in% NL_TEAMS) %>%
  mutate(player_type = "pitcher")

message(sprintf("Loaded %d hitter projections", nrow(hitter_projections)))
message(sprintf("Loaded %d pitcher projections", nrow(pitcher_projections)))

# --- Load Positions ---
message("\nLoading positions...")

positions <- read_csv("data/raw/positions_latest.csv", show_col_types = FALSE) %>%
  mutate(p_of = case_when(RF == 1 ~ 1, CF == 1 ~ 1, LF == 1 ~ 1, .default = 0)) %>%
  mutate(p_ci = case_when(`1B` == 1 ~ 1, `3B` == 1 ~ 1, .default = 0)) %>%
  mutate(p_mi = case_when(`2B` == 1 ~ 1, SS == 1 ~ 1, .default = 0)) %>%  
  mutate(p_sp = SP, p_rp = RP) %>%
  rename(player = PLAYER, p_c = C, p_1b = `1B`, p_2b = `2B`, p_3b = `3B`, p_ss = SS, p_dh = DH) %>% 
  select(player, p_c, p_1b, p_2b, p_3b, p_ss, p_of, p_ci, p_mi, p_dh, p_sp, p_rp)

message(sprintf("Loaded %d player positions", nrow(positions)))

# --- Normalize Names for Matching ---
# Handle common name variations (e.g., "Luis Robert Jr." vs "Luis Robert")
normalize_name <- function(name) {
  name %>%
    str_replace_all(" Jr\\.?$", "") %>%
    str_replace_all(" Sr\\.?$", "") %>%
    str_replace_all(" III$", "") %>%
    str_replace_all(" II$", "") %>%
    str_trim()
}

hitter_projections <- hitter_projections %>%
  mutate(name_normalized = normalize_name(Name))

pitcher_projections <- pitcher_projections %>%
  mutate(name_normalized = normalize_name(Name))

positions <- positions %>%
  mutate(name_normalized = normalize_name(player))

# --- Join Hitters with Positions ---
message("\nJoining projections with positions...")

# Try exact match first, then fuzzy match for unmatched
# Handle cases where multiple position entries match (e.g., Luis Garcia vs Luis Garcia Jr.)
# Take first match for each player
hitters_exact <- hitter_projections %>%
  inner_join(positions, by = "name_normalized") %>%
  group_by(Name, Team, player_type) %>%
  slice(1) %>%
  ungroup()

hitters_unmatched <- hitter_projections %>%
  anti_join(positions, by = "name_normalized")

if (nrow(hitters_unmatched) > 0) {
  hitters_fuzzy <- hitters_unmatched %>%
    stringdist_left_join(positions, by = c("name_normalized" = "name_normalized"), 
                         max_dist = 2, distance_col = "dist") %>%
    group_by(Name) %>%
    slice_min(dist, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-dist) %>%
    rename(name_normalized = name_normalized.x) %>%
    select(-name_normalized.y)
  
  hitters_with_pos <- bind_rows(hitters_exact, hitters_fuzzy)
} else {
  hitters_with_pos <- hitters_exact
}

hitters_with_pos <- hitters_with_pos %>% 
  select(-name_normalized, -any_of("player"))

# --- Join Pitchers with Positions ---
# Handle cases where multiple position entries match
# Take first match for each player
pitchers_exact <- pitcher_projections %>%
  inner_join(positions, by = "name_normalized") %>%
  group_by(Name, Team, player_type) %>%
  slice(1) %>%
  ungroup()

pitchers_unmatched <- pitcher_projections %>%
  anti_join(positions, by = "name_normalized")

if (nrow(pitchers_unmatched) > 0) {
  pitchers_fuzzy <- pitchers_unmatched %>%
    stringdist_left_join(positions, by = c("name_normalized" = "name_normalized"), 
                         max_dist = 2, distance_col = "dist") %>%
    group_by(Name) %>%
    slice_min(dist, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-dist) %>%
    rename(name_normalized = name_normalized.x) %>%
    select(-name_normalized.y)
  
  pitchers_with_pos <- bind_rows(pitchers_exact, pitchers_fuzzy)
} else {
  pitchers_with_pos <- pitchers_exact
}

pitchers_with_pos <- pitchers_with_pos %>% 
  select(-name_normalized, -any_of("player"))

message(sprintf("Hitters with positions: %d", nrow(hitters_with_pos)))
message(sprintf("Pitchers with positions: %d", nrow(pitchers_with_pos)))

# --- Calculate SGP for Hitters ---
message("\nCalculating SGP for hitters...")

hitters_sgp <- hitters_with_pos %>%
  mutate(
    # Counting stats: direct multiplication
    sgp_R = R * uv["R"],
    sgp_HR = HR * uv["HR"],
    sgp_RBI = RBI * uv["RBI"],
    sgp_SB = SB * uv["SB"],
    # Rate stat: AVG contribution = (AVG - repl_AVG) * AB * unit_value_per_H
    sgp_AVG = (AVG - repl_AVG) * AB * uv["AVG"],
    # Total hitter SGP
    sgp_hitting = sgp_R + sgp_HR + sgp_RBI + sgp_SB + sgp_AVG,
    # No pitching contribution for hitters (unless they're Ohtani)
    sgp_pitching = 0,
    sgp_total = sgp_hitting
  )

# --- Calculate SGP for Pitchers ---
message("Calculating SGP for pitchers...")

pitchers_sgp <- pitchers_with_pos %>%
  mutate(
    # Counting stats: direct multiplication
    sgp_W = W * uv["W"],
    sgp_SV = SV * uv["SV"],
    sgp_SO = SO * uv["SO"],
    # Rate stats: (repl - player) * IP * unit_value (lower is better, so ER/WH prevented)
    # ERA: ER per 9 IP, so ER = ERA * IP / 9
    # ER prevented = (repl_ERA - ERA) * IP / 9
    sgp_ERA = (repl_ERA - ERA) * IP / 9 * uv["ERA"],
    # WHIP: WH per IP, so WH = WHIP * IP
    # WH prevented = (repl_WHIP - WHIP) * IP
    sgp_WHIP = (repl_WHIP - WHIP) * IP * uv["WHIP"],
    # Total pitcher SGP
    sgp_pitching = sgp_W + sgp_SV + sgp_SO + sgp_ERA + sgp_WHIP,
    # No hitting contribution for pitchers
    sgp_hitting = 0,
    sgp_total = sgp_pitching
  )

# --- Handle Shohei Ohtani (two-way player) ---
message("\nHandling two-way players (Shohei Ohtani)...")

# Check if Ohtani is in both projections
ohtani_hitter <- hitters_sgp %>% filter(str_detect(Name, "Shohei Ohtani"))
ohtani_pitcher <- pitchers_sgp %>% filter(str_detect(Name, "Shohei Ohtani"))

if (nrow(ohtani_hitter) > 0 && nrow(ohtani_pitcher) > 0) {
  message("Found Shohei Ohtani in both hitter and pitcher projections - combining...")
  
  # Combine Ohtani's hitting and pitching SGP
  ohtani_combined <- ohtani_hitter %>%
    mutate(
      sgp_W = ohtani_pitcher$sgp_W[1],
      sgp_SV = ohtani_pitcher$sgp_SV[1],
      sgp_SO = ohtani_pitcher$sgp_SO[1],
      sgp_ERA = ohtani_pitcher$sgp_ERA[1],
      sgp_WHIP = ohtani_pitcher$sgp_WHIP[1],
      sgp_pitching = ohtani_pitcher$sgp_pitching[1],
      sgp_total = sgp_hitting + sgp_pitching,
      # Add pitching stats
      W = ohtani_pitcher$W[1],
      SV = ohtani_pitcher$SV[1],
      SO = ohtani_pitcher$SO[1],
      IP = ohtani_pitcher$IP[1],
      ERA = ohtani_pitcher$ERA[1],
      WHIP = ohtani_pitcher$WHIP[1],
      # Mark as two-way
      player_type = "two-way"
    )
  
  # Remove Ohtani from hitters and add combined version
  hitters_sgp <- hitters_sgp %>% 
    filter(!str_detect(Name, "Shohei Ohtani")) %>%
    bind_rows(ohtani_combined)
  
  # Remove Ohtani from pitchers (he's now in hitters as two-way)
  pitchers_sgp <- pitchers_sgp %>% 
    filter(!str_detect(Name, "Shohei Ohtani"))
}

# --- Combine All Players ---
message("\nCombining all players...")

# Select common columns for combining
hitter_cols <- c("Name", "Team", "player_type", 
                 "PA", "AB", "H", "HR", "R", "RBI", "SB", "AVG",
                 "sgp_R", "sgp_HR", "sgp_RBI", "sgp_SB", "sgp_AVG", "sgp_hitting",
                 "p_c", "p_1b", "p_2b", "p_3b", "p_ss", "p_of", "p_ci", "p_mi", "p_dh", "p_sp", "p_rp")

pitcher_cols <- c("Name", "Team", "player_type",
                  "IP", "W", "SV", "SO", "ERA", "WHIP",
                  "sgp_W", "sgp_SV", "sgp_SO", "sgp_ERA", "sgp_WHIP", "sgp_pitching",
                  "p_c", "p_1b", "p_2b", "p_3b", "p_ss", "p_of", "p_ci", "p_mi", "p_dh", "p_sp", "p_rp")

# Add missing columns
hitters_final <- hitters_sgp %>%
  mutate(
    IP = NA_real_, W = NA_real_, SV = NA_real_, 
    sgp_W = 0, sgp_SV = 0, sgp_SO = 0, sgp_ERA = 0, sgp_WHIP = 0
  ) %>%
  # Keep pitching stats for Ohtani
  mutate(
    IP = ifelse(player_type == "two-way", IP, NA_real_),
    W = ifelse(player_type == "two-way", W, NA_real_),
    SV = ifelse(player_type == "two-way", SV, NA_real_),
    SO = ifelse(player_type == "two-way", SO, NA_real_),
    ERA = ifelse(player_type == "two-way", ERA, NA_real_),
    WHIP = ifelse(player_type == "two-way", WHIP, NA_real_),
    sgp_W = ifelse(player_type == "two-way", sgp_W, 0),
    sgp_SV = ifelse(player_type == "two-way", sgp_SV, 0),
    sgp_SO = ifelse(player_type == "two-way", sgp_SO, 0),
    sgp_ERA = ifelse(player_type == "two-way", sgp_ERA, 0),
    sgp_WHIP = ifelse(player_type == "two-way", sgp_WHIP, 0),
    sgp_pitching = ifelse(player_type == "two-way", sgp_pitching, 0)
  ) %>%
  mutate(sgp_total = sgp_hitting + sgp_pitching)

pitchers_final <- pitchers_sgp %>%
  mutate(
    PA = NA_real_, AB = NA_real_, H = NA_real_, HR = NA_real_, 
    R = NA_real_, RBI = NA_real_, SB = NA_real_, AVG = NA_real_,
    sgp_R = 0, sgp_HR = 0, sgp_RBI = 0, sgp_SB = 0, sgp_AVG = 0, sgp_hitting = 0
  ) %>%
  mutate(sgp_total = sgp_hitting + sgp_pitching)

# Combine
all_players <- bind_rows(
  hitters_final %>% select(Name, Team, player_type, PA, AB, H, HR, R, RBI, SB, AVG,
                           IP, W, SV, SO, ERA, WHIP,
                           sgp_R, sgp_HR, sgp_RBI, sgp_SB, sgp_AVG, sgp_hitting,
                           sgp_W, sgp_SV, sgp_SO, sgp_ERA, sgp_WHIP, sgp_pitching,
                           sgp_total,
                           p_c, p_1b, p_2b, p_3b, p_ss, p_of, p_ci, p_mi, p_dh, p_sp, p_rp),
  pitchers_final %>% select(Name, Team, player_type, PA, AB, H, HR, R, RBI, SB, AVG,
                            IP, W, SV, SO, ERA, WHIP,
                            sgp_R, sgp_HR, sgp_RBI, sgp_SB, sgp_AVG, sgp_hitting,
                            sgp_W, sgp_SV, sgp_SO, sgp_ERA, sgp_WHIP, sgp_pitching,
                            sgp_total,
                            p_c, p_1b, p_2b, p_3b, p_ss, p_of, p_ci, p_mi, p_dh, p_sp, p_rp)
) %>%
  arrange(desc(sgp_total))

message(sprintf("\nTotal players: %d", nrow(all_players)))
message(sprintf("  Hitters: %d", sum(all_players$player_type == "hitter")))
message(sprintf("  Pitchers: %d", sum(all_players$player_type == "pitcher")))
message(sprintf("  Two-way: %d", sum(all_players$player_type == "two-way")))

# Verify count matches original projections
original_count <- nrow(hitter_projections) + nrow(pitcher_projections) - 
                  (nrow(ohtani_hitter) > 0 && nrow(ohtani_pitcher) > 0)  # Subtract 1 if Ohtani was in both
message(sprintf("  (Expected from projections: %d hitters + %d pitchers)", 
                nrow(hitter_projections), nrow(pitcher_projections)))

# --- Show Top Players by Position ---
message("\n=== Top SGP Players by Position ===\n")

show_top_at_position <- function(data, pos_col, pos_name, n = 5) {
  top_players <- data %>%
    filter(get(pos_col) == 1 | is.na(get(pos_col))) %>%
    filter(if (pos_col %in% c("p_sp", "p_rp")) player_type %in% c("pitcher", "two-way") 
           else player_type %in% c("hitter", "two-way")) %>%
    arrange(desc(sgp_total)) %>%
    head(n)
  
  cat(sprintf("\n%s (top %d):\n", pos_name, n))
  for (i in 1:nrow(top_players)) {
    p <- top_players[i, ]
    if (p$player_type %in% c("hitter", "two-way") && !is.na(p$PA)) {
      cat(sprintf("  %d. %s (%s) - SGP: %.2f (R:%.2f HR:%.2f RBI:%.2f SB:%.2f AVG:%.2f)\n",
                  i, p$Name, p$Team, p$sgp_total,
                  p$sgp_R, p$sgp_HR, p$sgp_RBI, p$sgp_SB, p$sgp_AVG))
    } else {
      cat(sprintf("  %d. %s (%s) - SGP: %.2f (W:%.2f SV:%.2f SO:%.2f ERA:%.2f WHIP:%.2f)\n",
                  i, p$Name, p$Team, p$sgp_total,
                  p$sgp_W, p$sgp_SV, p$sgp_SO, p$sgp_ERA, p$sgp_WHIP))
    }
  }
}

# Show top players at each position
show_top_at_position(all_players, "p_c", "Catcher (C)")
show_top_at_position(all_players, "p_1b", "First Base (1B)")
show_top_at_position(all_players, "p_2b", "Second Base (2B)")
show_top_at_position(all_players, "p_3b", "Third Base (3B)")
show_top_at_position(all_players, "p_ss", "Shortstop (SS)")
show_top_at_position(all_players, "p_of", "Outfield (OF)")
show_top_at_position(all_players, "p_sp", "Starting Pitcher (SP)")
show_top_at_position(all_players, "p_rp", "Relief Pitcher (RP)")

# Also show overall top 10
cat("\n\nOverall Top 10 SGP:\n")
top10 <- all_players %>% head(10)
for (i in 1:nrow(top10)) {
  p <- top10[i, ]
  cat(sprintf("  %d. %s (%s, %s) - SGP: %.2f (hitting: %.2f, pitching: %.2f)\n",
              i, p$Name, p$Team, p$player_type, p$sgp_total, p$sgp_hitting, p$sgp_pitching))
}

# --- Export Results ---
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(all_players, "data/processed/player_sgp.csv")
message("\nExported player SGP to data/processed/player_sgp.csv")

# Return for interactive use
invisible(all_players)
