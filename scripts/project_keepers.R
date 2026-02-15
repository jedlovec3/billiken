# project_keepers.R
# Project keepers for every Billiken League team based on expected value
# and export draft-eligible players

# Load libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(fuzzyjoin)
})

# Set projections year
projections_year <- Sys.getenv("BILLIKEN_PROJECTIONS_YEAR", unset = "2026")

# --- Load Data ---
message("Loading data...")

# Load pre-freeze rosters from local CSV
prefreeze_rosters <- read_csv("data/raw/prefreeze_rosters_latest.csv", show_col_types = FALSE) %>% 
  filter(!is.na(player)) %>% 
  mutate(across(c("salary"), ~gsub("\\$", "", .) %>% as.numeric))

# Load salaries from local CSV
salaries <- read_csv("data/raw/salaries_latest.csv", show_col_types = FALSE) %>%
  rename(new_salary = Salary) %>% 
  filter(!is.na(Player)) %>%  
  mutate(across(c("new_salary"), ~gsub("\\$", "", .) %>% as.numeric))

# Read positions from ESPN API
positions <- read_csv("data/raw/positions_latest.csv", show_col_types = FALSE) %>%
  mutate(p_of = case_when(RF == 1 ~ 1, CF == 1 ~ 1, LF == 1 ~ 1, .default = 0)) %>%
  mutate(p_ci = case_when(`1B` == 1 ~ 1, `3B` == 1 ~ 1, .default = 0)) %>%
  mutate(p_mi = case_when(`2B` == 1 ~ 1, SS == 1 ~ 1, .default = 0)) %>%  
  rename(player = PLAYER, p_c = C, p_1b = `1B`, p_2b = `2B`, p_3b = `3B`, p_ss = SS) %>% 
  select(player, p_c, p_1b, p_2b, p_3b, p_ss, p_of, p_ci, p_mi)

# Load FanGraphs projections
hitter_projections <- read_csv(paste0("hitter_projections_", projections_year, ".csv"), show_col_types = FALSE) %>% 
  mutate(Name = stringi::stri_trans_general(Name, "Latin-ASCII"))

pitcher_projections <- read_csv(paste0("pitcher_projections_", projections_year, ".csv"), show_col_types = FALSE) %>%
  mutate(Name = stringi::stri_trans_general(Name, "Latin-ASCII"))

# --- Calculate Team Totals ---
message("Calculating team totals...")

hitter_team_totals <- hitter_projections %>% 
  filter(Team %in% c('ATL','LAD','SDP','ARI','NYM','PHI','MIL','STL','CHC','SFG','CIN','COL','PIT','MIA','WSN','NA')) %>%
  stringdist_left_join(prefreeze_rosters, by = c("Name" = "player"), max_dist = 2) %>% 
  group_by(billikenTeam) %>% 
  summarize(n=n(), PA = sum(PA), AB = sum(AB), H = sum(H), HR = sum(HR), R = sum(R), RBI = sum(RBI), SB = sum(SB), AVG = sum(H)/sum(AB))

pitcher_team_totals <- pitcher_projections %>% 
  filter(Team %in% c('ATL','LAD','SDP','ARI','NYM','PHI','MIL','STL','CHC','SFG','CIN','COL','PIT','MIA','WSN','NA')) %>%
  stringdist_left_join(prefreeze_rosters, by = c("Name" = "player"), max_dist = 2) %>% 
  group_by(billikenTeam) %>% 
  summarize(n=n(), W = sum(W), SV = sum(SV), IP = sum(IP), SO = sum(SO), ER = sum(ER), H = sum(H), BB = sum(BB), ERA = sum(ER)*9/sum(IP), WHIP = (sum(H)+sum(BB))/sum(IP))

# --- Calculate Team Standings ---
message("Calculating team standings...")

n_teams <- pull(count(hitter_team_totals %>% filter(!is.na(billikenTeam)) %>% distinct(billikenTeam)))

hitter_points <- hitter_team_totals %>% 
  filter(!is.na(billikenTeam)) %>% 
  mutate(hr = n_teams+1 - dense_rank(desc(HR)), r = n_teams+1 - dense_rank(desc(R)), rbi = n_teams+1 - dense_rank(desc(RBI)), sb = n_teams+1 - dense_rank(desc(SB)), avg = n_teams+1 - dense_rank(desc(AVG))) %>% 
  mutate(hr_pct = (hr-1)/(n_teams-1), r_pct = (r-1)/(n_teams-1), rbi_pct = (rbi-1)/(n_teams-1), sb_pct = (sb-1)/(n_teams-1), avg_pct = (avg-1)/(n_teams-1)) %>% 
  mutate(hit = hr + r + rbi + sb + avg)

pitcher_points <- pitcher_team_totals %>% 
  filter(!is.na(billikenTeam)) %>% 
  mutate(w = n_teams+1 - dense_rank(desc(W)), sv = n_teams+1 - dense_rank(desc(SV)), so = n_teams+1 - dense_rank(desc(SO)), era = n_teams+1 - dense_rank(ERA), whip = n_teams+1 - dense_rank(WHIP)) %>%
  mutate(w_pct = (w-1)/(n_teams-1), sv_pct = (sv-1)/(n_teams-1), so_pct = (so-1)/(n_teams-1), era_pct = (era-1)/(n_teams-1), whip_pct = (whip-1)/(n_teams-1)) %>%
  mutate(pit = w + sv + so + era + whip)

# --- Build Models ---
message("Building statistical models...")

# Linear models by category
hr_model <- lm(hr ~ HR, hitter_points) 
r_model <- lm(r ~ R, hitter_points) 
rbi_model <- lm(rbi ~ RBI, hitter_points) 
sb_model <- lm(sb ~ SB, hitter_points) 
avg_model <- lm(avg ~ AVG, hitter_points) 

w_model <- lm(w ~ W, pitcher_points) 
sv_model <- lm(sv ~ SV, pitcher_points) 
so_model <- lm(so ~ SO, pitcher_points) 
era_model <- lm(era ~ ERA, pitcher_points) 
whip_model <- lm(whip ~ WHIP, pitcher_points) 

# Extract coefficients
hr_factor <- hr_model$coefficients["HR"]
r_factor <- r_model$coefficients["R"]
rbi_factor <- rbi_model$coefficients["RBI"]
sb_factor <- sb_model$coefficients["SB"]
avg_factor <- avg_model$coefficients["AVG"]

w_factor <- w_model$coefficients["W"]
sv_factor <- sv_model$coefficients["SV"]
so_factor <- so_model$coefficients["SO"]
era_factor <- era_model$coefficients["ERA"]
whip_factor <- whip_model$coefficients["WHIP"]

# Baseline stats
baseline_ab <- 5000
baseline_avg <- .255
baseline_h <- baseline_ab*baseline_avg

baseline_ip <- 1200
baseline_era <- 4.05
baseline_whip <- 1.24
baseline_er <- baseline_ip*baseline_era/9
baseline_wh <- baseline_ip*baseline_whip

# --- Calculate Point Values ---
message("Calculating player point values...")

hitter_projections <- hitter_projections %>% 
  mutate(point_value = round(HR * hr_factor + R * r_factor + RBI * rbi_factor + SB * sb_factor + avg_factor * ((baseline_h + H)/(baseline_ab + AB) - baseline_h/baseline_ab),1))

pitcher_projections <- pitcher_projections %>% 
  mutate(point_value = round(W * w_factor + SV * sv_factor + SO * so_factor + era_factor * (9*(baseline_er + ER)/(baseline_ip + IP) - 9*baseline_er/baseline_ip) + whip_factor * ((baseline_wh + BB + H)/(baseline_ip + IP) - baseline_wh/baseline_ip),1))

# --- Create Projected Players ---
message("Creating projected players dataframe...")

projected_players <- bind_rows(hitter_projections, pitcher_projections) %>% 
  filter(Team %in% c('ATL','LAD','SDP','ARI','NYM','PHI','MIL','STL','CHC','SFG','CIN','COL','PIT','MIA','WSN','NA')) %>%
  stringdist_left_join(prefreeze_rosters, by = c("Name" = "player"), max_dist = 2) %>% 
  stringdist_left_join(positions, by = c("Name" = "player"), max_dist = 2) %>% 
  stringdist_left_join(salaries, by = c("Name" = "Player"), max_dist = 2) %>% 
  mutate(salary = case_when(!is.na(billikenTeam) ~ salary, TRUE ~ new_salary)) %>% 
  mutate(AVG = round(AVG,3), ERA = round(ERA,2), WHIP = round(WHIP,2), SO = case_when(IP == 0 ~ NA, IP > 0 ~ SO)) %>%  
  mutate(HR = case_when(PA == 0 ~ NA, PA > 0 ~ HR), R = case_when(PA == 0 ~ NA, PA > 0 ~ R), AVG = case_when(PA == 0 ~ NA, PA > 0 ~ AVG)) %>% 
  # Calculate WH (walks + hits) for WHIP and ER for ERA in standings
  mutate(WH = ifelse(IP > 0, WHIP * IP, NA_real_)) %>%
  mutate(ER = ifelse(IP > 0, ERA * IP / 9, NA_real_)) %>%
  select(Name, billikenTeam, contract, salary, Team, PA, AB, H, HR, R, RBI, SB, AVG, IP, W, SV, SO, ER, WH, ERA, WHIP, point_value, p_c, p_1b, p_2b, p_3b, p_ss, p_of, p_ci, p_mi) %>%
  arrange(desc(point_value))

# --- Calculate Replacement Levels by Position ---
message("Calculating replacement levels by position...")

# Calculate replacement level point values for each position
rl_c <- projected_players %>% 
  filter(p_c == 1) %>% 
  arrange(desc(point_value)) %>%
  slice(21) %>% 
  pull(point_value)

rl_1b <- projected_players %>% 
  filter(p_1b == 1) %>% 
  arrange(desc(point_value)) %>%
  slice(16) %>% 
  pull(point_value)

rl_2b <- projected_players %>% 
  filter(p_2b == 1) %>% 
  arrange(desc(point_value)) %>%
  slice(16) %>% 
  pull(point_value)

rl_3b <- projected_players %>% 
  filter(p_3b == 1) %>% 
  arrange(desc(point_value)) %>%
  slice(16) %>% 
  pull(point_value)

rl_ss <- projected_players %>% 
  filter(p_ss == 1) %>% 
  arrange(desc(point_value)) %>%
  slice(16) %>% 
  pull(point_value)

rl_of <- projected_players %>% 
  filter(p_of == 1) %>% 
  arrange(desc(point_value)) %>%
  slice(51) %>% 
  pull(point_value)

rl_ci <- projected_players %>% 
  filter(p_ci == 1) %>% 
  arrange(desc(point_value)) %>%
  slice(31) %>% 
  pull(point_value)

rl_mi <- projected_players %>% 
  filter(p_mi == 1) %>% 
  arrange(desc(point_value)) %>%
  slice(31) %>% 
  pull(point_value)

rl_util <- projected_players %>% 
  arrange(desc(point_value)) %>%
  slice(141) %>% 
  pull(point_value)

rl_p <- projected_players %>% 
  filter(IP > 0) %>% 
  arrange(desc(point_value)) %>%
  slice(91) %>% 
  pull(point_value)

message(sprintf("Replacement levels: C=%.1f, 1B=%.1f, 2B=%.1f, 3B=%.1f, SS=%.1f, OF=%.1f, CI=%.1f, MI=%.1f, Util=%.1f, P=%.1f",
                rl_c, rl_1b, rl_2b, rl_3b, rl_ss, rl_of, rl_ci, rl_mi, rl_util, rl_p))

# --- Calculate Points Above Replacement (PAR) ---
message("Calculating points above replacement...")

# For each player, find their best (lowest) eligible replacement level
par <- projected_players %>% 
  mutate(
    repl = case_when(
      # Pitchers
      IP > 0 ~ rl_p,
      # Position players - use the lowest (best) replacement level they qualify for
      TRUE ~ pmin(
        ifelse(p_c == 1, rl_c, 999),
        ifelse(p_1b == 1, rl_1b, 999),
        ifelse(p_2b == 1, rl_2b, 999),
        ifelse(p_3b == 1, rl_3b, 999),
        ifelse(p_ss == 1, rl_ss, 999),
        ifelse(p_of == 1, rl_of, 999),
        ifelse(p_ci == 1, rl_ci, 999),
        ifelse(p_mi == 1, rl_mi, 999),
        rl_util,  # Everyone qualifies for utility
        na.rm = TRUE
      )
    )
  ) %>% 
  mutate(par = point_value - repl) %>% 
  arrange(desc(par)) %>% 
  select(Name, Team, billikenTeam, contract, salary, point_value, repl, par, PA, AB, H, HR, R, RBI, SB, AVG, IP, W, SV, SO, ER, WH, ERA, WHIP, p_c, p_1b, p_2b, p_3b, p_ss, p_of, p_ci, p_mi)

# --- Calculate Expected Value (EV) ---
message("Calculating expected value...")

# Build linear model: salary ~ par (for players with current salaries)
# This estimates market value based on points above replacement
ev_data <- par %>% filter(!is.na(salary) & !is.na(billikenTeam))
ev_model <- lm(salary ~ par, data = ev_data)

# Extract coefficients (y = mx + b form)
ev_slope <- ev_model$coefficients["par"]        # m (multiplier for PAR)
ev_intercept <- ev_model$coefficients["(Intercept)"]  # b (base salary)

message(sprintf("EV Model: salary = %.3f * par + %.3f (R² = %.3f)",
                ev_slope, ev_intercept, summary(ev_model)$r.squared))

# Calculate expected value for all players using model coefficients
par$ev <- par$par * ev_slope + ev_intercept
par$surplus <- round(par$ev - par$salary, 1)
par$ev <- round(par$ev, 1)

# Export full projections
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(par, "data/processed/projections_2026.csv")
message("Exported full projections to data/processed/projections_2026.csv")

# --- Project Keepers for Each Team ---
message("Projecting keepers for each team...")

# Define keeper limits for each team
keeper_limits <- list(
  "Blue Socks" = 15,
  "Melonheads" = 15,
  "Erie Lakers" = 10,
  "National Pastime" = 15,
  "Big Red Machine" = 15,
  "Free At Last" = 15,
  "Free Birds" = 15,
  "Westside Marauders" = 15,
  "Louisville Sluggers" = 11,
  "Hoosiers" = 12
)

# Project keepers for all teams
projected_keepers_list <- list()

for (team_name in names(keeper_limits)) {
  limit <- keeper_limits[[team_name]]
  
  team_keepers <- par %>% 
    filter(billikenTeam == team_name) %>% 
    arrange(desc(ev)) %>% 
    slice_head(n = limit)
  
  projected_keepers_list[[team_name]] <- team_keepers
  
  # Print keeper summary
  message(sprintf("\n%s (%d keepers):", team_name, nrow(team_keepers)))
  for (i in 1:nrow(team_keepers)) {
    player <- team_keepers[i,]
    message(sprintf("  %d. %s - EV: $%s, Salary: $%s, PAR: %.1f", 
                    i, player$Name, player$ev, player$salary, player$par))
  }
}

# Combine all projected keepers
projected_keepers_full <- bind_rows(projected_keepers_list)

# Export full keeper details
write_csv(projected_keepers_full, "data/processed/projected_keepers.csv")
message(sprintf("Exported %d projected keepers to data/processed/projected_keepers.csv", nrow(projected_keepers_full)))

# Create simplified version for joins
projected_keepers <- projected_keepers_full %>% 
  select(Name, billikenTeam) %>% 
  rename(keepingTeam = billikenTeam)

message(sprintf("Total projected keepers: %d", nrow(projected_keepers)))

# --- Export Draft Eligible Players ---
message("Exporting draft eligible players...")

projected_draft_eligible <- par %>% 
  left_join(projected_keepers, by = join_by(Name)) %>% 
  filter(is.na(keepingTeam)) %>% 
  relocate(ev, .after = par) %>% 
  relocate(surplus, .after = ev) %>% 
  arrange(desc(ev)) %>% 
  mutate(pick = row_number())

# Write to CSV
write_csv(projected_draft_eligible, "data/processed/projected_draft_eligible.csv")

message(sprintf("Exported %d draft eligible players to data/processed/projected_draft_eligible.csv", nrow(projected_draft_eligible)))

# Print summary
message("\n=== Draft Eligible Summary ===")
message(sprintf("Total players available: %d", nrow(projected_draft_eligible)))
message(sprintf("Top 10 by EV:"))
for (i in 1:min(10, nrow(projected_draft_eligible))) {
  player <- projected_draft_eligible[i,]
  message(sprintf("  %d. %s (%s) - EV: $%s, PAR: %.1f", 
                  i, player$Name, player$Team, player$ev, player$par))
}

message("\nDone!")
