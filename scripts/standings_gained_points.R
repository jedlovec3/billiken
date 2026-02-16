# scripts/standings_gained_points.R
# Calculate the value of each roto category on a common scale based on historical standings.
#
# For each category:
# - Last place receives 0.0 points, first place receives 1.0 points
# - Middle places receive p/(n-1) where p = places above last, n = total teams
#
# Fits a logit curve to each category based on stat fraction relative to first place.
# Returns stat values corresponding to replacement level and championship level.
# Calculates marginal unit values for each stat category.

suppressPackageStartupMessages({
  library(tidyverse)
})

# --- Parameters ---
# Team replacement level: near last place in category
TEAM_REPLACEMENT_LEVEL <- 0.10

# Team championship level: near first place in category  
TEAM_CHAMPIONSHIP_LEVEL <- 0.95

# --- Load Historical Standings ---
message("Loading historical standings...")

standings <- read_csv("data/raw/standings_history_latest.csv", show_col_types = FALSE)

# Define categories and whether higher is better
categories <- list(
  # Hitting (higher is better)
  R = list(higher_better = TRUE),
  HR = list(higher_better = TRUE),
  RBI = list(higher_better = TRUE),
  SB = list(higher_better = TRUE),
  AVG = list(higher_better = TRUE),
  # Pitching (higher is better for counting stats)
  W = list(higher_better = TRUE),
  SV = list(higher_better = TRUE),
  SO = list(higher_better = TRUE),
  # Pitching ratios (lower is better)
  ERA = list(higher_better = FALSE),
  WHIP = list(higher_better = FALSE)
)

# --- Calculate Category Points ---
message("Calculating category standings points...")

# For each season, rank teams in each category and assign points
standings_with_points <- standings %>%
  group_by(season) %>%
  mutate(n_teams = n()) %>%
  ungroup()

# Calculate points for each category
for (cat_name in names(categories)) {
  higher_better <- categories[[cat_name]]$higher_better
  points_col <- paste0(cat_name, "_pts")
  rank_col <- paste0(cat_name, "_rank")
  
  standings_with_points <- standings_with_points %>%
    group_by(season) %>%
    mutate(
      # Rank: 1 = best, n = worst
      !!rank_col := if (higher_better) {
        rank(-get(cat_name), ties.method = "average")
      } else {
        rank(get(cat_name), ties.method = "average")
      },
      # Points: first place = 1.0, last place = 0.0
      # p = places above last = n_teams - rank
      !!points_col := (n_teams - get(rank_col)) / (n_teams - 1)
    ) %>%
    ungroup()
}

# --- Calculate Stat Fractions ---
message("Calculating stat fractions relative to first place...")

# For each category, calculate fraction relative to best value in that season
for (cat_name in names(categories)) {
  higher_better <- categories[[cat_name]]$higher_better
  frac_col <- paste0(cat_name, "_frac")
  
  standings_with_points <- standings_with_points %>%
    group_by(season) %>%
    mutate(
      !!frac_col := if (higher_better) {
        # Higher is better: fraction = value / max(value)
        get(cat_name) / max(get(cat_name), na.rm = TRUE)
      } else {
        # Lower is better: fraction = min(value) / value
        min(get(cat_name), na.rm = TRUE) / get(cat_name)
      }
    ) %>%
    ungroup()
}

# --- Fit Logit Models ---
message("Fitting logit models for each category...")

# Function to fit logit and extract key values
fit_category_model <- function(data, cat_name, replacement_level, championship_level) {
  points_col <- paste0(cat_name, "_pts")
  frac_col <- paste0(cat_name, "_frac")
  higher_better <- categories[[cat_name]]$higher_better
  
  # Prepare data for logit (need to handle 0 and 1 endpoints)
  model_data <- data %>%
    select(season, team_name, 
           stat = all_of(cat_name), 
           points = all_of(points_col), 
           frac = all_of(frac_col)) %>%
    filter(!is.na(stat), !is.na(frac), frac > 0, frac <= 1) %>%
    # Adjust points slightly away from 0 and 1 for logit
    mutate(points_adj = pmin(pmax(points, 0.001), 0.999))
  
  # Fit logit model: logit(points) ~ frac
  # Using quasibinomial to handle non-integer weights
  model <- glm(points_adj ~ frac, 
               data = model_data, 
               family = quasibinomial(link = "logit"))
  
  # Extract coefficients
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  
  # Function to convert fraction to points
  frac_to_points <- function(f) {
    plogis(intercept + slope * f)
  }
  
  # Function to convert points to fraction
  points_to_frac <- function(p) {
    # logit(p) = intercept + slope * frac
    # frac = (logit(p) - intercept) / slope
    (qlogis(p) - intercept) / slope
  }
  
  # Find fractions for replacement and championship levels
  frac_replacement <- points_to_frac(replacement_level)
  frac_championship <- points_to_frac(championship_level)
  
  # Get reference values (average first place value across seasons)
  ref_values <- data %>%
    group_by(season) %>%
    summarize(
      best_val = if (higher_better) max(get(cat_name), na.rm = TRUE) 
                 else min(get(cat_name), na.rm = TRUE),
      .groups = "drop"
    )
  avg_best <- mean(ref_values$best_val)
  
  # Convert fractions back to actual stat values
  if (higher_better) {
    # frac = value / best, so value = frac * best
    stat_replacement <- frac_replacement * avg_best
    stat_championship <- frac_championship * avg_best
  } else {
    # frac = best / value, so value = best / frac
    stat_replacement <- avg_best / frac_replacement
    stat_championship <- avg_best / frac_championship
  }
  
  list(
    category = cat_name,
    higher_better = higher_better,
    intercept = intercept,
    slope = slope,
    avg_first_place = avg_best,
    frac_replacement = frac_replacement,
    frac_championship = frac_championship,
    stat_replacement = stat_replacement,
    stat_championship = stat_championship,
    model = model,
    data = model_data
  )
}

# Fit models for all categories
results <- map(names(categories), ~fit_category_model(
  standings_with_points, .x, 
  TEAM_REPLACEMENT_LEVEL, 
  TEAM_CHAMPIONSHIP_LEVEL
))
names(results) <- names(categories)

# --- Output Results ---
message(sprintf("\n=== Category Value Scaling Results ==="))
message(sprintf("Team Replacement Level: %.0f%%", TEAM_REPLACEMENT_LEVEL * 100))
message(sprintf("Team Championship Level: %.0f%%\n", TEAM_CHAMPIONSHIP_LEVEL * 100))

# Create summary table
summary_table <- map_dfr(results, function(r) {
  tibble(
    Category = r$category,
    `Higher Better` = r$higher_better,
    `Avg 1st Place` = r$avg_first_place,
    `Replacement Value` = r$stat_replacement,
    `Championship Value` = r$stat_championship,
    `Replacement Frac` = r$frac_replacement,
    `Championship Frac` = r$frac_championship
  )
})

# Print results
for (r in results) {
  cat(sprintf("\n%s (%s):\n", 
              r$category, 
              if(r$higher_better) "higher is better" else "lower is better"))
  cat(sprintf("  Avg first place: %.2f\n", r$avg_first_place))
  cat(sprintf("  Team replacement (%.0f%%):  %.2f (fraction: %.3f)\n", 
              TEAM_REPLACEMENT_LEVEL * 100, r$stat_replacement, r$frac_replacement))
  cat(sprintf("  Team championship (%.0f%%): %.2f (fraction: %.3f)\n", 
              TEAM_CHAMPIONSHIP_LEVEL * 100, r$stat_championship, r$frac_championship))
  cat(sprintf("  Logit: intercept=%.3f, slope=%.3f\n", 
              r$intercept, r$slope))
}

# --- Calculate Marginal Unit Values ---
message("\n=== Marginal Unit Values ===")

# Calculate average AB and IP from historical standings
avg_AB <- mean(standings$AB, na.rm = TRUE)
avg_IP <- mean(standings$IP, na.rm = TRUE)
message(sprintf("Average team AB: %.1f", avg_AB))
message(sprintf("Average team IP: %.1f\n", avg_IP))

# Calculate unit values for each category
unit_values <- map_dfr(names(categories), function(cat_name) {
  r <- results[[cat_name]]
  championship <- r$stat_championship
  replacement <- r$stat_replacement
  higher_better <- r$higher_better
  
  if (cat_name %in% c("R", "HR", "RBI", "SB", "W", "SV", "SO")) {
    # Counting stats: unit_value = 1 / (championship - replacement)
    marginal_diff <- championship - replacement
    unit_value <- 1 / marginal_diff
    unit_stat <- cat_name
    
    tibble(
      Category = cat_name,
      Type = "counting",
      `Unit Stat` = unit_stat,
      Replacement = round(replacement, 2),
      Championship = round(championship, 2),
      `Marginal Diff` = round(marginal_diff, 2),
      `Unit Value` = unit_value
    )
  } else if (cat_name == "AVG") {
    # AVG: marginal_H = avg_AB * (championship_AVG - replacement_AVG)
    marginal_H <- avg_AB * (championship - replacement)
    unit_value <- 1 / marginal_H
    
    tibble(
      Category = cat_name,
      Type = "rate",
      `Unit Stat` = "H",
      Replacement = round(replacement, 4),
      Championship = round(championship, 4),
      `Marginal Diff` = round(marginal_H, 2),
      `Unit Value` = unit_value
    )
  } else if (cat_name == "ERA") {
    # ERA: marginal_ER = avg_IP * (replacement_ERA - championship_ERA) / 9
    # Lower is better, so replacement - championship gives ER prevented
    marginal_ER <- avg_IP * (replacement - championship) / 9
    unit_value <- 1 / marginal_ER
    
    tibble(
      Category = cat_name,
      Type = "rate",
      `Unit Stat` = "ER prevented",
      Replacement = round(replacement, 2),
      Championship = round(championship, 2),
      `Marginal Diff` = round(marginal_ER, 2),
      `Unit Value` = unit_value
    )
  } else if (cat_name == "WHIP") {
    # WHIP: marginal_WH = avg_IP * (replacement_WHIP - championship_WHIP)
    # Lower is better, so replacement - championship gives WH prevented
    marginal_WH <- avg_IP * (replacement - championship)
    unit_value <- 1 / marginal_WH
    
    tibble(
      Category = cat_name,
      Type = "rate",
      `Unit Stat` = "WH prevented",
      Replacement = round(replacement, 2),
      Championship = round(championship, 2),
      `Marginal Diff` = round(marginal_WH, 2),
      `Unit Value` = unit_value
    )
  }
})

# Print unit values
cat("\nMarginal Unit Values (points per unit stat):\n")
cat("\nCounting Stats:\n")
for (i in 1:nrow(unit_values)) {
  row <- unit_values[i, ]
  if (row$Type == "counting") {
    cat(sprintf("  %s: %.5f pts per %s (%.1f to %.1f, diff=%.1f)\n",
                row$Category, row$`Unit Value`, row$`Unit Stat`,
                row$Replacement, row$Championship, row$`Marginal Diff`))
  }
}

cat("\nRate Stats:\n")
for (i in 1:nrow(unit_values)) {
  row <- unit_values[i, ]
  if (row$Type == "rate") {
    cat(sprintf("  %s: %.5f pts per %s (%.4f to %.4f, marginal diff=%.1f)\n",
                row$Category, row$`Unit Value`, row$`Unit Stat`,
                row$Replacement, row$Championship, row$`Marginal Diff`))
  }
}

# Export unit values
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(unit_values, "data/processed/category_unit_values.csv")
message("\nExported unit values to data/processed/category_unit_values.csv")

# Export summary
write_csv(summary_table, "data/processed/category_value_scaling.csv")
message("Exported summary to data/processed/category_value_scaling.csv")

# Export full standings with points for analysis
standings_export <- standings_with_points %>%
  select(season, team_name, abbrev, final_standing, points_for,
         R, R_pts, R_frac,
         HR, HR_pts, HR_frac,
         RBI, RBI_pts, RBI_frac,
         SB, SB_pts, SB_frac,
         AVG, AVG_pts, AVG_frac,
         W, W_pts, W_frac,
         SV, SV_pts, SV_frac,
         SO, SO_pts, SO_frac,
         ERA, ERA_pts, ERA_frac,
         WHIP, WHIP_pts, WHIP_frac)

write_csv(standings_export, "data/processed/standings_with_category_points.csv")
message("Exported detailed standings to data/processed/standings_with_category_points.csv")

# Return results for interactive use
invisible(list(
  summary = summary_table,
  unit_values = unit_values,
  models = results,
  standings = standings_with_points,
  avg_AB = avg_AB,
  avg_IP = avg_IP
))
