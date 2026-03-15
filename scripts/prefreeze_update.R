# scripts/prefreeze_update.R

# prefreeze_update.R
# Run the data and player value updates for pre-freeze rosters. 
#

setwd("/app")

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringi)
})

# -----------------
# Args
# -----------------
# Optional CLI arg: update_replacement_level (default 0)
# Example:
#   Rscript scripts/prefreeze_update.R 0
# or:
#   Rscript scripts/prefreeze_update.R update_replacement_level=0
args <- commandArgs(trailingOnly = TRUE)
update_replacement_level <- 0
if (length(args) >= 1) {
  a1 <- args[[1]]
  if (grepl("=", a1, fixed = TRUE)) {
    parts <- strsplit(a1, "=", fixed = TRUE)[[1]]
    if (length(parts) == 2 && parts[[1]] %in% c("update_replacement_level", "--update_replacement_level")) {
      update_replacement_level <- as.numeric(parts[[2]])
    }
  } else {
    update_replacement_level <- as.numeric(a1)
  }
}
if (is.na(update_replacement_level) || update_replacement_level < 0) {
  stop("update_replacement_level must be a non-negative number (e.g. 0, 0.10).")
}

message(sprintf("Running simulate_keepers.R with update_replacement_level = %.3f", update_replacement_level))


message("Starting Pre-Freeze update. Pulling Billiken, FanGraphs, & ESPN data...")
source("scripts/draft_day_update.R")

message("Pulled Billiken, FanGraphs, & ESPN data.")

if (update_replacement_level == 1) {
  message("Fetching ESPN historical standings...")
  source("scripts/fetch_espn_standings.R")
  
  message("Pulled ESPN historical standings. Calculating Team Standings Gained Points...")
  source("scripts/standings_gained_points.R")

  message("Calculated Team Standings Gained Points. Calculating initial Player Standings Gained Points...")
  source("scripts/calculate_player_sgp.R")

  message("Optimizing rosters to calculate replacement level... (this may take a while)")
  source("scripts/optimize_rosters_sgp.R")
  
  message("Optimized rosters to calculate replacement level.")
}

message("Calculating Player Standings Gained Points...")
source("scripts/calculate_player_sgp.R")

message("Calculated Player Standings Gained Points. Calculating player value...")
source("scripts/calculate_player_value.R")

message("Calculated player value. Simulating keepers...")
source("scripts/simulate_keepers.R")
simulate_keepers(
  sgpar_random = 0.00,
  keepers_path = "data/raw/keepers.csv",
  salaries_path = "data/raw/salaries_latest.csv"
)

message("Simulated keepers. Update current rosters.")
source("scripts/update_current_rosters.R")

message("Current rosters updated.")

message("prefreeze_update.R is complete!")
