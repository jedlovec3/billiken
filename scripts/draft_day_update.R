# scripts/draft_day_update.R
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(googlesheets4)
  library(httr2)
})


req_env <- function(name) {
  val <- Sys.getenv(name, unset = "")
  if (identical(val, "")) stop(sprintf("Missing required env var: %s", name), call. = FALSE)
  val
}

sheet_id <- req_env("BILLIKEN_SHEET_ID")

tab_prefreeze_rosters <- Sys.getenv("BILLIKEN_TAB_PREFREEZE_ROSTERS", unset = "PreFreezeRosters")
tab_frozen_rosters    <- Sys.getenv("BILLIKEN_TAB_FROZEN_ROSTERS",    unset = "FrozenRosters")
tab_draft             <- Sys.getenv("BILLIKEN_TAB_DRAFT",             unset = "Draft")
tab_salaries          <- Sys.getenv("BILLIKEN_TAB_SALARIES",          unset = "Salaries")

message("Reading Billiken Google Sheet (de-authed/public)…")
gs4_deauth()

prefreeze_rosters <- read_sheet(sheet_id, sheet = tab_prefreeze_rosters, col_types = "ccccccd") %>%
  # Remove outdated/incorrect columns if present
  select(-any_of(c("team", "2021eligibility")))
frozen_rosters    <- read_sheet(sheet_id, sheet = tab_frozen_rosters, col_types = "ccccdc") %>% 
  rename(billikenTeam = "Owner")
draft             <- read_sheet(sheet_id, sheet = tab_draft) %>%
  # Remove redundant columns if present
  select(-(1:2)) %>% 
  rename(Player = "Player...7")
salaries          <- read_sheet(sheet_id, sheet = tab_salaries, col_types = "cdc")

# Write raw extracts (timestamped)
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
write_csv(prefreeze_rosters, file.path("data/raw", paste0("prefreeze_rosters_", ts, ".csv")))
write_csv(frozen_rosters,    file.path("data/raw", paste0("keepers_",           ts, ".csv")))
write_csv(draft,             file.path("data/raw", paste0("draft_",             ts, ".csv")))
write_csv(salaries,          file.path("data/raw", paste0("salaries_",          ts, ".csv")))

# Also write "latest" stable filenames your app/notebooks can read
write_csv(prefreeze_rosters, file.path("data/raw", "prefreeze_rosters_latest.csv"))
write_csv(frozen_rosters,    file.path("data/raw", "keepers.csv"))
write_csv(draft,             file.path("data/raw", "draft_latest.csv"))
write_csv(salaries,          file.path("data/raw", "salaries_latest.csv"))

message("Wrote raw + latest CSVs under data/raw/. Downloading FanGraphs projections + auction $ values...")

source("scripts/download_fangraphs_projections.R")

message("Fetching ESPN positions...")

source("scripts/fetch_espn_positions.R")
fetch_espn_positions()

message("Done. Fetched ESPN positions from API")
