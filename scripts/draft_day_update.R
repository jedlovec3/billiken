# scripts/draft_day_update.R
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(googlesheets4)
})

req_env <- function(name) {
  val <- Sys.getenv(name, unset = "")
  if (identical(val, "")) stop(sprintf("Missing required env var: %s", name), call. = FALSE)
  val
}

sheet_id <- req_env("BILLIKEN_SHEET_ID")

tab_rosters   <- Sys.getenv("BILLIKEN_TAB_ROSTERS",   unset = "PreFreezeRosters")
tab_draft     <- Sys.getenv("BILLIKEN_TAB_DRAFT",     unset = "Draft")
tab_salaries  <- Sys.getenv("BILLIKEN_TAB_SALARIES",  unset = "Salaries")
tab_positions <- Sys.getenv("BILLIKEN_TAB_POSITIONS", unset = "Positions")

message("Reading Billiken Google Sheet (de-authed/public)…")
gs4_deauth()

rosters   <- read_sheet(sheet_id, sheet = tab_rosters)
draft     <- read_sheet(sheet_id, sheet = tab_draft)
salaries  <- read_sheet(sheet_id, sheet = tab_salaries)
positions <- read_sheet(sheet_id, sheet = tab_positions)

# Write raw extracts (timestamped)
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
write_csv(rosters,   file.path("data/raw", paste0("rosters_",   ts, ".csv")))
write_csv(draft,     file.path("data/raw", paste0("draft_",     ts, ".csv")))
write_csv(salaries,  file.path("data/raw", paste0("salaries_",  ts, ".csv")))
write_csv(positions, file.path("data/raw", paste0("positions_", ts, ".csv")))

# Also write “latest” stable filenames your app/notebooks can read
write_csv(rosters,   file.path("data/raw", "rosters_latest.csv"))
write_csv(draft,     file.path("data/raw", "draft_latest.csv"))
write_csv(salaries,  file.path("data/raw", "salaries_latest.csv"))
write_csv(positions, file.path("data/raw", "positions_latest.csv"))

message("Wrote raw + latest CSVs under data/raw/. Fetching ESPN positions...")

source("scripts/fetch_espn_positions.R")
fetch_espn_positions()

message("Done. Fetched ESPN positions from API")
