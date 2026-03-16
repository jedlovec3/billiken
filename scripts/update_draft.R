# scripts/update_draft.R
# Pulls only the draft tab from the Billiken Google Sheet and writes it to
# data/raw/draft_latest.csv.  Designed to be fast — no FanGraphs or ESPN calls.

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

sheet_id  <- req_env("BILLIKEN_SHEET_ID")
tab_draft <- Sys.getenv("BILLIKEN_TAB_DRAFT", unset = "Draft")

message("Reading draft tab from Billiken Google Sheet (de-authed/public)…")
gs4_deauth()

draft <- read_sheet(sheet_id, sheet = tab_draft) %>%
  select(-(1:2)) %>%
  rename(Player = "Player...7")

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
write_csv(draft, file.path("data/raw", "draft_latest.csv"))

message("Wrote data/raw/draft_latest.csv")
message("Done.")
