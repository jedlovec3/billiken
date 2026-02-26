#!/usr/bin/env Rscript

# scripts/download_fangraphs_auction_values.R
# Fetch auction calculator $ values from Fangraphs and write to data/raw.
#
# Output:
# - data/raw/auction_values_<year>.csv

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(readr)
})

# Optional: if sourced from another script, fangraphs_login.R may already be loaded.
if (!exists("fg_login")) {
  if (file.exists("scripts/fangraphs_login.R")) {
    source("scripts/fangraphs_login.R")
  }
}

if (exists("fg_login")) {
  # Ensure we have a valid cookie jar session for Cloudflare-gated endpoints.
  fg_login()
}

projections_year <- Sys.getenv("BILLIKEN_PROJECTIONS_YEAR", unset = "2026")

.ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
.ref <- "https://www.fangraphs.com/fantasy-tools/auction-calculator"

# NOTE: This query string is intentionally aligned to the user-provided auction calculator URL.
# If league settings change, update these params (or parameterize them via env vars later).
.base_qs <- paste0(
  "teams=10&lg=NL&dollars=270&mb=1&mp=20&msp=5&mrp=5&players=&proj=fangraphsdc&split=&",
  "points=c%7C0%2C1%2C2%2C3%2C4%7C0%2C1%2C2%2C3%2C4&rep=0&drp=0&",
  "pp=C%2CSS%2C2B%2C3B%2COF%2C1B&",
  "pos=2%2C1%2C1%2C1%2C5%2C1%2C1%2C1%2C0%2C1%2C0%2C0%2C9%2C0%2C0&",
  "sort=&view=0"
)

.fetch_type <- function(type) {
  stopifnot(type %in% c("bat", "pit"))

  url <- paste0(
    "https://www.fangraphs.com/api/fantasy/auction-calculator/data?type=",
    type,
    "&",
    .base_qs
  )

  resp <- request(url) |>
    req_cookie_preserve(path = "~/.fangraphs_cookiejar") |>
    req_user_agent(.ua) |>
    req_headers(
      Referer = .ref,
      Accept = "application/json, text/plain, */*"
    ) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) {
    stop(
      sprintf(
        "Auction calculator API request failed (type=%s, status=%s)",
        type,
        resp_status(resp)
      ),
      call. = FALSE
    )
  }

  obj <- fromJSON(resp_body_string(resp), flatten = TRUE)

  out <- obj$data %>%
    transmute(
      playerid = as.integer(playerid),
      Team = as.character(Team),
      PlayerName = as.character(PlayerName),
      auction_type = type,
      fg_auction_dollars = as.numeric(Dollars)
    )

  out
}

message("Fetching Fangraphs auction calculator values (batters)...")
auc_bat <- .fetch_type("bat")
message(sprintf("  rows: %d", nrow(auc_bat)))

message("Fetching Fangraphs auction calculator values (pitchers)...")
auc_pit <- .fetch_type("pit")
message(sprintf("  rows: %d", nrow(auc_pit)))

auction_values <- bind_rows(auc_bat, auc_pit) %>%
  arrange(desc(fg_auction_dollars))

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
out_path <- file.path("data/raw", paste0("auction_values_", projections_year, ".csv"))
write_csv(auction_values, out_path)
message(sprintf("Wrote %s", out_path))

invisible(auction_values)
