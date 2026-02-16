#!/usr/bin/env Rscript

library(here)
source(here::here("scripts", "fangraphs_login.R"))

# Always refresh login (safe and fast)
fg_login()

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(tidyverse)
})

# Get projections year from environment or default to 2026
projections_year <- Sys.getenv("BILLIKEN_PROJECTIONS_YEAR", unset = "2026")

# Fangraphs Depth Charts endpoints
fg_urls <- list(
  hitters  = "https://www.fangraphs.com/api/projections?type=fangraphsdc&stats=bat&pos=all&team=0&players=0&lg=all&z=1769217940966",
  pitchers = "https://www.fangraphs.com/api/projections?type=fangraphsdc&stats=pit&pos=all&team=0&players=0&lg=all&z=1769216772671"
)

output_files <- list(
  hitters  = file.path("data/raw", paste0("hitter_projections_", projections_year, ".csv")),
  pitchers = file.path("data/raw", paste0("pitcher_projections_", projections_year, ".csv"))
)

fetch_projection <- function(url, path, type) {
  
  message("Requesting ", url)
  
  req <- request(url) |>
    req_cookie_preserve(path = "~/.fangraphs_cookiejar") |>
    req_user_agent(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
    ) |>
    req_headers(
      Referer = "https://www.fangraphs.com/projections",
      `X-Requested-With` = "XMLHttpRequest",
      Accept = "application/json, text/plain, */*",
      Origin = "https://www.fangraphs.com"
    ) |>
    req_error(is_error = function(resp) FALSE)
  
  
  resp <- req_perform(req)
  
  status <- resp_status(resp)
  
  if (status != 200) {
    stop(sprintf(
      "Fangraphs request failed (%s). Login likely expired or blocked.",
      status
    ), call. = FALSE)
  }
  
  # ---- READ JSON DIRECTLY FROM HTTR2 ----
  content_text <- resp_body_string(resp)
  data <- fromJSON(content_text, flatten = TRUE)
  
  df <- as_tibble(data)
  
  # Standardize player name column
  name_cols <- c("PlayerName", "Name", "playerName", "name")
  for (nc in name_cols) {
    if (nc %in% names(df)) {
      df <- df %>% rename(Name = !!sym(nc))
      break
    }
  }
  
  # Standardize team column
  team_cols <- c("TeamAbbr", "team", "Team")
  for (tc in team_cols) {
    if (tc %in% names(df) && tc != "Team") {
      df <- df %>% rename(Team = !!sym(tc))
      break
    }
  }
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, path)
  
  info <- file.info(path)
  message(sprintf("Wrote %s (%d rows, %s bytes)", path, nrow(df), format(info$size, big.mark=",")))
}

fetch_projection(fg_urls$hitters,  output_files$hitters, "hitters")
fetch_projection(fg_urls$pitchers, output_files$pitchers, "pitchers")

message("Done.")
