#!/usr/bin/env Rscript

# Download FanGraphs Depth Charts projections for hitters and pitchers
# Requires env var FANGRAPHS_COOKIE to contain your full Cookie header
# from an authenticated Fangraphs session.

suppressPackageStartupMessages({
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The 'httr' package is required. Install it with install.packages('httr').", call. = FALSE)
  }
})

library(httr)

cookie <- Sys.getenv("FANGRAPHS_COOKIE")
if (identical(cookie, "")) {
  stop("FANGRAPHS_COOKIE is not set. Export it in your shell (see .zshrc) before running this script.", call. = FALSE)
}

# Locked-in Depth Charts export URLs (global settings; you can filter locally later)
fg_urls <- list(
  hitters  = "https://www.fangraphs.com/api/projections?type=fangraphsdc&stats=bat&pos=all&team=0&players=0&lg=all&z=1769217940966&download=1",
  pitchers = "https://www.fangraphs.com/api/projections?type=fangraphsdc&stats=pit&pos=all&team=0&players=0&lg=all&z=1769216772671&download=1"
)

output_files <- list(
  hitters  = "hitter_projections_2026.csv",
  pitchers = "pitcher_projections_2026.csv"
)

fetch_projection <- function(url, path) {
  message("Requesting ", url)
  resp <- httr::GET(url, httr::add_headers(Cookie = cookie))

  # Clearer error if auth/session is bad
  if (httr::status_code(resp) != 200) {
    stop(sprintf("Request for %s failed with status %s. Check your FANGRAPHS_COOKIE and login state.",
                 url, httr::status_code(resp)), call. = FALSE)
  }

  raw <- httr::content(resp, as = "raw")
  writeBin(raw, path)

  info <- file.info(path)
  message(sprintf("Wrote %s (%s bytes)", path, format(info$size, big.mark = ",")))
}

fetch_projection(fg_urls$hitters,  output_files$hitters)
fetch_projection(fg_urls$pitchers, output_files$pitchers)

message("Done.")
