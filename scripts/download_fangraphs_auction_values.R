#!/usr/bin/env Rscript

# scripts/download_fangraphs_auction_values.R
# Fetch auction calculator $ values from FanGraphs and write to data/raw.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(readr)
})

auction_projection_type <- function() {
  Sys.getenv("FANGRAPHS_AUCTION_PROJ", unset = "fangraphsdc")
}

auction_output_path <- function(projections_year,
                                projection_type = auction_projection_type(),
                                explicit_outfile = Sys.getenv(
                                  "FANGRAPHS_AUCTION_OUTFILE",
                                  unset = ""
                                )) {
  if (nzchar(explicit_outfile)) return(explicit_outfile)

  prefix <- if (identical(projection_type, "rfangraphsdc")) {
    "auction_values_ros_"
  } else {
    "auction_values_"
  }
  file.path("data/raw", paste0(prefix, projections_year, ".csv"))
}

build_auction_query <- function(projection_type = auction_projection_type()) {
  paste0(
    "teams=10&lg=NL&dollars=270&mb=1&mp=20&msp=5&mrp=5&players=&proj=",
    projection_type,
    "&split=&",
    "points=c%7C0%2C1%2C2%2C3%2C4%7C0%2C1%2C2%2C3%2C4&rep=0&drp=0&",
    "pp=C%2CSS%2C2B%2C3B%2COF%2C1B&",
    "pos=2%2C1%2C1%2C1%2C5%2C1%2C1%2C1%2C0%2C1%2C0%2C0%2C9%2C0%2C0&",
    "sort=&view=0"
  )
}

download_fangraphs_auction_values <- function(
  projections_year = Sys.getenv("BILLIKEN_PROJECTIONS_YEAR", unset = "2026"),
  projection_type = auction_projection_type(),
  out_path = auction_output_path(projections_year, projection_type)
) {
  fg_cookie <- Sys.getenv("FANGRAPHS_COOKIE")
  if (fg_cookie == "") {
    stop("FANGRAPHS_COOKIE environment variable not set", call. = FALSE)
  }

  message("Cookie length: ", nchar(fg_cookie))

  if (!exists("fg_login")) {
    if (file.exists("scripts/fangraphs_login.R")) {
      source("scripts/fangraphs_login.R")
    }
  }

  ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
  ref <- "https://www.fangraphs.com/fantasy-tools/auction-calculator"
  base_qs <- build_auction_query(projection_type)

  fetch_type <- function(type) {
    stopifnot(type %in% c("bat", "pit"))

    url <- paste0(
      "https://www.fangraphs.com/api/fantasy/auction-calculator/data?type=",
      type,
      "&",
      base_qs
    )

    resp <- request(url) |>
      req_headers(
        Cookie = fg_cookie,
        Referer = ref,
        Accept = "application/json, text/plain, */*"
      ) |>
      req_user_agent(ua) |>
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
    raw <- obj$data

    mlbam_col <- intersect(c("xMLBAMID", "xMLBAMId", "MLBAMId", "MLBAMID"),
                           names(raw))
    if (length(mlbam_col) > 0) {
      raw$xMLBAMID <- as.integer(raw[[mlbam_col[1]]])
    } else {
      warning(sprintf("No MLBAM ID column found in auction calculator response (type=%s)", type))
      raw$xMLBAMID <- NA_integer_
    }

    raw %>%
      transmute(
        playerid = as.integer(playerid),
        xMLBAMID = xMLBAMID,
        Team = as.character(Team),
        PlayerName = as.character(PlayerName),
        auction_type = type,
        fg_auction_dollars = as.numeric(Dollars)
      )
  }

  message("Fetching FanGraphs auction calculator values (batters)...")
  auc_bat <- fetch_type("bat")
  message(sprintf("  rows: %d", nrow(auc_bat)))

  message("Fetching FanGraphs auction calculator values (pitchers)...")
  auc_pit <- fetch_type("pit")
  message(sprintf("  rows: %d", nrow(auc_pit)))

  auction_values <- bind_rows(auc_bat, auc_pit) %>%
    arrange(desc(fg_auction_dollars))

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(auction_values, out_path)
  message(sprintf("Wrote %s", out_path))

  invisible(auction_values)
}

if (sys.nframe() == 0L && !interactive()) {
  download_fangraphs_auction_values()
}
