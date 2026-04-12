#!/usr/bin/env Rscript
# scripts/download_ros_projections.R
# Download FanGraphs Rest-of-Season (ROS) Depth Charts projections.
#
# Outputs:
#   data/raw/ros_hitter_projections_{year}.csv
#   data/raw/ros_pitcher_projections_{year}.csv
#
# Requires env vars:
#   FANGRAPHS_COOKIE — FanGraphs session cookie
#   Optionally: FANGRAPHS_USER, FANGRAPHS_PASS (for login refresh)
#   BILLIKEN_PROJECTIONS_YEAR (defaults to current year)

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(tidyverse)
})

.ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"

#' Download FanGraphs ROS projections.
#'
#' @param projection_year Season year. Defaults to BILLIKEN_PROJECTIONS_YEAR or current year.
#' @return list with $hitters and $pitchers tibbles.
download_ros_projections <- function(projection_year = NULL) {
  if (is.null(projection_year)) {
    projection_year <- Sys.getenv("BILLIKEN_PROJECTIONS_YEAR",
                                   unset = format(Sys.Date(), "%Y"))
  }

  # Try FanGraphs login if credentials are available
  login_script <- "scripts/fangraphs_login.R"
  if (!file.exists(login_script)) {
    login_script <- file.path(getwd(), login_script)
  }
  if (file.exists(login_script)) {
    tryCatch({
      source(login_script, local = TRUE)
      if (Sys.getenv("FANGRAPHS_USER") != "" &&
          Sys.getenv("FANGRAPHS_PASS") != "") {
        fg_login()
        message("FanGraphs login refreshed.")
      }
    }, error = function(e) {
      message("FanGraphs login skipped (non-fatal): ", e$message)
    })
  }

  fg_cookie <- Sys.getenv("FANGRAPHS_COOKIE")
  if (fg_cookie == "") {
    stop("FANGRAPHS_COOKIE environment variable not set. ",
         "Extract your FanGraphs session cookie and set it.",
         call. = FALSE)
  }

  # ROS Depth Charts endpoints (note 'r' prefix = rest-of-season)
  urls <- list(
    hitters = paste0(
      "https://www.fangraphs.com/api/projections?",
      "type=rfangraphsdc&stats=bat&pos=all&team=0&players=0&lg=all"
    ),
    pitchers = paste0(
      "https://www.fangraphs.com/api/projections?",
      "type=rfangraphsdc&stats=pit&pos=all&team=0&players=0&lg=all"
    )
  )

  output_paths <- list(
    hitters  = file.path("data/raw",
                         paste0("ros_hitter_projections_", projection_year, ".csv")),
    pitchers = file.path("data/raw",
                         paste0("ros_pitcher_projections_", projection_year, ".csv"))
  )

  fetch_one <- function(url, path, label) {
    message(sprintf("Downloading FanGraphs ROS %s projections...", label))

    req <- request(url) |>
      req_headers(
        Cookie = fg_cookie,
        Referer = "https://www.fangraphs.com/projections",
        `X-Requested-With` = "XMLHttpRequest",
        Accept = "application/json, text/plain, */*",
        Origin = "https://www.fangraphs.com"
      ) |>
      req_user_agent(.ua) |>
      req_error(is_error = function(resp) FALSE)

    resp <- req_perform(req)
    status <- resp_status(resp)

    if (status != 200) {
      stop(sprintf(
        "FanGraphs %s request failed (HTTP %s). Cookie may have expired.",
        label, status
      ), call. = FALSE)
    }

    content_text <- resp_body_string(resp)
    data <- fromJSON(content_text, flatten = TRUE)
    df <- as_tibble(data)

    if (nrow(df) == 0) {
      warning(sprintf("FanGraphs returned 0 %s projections — ROS may not be available yet.", label))
      return(df)
    }

    # Standardize player name column -> "Name"
    name_cols <- c("PlayerName", "playerName", "name")
    for (nc in name_cols) {
      if (nc %in% names(df)) {
        df <- df %>% rename(Name = !!sym(nc))
        break
      }
    }

    # Standardize team column -> "Team"
    team_cols <- c("TeamAbbr", "team")
    for (tc in team_cols) {
      if (tc %in% names(df)) {
        df <- df %>% rename(Team = !!sym(tc))
        break
      }
    }

    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    write_csv(df, path)
    message(sprintf("Wrote %s (%d rows)", path, nrow(df)))

    invisible(df)
  }

  hitters  <- fetch_one(urls$hitters,  output_paths$hitters,  "hitter")
  pitchers <- fetch_one(urls$pitchers, output_paths$pitchers, "pitcher")

  message("ROS projection download complete.")
  invisible(list(hitters = hitters, pitchers = pitchers))
}

# Auto-run only when executed directly (not when source()'d)
if (sys.nframe() == 0L && !interactive()) {
  download_ros_projections()
}
