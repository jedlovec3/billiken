#!/usr/bin/env Rscript

# scripts/download_future_projections.R
# Download FanGraphs ZiPS future-season projections for prospect valuation.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(tidyverse)
})

future_projection_specs <- function(current_year = as.integer(
  Sys.getenv("BILLIKEN_PROJECTIONS_YEAR", unset = format(Sys.Date(), "%Y"))
)) {
  tibble(
    season = as.integer(c(current_year + 1L, current_year + 1L,
                          current_year + 2L, current_year + 2L)),
    projection_type = c("zipsp1", "zipsp1", "zipsp2", "zipsp2"),
    stats = c("bat", "pit", "bat", "pit"),
    label = c("hitter", "pitcher", "hitter", "pitcher"),
    path = file.path(
      "data/raw",
      c(
        paste0("future_hitter_projections_", current_year + 1L, ".csv"),
        paste0("future_pitcher_projections_", current_year + 1L, ".csv"),
        paste0("future_hitter_projections_", current_year + 2L, ".csv"),
        paste0("future_pitcher_projections_", current_year + 2L, ".csv")
      )
    )
  )
}

download_future_projections <- function(
  current_year = as.integer(Sys.getenv("BILLIKEN_PROJECTIONS_YEAR",
                                       unset = format(Sys.Date(), "%Y"))),
  specs = future_projection_specs(current_year)
) {
  fg_cookie <- Sys.getenv("FANGRAPHS_COOKIE")
  if (fg_cookie == "") {
    stop("FANGRAPHS_COOKIE environment variable not set", call. = FALSE)
  }

  ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"

  fetch_one <- function(spec) {
    url <- paste0(
      "https://www.fangraphs.com/api/projections?",
      "type=", spec$projection_type,
      "&stats=", spec$stats,
      "&pos=all&team=0&players=0&lg=all"
    )

    message(sprintf(
      "Downloading FanGraphs %s %s projections...",
      spec$season,
      spec$label
    ))

    resp <- request(url) |>
      req_headers(
        Cookie = fg_cookie,
        Referer = "https://www.fangraphs.com/projections",
        `X-Requested-With` = "XMLHttpRequest",
        Accept = "application/json, text/plain, */*",
        Origin = "https://www.fangraphs.com"
      ) |>
      req_user_agent(ua) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()

    if (resp_status(resp) != 200) {
      stop(sprintf(
        "FanGraphs future projection request failed (season=%s, label=%s, HTTP %s)",
        spec$season,
        spec$label,
        resp_status(resp)
      ), call. = FALSE)
    }

    df <- as_tibble(fromJSON(resp_body_string(resp), flatten = TRUE))
    if (nrow(df) == 0) {
      warning(sprintf("FanGraphs returned 0 rows for %s %s projections.",
                      spec$season, spec$label))
    }

    name_cols <- c("PlayerName", "Name", "playerName", "name")
    for (nc in name_cols) {
      if (nc %in% names(df) && nc != "Name") {
        df <- df %>% rename(Name = !!sym(nc))
        break
      }
    }

    team_cols <- c("TeamAbbr", "team", "Team")
    for (tc in team_cols) {
      if (tc %in% names(df) && tc != "Team") {
        df <- df %>% rename(Team = !!sym(tc))
        break
      }
    }

    if ("K" %in% names(df) && !"SO" %in% names(df)) {
      df <- df %>% rename(SO = K)
    }

    df <- df %>%
      mutate(
        future_projection_year = spec$season,
        future_projection_type = spec$projection_type,
        future_projection_player_type = spec$label
      )

    dir.create(dirname(spec$path), recursive = TRUE, showWarnings = FALSE)
    write_csv(df, spec$path)
    message(sprintf("Wrote %s (%d rows)", spec$path, nrow(df)))
    invisible(df)
  }

  rows <- lapply(seq_len(nrow(specs)), function(i) fetch_one(specs[i, ]))
  invisible(rows)
}

if (sys.nframe() == 0L && !interactive()) {
  download_future_projections()
}
