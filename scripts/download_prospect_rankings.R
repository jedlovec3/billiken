#!/usr/bin/env Rscript

# scripts/download_prospect_rankings.R
# Fetch prospect ranking sources and write cached raw CSV snapshots.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(tidyverse)
})

html_unescape_min <- function(x) {
  x %>%
    str_replace_all("&quot;", "\"") %>%
    str_replace_all("&#x27;", "'") %>%
    str_replace_all("&#39;", "'") %>%
    str_replace_all("&amp;", "&") %>%
    str_replace_all("&lt;", "<") %>%
    str_replace_all("&gt;", ">")
}

.extract_json_objects <- function(html) {
  props <- str_match_all(html, 'data-props="([^"]+)"')[[1]]
  if (nrow(props) == 0) return(list())

  map(props[, 2], function(raw) {
    txt <- html_unescape_min(raw)
    tryCatch(fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  }) %>%
    compact()
}

.flatten_json <- function(x) {
  out <- list()
  walk_json <- function(obj) {
    if (is.list(obj)) {
      out[[length(out) + 1L]] <<- obj
      for (child in obj) walk_json(child)
    }
  }
  walk_json(x)
  out
}

.get_ref <- function(x) {
  if (is.list(x) && !is.null(x[["__ref"]])) return(x[["__ref"]])
  NA_character_
}

first_present <- function(..., default = NA) {
  vals <- list(...)
  for (v in vals) {
    if (!is.null(v) && length(v) > 0 && !is.na(v[[1]])) return(v[[1]])
  }
  default
}

df_col <- function(df, candidates, default) {
  for (candidate in candidates) {
    if (candidate %in% names(df)) return(df[[candidate]])
  }
  rep(default, nrow(df))
}

parse_numeric_prefix <- function(x) {
  suppressWarnings(as.numeric(str_extract(as.character(x), "\\d+(?:[.]\\d+)?")))
}

empty_fangraphs_prospects <- function() {
  tibble(
    Name = character(), source = character(), source_rank = numeric(),
    mlb_org = character(), position = character(), level = character(),
    eta = integer(), age = numeric(), fg_rank = numeric(),
    fg_fv = numeric(), fg_risk = character()
  )
}

.find_entity_map <- function(objects) {
  candidates <- objects %>%
    map(.flatten_json) %>%
    purrr::flatten()

  named <- keep(candidates, function(x) length(names(x)) > 0)
  entity_maps <- keep(named, function(x) {
    any(str_detect(names(x), "^(Person|Prospect):"))
  })

  if (length(entity_maps) == 0) return(list())
  entity_maps[[1]]
}

extract_mlb_prospects_from_html <- function(html) {
  objects <- .extract_json_objects(html)
  entity_map <- .find_entity_map(objects)
  if (length(entity_map) == 0) {
    return(tibble(
      Name = character(), source = character(), source_rank = integer(),
      mlb_org = character(), position = character(), level = character(),
      eta = integer(), age = numeric()
    ))
  }

  prospect_keys <- names(entity_map)[str_detect(names(entity_map), "^Prospect:")]
  rows <- map_dfr(prospect_keys, function(key) {
    p <- entity_map[[key]]
    person_ref <- .get_ref(p$person)
    person <- if (!is.na(person_ref) && person_ref %in% names(entity_map)) {
      entity_map[[person_ref]]
    } else {
      list()
    }

    first <- first_present(person$useName, person$firstName, p$firstName, default = "")
    last <- first_present(person$useLastName, person$lastName, p$lastName, default = "")
    full_name <- str_squish(first_present(p$playerName, p$name, paste(first, last)))

    team <- p$team
    org <- if (is.list(team)) {
      first_present(team$abbreviation, team$abbrev, team$name, default = NA_character_)
    } else {
      NA_character_
    }

    pos <- if (is.list(person$primaryPosition)) {
      first_present(person$primaryPosition$abbreviation, default = NA_character_)
    } else if (is.list(p$primaryPosition)) {
      first_present(p$primaryPosition$abbreviation, default = NA_character_)
    } else {
      first_present(p$position, default = NA_character_)
    }

    tibble(
      Name = full_name,
      source = "mlb_pipeline",
      source_rank = suppressWarnings(as.integer(first_present(p$rank, p$ranking, default = NA_integer_))),
      mlb_org = org,
      position = pos,
      level = as.character(first_present(p$level, default = NA_character_)),
      eta = suppressWarnings(as.integer(first_present(p$eta, p$ETA, default = NA_integer_))),
      age = suppressWarnings(as.numeric(first_present(person$currentAge, p$age, default = NA_real_)))
    )
  })

  rows %>%
    filter(!is.na(Name), Name != "") %>%
    arrange(source_rank)
}

fetch_mlb_prospects <- function(url = "https://www.mlb.com/milb/prospects") {
  resp <- request(url) |>
    req_user_agent("Mozilla/5.0") |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  if (resp_status(resp) != 200) {
    stop(sprintf("MLB prospect request failed (HTTP %s)", resp_status(resp)),
         call. = FALSE)
  }

  extract_mlb_prospects_from_html(resp_body_string(resp))
}

extract_next_data_json <- function(html) {
  matches <- str_match(
    html,
    '<script[^>]+id=["\']__NEXT_DATA__["\'][^>]*>(.*?)</script>'
  )
  if (is.na(matches[, 2])) {
    stop("FanGraphs Board HTML did not include __NEXT_DATA__ payload.",
         call. = FALSE)
  }

  fromJSON(html_unescape_min(matches[, 2]), simplifyVector = FALSE)
}

extract_fangraphs_board_rows <- function(next_data) {
  queries <- next_data$props$pageProps$dehydratedState$queries
  if (is.null(queries) || length(queries) == 0) return(list())

  candidates <- keep(queries, function(query) {
    key <- paste(unlist(query$queryKey), collapse = " ")
    str_detect(key, "prospects/the-board")
  })

  if (length(candidates) == 0) {
    candidates <- keep(queries, function(query) {
      rows <- query$state$data
      is.list(rows) &&
        length(rows) > 0 &&
        is.list(rows[[1]]) &&
        !is.null(rows[[1]]$playerName)
    })
  }

  if (length(candidates) == 0) return(list())
  candidates[[1]]$state$data
}

extract_fangraphs_board_from_html <- function(html) {
  rows <- extract_next_data_json(html) %>%
    extract_fangraphs_board_rows()

  if (length(rows) == 0) return(empty_fangraphs_prospects())

  map_dfr(rows, function(row) {
    source_rank <- parse_numeric_prefix(first_present(
      row$Ovr_Rank, row$cOvr_Rank, row$cOVR, default = NA_real_
    ))
    fv <- parse_numeric_prefix(first_present(
      row$FV_Current, row$cFV, row$FV, default = NA_real_
    ))

    tibble(
      Name = str_squish(as.character(first_present(
        row$playerName, row$Name, row$Player, default = NA_character_
      ))),
      source = "fangraphs",
      source_rank = source_rank,
      mlb_org = as.character(first_present(
        row$Team, row$Org, row$Signed_Org, default = NA_character_
      )),
      position = as.character(first_present(
        row$Position, row$positionDB, row$Pos, default = NA_character_
      )),
      level = as.character(first_present(
        row$llevel, row$mlevel, row$Level, default = NA_character_
      )),
      eta = suppressWarnings(as.integer(first_present(
        row$ETA_Current, row$cETA, row$ETA, default = NA_integer_
      ))),
      age = suppressWarnings(as.numeric(first_present(
        row$Age, row$age, default = NA_real_
      ))),
      fg_rank = source_rank,
      fg_fv = fv,
      fg_risk = as.character(first_present(
        row$cRisk, row$Risk, row$Variance, default = NA_character_
      ))
    )
  }) %>%
    filter(!is.na(Name), Name != "") %>%
    arrange(is.na(source_rank), source_rank)
}

read_fangraphs_prospect_csv <- function(csv_url,
                                        fg_cookie = Sys.getenv("FANGRAPHS_COOKIE", unset = "")) {
  if (str_detect(csv_url, "^https?://")) {
    req <- request(csv_url) |>
      req_user_agent("Mozilla/5.0") |>
      req_headers(
        Accept = "text/csv, application/csv, text/plain, */*",
        Referer = "https://www.fangraphs.com/prospects/the-board"
      ) |>
      req_error(is_error = function(resp) FALSE)

    if (nzchar(fg_cookie)) {
      req <- req |> req_headers(Cookie = fg_cookie)
    }

    resp <- req_perform(req)
    status <- resp_status(resp)
    body <- resp_body_string(resp)

    if (status != 200) {
      if (status %in% c(401, 403)) {
        stop(sprintf(
          "FanGraphs prospect request failed (HTTP %s). Set FANGRAPHS_COOKIE from a logged-in browser session so Railway can read the Board page.",
          status
        ), call. = FALSE)
      }
      stop(sprintf("FanGraphs prospect CSV request failed (HTTP %s)", status),
           call. = FALSE)
    }
    if (str_detect(str_sub(str_squish(body), 1, 80), "^<")) {
      return(extract_fangraphs_board_from_html(body))
    }

    return(read_csv(I(body), show_col_types = FALSE))
  }

  read_csv(csv_url, show_col_types = FALSE)
}

standardize_fangraphs_prospects <- function(fg_raw) {
  fg_raw <- fg_raw %>%
    rename_with(~ "Name", any_of(c("Player", "PlayerName", "Name")))

  if (!"Name" %in% names(fg_raw)) {
    stop("FanGraphs prospect CSV must include a Name/Player/PlayerName column.",
         call. = FALSE)
  }

  source_rank <- parse_numeric_prefix(coalesce(
    df_col(fg_raw, c("source_rank"), NA_real_),
    df_col(fg_raw, c("Rank"), NA_real_),
    df_col(fg_raw, c("FV Rank"), NA_real_)
  ))

  tibble(
    Name = str_squish(as.character(fg_raw$Name)),
    source = "fangraphs",
    source_rank = source_rank,
    mlb_org = as.character(coalesce(
      df_col(fg_raw, c("mlb_org"), NA_character_),
      df_col(fg_raw, c("Org"), NA_character_),
      df_col(fg_raw, c("Team"), NA_character_)
    )),
    position = as.character(coalesce(
      df_col(fg_raw, c("position"), NA_character_),
      df_col(fg_raw, c("Pos"), NA_character_),
      df_col(fg_raw, c("Position"), NA_character_)
    )),
    level = as.character(coalesce(
      df_col(fg_raw, c("level"), NA_character_),
      df_col(fg_raw, c("Level"), NA_character_)
    )),
    eta = suppressWarnings(as.integer(coalesce(
      df_col(fg_raw, c("eta"), NA_integer_),
      df_col(fg_raw, c("ETA"), NA_integer_)
    ))),
    age = suppressWarnings(as.numeric(coalesce(
      df_col(fg_raw, c("age"), NA_real_),
      df_col(fg_raw, c("Age"), NA_real_)
    ))),
    fg_rank = source_rank,
    fg_fv = parse_numeric_prefix(coalesce(
      df_col(fg_raw, c("fg_fv"), NA_real_),
      df_col(fg_raw, c("FV"), NA_real_)
    )),
    fg_risk = as.character(coalesce(
      df_col(fg_raw, c("fg_risk"), NA_character_),
      df_col(fg_raw, c("Risk"), NA_character_)
    ))
  ) %>%
    filter(!is.na(Name), Name != "")
}

fetch_fangraphs_prospects <- function() {
  csv_url <- Sys.getenv("FANGRAPHS_PROSPECTS_CSV_URL", unset = "")
  if (!nzchar(csv_url)) {
    warning("FANGRAPHS_PROSPECTS_CSV_URL not set; skipping FanGraphs prospect export.")
    return(empty_fangraphs_prospects())
  }

  read_fangraphs_prospect_csv(csv_url) %>%
    standardize_fangraphs_prospects()
}

write_prospect_snapshot <- function(df, latest_path, timestamp = Sys.time()) {
  dir.create(dirname(latest_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, latest_path)

  stamp <- format(timestamp, "%Y%m%d_%H%M%S")
  stamped <- str_replace(latest_path, "_latest[.]csv$", paste0("_", stamp, ".csv"))
  write_csv(df, stamped)
  invisible(latest_path)
}

download_prospect_rankings <- function() {
  warnings <- character(0)

  fetch_source <- function(label, fn) {
    source_warnings <- character(0)
    rows <- tryCatch(
      withCallingHandlers(
        fn(),
        warning = function(w) {
          source_warnings <<- c(source_warnings, w$message)
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        source_warnings <<- c(
          source_warnings,
          sprintf("%s prospect fetch failed: %s", label, e$message)
        )
        tibble()
      }
    )
    warnings <<- c(warnings, source_warnings)
    rows
  }

  mlb <- fetch_source("MLB", fetch_mlb_prospects)
  fg <- fetch_source("FanGraphs", fetch_fangraphs_prospects)

  if (nrow(mlb) == 0) {
    warnings <- c(warnings, "MLB prospect source returned 0 rows.")
  }
  if (nrow(fg) == 0) {
    warnings <- c(warnings, "FanGraphs prospect source returned 0 rows.")
  }
  warnings <- unique(warnings)

  write_prospect_snapshot(mlb, "data/raw/prospects_mlb_latest.csv")
  write_prospect_snapshot(fg, "data/raw/prospects_fangraphs_latest.csv")

  status <- list(
    last_updated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status = if (nrow(mlb) > 0 || nrow(fg) > 0) "success" else "error",
    warnings = warnings,
    n_mlb = nrow(mlb),
    n_fangraphs = nrow(fg)
  )
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  write_json(status, "data/processed/prospect_rankings_status.json",
             auto_unbox = TRUE, pretty = TRUE)

  if (nrow(mlb) == 0 && nrow(fg) == 0) {
    stop(
      sprintf(
        "No prospect ranking sources were available. Details: %s",
        paste(warnings, collapse = " | ")
      ),
      call. = FALSE
    )
  }

  invisible(list(mlb = mlb, fangraphs = fg, status = status))
}

if (sys.nframe() == 0L && !interactive()) {
  download_prospect_rankings()
}
