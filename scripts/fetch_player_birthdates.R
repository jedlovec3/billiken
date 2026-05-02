# fetch_player_birthdates.R
#
# Phase 2 helper. Builds and lazily refreshes a cache of MLB player
# birthdates at `data/processed/player_birthdates.csv` so that
# build_team_assets.R can compute age and apply an aging curve when
# projecting multi-year value.
#
# Strategy:
#   1. Read the existing cache (if any) so we never re-fetch a known
#      birthdate.
#   2. Build a list of {Name, key_keep, mlbamid} for every Billiken player
#      we know about. The xMLBAMID lives in
#      `data/raw/auction_values_2026.csv` (column xMLBAMID); we join by
#      normalized FanGraphs Name.
#   3. For any MLBAMID we don't yet have a birthdate for, query the public
#      MLB Stats API (`https://statsapi.mlb.com/api/v1/people/<id>`) and
#      parse `birthDate`. The endpoint is unauthenticated and the same
#      service ESPN/MLB sites use; no token needed.
#   4. Append new rows to the cache and write it back atomically.
#
# Run standalone:
#   Rscript scripts/fetch_player_birthdates.R
#
# Source from another script:
#   source("scripts/fetch_player_birthdates.R")  # no auto-run if sourced
#
# Behavior on failure: if the API is unreachable or any individual
# request fails, the cache is preserved and the script exits cleanly.
# Subsequent runs will retry.

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringi)
  library(httr2)
  library(jsonlite)
})

.fpb_normalize_name <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    stringr::str_replace_all("\u00A0", " ") %>%
    stringr::str_replace_all("[.]", "") %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()
}

.fpb_strip_suffixes <- function(x) {
  x %>% stringr::str_replace_all(",|\\s+(jr|sr|ii|iii|iv|v)\\.?$", "")
}

.fpb_resolve_root <- function() {
  if (file.exists("billiken.Rproj")) return(getwd())
  if (file.exists("../billiken.Rproj")) return(normalizePath(".."))
  getwd()
}

# Fetch a single MLBAM player record. Returns NA on any failure.
.fpb_fetch_birthdate <- function(mlbamid) {
  if (is.na(mlbamid) || mlbamid == "" || mlbamid == "0") return(NA_character_)
  url <- sprintf("https://statsapi.mlb.com/api/v1/people/%s", mlbamid)
  out <- tryCatch({
    resp <- httr2::request(url) %>%
      httr2::req_timeout(10) %>%
      httr2::req_error(is_error = \(resp) FALSE) %>%
      httr2::req_perform()

    if (httr2::resp_status(resp) >= 300) return(NA_character_)

    body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    people <- body$people
    if (is.null(people) || length(people) == 0) return(NA_character_)
    bd <- people[[1]]$birthDate
    if (is.null(bd) || nchar(bd) == 0) return(NA_character_)
    bd
  }, error = function(e) NA_character_)
  out
}

# Public helper. Sources the player universe (auction_values + team_assets
# if available), refreshes the cache for any MLBAMID without a known
# birthdate, and returns the full cache as a tibble.
build_player_birthdates_cache <- function(
  auction_values_path = "data/raw/auction_values_2026.csv",
  team_assets_path    = "data/processed/team_assets.csv",
  cache_path          = "data/processed/player_birthdates.csv",
  max_lookups         = 1000L,
  verbose             = TRUE
) {
  root <- .fpb_resolve_root()
  resolve <- function(p) file.path(root, p)

  cache <- if (file.exists(resolve(cache_path))) {
    readr::read_csv(resolve(cache_path), show_col_types = FALSE) %>%
      mutate(mlbamid = as.character(mlbamid))
  } else {
    tibble(
      mlbamid    = character(),
      Name       = character(),
      key_keep   = character(),
      birth_date = character(),
      birth_year = integer(),
      fetched_at = character()
    )
  }

  # Source the player universe from auction_values (carries MLBAMID).
  if (!file.exists(resolve(auction_values_path))) {
    if (verbose) message(sprintf("No %s; skipping birthdate refresh.", auction_values_path))
    return(cache)
  }

  auction <- readr::read_csv(resolve(auction_values_path), show_col_types = FALSE) %>%
    rename_with(~ "mlbamid", any_of(c("xMLBAMID", "MLBAMID", "mlbamid"))) %>%
    rename_with(~ "Name",    any_of(c("PlayerName", "Player", "Name"))) %>%
    filter(!is.na(mlbamid), mlbamid != "", mlbamid != "0", !is.na(Name)) %>%
    mutate(
      mlbamid  = as.character(mlbamid),
      key_keep = .fpb_normalize_name(Name)
    ) %>%
    distinct(mlbamid, .keep_all = TRUE) %>%
    select(mlbamid, Name, key_keep)

  # Restrict to players currently rostered if team_assets is available, so
  # we don't pay the API cost for the whole player universe up front.
  if (file.exists(resolve(team_assets_path))) {
    rostered_keys <- readr::read_csv(resolve(team_assets_path), show_col_types = FALSE) %>%
      mutate(key_keep = .fpb_normalize_name(Name)) %>%
      pull(key_keep) %>%
      unique()
    auction <- auction %>% filter(key_keep %in% rostered_keys)
    if (verbose) {
      message(sprintf("Restricting birthdate refresh to %d rostered MLBAMIDs",
                      nrow(auction)))
    }
  }

  needed <- auction %>%
    anti_join(cache %>% filter(!is.na(birth_date)), by = "mlbamid") %>%
    head(max_lookups)

  if (nrow(needed) == 0) {
    if (verbose) message("Birthdate cache already covers every rostered MLBAMID.")
    return(cache)
  }

  if (verbose) {
    message(sprintf("Fetching %d new birthdate(s) from MLB Stats API...",
                    nrow(needed)))
  }

  fetched_rows <- vector("list", nrow(needed))
  for (i in seq_len(nrow(needed))) {
    row <- needed[i, ]
    bd  <- .fpb_fetch_birthdate(row$mlbamid)
    fetched_rows[[i]] <- tibble(
      mlbamid    = row$mlbamid,
      Name       = row$Name,
      key_keep   = row$key_keep,
      birth_date = bd,
      birth_year = if (!is.na(bd)) suppressWarnings(as.integer(substr(bd, 1, 4))) else NA_integer_,
      fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
    # Light pacing to be polite to the public API.
    Sys.sleep(0.05)
  }

  new_rows <- bind_rows(fetched_rows)
  cache <- bind_rows(
    cache %>% filter(!mlbamid %in% new_rows$mlbamid),
    new_rows
  )

  dir.create(dirname(resolve(cache_path)), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(cache, resolve(cache_path))

  n_resolved <- sum(!is.na(new_rows$birth_date))
  if (verbose) {
    message(sprintf(
      "Birthdate cache updated: +%d resolved, %d failed (cache now %d rows)",
      n_resolved, nrow(new_rows) - n_resolved, nrow(cache)
    ))
  }

  cache
}

# Auto-run only when invoked via Rscript (not when sourced into another
# script that already controls execution).
if (sys.nframe() == 0L) {
  build_player_birthdates_cache()
}
