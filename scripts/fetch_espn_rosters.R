# scripts/fetch_espn_rosters.R
# Fetch current ESPN Fantasy Baseball rosters via ESPN v3 API.
#
# Output CSV schema:
#   team_id, team_name, player_id, player_name, pro_team_id,
#   default_position_id, lineup_slot_id, lineup_slot
#
# Requires env vars:
#   ESPN_LEAGUE_ID
#   ESPN_SEASON (or defaults to current year)
#   ESPN_S2, SWID (for private leagues)

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

req_env <- function(name) {
  val <- Sys.getenv(name, unset = "")
  if (identical(val, "")) stop(sprintf("Missing env var: %s", name), call. = FALSE)
  val
}

.fetch_espn_json_roster <- function(url, query = NULL, espn_s2 = "", swid = "") {
  hdrs <- c(
    "Accept" = "application/json",
    "User-Agent" = "Mozilla/5.0"
  )
  if (espn_s2 != "" && swid != "") {
    hdrs["Cookie"] <- paste0("espn_s2=", espn_s2, "; SWID=", swid)
  }

  resp <- GET(url, add_headers(.headers = hdrs), query = query)

  status <- status_code(resp)
  txt <- content(resp, "text", encoding = "UTF-8")

  if (status >= 300) {
    stop(sprintf(
      "HTTP %s from ESPN.\nURL: %s\nFirst 300 chars:\n%s",
      status, url, substr(txt, 1, 300)
    ), call. = FALSE)
  }

  fromJSON(txt, simplifyVector = FALSE)
}

# ESPN lineup slot ID -> label mapping for fantasy baseball
.espn_slot_label <- function(slot_id) {
  labels <- c(
    "0" = "C", "1" = "1B", "2" = "2B", "3" = "3B", "4" = "SS",
    "5" = "LF", "6" = "CF", "7" = "RF",
    "12" = "UTIL", "13" = "DH", "14" = "P",
    "15" = "SP", "16" = "RP",
    "19" = "IF", "20" = "BE", "21" = "IL",
    "23" = "OF", "24" = "MI", "25" = "CI"
  )
  label <- labels[as.character(slot_id)]
  if (is.na(label)) paste0("SLOT_", slot_id) else as.character(label)
}

#' Fetch current ESPN Fantasy Baseball rosters.
#'
#' @param season Season year. Defaults to ESPN_SEASON env var or current year.
#' @param out_latest Path for the "latest" output CSV. NULL to skip writing.
#' @param out_dir Directory for timestamped snapshots.
#' @param verbose Print progress messages.
#' @return tibble with one row per rostered player.
fetch_espn_rosters <- function(
    season = NULL,
    out_latest = "data/raw/espn_rosters_latest.csv",
    out_dir = "data/raw",
    verbose = FALSE
) {
  league_id <- req_env("ESPN_LEAGUE_ID")
  if (is.null(season)) {
    season <- as.integer(Sys.getenv("ESPN_SEASON",
                                     unset = format(Sys.Date(), "%Y")))
  }
  espn_s2 <- Sys.getenv("ESPN_S2", unset = "")
  swid    <- Sys.getenv("SWID", unset = "")

  if (espn_s2 == "" || swid == "") {
    message("Note: ESPN_S2 or SWID missing; this will only work for public leagues.")
  }

  api_host <- "https://lm-api-reads.fantasy.espn.com/apis/v3"
  base_url <- sprintf("%s/games/flb/seasons/%d/segments/0/leagues/%s",
                      api_host, as.integer(season), league_id)

  if (verbose) message("Fetching ESPN rosters from: ", base_url)

  # Step A: fetch team names via mTeam view
  dat_teams <- .fetch_espn_json_roster(
    base_url, query = list(view = "mTeam"),
    espn_s2 = espn_s2, swid = swid
  )
  team_lookup <- list()
  for (t in (dat_teams$teams %||% list())) {
    tid <- as.character(t$id %||% "")
    if (tid != "") team_lookup[[tid]] <- t$name %||% NA_character_
  }
  if (verbose) message(sprintf("  Got names for %d teams", length(team_lookup)))

  # Step B: fetch roster entries via mRoster view
  dat_roster <- .fetch_espn_json_roster(
    base_url, query = list(view = "mRoster"),
    espn_s2 = espn_s2, swid = swid
  )

  if (is.null(dat_roster$teams)) {
    stop("No teams found in ESPN roster response.", call. = FALSE)
  }

  rows <- list()

  for (t in dat_roster$teams) {
    team_id   <- t$id %||% NA_integer_
    # Look up team name from mTeam data
    team_name <- team_lookup[[as.character(team_id)]] %||%
                 t$name %||% NA_character_

    entries <- t$roster$entries %||% list()
    if (length(entries) == 0) next

    for (e in entries) {
      player_info <- e$playerPoolEntry$player %||% list()
      slot_id     <- e$lineupSlotId %||% NA_integer_

      rows[[length(rows) + 1]] <- tibble(
        team_id             = as.integer(team_id),
        team_name           = as.character(team_name),
        player_id           = as.integer(player_info$id %||% e$playerId %||% NA_integer_),
        player_name         = as.character(player_info$fullName %||% NA_character_),
        pro_team_id         = as.integer(player_info$proTeamId %||% NA_integer_),
        default_position_id = as.integer(player_info$defaultPositionId %||% NA_integer_),
        lineup_slot_id      = as.integer(slot_id),
        lineup_slot         = .espn_slot_label(slot_id)
      )
    }
  }

  result <- bind_rows(rows)

  if (nrow(result) > 0 && !is.null(out_latest)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    write_csv(result, out_latest)

    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    snap_path <- file.path(out_dir, paste0("espn_rosters_", ts, ".csv"))
    write_csv(result, snap_path)

    if (verbose) message("Wrote: ", out_latest, " and ", snap_path)
  }

  if (verbose) {
    message(sprintf("Fetched %d players across %d teams",
                    nrow(result), length(unique(result$team_name))))
  }

  invisible(result)
}

# Auto-run only when executed directly (not when source()'d)
if (sys.nframe() == 0L && !interactive()) {
  fetch_espn_rosters(verbose = TRUE)
}
