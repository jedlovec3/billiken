# scripts/fetch_espn_standings.R
# Fetch ESPN Fantasy Baseball historic standings via ESPN v3 API.
#
# Output CSV schema:
#   season, team_id, team_name, abbrev, owner, final_standing, points_for, wins, losses, ties
#
# .Renviron (gitignored):
#   ESPN_LEAGUE_ID="14845"
#   ESPN_S2="..."   # required for private leagues
#   SWID="{...}"    # required for private leagues

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

.fetch_espn_json <- function(url, query = NULL, espn_s2 = "", swid = "") {
  hdrs <- c(
    "Accept" = "application/json",
    "User-Agent" = "Mozilla/5.0"
  )
  if (espn_s2 != "" && swid != "") {
    hdrs["Cookie"] <- paste0("espn_s2=", espn_s2, "; SWID=", swid)
  }
  
  resp <- GET(url, add_headers(.headers = hdrs), query = query)
  
  status <- status_code(resp)
  ctype  <- headers(resp)[["content-type"]] %||% ""
  txt    <- content(resp, "text", encoding = "UTF-8")
  
  if (status >= 300) {
    stop(sprintf(
      "HTTP %s from ESPN.\nURL: %s\nQuery: %s\nContent-Type: %s\nFirst 300 chars:\n%s",
      status,
      url,
      if (is.null(query)) "" else paste(names(query), query, sep = "=", collapse = "&"),
      ctype,
      substr(txt, 1, 300)
    ), call. = FALSE)
  }
  
  fromJSON(txt, simplifyVector = FALSE)
}

# ESPN Baseball stat ID mapping
# These IDs map to the statId field in the ESPN API response
ESPN_STAT_IDS <- list(
  # Hitting stats
  R = 20,      # Runs
  HR = 5,      # Home Runs  
  RBI = 21,    # Runs Batted In
  SB = 23,     # Stolen Bases
  AVG = 2,     # Batting Average (calculated: H/AB)
  H = 1,       # Hits
  AB = 0,      # At Bats
  # Pitching stats
  W = 53,      # Wins
  SV = 57,     # Saves
  SO = 48,     # Strikeouts
  ERA = 47,    # Earned Run Average
  WHIP = 41,   # Walks + Hits per IP
  IP_OUTS = 34,# Innings Pitched (stored as outs, divide by 3)
  ER = 45,     # Earned Runs
  BB = 39,     # Walks (pitching)
  HA = 37      # Hits Allowed
)

#' Fetch ESPN historic standings for a given season.
#'
#' @param season The season year to fetch (e.g., 2025).
#' @param verbose If TRUE, prints progress.
#' @return tibble with columns: season, team_id, team_name, abbrev, owner, 
#'         final_standing, points_for, plus roto category totals
fetch_espn_standings <- function(season, verbose = FALSE) {
  league_id <- req_env("ESPN_LEAGUE_ID")
  espn_s2 <- Sys.getenv("ESPN_S2", unset = "")
  swid    <- Sys.getenv("SWID", unset = "")
  
  if (espn_s2 == "" || swid == "") {
    message("Note: ESPN_S2 or SWID missing; this will only work for public leagues.")
  }
  
  api_host <- "https://lm-api-reads.fantasy.espn.com/apis/v3"
  
  # Use leagueHistory endpoint for older seasons, regular endpoint for recent
  if (season < 2018) {
    base_url <- sprintf("%s/games/flb/leagueHistory/%s", api_host, league_id)
    query <- list(seasonId = season, view = "mTeam")
  } else {
    base_url <- sprintf("%s/games/flb/seasons/%d/segments/0/leagues/%s", api_host, season, league_id)
    query <- list(view = "mTeam")
  }
  
  if (verbose) {
    message("ESPN URL: ", base_url)
    message("Query: ", paste(names(query), query, sep = "=", collapse = "&"))
  }
  
  dat <- .fetch_espn_json(base_url, query = query, espn_s2 = espn_s2, swid = swid)
  
  # For leagueHistory, response is a list with one element
  if (is.list(dat) && length(dat) == 1 && !is.null(dat[[1]]$teams)) {
    dat <- dat[[1]]
  }
  
  if (is.null(dat$teams)) {
    stop("No teams found in ESPN response.", call. = FALSE)
  }
  
  teams <- dat$teams
  members <- dat$members %||% list()
  
  extract_team <- function(t) {
    # Extract owner info (primary owner)
    owners <- t$owners %||% list()
    owner_name <- if (length(owners) > 0) {
      owner_id <- owners[[1]]
      member <- Filter(function(m) identical(m$id, owner_id), members)
      if (length(member) > 0) {
        paste(member[[1]]$firstName %||% "", member[[1]]$lastName %||% "")
      } else {
        owner_id
      }
    } else {
      NA_character_
    }
    
    # Extract record
    record <- t$record$overall %||% list()
    
    # Extract cumulative stats from valuesByStat
    values_by_stat <- t$valuesByStat %||% list()
    
    # Helper to get stat value by ID
    get_stat <- function(stat_name) {
      stat_id <- as.character(ESPN_STAT_IDS[[stat_name]])
      val <- values_by_stat[[stat_id]]
      if (is.null(val)) NA_real_ else as.numeric(val)
    }
    
    tibble(
      season = season,
      team_id = t$id %||% NA_integer_,
      team_name = t$name %||% NA_character_,
      abbrev = t$abbrev %||% NA_character_,
      owner = trimws(owner_name),
      final_standing = t$playoffSeed %||% t$rankCalculatedFinal %||% NA_integer_,
      points_for = t$points %||% record$pointsFor %||% NA_real_,
      # Hitting categories
      R = get_stat("R"),
      HR = get_stat("HR"),
      RBI = get_stat("RBI"),
      SB = get_stat("SB"),
      AVG = get_stat("AVG"),
      AB = get_stat("AB"),
      H = get_stat("H"),
      # Pitching categories  
      W = get_stat("W"),
      SV = get_stat("SV"),
      SO = get_stat("SO"),
      ERA = get_stat("ERA"),
      WHIP = get_stat("WHIP"),
      IP = round(get_stat("IP_OUTS") / 3, 1),  # Convert outs to innings
      ER = get_stat("ER"),
      BB = get_stat("BB"),
      HA = get_stat("HA")
    )
  }
  
  standings <- bind_rows(lapply(teams, extract_team)) %>%
    arrange(final_standing)
  
  if (verbose) {
    message(sprintf("Fetched %d teams for season %d", nrow(standings), season))
  }
  
  standings
}

#' Fetch ESPN standings for multiple seasons and write to CSV.
#'
#' @param seasons Vector of season years to fetch.
#' @param out_latest Path for the "latest" output CSV.
#' @param out_dir Directory where timestamped snapshots are written (optional).
#' @param verbose If TRUE, prints progress.
#' @return tibble with standings for all requested seasons.
fetch_espn_standings_history <- function(
    seasons,
    out_latest = "data/raw/standings_history_latest.csv",
    out_dir = "data/raw",
    verbose = FALSE
) {
  all_standings <- list()
  
  for (season in seasons) {
    if (verbose) message(sprintf("\nFetching season %d...", season))
    tryCatch({
      standings <- fetch_espn_standings(season, verbose = verbose)
      all_standings[[as.character(season)]] <- standings
    }, error = function(e) {
      warning(sprintf("Failed to fetch season %d: %s", season, e$message))
    })
  }
  
  result <- bind_rows(all_standings)
  
  if (nrow(result) > 0) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Write latest + timestamped snapshot
    write_csv(result, out_latest)
    
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    snap_path <- file.path(out_dir, paste0("standings_history_", ts, ".csv"))
    write_csv(result, snap_path)
    
    if (verbose) message("\nWrote: ", out_latest, " and ", snap_path)
  }
  
  invisible(result)
}

# Allow running as a script:
if (identical(environmentName(environment()), "R_GlobalEnv")) {
  if (!interactive()) {
    # Default: fetch last 5 seasons
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    seasons <- (current_year - 5):(current_year - 1)
    
    message("Fetching ESPN standings history for seasons: ", paste(seasons, collapse = ", "))
    fetch_espn_standings_history(seasons, verbose = TRUE)
  }
}
