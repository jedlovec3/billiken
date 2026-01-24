# scripts/fetch_espn_positions.R
# Fetch ESPN Fantasy Baseball player eligibility (positions) via ESPN v3 API.
#
# Output CSV schema:
#   PLAYER, C, 1B, 2B, 3B, SS, LF, CF, RF, DH, SP, RP
#
# .Renviron (gitignored):
#   ESPN_LEAGUE_ID="14845"
#   ESPN_SEASON="2025"
#   ESPN_SCORING_PERIOD_ID="196"
#   ESPN_PLATFORM_VERSION="7d4eaefaf4829a7a88c1ee957dc86f9ed5b7c0ce"  # optional
#   ESPN_S2="..."   # required for private leagues
#   SWID="{...}"    # required for private leagues

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(readr)
  library(tibble)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

req_env <- function(name) {
  val <- Sys.getenv(name, unset = "")
  if (identical(val, "")) stop(sprintf("Missing env var: %s", name), call. = FALSE)
  val
}

.fetch_espn_json <- function(url, query = NULL, xff = NULL, espn_s2 = "", swid = "") {
  hdrs <- c(
    "Accept" = "application/json",
    "User-Agent" = "Mozilla/5.0"
  )
  if (!is.null(xff)) hdrs["X-Fantasy-Filter"] <- xff
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
  
  if (grepl("text/html", ctype, ignore.case = TRUE) || grepl("^\\s*<!DOCTYPE html", txt)) {
    stop(sprintf(
      "Expected JSON but got HTML.\nURL: %s\nContent-Type: %s\nFirst 300 chars:\n%s",
      url, ctype, substr(txt, 1, 300)
    ), call. = FALSE)
  }
  
  fromJSON(txt, simplifyVector = FALSE)
}

.extract_player_rows <- function(dat) {
  # Unwrap list-of-one (rare, but safe)
  if (is.list(dat) && length(dat) == 1 && is.list(dat[[1]]) && !is.null(dat[[1]]$players)) {
    dat <- dat[[1]]
  }
  if (is.null(dat$players)) return(NULL)
  
  p <- dat$players
  
  # Case A: data.frame -> list-of-rows
  if (is.data.frame(p) && nrow(p) > 0) return(split(p, seq_len(nrow(p))))
  
  # Case B: wrapper contains $players
  if (is.list(p) && !is.null(p$players) && is.list(p$players) && length(p$players) > 0) return(p$players)
  
  # Case C/D: list of records (named or unnamed)
  if (is.list(p) && length(p) > 0 && is.list(p[[1]])) return(p)
  
  NULL
}

#' Fetch ESPN positions eligibility and write to CSV.
#'
#' @param out_latest Path for the "latest" output CSV.
#' @param out_dir Directory where timestamped snapshots are written (optional).
#' @param limit Page size for ESPN API paging.
#' @param verbose If TRUE, prints progress.
#' @return tibble with columns PLAYER, C, 1B, 2B, 3B, SS, LF, CF, RF, DH, SP, RP
fetch_espn_positions <- function(
    out_latest = "data/raw/positions_latest.csv",
    out_dir = "data/raw",
    limit = 500,
    verbose = FALSE
) {
  league_id <- req_env("ESPN_LEAGUE_ID")
  season    <- as.integer(req_env("ESPN_SEASON"))
  
  scoring_period_id <- as.integer(Sys.getenv("ESPN_SCORING_PERIOD_ID", unset = ""))
  if (is.na(scoring_period_id)) {
    stop("Missing ESPN_SCORING_PERIOD_ID (e.g., 196) in .Renviron", call. = FALSE)
  }
  
  platform_version <- Sys.getenv("ESPN_PLATFORM_VERSION", unset = "")
  espn_s2 <- Sys.getenv("ESPN_S2", unset = "")
  swid    <- Sys.getenv("SWID", unset = "")
  
  if (espn_s2 == "" || swid == "") {
    message("Note: ESPN_S2 or SWID missing; this will only work for public leagues.")
  }
  
  api_host <- "https://lm-api-reads.fantasy.espn.com/apis/v3"
  base_url <- sprintf("%s/games/flb/seasons/%d/segments/0/leagues/%s", api_host, season, league_id)
  
  query <- list(scoringPeriodId = scoring_period_id, view = "kona_player_info")
  if (platform_version != "") query$platformVersion <- platform_version
  
  if (verbose) {
    message("ESPN URL: ", base_url)
    message("Query: ", paste(names(query), query, sep = "=", collapse = "&"))
  }
  
  fetch_page <- function(offset) {
    xff <- toJSON(
      list(
        players = list(
          limit = limit,
          offset = offset,
          sortPercOwned = list(sortPriority = 1, sortAsc = FALSE) # required with limit/offset
        )
      ),
      auto_unbox = TRUE
    )
    
    dat <- .fetch_espn_json(base_url, query = query, xff = xff, espn_s2 = espn_s2, swid = swid)
    rows <- .extract_player_rows(dat)
    if (is.null(rows) || length(rows) == 0) return(tibble())
    
    get_id <- function(x) if (is.list(x)) as.integer(x$player$id %||% x$id %||% NA_integer_) else NA_integer_
    get_name <- function(x) if (is.list(x)) (x$player$fullName %||% x$fullName %||% NA_character_) else NA_character_
    get_slots <- function(x) {
      if (!is.list(x)) return(integer(0))
      s <- x$player$eligibleSlots %||% x$eligibleSlots %||% integer(0)
      as.integer(unlist(s))
    }
    
    tibble(
      espn_player_id = map_int(rows, get_id),
      player = map_chr(rows, get_name),
      eligibleSlots = map(rows, get_slots)
    ) %>%
      filter(!is.na(espn_player_id), !is.na(player))
  }
  
  pages <- list()
  offset <- 0
  repeat {
    if (verbose) message("Fetching offset=", offset)
    p <- fetch_page(offset)
    if (nrow(p) == 0) break
    pages[[length(pages) + 1]] <- p
    offset <- offset + limit
    if (offset > 40000) break
  }
  
  players <- bind_rows(pages) %>% distinct(espn_player_id, .keep_all = TRUE)
  if (verbose) {
    message("Players fetched: ", nrow(players))
    message("Players with any eligibleSlots: ", sum(lengths(players$eligibleSlots) > 0))
  }
  if (nrow(players) == 0) stop("No players returned. Check cookies and scoringPeriodId.", call. = FALSE)
  
  # SlotId -> label mapping (your league confirmed these)
  slot_to_label <- c(
    "0"  = "C",
    "1"  = "1B",
    "2"  = "2B",
    "3"  = "3B",
    "4"  = "SS",
    "5"  = "OF",
    "12" = "DH",
    "14" = "SP",
    "15" = "RP"
  )
  
  elig_long <- players %>%
    select(player, eligibleSlots) %>%
    unnest(eligibleSlots) %>%
    rename(slotId = eligibleSlots) %>%
    distinct()
  
  elig_wide <- elig_long %>%
    mutate(pos = slot_to_label[as.character(slotId)]) %>%
    filter(!is.na(pos)) %>%
    mutate(val = 1L) %>%
    select(player, pos, val) %>%
    pivot_wider(names_from = pos, values_from = val, values_fill = 0L)
  
  desired <- c("PLAYER","C","1B","2B","3B","SS","LF","CF","RF","DH","SP","RP")
  out <- elig_wide %>% rename(PLAYER = player)
  
  # Policy: if ESPN only provides OF, copy to LF/CF/RF
  if ("OF" %in% names(out)) {
    if (!("LF" %in% names(out))) out$LF <- out$OF
    if (!("CF" %in% names(out))) out$CF <- out$OF
    if (!("RF" %in% names(out))) out$RF <- out$OF
    out$OF <- NULL
  }
  
  for (col in setdiff(desired, names(out))) out[[col]] <- 0L
  out <- out[, desired]
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Write latest + timestamped snapshot
  write_csv(out, out_latest)
  
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  snap_path <- file.path(out_dir, paste0("positions_", ts, ".csv"))
  write_csv(out, snap_path)
  
  if (verbose) message("Wrote: ", out_latest, " and ", snap_path)
  invisible(out)
}

# Allow running as a script:
if (identical(environmentName(environment()), "R_GlobalEnv")) {
  # If sourced, do nothing. If executed via Rscript, run once.
  # Heuristic: if interactive() is FALSE, assume script run.
  if (!interactive()) {
    fetch_espn_positions(verbose = TRUE)
  }
}
