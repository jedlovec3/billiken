# scripts/trade_utils.R

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

.is_naish <- function(x) {
  x2 <- as.character(x)
  x2 <- trimws(x2)
  is.na(x) | x2 == "" | tolower(x2) %in% c("na", "null", "none", "free agent", "free agents", "fa", "available")
}

.standardize_team <- function(x) {
  x2 <- as.character(x)
  x2 <- trimws(x2)
  x2[.is_naish(x2)] <- NA_character_
  tolower(x2)
}

.get_col_name <- function(df, candidates) {
  nm <- names(df)
  lower <- tolower(nm)
  idx <- match(tolower(candidates), lower)
  idx <- idx[!is.na(idx)][1]
  if (is.na(idx) || is.null(idx)) return(NULL)
  nm[[idx]]
}

#' Read a scenario CSV that may contain both player trades and draft-pick trades.
#'
#' Player move rows (required):
#' - player (non-empty)
#' - from_team
#' - to_team (optional: blank/NA/NULL means "drop to free agent pool")
#' - ForceKeeper (optional): 1 = force keep, 0 = force drop
#' - DropPenalty (optional): numeric dead-money hit applied when the player is dropped
#'
#' Draft pick trade rows (required):
#' - round
#' - pick
#' - from_team
#' - to_team
#' - player should be empty/NA
#'
#' Backwards-compatible with the original 3-column format:
#' player,from_team,to_team
read_trade_scenario_csv <- function(trades_path) {
  raw <- readr::read_csv(trades_path, show_col_types = FALSE)

  player_col <- .get_col_name(raw, c("player"))
  from_col <- .get_col_name(raw, c("from_team", "from"))
  to_col <- .get_col_name(raw, c("to_team", "to"))
  round_col <- .get_col_name(raw, c("round"))
  pick_col <- .get_col_name(raw, c("pick"))
  force_col <- .get_col_name(raw, c("forcekeeper", "force_keeper", "force_keeper", "forcekeeper", "ForceKeeper"))
  penalty_col <- .get_col_name(raw, c("droppenalty", "drop_penalty", "salary_penalty", "cap_penalty", "dead_money", "DropPenalty", "SalaryPenalty"))

  required <- c("from_team" = from_col, "to_team" = to_col)
  missing <- names(required)[vapply(required, is.null, logical(1))]
  if (length(missing) > 0) {
    stop(sprintf(
      "Trades file is missing required columns: %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  # player is optional in the "pick trade" format, but required for player trades.
  player_vec <- if (is.null(player_col)) NA_character_ else as.character(raw[[player_col]])

  round_vec <- if (is.null(round_col)) NA_integer_ else suppressWarnings(as.integer(raw[[round_col]]))
  pick_vec <- if (is.null(pick_col)) NA_integer_ else suppressWarnings(as.integer(raw[[pick_col]]))

  force_vec <- if (is.null(force_col)) {
    NA_integer_
  } else {
    x <- raw[[force_col]]
    x <- ifelse(.is_naish(x), NA_character_, trimws(as.character(x)))
    suppressWarnings(as.integer(x))
  }

  penalty_vec <- if (is.null(penalty_col)) {
    NA_real_
  } else {
    suppressWarnings(as.numeric(raw[[penalty_col]]))
  }

  trades <- tibble::tibble(
    player = player_vec,
    from_team = as.character(raw[[from_col]]),
    to_team = as.character(raw[[to_col]]),
    round = round_vec,
    pick = pick_vec,
    force_keeper = force_vec,
    drop_penalty = penalty_vec
  ) %>%
    mutate(
      player = ifelse(.is_naish(player), NA_character_, trimws(as.character(player))),
      from_team = ifelse(.is_naish(from_team), NA_character_, trimws(as.character(from_team))),
      to_team = ifelse(.is_naish(to_team), NA_character_, trimws(as.character(to_team))),
      force_keeper = suppressWarnings(as.integer(force_keeper)),
      drop_penalty = suppressWarnings(as.numeric(drop_penalty))
    )

  bad_force <- which(!is.na(trades$force_keeper) & !(trades$force_keeper %in% c(0L, 1L)))
  if (length(bad_force) > 0) {
    stop(sprintf(
      "Trades file contains invalid ForceKeeper values (must be 0/1) in rows: %s",
      paste(bad_force, collapse = ", ")
    ), call. = FALSE)
  }

  is_player_trade <- !is.na(trades$player)
  is_pick_trade <- !is.na(trades$round) & !is.na(trades$pick)

  bad_rows <- which(
    !is_player_trade & !is_pick_trade & (
      !is.na(trades$from_team) |
        !is.na(trades$to_team) |
        !is.na(trades$force_keeper) |
        !is.na(trades$drop_penalty)
    )
  )
  if (length(bad_rows) > 0) {
    stop(sprintf(
      "Trades file contains %d row(s) that are neither a player trade nor a pick trade (rows: %s).",
      length(bad_rows),
      paste(bad_rows, collapse = ", ")
    ), call. = FALSE)
  }

  both_rows <- which(is_player_trade & is_pick_trade)
  if (length(both_rows) > 0) {
    stop(sprintf(
      "Trades file contains %d row(s) that specify both a player and a draft pick (rows: %s).",
      length(both_rows),
      paste(both_rows, collapse = ", ")
    ), call. = FALSE)
  }

  player_trades <- trades %>%
    filter(is_player_trade) %>%
    transmute(
      player = player,
      from_team = from_team,
      to_team = to_team,
      force_keeper = force_keeper,
      drop_penalty = drop_penalty
    ) %>%
    filter(!is.na(from_team))

  pick_trades <- trades %>%
    filter(!is_player_trade & is_pick_trade) %>%
    transmute(
      round = as.integer(round),
      pick = as.integer(pick),
      from_team = from_team,
      to_team = to_team
    ) %>%
    filter(!is.na(round) & !is.na(pick) & !is.na(from_team) & !is.na(to_team))

  list(player_trades = player_trades, pick_trades = pick_trades)
}

#' Read just the player-trade rows from a scenario CSV.
#'
#' (Backwards-compatible name; existing callers expect a data frame.)
read_trades_csv <- function(trades_path) {
  read_trade_scenario_csv(trades_path)$player_trades
}

#' Read just the pick-trade rows from a scenario CSV.
read_pick_trades_csv <- function(trades_path) {
  read_trade_scenario_csv(trades_path)$pick_trades
}

#' Apply draft-pick trades to a draft CSV data frame.
#'
#' The draft table is expected to have at least:
#' - Round
#' - Pick
#' - Team
#'
#' If Player exists and the pick is already used, the function will error.
apply_pick_trades_to_draft <- function(draft, pick_trades, strict_owner_check = TRUE) {
  if (nrow(pick_trades) == 0) return(draft)

  required <- c("Round", "Pick", "Team")
  missing <- setdiff(required, names(draft))
  if (length(missing) > 0) {
    stop(sprintf(
      "Draft file is missing required columns: %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  out <- draft %>%
    mutate(
      Round = suppressWarnings(as.integer(Round)),
      Pick = suppressWarnings(as.integer(Pick)),
      Team = as.character(Team)
    )

  # Canonicalize team names based on what's already present in the draft file.
  teams <- unique(out$Team[!is.na(out$Team) & out$Team != ""])
  team_map <- stats::setNames(teams, .standardize_team(teams))

  owner_map <- NULL
  if ("Owner" %in% names(out)) {
    owner_map <- out %>%
      filter(!is.na(Owner) & Owner != "", !is.na(Team) & Team != "") %>%
      distinct(Team, Owner) %>%
      group_by(Team) %>%
      slice(1) %>%
      ungroup() %>%
      {stats::setNames(.$Owner, .$Team)}
  }

  for (i in seq_len(nrow(pick_trades))) {
    r <- as.integer(pick_trades$round[[i]])
    p <- as.integer(pick_trades$pick[[i]])
    from_team <- pick_trades$from_team[[i]]
    to_team <- pick_trades$to_team[[i]]

    idx <- which(out$Round == r & out$Pick == p)
    if (length(idx) == 0) {
      stop(sprintf("Pick trade %d: no pick found for round=%d pick=%d.", i, r, p), call. = FALSE)
    }
    if (length(idx) > 1) {
      stop(sprintf(
        "Pick trade %d: multiple picks found for round=%d pick=%d; cannot disambiguate.",
        i, r, p
      ), call. = FALSE)
    }

    if ("Player" %in% names(out)) {
      already_picked <- !is.na(out$Player[idx]) && trimws(as.character(out$Player[idx])) != ""
      if (already_picked) {
        stop(sprintf(
          "Pick trade %d: round=%d pick=%d already has a Player assigned ('%s').",
          i, r, p, as.character(out$Player[idx])
        ), call. = FALSE)
      }
    }

    from_key <- .standardize_team(from_team)
    to_key <- .standardize_team(to_team)

    if (!from_key %in% names(team_map)) {
      stop(sprintf(
        "Pick trade %d: from_team '%s' not found in draft file.",
        i, from_team
      ), call. = FALSE)
    }

    if (!to_key %in% names(team_map)) {
      stop(sprintf(
        "Pick trade %d: to_team '%s' not found in draft file.",
        i, to_team
      ), call. = FALSE)
    }

    from_canon <- unname(team_map[[from_key]])
    to_canon <- unname(team_map[[to_key]])

    if (isTRUE(strict_owner_check)) {
      current_team <- as.character(out$Team[idx])
      if (.standardize_team(current_team) != .standardize_team(from_canon)) {
        stop(sprintf(
          "Pick trade %d: round=%d pick=%d is currently owned by '%s' (expected from_team '%s').",
          i, r, p, current_team, from_canon
        ), call. = FALSE)
      }
    }

    out$Team[idx] <- to_canon

    if (!is.null(owner_map) && to_canon %in% names(owner_map)) {
      out$Owner[idx] <- unname(owner_map[[to_canon]])
    }

    if ("Notes" %in% names(out)) {
      note <- sprintf("Scenario pick trade: %s -> %s", from_canon, to_canon)
      out$Notes[idx] <- ifelse(
        is.na(out$Notes[idx]) || trimws(as.character(out$Notes[idx])) == "",
        note,
        paste0(as.character(out$Notes[idx]), "; ", note)
      )
    }
  }

  out
}

#' Apply hypothetical trades to a prefreeze roster table.
#'
#' The roster is expected to have at least:
#' - billikenTeam
#' - player
#' - contract
#' - salary
#'
#' Salary and contract are preserved; only billikenTeam changes.
apply_trades_to_prefreeze_rosters <- function(prefreeze_rosters, trades) {
  if (!all(c("billikenTeam", "player") %in% names(prefreeze_rosters))) {
    stop(
      "prefreeze_rosters must include columns: billikenTeam, player",
      call. = FALSE
    )
  }

  if (nrow(trades) == 0) return(prefreeze_rosters)

  # Map normalized -> canonical team name as it appears in roster file.
  teams <- unique(prefreeze_rosters$billikenTeam[!is.na(prefreeze_rosters$billikenTeam)])
  team_map <- stats::setNames(teams, .standardize_team(teams))

  out <- prefreeze_rosters

  for (i in seq_len(nrow(trades))) {
    player <- trades$player[[i]]
    from_team <- trades$from_team[[i]]
    to_team <- trades$to_team[[i]]

    from_key <- .standardize_team(from_team)
    to_key <- .standardize_team(to_team)

    if (is.na(from_key) || !from_key %in% names(team_map)) {
      stop(sprintf(
        "Trade %d: from_team '%s' not found in prefreeze rosters.",
        i, from_team
      ), call. = FALSE)
    }

    is_drop <- is.na(to_key)
    if (!is_drop && !to_key %in% names(team_map)) {
      stop(sprintf(
        "Trade %d: to_team '%s' not found in prefreeze rosters.",
        i, to_team
      ), call. = FALSE)
    }

    from_canon <- unname(team_map[[from_key]])
    to_canon <- if (is_drop) NA_character_ else unname(team_map[[to_key]])

    matches <- which(
      !is.na(out$player) &
        out$player == player &
        !is.na(out$billikenTeam) &
        .standardize_team(out$billikenTeam) == .standardize_team(from_canon)
    )

    if (length(matches) == 0) {
      stop(sprintf(
        "Trade %d: player '%s' not found on team '%s'.",
        i, player, from_canon
      ), call. = FALSE)
    }

    out$billikenTeam[matches] <- to_canon
  }

  out
}
