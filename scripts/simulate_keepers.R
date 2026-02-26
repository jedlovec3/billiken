# simulate_keepers.R
# Simulate keeper selections from pre-freeze rosters using SGPAR values.
#
# Default (backwards compatible) behavior reads/writes under:
# - data/raw/prefreeze_rosters_latest.csv
# - data/processed/projected_player_value.csv
# - data/processed/{projections_prefreeze.csv, simulated_keepers.csv}
#
# This script now supports "what-if" scenarios via a trade overlay.

simulate_keepers <- function(
  sgpar_random = 0,
  prefreeze_rosters_path = "data/raw/prefreeze_rosters_latest.csv",
  projected_player_value_path = "data/processed/projected_player_value.csv",
  trades_path = NULL,
  output_dir = "data/processed",
  seed = NULL
) {
  cat("Running simulation with sgpar_random =", sgpar_random, "\n")

  suppressPackageStartupMessages({
    library(tidyverse)
    library(stringi)
  })

  # Optional helpers (for robust paths + trade overlay)
  if (file.exists("scripts/paths.R")) source("scripts/paths.R")
  if (file.exists("paths.R")) source("paths.R")
  if (file.exists("scripts/trade_utils.R")) source("scripts/trade_utils.R")
  if (file.exists("trade_utils.R")) source("trade_utils.R")

  root <- if (exists("find_project_root")) find_project_root() else getwd()

  resolve_path <- function(p) {
    if (is.null(p)) return(NULL)
    if (grepl("^/", p)) return(p)
    file.path(root, p)
  }

  out_dir <- resolve_path(output_dir)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  message(sprintf("Running simulate_keepers() with sgpar_random = %.3f", sgpar_random))

  # -----------------
  # Helpers
  # -----------------
  normalize_name <- function(x) {
    x %>%
      stri_trans_general("Latin-ASCII") %>%
      str_replace_all("\\u00A0", " ") %>%
      str_replace_all("[.]", "") %>%
      str_squish() %>%
      str_to_lower()
  }

  strip_suffixes <- function(x) {
    # Remove common suffixes that frequently appear inconsistently in sources
    # e.g. "Luis Robert Jr." vs "Luis Robert"
    x %>%
      str_replace_all(",|\\s+(jr|sr|ii|iii|iv|v)\\.?$", "")
  }

  resolve_roster_matches <- function(joined) {
    # joined includes a synthetic row_id, plus roster columns that may create duplicates.
    # We pick the best roster match per row_id, preferring pitcher/hitter alignment when possible.
    joined %>%
      mutate(
        proj_is_pitcher = player_type %in% c("pitcher"),
        roster_is_pitcher = slot == "P",
        match_score = case_when(
          is.na(billikenTeam) ~ -10,
          proj_is_pitcher & roster_is_pitcher ~ 2,
          (!proj_is_pitcher) & (!roster_is_pitcher) ~ 1,
          TRUE ~ 0
        )
      ) %>%
      group_by(row_id) %>%
      arrange(desc(match_score), desc(!is.na(billikenTeam))) %>%
      slice(1) %>%
      ungroup() %>%
      select(-proj_is_pitcher, -roster_is_pitcher, -match_score)
  }

  # -----------------
  # Load inputs
  # -----------------
  message("Loading inputs...")

  proj <- readr::read_csv(resolve_path(projected_player_value_path), show_col_types = FALSE)

  rosters_raw <- readr::read_csv(resolve_path(prefreeze_rosters_path), show_col_types = FALSE)

  prefreeze_rosters <- rosters_raw %>%
    filter(
      !is.na(player),
      !is.na(billikenTeam),
      player != "PLAYER",
      slot != "SLOT"
    ) %>%
    transmute(
      billikenTeam,
      slot,
      player,
      contract,
      salary = as.numeric(salary)
    )

  scenario_player_trades <- tibble::tibble()

  # Optional: apply trade overlay (moves players between teams before keeper selection)
  if (!is.null(trades_path)) {
    if (!exists("read_trade_scenario_csv") || !exists("apply_trades_to_prefreeze_rosters")) {
      stop("Trade helpers not found; expected scripts/trade_utils.R.", call. = FALSE)
    }

    scenario <- read_trade_scenario_csv(resolve_path(trades_path))
    scenario_player_trades <- scenario$player_trades

    bad_force <- scenario_player_trades %>%
      filter(!is.na(force_keeper) & force_keeper == 1L & is.na(to_team))
    if (nrow(bad_force) > 0) {
      stop(
        "ForceKeeper=1 rows must specify a real to_team (or keep to_team==from_team); to_team cannot be NA/NULL.",
        call. = FALSE
      )
    }

    message(sprintf("Applying %d player-move row(s) from %s", nrow(scenario_player_trades), trades_path))
    prefreeze_rosters <- apply_trades_to_prefreeze_rosters(prefreeze_rosters, scenario_player_trades)
  }

  # -----------------
  # Join projections to prefreeze rosters (careful name handling)
  # -----------------
  message("Joining projections to prefreeze rosters...")

  proj_keyed <- proj %>%
    mutate(
      row_id = row_number(),
      key_keep = normalize_name(Name),
      key_strip = normalize_name(strip_suffixes(Name))
    )

  rosters_keyed <- prefreeze_rosters %>%
    mutate(
      key_keep = normalize_name(player),
      key_strip = normalize_name(strip_suffixes(player))
    )

  # Pass 1: exact-ish name match (keep suffixes)
  pass1 <- proj_keyed %>%
    left_join(
      rosters_keyed %>% select(key_keep, billikenTeam, slot, contract, salary),
      by = join_by(key_keep),
      relationship = "many-to-many"
    ) %>%
    resolve_roster_matches()

  # Pass 2: for any unmatched players, try stripping suffixes (Jr/Sr/etc)
  unmatched <- pass1 %>% filter(is.na(billikenTeam))
  matched <- pass1 %>% filter(!is.na(billikenTeam))

  pass2 <- unmatched %>%
    select(-billikenTeam, -slot, -contract, -salary) %>%
    left_join(
      rosters_keyed %>% select(key_strip, billikenTeam, slot, contract, salary),
      by = join_by(key_strip),
      relationship = "many-to-many"
    ) %>%
    resolve_roster_matches()

  projections_prefreeze <- bind_rows(matched, pass2) %>%
    arrange(row_id) %>%
    select(-row_id, -key_keep, -key_strip)

  # -----------------
  # Export projections_prefreeze.csv
  # -----------------
  message("Writing projections_prefreeze.csv...")
  readr::write_csv(projections_prefreeze, file.path(out_dir, "projections_prefreeze.csv"))

  # -----------------
  # Simulate keepers (by sgpar, optionally with randomness)
  # -----------------
  message("Simulating keepers...")

  keeper_limits <- list(
    "Blue Socks" = 15,
    "Melonheads" = 15,
    "Erie Lakers" = 10,
    "National Pastime" = 15,
    "Big Red Machine" = 15,
    "Free At Last" = 15,
    "Free Birds" = 15,
    "Westside Marauders" = 15,
    "Louisville Sluggers" = 11,
    "Hoosiers" = 12
  )

  if (!is.null(seed)) {
    set.seed(seed)
  } else {
    set.seed(NULL)
  }

  projections_prefreeze <- projections_prefreeze %>%
    mutate(
      sgpar_random_delta = ifelse(
        sgpar_random > 0,
        sgpar * runif(n(), min = -sgpar_random, max = sgpar_random),
        0
      ),
      sgpar_randomized = sgpar + sgpar_random_delta
    )

  # -----------------
  # Optional: keeper overrides + cap penalties from scenario file
  # -----------------
  keeper_overrides_resolved <- tibble::tibble(
    Name = character(),
    billikenTeam = character(),
    force_keeper = integer(),
    drop_penalty = numeric(),
    player = character(),
    from_team = character(),
    to_team = character()
  )
  drop_penalties_by_team <- tibble::tibble(billikenTeam = character(), cap_penalty = numeric())

  if (nrow(scenario_player_trades) > 0) {
    team_map <- stats::setNames(names(keeper_limits), .standardize_team(names(keeper_limits)))

    canon_team <- function(x) {
      key <- .standardize_team(x)
      if (is.na(key) || !key %in% names(team_map)) {
        stop(sprintf("Unknown team in scenario file: '%s'", as.character(x)), call. = FALSE)
      }
      unname(team_map[[key]])
    }

    drop_penalties_by_team <- scenario_player_trades %>%
      mutate(
        is_drop = is.na(to_team) | (!is.na(force_keeper) & force_keeper == 0L),
        dropper_team_raw = ifelse(
          !is.na(force_keeper) & force_keeper == 0L & !is.na(to_team),
          to_team,
          from_team
        ),
        cap_penalty = replace_na(drop_penalty, 0)
      ) %>%
      filter(is_drop) %>%
      mutate(
        billikenTeam = vapply(dropper_team_raw, canon_team, character(1))
      ) %>%
      group_by(billikenTeam) %>%
      summarise(cap_penalty = sum(cap_penalty, na.rm = TRUE), .groups = "drop") %>%
      arrange(billikenTeam)

    readr::write_csv(drop_penalties_by_team, file.path(out_dir, "salary_cap_penalties.csv"))

    overrides <- scenario_player_trades %>%
      filter(!is.na(force_keeper)) %>%
      # If you explicitly drop a player via to_team=NA/NULL, they won't be on any roster;
      # ForceKeeper=0 is redundant in that case.
      filter(!(force_keeper == 0L & is.na(to_team)))

    if (nrow(overrides) > 0) {
      proj_keys <- projections_prefreeze %>%
        transmute(
          Name,
          roster_team = billikenTeam,
          key_keep = normalize_name(Name),
          key_strip = normalize_name(strip_suffixes(Name))
        )

      resolve_name <- function(p) {
        p_keep <- normalize_name(p)
        p_strip <- normalize_name(strip_suffixes(p))

        c_keep <- proj_keys %>% filter(key_keep == p_keep) %>% pull(Name) %>% unique()
        if (length(c_keep) == 1) return(c_keep)

        c_strip <- proj_keys %>% filter(key_strip == p_strip) %>% pull(Name) %>% unique()
        if (length(c_strip) == 1) return(c_strip)

        c_all <- unique(c(c_keep, c_strip))
        if (length(c_all) > 1) {
          stop(sprintf(
            "Keeper override player '%s' matches multiple projection names: %s",
            p,
            paste(c_all, collapse = ", ")
          ), call. = FALSE)
        }

        stop(sprintf(
          "Keeper override player '%s' not found in projections; check spelling or name normalization.",
          p
        ), call. = FALSE)
      }

      keeper_overrides_resolved <- overrides %>%
        mutate(
          Name = vapply(player, resolve_name, character(1)),
          team_raw = ifelse(!is.na(to_team), to_team, from_team),
          billikenTeam = vapply(team_raw, canon_team, character(1)),
          force_keeper = as.integer(force_keeper)
        ) %>%
        select(Name, billikenTeam, force_keeper, drop_penalty, player, from_team, to_team)

      # Validate that the player is on the specified team after applying player moves.
      roster_team_map <- projections_prefreeze %>%
        select(Name, roster_team = billikenTeam) %>%
        distinct()

      bad_team <- keeper_overrides_resolved %>%
        left_join(roster_team_map, by = "Name") %>%
        filter(is.na(roster_team) | roster_team != billikenTeam)

      if (nrow(bad_team) > 0) {
        msg <- paste0(
          "ForceKeeper row(s) refer to a player/team combo that doesn't match the post-trade rosters.\n",
          "Example: ", bad_team$player[[1]], " is on '", bad_team$roster_team[[1]], "' (not '", bad_team$billikenTeam[[1]], "')."
        )
        stop(msg, call. = FALSE)
      }

      conflicts <- keeper_overrides_resolved %>%
        group_by(Name, billikenTeam) %>%
        summarise(n_vals = n_distinct(force_keeper), .groups = "drop") %>%
        filter(n_vals > 1)

      if (nrow(conflicts) > 0) {
        stop("Conflicting ForceKeeper overrides found for at least one player/team.", call. = FALSE)
      }

      readr::write_csv(keeper_overrides_resolved, file.path(out_dir, "keeper_overrides_resolved.csv"))
    }
  } else {
    # Write an empty file for convenience/consistency.
    readr::write_csv(drop_penalties_by_team, file.path(out_dir, "salary_cap_penalties.csv"))
  }

  simulated_keepers_list <- list()

  for (team_name in names(keeper_limits)) {
    limit <- keeper_limits[[team_name]]

    force_keep_names <- keeper_overrides_resolved %>%
      filter(billikenTeam == team_name, force_keeper == 1L) %>%
      pull(Name) %>%
      unique()

    force_drop_names <- keeper_overrides_resolved %>%
      filter(billikenTeam == team_name, force_keeper == 0L) %>%
      pull(Name) %>%
      unique()

    both <- intersect(force_keep_names, force_drop_names)
    if (length(both) > 0) {
      stop(sprintf(
        "%s: ForceKeeper has conflicting keep/drop entries for: %s",
        team_name,
        paste(both, collapse = ", ")
      ), call. = FALSE)
    }

    team_pool <- projections_prefreeze %>%
      filter(billikenTeam == team_name)

    forced_keeps <- team_pool %>%
      filter(Name %in% force_keep_names)

    if (length(force_keep_names) > 0 && nrow(forced_keeps) != length(force_keep_names)) {
      missing <- setdiff(force_keep_names, forced_keeps$Name)
      stop(sprintf(
        "%s: ForceKeeper=1 specified for player(s) not found on that team's roster: %s",
        team_name,
        paste(missing, collapse = ", ")
      ), call. = FALSE)
    }

    if (nrow(forced_keeps) > limit) {
      stop(sprintf(
        "%s: %d forced keepers exceeds keeper limit (%d).",
        team_name,
        nrow(forced_keeps),
        limit
      ), call. = FALSE)
    }

    remaining_n <- limit - nrow(forced_keeps)

    ranked_keeps <- team_pool %>%
      filter(!Name %in% c(force_keep_names, force_drop_names)) %>%
      filter(!is.na(sgpar) & sgpar >= 0.0) %>%
      arrange(desc(sgpar_randomized), desc(sgpar)) %>%
      slice_head(n = remaining_n)

    team_keepers <- bind_rows(forced_keeps, ranked_keeps)

    simulated_keepers_list[[team_name]] <- team_keepers

    message(sprintf(
      "%s: keeping %d (limit %d; forced %d)",
      team_name,
      nrow(team_keepers),
      limit,
      nrow(forced_keeps)
    ))
  }

  simulated_keepers <- bind_rows(simulated_keepers_list)

  message("Writing simulated_keepers.csv...")
  readr::write_csv(simulated_keepers, file.path(out_dir, "simulated_keepers.csv"))

  message("Done!")

  invisible(list(
    projections_prefreeze = projections_prefreeze,
    simulated_keepers = simulated_keepers,
    drop_penalties_by_team = drop_penalties_by_team,
    keeper_overrides_resolved = keeper_overrides_resolved
  ))
}
