# scripts/draft_simulation_lib.R
# Draft simulation library (importable; no auto-run side effects)

suppressPackageStartupMessages({
  library(tidyverse)
})

# Optional path helper
if (file.exists("scripts/paths.R")) source("scripts/paths.R")
if (file.exists("paths.R")) source("paths.R")

.sim_root <- if (exists("find_project_root")) find_project_root() else getwd()

.resolve_path <- function(p) {
  if (is.null(p)) return(NULL)
  if (grepl("^/", p)) return(p)
  file.path(.sim_root, p)
}

# ============================================================================
# CONFIGURATION
# ============================================================================

# Salary cap per team
SALARY_CAP <- 270

get_salary_cap <- function(team_name, salary_cap_by_team = NULL) {
  if (is.null(salary_cap_by_team)) return(SALARY_CAP)

  # Accept either a named vector/list (per-team) or a scalar.
  if (length(salary_cap_by_team) == 1 && (is.null(names(salary_cap_by_team)) || names(salary_cap_by_team) == "")) {
    cap <- suppressWarnings(as.numeric(salary_cap_by_team))
    if (is.na(cap)) return(SALARY_CAP)
    return(cap)
  }

  if (is.null(names(salary_cap_by_team))) return(SALARY_CAP)

  nm <- toupper(names(salary_cap_by_team))
  idx <- match(toupper(as.character(team_name)), nm)
  if (is.na(idx)) return(SALARY_CAP)

  cap <- suppressWarnings(as.numeric(salary_cap_by_team[[idx]]))
  if (is.na(cap)) return(SALARY_CAP)
  cap
}

# Default salary for unpriced players
DEFAULT_SALARY <- 1

# Number of teams
N_TEAMS <- 10

# Roster structure (23 active slots per team)
SLOT_STRUCTURE <- c(
  C = 2, `1B` = 1, `2B` = 1, `3B` = 1, SS = 1,
  OF = 5, MI = 1, CI = 1, Util = 1, P = 9
)

# Replacement levels by position (for slot assignment priority - lower = scarcer)
REPLACEMENT_LEVELS <- tibble(
  pos = c("C", "1B", "2B", "3B", "SS", "OF", "CI", "MI", "Util", "P"),
  repl_level = c(1.2, 3.0, 3.2, 2.4, 3.6, 2.3, 2.6, 3.2, 3.7, 3.1)
)

# ============================================================================
# DATA LOADING / TEMPLATE BUILDING
# ============================================================================

load_salaries <- function(salaries_path = "data/raw/salaries_latest.csv") {
  salaries_path <- .resolve_path(salaries_path)

  if (!file.exists(salaries_path)) {
    return(tibble(Name = character(), salary = numeric()))
  }

  salaries <- readr::read_csv(salaries_path, show_col_types = FALSE)

  if (!"Player" %in% names(salaries) || !"Salary" %in% names(salaries)) {
    return(tibble(Name = character(), salary = numeric()))
  }

  salaries %>%
    filter(!is.na(Player)) %>%
    transmute(
      Name = as.character(Player),
      salary = as.numeric(gsub("\\$", "", as.character(Salary)))
    ) %>%
    mutate(
      # Treat <= 0 as missing; minimum auction price is $1.
      salary = ifelse(is.na(salary) | salary <= 0, NA_real_, salary)
    )
}

apply_fangraphs_salary_fallback <- function(player_pool, rosters_template, verbose = TRUE) {
  rostered_players <- rosters_template %>%
    filter(!is.na(player)) %>%
    pull(player) %>%
    unique()

  eligible_pool <- player_pool %>%
    filter(!Name %in% rostered_players)

  if (nrow(eligible_pool) == 0) return(player_pool)

  has_any_sheet_salaries <- eligible_pool %>%
    summarise(any_salary = any(!is.na(salary) & salary > 0), .groups = "drop") %>%
    pull(any_salary)

  if (isTRUE(has_any_sheet_salaries)) return(player_pool)

  if (!"fg_auction_dollars" %in% names(player_pool)) {
    if (verbose) message("No sheet salaries found for draft-eligible players, but fg_auction_dollars is missing; leaving salaries as default.")
    return(player_pool)
  }

  if (verbose) {
    message("No sheet salaries found for draft-eligible players; using FanGraphs auction values (fg_auction_dollars) as salary proxies.")
  }

  eligible_names <- eligible_pool$Name

  player_pool %>%
    mutate(
      salary = ifelse(
        Name %in% eligible_names,
        pmax(DEFAULT_SALARY, round(replace_na(fg_auction_dollars, DEFAULT_SALARY))),
        salary
      )
    )
}

#' Load projected player values and attach salary/contract defaults.
load_draft_pool <- function(
  projected_player_value_path = "data/processed/projected_player_value.csv",
  salaries_path = "data/raw/salaries_latest.csv"
) {
  projected_player_value_path <- .resolve_path(projected_player_value_path)
  player_value <- readr::read_csv(projected_player_value_path, show_col_types = FALSE)

  salaries <- load_salaries(salaries_path)

  pool <- player_value %>%
    left_join(salaries %>% rename(lookup_salary = salary), by = "Name") %>%
    mutate(
      billikenTeam = NA_character_,
      contract = "1",
      # Keep as NA when missing; defaulting happens at pick-time.
      salary = lookup_salary,
      # Add pitcher flag based on IP
      p_p = ifelse(!is.na(IP) & IP > 0, 1, 0),
      # Add util eligibility for all hitters
      p_util = ifelse(p_p == 0, 1, 0)
    ) %>%
    select(-lookup_salary)

  pool
}

#' Load draft order from the latest CSV extract.
load_draft_order <- function(draft_path = "data/raw/draft_latest.csv") {
  draft_path <- .resolve_path(draft_path)
  draft <- readr::read_csv(draft_path, show_col_types = FALSE)

  stopifnot(all(c("Round", "Pick", "Team") %in% names(draft)))

  # Standardize empty cells
  draft %>%
    transmute(
      Round = as.numeric(Round),
      Pick = as.numeric(Pick),
      billikenTeam = toupper(as.character(Team)),
      player = as.character(Player),
      salary = as.numeric(Salary)
    ) %>%
    mutate(
      player = ifelse(is.na(player) | player == "NA", NA_character_, player),
      salary = ifelse(is.na(salary), DEFAULT_SALARY, salary)
    ) %>%
    filter(!is.na(billikenTeam) & billikenTeam != "")
}

#' Load finalized keepers (data/raw/keepers.csv), if any.
#'
#' Returns a data frame compatible with fill_simulated_keepers():
#' Name, billikenTeam, salary, contract
load_final_keepers <- function(keepers_path = "data/raw/keepers.csv") {
  keepers_path <- .resolve_path(keepers_path)

  # Be tolerant of case differences (Keepers.csv vs keepers.csv)
  keepers_candidates <- unique(c(
    keepers_path,
    sub("keepers\\.csv$", "Keepers.csv", keepers_path),
    sub("Keepers\\.csv$", "keepers.csv", keepers_path)
  ))

  keepers_file <- keepers_candidates[file.exists(keepers_candidates)][1]
  if (is.na(keepers_file) || is.null(keepers_file) || keepers_file == "") {
    return(tibble())
  }

  k_raw <- readr::read_csv(keepers_file, show_col_types = FALSE)

  if (!all(c("Player", "billikenTeam") %in% names(k_raw))) {
    return(tibble())
  }

  # Salary / contract are optional depending on sheet state; default if missing
  if (!"Salary" %in% names(k_raw)) k_raw$Salary <- DEFAULT_SALARY
  if (!"Contract" %in% names(k_raw)) k_raw$Contract <- "1"

  k <- k_raw %>%
    transmute(
      Name = as.character(Player),
      billikenTeam = as.character(billikenTeam),
      contract = as.character(Contract),
      salary = as.numeric(Salary)
    ) %>%
    mutate(
      Name = ifelse(is.na(Name) | Name == "NA" | trimws(Name) == "", NA_character_, Name),
      billikenTeam = ifelse(
        is.na(billikenTeam) | billikenTeam == "NA" | trimws(billikenTeam) == "",
        NA_character_,
        billikenTeam
      ),
      salary = ifelse(is.na(salary), DEFAULT_SALARY, salary),
      contract = ifelse(is.na(contract) | contract == "NA" | trimws(contract) == "", "1", contract)
    ) %>%
    filter(!is.na(Name), !is.na(billikenTeam))

  k
}

#' Load simulated keepers (data/processed/simulated_keepers.csv), if present.
#'
#' Returns a data frame compatible with fill_simulated_keepers():
#' Name, billikenTeam, salary, contract
load_simulated_keepers_file <- function(simulated_keepers_path = "data/processed/simulated_keepers.csv") {
  simulated_keepers_path <- .resolve_path(simulated_keepers_path)

  if (!file.exists(simulated_keepers_path)) {
    return(tibble())
  }

  k_raw <- readr::read_csv(simulated_keepers_path, show_col_types = FALSE)

  if (!all(c("Name", "billikenTeam") %in% names(k_raw))) {
    return(tibble())
  }

  if (!"salary" %in% names(k_raw)) k_raw$salary <- DEFAULT_SALARY
  if (!"contract" %in% names(k_raw)) k_raw$contract <- "1"

  k_raw %>%
    transmute(
      Name = as.character(Name),
      billikenTeam = as.character(billikenTeam),
      contract = as.character(contract),
      salary = as.numeric(salary)
    ) %>%
    mutate(
      Name = ifelse(is.na(Name) | Name == "NA" | trimws(Name) == "", NA_character_, Name),
      billikenTeam = ifelse(
        is.na(billikenTeam) | billikenTeam == "NA" | trimws(billikenTeam) == "",
        NA_character_,
        billikenTeam
      ),
      salary = ifelse(is.na(salary), DEFAULT_SALARY, salary),
      contract = ifelse(is.na(contract) | contract == "NA" | trimws(contract) == "", "1", contract)
    ) %>%
    filter(!is.na(Name), !is.na(billikenTeam))
}

#' Choose default keepers: prefer finalized keepers.csv if it has any rows;
#' otherwise fall back to simulated_keepers.csv.
load_default_keepers <- function(
  keepers_path = "data/raw/keepers.csv",
  simulated_keepers_path = "data/processed/simulated_keepers.csv",
  verbose = TRUE
) {
  final_k <- load_final_keepers(keepers_path)

  if (nrow(final_k) > 0) {
    if (verbose) message(sprintf("Using finalized keepers from %s (%d keepers)", keepers_path, nrow(final_k)))
    return(final_k)
  }

  sim_k <- load_simulated_keepers_file(simulated_keepers_path)
  if (nrow(sim_k) > 0) {
    if (verbose) message(sprintf("No finalized keepers found; using simulated keepers from %s (%d keepers)", simulated_keepers_path, nrow(sim_k)))
    return(sim_k)
  }

  if (verbose) message("No keepers found (finalized or simulated); simulating from empty rosters.")
  tibble()
}

#' Build an empty roster skeleton (23 active slots per team).
build_empty_rosters <- function(teams, slot_structure = SLOT_STRUCTURE) {
  slot_names <- rep(names(slot_structure), slot_structure)

  map_dfr(teams, function(team) {
    tibble(
      team = team,
      pos = slot_names,
      player = NA_character_,
      contract = NA_character_,
      salary = NA_real_
    ) %>%
      group_by(team) %>%
      mutate(slot_id = row_number()) %>%
      ungroup()
  })
}

# ============================================================================
# ROSTER SLOT / POSITION HELPERS
# ============================================================================

get_player_positions <- function(player_row) {
  positions <- c()

  # Pitchers
  if (!is.null(player_row$p_p) && !is.na(player_row$p_p) && player_row$p_p == 1) return(c("P"))
  if (!is.null(player_row$p_sp) && !is.na(player_row$p_sp) && player_row$p_sp == 1) return(c("P"))
  if (!is.null(player_row$p_rp) && !is.na(player_row$p_rp) && player_row$p_rp == 1) return(c("P"))

  if (!is.null(player_row$p_c) && !is.na(player_row$p_c) && player_row$p_c == 1) positions <- c(positions, "C")
  if (!is.null(player_row$p_1b) && !is.na(player_row$p_1b) && player_row$p_1b == 1) positions <- c(positions, "1B", "CI")
  if (!is.null(player_row$p_2b) && !is.na(player_row$p_2b) && player_row$p_2b == 1) positions <- c(positions, "2B", "MI")
  if (!is.null(player_row$p_3b) && !is.na(player_row$p_3b) && player_row$p_3b == 1) positions <- c(positions, "3B", "CI")
  if (!is.null(player_row$p_ss) && !is.na(player_row$p_ss) && player_row$p_ss == 1) positions <- c(positions, "SS", "MI")
  if (!is.null(player_row$p_of) && !is.na(player_row$p_of) && player_row$p_of == 1) positions <- c(positions, "OF")
  if (!is.null(player_row$p_ci) && !is.na(player_row$p_ci) && player_row$p_ci == 1) positions <- c(positions, "CI")
  if (!is.null(player_row$p_mi) && !is.na(player_row$p_mi) && player_row$p_mi == 1) positions <- c(positions, "MI")
  if (!is.null(player_row$p_dh) && !is.na(player_row$p_dh) && player_row$p_dh == 1) positions <- c(positions, "Util")

  # All hitters can play utility
  if (length(positions) > 0) positions <- c(positions, "Util")

  unique(positions)
}

calculate_team_salary <- function(rosters, team_name) {
  rosters %>%
    filter(team == team_name, !is.na(player)) %>%
    summarise(s = sum(salary, na.rm = TRUE), .groups = "drop") %>%
    pull(s)
}

has_open_slots <- function(rosters, team_name) {
  rosters %>% filter(team == team_name, is.na(player)) %>% nrow() > 0
}

find_best_slot <- function(rosters, team_name, player_row) {
  team_roster <- rosters %>% filter(team == team_name)

  open_slots <- team_roster %>% filter(is.na(player))
  if (nrow(open_slots) == 0) return(NA_integer_)

  player_positions <- get_player_positions(player_row)
  if (length(player_positions) == 0) return(NA_integer_)

  eligible_slots <- open_slots %>%
    filter(pos %in% player_positions) %>%
    left_join(REPLACEMENT_LEVELS, by = "pos") %>%
    arrange(repl_level) %>%
    slice(1)

  if (nrow(eligible_slots) == 0) return(NA_integer_)
  eligible_slots$slot_id[1]
}

assign_player <- function(rosters, team_name, player_name, player_salary, player_contract, player_row) {
  slot_id <- find_best_slot(rosters, team_name, player_row)
  if (is.na(slot_id)) return(rosters)

  rosters %>%
    mutate(
      player = ifelse(team == team_name & slot_id == !!slot_id, player_name, player),
      salary = ifelse(team == team_name & slot_id == !!slot_id, player_salary, salary),
      contract = ifelse(team == team_name & slot_id == !!slot_id, player_contract, contract)
    )
}

# ============================================================================
# KEEPER / DRAFT FILL HELPERS
# ============================================================================

fill_simulated_keepers <- function(rosters, simulated_keepers, player_pool, force = FALSE) {
  if (nrow(simulated_keepers) == 0) return(rosters)

  filled_count <- rosters %>% filter(!is.na(player)) %>% nrow()
  if (!force && filled_count > 0) {
    return(rosters)
  }

  for (i in seq_len(nrow(simulated_keepers))) {
    player_name <- simulated_keepers$Name[i]
    team_name <- toupper(simulated_keepers$billikenTeam[i])
    player_salary <- simulated_keepers$salary[i]
    player_contract <- as.character(simulated_keepers$contract[i])

    if (is.na(team_name) || team_name == "") next

    player_row <- player_pool %>% filter(Name == player_name)
    if (nrow(player_row) == 0) next

    rosters <- assign_player(rosters, team_name, player_name, player_salary, player_contract, player_row)
  }

  rosters
}

apply_forced_picks <- function(draft_order, forced_picks) {
  if (is.null(forced_picks) || nrow(forced_picks) == 0) return(draft_order)

  for (i in seq_len(nrow(forced_picks))) {
    r <- forced_picks$Round[i]
    p <- forced_picks$Pick[i]
    player_name <- forced_picks$player[i]
    player_salary <- forced_picks$salary[i]

    idx <- which(draft_order$Round == r & draft_order$Pick == p)
    if (length(idx) == 0) {
      warning(sprintf("Forced pick: no slot found for round=%d pick=%d; skipping.", r, p))
      next
    }

    draft_order$player[idx[1]] <- player_name
    draft_order$salary[idx[1]] <- player_salary
  }

  draft_order
}

fill_existing_draft_picks <- function(rosters, draft_order, player_pool) {
  picked <- draft_order %>%
    filter(!is.na(player) & player != "" & player != "pass")

  if (nrow(picked) == 0) return(rosters)

  for (i in seq_len(nrow(picked))) {
    team_name <- picked$billikenTeam[i]
    player_name <- picked$player[i]
    player_salary <- picked$salary[i]

    player_row <- player_pool %>% filter(Name == player_name)
    if (nrow(player_row) == 0) next

    rosters <- assign_player(rosters, team_name, player_name, player_salary, "1", player_row)
  }

  rosters
}

# ============================================================================
# DRAFT SIMULATION
# ============================================================================

get_next_pick <- function(draft_order) {
  next_pick <- draft_order %>%
    filter(is.na(player) | player == "") %>%
    arrange(Round, Pick) %>%
    slice(1)

  if (nrow(next_pick) == 0) {
    return(list(team = NA, round = NA, pick = NA, idx = NA))
  }

  idx <- which(
    draft_order$Round == next_pick$Round[1] &
      draft_order$Pick == next_pick$Pick[1] &
      (is.na(draft_order$player) | draft_order$player == "")
  )[1]

  list(
    team = next_pick$billikenTeam[1],
    round = next_pick$Round[1],
    pick = next_pick$Pick[1],
    idx = idx
  )
}

get_available_players <- function(rosters, player_pool, team_name) {
  rostered_players <- rosters %>%
    filter(!is.na(player)) %>%
    pull(player) %>%
    unique()

  available <- player_pool %>%
    filter(is.na(billikenTeam)) %>%
    filter(!Name %in% rostered_players)

  open_slots <- rosters %>%
    filter(team == team_name, is.na(player)) %>%
    pull(pos) %>%
    unique()

  if (length(open_slots) == 0) return(tibble())

  eligible_players <- available %>%
    rowwise() %>%
    mutate(
      can_fill = {
        positions <- get_player_positions(pick(everything()))
        any(positions %in% open_slots)
      }
    ) %>%
    ungroup() %>%
    filter(can_fill) %>%
    select(-can_fill)

  eligible_players
}

make_pick <- function(rosters, draft_order, player_pool, randomness_pct = 0.10, salary_cap_by_team = NULL) {
  next_pick <- get_next_pick(draft_order)

  if (is.na(next_pick$team)) {
    return(list(rosters = rosters, draft_order = draft_order, picked = NA))
  }

  team_name <- next_pick$team

  if (!has_open_slots(rosters, team_name)) {
    draft_order$player[next_pick$idx] <- "pass"
    return(list(rosters = rosters, draft_order = draft_order, picked = "pass"))
  }

  eligible <- get_available_players(rosters, player_pool, team_name)
  if (nrow(eligible) == 0) {
    draft_order$player[next_pick$idx] <- "pass"
    return(list(rosters = rosters, draft_order = draft_order, picked = "pass"))
  }

  current_salary <- calculate_team_salary(rosters, team_name)
  remaining_cap <- get_salary_cap(team_name, salary_cap_by_team) - current_salary

  eligible <- eligible %>%
    mutate(player_salary = ifelse(is.na(salary), DEFAULT_SALARY, salary)) %>%
    filter(player_salary <= remaining_cap)

  if (nrow(eligible) == 0) {
    draft_order$player[next_pick$idx] <- "pass"
    return(list(rosters = rosters, draft_order = draft_order, picked = "pass"))
  }

  eligible <- eligible %>%
    mutate(
      rand_factor = runif(n(), min = 1 - randomness_pct, max = 1 + randomness_pct),
      rand_sgpar = sgpar * rand_factor
    ) %>%
    arrange(desc(rand_sgpar))

  selected_player <- eligible %>% slice(1)
  player_name <- selected_player$Name[1]
  player_salary <- ifelse(is.na(selected_player$salary[1]), DEFAULT_SALARY, selected_player$salary[1])
  player_contract <- ifelse(is.na(selected_player$contract[1]), "1", selected_player$contract[1])

  rosters <- assign_player(rosters, team_name, player_name, player_salary, player_contract, selected_player)

  draft_order$player[next_pick$idx] <- player_name
  draft_order$salary[next_pick$idx] <- player_salary

  list(rosters = rosters, draft_order = draft_order, picked = player_name)
}

simulate_draft <- function(rosters_template, draft_order_template, player_pool, randomness_pct = 0.10, salary_cap_by_team = NULL) {
  rosters <- rosters_template
  draft_order <- draft_order_template

  max_picks <- nrow(draft_order)
  picks_made <- 0

  while (picks_made < max_picks) {
    result <- make_pick(rosters, draft_order, player_pool, randomness_pct, salary_cap_by_team)
    rosters <- result$rosters
    draft_order <- result$draft_order

    if (is.na(result$picked)) break
    picks_made <- picks_made + 1
  }

  list(rosters = rosters, draft_order = draft_order)
}

# ============================================================================
# STANDINGS
# ============================================================================

calculate_standings <- function(rosters, player_pool, sim_num = 1) {
  rostered <- rosters %>%
    filter(!is.na(player)) %>%
    select(team, player, salary)

  roster_stats <- rostered %>%
    left_join(
      player_pool %>%
        select(Name, PA, AB, H, HR, R, RBI, SB, AVG, IP, W, SV, SO, ERA, WHIP, sgpar) %>%
        mutate(
          ER = ifelse(!is.na(IP) & IP > 0, ERA * IP / 9, NA_real_),
          WH = ifelse(!is.na(IP) & IP > 0, WHIP * IP, NA_real_)
        ),
      by = c("player" = "Name")
    )

  team_totals <- roster_stats %>%
    group_by(team) %>%
    summarise(
      n = n(),
      total_salary = sum(salary, na.rm = TRUE),
      sgpar = sum(sgpar, na.rm = TRUE),
      PA = sum(PA, na.rm = TRUE),
      AB = sum(AB, na.rm = TRUE),
      H = sum(H, na.rm = TRUE),
      HR = sum(HR, na.rm = TRUE),
      R = sum(R, na.rm = TRUE),
      RBI = sum(RBI, na.rm = TRUE),
      SB = sum(SB, na.rm = TRUE),
      AVG = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
      IP = sum(IP, na.rm = TRUE),
      W = sum(W, na.rm = TRUE),
      SV = sum(SV, na.rm = TRUE),
      SO = sum(SO, na.rm = TRUE),
      ER = sum(ER, na.rm = TRUE),
      WH = sum(WH, na.rm = TRUE),
      ERA = sum(ER, na.rm = TRUE) * 9 / sum(IP, na.rm = TRUE),
      WHIP = sum(WH, na.rm = TRUE) / sum(IP, na.rm = TRUE),
      .groups = "drop"
    )

  standings <- team_totals %>%
    mutate(
      hr_pts = N_TEAMS + 1 - dense_rank(desc(HR)),
      r_pts = N_TEAMS + 1 - dense_rank(desc(R)),
      rbi_pts = N_TEAMS + 1 - dense_rank(desc(RBI)),
      sb_pts = N_TEAMS + 1 - dense_rank(desc(SB)),
      avg_pts = N_TEAMS + 1 - dense_rank(desc(AVG)),
      w_pts = N_TEAMS + 1 - dense_rank(desc(W)),
      sv_pts = N_TEAMS + 1 - dense_rank(desc(SV)),
      so_pts = N_TEAMS + 1 - dense_rank(desc(SO)),
      era_pts = N_TEAMS + 1 - dense_rank(ERA),
      whip_pts = N_TEAMS + 1 - dense_rank(WHIP)
    ) %>%
    mutate(
      hit_pts = hr_pts + r_pts + rbi_pts + sb_pts + avg_pts,
      pit_pts = w_pts + sv_pts + so_pts + era_pts + whip_pts,
      total_pts = hit_pts + pit_pts,
      rank = dense_rank(desc(total_pts)),
      sim_num = sim_num
    ) %>%
    arrange(desc(total_pts))

  standings
}

# ============================================================================
# PUBLIC ENTRYPOINTS
# ============================================================================

run_simulations <- function(
  n_sims = 20,
  randomness_pct = 0.10,
  simulated_keepers = NULL,
  seed = NULL,
  verbose = TRUE,
  projected_player_value_path = "data/processed/projected_player_value.csv",
  salaries_path = "data/raw/salaries_latest.csv",
  draft_path = "data/raw/draft_latest.csv",
  salary_cap_by_team = NULL,
  # Default keeper selection:
  # 1) If data/raw/keepers.csv has any keepers, use that.
  # 2) Otherwise use data/processed/simulated_keepers.csv, if present.
  keepers_path = "data/raw/keepers.csv",
  simulated_keepers_path = "data/processed/simulated_keepers.csv",
  use_default_keepers = TRUE,
  force_simulated_keepers = FALSE,
  forced_picks = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  if (verbose) message("Loading data...")
  player_pool <- load_draft_pool(projected_player_value_path, salaries_path)
  draft_order_template <- load_draft_order(draft_path)

  teams <- draft_order_template %>% distinct(billikenTeam) %>% pull(billikenTeam) %>% sort()
  if (length(teams) != N_TEAMS) {
    warning(sprintf("Expected %d teams, found %d in draft file.", N_TEAMS, length(teams)))
  }

  rosters_template <- build_empty_rosters(teams)
  rosters_template <- fill_existing_draft_picks(rosters_template, draft_order_template, player_pool)

  if (is.null(simulated_keepers) && isTRUE(use_default_keepers)) {
    simulated_keepers <- load_default_keepers(
      keepers_path = keepers_path,
      simulated_keepers_path = simulated_keepers_path,
      verbose = verbose
    )
  }

  if (!is.null(simulated_keepers)) {
    rosters_template <- fill_simulated_keepers(
      rosters_template,
      simulated_keepers,
      player_pool,
      force = force_simulated_keepers
    )
  }

  # Apply forced picks AFTER keepers are loaded so they don't trip the
  # filled_count > 0 guard in fill_simulated_keepers.
  if (!is.null(forced_picks) && nrow(forced_picks) > 0) {
    draft_order_template <- apply_forced_picks(draft_order_template, forced_picks)
    for (i in seq_len(nrow(forced_picks))) {
      fp_round <- forced_picks$Round[i]
      fp_pick  <- forced_picks$Pick[i]
      fp_name  <- forced_picks$player[i]
      fp_sal   <- forced_picks$salary[i]
      idx <- which(draft_order_template$Round == fp_round & draft_order_template$Pick == fp_pick)
      if (length(idx) == 0) next
      fp_team <- draft_order_template$billikenTeam[idx[1]]
      fp_row  <- player_pool %>% filter(Name == fp_name)
      if (nrow(fp_row) == 0) next
      rosters_template <- assign_player(rosters_template, fp_team, fp_name, fp_sal, "1", fp_row)
    }
  }

  # If the salaries sheet doesn't have any usable salaries for draft-eligible players,
  # treat that as "not available yet" and fall back to FanGraphs auction values.
  player_pool <- apply_fangraphs_salary_fallback(player_pool, rosters_template, verbose = verbose)

  all_standings <- tibble()

  for (i in seq_len(n_sims)) {
    if (verbose) message(sprintf("Running simulation %d/%d...", i, n_sims))

    suppressWarnings({
      result <- simulate_draft(rosters_template, draft_order_template, player_pool, randomness_pct, salary_cap_by_team)
      standings <- calculate_standings(result$rosters, player_pool, sim_num = i)
    })

    all_standings <- bind_rows(all_standings, standings)
  }

  all_standings
}

summarize_simulations <- function(all_standings) {
  all_standings %>%
    group_by(team) %>%
    summarise(
      n_sims = n(),
      wins = sum(rank == 1),
      top_3 = sum(rank <= 3),
      avg_rank = mean(rank),
      avg_pts = mean(total_pts),
      avg_sgpar = mean(sgpar),
      avg_hit_pts = mean(hit_pts),
      avg_pit_pts = mean(pit_pts),
      min_pts = min(total_pts),
      max_pts = max(total_pts),
      .groups = "drop"
    ) %>%
    arrange(avg_rank)
}
