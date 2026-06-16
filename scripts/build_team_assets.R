# build_team_assets.R
#
# Phases 1 + 2 of the trade-analysis tooling. Produces a single canonical
# table `data/processed/team_assets.csv` that has one row per rostered
# Billiken player, joining:
#
#   * Current ESPN rosters (data/raw/espn_rosters_latest.csv)
#   * Keeper contract codes / salaries (data/raw/keepers.csv)
#   * Drafted-player salaries as a fallback for non-keepers
#     (data/raw/draft_latest.csv) and a last-resort lookup against
#     data/raw/salaries_latest.csv
#   * Full-season player value (data/processed/projected_player_value.csv)
#     for sgpar / standings_value / fg_auction_dollars
#   * In-season ROS player detail (data/processed/inseason_team_details.csv)
#     for ROS counting stats (sgp_total + roster_status if present)
#   * Position eligibility (data/raw/positions_latest.csv)
#   * Player birthdates (data/processed/player_birthdates.csv) for the
#     aging curve. Lazily refreshed by scripts/fetch_player_birthdates.R
#     when a cache miss is detected.
#
# Phase 2 adds multi-year value: per-year sgpar/dollar/salary/surplus
# through the player's contract_end, plus discounted aggregates
# (win_now_value, future_value, total_value) using gamma = 0.7. Salary
# path follows the constitution: flat in years 1-2, free same-salary
# keep in the opt year, +$5/yr per added year past opt.
#
# This file is the single source of truth for every later phase of the
# trade-analysis tooling.
#
# Run:
#   Rscript scripts/build_team_assets.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringi)
  library(fuzzyjoin)
})

source("scripts/prospect_value_utils.R")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

CURRENT_YEAR <- as.integer(Sys.getenv("BILLIKEN_PROJECTIONS_YEAR",
                                      unset = format(Sys.Date(), "%Y")))

normalize_name <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("\u00A0", " ") %>%
    str_replace_all("[.]", "") %>%
    str_squish() %>%
    str_to_lower()
}

strip_suffixes <- function(x) {
  x %>% str_replace_all(",|\\s+(jr|sr|ii|iii|iv|v)\\.?$", "")
}

normalize_team <- function(x) {
  if (length(x) == 0) return(character(0))
  x %>%
    as.character() %>%
    str_squish() %>%
    str_to_upper()
}

# Active position slot codes used by ESPN that imply the player is in a
# starting lineup spot (vs bench / IL / minors).
ACTIVE_SLOTS <- c(
  "C", "1B", "2B", "3B", "SS", "OF", "LF", "CF", "RF", "DH",
  "CI", "MI", "UTIL", "P", "SP", "RP"
)

slot_to_status <- function(slot) {
  s <- toupper(str_squish(as.character(slot)))
  case_when(
    s %in% c("BE", "BENCH")                  ~ "bench",
    s %in% c("IL")                           ~ "IL",
    s %in% c("MIN", "MINORS", "MINOR")       ~ "minors",
    s %in% ACTIVE_SLOTS                      ~ "active",
    TRUE                                     ~ "other"
  )
}

# Convert a keepers.csv `Contract` cell into normalized fields.
#
# `contract_end` is the last calendar year the player can be retained on
# the roster without re-entering the auction, assuming the GM exercises
# every keep option at the existing salary. The opt-year keep is treated
# as exercised (it is free — same salary, one more year), so a year2
# player has contract_end = current_year + 1 (the opt year), and a year1
# player has contract_end = current_year + 2 (year 2 + opt year).
# Extending past the opt year costs +$5/yr and is modeled in Phase 2.
parse_contract <- function(contract_code) {
  code <- str_squish(as.character(contract_code))
  code_lower <- str_to_lower(code)

  status <- case_when(
    is.na(code) | code == "" | code_lower %in% c("na", "null") ~ NA_character_,
    code_lower == "1"                                          ~ "year1",
    code_lower == "2"                                          ~ "year2",
    code_lower == "opt"                                        ~ "opt",
    grepl("^[0-9]{4}$", code)                                  ~ "extended",
    TRUE                                                       ~ NA_character_
  )

  end <- case_when(
    status == "year1"    ~ as.integer(CURRENT_YEAR + 2L),  # year2 + opt
    status == "year2"    ~ as.integer(CURRENT_YEAR + 1L),  # opt year
    status == "opt"      ~ as.integer(CURRENT_YEAR),       # FA after this year
    status == "extended" ~ suppressWarnings(as.integer(code)),
    TRUE                 ~ NA_integer_
  )

  tibble(contract_status = status, contract_end = end)
}

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

repo_root <- if (file.exists("billiken.Rproj")) {
  getwd()
} else if (file.exists("../billiken.Rproj")) {
  normalizePath("..")
} else {
  getwd()
}

resolve_path <- function(p) file.path(repo_root, p)

message(sprintf("Building team_assets.csv for %d (root: %s)",
                CURRENT_YEAR, repo_root))

espn_rosters <- read_csv(resolve_path("data/raw/espn_rosters_latest.csv"),
                         show_col_types = FALSE)

keepers_raw <- read_csv(resolve_path("data/raw/keepers.csv"),
                        show_col_types = FALSE)

draft_raw <- read_csv(resolve_path("data/raw/draft_latest.csv"),
                      show_col_types = FALSE)

salaries_raw <- read_csv(resolve_path("data/raw/salaries_latest.csv"),
                         show_col_types = FALSE)

ppv <- read_csv(resolve_path("data/processed/projected_player_value.csv"),
                show_col_types = FALSE)

positions_raw <- read_csv(resolve_path("data/raw/positions_latest.csv"),
                          show_col_types = FALSE)

ros_path <- resolve_path("data/processed/inseason_team_details.csv")
ros_detail <- if (file.exists(ros_path)) {
  read_csv(ros_path, show_col_types = FALSE)
} else {
  tibble()
}

auction_ros_path <- resolve_path(file.path(
  "data/raw",
  paste0("auction_values_ros_", CURRENT_YEAR, ".csv")
))
auction_ros_raw <- if (file.exists(auction_ros_path)) {
  read_csv(auction_ros_path, show_col_types = FALSE)
} else {
  tibble()
}

prospect_values_path <- resolve_path("data/processed/prospect_values.csv")
prospect_values_raw <- if (file.exists(prospect_values_path)) {
  read_csv(prospect_values_path, show_col_types = FALSE)
} else {
  tibble()
}

# ---------------------------------------------------------------------------
# Roster skeleton: one row per rostered player
# ---------------------------------------------------------------------------

rosters <- espn_rosters %>%
  transmute(
    billikenTeam = normalize_team(team_name),
    Name         = str_squish(player_name),
    espn_player_id = player_id,
    lineup_slot  = lineup_slot,
    roster_status = slot_to_status(lineup_slot),
    key_keep   = normalize_name(Name),
    key_strip  = normalize_name(strip_suffixes(Name))
  )

message(sprintf("ESPN rosters: %d players across %d teams",
                nrow(rosters),
                n_distinct(rosters$billikenTeam)))

# ---------------------------------------------------------------------------
# Contract / salary lookup
# ---------------------------------------------------------------------------

# Keepers carry the canonical contract code + salary.
keepers <- keepers_raw %>%
  filter(!is.na(Player), Player != "NA") %>%
  transmute(
    Name = str_squish(as.character(Player)),
    contract_code = str_squish(as.character(Contract)),
    salary_keeper = suppressWarnings(as.numeric(Salary)),
    keeper_team   = normalize_team(billikenTeam),
    key_keep   = normalize_name(Name),
    key_strip  = normalize_name(strip_suffixes(Name))
  )

# Drafted players default to year 1 contracts at their draft salary.
drafted <- draft_raw %>%
  filter(!is.na(Player), Player != "NA") %>%
  transmute(
    Name = str_squish(as.character(Player)),
    contract_code_draft = "1",
    salary_draft = suppressWarnings(as.numeric(Salary)),
    draft_team   = normalize_team(Team),
    key_keep   = normalize_name(Name),
    key_strip  = normalize_name(strip_suffixes(Name))
  )

# Last-resort salary lookup (e.g. for FAAB pickups whose contract is not
# yet recorded; treat as year 1 default).
sal_lookup <- salaries_raw %>%
  rename_with(~ "billikenTeam", any_of(c("Billiken Team", "billikenTeam"))) %>%
  filter(!is.na(Player), Player != "NA") %>%
  transmute(
    Name = str_squish(as.character(Player)),
    salary_lookup = suppressWarnings(as.numeric(Salary)),
    key_keep   = normalize_name(Name),
    key_strip  = normalize_name(strip_suffixes(Name))
  )

# Helper: left-join `lookup` onto `df` matching first by exact normalized
# name, then by suffix-stripped normalized name, and finally by a
# distance-1 fuzzy match on the normalized name (catches sheet typos like
# "Christoper Morel" vs "Christopher Morel"). `cols` is a character
# vector of columns to bring across from `lookup`.
join_by_normalized_name <- function(df, lookup, cols, fuzzy_max_dist = 1L) {
  exact <- lookup %>%
    distinct(key_keep, .keep_all = TRUE) %>%
    select(key_keep, all_of(cols)) %>%
    rename_with(~ paste0(.x, "_e"), all_of(cols))

  stripped <- lookup %>%
    distinct(key_strip, .keep_all = TRUE) %>%
    select(key_strip, all_of(cols)) %>%
    rename_with(~ paste0(.x, "_s"), all_of(cols))

  out <- df %>%
    left_join(exact, by = "key_keep") %>%
    left_join(stripped, by = "key_strip")

  for (col in cols) {
    e <- paste0(col, "_e")
    s <- paste0(col, "_s")
    out[[col]] <- coalesce(out[[e]], out[[s]])
    out[[e]] <- NULL
    out[[s]] <- NULL
  }

  if (fuzzy_max_dist > 0) {
    needs_fuzzy <- rowSums(!is.na(out[, cols, drop = FALSE])) == 0
    if (any(needs_fuzzy)) {
      fuzzy_lookup <- lookup %>%
        distinct(key_keep, .keep_all = TRUE) %>%
        select(key_keep, all_of(cols)) %>%
        rename_with(~ paste0(.x, "_f"), all_of(cols))

      fuzzy_in <- out[needs_fuzzy, , drop = FALSE]
      fuzzy_join <- fuzzy_in %>%
        mutate(.row_id = row_number()) %>%
        stringdist_left_join(
          fuzzy_lookup,
          by = c("key_keep" = "key_keep"),
          max_dist = fuzzy_max_dist,
          distance_col = ".dist"
        ) %>%
        group_by(.row_id) %>%
        slice_min(.dist, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(.row_id, ends_with("_f"))

      idx <- which(needs_fuzzy)
      for (col in cols) {
        f <- paste0(col, "_f")
        out[[col]][idx] <- coalesce(out[[col]][idx], fuzzy_join[[f]])
      }
    }
  }

  out
}

assets <- rosters %>%
  join_by_normalized_name(keepers,
                          cols = c("contract_code", "salary_keeper", "keeper_team")) %>%
  join_by_normalized_name(drafted,
                          cols = c("contract_code_draft", "salary_draft", "draft_team")) %>%
  join_by_normalized_name(sal_lookup, cols = c("salary_lookup")) %>%
  mutate(
    # Prefer keeper info; fall back to draft (year 1); then sheet salaries.
    contract_code = coalesce(contract_code, contract_code_draft, "1"),
    salary_2026 = coalesce(salary_keeper, salary_draft, salary_lookup, 1),
    contract_source = case_when(
      !is.na(salary_keeper) ~ "keeper",
      !is.na(salary_draft)  ~ "draft",
      !is.na(salary_lookup) ~ "salaries_sheet",
      TRUE                  ~ "default"
    )
  ) %>%
  select(-contract_code_draft, -salary_keeper, -salary_draft, -salary_lookup,
         -keeper_team, -draft_team)

# Parse the contract code into status + end-year fields.
parsed <- bind_rows(lapply(assets$contract_code, parse_contract))
assets <- bind_cols(assets, parsed) %>%
  mutate(
    years_remaining = pmax(0L, as.integer(contract_end) - CURRENT_YEAR),
    # A player is "expiring after the current year" only if there is no
    # remaining keeper option to extend them. year1 / year2 players still
    # have the upcoming opt-year decision available, so they are NOT
    # expiring. Truly expiring players are:
    #   * contract_status == "opt"          (in their option year already)
    #   * contract_status == "extended" AND contract_end == CURRENT_YEAR
    #     (extended through this year only)
    is_expiring_after_2026 = case_when(
      contract_status == "opt"                                       ~ TRUE,
      contract_status == "extended" & contract_end == CURRENT_YEAR   ~ TRUE,
      TRUE                                                           ~ FALSE
    ),
    # Encode what decision the GM faces this offseason. Useful on the
    # dashboard for picking out which kept players are at a contract
    # inflection point.
    next_offseason_decision = case_when(
      contract_status == "year1"                                       ~ "auto-rolls-to-year2",
      contract_status == "year2"                                       ~ "opt-year-decision",
      contract_status == "opt"                                         ~ "expires-or-extend",
      contract_status == "extended" & contract_end == CURRENT_YEAR     ~ "expires-or-extend",
      contract_status == "extended" & contract_end >  CURRENT_YEAR     ~ "none",
      TRUE                                                             ~ NA_character_
    )
  )

# ---------------------------------------------------------------------------
# Player value: sgpar / dollars / fg auction (full-season projection)
# ---------------------------------------------------------------------------

ppv_lookup <- ppv %>%
  filter(!is.na(Name)) %>%
  transmute(
    Name = str_squish(as.character(Name)),
    player_type        = player_type,
    fg_auction_dollars = suppressWarnings(as.numeric(fg_auction_dollars)),
    sgpar_full_2026    = suppressWarnings(as.numeric(sgpar)),
    dollar_value_2026  = suppressWarnings(as.numeric(standings_value)),
    key_keep   = normalize_name(Name),
    key_strip  = normalize_name(strip_suffixes(Name))
  )

assets <- assets %>%
  join_by_normalized_name(
    ppv_lookup,
    cols = c("player_type", "fg_auction_dollars",
             "sgpar_full_2026", "dollar_value_2026")
  )

if (nrow(auction_ros_raw) > 0) {
  ros_auction_lookup <- auction_ros_raw %>%
    filter(!is.na(PlayerName), PlayerName != "") %>%
    transmute(
      Name = str_squish(as.character(PlayerName)),
      fg_ros_auction_dollars = suppressWarnings(as.numeric(fg_auction_dollars)),
      key_keep = normalize_name(Name),
      key_strip = normalize_name(strip_suffixes(Name))
    )

  assets <- assets %>%
    join_by_normalized_name(
      ros_auction_lookup,
      cols = c("fg_ros_auction_dollars")
    )
} else {
  assets <- assets %>%
    mutate(fg_ros_auction_dollars = NA_real_)
}

# ---------------------------------------------------------------------------
# Optional: ROS detail
# ---------------------------------------------------------------------------

if (nrow(ros_detail) > 0) {
  ros_lookup <- ros_detail %>%
    transmute(
      Name = str_squish(as.character(player_name)),
      ros_player_type = if ("player_type" %in% names(.)) player_type else NA_character_,
      ros_sgp_2026  = if ("sgp_total" %in% names(.)) suppressWarnings(as.numeric(sgp_total)) else NA_real_,
      ros_R   = if ("R" %in% names(.))   suppressWarnings(as.numeric(R))   else NA_real_,
      ros_HR  = if ("HR" %in% names(.))  suppressWarnings(as.numeric(HR))  else NA_real_,
      ros_RBI = if ("RBI" %in% names(.)) suppressWarnings(as.numeric(RBI)) else NA_real_,
      ros_SB  = if ("SB" %in% names(.))  suppressWarnings(as.numeric(SB))  else NA_real_,
      ros_W   = if ("W" %in% names(.))   suppressWarnings(as.numeric(W))   else NA_real_,
      ros_SV  = if ("SV" %in% names(.))  suppressWarnings(as.numeric(SV))  else NA_real_,
      ros_SO  = if ("SO" %in% names(.))  suppressWarnings(as.numeric(SO))  else NA_real_,
      key_keep  = normalize_name(Name),
      key_strip = normalize_name(strip_suffixes(Name))
    )

  assets <- assets %>%
    join_by_normalized_name(
      ros_lookup,
      cols = c("ros_player_type", "ros_sgp_2026",
               "ros_R", "ros_HR", "ros_RBI", "ros_SB",
               "ros_W", "ros_SV", "ros_SO")
    ) %>%
    mutate(player_type = coalesce(player_type, ros_player_type)) %>%
    select(-ros_player_type)
} else {
  assets <- assets %>%
    mutate(
      ros_sgp_2026 = NA_real_,
      ros_R = NA_real_, ros_HR = NA_real_, ros_RBI = NA_real_, ros_SB = NA_real_,
      ros_W = NA_real_, ros_SV = NA_real_, ros_SO = NA_real_
    )
}

assets <- assets %>%
  mutate(
    dashboard_value_2026 = coalesce(
      fg_ros_auction_dollars,
      fg_auction_dollars,
      dollar_value_2026
    ),
    dashboard_value_source = case_when(
      !is.na(fg_ros_auction_dollars) ~ "fangraphs_ros_auction",
      !is.na(fg_auction_dollars) ~ "fangraphs_fullseason_auction",
      !is.na(dollar_value_2026) ~ "sgpar_standings_value",
      TRUE ~ NA_character_
    )
  )

# ---------------------------------------------------------------------------
# Positions: pipe-separated eligibility string
# ---------------------------------------------------------------------------

pos_long <- positions_raw %>%
  pivot_longer(-PLAYER, names_to = "position", values_to = "eligible") %>%
  filter(!is.na(eligible) & eligible == 1) %>%
  group_by(PLAYER) %>%
  summarise(positions = paste(position, collapse = "|"), .groups = "drop") %>%
  transmute(
    Name = str_squish(as.character(PLAYER)),
    positions,
    key_keep  = normalize_name(Name),
    key_strip = normalize_name(strip_suffixes(Name))
  )

assets <- assets %>%
  join_by_normalized_name(pos_long, cols = c("positions"))

# ---------------------------------------------------------------------------
# Prospect value overlay
# ---------------------------------------------------------------------------

if (nrow(prospect_values_raw) > 0) {
  prospect_lookup <- prospect_values_raw %>%
    transmute(
      Name = str_squish(as.character(Name)),
      prospect_value = suppressWarnings(as.numeric(prospect_value)),
      prospect_value_2027 = suppressWarnings(as.numeric(prospect_value_2027)),
      prospect_value_2028 = suppressWarnings(as.numeric(prospect_value_2028)),
      prospect_value_2029 = suppressWarnings(as.numeric(prospect_value_2029)),
      consensus_rank = suppressWarnings(as.numeric(consensus_rank)),
      prospect_eta = suppressWarnings(as.integer(eta)),
      prospect_value_source = as.character(prospect_value_source),
      future_projection_source = as.character(future_projection_source),
      key_keep = normalize_name(Name),
      key_strip = normalize_name(strip_suffixes(Name))
    )

  assets <- assets %>%
    join_by_normalized_name(
      prospect_lookup,
      cols = c(
        "prospect_value", "prospect_value_2027", "prospect_value_2028",
        "prospect_value_2029", "consensus_rank", "prospect_eta",
        "prospect_value_source", "future_projection_source"
      )
    )
} else {
  assets <- assets %>%
    mutate(
      prospect_value = NA_real_,
      prospect_value_2027 = NA_real_,
      prospect_value_2028 = NA_real_,
      prospect_value_2029 = NA_real_,
      consensus_rank = NA_real_,
      prospect_eta = NA_integer_,
      prospect_value_source = NA_character_,
      future_projection_source = NA_character_
    )
}

# ---------------------------------------------------------------------------
# Phase 2 — multi-year value
#
#   * Birthdate → age_2026 via the lazy MLB Stats API cache. We refresh
#     the cache here so first-time runs auto-populate it.
#   * Aging curve: sgpar / dollar_value decay 0%/yr through age 30, 5%/yr
#     for ages 31–33, 10%/yr for age 34+. Same shape for hitters and
#     pitchers in v1.
#   * Salary path: flat in years 1–2, free same-salary keep in the opt
#     year, +$5/yr per added year past opt. Beyond contract_end the
#     player re-enters the auction (surplus_y = 0).
#   * Discount factor gamma = 0.7 (configurable below).
#   * Aggregates: win_now_value (= surplus_2026), future_value (sum of
#     surplus_y * gamma^(y - 2026) for y > 2026 through contract_end),
#     and total_value (= win_now + future).
# ---------------------------------------------------------------------------

GAMMA   <- 0.7
HORIZON <- 4L  # cap at 4 future years (2027–2030 in 2026)

# Refresh / load the birthdate cache. fetch_player_birthdates.R is
# idempotent and tolerant of API failures, so this is safe to call on
# every build.
local({
  birthdates_env <- new.env(parent = globalenv())
  source(resolve_path("scripts/fetch_player_birthdates.R"),
         local = birthdates_env)
  tryCatch(
    birthdates_env$build_player_birthdates_cache(verbose = FALSE),
    error = function(e) {
      message("WARNING: birthdate refresh failed: ", e$message,
              " (continuing with existing cache, if any)")
    }
  )
})

birthdates_path <- resolve_path("data/processed/player_birthdates.csv")
birthdates_lookup <- if (file.exists(birthdates_path)) {
  read_csv(birthdates_path, show_col_types = FALSE) %>%
    filter(!is.na(birth_year)) %>%
    transmute(
      Name        = str_squish(as.character(Name)),
      birth_year  = as.integer(birth_year),
      key_keep    = normalize_name(Name),
      key_strip   = normalize_name(strip_suffixes(Name))
    )
} else {
  tibble(Name = character(0), birth_year = integer(0),
         key_keep = character(0), key_strip = character(0))
}

assets <- assets %>%
  join_by_normalized_name(birthdates_lookup, cols = c("birth_year")) %>%
  mutate(
    age_2026 = ifelse(!is.na(birth_year), CURRENT_YEAR - birth_year, NA_integer_)
  )

# Aging multiplier from CURRENT_YEAR to year `y` for a player who is
# `age_now` years old in CURRENT_YEAR. Returns 1 for y == CURRENT_YEAR
# and a compounded decay factor for y > CURRENT_YEAR.
aging_factor <- function(age_now, y) {
  if (is.na(age_now)) return(1)  # unknown age → hold flat (best guess)
  delta <- y - CURRENT_YEAR
  if (delta <= 0) return(1)
  factor <- 1
  for (i in seq_len(delta)) {
    age_at_year <- age_now + i
    decay <- if (age_at_year <= 30) 0
             else if (age_at_year <= 33) 0.05
             else 0.10
    factor <- factor * (1 - decay)
  }
  factor
}

# Salary in year y for a player whose 2026 salary, contract_status, and
# contract_end are known. Returns NA past contract_end (player would have
# re-entered the auction).
salary_in_year <- function(salary_2026, status, contract_end, y) {
  if (is.na(y) || is.na(salary_2026) || is.na(status)) return(NA_real_)
  if (y == CURRENT_YEAR) return(salary_2026)
  if (is.na(contract_end) || y > contract_end) return(NA_real_)
  # Within the keepable window the salary stays flat (years 1, 2, opt are
  # all the same salary as the original contract). Extensions past the
  # opt year would already have rolled `contract_status` to "extended"
  # with the salary frozen at the extended rate, so we don't add the
  # +$5/yr again here.
  salary_2026
}

# Build per-player multi-year columns. We loop over years 2026..2026+HORIZON
# and emit sgpar_y, dollar_value_y, salary_y, surplus_y. Any year past
# `contract_end` produces surplus_y = 0 (asset value of an expired
# contract is whatever the auction would price the player at, which is
# zero from the keeper-cost perspective).
year_offsets <- 0:HORIZON
years        <- CURRENT_YEAR + year_offsets

multi_year <- assets %>%
  rowwise() %>%
  mutate(
    .factor_list = list(vapply(years, function(y) aging_factor(age_2026, y), numeric(1))),
    .salary_list = list(vapply(years, function(y)
      salary_in_year(salary_2026, contract_status, contract_end, y),
      numeric(1)))
  ) %>%
  ungroup() %>%
  mutate(
    sgpar_2026          = sgpar_full_2026,
    sgpar_2027          = sgpar_full_2026 * map_dbl(.factor_list, 2),
    sgpar_2028          = sgpar_full_2026 * map_dbl(.factor_list, 3),
    sgpar_2029          = sgpar_full_2026 * map_dbl(.factor_list, 4),
    sgpar_2030          = sgpar_full_2026 * map_dbl(.factor_list, 5),
    dollar_value_2027   = dollar_value_2026 * map_dbl(.factor_list, 2),
    dollar_value_2028   = dollar_value_2026 * map_dbl(.factor_list, 3),
    dollar_value_2029   = dollar_value_2026 * map_dbl(.factor_list, 4),
    dollar_value_2030   = dollar_value_2026 * map_dbl(.factor_list, 5),
    salary_2027         = map_dbl(.salary_list, 2),
    salary_2028         = map_dbl(.salary_list, 3),
    salary_2029         = map_dbl(.salary_list, 4),
    salary_2030         = map_dbl(.salary_list, 5),
    surplus_2027        = ifelse(is.na(salary_2027), 0, dollar_value_2027 - salary_2027),
    surplus_2028        = ifelse(is.na(salary_2028), 0, dollar_value_2028 - salary_2028),
    surplus_2029        = ifelse(is.na(salary_2029), 0, dollar_value_2029 - salary_2029),
    surplus_2030        = ifelse(is.na(salary_2030), 0, dollar_value_2030 - salary_2030)
  ) %>%
  select(-.factor_list, -.salary_list)

assets <- multi_year %>%
  mutate(
    # Legacy SGP-surplus column kept for back-compat and side-by-side comparison.
    win_now_surplus_sgp = coalesce(dollar_value_2026 - salary_2026, 0),
    # In-season trade math uses FanGraphs ROS auction dollars when available.
    # Salary is intentionally NOT subtracted: in-season the current-year salary
    # is sunk for the keeper who paid it in March and is not transferred mid-
    # season, so the receiving team's win-now gain is the raw rest-of-season
    # auction value of the player.
    #
    # For the ~30% of rostered players FanGraphs has no ROS auction price for
    # (deep bench / minors / Triple-A callups), fall back to the legacy SGP
    # surplus so the column is still populated.
    win_now_value = coalesce(fg_ros_auction_dollars, win_now_surplus_sgp, 0),
    drop_penalty_liability = drop_penalty_liability(
      contract_status,
      contract_end,
      current_year = CURRENT_YEAR
    ),
    future_value  = calculate_future_asset_value(
      surplus_2027 = surplus_2027,
      surplus_2028 = surplus_2028,
      surplus_2029 = surplus_2029,
      surplus_2030 = surplus_2030,
      prospect_value = prospect_value,
      drop_penalty_liability = drop_penalty_liability,
      gamma = GAMMA
    ),
    total_value   = win_now_value + future_value
  )

# ---------------------------------------------------------------------------
# Final shape
# ---------------------------------------------------------------------------

team_assets <- assets %>%
  mutate(
    surplus_2026 = dollar_value_2026 - salary_2026
  ) %>%
  select(
    billikenTeam,
    Name,
    player_type,
    positions,
    lineup_slot,
    roster_status,
    salary_2026,
    contract_code,
    contract_status,
    contract_end,
    years_remaining,
    is_expiring_after_2026,
    next_offseason_decision,
    contract_source,
    age_2026,
    birth_year,
    ros_sgp_2026,
    sgpar_full_2026,
    fg_auction_dollars,
    fg_ros_auction_dollars,
    dollar_value_2026,
    dashboard_value_2026,
    dashboard_value_source,
    surplus_2026,
    prospect_value,
    prospect_value_2027,
    prospect_value_2028,
    prospect_value_2029,
    consensus_rank,
    prospect_eta,
    prospect_value_source,
    future_projection_source,
    drop_penalty_liability,
    sgpar_2027, sgpar_2028, sgpar_2029, sgpar_2030,
    dollar_value_2027, dollar_value_2028, dollar_value_2029, dollar_value_2030,
    salary_2027, salary_2028, salary_2029, salary_2030,
    surplus_2027, surplus_2028, surplus_2029, surplus_2030,
    win_now_surplus_sgp,
    win_now_value,
    future_value,
    total_value,
    ros_R, ros_HR, ros_RBI, ros_SB,
    ros_W, ros_SV, ros_SO,
    espn_player_id
  ) %>%
  arrange(billikenTeam, desc(coalesce(total_value, dollar_value_2026, 0)))

out_path <- resolve_path("data/processed/team_assets.csv")
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write_csv(team_assets, out_path)

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

n_total <- nrow(team_assets)
n_with_value <- sum(!is.na(team_assets$dollar_value_2026))
n_with_dashboard_value <- sum(!is.na(team_assets$dashboard_value_2026))
n_with_contract <- sum(!is.na(team_assets$contract_status))
n_with_positions <- sum(!is.na(team_assets$positions))

message(sprintf("Wrote %s", out_path))
message(sprintf("  rows:                %d", n_total))
message(sprintf("  with dollar_value:   %d (%.1f%%)",
                n_with_value, 100 * n_with_value / n_total))
message(sprintf("  with dashboard_value:%d (%.1f%%)",
                n_with_dashboard_value, 100 * n_with_dashboard_value / n_total))
message(sprintf("  with contract code:  %d (%.1f%%)",
                n_with_contract, 100 * n_with_contract / n_total))
message(sprintf("  with positions:      %d (%.1f%%)",
                n_with_positions, 100 * n_with_positions / n_total))

team_assets %>%
  count(contract_source) %>%
  pwalk(~ message(sprintf("  contract_source = %-15s: %d", ..1, ..2)))
