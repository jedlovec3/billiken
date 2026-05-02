# build_team_assets.R
#
# Phase 1 of the trade-analysis tooling. Produces a single canonical table
# `data/processed/team_assets.csv` that has one row per rostered Billiken
# player, joining:
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
    ros_sgp_2026,
    sgpar_full_2026,
    fg_auction_dollars,
    dollar_value_2026,
    surplus_2026,
    ros_R, ros_HR, ros_RBI, ros_SB,
    ros_W, ros_SV, ros_SO,
    espn_player_id
  ) %>%
  arrange(billikenTeam, desc(coalesce(dollar_value_2026, 0)))

out_path <- resolve_path("data/processed/team_assets.csv")
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write_csv(team_assets, out_path)

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

n_total <- nrow(team_assets)
n_with_value <- sum(!is.na(team_assets$dollar_value_2026))
n_with_contract <- sum(!is.na(team_assets$contract_status))
n_with_positions <- sum(!is.na(team_assets$positions))

message(sprintf("Wrote %s", out_path))
message(sprintf("  rows:                %d", n_total))
message(sprintf("  with dollar_value:   %d (%.1f%%)",
                n_with_value, 100 * n_with_value / n_total))
message(sprintf("  with contract code:  %d (%.1f%%)",
                n_with_contract, 100 * n_with_contract / n_total))
message(sprintf("  with positions:      %d (%.1f%%)",
                n_with_positions, 100 * n_with_positions / n_total))

team_assets %>%
  count(contract_source) %>%
  pwalk(~ message(sprintf("  contract_source = %-15s: %d", ..1, ..2)))
