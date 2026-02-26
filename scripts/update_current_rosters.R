# scripts/update_current_rosters.R
# Build preseason rosters by assigning keepers and drafted players to roster
# slots using maximum bipartite matching based on ESPN position eligibility.

suppressPackageStartupMessages({
  library(tidyverse)
  library(igraph)
  library(fuzzyjoin)
})

# --- Roster slot structure (23 active slots per team) ---
slot_structure <- c(
  C = 2, `1B` = 1, `2B` = 1, `3B` = 1, SS = 1,
  OF = 5, MI = 1, CI = 1, Util = 1, P = 9
)
slot_names <- rep(names(slot_structure), slot_structure)

# --- Helper: normalise player names for matching ---
normalize_name <- function(name) {
  name %>%
    str_replace_all(" Jr\\.?$", "") %>%
    str_replace_all(" Sr\\.?$", "") %>%
    str_replace_all(" III$", "") %>%
    str_replace_all(" II$", "") %>%
    str_trim()
}

# --------------------------------------------------------------------------
# 1. Load team names from draft
# --------------------------------------------------------------------------
message("Loading draft data...")
draft <- read_csv("data/raw/draft_latest.csv", show_col_types = FALSE) %>%
  filter(!is.na(Team))

teams <- draft %>%
  distinct(Team) %>%
  mutate(billikenTeam = toupper(Team)) %>%
  pull(billikenTeam) %>%
  sort()

message(sprintf("Found %d teams: %s", length(teams), paste(teams, collapse = ", ")))
stopifnot(length(teams) == 10)

# --------------------------------------------------------------------------
# 2. Create empty roster skeleton (23 active slots per team)
# --------------------------------------------------------------------------
message("Creating empty roster skeletons...")

empty_rosters <- map_dfr(teams, function(team) {
  tibble(
    billikenTeam = team,
    Position     = slot_names,
    Player       = NA_character_,
    Contract     = NA_character_,
    Salary       = NA_real_
  )
})

# --------------------------------------------------------------------------
# 3. Load keepers and check which teams have finalized them
# --------------------------------------------------------------------------
message("Loading keepers...")
keepers_raw <- read_csv("data/raw/keepers.csv", show_col_types = FALSE)

keepers <- keepers_raw %>%
  filter(!is.na(Player) & Player != "NA") %>%
  select(Player, Contract, Salary, billikenTeam) %>%
  mutate(
    billikenTeam = toupper(billikenTeam),
    Contract     = as.character(Contract),
    Salary       = as.numeric(Salary),
    source       = "keeper"
  )

keeper_counts <- tibble(billikenTeam = teams) %>%
  left_join(
    keepers %>% count(billikenTeam, name = "n_keepers"),
    by = "billikenTeam"
  ) %>%
  replace_na(list(n_keepers = 0))

teams_with_keepers    <- keeper_counts %>% filter(n_keepers > 0) %>% pull(billikenTeam)
teams_without_keepers <- keeper_counts %>% filter(n_keepers == 0) %>% pull(billikenTeam)

message(sprintf("Teams with finalized keepers: %d of %d",
                length(teams_with_keepers), length(teams)))

if (length(teams_with_keepers) > 0) {
  keeper_counts %>% filter(n_keepers > 0) %>%
    pwalk(~ message(sprintf("  %s: %d keepers", ..1, ..2)))
}

# --------------------------------------------------------------------------
# 4. Check whether the draft has started
# --------------------------------------------------------------------------
draft_picks <- draft %>%
  filter(!is.na(Player) & Player != "NA") %>%
  mutate(
    billikenTeam = toupper(Team),
    Salary       = as.numeric(Salary),
    Contract     = NA_character_,
    source       = "draft"
  ) %>%
  select(Player, Contract, Salary, billikenTeam, source)

n_drafted <- nrow(draft_picks)

if (length(teams_with_keepers) == length(teams) && n_drafted == 0) {
  message("All teams have keepers finalized. Draft has not started.")
} else if (length(teams_with_keepers) == 0 && n_drafted == 0) {
  message("No keepers finalized and draft has not started.")
} else if (n_drafted > 0) {
  message(sprintf("Draft has started: %d players drafted.", n_drafted))
}

# --------------------------------------------------------------------------
# 5. Combine keepers + drafted players
# --------------------------------------------------------------------------
all_players <- bind_rows(keepers, draft_picks)

if (nrow(all_players) == 0) {
  message("\nNo players to assign. Writing empty rosters.")
  dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
  write_csv(empty_rosters, "data/processed/preseason_rosters.csv")
  message("✓ Wrote data/processed/preseason_rosters.csv")
  quit(save = "no", status = 0)
}

# --------------------------------------------------------------------------
# 6. Load position eligibility and join to rostered players
# --------------------------------------------------------------------------
message("\nLoading position eligibility...")
positions <- read_csv("data/raw/positions_latest.csv", show_col_types = FALSE) %>%
  mutate(name_normalized = normalize_name(PLAYER))

all_players <- all_players %>%
  mutate(name_normalized = normalize_name(Player))

# Exact match first
matched_exact <- all_players %>%
  inner_join(positions, by = "name_normalized")

unmatched <- all_players %>%
  anti_join(positions, by = "name_normalized")

if (nrow(unmatched) > 0) {
  message(sprintf("Fuzzy matching %d unmatched players...", nrow(unmatched)))
  matched_fuzzy <- unmatched %>%
    stringdist_left_join(
      positions,
      by         = c("name_normalized" = "name_normalized"),
      max_dist   = 2,
      distance_col = "dist"
    ) %>%
    group_by(Player, billikenTeam) %>%
    slice_min(dist, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-dist) %>%
    rename(name_normalized = name_normalized.x) %>%
    select(-name_normalized.y)

  still_unmatched <- matched_fuzzy %>% filter(is.na(PLAYER))
  if (nrow(still_unmatched) > 0) {
    message(sprintf("WARNING: %d players could not be matched to positions:",
                    nrow(still_unmatched)))
    walk(still_unmatched$Player, ~ message(sprintf("  %s", .x)))
  }

  all_with_pos <- bind_rows(matched_exact, matched_fuzzy) %>%
    filter(!is.na(PLAYER))
} else {
  all_with_pos <- matched_exact
}

# Derive composite eligibility flags
all_with_pos <- all_with_pos %>%
  mutate(
    elig_C    = replace_na(C,    0),
    elig_1B   = replace_na(`1B`, 0),
    elig_2B   = replace_na(`2B`, 0),
    elig_3B   = replace_na(`3B`, 0),
    elig_SS   = replace_na(SS,   0),
    elig_OF   = pmax(replace_na(LF, 0), replace_na(CF, 0), replace_na(RF, 0)),
    elig_MI   = pmax(replace_na(`2B`, 0), replace_na(SS, 0)),
    elig_CI   = pmax(replace_na(`1B`, 0), replace_na(`3B`, 0)),
    elig_Util = as.integer(
      pmax(replace_na(C, 0), replace_na(`1B`, 0), replace_na(`2B`, 0),
           replace_na(`3B`, 0), replace_na(SS, 0), replace_na(LF, 0),
           replace_na(CF, 0), replace_na(RF, 0), replace_na(DH, 0)) == 1
    ),
    elig_P    = pmax(replace_na(SP, 0), replace_na(RP, 0))
  )

# --------------------------------------------------------------------------
# 7. Bipartite matching per team
# --------------------------------------------------------------------------
message("\nAssigning players to roster slots via bipartite matching...")

assign_players_to_slots <- function(team_name, team_players, slot_names) {

  n_p <- nrow(team_players)
  n_s <- length(slot_names)

  if (n_p == 0) {
    return(list(
      assigned   = tibble(billikenTeam = team_name, Position = slot_names,
                          Player = NA_character_, Contract = NA_character_,
                          Salary = NA_real_),
      unassigned = tibble()
    ))
  }

  # Build edge list (player i <-> slot j)
  edges <- integer(0)
  for (i in seq_len(n_p)) {
    p <- team_players[i, ]
    for (j in seq_len(n_s)) {
      eligible <- switch(slot_names[j],
        "C"    = p$elig_C    == 1,
        "1B"   = p$elig_1B   == 1,
        "2B"   = p$elig_2B   == 1,
        "3B"   = p$elig_3B   == 1,
        "SS"   = p$elig_SS   == 1,
        "OF"   = p$elig_OF   == 1,
        "MI"   = p$elig_MI   == 1,
        "CI"   = p$elig_CI   == 1,
        "Util" = p$elig_Util == 1,
        "P"    = p$elig_P    == 1,
        FALSE
      )
      if (isTRUE(eligible)) {
        edges <- c(edges, i, n_p + j)
      }
    }
  }

  # Start with empty result
  result <- tibble(
    billikenTeam = team_name,
    Position     = slot_names,
    Player       = NA_character_,
    Contract     = NA_character_,
    Salary       = NA_real_
  )

  if (length(edges) == 0) {
    warning(sprintf("No eligible slot assignments found for %s", team_name))
    return(list(assigned = result, unassigned = team_players))
  }

  # Create bipartite graph and solve matching
  g <- make_bipartite_graph(
    types = c(rep(FALSE, n_p), rep(TRUE, n_s)),
    edges = edges
  )
  V(g)$name <- c(paste0("p", seq_len(n_p)), paste0("s", seq_len(n_s)))

  m <- max_bipartite_match(g)

  # Extract assignments
  assigned_indices <- integer(0)
  for (j in seq_len(n_s)) {
    matched_vertex <- m$matching[paste0("s", j)]
    if (!is.na(matched_vertex)) {
      idx <- as.integer(sub("^p", "", matched_vertex))
      result$Player[j]   <- team_players$Player[idx]
      result$Contract[j] <- team_players$Contract[idx]
      result$Salary[j]   <- team_players$Salary[idx]
      assigned_indices    <- c(assigned_indices, idx)
    }
  }

  # Determine unassigned players
  if (length(assigned_indices) == n_p) {
    unassigned <- tibble()
  } else if (length(assigned_indices) > 0) {
    unassigned <- team_players %>% slice(-assigned_indices)
  } else {
    unassigned <- team_players
  }

  list(assigned = result, unassigned = unassigned)
}

# Process each team
final_rosters <- tibble()
overflow_rows <- tibble()

for (team in teams) {
  team_players <- all_with_pos %>% filter(billikenTeam == team)

  if (nrow(team_players) == 0) {
    # No players yet — empty active roster
    team_roster <- tibble(
      billikenTeam = team,
      Position     = slot_names,
      Player       = NA_character_,
      Contract     = NA_character_,
      Salary       = NA_real_
    )
    final_rosters <- bind_rows(final_rosters, team_roster)
    message(sprintf("  %s: 0 players (no keepers or draft picks)", team))
    next
  }

  result <- assign_players_to_slots(team, team_players, slot_names)
  final_rosters <- bind_rows(final_rosters, result$assigned)

  n_assigned <- sum(!is.na(result$assigned$Player))
  message(sprintf("  %s: %d/%d players assigned to active slots",
                  team, n_assigned, nrow(team_players)))

  # Overflow players go to Minors (up to 3) then IL
  if (nrow(result$unassigned) > 0) {
    overflow <- result$unassigned %>%
      select(Player, Contract, Salary, billikenTeam)

    n_minors <- min(nrow(overflow), 3)
    if (n_minors > 0) {
      minors_rows <- overflow %>%
        slice_head(n = n_minors) %>%
        mutate(Position = "Minors")
      overflow_rows <- bind_rows(overflow_rows, minors_rows)
    }
    if (nrow(overflow) > 3) {
      il_rows <- overflow %>%
        slice_tail(n = nrow(overflow) - 3) %>%
        mutate(Position = "IL")
      overflow_rows <- bind_rows(overflow_rows, il_rows)
    }

    message(sprintf("    + %d overflow -> Minors/IL", nrow(overflow)))
  }
}

# Append any Minors/IL rows
if (nrow(overflow_rows) > 0) {
  overflow_rows <- overflow_rows %>%
    select(billikenTeam, Position, Player, Contract, Salary)
  final_rosters <- bind_rows(final_rosters, overflow_rows)
}

# --------------------------------------------------------------------------
# 8. Write output
# --------------------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(final_rosters, "data/processed/preseason_rosters.csv")

n_filled <- sum(!is.na(final_rosters$Player))
n_total  <- nrow(final_rosters)
message(sprintf("\n✓ Wrote data/processed/preseason_rosters.csv"))
message(sprintf("  %d slots total, %d filled, %d empty", n_total, n_filled, n_total - n_filled))
