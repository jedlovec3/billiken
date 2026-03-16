# scripts/compare_draft_picks.R
# Compare projected standings for different draft pick candidates.
#
# Usage:
#   Rscript scripts/compare_draft_picks.R \
#     --players="Bryce Harper,Bo Bichette" \
#     --team="Blue Socks" \
#     --n_sims=100 \
#     --seed=42
#
#   # Auto-detect top 10 available players for the team:
#   Rscript scripts/compare_draft_picks.R --team="Blue Socks"
#
# Options:
#   --players     Comma-separated list of candidate player names (optional;
#                 if omitted, the top --top_n available players by sgpar are used)
#   --team        Your team name (default: "Blue Socks")
#   --top_n       Number of top available players to compare when --players is
#                 omitted (default: 10)
#   --round       Force a specific draft round (default: auto-detect next open pick)
#   --pick        Force a specific draft pick number (default: auto-detect)
#   --n_sims      Number of simulations per candidate (default: 100)
#   --randomness  Randomness percentage for other teams' picks (default: 0.10)
#   --seed        Random seed for reproducibility (default: 42)
#   --verbose     Print simulation progress (default: false)
#   --output      Path to write the comparison CSV (default: auto-generated under data/compare_picks/)

suppressPackageStartupMessages({
  library(tidyverse)
})

source("scripts/draft_simulation_lib.R")

# --------------------------------------------------------------------------
# Argument parsing (reuse the pattern from run_trade_scenario.R)
# --------------------------------------------------------------------------

parse_kv_args <- function(args) {
  out <- list()
  for (a in args) {
    if (!startsWith(a, "--")) next
    kv <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]]
    if (length(kv) < 2) next
    # Rejoin in case value itself contains '='
    out[[kv[[1]]]] <- paste(kv[-1], collapse = "=")
  }
  out
}

get_arg <- function(kv, key, default = NULL) {
  if (!is.null(kv[[key]])) kv[[key]] else default
}

to_bool <- function(x, default = FALSE) {
  if (is.null(x)) return(default)
  tolower(trimws(as.character(x))) %in% c("1", "true", "t", "yes", "y")
}

kv <- parse_kv_args(commandArgs(trailingOnly = TRUE))

players_raw  <- get_arg(kv, "players", NULL)
team_name    <- toupper(get_arg(kv, "team", "Blue Socks"))
top_n        <- as.integer(get_arg(kv, "top_n", 10))
round_arg    <- get_arg(kv, "round", NULL)
pick_arg     <- get_arg(kv, "pick", NULL)
n_sims       <- as.integer(get_arg(kv, "n_sims", 100))
randomness   <- as.numeric(get_arg(kv, "randomness", 0.10))
seed         <- as.integer(get_arg(kv, "seed", 42))
verbose      <- to_bool(get_arg(kv, "verbose", "false"), default = FALSE)

projected_player_value_path <- get_arg(kv, "projected_player_value", "data/processed/projected_player_value.csv")
salaries_path <- get_arg(kv, "salaries", "data/raw/salaries_latest.csv")
draft_path    <- get_arg(kv, "draft", "data/raw/draft_latest.csv")
output_path   <- get_arg(kv, "output", NULL)

auto_detect_players <- is.null(players_raw) || identical(players_raw, "")

if (!auto_detect_players) {
  candidates <- trimws(strsplit(players_raw, ",", fixed = TRUE)[[1]])
  candidates <- candidates[candidates != ""]
  if (length(candidates) == 0) {
    stop("No valid player names provided in --players.", call. = FALSE)
  }
}

# --------------------------------------------------------------------------
# Detect next open pick for the team
# --------------------------------------------------------------------------

draft_order <- load_draft_order(draft_path)

if (!is.null(round_arg) && !is.null(pick_arg)) {
  target_round <- as.integer(round_arg)
  target_pick  <- as.integer(pick_arg)
  message(sprintf("Using specified pick: round %d, pick %d", target_round, target_pick))
} else {
  next_open <- draft_order %>%
    filter(
      toupper(billikenTeam) == team_name,
      is.na(player) | player == ""
    ) %>%
    arrange(Round, Pick) %>%
    slice(1)

  if (nrow(next_open) == 0) {
    stop(sprintf("No open picks found for team '%s' in the draft order.", team_name), call. = FALSE)
  }

  target_round <- next_open$Round[1]
  target_pick  <- next_open$Pick[1]
  message(sprintf("Auto-detected next open pick for %s: round %d, pick %d", team_name, target_round, target_pick))
}

# --------------------------------------------------------------------------
# Build the player pool and identify unavailable players (drafted + kept)
# --------------------------------------------------------------------------

player_pool <- load_draft_pool(projected_player_value_path, salaries_path)

already_drafted <- draft_order %>%
  filter(!is.na(player) & player != "" & player != "pass") %>%
  pull(player)

keepers <- load_default_keepers(verbose = verbose)
kept_players <- if (nrow(keepers) > 0) keepers$Name else character()

unavailable <- unique(c(already_drafted, kept_players))

# --------------------------------------------------------------------------
# Prepare simulation context (need roster template for slot eligibility)
# --------------------------------------------------------------------------

message("Preparing simulation context...")
sim_ctx <- prepare_sim_context(
  projected_player_value_path = projected_player_value_path,
  salaries_path = salaries_path,
  draft_path = draft_path,
  verbose = verbose
)

# Helper: can this player be assigned to an open slot on the team?
can_fit_roster <- function(player_name, team, rosters_tmpl, pool) {
  row <- pool %>% filter(Name == player_name)
  if (nrow(row) == 0) return(FALSE)
  !is.na(find_best_slot(rosters_tmpl, team, row))
}

# --------------------------------------------------------------------------
# Determine candidates (auto-detect or validate user-supplied list)
# --------------------------------------------------------------------------

if (auto_detect_players) {
  # Find the top N available players by sgpar that can fit on the roster
  available_pool <- player_pool %>%
    filter(!Name %in% unavailable) %>%
    filter(!is.na(sgpar)) %>%
    arrange(desc(sgpar))

  fits <- vapply(available_pool$Name, function(nm) {
    can_fit_roster(nm, team_name, sim_ctx$rosters_template, sim_ctx$player_pool)
  }, logical(1))
  available_pool <- available_pool[fits, ] %>% slice_head(n = top_n)

  if (nrow(available_pool) == 0) {
    stop("No available players found that fit an open roster slot.", call. = FALSE)
  }

  candidates <- available_pool$Name
  message(sprintf("Auto-selected top %d available players with open roster slots:", length(candidates)))
  for (nm in candidates) {
    sgp <- available_pool$sgpar[available_pool$Name == nm]
    message(sprintf("  %s (sgpar: %.2f)", nm, sgp))
  }
}

missing    <- character()
drafted    <- character()
no_slot    <- character()
valid      <- character()
salary_map <- numeric()

for (cand in candidates) {
  row <- player_pool %>% filter(Name == cand)
  if (nrow(row) == 0) {
    missing <- c(missing, cand)
    next
  }
  if (cand %in% unavailable) {
    drafted <- c(drafted, cand)
    next
  }
  if (!can_fit_roster(cand, team_name, sim_ctx$rosters_template, sim_ctx$player_pool)) {
    no_slot <- c(no_slot, cand)
    next
  }
  valid <- c(valid, cand)
  sal <- row$salary[1]
  salary_map[cand] <- ifelse(is.na(sal), DEFAULT_SALARY, sal)
}

if (length(missing) > 0) {
  message(sprintf("WARNING: Player(s) not found in projections: %s", paste(missing, collapse = ", ")))
}
if (length(drafted) > 0) {
  message(sprintf("WARNING: Player(s) already kept or drafted: %s", paste(drafted, collapse = ", ")))
}
if (length(no_slot) > 0) {
  message(sprintf("WARNING: No open roster slot for: %s", paste(no_slot, collapse = ", ")))
}
if (length(valid) == 0) {
  stop("No valid candidates remaining after validation.", call. = FALSE)
}

# --------------------------------------------------------------------------
# Run simulations for each candidate
# --------------------------------------------------------------------------

message(sprintf(
  "\n=== Comparing %d candidate(s) for %s (round %d, pick %d) | %d sims each ===\n",
  length(valid), team_name, target_round, target_pick, n_sims
))

results <- list()

for (cand in valid) {
  message(sprintf("--- Simulating: %s ($%d) ---", cand, salary_map[cand]))

  forced <- tibble(
    Round  = target_round,
    Pick   = target_pick,
    player = cand,
    salary = salary_map[cand]
  )

  all_standings <- run_simulations_from_context(
    sim_ctx,
    n_sims = n_sims,
    randomness_pct = randomness,
    seed = seed,
    verbose = verbose,
    forced_picks = forced
  )

  team_results <- all_standings %>% filter(toupper(team) == team_name)

  if (nrow(team_results) == 0) {
    message(sprintf("  WARNING: No results for %s — skipping.", team_name))
    next
  }

  results[[cand]] <- tibble(
    player      = cand,
    salary      = salary_map[cand],
    avg_rank    = mean(team_results$rank),
    avg_pts     = mean(team_results$total_pts),
    win_pct     = 100 * mean(team_results$rank == 1),
    top3_pct    = 100 * mean(team_results$rank <= 3),
    avg_hit_pts = mean(team_results$hit_pts),
    avg_pit_pts = mean(team_results$pit_pts),
    min_pts     = min(team_results$total_pts),
    max_pts     = max(team_results$total_pts)
  )
}

if (length(results) == 0) {
  stop("No successful simulations.", call. = FALSE)
}

# --------------------------------------------------------------------------
# Print comparison
# --------------------------------------------------------------------------

comparison <- bind_rows(results) %>% arrange(avg_rank)

message(sprintf("\n=== %s Draft Pick Comparison (round %d, pick %d) ===\n", team_name, target_round, target_pick))

# Print a formatted summary
for (i in seq_len(nrow(comparison))) {
  row <- comparison[i, ]
  message(sprintf(
    "%d. %s ($%d)",
    i, row$player, row$salary
  ))
  message(sprintf(
    "   Avg rank: %.2f | Avg pts: %.1f (%.1f-%.1f) | Wins: %.1f%% | Top 3: %.1f%%",
    row$avg_rank, row$avg_pts, row$min_pts, row$max_pts, row$win_pct, row$top3_pct
  ))
  message(sprintf(
    "   Hit pts: %.1f | Pit pts: %.1f",
    row$avg_hit_pts, row$avg_pit_pts
  ))
  message("")
}

# Also print a compact one-line-per-player table
cat("\n")
header <- sprintf("%-25s %6s %8s %8s %7s %7s %8s %8s",
                  "Player", "Salary", "AvgRank", "AvgPts", "Win%", "Top3%", "HitPts", "PitPts")
cat(header, "\n")
cat(paste(rep("-", nchar(header)), collapse = ""), "\n")

for (i in seq_len(nrow(comparison))) {
  row <- comparison[i, ]
  cat(sprintf("%-25s %5d  %7.2f  %7.1f  %6.1f  %6.1f  %7.1f  %7.1f\n",
              row$player, row$salary, row$avg_rank, row$avg_pts,
              row$win_pct, row$top3_pct, row$avg_hit_pts, row$avg_pit_pts))
}

# --------------------------------------------------------------------------
# Write comparison CSV
# --------------------------------------------------------------------------

if (is.null(output_path)) {
  out_dir <- "data/compare_picks"
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_path <- file.path(out_dir, sprintf("%s_round%d_pick%d_%s.csv", tolower(team_name), target_round, target_pick, ts))
}

readr::write_csv(comparison, output_path)
message(sprintf("\nComparison written to %s", output_path))

message("Done!")
