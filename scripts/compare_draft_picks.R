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
# Options:
#   --players     Comma-separated list of candidate player names (required)
#   --team        Your team name (default: "Blue Socks")
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

players_raw <- get_arg(kv, "players", NULL)
if (is.null(players_raw) || identical(players_raw, "")) {
  stop("Missing required argument: --players=\"Player A,Player B,...\"", call. = FALSE)
}

candidates <- trimws(strsplit(players_raw, ",", fixed = TRUE)[[1]])
candidates <- candidates[candidates != ""]

if (length(candidates) == 0) {
  stop("No valid player names provided in --players.", call. = FALSE)
}

team_name    <- toupper(get_arg(kv, "team", "Blue Socks"))
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
# Validate candidates exist in the player pool
# --------------------------------------------------------------------------

player_pool <- load_draft_pool(projected_player_value_path, salaries_path)

# Also check they aren't already rostered/drafted
already_picked <- draft_order %>%
  filter(!is.na(player) & player != "" & player != "pass") %>%
  pull(player)

missing    <- character()
drafted    <- character()
valid      <- character()
salary_map <- numeric()

for (cand in candidates) {
  row <- player_pool %>% filter(Name == cand)
  if (nrow(row) == 0) {
    missing <- c(missing, cand)
    next
  }
  if (cand %in% already_picked) {
    drafted <- c(drafted, cand)
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
  message(sprintf("WARNING: Player(s) already drafted: %s", paste(drafted, collapse = ", ")))
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

  all_standings <- run_simulations(
    n_sims = n_sims,
    randomness_pct = randomness,
    seed = seed,
    verbose = verbose,
    projected_player_value_path = projected_player_value_path,
    salaries_path = salaries_path,
    draft_path = draft_path,
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
