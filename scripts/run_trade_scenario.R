# scripts/run_trade_scenario.R
# Compare baseline vs a hypothetical trade scenario.
#
# Usage example:
#   Rscript scripts/run_trade_scenario.R \
#     --trades=scenarios/miller_for_yam.csv \
#     --scenario=miller_for_yam \
#     --n_sims=200 \
#     --randomness=0.10 \
#     --seed=42 \
#     --baseline_cache=true
#
# Baseline caching (optional):
# - --baseline_cache=true: store/reuse a shared baseline across scenarios (default true).
# - --baseline_refresh=true: force baseline recompute even if cached files exist.
# - --baseline_id=...: override the computed baseline cache key.
# - --baseline_cache_root=...: where to store shared baselines (default data/scenarios/_baseline).
#
# Scenario CSV format (can mix row types in one file):
# - Player move row: player,from_team,to_team[,ForceKeeper,DropPenalty]
#     - If to_team is blank/NA/NULL, the player is dropped to the free agent pool.
#     - ForceKeeper (optional): 1 = force keep, 0 = force drop (after applying player moves).
#       For a "pure" keeper override (no trade), set to_team == from_team.
#     - DropPenalty (optional): numeric dead-money hit charged to the dropping team when the
#       player is dropped (to_team blank/NA/NULL or ForceKeeper=0).
# - Pick trade row: (blank player),from_team,to_team,round,pick

suppressPackageStartupMessages({
  library(tidyverse)
})

# Local helpers
if (file.exists("scripts/paths.R")) source("scripts/paths.R")
if (file.exists("paths.R")) source("paths.R")

if (file.exists("scripts/simulate_keepers.R")) source("scripts/simulate_keepers.R")
if (file.exists("simulate_keepers.R")) source("simulate_keepers.R")

if (file.exists("scripts/trade_utils.R")) source("scripts/trade_utils.R")
if (file.exists("trade_utils.R")) source("trade_utils.R")

if (file.exists("scripts/draft_simulation_lib.R")) source("scripts/draft_simulation_lib.R")
if (file.exists("draft_simulation_lib.R")) source("draft_simulation_lib.R")

root <- if (exists("find_project_root")) find_project_root() else getwd()

parse_kv_args <- function(args) {
  out <- list()
  for (a in args) {
    if (!startsWith(a, "--")) next
    kv <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]]
    if (length(kv) != 2) next
    out[[kv[[1]]]] <- kv[[2]]
  }
  out
}

get_arg <- function(kv, key, default = NULL) {
  if (!is.null(kv[[key]])) kv[[key]] else default
}

to_bool <- function(x, default = FALSE) {
  if (is.null(x)) return(default)
  x2 <- tolower(trimws(as.character(x)))
  x2 %in% c("1", "true", "t", "yes", "y")
}

kv <- parse_kv_args(commandArgs(trailingOnly = TRUE))

abs_path <- function(p) {
  if (is.null(p)) return(NULL)
  if (grepl("^/", p)) p else file.path(root, p)
}

trades_path <- get_arg(kv, "trades", NULL)
if (is.null(trades_path) || identical(trades_path, "")) {
  stop("Missing required argument: --trades=path/to/trades.csv", call. = FALSE)
}

trades_path_abs <- abs_path(trades_path)
if (!file.exists(trades_path_abs)) {
  stop(sprintf("Trades file not found: %s", trades_path_abs), call. = FALSE)
}

scenario_name <- get_arg(
  kv,
  "scenario",
  tools::file_path_sans_ext(basename(trades_path_abs))
)

n_sims <- as.integer(get_arg(kv, "n_sims", 200))
randomness <- as.numeric(get_arg(kv, "randomness", 0.10))
sgpar_random <- as.numeric(get_arg(kv, "sgpar_random", 0))
seed <- as.integer(get_arg(kv, "seed", 42))
verbose <- to_bool(get_arg(kv, "verbose", "true"), default = TRUE)

prefreeze_rosters_path <- get_arg(kv, "prefreeze_rosters", "data/raw/prefreeze_rosters_latest.csv")
projected_player_value_path <- get_arg(kv, "projected_player_value", "data/processed/projected_player_value.csv")
draft_path <- get_arg(kv, "draft", "data/raw/draft_latest.csv")
salaries_path <- get_arg(kv, "salaries", "data/raw/salaries_latest.csv")

prefreeze_rosters_path_abs <- abs_path(prefreeze_rosters_path)
projected_player_value_path_abs <- abs_path(projected_player_value_path)
draft_path_abs <- abs_path(draft_path)
salaries_path_abs <- abs_path(salaries_path)

if (is.na(n_sims) || n_sims <= 0) stop("--n_sims must be a positive integer", call. = FALSE)
if (is.na(randomness) || randomness < 0) stop("--randomness must be >= 0", call. = FALSE)

# Baseline caching / reuse
# - baseline_cache=true: store baseline outputs in a shared cache directory keyed by inputs + params.
# - baseline_refresh=true: force recompute even if cached baseline exists.
# - baseline_id=...: override the computed cache key.
# - baseline_cache_root=...: where to store shared baselines (default: data/scenarios/_baseline)
baseline_cache <- to_bool(get_arg(kv, "baseline_cache", "true"), default = TRUE)
baseline_refresh <- to_bool(get_arg(kv, "baseline_refresh", "false"), default = FALSE)
baseline_cache_root <- get_arg(kv, "baseline_cache_root", "data/scenarios/_baseline")
baseline_id <- get_arg(kv, "baseline_id", NULL)

short_md5 <- function(p) {
  if (is.null(p) || is.na(p) || !file.exists(p)) return("missing")
  substr(unname(tools::md5sum(p)), 1, 8)
}

if (is.null(baseline_id) || identical(baseline_id, "")) {
  baseline_id <- sprintf(
    "n%d_rand%s_sgpar%s_seed%d_pre%s_proj%s_draft%s_sal%s",
    n_sims,
    formatC(randomness, format = "f", digits = 3),
    formatC(sgpar_random, format = "f", digits = 3),
    seed,
    short_md5(prefreeze_rosters_path_abs),
    short_md5(projected_player_value_path_abs),
    short_md5(draft_path_abs),
    short_md5(salaries_path_abs)
  )
}

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_root <- file.path(root, "data", "scenarios", scenario_name, ts)
scenario_dir <- file.path(out_root, "scenario")

if (isTRUE(baseline_cache)) {
  baseline_cache_root_abs <- abs_path(baseline_cache_root)
  baseline_dir <- file.path(baseline_cache_root_abs, baseline_id)
} else {
  baseline_dir <- file.path(out_root, "baseline")
}

dir.create(baseline_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(scenario_dir, recursive = TRUE, showWarnings = FALSE)

# Record which baseline was used for this scenario run.
writeLines(baseline_dir, file.path(out_root, "baseline_path.txt"))

message("\n=== Trade scenario simulation ===")
message(sprintf("Scenario: %s", scenario_name))
message(sprintf("Trades file: %s", trades_path_abs))
message(sprintf("n_sims=%d, randomness=%.3f, seed=%d", n_sims, randomness, seed))
message(sprintf("Baseline: %s", baseline_dir))
message(sprintf("Output: %s", out_root))

# -----------------
# Baseline
# -----------------
baseline_all_path <- file.path(baseline_dir, "standings_all.csv")
baseline_summary_path <- file.path(baseline_dir, "standings_summary.csv")

baseline_done <- file.exists(baseline_all_path) && file.exists(baseline_summary_path)

if (baseline_done && !isTRUE(baseline_refresh)) {
  message("\n--- Baseline: reusing cached standings ---")
  baseline_all <- readr::read_csv(baseline_all_path, show_col_types = FALSE)
  baseline_summary <- readr::read_csv(baseline_summary_path, show_col_types = FALSE)
} else {
  message("\n--- Baseline: simulate keepers ---")
  baseline_keeper_res <- simulate_keepers(
    sgpar_random = sgpar_random,
    prefreeze_rosters_path = prefreeze_rosters_path_abs,
    projected_player_value_path = projected_player_value_path_abs,
    trades_path = NULL,
    output_dir = baseline_dir,
    seed = seed
  )

  baseline_keepers <- baseline_keeper_res$simulated_keepers

  baseline_salary_cap_by_team <- NULL
  if (!is.null(baseline_keeper_res$drop_penalties_by_team) && nrow(baseline_keeper_res$drop_penalties_by_team) > 0) {
    caps <- baseline_keeper_res$drop_penalties_by_team %>%
      mutate(salary_cap = SALARY_CAP - cap_penalty) %>%
      transmute(team = toupper(billikenTeam), salary_cap)

    baseline_salary_cap_by_team <- stats::setNames(caps$salary_cap, caps$team)
    readr::write_csv(caps, file.path(baseline_dir, "salary_caps.csv"))
  }

  message("\n--- Baseline: simulate draft + standings ---")
  baseline_all <- run_simulations(
    n_sims = n_sims,
    randomness_pct = randomness,
    simulated_keepers = baseline_keepers,
    seed = seed,
    verbose = verbose,
    projected_player_value_path = projected_player_value_path_abs,
    salaries_path = salaries_path_abs,
    draft_path = draft_path_abs,
    salary_cap_by_team = baseline_salary_cap_by_team
  )

  baseline_summary <- summarize_simulations(baseline_all)

  readr::write_csv(baseline_all, baseline_all_path)
  readr::write_csv(baseline_summary, baseline_summary_path)
}

# -----------------
# Scenario
# -----------------
message("\n--- Scenario: simulate keepers (with trades) ---")
scenario_keeper_res <- simulate_keepers(
  sgpar_random = sgpar_random,
  prefreeze_rosters_path = prefreeze_rosters_path_abs,
  projected_player_value_path = projected_player_value_path_abs,
  trades_path = trades_path_abs,
  output_dir = scenario_dir,
  seed = seed
)

scenario_keepers <- scenario_keeper_res$simulated_keepers

scenario_salary_cap_by_team <- NULL
if (!is.null(scenario_keeper_res$drop_penalties_by_team) && nrow(scenario_keeper_res$drop_penalties_by_team) > 0) {
  caps <- scenario_keeper_res$drop_penalties_by_team %>%
    mutate(salary_cap = SALARY_CAP - cap_penalty) %>%
    transmute(team = toupper(billikenTeam), salary_cap)

  scenario_salary_cap_by_team <- stats::setNames(caps$salary_cap, caps$team)
  readr::write_csv(caps, file.path(scenario_dir, "salary_caps.csv"))
}

message("\n--- Scenario: simulate draft + standings ---")

scenario_draft_path_abs <- draft_path_abs
if (exists("read_pick_trades_csv") && exists("apply_pick_trades_to_draft")) {
  pick_trades <- read_pick_trades_csv(trades_path_abs)

  if (nrow(pick_trades) > 0) {
    message(sprintf("Applying %d pick trade row(s) to draft order...", nrow(pick_trades)))

    draft_raw <- readr::read_csv(draft_path_abs, show_col_types = FALSE)
    draft_updated <- apply_pick_trades_to_draft(draft_raw, pick_trades)

    scenario_draft_path_abs <- file.path(scenario_dir, "draft_order_scenario.csv")
    readr::write_csv(draft_updated, scenario_draft_path_abs)
  }
}

scenario_all <- run_simulations(
  n_sims = n_sims,
  randomness_pct = randomness,
  simulated_keepers = scenario_keepers,
  seed = seed,
  verbose = verbose,
  projected_player_value_path = projected_player_value_path_abs,
  salaries_path = salaries_path_abs,
  draft_path = scenario_draft_path_abs,
  salary_cap_by_team = scenario_salary_cap_by_team
)

scenario_summary <- summarize_simulations(scenario_all)

readr::write_csv(scenario_all, file.path(scenario_dir, "standings_all.csv"))
readr::write_csv(scenario_summary, file.path(scenario_dir, "standings_summary.csv"))

# -----------------
# Delta
# -----------------
message("\n--- Delta vs baseline ---")

baseline_pref <- baseline_summary %>%
  rename_with(~ paste0("baseline_", .x), -team)
scenario_pref <- scenario_summary %>%
  rename_with(~ paste0("scenario_", .x), -team)

delta <- baseline_pref %>%
  inner_join(scenario_pref, by = "team") %>%
  mutate(
    delta_avg_pts = scenario_avg_pts - baseline_avg_pts,
    delta_avg_rank = scenario_avg_rank - baseline_avg_rank,
    delta_wins = scenario_wins - baseline_wins,
    delta_top_3 = scenario_top_3 - baseline_top_3,
    delta_avg_hit_pts = scenario_avg_hit_pts - baseline_avg_hit_pts,
    delta_avg_pit_pts = scenario_avg_pit_pts - baseline_avg_pit_pts
  ) %>%
  arrange(desc(delta_avg_pts))

readr::write_csv(delta, file.path(out_root, "delta_summary.csv"))

message("\nTop +delta_avg_pts:")
print(delta %>% select(team, baseline_avg_pts, scenario_avg_pts, delta_avg_pts, baseline_avg_rank, scenario_avg_rank, delta_avg_rank) %>% head(10))

message("\nBottom delta_avg_pts:")
print(delta %>% select(team, baseline_avg_pts, scenario_avg_pts, delta_avg_pts, baseline_avg_rank, scenario_avg_rank, delta_avg_rank) %>% tail(10))

# Convenience pointer
latest_path <- file.path(root, "data", "scenarios", scenario_name, "latest.txt")
dir.create(dirname(latest_path), recursive = TRUE, showWarnings = FALSE)
writeLines(out_root, latest_path)

message("\nDone!")
