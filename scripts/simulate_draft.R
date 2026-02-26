# scripts/simulate_draft.R
# Backwards-compatible runner for the draft simulation.
#
# The implementation lives in scripts/draft_simulation_lib.R so it can be reused
# by scenario tooling (e.g. hypothetical trades).

source("scripts/draft_simulation_lib.R")

message("\n=== Billiken League Draft Simulation ===\n")

all_standings <- run_simulations(n_sims = 100, randomness_pct = 0.10, verbose = TRUE)
summary <- summarize_simulations(all_standings)

message("\n=== Simulation Results ===\n")
print(summary, n = nrow(summary))

# Blue Socks specific analysis
message("\n=== Blue Socks Analysis ===\n")
blue_socks <- all_standings %>%
  filter(team == "BLUE SOCKS")

if (nrow(blue_socks) > 0) {
  message(sprintf("Average rank: %.2f", mean(blue_socks$rank)))
  message(sprintf(
    "Wins: %d/%d (%.1f%%)",
    sum(blue_socks$rank == 1),
    nrow(blue_socks),
    100 * mean(blue_socks$rank == 1)
  ))
  message(sprintf(
    "Top 3 finishes: %d/%d (%.1f%%)",
    sum(blue_socks$rank <= 3),
    nrow(blue_socks),
    100 * mean(blue_socks$rank <= 3)
  ))
  message(sprintf(
    "Average total points: %.1f (range: %.1f - %.1f)",
    mean(blue_socks$total_pts),
    min(blue_socks$total_pts),
    max(blue_socks$total_pts)
  ))
}

message("\nDone!")
