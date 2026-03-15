args <- commandArgs(trailingOnly=TRUE)

players <- args[1]
n_sims <- args[2]

cmd <- paste(
  "Rscript scripts/compare_draft_picks.R",
  paste0("--players='", players, "'"),
  paste0("--n_sims=", n_sims)
)

system(cmd)