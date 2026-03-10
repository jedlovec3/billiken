library(jsonlite)

args <- commandArgs(trailingOnly=TRUE)
draft_state_file <- args[1]

# run your projection pipeline
source("scripts/prefreeze_update.R")

# run draft comparison
system("Rscript scripts/compare_draft_picks.R --players='Bryce Harper,Bo Bichette,Freddie Freeman,Devin Williams,Konnor Griffin' --n_sims=200")

# return success
write_json(list(status="complete"), "api/sim_status.json")