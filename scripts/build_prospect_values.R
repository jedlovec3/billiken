#!/usr/bin/env Rscript

# scripts/build_prospect_values.R
# Build consensus prospect values from cached FanGraphs and MLB ranking inputs.

suppressPackageStartupMessages({
  library(tidyverse)
})

source("scripts/prospect_value_utils.R")

CURRENT_YEAR <- as.integer(Sys.getenv("BILLIKEN_PROJECTIONS_YEAR",
                                      unset = format(Sys.Date(), "%Y")))

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(tibble())
  tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) tibble())
}

mlb_raw <- safe_read_csv("data/raw/prospects_mlb_latest.csv")
fg_raw <- safe_read_csv("data/raw/prospects_fangraphs_latest.csv")

prospect_values <- build_consensus_prospect_values(
  mlb_rankings = mlb_raw,
  fg_rankings = fg_raw,
  current_year = CURRENT_YEAR
)

# Future projection files are preserved as a source-confidence signal in v1.
# The projection-to-dollar path can be expanded later without changing this
# output schema.
future_files <- list.files(
  "data/raw",
  pattern = "^future_(hitter|pitcher)_projections_[0-9]{4}[.]csv$",
  full.names = TRUE
)

if (length(future_files) > 0 && nrow(prospect_values) > 0) {
  future_raw <- map_dfr(future_files, safe_read_csv)

  future_names <- if ("Name" %in% names(future_raw)) {
    future_raw %>%
      transmute(
        name_normalized = normalize_trade_name(Name),
        future_projection_source = "fangraphs_zips_future"
      ) %>%
      distinct(name_normalized, .keep_all = TRUE)
  } else {
    tibble(name_normalized = character(),
           future_projection_source = character())
  }

  prospect_values <- prospect_values %>%
    left_join(future_names, by = "name_normalized", suffix = c("", "_future")) %>%
    mutate(
      future_projection_source = coalesce(
        future_projection_source_future,
        future_projection_source
      ),
      prospect_value_source = if_else(
        !is.na(future_projection_source_future),
        paste0(prospect_value_source, "+zips_signal"),
        prospect_value_source
      )
    ) %>%
    select(-future_projection_source_future)
}

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(prospect_values, "data/processed/prospect_values.csv")

message(sprintf("Wrote data/processed/prospect_values.csv (%d rows)",
                nrow(prospect_values)))

invisible(prospect_values)
