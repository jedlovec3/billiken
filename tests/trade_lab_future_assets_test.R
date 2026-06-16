#!/usr/bin/env Rscript

Sys.setenv(RENV_CONFIG_AUTOLOADER_ENABLED = "false")

suppressPackageStartupMessages({
  library(tidyverse)
})

source("scripts/prospect_value_utils.R")
source("scripts/download_fangraphs_auction_values.R")
source("scripts/download_future_projections.R")
source("scripts/download_prospect_rankings.R")

assert_true <- function(x, message) {
  if (!isTRUE(x)) stop(message, call. = FALSE)
}

assert_equal <- function(actual, expected, message) {
  if (!identical(actual, expected)) {
    stop(sprintf(
      "%s\nactual:   %s\nexpected: %s",
      message,
      paste(capture.output(str(actual)), collapse = " "),
      paste(capture.output(str(expected)), collapse = " ")
    ), call. = FALSE)
  }
}

assert_near <- function(actual, expected, tolerance = 1e-6, message) {
  if (is.na(actual) || abs(actual - expected) > tolerance) {
    stop(sprintf("%s\nactual: %s expected: %s", message, actual, expected),
         call. = FALSE)
  }
}

test_eta_multiplier <- function() {
  assert_near(eta_multiplier(2026, current_year = 2026), 1.00,
              message = "2026 ETA should receive full value")
  assert_near(eta_multiplier(2027, current_year = 2026), 0.75,
              message = "2027 ETA should be discounted")
  assert_near(eta_multiplier(2028, current_year = 2026), 0.55,
              message = "2028 ETA should be discounted more")
  assert_near(eta_multiplier(2030, current_year = 2026), 0.35,
              message = "Long-range ETA should receive tail discount")
  assert_near(eta_multiplier(NA, current_year = 2026), 0.50,
              message = "Missing ETA should use neutral prospect discount")
}

test_consensus_values <- function() {
  mlb <- tibble(
    Name = c("Fast Arriver", "Far Away", "MLB Only"),
    source_rank = c(10, 10, 60),
    mlb_org = c("WSN", "WSN", "CHC"),
    position = c("OF", "OF", "SS"),
    level = c("AAA", "A", "AA"),
    eta = c(2026, 2028, 2027)
  )
  fg <- tibble(
    Name = c("Fast Arriver", "FV Only"),
    fg_rank = c(8, NA_real_),
    fg_fv = c(55, 50),
    fg_risk = c("Medium", "High"),
    mlb_org = c("WSN", "PIT"),
    position = c("OF", "P"),
    level = c("AAA", "AA"),
    eta = c(2026, 2027)
  )

  out <- build_consensus_prospect_values(mlb, fg, current_year = 2026)
  fast <- out %>% filter(Name == "Fast Arriver")
  far <- out %>% filter(Name == "Far Away")
  fv_only <- out %>% filter(Name == "FV Only")

  assert_near(fast$consensus_rank, 8.8, tolerance = 0.0001,
              message = "Consensus rank should blend FanGraphs and MLB ranks")
  assert_true(fast$prospect_value > far$prospect_value,
              "Same-ranked 2026 ETA prospect should be worth more than 2028 ETA")
  assert_true(!is.na(fv_only$consensus_rank),
              "FV-only FanGraphs prospects should map to a rank band")
  assert_true(all(c("prospect_value_2027", "prospect_value_2028",
                    "prospect_value_2029") %in% names(out)),
              "Prospect output should include yearly value stream columns")
}

test_future_asset_value <- function() {
  value <- calculate_future_asset_value(
    surplus_2027 = 10,
    surplus_2028 = 5,
    surplus_2029 = 0,
    surplus_2030 = 0,
    prospect_value = 8,
    drop_penalty_liability = 5,
    gamma = 0.7
  )
  assert_near(value, 12.45,
              message = "Future value should combine discounted surplus, prospect value, and drop penalty")
}

test_rebuild_targets_include_picks_and_prospects <- function() {
  assets <- tibble(
    asset_id = c("Veteran", "pick_2027_R01", "Prospect A"),
    asset_type = c("player", "pick", "prospect"),
    v_to_me = c(2, 22, 18),
    v_to_partner = c(1, 8, 7),
    future_value = c(2, 22, 18),
    prospect_value = c(0, 0, 18),
    pick_value = c(0, 22, 0)
  )

  targets <- select_trade_targets_for_posture(assets, "rebuild",
                                              min_target_value = 1.5,
                                              min_rebuild_arb = -2,
                                              top_n = 10)
  assert_true("pick_2027_R01" %in% targets$asset_id,
              "Rebuild targets should include draft picks")
  assert_true("Prospect A" %in% targets$asset_id,
              "Rebuild targets should include prospects")
}

test_auction_output_path <- function() {
  path <- auction_output_path(2026, "rfangraphsdc", "")
  assert_equal(path, file.path("data/raw", "auction_values_ros_2026.csv"),
               "ROS auction values should use the filename build_team_assets expects")
}

test_future_projection_specs <- function() {
  specs <- future_projection_specs(2026)
  expected <- tibble(
    season = c(2027L, 2027L, 2028L, 2028L),
    projection_type = c("zipsp1", "zipsp1", "zipsp2", "zipsp2"),
    stats = c("bat", "pit", "bat", "pit"),
    label = c("hitter", "pitcher", "hitter", "pitcher"),
    path = file.path(
      "data/raw",
      c(
        "future_hitter_projections_2027.csv",
        "future_pitcher_projections_2027.csv",
        "future_hitter_projections_2028.csv",
        "future_pitcher_projections_2028.csv"
      )
    )
  )
  assert_equal(specs, expected,
               "Future projection specs should cover 2027/2028 hitters and pitchers")
}

test_mlb_payload_parser <- function() {
  html <- paste0(
    '<span data-props="{&quot;apolloState&quot;:{',
    '&quot;ROOT_QUERY&quot;:{&quot;prospects({})&quot;:{&quot;__refs&quot;:[',
    '&quot;Prospect:1&quot;]}},',
    '&quot;Prospect:1&quot;:{&quot;__typename&quot;:&quot;Prospect&quot;,',
    '&quot;rank&quot;:12,&quot;eta&quot;:&quot;2026&quot;,&quot;level&quot;:&quot;AAA&quot;,',
    '&quot;team&quot;:{&quot;name&quot;:&quot;Washington Nationals&quot;,&quot;abbreviation&quot;:&quot;WSH&quot;},',
    '&quot;person&quot;:{&quot;__ref&quot;:&quot;Person:123&quot;}},',
    '&quot;Person:123&quot;:{&quot;__typename&quot;:&quot;Person&quot;,',
    '&quot;useName&quot;:&quot;Dylan&quot;,&quot;useLastName&quot;:&quot;Crews&quot;,',
    '&quot;primaryPosition&quot;:{&quot;abbreviation&quot;:&quot;OF&quot;},',
    '&quot;currentAge&quot;:24}}}"></span>'
  )

  out <- extract_mlb_prospects_from_html(html)
  assert_equal(out$Name[[1]], "Dylan Crews",
               "MLB parser should resolve prospect person references")
  assert_equal(out$source_rank[[1]], 12L,
               "MLB parser should read rank")
  assert_equal(out$eta[[1]], 2026L,
               "MLB parser should parse ETA as an integer")
  assert_equal(out$mlb_org[[1]], "WSH",
               "MLB parser should use team abbreviation")
}

tests <- list(
  eta_multiplier = test_eta_multiplier,
  consensus_values = test_consensus_values,
  future_asset_value = test_future_asset_value,
  rebuild_targets = test_rebuild_targets_include_picks_and_prospects,
  auction_output_path = test_auction_output_path,
  future_projection_specs = test_future_projection_specs,
  mlb_payload_parser = test_mlb_payload_parser
)

for (nm in names(tests)) {
  tests[[nm]]()
  cat(sprintf("PASS %s\n", nm))
}

cat("All future asset tests passed\n")
