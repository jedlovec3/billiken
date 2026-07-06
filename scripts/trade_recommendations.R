# scripts/trade_recommendations.R
#
# Phase 5 of the Trade Lab tooling: a greedy two-team trade matchmaker.
#
# For each ordered pair (my_team, partner_team), score every partner-team
# player as a candidate trade target and propose the minimal offer from
# my-team's assets that the partner would plausibly accept. "Minimal" =
# smallest dollar value going out of my side that still satisfies the
# partner's posture-weighted value of what they're receiving (plus a small
# premium so the deal feels worth doing from their side).
#
# Inputs:
#   data/processed/team_assets.csv      (per-player win_now / future / total)
#   data/processed/team_posture.csv     (per-team posture + finish + cap)
#   data/processed/draft_pick_values.csv (per-team next-season pick values)
#
# Output:
#   data/processed/trade_targets.csv
#     one row per accepted (my_team, partner_team, target_player) suggestion,
#     ranked per partner by my_value_delta desc.
#
# Algorithm (v1):
#   1. Compute posture-weighted asset valuation from each team's POV:
#        value_to_team = w_win_now(posture) * win_now_value
#                      + w_future (posture) * future_value
#      Picks contribute only to future_value (next-season auction surplus);
#      players' future_value already includes the Phase 2 drop-penalty
#      haircut, so no extra adjustment is needed.
#
#   2. For each (me, partner) pair:
#        a. Rank partner's players by `v_to_me - v_to_partner` desc — that's
#           the arbitrage from my side (they undervalue, I value highly).
#        b. For each candidate target (top N by arbitrage), greedy-build my
#           offer by sorting my own assets by `v_to_partner - v_to_me` desc
#           (give up assets that are worth more to them than to me first)
#           until incoming_to_partner >= target.v_to_partner + premium, up
#           to MAX_OFFER_SIZE assets.
#        c. Keep the trade iff my_net = target.v_to_me - sum(offer.v_to_me)
#           is strictly positive.
#
#   3. Per (my_team, partner_team), retain only the top TOP_N_PER_PARTNER
#      trades by `my_value_delta`.
#
# Conventions:
#   * Two-team trades only in v1. Data model leaves room for 3-team later.
#   * Only next-season picks are tradeable (per the constitution).
#   * Player contracts go with the player — no adjustment needed on receive.
#   * Drop-penalty haircut on long extensions is already encoded in
#     team_assets.future_value from build_team_assets.R Phase 2.
#
# Run:
#   Rscript scripts/trade_recommendations.R

suppressPackageStartupMessages({
  library(tidyverse)
})

source("scripts/prospect_value_utils.R")

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

CURRENT_YEAR <- as.integer(Sys.getenv("BILLIKEN_PROJECTIONS_YEAR",
                                      unset = format(Sys.Date(), "%Y")))
NEXT_YEAR <- CURRENT_YEAR + 1L

# Posture-driven weights. Higher w_win_now = a contender's POV; higher
# w_future = a rebuilder's POV. Tunable.
POSTURE_WEIGHTS <- tribble(
  ~posture,    ~w_win_now, ~w_future,
  "contender", 1.0,        0.3,
  "bubble",    0.8,        0.5,
  "mid",       0.6,        0.7,
  "rebuild",   0.0,        1.0
)

# Premium added to the partner's required incoming value so accepted offers
# beat "barely break-even" and feel worth proposing.
TRADE_PREMIUM         <- 1.0

# Caps to keep offers and output sizes reasonable.
MAX_OFFER_SIZE        <- 4L
TOP_N_CANDIDATE_TARGETS <- 20L
TOP_N_PER_PARTNER     <- 5L

# Don't bother proposing trades for sub-threshold targets (noise).
MIN_TARGET_VALUE_TO_ME <- 1.5

# Rebuilders often "lose" on posture-weighted arb vs contenders for the same
# prospect (partner weights win-now higher). Allow small negative arb and
# rank by v_to_me instead of get_arb for rebuild posture.
MIN_TARGET_ARB_REBUILD <- -2.0

# Don't offer assets the partner wouldn't actually take. An asset with
# v_to_partner <= 0 is a net liability to the receiver (e.g. a player on a
# bad contract with no future value), and no real GM accepts "a star plus a
# cap albatross" just because the math says the deltas add up. Filtering
# these out prevents the greedy from gaming the offer with dump pieces.
MIN_ASSET_V_TO_PARTNER <- 0.5

# Every accepted trade must include at least this many assets going from
# my side to the partner. Guards against "free target" trades that emerge
# when a partner-side target has negative v_to_partner (e.g. a star on a
# heavily underwater extension): the partner's `need` threshold goes
# negative, the greedy adds 0 assets, and the matchmaker proposes giving
# up literally nothing for the player. No real GM accepts "you take my
# bad contract and I give you nothing," even if dropping the player
# would cost them the drop penalty.
MIN_OFFER_SIZE <- 1L

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

repo_root <- if (file.exists("billiken.Rproj")) {
  getwd()
} else if (file.exists("../billiken.Rproj")) {
  normalizePath("..")
} else {
  getwd()
}
resolve_path <- function(p) file.path(repo_root, p)

assets_path  <- resolve_path("data/processed/team_assets.csv")
posture_path <- resolve_path("data/processed/team_posture.csv")
picks_path   <- resolve_path("data/processed/draft_pick_values.csv")

for (p in c(assets_path, posture_path, picks_path)) {
  if (!file.exists(p)) {
    stop(sprintf("Missing input: %s", p), call. = FALSE)
  }
}

# ---------------------------------------------------------------------------
# Load inputs
# ---------------------------------------------------------------------------

team_assets  <- read_csv(assets_path,  show_col_types = FALSE)
team_posture <- read_csv(posture_path, show_col_types = FALSE)
draft_picks  <- read_csv(picks_path,   show_col_types = FALSE)

for (col in c("prospect_value", "consensus_rank", "prospect_eta",
              "prospect_value_source", "future_projection_source")) {
  if (!col %in% names(team_assets)) team_assets[[col]] <- NA
}

# ---------------------------------------------------------------------------
# Per-team weights
# ---------------------------------------------------------------------------

team_weights <- team_posture %>%
  select(billikenTeam, team_name, posture, proj_finish) %>%
  left_join(POSTURE_WEIGHTS, by = "posture") %>%
  # Fall back to "mid" weights for any team with an unrecognised posture so
  # the matchmaker never silently drops a partner.
  mutate(
    w_win_now = coalesce(w_win_now, 0.6),
    w_future  = coalesce(w_future,  0.7)
  )

teams <- team_weights$billikenTeam

# ---------------------------------------------------------------------------
# Build asset universe: players + next-season picks
# ---------------------------------------------------------------------------

player_assets <- team_assets %>%
  filter(!is.na(billikenTeam)) %>%
  transmute(
    billikenTeam,
    asset_type   = if_else(coalesce(as.numeric(prospect_value), 0) > 0,
                           "prospect", "player"),
    asset_id     = Name,
    asset_label  = sprintf("%s (%s, %s, $%g, %s)",
                           Name,
                           coalesce(positions, ""),
                           coalesce(contract_status, ""),
                           coalesce(salary_2026, 0),
                           paste0(coalesce(years_remaining, 0L), "y")),
    win_now_value = coalesce(win_now_value, 0),
    future_value  = coalesce(future_value,  0),
    prospect_value = coalesce(as.numeric(prospect_value), 0),
    pick_value = 0,
    consensus_rank = suppressWarnings(as.numeric(consensus_rank)),
    prospect_eta = suppressWarnings(as.integer(prospect_eta)),
    prospect_value_source = as.character(prospect_value_source),
    future_projection_source = as.character(future_projection_source),
    is_expiring   = coalesce(is_expiring_after_2026, FALSE)
  )

pick_assets <- draft_picks %>%
  filter(season == NEXT_YEAR) %>%
  transmute(
    billikenTeam,
    asset_type   = "pick",
    asset_id     = sprintf("pick_%d_R%02d", season, round),
    asset_label  = sprintf("%d R%d pick", season, round),
    win_now_value = 0,                            # picks pay off in NEXT_YEAR
    future_value  = expected_dollar_value,
    prospect_value = 0,
    pick_value = expected_dollar_value,
    consensus_rank = NA_real_,
    prospect_eta = NA_integer_,
    prospect_value_source = NA_character_,
    future_projection_source = as.character(curve_source),
    is_expiring   = FALSE
  )

all_assets <- bind_rows(player_assets, pick_assets)

# Helper to compute team-perspective value for a data frame of assets.
attach_team_values <- function(assets_df, my_w, p_w) {
  assets_df %>%
    mutate(
      v_to_me      = my_w$w_win_now * win_now_value + my_w$w_future * future_value,
      v_to_partner = p_w$w_win_now  * win_now_value + p_w$w_future  * future_value
    )
}

# Partner players to consider as acquisition targets (posture-aware).
select_targets <- function(partner_priced, my_posture) {
  select_trade_targets_for_posture(
    partner_priced,
    my_posture,
    min_target_value = MIN_TARGET_VALUE_TO_ME,
    min_rebuild_arb = MIN_TARGET_ARB_REBUILD,
    top_n = TOP_N_CANDIDATE_TARGETS
  )
}

# Greedy offer for one target; returns a one-row tibble or NULL.
build_trade_row <- function(target, offer_pool, my_w, p_w, me, partner,
                            trade_quality = "strong") {
  need <- max(target$v_to_partner + TRADE_PREMIUM, TRADE_PREMIUM)

  offer_rows          <- list()
  incoming_to_partner <- 0
  outgoing_to_me      <- 0

  for (j in seq_len(nrow(offer_pool))) {
    if (incoming_to_partner >= need &&
        length(offer_rows) >= MIN_OFFER_SIZE) break
    if (length(offer_rows) >= MAX_OFFER_SIZE) break

    a <- offer_pool[j, ]
    offer_rows[[length(offer_rows) + 1]] <- a
    incoming_to_partner <- incoming_to_partner + a$v_to_partner
    outgoing_to_me      <- outgoing_to_me      + a$v_to_me
  }

  if (length(offer_rows) < MIN_OFFER_SIZE) return(NULL)
  if (incoming_to_partner < need) return(NULL)

  offer_df            <- bind_rows(offer_rows)
  my_value_delta      <- target$v_to_me      - outgoing_to_me
  partner_value_delta <- incoming_to_partner - target$v_to_partner

  offer_win_now  <- sum(offer_df$win_now_value)
  offer_future   <- sum(offer_df$future_value)
  my_win_now_delta    <- target$win_now_value - offer_win_now
  my_future_delta     <- target$future_value  - offer_future
  # Contender should gain win-now from the swap (veterans in, win-now out).
  partner_win_now_delta <- offer_win_now - target$win_now_value

  if (trade_quality == "strong" && my_value_delta <= 0) return(NULL)

  # Rebuild: require positive future-value net; don't dump future for win-now.
  if (my_w$posture == "rebuild") {
    if (my_future_delta <= 0) return(NULL)
    if (p_w$posture %in% c("contender", "bubble") && partner_win_now_delta <= 0) {
      return(NULL)
    }
  }

  # Contender initiating: require positive win-now net.
  if (my_w$posture %in% c("contender", "bubble")) {
    if (my_win_now_delta <= 0) return(NULL)
  }

  tibble(
    my_team               = me,
    my_posture            = my_w$posture,
    partner_team          = partner,
    partner_posture       = p_w$posture,
    target_player         = target$asset_id,
    target_asset_type     = target$asset_type,
    target_asset_label    = target$asset_label,
    target_v_to_me        = target$v_to_me,
    target_v_to_partner   = target$v_to_partner,
    target_prospect_value = coalesce(target$prospect_value, 0),
    target_pick_value     = coalesce(target$pick_value, 0),
    proposed_offer        = paste(offer_df$asset_label, collapse = "|"),
    proposed_offer_ids    = paste(offer_df$asset_id,    collapse = "|"),
    offer_size            = nrow(offer_df),
    offer_v_to_partner    = incoming_to_partner,
    offer_v_to_me         = outgoing_to_me,
    my_value_delta        = my_value_delta,
    partner_value_delta   = partner_value_delta,
    my_win_now_delta      = my_win_now_delta,
    my_future_delta       = my_future_delta,
    partner_win_now_delta = partner_win_now_delta,
    trade_quality         = trade_quality,
    notes                 = sprintf(
      "%s receives %s from %s; postures %s/%s; %s",
      me, target$asset_id, partner, my_w$posture, p_w$posture, trade_quality
    )
  )
}

# ---------------------------------------------------------------------------
# Main loop: per (me, partner) pair, propose offers for top arbitrage targets
# ---------------------------------------------------------------------------

results <- list()

for (me in teams) {
  my_w <- team_weights %>% filter(billikenTeam == me)
  if (nrow(my_w) == 0) next

  my_assets_raw <- all_assets %>% filter(billikenTeam == me)

  for (partner in setdiff(teams, me)) {
    p_w <- team_weights %>% filter(billikenTeam == partner)
    if (nrow(p_w) == 0) next

    partner_assets_raw <- all_assets %>% filter(billikenTeam == partner)

    my_priced      <- attach_team_values(my_assets_raw,      my_w, p_w)
    partner_priced <- attach_team_values(partner_assets_raw, my_w, p_w)

    # Pre-sort my offer pool: send them stuff worth more to them than to me,
    # but only include assets the partner would actually accept (positive
    # value to them). Drops cap albatrosses out of the candidate pool.
    offer_pool <- my_priced %>%
      filter(v_to_partner >= MIN_ASSET_V_TO_PARTNER) %>%
      mutate(give_arb = v_to_partner - v_to_me)

    # Rebuild selling to contenders: lead with expiring vets (win-now, no future).
    if (my_w$posture == "rebuild" &&
        p_w$posture %in% c("contender", "bubble")) {
      offer_pool <- offer_pool %>%
        arrange(
          desc(coalesce(is_expiring, FALSE)),
          desc(win_now_value / pmax(future_value, 0.25)),
          desc(give_arb)
        )
    } else {
      offer_pool <- offer_pool %>% arrange(desc(give_arb))
    }

    targets <- select_targets(partner_priced, my_w$posture)
    if (nrow(targets) == 0) next

    pair_trades <- list()

    for (i in seq_len(nrow(targets))) {
      target <- targets[i, ]
      row <- build_trade_row(
        target, offer_pool, my_w, p_w, me, partner,
        trade_quality = "strong"
      )
      if (!is.null(row)) pair_trades[[length(pair_trades) + 1]] <- row
    }

    # Stretch pass: only for non-rebuild (rebuild trades must clear future delta).
    if (my_w$posture != "rebuild" &&
        length(pair_trades) < TOP_N_PER_PARTNER && nrow(offer_pool) > 0) {
      existing_targets <- if (length(pair_trades) > 0) {
        map_chr(pair_trades, ~ .x$target_player)
      } else {
        character(0)
      }
      stretch_targets <- targets %>%
        filter(!asset_id %in% existing_targets)

      for (i in seq_len(nrow(stretch_targets))) {
        if (length(pair_trades) >= TOP_N_PER_PARTNER) break
        target <- stretch_targets[i, ]
        row <- build_trade_row(
          target, offer_pool, my_w, p_w, me, partner,
          trade_quality = "stretch"
        )
        if (!is.null(row)) pair_trades[[length(pair_trades) + 1]] <- row
      }
    }

    # Last resort (non-rebuild only): top v_to_me targets with any valid offer.
    if (my_w$posture != "rebuild" &&
        length(pair_trades) == 0 && nrow(offer_pool) > 0) {
      fallback_targets <- partner_priced %>%
        filter(asset_type == "player", v_to_me >= MIN_TARGET_VALUE_TO_ME) %>%
        arrange(desc(v_to_me)) %>%
        head(TOP_N_PER_PARTNER)

      for (i in seq_len(nrow(fallback_targets))) {
        target <- fallback_targets[i, ]
        row <- build_trade_row(
          target, offer_pool, my_w, p_w, me, partner,
          trade_quality = "fallback"
        )
        if (!is.null(row)) pair_trades[[length(pair_trades) + 1]] <- row
      }
    }

    if (length(pair_trades) > 0) {
      results <- c(results, pair_trades)
    }
  }
}

trade_targets <- bind_rows(results)

if (nrow(trade_targets) == 0) {
  message("No qualifying trades found; writing empty file.")
} else {
  trade_targets <- trade_targets %>%
    group_by(my_team, partner_team) %>%
    arrange(
      desc(trade_quality == "strong"),
      desc(trade_quality == "stretch"),
      desc(coalesce(my_future_delta, my_value_delta)),
      desc(my_value_delta)
    ) %>%
    slice_head(n = TOP_N_PER_PARTNER) %>%
    ungroup() %>%
    arrange(my_team, partner_team, desc(coalesce(my_future_delta, my_value_delta)))
}

# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------

out_path <- resolve_path("data/processed/trade_targets.csv")
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write_csv(trade_targets, out_path)

# ---------------------------------------------------------------------------
# Console summary
# ---------------------------------------------------------------------------

message(sprintf("Wrote %s", out_path))
message(sprintf("  rows: %d", nrow(trade_targets)))

if (nrow(trade_targets) > 0) {
  message("\nTop suggestions across all partners:")
  trade_targets %>%
    arrange(desc(my_value_delta)) %>%
    head(15) %>%
    transmute(
      line = sprintf("  %-22s -> %-22s  get=%-25s  my_d=%+5.1f  prt_d=%+5.1f",
                     my_team, partner_team,
                     str_trunc(target_player, 25),
                     my_value_delta,
                     partner_value_delta)
    ) %>%
    pull(line) %>%
    walk(message)
}
