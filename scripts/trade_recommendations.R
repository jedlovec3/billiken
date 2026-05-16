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
  "rebuild",   0.2,        1.0
)

# Premium added to the partner's required incoming value so accepted offers
# beat "barely break-even" and feel worth proposing.
TRADE_PREMIUM         <- 1.0

# Caps to keep offers and output sizes reasonable.
MAX_OFFER_SIZE        <- 4L
TOP_N_CANDIDATE_TARGETS <- 20L
TOP_N_PER_PARTNER     <- 5L

# Don't bother proposing trades for sub-threshold targets (noise).
MIN_TARGET_VALUE_TO_ME <- 3.0

# Don't offer assets the partner wouldn't actually take. An asset with
# v_to_partner <= 0 is a net liability to the receiver (e.g. a player on a
# bad contract with no future value), and no real GM accepts "a star plus a
# cap albatross" just because the math says the deltas add up. Filtering
# these out prevents the greedy from gaming the offer with dump pieces.
MIN_ASSET_V_TO_PARTNER <- 0.5

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
    asset_type   = "player",
    asset_id     = Name,
    asset_label  = sprintf("%s (%s, %s, $%g, %s)",
                           Name,
                           coalesce(positions, ""),
                           coalesce(contract_status, ""),
                           coalesce(salary_2026, 0),
                           paste0(coalesce(years_remaining, 0L), "y")),
    win_now_value = coalesce(win_now_value, 0),
    future_value  = coalesce(future_value,  0)
  )

pick_assets <- draft_picks %>%
  filter(season == NEXT_YEAR) %>%
  transmute(
    billikenTeam,
    asset_type   = "pick",
    asset_id     = sprintf("pick_%d_R%02d", season, round),
    asset_label  = sprintf("%d R%d pick", season, round),
    win_now_value = 0,                            # picks pay off in NEXT_YEAR
    future_value  = expected_dollar_value
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
      mutate(give_arb = v_to_partner - v_to_me) %>%
      arrange(desc(give_arb))

    # Targets are partner's players that I value strictly more than they do.
    targets <- partner_priced %>%
      filter(asset_type == "player") %>%
      mutate(get_arb = v_to_me - v_to_partner) %>%
      filter(get_arb > 0, v_to_me >= MIN_TARGET_VALUE_TO_ME) %>%
      arrange(desc(get_arb)) %>%
      head(TOP_N_CANDIDATE_TARGETS)

    if (nrow(targets) == 0) next

    for (i in seq_len(nrow(targets))) {
      target <- targets[i, ]
      need   <- target$v_to_partner + TRADE_PREMIUM

      offer_rows         <- list()
      incoming_to_partner <- 0
      outgoing_to_me      <- 0

      for (j in seq_len(nrow(offer_pool))) {
        if (incoming_to_partner >= need) break
        if (length(offer_rows) >= MAX_OFFER_SIZE) break

        a <- offer_pool[j, ]
        offer_rows[[length(offer_rows) + 1]] <- a
        incoming_to_partner <- incoming_to_partner + a$v_to_partner
        outgoing_to_me      <- outgoing_to_me      + a$v_to_me
      }

      if (incoming_to_partner < need) next

      offer_df            <- bind_rows(offer_rows)
      my_value_delta      <- target$v_to_me      - outgoing_to_me
      partner_value_delta <- incoming_to_partner - target$v_to_partner

      if (my_value_delta <= 0) next

      results[[length(results) + 1]] <- tibble(
        my_team               = me,
        my_posture            = my_w$posture,
        partner_team          = partner,
        partner_posture       = p_w$posture,
        target_player         = target$asset_id,
        target_v_to_me        = target$v_to_me,
        target_v_to_partner   = target$v_to_partner,
        proposed_offer        = paste(offer_df$asset_label, collapse = "|"),
        proposed_offer_ids    = paste(offer_df$asset_id,    collapse = "|"),
        offer_size            = nrow(offer_df),
        offer_v_to_partner    = incoming_to_partner,
        offer_v_to_me         = outgoing_to_me,
        my_value_delta        = my_value_delta,
        partner_value_delta   = partner_value_delta,
        notes                 = sprintf("%s receives %s from %s; postures %s/%s",
                                        me, target$asset_id, partner,
                                        my_w$posture, p_w$posture)
      )
    }
  }
}

trade_targets <- bind_rows(results)

if (nrow(trade_targets) == 0) {
  message("No qualifying trades found; writing empty file.")
} else {
  trade_targets <- trade_targets %>%
    group_by(my_team, partner_team) %>%
    slice_max(my_value_delta, n = TOP_N_PER_PARTNER, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(my_team, partner_team, desc(my_value_delta))
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
