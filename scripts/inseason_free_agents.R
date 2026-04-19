# scripts/inseason_free_agents.R
# Compute in-season "best available" free agents from FanGraphs ROS projections.
#
# A free agent = a player in the FanGraphs ROS projections (NL-filtered) who is
# NOT currently on any Billiken fantasy team roster.
#
# Each player is scored using the same unit-value based SGP formulas as
# scripts/calculate_player_sgp.R, but using ROS stats instead of full-season.
#
# Positional eligibility is joined from data/raw/positions_latest.csv when
# available (the file is produced by scripts/fetch_espn_positions.R).

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringi)
  library(fuzzyjoin)
})

# ---------------------------------------------------------------------------
# Position eligibility loader
# ---------------------------------------------------------------------------
# positions_latest.csv schema:
#   PLAYER, C, 1B, 2B, 3B, SS, LF, CF, RF, DH, SP, RP  (all 0/1)
#
# Returns tibble(name_normalized, positions) where `positions` is a
# comma-separated string of eligible labels (e.g. "1B,OF,DH"). OF is added
# whenever any of LF/CF/RF is eligible.
load_position_eligibility <- function(
  path = "data/raw/positions_latest.csv",
  normalize_fn
) {
  if (!file.exists(path)) return(NULL)

  pos <- tryCatch(
    suppressMessages(read_csv(path, show_col_types = FALSE)),
    error = function(e) NULL
  )
  if (is.null(pos) || nrow(pos) == 0) return(NULL)

  pos_long <- pos %>%
    pivot_longer(
      cols = -PLAYER,
      names_to = "pos",
      values_to = "eligible"
    ) %>%
    filter(!is.na(eligible), eligible == 1)

  # Collapse LF/CF/RF -> OF (the Billiken league uses OF, not corner/center
  # splits) but preserve the individual labels too for future filtering.
  pos_long <- pos_long %>%
    mutate(
      pos = if_else(pos %in% c("LF", "CF", "RF"), "OF", pos)
    ) %>%
    distinct(PLAYER, pos)

  # Join separator is '|' rather than ',' so the resulting CSV stays safe
  # for the naive comma-split parser used by server.js::csvToJson.
  pos_long %>%
    mutate(name_normalized = normalize_fn(PLAYER)) %>%
    group_by(name_normalized) %>%
    summarize(
      positions = paste(sort(unique(pos)), collapse = "|"),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# Unit values loader
# ---------------------------------------------------------------------------
# Returns list(uv = named-vector, repl_AVG, repl_ERA, repl_WHIP, source)
# where source is "unit_values" when the CSVs were found and "fallback"
# otherwise.
load_unit_values <- function(
  uv_path = "data/processed/category_unit_values.csv",
  scale_path = "data/processed/category_value_scaling.csv"
) {
  if (!file.exists(uv_path) || !file.exists(scale_path)) {
    return(list(
      uv = NULL, repl_AVG = NA_real_, repl_ERA = NA_real_,
      repl_WHIP = NA_real_, source = "fallback"
    ))
  }

  uv_df <- tryCatch(
    suppressMessages(read_csv(uv_path, show_col_types = FALSE)),
    error = function(e) NULL
  )
  scale_df <- tryCatch(
    suppressMessages(read_csv(scale_path, show_col_types = FALSE)),
    error = function(e) NULL
  )
  if (is.null(uv_df) || is.null(scale_df)) {
    return(list(
      uv = NULL, repl_AVG = NA_real_, repl_ERA = NA_real_,
      repl_WHIP = NA_real_, source = "fallback"
    ))
  }

  uv <- setNames(uv_df$`Unit Value`, uv_df$Category)
  repl_AVG  <- scale_df %>% filter(Category == "AVG")  %>% pull(`Replacement Value`)
  repl_ERA  <- scale_df %>% filter(Category == "ERA")  %>% pull(`Replacement Value`)
  repl_WHIP <- scale_df %>% filter(Category == "WHIP") %>% pull(`Replacement Value`)

  list(
    uv = uv,
    repl_AVG  = if (length(repl_AVG)  == 1) repl_AVG  else 0.260,
    repl_ERA  = if (length(repl_ERA)  == 1) repl_ERA  else 4.50,
    repl_WHIP = if (length(repl_WHIP) == 1) repl_WHIP else 1.35,
    source = "unit_values"
  )
}

# ---------------------------------------------------------------------------
# SGP helpers
# ---------------------------------------------------------------------------
.compute_hitter_sgp <- function(df, uv, repl_AVG) {
  df %>%
    mutate(
      sgp_R   = coalesce(R,   0) * uv["R"],
      sgp_HR  = coalesce(HR,  0) * uv["HR"],
      sgp_RBI = coalesce(RBI, 0) * uv["RBI"],
      sgp_SB  = coalesce(SB,  0) * uv["SB"],
      sgp_AVG = (coalesce(AVG, repl_AVG) - repl_AVG) *
                coalesce(AB, 0) * uv["AVG"],
      sgp_hitting  = sgp_R + sgp_HR + sgp_RBI + sgp_SB + sgp_AVG,
      sgp_pitching = 0,
      sgp_total    = sgp_hitting
    )
}

.compute_pitcher_sgp <- function(df, uv, repl_ERA, repl_WHIP) {
  df %>%
    mutate(
      sgp_W  = coalesce(W,  0) * uv["W"],
      sgp_SV = coalesce(SV, 0) * uv["SV"],
      sgp_SO = coalesce(SO, 0) * uv["SO"],
      sgp_ERA  = (repl_ERA  - coalesce(ERA,  repl_ERA))  *
                 coalesce(IP, 0) / 9 * uv["ERA"],
      sgp_WHIP = (repl_WHIP - coalesce(WHIP, repl_WHIP)) *
                 coalesce(IP, 0)     * uv["WHIP"],
      sgp_pitching = sgp_W + sgp_SV + sgp_SO + sgp_ERA + sgp_WHIP,
      sgp_hitting  = 0,
      sgp_total    = sgp_pitching
    )
}

# Fallback ranking when unit values aren't available: mean of per-column
# z-scores (higher is better). For pitchers ERA/WHIP are flipped.
.compute_fallback_score <- function(df, cols, invert = character(0)) {
  z <- map_dfc(cols, function(col) {
    v <- df[[col]]
    if (is.null(v)) return(tibble(!!col := rep(0, nrow(df))))
    mu <- mean(v, na.rm = TRUE)
    sd_ <- sd(v, na.rm = TRUE)
    if (is.na(sd_) || sd_ == 0) return(tibble(!!col := rep(0, nrow(df))))
    z <- (v - mu) / sd_
    if (col %in% invert) z <- -z
    tibble(!!col := replace_na(z, 0))
  })
  rowMeans(as.matrix(z), na.rm = TRUE)
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------
#' Compute in-season free-agent rankings.
#'
#' @param ros_hitters  Tibble of FanGraphs ROS hitter projections. Must contain
#'   columns: Name, Team, name_normalized, AB, H, R, HR, RBI, SB, AVG (AVG
#'   optional; will be derived from H/AB if missing).
#' @param ros_pitchers Tibble of FanGraphs ROS pitcher projections. Must
#'   contain: Name, Team, name_normalized, IP, W, SV, SO, ERA, WHIP (ER/HA/BB
#'   optional). See inseason_update.R for the standard renaming (K->SO, H->HA).
#' @param rostered_names_normalized Character vector of normalized names that
#'   are currently on a Billiken roster (excluded from the output).
#' @param positions_path Path to positions_latest.csv; set NULL to skip.
#' @param normalize_fn Name normalization function. Must match the one used on
#'   ros_*$name_normalized and rostered_names_normalized.
#' @return list with $free_agents tibble, $source ("unit_values"|"fallback"),
#'   $n_free_agents integer.
compute_inseason_free_agents <- function(
  ros_hitters,
  ros_pitchers,
  rostered_names_normalized,
  positions_path = "data/raw/positions_latest.csv",
  normalize_fn = NULL
) {
  if (is.null(normalize_fn)) {
    normalize_fn <- function(name) {
      name %>%
        stri_trans_general("Latin-ASCII") %>%
        str_replace_all(" Jr\\.?$", "") %>%
        str_replace_all(" Sr\\.?$", "") %>%
        str_replace_all(" III$",    "") %>%
        str_replace_all(" II$",     "") %>%
        str_trim()
    }
  }

  rostered <- unique(as.character(rostered_names_normalized))

  # Ensure expected columns exist on the ROS frames; fill missing with NA so
  # the SGP helpers don't blow up if FanGraphs changes a column name.
  ensure_cols <- function(df, cols) {
    for (c in cols) if (!c %in% names(df)) df[[c]] <- NA_real_
    df
  }
  ros_hitters  <- ensure_cols(ros_hitters,
    c("AB", "H", "R", "HR", "RBI", "SB", "AVG"))
  ros_pitchers <- ensure_cols(ros_pitchers,
    c("IP", "W", "SV", "SO", "ERA", "WHIP"))

  # Derive AVG from H/AB when missing
  ros_hitters <- ros_hitters %>%
    mutate(
      AVG = if_else(
        is.na(AVG) & !is.na(H) & !is.na(AB) & AB > 0,
        H / AB, AVG
      )
    )

  # Free-agent filter
  fa_hitters  <- ros_hitters  %>% filter(!name_normalized %in% rostered)
  fa_pitchers <- ros_pitchers %>% filter(!name_normalized %in% rostered)

  uv_info <- load_unit_values()
  source  <- uv_info$source

  # ---- Compute SGP (or fallback) ----
  if (source == "unit_values") {
    uv        <- uv_info$uv
    repl_AVG  <- uv_info$repl_AVG
    repl_ERA  <- uv_info$repl_ERA
    repl_WHIP <- uv_info$repl_WHIP

    fa_hitters_scored <- .compute_hitter_sgp(fa_hitters, uv, repl_AVG) %>%
      mutate(player_type = "hitter")
    fa_pitchers_scored <- .compute_pitcher_sgp(
      fa_pitchers, uv, repl_ERA, repl_WHIP
    ) %>%
      mutate(player_type = "pitcher")
  } else {
    message("WARNING: category_unit_values.csv missing; ",
            "using z-score fallback for free-agent rankings.")
    fa_hitters_scored <- fa_hitters %>%
      mutate(
        player_type = "hitter",
        sgp_R = NA_real_, sgp_HR = NA_real_, sgp_RBI = NA_real_,
        sgp_SB = NA_real_, sgp_AVG = NA_real_,
        sgp_hitting  = .compute_fallback_score(
          ., c("R", "HR", "RBI", "SB", "AVG")
        ),
        sgp_pitching = 0,
        sgp_total    = sgp_hitting
      )
    fa_pitchers_scored <- fa_pitchers %>%
      mutate(
        player_type = "pitcher",
        sgp_W = NA_real_, sgp_SV = NA_real_, sgp_SO = NA_real_,
        sgp_ERA = NA_real_, sgp_WHIP = NA_real_,
        sgp_pitching = .compute_fallback_score(
          ., c("W", "SV", "SO", "ERA", "WHIP"),
          invert = c("ERA", "WHIP")
        ),
        sgp_hitting  = 0,
        sgp_total    = sgp_pitching
      )
  }

  # ---- Harmonize columns ----
  hitter_cols <- c("Name", "Team", "name_normalized", "player_type",
                   "AB", "H", "R", "HR", "RBI", "SB", "AVG",
                   "sgp_R", "sgp_HR", "sgp_RBI", "sgp_SB", "sgp_AVG",
                   "sgp_hitting", "sgp_pitching", "sgp_total")
  pitcher_cols <- c("Name", "Team", "name_normalized", "player_type",
                    "IP", "W", "SV", "SO", "ERA", "WHIP",
                    "sgp_W", "sgp_SV", "sgp_SO", "sgp_ERA", "sgp_WHIP",
                    "sgp_hitting", "sgp_pitching", "sgp_total")

  hitters_out <- fa_hitters_scored %>%
    select(any_of(hitter_cols)) %>%
    mutate(
      IP = NA_real_, W = NA_real_, SV = NA_real_, SO = NA_real_,
      ERA = NA_real_, WHIP = NA_real_
    )
  pitchers_out <- fa_pitchers_scored %>%
    select(any_of(pitcher_cols)) %>%
    mutate(
      AB = NA_real_, H = NA_real_, R = NA_real_, HR = NA_real_,
      RBI = NA_real_, SB = NA_real_, AVG = NA_real_
    )

  combined <- bind_rows(hitters_out, pitchers_out)

  # ---- Position eligibility ----
  pos_df <- load_position_eligibility(positions_path, normalize_fn)
  if (!is.null(pos_df)) {
    combined <- combined %>%
      left_join(pos_df, by = "name_normalized") %>%
      # Default position: hitter -> "UTIL" if unknown; pitcher -> "P" if unknown
      mutate(
        positions = case_when(
          !is.na(positions) ~ positions,
          player_type == "hitter"  ~ NA_character_,
          player_type == "pitcher" ~ NA_character_,
          TRUE ~ NA_character_
        )
      )
  } else {
    combined <- combined %>% mutate(positions = NA_character_)
  }

  # ---- Ranks ----
  combined <- combined %>%
    arrange(desc(sgp_total)) %>%
    mutate(rank_overall = row_number()) %>%
    group_by(player_type) %>%
    mutate(rank_by_type = row_number()) %>%
    ungroup()

  # Final column order
  final_cols <- c(
    "rank_overall", "rank_by_type", "player_type",
    "Name", "Team", "positions",
    "AB", "H", "R", "HR", "RBI", "SB", "AVG",
    "IP", "W", "SV", "SO", "ERA", "WHIP",
    "sgp_R", "sgp_HR", "sgp_RBI", "sgp_SB", "sgp_AVG",
    "sgp_W", "sgp_SV", "sgp_SO", "sgp_ERA", "sgp_WHIP",
    "sgp_hitting", "sgp_pitching", "sgp_total"
  )
  present <- intersect(final_cols, names(combined))
  combined <- combined %>% select(all_of(present))

  list(
    free_agents   = combined,
    source        = source,
    n_free_agents = nrow(combined)
  )
}

# ---------------------------------------------------------------------------
# Score rostered players too (used to attach sgp_total to team_details.csv).
# Exposed so inseason_update.R can keep a single place for SGP scoring.
# ---------------------------------------------------------------------------
#' @param roster_hitters  output of the rosters-x-ROS join in inseason_update.R
#' @param roster_pitchers output of the rosters-x-ROS join in inseason_update.R
#' @return list($hitters, $pitchers) with added sgp_* columns + source.
score_rostered_players <- function(roster_hitters, roster_pitchers) {
  uv_info <- load_unit_values()

  ensure_cols <- function(df, cols) {
    for (c in cols) if (!c %in% names(df)) df[[c]] <- NA_real_
    df
  }
  roster_hitters  <- ensure_cols(roster_hitters,
    c("AB", "H", "R", "HR", "RBI", "SB", "AVG"))
  roster_pitchers <- ensure_cols(roster_pitchers,
    c("IP", "W", "SV", "SO", "ERA", "WHIP"))

  roster_hitters <- roster_hitters %>%
    mutate(
      AVG = if_else(
        is.na(AVG) & !is.na(H) & !is.na(AB) & AB > 0,
        H / AB, AVG
      )
    )

  if (uv_info$source == "unit_values") {
    hit <- .compute_hitter_sgp(
      roster_hitters, uv_info$uv, uv_info$repl_AVG
    )
    pit <- .compute_pitcher_sgp(
      roster_pitchers, uv_info$uv, uv_info$repl_ERA, uv_info$repl_WHIP
    )
  } else {
    hit <- roster_hitters %>%
      mutate(
        sgp_hitting  = .compute_fallback_score(
          ., c("R", "HR", "RBI", "SB", "AVG")
        ),
        sgp_pitching = 0,
        sgp_total    = sgp_hitting
      )
    pit <- roster_pitchers %>%
      mutate(
        sgp_pitching = .compute_fallback_score(
          ., c("W", "SV", "SO", "ERA", "WHIP"),
          invert = c("ERA", "WHIP")
        ),
        sgp_hitting  = 0,
        sgp_total    = sgp_pitching
      )
  }

  list(hitters = hit, pitchers = pit, source = uv_info$source)
}
