# Shared helpers for Trade Lab future-asset valuation.

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringi)
})

normalize_trade_name <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("\u00A0", " ") %>%
    str_replace_all("[.]", "") %>%
    str_replace_all(",|\\s+(jr|sr|ii|iii|iv|v)\\.?$", "") %>%
    str_squish() %>%
    str_to_lower()
}

col_or <- function(df, candidates, default) {
  for (candidate in candidates) {
    if (candidate %in% names(df)) return(df[[candidate]])
  }
  rep(default, nrow(df))
}

eta_multiplier <- function(eta, current_year = as.integer(format(Sys.Date(), "%Y"))) {
  eta_num <- suppressWarnings(as.integer(eta))
  case_when(
    is.na(eta_num)                 ~ 0.50,
    eta_num <= current_year        ~ 1.00,
    eta_num == current_year + 1L   ~ 0.75,
    eta_num == current_year + 2L   ~ 0.55,
    TRUE                           ~ 0.35
  )
}

prospect_rank_value <- function(rank, top_value = 35, floor_value = 2,
                                decay = 0.035) {
  rank_num <- suppressWarnings(as.numeric(rank))
  ifelse(
    is.na(rank_num) | rank_num <= 0,
    NA_real_,
    floor_value + (top_value - floor_value) * exp(-decay * (rank_num - 1))
  )
}

fv_to_rank <- function(fv) {
  fv_num <- suppressWarnings(as.numeric(fv))
  case_when(
    is.na(fv_num)  ~ NA_real_,
    fv_num >= 70   ~ 1,
    fv_num >= 65   ~ 5,
    fv_num >= 60   ~ 12,
    fv_num >= 55   ~ 25,
    fv_num >= 50   ~ 55,
    fv_num >= 45   ~ 95,
    fv_num >= 40   ~ 150,
    TRUE           ~ NA_real_
  )
}

risk_multiplier <- function(risk) {
  r <- str_to_lower(str_squish(as.character(risk)))
  case_when(
    is.na(r) | r == ""                 ~ 0.90,
    str_detect(r, "low")               ~ 1.00,
    str_detect(r, "med|mod")           ~ 0.90,
    str_detect(r, "extreme|very high") ~ 0.60,
    str_detect(r, "high")              ~ 0.75,
    TRUE                               ~ 0.85
  )
}

allocate_prospect_value_by_eta <- function(value, eta,
                                           current_year = as.integer(format(Sys.Date(), "%Y"))) {
  eta_num <- suppressWarnings(as.integer(eta))
  value <- coalesce(as.numeric(value), 0)
  if (is.na(eta_num) || eta_num <= current_year + 1L) {
    return(c(value * 0.60, value * 0.25, value * 0.15))
  }
  if (eta_num == current_year + 2L) {
    return(c(0, value * 0.60, value * 0.40))
  }
  c(0, 0, value)
}

.standardize_mlb_prospects <- function(mlb_rankings) {
  if (is.null(mlb_rankings) || nrow(mlb_rankings) == 0) {
    return(tibble(
      name_normalized = character(),
      Name = character(),
      mlb_rank = numeric(),
      mlb_org_mlb = character(),
      position_mlb = character(),
      level_mlb = character(),
      eta_mlb = integer(),
      age_mlb = numeric()
    ))
  }

  df <- mlb_rankings
  names <- col_or(df, c("Name", "Player", "PlayerName"), NA_character_)

  tibble(
    name_normalized = normalize_trade_name(names),
    Name = str_squish(as.character(names)),
    mlb_rank = suppressWarnings(as.numeric(coalesce(
      col_or(df, c("mlb_rank"), NA_real_),
      col_or(df, c("source_rank"), NA_real_)
    ))),
    mlb_org_mlb = as.character(col_or(df, c("mlb_org"), NA_character_)),
    position_mlb = as.character(col_or(df, c("position"), NA_character_)),
    level_mlb = as.character(col_or(df, c("level"), NA_character_)),
    eta_mlb = suppressWarnings(as.integer(col_or(df, c("eta"), NA_integer_))),
    age_mlb = suppressWarnings(as.numeric(col_or(df, c("age"), NA_real_)))
  ) %>%
    filter(!is.na(name_normalized), name_normalized != "") %>%
    group_by(name_normalized) %>%
    slice_min(mlb_rank, n = 1, with_ties = FALSE) %>%
    ungroup()
}

.standardize_fg_prospects <- function(fg_rankings) {
  if (is.null(fg_rankings) || nrow(fg_rankings) == 0) {
    return(tibble(
      name_normalized = character(),
      Name_fg = character(),
      fg_rank = numeric(),
      fg_fv = numeric(),
      fg_risk = character(),
      mlb_org_fg = character(),
      position_fg = character(),
      level_fg = character(),
      eta_fg = integer(),
      age_fg = numeric()
    ))
  }

  df <- fg_rankings
  names <- col_or(df, c("Name", "Player", "PlayerName"), NA_character_)

  tibble(
    name_normalized = normalize_trade_name(names),
    Name_fg = str_squish(as.character(names)),
    fg_rank = suppressWarnings(as.numeric(coalesce(
      col_or(df, c("fg_rank"), NA_real_),
      col_or(df, c("source_rank"), NA_real_)
    ))),
    fg_fv = suppressWarnings(as.numeric(col_or(df, c("fg_fv"), NA_real_))),
    fg_risk = as.character(col_or(df, c("fg_risk"), NA_character_)),
    mlb_org_fg = as.character(col_or(df, c("mlb_org"), NA_character_)),
    position_fg = as.character(col_or(df, c("position"), NA_character_)),
    level_fg = as.character(col_or(df, c("level"), NA_character_)),
    eta_fg = suppressWarnings(as.integer(col_or(df, c("eta"), NA_integer_))),
    age_fg = suppressWarnings(as.numeric(col_or(df, c("age"), NA_real_)))
  ) %>%
    filter(!is.na(name_normalized), name_normalized != "") %>%
    mutate(fg_rank = coalesce(fg_rank, fv_to_rank(fg_fv))) %>%
    group_by(name_normalized) %>%
    arrange(is.na(fg_rank), fg_rank) %>%
    slice(1) %>%
    ungroup()
}

build_consensus_prospect_values <- function(mlb_rankings = tibble(),
                                            fg_rankings = tibble(),
                                            current_year = as.integer(format(Sys.Date(), "%Y"))) {
  mlb <- .standardize_mlb_prospects(mlb_rankings)
  fg <- .standardize_fg_prospects(fg_rankings)

  joined <- full_join(mlb, fg, by = "name_normalized") %>%
    mutate(
      Name = coalesce(Name, Name_fg),
      mlb_org = coalesce(mlb_org_fg, mlb_org_mlb),
      position = coalesce(position_fg, position_mlb),
      level = coalesce(level_fg, level_mlb),
      eta = coalesce(eta_fg, eta_mlb),
      age = coalesce(age_fg, age_mlb),
      consensus_rank = case_when(
        !is.na(fg_rank) & !is.na(mlb_rank) ~ 0.6 * fg_rank + 0.4 * mlb_rank,
        !is.na(fg_rank)                    ~ fg_rank,
        !is.na(mlb_rank)                   ~ mlb_rank,
        TRUE                               ~ NA_real_
      ),
      prospect_value_source = case_when(
        !is.na(fg_rank) & !is.na(mlb_rank) ~ "fangraphs_mlb_consensus",
        !is.na(fg_rank)                    ~ "fangraphs",
        !is.na(mlb_rank)                   ~ "mlb_pipeline",
        TRUE                               ~ "heuristic"
      ),
      rank_value = coalesce(prospect_rank_value(consensus_rank), 1.0),
      prospect_value = rank_value *
        eta_multiplier(eta, current_year = current_year) *
        risk_multiplier(fg_risk)
    )

  if (nrow(joined) == 0) {
    return(tibble(
      Name = character(), name_normalized = character(), mlb_org = character(),
      position = character(), level = character(), eta = integer(),
      age = numeric(), fg_rank = numeric(), fg_fv = numeric(),
      fg_risk = character(), mlb_rank = numeric(),
      consensus_rank = numeric(), future_projection_source = character(),
      prospect_value_2027 = numeric(), prospect_value_2028 = numeric(),
      prospect_value_2029 = numeric(), prospect_value = numeric(),
      prospect_value_source = character()
    ))
  }

  yearly <- pmap_dfr(
    list(joined$prospect_value, joined$eta),
    function(value, eta) {
      vals <- allocate_prospect_value_by_eta(value, eta, current_year)
      tibble(
        prospect_value_2027 = vals[[1]],
        prospect_value_2028 = vals[[2]],
        prospect_value_2029 = vals[[3]]
      )
    }
  )

  bind_cols(joined, yearly) %>%
    mutate(future_projection_source = NA_character_) %>%
    select(
      Name, name_normalized, mlb_org, position, level, eta, age,
      fg_rank, fg_fv, fg_risk, mlb_rank, consensus_rank,
      future_projection_source,
      prospect_value_2027, prospect_value_2028, prospect_value_2029,
      prospect_value, prospect_value_source
    ) %>%
    arrange(is.na(consensus_rank), consensus_rank, desc(prospect_value))
}

drop_penalty_liability <- function(contract_status, contract_end,
                                   current_year = as.integer(format(Sys.Date(), "%Y"))) {
  status <- str_to_lower(as.character(contract_status))
  end <- suppressWarnings(as.integer(contract_end))
  ifelse(
    status == "extended" & !is.na(end) & end > current_year,
    pmax(0, end - current_year) * 5,
    0
  )
}

calculate_ros_standings_value <- function(ros_sgp = NA_real_,
                                          full_sgpar = NA_real_,
                                          full_standings_value = NA_real_) {
  ros_sgp <- suppressWarnings(as.numeric(ros_sgp))
  full_sgpar <- suppressWarnings(as.numeric(full_sgpar))
  full_standings_value <- suppressWarnings(as.numeric(full_standings_value))

  ifelse(
    is.na(ros_sgp) | is.na(full_sgpar) | is.na(full_standings_value) |
      full_sgpar <= 0 | full_standings_value <= 0,
    NA_real_,
    full_standings_value * pmax(0, pmin(1, ros_sgp / full_sgpar))
  )
}

derive_dollars_per_sgpar <- function(player_values,
                                     categories = c("R", "RBI", "HR", "SB", "AVG",
                                                    "W", "SV", "SO", "ERA", "WHIP")) {
  out <- map_dbl(categories, function(cat) {
    sgp_col <- paste0("sgpar_", cat)
    dollar_col <- paste0("$_", cat)
    if (!all(c(sgp_col, dollar_col) %in% names(player_values))) return(NA_real_)

    sgp <- suppressWarnings(as.numeric(player_values[[sgp_col]]))
    dollars <- suppressWarnings(as.numeric(player_values[[dollar_col]]))
    ratios <- dollars / sgp
    ratios <- ratios[is.finite(ratios) & abs(sgp) > 1e-9]
    if (length(ratios) == 0) return(NA_real_)
    median(ratios, na.rm = TRUE)
  })
  names(out) <- categories
  out
}

calculate_ros_category_standings_value <- function(
  ros_sgp_R = NA_real_, ros_sgp_RBI = NA_real_, ros_sgp_HR = NA_real_,
  ros_sgp_SB = NA_real_, ros_sgp_AVG = NA_real_, ros_sgp_W = NA_real_,
  ros_sgp_SV = NA_real_, ros_sgp_SO = NA_real_, ros_sgp_ERA = NA_real_,
  ros_sgp_WHIP = NA_real_, dollars_per_sgpar
) {
  components <- tibble(
    R = suppressWarnings(as.numeric(ros_sgp_R)),
    RBI = suppressWarnings(as.numeric(ros_sgp_RBI)),
    HR = suppressWarnings(as.numeric(ros_sgp_HR)),
    SB = suppressWarnings(as.numeric(ros_sgp_SB)),
    AVG = suppressWarnings(as.numeric(ros_sgp_AVG)),
    W = suppressWarnings(as.numeric(ros_sgp_W)),
    SV = suppressWarnings(as.numeric(ros_sgp_SV)),
    SO = suppressWarnings(as.numeric(ros_sgp_SO)),
    ERA = suppressWarnings(as.numeric(ros_sgp_ERA)),
    WHIP = suppressWarnings(as.numeric(ros_sgp_WHIP))
  )

  categories <- names(components)
  rates <- suppressWarnings(as.numeric(dollars_per_sgpar[categories]))
  values <- sweep(as.matrix(components), 2, rates, `*`)
  rate_matrix <- matrix(rates, nrow = nrow(components), ncol = length(rates),
                        byrow = TRUE)
  present <- rowSums(!is.na(as.matrix(components)) & !is.na(rate_matrix)) > 0
  out <- rowSums(values, na.rm = TRUE)
  ifelse(present, out, NA_real_)
}

choose_win_now_value <- function(ros_standings_value = NA_real_,
                                 fg_ros_auction_dollars = NA_real_,
                                 win_now_surplus_sgp = NA_real_) {
  coalesce(
    suppressWarnings(as.numeric(ros_standings_value)),
    suppressWarnings(as.numeric(fg_ros_auction_dollars)),
    suppressWarnings(as.numeric(win_now_surplus_sgp)),
    0
  )
}

choose_win_now_value_source <- function(ros_standings_value = NA_real_,
                                        fg_ros_auction_dollars = NA_real_,
                                        win_now_surplus_sgp = NA_real_) {
  case_when(
    !is.na(suppressWarnings(as.numeric(ros_standings_value))) ~
      "ros_category_standings_value",
    !is.na(suppressWarnings(as.numeric(fg_ros_auction_dollars))) ~
      "fangraphs_ros_auction",
    !is.na(suppressWarnings(as.numeric(win_now_surplus_sgp))) ~
      "sgpar_surplus_fallback",
    TRUE ~ "zero_fallback"
  )
}

future_year_selected_value <- function(projection_value = 0, prospect_value = 0,
                                       salary = NA_real_) {
  projection_value <- coalesce(as.numeric(projection_value), 0)
  prospect_value <- coalesce(as.numeric(prospect_value), 0)
  salary <- suppressWarnings(as.numeric(salary))
  ifelse(is.na(salary), NA_real_, pmax(projection_value, prospect_value))
}

future_year_net_value <- function(projection_value = 0, prospect_value = 0,
                                  salary = NA_real_) {
  selected <- future_year_selected_value(projection_value, prospect_value, salary)
  salary <- suppressWarnings(as.numeric(salary))
  ifelse(is.na(salary), 0, selected - salary)
}

future_year_value_source <- function(projection_value = 0, prospect_value = 0,
                                     salary = NA_real_) {
  projection_value <- coalesce(as.numeric(projection_value), 0)
  prospect_value <- coalesce(as.numeric(prospect_value), 0)
  salary <- suppressWarnings(as.numeric(salary))
  case_when(
    is.na(salary) ~ "not_under_contract",
    projection_value <= 0 & prospect_value <= 0 ~ "none",
    prospect_value > projection_value ~ "prospect",
    TRUE ~ "projection"
  )
}

calculate_future_asset_value <- function(surplus_2027 = 0, surplus_2028 = 0,
                                         surplus_2029 = 0, surplus_2030 = 0,
                                         prospect_value = 0,
                                         projection_value_2027 = NULL,
                                         projection_value_2028 = NULL,
                                         projection_value_2029 = NULL,
                                         projection_value_2030 = NULL,
                                         prospect_value_2027 = NULL,
                                         prospect_value_2028 = NULL,
                                         prospect_value_2029 = NULL,
                                         prospect_value_2030 = 0,
                                         salary_2027 = NULL,
                                         salary_2028 = NULL,
                                         salary_2029 = NULL,
                                         salary_2030 = NULL,
                                         drop_penalty_liability = 0,
                                         gamma = 0.7) {
  if (!is.null(projection_value_2027) || !is.null(salary_2027) ||
      !is.null(prospect_value_2027)) {
    net_2027 <- future_year_net_value(projection_value_2027, prospect_value_2027, salary_2027)
    net_2028 <- future_year_net_value(projection_value_2028, prospect_value_2028, salary_2028)
    net_2029 <- future_year_net_value(projection_value_2029, prospect_value_2029, salary_2029)
    net_2030 <- future_year_net_value(projection_value_2030, prospect_value_2030, salary_2030)

    return(
      net_2027 * gamma +
        net_2028 * gamma^2 +
        net_2029 * gamma^3 +
        net_2030 * gamma^4 -
        coalesce(as.numeric(drop_penalty_liability), 0)
    )
  }

  coalesce(as.numeric(surplus_2027), 0) * gamma +
    coalesce(as.numeric(surplus_2028), 0) * gamma^2 +
    coalesce(as.numeric(surplus_2029), 0) * gamma^3 +
    coalesce(as.numeric(surplus_2030), 0) * gamma^4 +
    coalesce(as.numeric(prospect_value), 0) -
    coalesce(as.numeric(drop_penalty_liability), 0)
}

build_effective_team_weights <- function(team_posture, posture_weights) {
  valid_postures <- as.character(posture_weights$posture)

  actual_teams <- team_posture %>%
    mutate(
      actual_posture = if_else(
        as.character(posture) %in% valid_postures,
        as.character(posture),
        "mid"
      )
    ) %>%
    select(-posture)

  effective_weights <- posture_weights %>%
    transmute(
      effective_posture = as.character(posture),
      w_win_now = as.numeric(w_win_now),
      w_future = as.numeric(w_future)
    )

  tidyr::crossing(actual_teams, effective_weights)
}

select_trade_targets_for_posture <- function(priced_assets, my_posture,
                                             min_target_value = 1.5,
                                             min_rebuild_arb = -2.0,
                                             top_n = 20L) {
  assets <- priced_assets %>%
    mutate(
      asset_type = coalesce(as.character(asset_type), "player"),
      get_arb = v_to_me - v_to_partner,
      prospect_value = coalesce(as.numeric(prospect_value), 0),
      pick_value = coalesce(as.numeric(pick_value), 0),
      future_value = coalesce(as.numeric(future_value), 0)
    )

  if (identical(my_posture, "rebuild")) {
    return(
      assets %>%
        filter(
          asset_type %in% c("player", "prospect", "pick"),
          future_value >= min_target_value,
          get_arb >= min_rebuild_arb
        ) %>%
        arrange(desc(future_value), desc(prospect_value), desc(pick_value), desc(v_to_me)) %>%
        head(top_n)
    )
  }

  assets %>%
    filter(asset_type == "player", get_arb > 0, v_to_me >= min_target_value) %>%
    arrange(desc(get_arb)) %>%
    head(top_n)
}
