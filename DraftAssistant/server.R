#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(googlesheets4)
library(fuzzyjoin)

gs4_deauth()

#Pull pre-draft data
frozen_rosters <- read_sheet("https://docs.google.com/spreadsheets/d/1E-MsV8UEWyL3zv7ybS6a40l0sxIu7dmah2bhaBJaLNw/edit#gid=212188698", sheet = "FrozenRosters", col_types = 'cccccc')
draft <- read_sheet("https://docs.google.com/spreadsheets/d/1E-MsV8UEWyL3zv7ybS6a40l0sxIu7dmah2bhaBJaLNw/edit#gid=212188698", sheet = "Draft", col_types = 'ciiicccccc')
salaries <- read_sheet("https://docs.google.com/spreadsheets/d/1E-MsV8UEWyL3zv7ybS6a40l0sxIu7dmah2bhaBJaLNw/edit#gid=212188698", sheet = "Salaries", col_types = 'ccc') %>% 
    rename(new_salary = Salary)  %>% 
    filter(!is.na(Player)) %>%  
    mutate(across(c("new_salary"), ~gsub("\\$", "", .) %>% as.numeric))
positions <- suppressWarnings(read_sheet("https://docs.google.com/spreadsheets/d/1E-MsV8UEWyL3zv7ybS6a40l0sxIu7dmah2bhaBJaLNw/edit#gid=0", sheet = "Positions", col_types = 'ciiiiiiiiiii') %>% 
    mutate(PLAYER = gsub("\n.*","",PLAYER)) %>% 
    mutate(PLAYER = gsub("DTD.*","",PLAYER)) %>%
    mutate(p_of = case_when(RF == 1 ~ 1, CF == 1 ~ 1, LF == 1 ~ 1, .default = 0)) %>%
    mutate(p_ci = case_when(`1B` == 1 ~ 1, `3B` == 1 ~ 1, .default = 0)) %>%
    mutate(p_mi = case_when(`2B` == 1 ~ 1, SS == 1 ~ 1, .default = 0)) %>%  
    rename(player = PLAYER, p_c = C, p_1b = `1B`, p_2b = `2B`, p_3b = `3B`, p_ss = SS, p_dh = DH) %>% 
    select(player, p_c, p_1b, p_2b, p_3b, p_ss, p_of, p_ci, p_mi, p_dh))

#Clean up tables and join
frozen_rosters_clean <- frozen_rosters %>% 
  rename(billikenTeam = Owner, player = Player, contract = Contract, salary = Salary) %>% 
  mutate(across(c("salary"), ~gsub("\\$", "", .) %>% as.numeric), contract = contract %>% as.numeric, billikenTeam = str_to_title(billikenTeam)) %>% 
  select(player, billikenTeam, contract, salary)

draft_clean <- draft %>%
  rename(player = "Player...1", billikenTeam = Team, salary = Salary) %>% 
  mutate(contract = 1, salary = salary %>% as.numeric) %>% 
  select(player, billikenTeam, contract, salary)

rosters <- bind_rows(frozen_rosters_clean,draft_clean) 

rosters <- rosters %>% 
  filter(!is.na(player))

#Load FanGraphs projections downloaded locally
hitter_projections <- read_csv("hitter_projections_2024.csv") 
pitcher_projections <- read_csv("pitcher_projections_2024.csv")

hitter_projections_nl <- hitter_projections %>% 
  #Find NL projections only
  filter(Team %in% c('ATL','LAD','SDP','ARI','NYM','PHI','MIL','STL','CHC','SFG','CIN','COL','PIT','MIA','WSN','NA')) %>%
  stringdist_left_join(rosters, by = c("Name" = "player"), max_dist = 2)

pitcher_projections_nl <- pitcher_projections %>% 
  #Find NL projections only
  filter(Team %in% c('ATL','LAD','SDP','ARI','NYM','PHI','MIL','STL','CHC','SFG','CIN','COL','PIT','MIA','WSN','NA')) %>%
  stringdist_left_join(rosters, by = c("Name" = "player"), max_dist = 2)


#Combined totals from pre-freeze rosters
hitter_team_totals <- hitter_projections_nl %>% 
    group_by(billikenTeam) %>% 
    summarize(n=n(), PA = sum(PA), AB = sum(AB), H = sum(H), HR = sum(HR), R = sum(R), RBI = sum(RBI), SB = sum(SB), AVG = sum(H)/sum(AB))
pitcher_team_totals <- pitcher_projections_nl %>% 
    group_by(billikenTeam) %>% 
    summarize(n=n(), W = sum(W), SV = sum(SV), IP = sum(IP), SO = sum(SO), ER = sum(ER), H = sum(H), BB = sum(BB), ERA = sum(ER)*9/sum(IP), WHIP = (sum(H)+sum(BB))/sum(IP))

#Rank Team Totals
n_teams <- pull(count(hitter_team_totals %>% filter(!is.na(billikenTeam)) %>% distinct(billikenTeam)))

hitter_points <- hitter_team_totals %>% 
  filter(!is.na(billikenTeam)) %>% 
  mutate(hr = n_teams+1 - dense_rank(desc(HR)), r = n_teams+1 - dense_rank(desc(R)), rbi = n_teams+1 - dense_rank(desc(RBI)), sb = n_teams+1 - dense_rank(desc(SB)), avg = n_teams+1 - dense_rank(desc(AVG))) %>% 
  mutate(hr_pct = (hr-1)/(n_teams-1), r_pct = (r-1)/(n_teams-1), rbi_pct = (rbi-1)/(n_teams-1), sb_pct = (sb-1)/(n_teams-1), avg_pct = (avg-1)/(n_teams-1)) %>% 
  mutate(hit = hr + r + rbi + sb + avg) %>% 
  arrange(desc(hit))

pitcher_points <- pitcher_team_totals %>% 
  filter(!is.na(billikenTeam)) %>% 
  mutate(w = n_teams+1 - dense_rank(desc(W)), sv = n_teams+1 - dense_rank(desc(SV)), so = n_teams+1 - dense_rank(desc(SO)), era = n_teams+1 - dense_rank(ERA), whip = n_teams+1 - dense_rank(WHIP)) %>%
  mutate(w_pct = (w-1)/(n_teams-1), sv_pct = (sv-1)/(n_teams-1), so_pct = (so-1)/(n_teams-1), era_pct = (era-1)/(n_teams-1), whip_pct = (whip-1)/(n_teams-1)) %>%
  mutate(pit = w + sv + so + era + whip) %>% 
  arrange(desc(pit))


#Continuous models of hitting categories
hr_model_glm <- glm(hr_pct ~ HR, data = hitter_points, family = "binomial")
hitter_points$hr_pts_pred = predict(hr_model_glm, hitter_points, type="response")*n_teams

r_model_glm <- glm(r_pct ~ R, data = hitter_points, family = "binomial")
hitter_points$r_pts_pred = predict(r_model_glm, hitter_points, type="response")*n_teams

rbi_model_glm <- glm(rbi_pct ~ RBI, data = hitter_points, family = "binomial")
hitter_points$rbi_pts_pred = predict(rbi_model_glm, hitter_points, type="response")*n_teams

sb_model_glm <- glm(sb_pct ~ SB, data = hitter_points, family = "binomial")
hitter_points$sb_pts_pred = predict(sb_model_glm, hitter_points, type="response")*n_teams

avg_model_glm <- glm(avg_pct ~ AVG, data = hitter_points, family = "binomial")
hitter_points$avg_pts_pred = predict(hr_model_glm, hitter_points, type="response")*n_teams

hitter_points <- hitter_points %>% 
  mutate(hitter_points_pred = hr_pts_pred + r_pts_pred + rbi_pts_pred + sb_pts_pred + avg_pts_pred)


#Continuous models of pitching categories

w_model_glm <- glm(w_pct ~ W, data = pitcher_points, family = "binomial")
pitcher_points$w_pts_pred = predict(w_model_glm, pitcher_points, type="response")*n_teams

sv_model_glm <- glm(sv_pct ~ SV, data = pitcher_points, family = "binomial")
pitcher_points$sv_pts_pred = predict(sv_model_glm, pitcher_points, type="response")*n_teams

so_model_glm <- glm(so_pct ~ SO, data = pitcher_points, family = "binomial")
pitcher_points$so_pts_pred = predict(so_model_glm, pitcher_points, type="response")*n_teams

era_model_glm <- glm(era_pct ~ ERA, data = pitcher_points, family = "binomial")
pitcher_points$era_pts_pred = predict(era_model_glm, pitcher_points, type="response")*n_teams

whip_model_glm <- glm(whip_pct ~ WHIP, data = pitcher_points, family = "binomial")
pitcher_points$whip_pts_pred = predict(whip_model_glm, pitcher_points, type="response")*n_teams

pitcher_points <- pitcher_points %>% 
    mutate(pitcher_points_pred = w_pts_pred + sv_pts_pred + so_pts_pred + era_pts_pred + whip_pts_pred) %>% 
    arrange(desc(pitcher_points_pred))

projected_standings <- hitter_points %>% 
  inner_join(pitcher_points, by = join_by(billikenTeam)) %>% 
  mutate(total = round(hitter_points_pred + pitcher_points_pred,1), players = n.x + n.y) %>% 
  mutate(across(ends_with("_pts_pred"), round, 1)) %>% 
  #mutate(total = hit + pit) %>% 
  select(billikenTeam, players, total, hr_pts_pred, r_pts_pred, rbi_pts_pred, sb_pts_pred, avg_pts_pred, w_pts_pred, sv_pts_pred, so_pts_pred, era_pts_pred, whip_pts_pred) %>% 
  #select(billikenTeam, total, hr, r, rbi, sb, avg, w, sv, so, era, whip, total) %>% 
  arrange(desc(total)) 

projected_standings <- projected_standings %>% 
  rename(hr = hr_pts_pred, r = r_pts_pred, rbi = rbi_pts_pred, sb = sb_pts_pred, avg = avg_pts_pred, w = w_pts_pred, sv = sv_pts_pred,  so = so_pts_pred, era = era_pts_pred, whip = whip_pts_pred)

#Calculate and assign category point values
hr_model <- lm(hr ~ HR, hitter_points) 
r_model <- lm(r ~ R, hitter_points) 
rbi_model <- lm(rbi ~ RBI, hitter_points) 
sb_model <- lm(sb ~ SB, hitter_points) 
avg_model <- lm(avg ~ AVG, hitter_points) 

w_model <- lm(w ~ W, pitcher_points) 
sv_model <- lm(sv ~ SV, pitcher_points) 
so_model <- lm(so ~ SO, pitcher_points) 
era_model <- lm(era ~ ERA, pitcher_points) 
whip_model <- lm(whip ~ WHIP, pitcher_points) 

hr_factor = hr_model$coefficients["HR"]
r_factor = r_model$coefficients["R"]
rbi_factor = rbi_model$coefficients["RBI"]
sb_factor = sb_model$coefficients["SB"]
avg_factor = avg_model$coefficients["AVG"]

melonheads_h <- pull(hitter_team_totals %>% filter(billikenTeam == "Melonheads") %>% select(H))
melonheads_ab <- pull(hitter_team_totals %>% filter(billikenTeam == "Melonheads") %>% select(AB))

w_factor = w_model$coefficients["W"]
sv_factor = sv_model$coefficients["SV"]
so_factor = so_model$coefficients["SO"]
era_factor = era_model$coefficients["ERA"]
whip_factor = whip_model$coefficients["WHIP"]

melonheads_ip <- pull(pitcher_team_totals %>% filter(billikenTeam == "Melonheads") %>% select(IP))
melonheads_er <- pull(pitcher_team_totals %>% filter(billikenTeam == "Melonheads") %>% select(ER))
melonheads_wh <- pull(pitcher_team_totals %>% filter(billikenTeam == "Melonheads") %>% select(BB)) + pull(pitcher_team_totals %>% filter(billikenTeam == "Melonheads") %>% select(H))

hitter_projections_nl <- hitter_projections_nl %>% 
  mutate(point_value = round(HR * hr_factor + R * r_factor + RBI * rbi_factor + SB * sb_factor + avg_factor * ((melonheads_h + H)/(melonheads_ab + AB) - melonheads_h/melonheads_ab),1))

pitcher_projections_nl <- pitcher_projections_nl %>% 
  mutate(point_value = round(W * w_factor + SV * sv_factor + SO * so_factor + era_factor * (9*(melonheads_er + ER)/(melonheads_ip + IP) - 9*melonheads_er/melonheads_ip) + whip_factor * ((melonheads_wh + BB + H)/(melonheads_ip + IP) - melonheads_wh/melonheads_ip),1))

#Combined available player list
projected_players <- bind_rows(hitter_projections_nl, pitcher_projections_nl) %>% 
  filter(Team %in% c('ATL','LAD','SDP','ARI','NYM','PHI','MIL','STL','CHC','SFG','CIN','COL','PIT','MIA','WSN','NA')) %>%
  stringdist_left_join(positions, by = c("Name" = "player"), max_dist = 2) %>% 
  stringdist_left_join(salaries, by = c("Name" = "Player"), max_dist = 2) %>% 
  mutate(AVG = round(AVG,3), ERA = round(ERA,2), WHIP = round(WHIP,2), SO = case_when(IP == 0 ~ NA, IP > 0 ~ SO)) %>%  
  mutate(HR = case_when(PA == 0 ~ NA, PA > 0 ~ HR), R = case_when(PA == 0 ~ NA, PA > 0 ~ R)) %>% 
  mutate(salary = coalesce(salary,new_salary), contract = coalesce(contract,1)) %>% 
  select(Name, billikenTeam, contract, salary, Team, PA, HR, R, RBI, SB, AVG, IP, W, SV, SO, ERA, WHIP, point_value, p_c, p_1b, p_2b, p_3b, p_ss, p_of, p_ci, p_mi, p_dh) %>% 
  arrange(desc(point_value))

rl_c <- projected_players %>% 
  filter(p_c == 1) %>% 
  filter(row_number(desc(point_value)) == 21L) %>% 
  mutate(pos = 'c') %>% 
  select(Name, pos, point_value)

rl_1b <- projected_players %>% 
  filter(p_1b == 1) %>% 
  filter(row_number(desc(point_value)) == 16L) %>% 
  mutate(pos = '1b') %>% 
  select(Name, pos, point_value)

rl_2b <- projected_players %>% 
  filter(p_2b == 1) %>% 
  filter(row_number(desc(point_value)) == 16L) %>% 
  mutate(pos = '2b') %>% 
  select(Name, pos, point_value)

rl_3b <- projected_players %>% 
  filter(p_3b == 1) %>% 
  filter(row_number(desc(point_value)) == 16L) %>% 
  mutate(pos = '3b') %>% 
  select(Name, pos, point_value)

rl_ss <- projected_players %>% 
  filter(p_ss == 1) %>% 
  filter(row_number(desc(point_value)) == 16L) %>% 
  mutate(pos = 'ss') %>% 
  select(Name, pos, point_value)

rl_of <- projected_players %>% 
  filter(p_of == 1) %>% 
  filter(row_number(desc(point_value)) == 51L) %>% 
  mutate(pos = 'of') %>% 
  select(Name, pos, point_value)

rl_ci <- projected_players %>% 
  filter(p_ci == 1) %>% 
  filter(row_number(desc(point_value)) == 31L) %>% 
  mutate(pos = 'ci') %>% 
  select(Name, pos, point_value)

rl_mi <- projected_players %>% 
  filter(p_mi == 1) %>% 
  filter(row_number(desc(point_value)) == 31L) %>% 
  mutate(pos = 'mi') %>% 
  select(Name, pos, point_value)

rl_dh <- projected_players %>% 
  filter(p_dh == 1) %>% 
  filter(row_number(desc(point_value)) == 11L) %>% 
  mutate(pos = 'dh') %>% 
  select(Name, pos, point_value)

rl_util <- projected_players %>% 
  filter(row_number(desc(point_value)) == 151L) %>% 
  mutate(pos = 'util') %>% 
  select(Name, pos, point_value)

rl_p <- projected_players %>% 
  filter(IP > 0) %>% 
  filter(row_number(desc(point_value)) == 91L) %>% 
  mutate(pos = 'p') %>% 
  select(Name, pos, point_value)

replacement_level <- rbind(rl_c, rl_1b, rl_2b, rl_3b, rl_ss, rl_of, rl_ci, rl_mi, rl_dh, rl_util, rl_p)

rl_p_value <- pull(rl_p %>% select(point_value))
rl_c_value <- pull(rl_c %>% select(point_value))
rl_util_value <- pull(rl_util %>% select(point_value))

par <- projected_players %>% 
  mutate(repl = case_when(IP > 0 ~ rl_p_value, 
                          p_c == 1 ~ rl_c_value,
                          .default = rl_util_value)
  ) %>% 
  mutate(par = point_value - repl) %>% 
  arrange(desc(par)) %>% 
  select(Name, Team, billikenTeam, contract, salary, point_value, repl, par, PA, HR, R, RBI, SB, AVG, IP, W, SV, SO, ERA, WHIP, p_c, p_1b, p_2b, p_3b, p_ss, p_of, p_ci, p_mi, p_dh)


print("Finished running server")

# Define server logic required to draw a histogram
function(input, output, session) {
  
  #selected_team <- 
  
  selected_players <- reactive({
    par %>% filter(billikenTeam == input$team | input$team == "Available" & is.na(billikenTeam))
    })
  
  output$players <- renderDataTable(
    selected_players(), 
    options = list(
      pageLength = 20,
      autoWidth = TRUE,
      scrollX = TRUE, 
      columnDefs = list(list(targets=c(0), visible=TRUE, width='150')
      )
    )
    #datatable(available_players, options = list(orderClasses = TRUE, options = list(lengthMenu = c(10, 25), pageLength = 20)))
  )
  
  output$projected_standings <- renderDataTable(
    projected_standings, 
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      scrollX = TRUE, 
      columnDefs = list(list(targets=c(0), visible=TRUE, width='50'))
    )
  )
  
  #output$proj_standings <- renderDataTable(
  #  proj_standings
  #  #datatable(proj_standings, options = list(orderClasses = TRUE, options = list(pageLength = 10)))
  #)
  
  # output$proj_standings <- renderPlot({
  # 
  #     # generate bins based on input$bins from ui.R
  #     x    <- faithful[, 2]
  #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #     # draw the histogram with the specified number of bins
  #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
  #          xlab = 'Waiting time to next eruption (in mins)',
  #          main = 'Histogram of waiting times')
  
  #})
  
  print("Finished server function")
  
}

#update for frozen rosters + drafted players + salaries

#update with replacement level & value

#force zeroes in projected stats

#Fix team selector

#Team specific impact

#Show updated standings with draft pick?

#simulate rest of draft