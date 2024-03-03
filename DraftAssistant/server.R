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
prefreeze_rosters <- read_sheet("https://docs.google.com/spreadsheets/d/1E-MsV8UEWyL3zv7ybS6a40l0sxIu7dmah2bhaBJaLNw/edit#gid=0", sheet = "PreFreezeRosters", col_types = 'ccccccc') %>% 
  filter(!is.na(player)) %>% 
  mutate(across(c("salary"), ~gsub("\\$", "", .) %>% as.numeric))
frozen_rosters <- read_sheet("https://docs.google.com/spreadsheets/d/1E-MsV8UEWyL3zv7ybS6a40l0sxIu7dmah2bhaBJaLNw/edit#gid=1008004729", sheet = "FrozenRosters", col_types = 'cccccc')
salaries <- read_sheet("https://docs.google.com/spreadsheets/d/1E-MsV8UEWyL3zv7ybS6a40l0sxIu7dmah2bhaBJaLNw/edit#gid=1008004729", sheet = "Salaries", col_types = 'ccc')

#Load FanGraphs projections downloaded locally
hitter_projections <- read_csv("hitter_projections_2024.csv") 
pitcher_projections <- read_csv("pitcher_projections_2024.csv")

#Pull latest Billiken Draft data
draft <- read_sheet("https://docs.google.com/spreadsheets/d/1E-MsV8UEWyL3zv7ybS6a40l0sxIu7dmah2bhaBJaLNw/edit#gid=1008004729", sheet = "Draft", col_types = 'ciiicccccc')

#Combined totals from pre-freeze rosters
#Change to use frozen rosters after 3/10
hitter_team_totals <- hitter_projections %>% 
    #Find NL projections only
    filter(Team %in% c('ATL','LAD','SDP','ARI','NYM','PHI','MIL','STL','CHC','SFG','CIN','COL','PIT','MIA','WSN','NA')) %>%
    stringdist_left_join(prefreeze_rosters, by = c("Name" = "player"), max_dist = 2) %>% 
    group_by(billikenTeam) %>% 
    summarize(n=n(), PA = sum(PA), AB = sum(AB), H = sum(H), HR = sum(HR), R = sum(R), RBI = sum(RBI), SB = sum(SB), AVG = sum(H)/sum(AB))
pitcher_team_totals <- pitcher_projections %>% 
    #Find NL projections only
    filter(Team %in% c('ATL','LAD','SDP','ARI','NYM','PHI','MIL','STL','CHC','SFG','CIN','COL','PIT','MIA','WSN','NA')) %>%
    stringdist_left_join(prefreeze_rosters, by = c("Name" = "player"), max_dist = 2) %>% 
    group_by(billikenTeam) %>% 
    summarize(n=n(), W = sum(W), SV = sum(SV), IP = sum(IP), SO = sum(SO), ER = sum(ER), H = sum(H), BB = sum(BB), ERA = sum(ER)*9/sum(IP), WHIP = (sum(H)+sum(BB))/sum(IP)) 

#Rank Team Totals
hitter_points <- hitter_team_totals %>% 
  filter(!is.na(billikenTeam)) %>% 
  mutate(hr = 10 - dense_rank(desc(HR)), r = 10 - dense_rank(desc(R)), rbi = 10 - dense_rank(desc(RBI)), sb = 10 - dense_rank(desc(SB)), avg = 10 - dense_rank(desc(AVG))) %>% 
  mutate(hr_pct = (hr-1)/8, r_pct = (r-1)/8, rbi_pct = (rbi-1)/8, sb_pct = (sb-1)/8, avg_pct = (avg-1)/8) %>% 
  mutate(hit = hr + r + rbi + sb + avg) %>% 
  arrange(desc(hit))
pitcher_points <- pitcher_team_totals %>% 
  filter(!is.na(billikenTeam)) %>% 
  mutate(w = 10 - dense_rank(desc(W)), sv = 10 - dense_rank(desc(SV)), so = 10 - dense_rank(desc(SO)), era = 10 - dense_rank(ERA), whip = 10 - dense_rank(WHIP)) %>%
  mutate(w_pct = (w-1)/8, sv_pct = (sv-1)/8, so_pct = (so-1)/8, era_pct = (era-1)/8, whip_pct = (whip-1)/8) %>%
  mutate(pit = w + sv + so + era + whip) %>% 
  arrange(desc(pit)) 

#Continuous models of hitting categories
hr_model_glm <- glm(hr_pct ~ HR, data = hitter_points, family = "binomial")
hitter_points$hr_pts_pred = predict(hr_model_glm, hitter_points, type="response")*8+1
r_model_glm <- glm(r_pct ~ R, data = hitter_points, family = "binomial")
hitter_points$r_pts_pred = predict(r_model_glm, hitter_points, type="response")*8+1
rbi_model_glm <- glm(rbi_pct ~ RBI, data = hitter_points, family = "binomial")
hitter_points$rbi_pts_pred = predict(rbi_model_glm, hitter_points, type="response")*8+1
sb_model_glm <- glm(sb_pct ~ SB, data = hitter_points, family = "binomial")
hitter_points$sb_pts_pred = predict(sb_model_glm, hitter_points, type="response")*8+1
avg_model_glm <- glm(avg_pct ~ AVG, data = hitter_points, family = "binomial")
hitter_points$avg_pts_pred = predict(hr_model_glm, hitter_points, type="response")*8+1
hitter_points <- hitter_points %>% 
  mutate(hitter_points_pred = hr_pts_pred + r_pts_pred + rbi_pts_pred + sb_pts_pred + avg_pts_pred)

#Continuous models of pitching categories
w_model_glm <- glm(w_pct ~ W, data = pitcher_points, family = "binomial")
pitcher_points$w_pts_pred = predict(w_model_glm, pitcher_points, type="response")*8+1
sv_model_glm <- glm(sv_pct ~ SV, data = pitcher_points, family = "binomial")
pitcher_points$sv_pts_pred = predict(sv_model_glm, pitcher_points, type="response")*8+1
so_model_glm <- glm(so_pct ~ SO, data = pitcher_points, family = "binomial")
pitcher_points$so_pts_pred = predict(so_model_glm, pitcher_points, type="response")*8+1
era_model_glm <- glm(era_pct ~ ERA, data = pitcher_points, family = "binomial")
pitcher_points$era_pts_pred = predict(era_model_glm, pitcher_points, type="response")*8+1
whip_model_glm <- glm(whip_pct ~ WHIP, data = pitcher_points, family = "binomial")
pitcher_points$whip_pts_pred = predict(whip_model_glm, pitcher_points, type="response")*8+1
pitcher_points <- pitcher_points %>% 
    mutate(pitcher_points_pred = w_pts_pred + sv_pts_pred + so_pts_pred + era_pts_pred + whip_pts_pred)

projected_standings <- hitter_points %>% 
  inner_join(pitcher_points, by = join_by(billikenTeam)) %>% 
  mutate(total = round(hitter_points_pred + pitcher_points_pred,1)) %>% 
  #mutate(total = hit + pit) %>% 
  select(billikenTeam, total, hr, r, rbi, sb, avg, w, sv, so, era, whip, total) %>% 
  arrange(desc(total)) 

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

hitter_projections <- hitter_projections %>% 
  mutate(point_value = round(HR * hr_factor + R * r_factor + RBI * rbi_factor + SB * sb_factor + avg_factor * ((melonheads_h + H)/(melonheads_ab + AB) - melonheads_h/melonheads_ab),1))

pitcher_projections <- pitcher_projections %>% 
  mutate(point_value = round(W * w_factor + SV * sv_factor + SO * so_factor + era_factor * (9*(melonheads_er + ER)/(melonheads_ip + IP) - 9*melonheads_er/melonheads_ip) + whip_factor * ((melonheads_wh + BB + H)/(melonheads_ip + IP) - melonheads_wh/melonheads_ip),1))

#Combined available player list
available_players <- bind_rows(hitter_projections, pitcher_projections) %>% 
  filter(Team %in% c('ATL','LAD','SDP','ARI','NYM','PHI','MIL','STL','CHC','SFG','CIN','COL','PIT','MIA','WSN','NA')) %>%
  stringdist_left_join(prefreeze_rosters, by = c("Name" = "player"), max_dist = 2) %>% 
  filter(is.na(billikenTeam)) %>% 
  mutate(AVG = round(AVG,3), ERA = round(ERA,2), WHIP = round(WHIP,2)) %>%  
  select(Name, Team, point_value, PA, HR, R, RBI, SB, AVG, IP, W, SV, SO, ERA, WHIP) %>% 
  arrange(desc(point_value))

#available_players

print("Finished running server")

# Define server logic required to draw a histogram
function(input, output, session) {

  
    output$available_players <- renderDataTable(
      available_players, 
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
