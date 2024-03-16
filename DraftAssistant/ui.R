#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

fluidPage(

    # Application title
    titlePanel("Billiken League Draft Assistant"),

    mainPanel(
      
      # selectInput("team", "Select a player group:",
      #            list(
      #             `Available` = "Available",
      #             `Melonheads` = "Melonheads",
      #             `Ben's Team` = "Ben's Team",
      #             `Free Birds` = "Free Birds",
      #             `Westside Marauders` = "Westside Marauders",
      #             `Hoosiers` = "Hoosiers",
      #             `Louisville Sluggers` = "Louisville Sluggers",
      #             `Rebuilding Year` = "Rebuilding Year",
      #             `Erie Lakers` = "Erie Lakers",
      #             `Big Red Machine` = "Big Red Machine",
      #             `National Pastime` = "National Pastime"
      #                )
      # ),
      
      #dataTableOutput('projected_standings')
      dataTableOutput('available_players')
    ),

    sidebarPanel(
    dataTableOutput('projected_standings')
     )
        
)

# 
# #
# # This is the user-interface definition of a Shiny web application. You can
# # run the application by clicking 'Run App' above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
# 
# library(shiny)
# 
# # Define UI for application that draws a histogram
# fluidPage(
#   
#   # Application title
#   titlePanel("Billiken League Draft Assistant"),
#   
#   mainPanel(
#     dataTableOutput('available_players')
#   ),
#   
#   sidebarPanel(
#     dataTableOutput('projected_standings')
#   )
#   
# )
# 

