#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Billiken League Draft Assistant"),

    mainPanel(
      dataTableOutput('available_players')
    ),
    
    sidebarPanel(
      dataTableOutput('projected_standings')
    )
        
)


    
    # Sidebar with projected standings (& plot?)
#     
#     sidebarLayout(
#       sidebarPanel(
#         column(12, dataTableOutput('proj_standings'))
#       ),
#       
#         # sidebarPanel(
#         #     sliderInput("bins",
#         #                 "Number of bins:",
#         #                 min = 1,
#         #                 max = 50,
#         #                 value = 30)
#         # ),
# 
#         #Button to refresh the drafted player list
#         
#         # Show available player list
#         mainPanel(
#           tabsetPanel(
#             id = 'dataset',
#             tabPanel("All", dataTableOutput("available_players")),
#             tabPanel("P", dataTableOutput("available_players"))
#           #plotOutput("proj_standings")
#         )
#      )
# )
# )