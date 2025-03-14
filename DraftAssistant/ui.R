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
  
  tags$head(
    tags$style(HTML("table {table-layout: fixed;"))
  ),
    # Application title
    titlePanel("Billiken League Draft Assistant"),

    mainPanel(
      
      selectInput("team", "Select a player group:",
                 list(
                  `Available` = "Available",
                  `Melonheads` = "Melonheads",
                  `Free At Last` = "Free At Last",
                  `Blue Socks` = "Blue Socks",
                  `Free Birds` = "Free Birds",
                  `Westside Marauders` = "Westside Marauders",
                  `Hoosiers` = "Hoosiers",
                  `Louisville Sluggers` = "Louisville Sluggers",
                  `Erie Lakers` = "Erie Lakers",
                  `Big Red Machine` = "Big Red Machine",
                  `National Pastime` = "National Pastime"
                     )
      ),
      
      selectInput("pos", "Select a position:",
                  list(
                    "All","Hitters","P","C","1B","2B","3B","SS","OF","DH","CI","MI"
                  )
      ),
      
      
      DT::DTOutput('players')
    ),

    sidebarPanel(
      DT::DTOutput('projected_standings')
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
