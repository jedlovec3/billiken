.libPaths(c('/home/runner/R/x86_64-pc-linux-gnu-library/4.5', .libPaths()))

library(shiny)

shiny::runApp(
  appDir = "TradeScenarios",
  host = "0.0.0.0",
  port = 5000,
  launch.browser = FALSE
)
