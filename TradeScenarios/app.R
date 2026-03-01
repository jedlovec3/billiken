library(shiny)
library(DT)

scenarios_dir <- "../data/scenarios"

get_scenarios <- function() {
  dirs <- list.dirs(scenarios_dir, recursive = FALSE, full.names = FALSE)
  dirs <- dirs[dirs != "_baseline"]
  dirs[vapply(dirs, function(d) {
    file.exists(file.path(scenarios_dir, d, "latest.txt"))
  }, logical(1))]
}

prettify <- function(name) {
  tools::toTitleCase(gsub("_", " ", name))
}

load_delta <- function(scenario_name) {
  latest_path <- trimws(readLines(file.path(scenarios_dir, scenario_name, "latest.txt"), warn = FALSE)[1])
  run_folder  <- basename(latest_path)
  csv_path    <- file.path(scenarios_dir, scenario_name, run_folder, "delta_summary.csv")

  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  keep <- c("team", "baseline_avg_pts", "scenario_avg_pts",
            "delta_avg_pts", "delta_avg_rank", "delta_wins",
            "delta_top_3", "delta_avg_hit_pts", "delta_avg_pit_pts")
  df <- df[, keep]

  colnames(df) <- c("Team", "Baseline Pts", "Scenario Pts",
                    "\u0394 Pts", "\u0394 Rank", "\u0394 Wins",
                    "\u0394 Top 3", "\u0394 Hit Pts", "\u0394 Pitch Pts")

  num_cols <- colnames(df)[colnames(df) != "Team"]
  df[num_cols] <- lapply(df[num_cols], round, digits = 1)

  df[order(-df[["\u0394 Pts"]]), ]
}

scenario_names   <- get_scenarios()
scenario_choices <- setNames(scenario_names, vapply(scenario_names, prettify, character(1)))

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
      .well { background-color: #f8f9fa; border: none; box-shadow: none; }
      h2 { margin-bottom: 4px; }
      .scenario-subtitle { color: #6c757d; margin-bottom: 20px; font-size: 14px; }
    "))
  ),

  titlePanel(
    div(
      h2("Billiken League \u2014 Trade Scenarios"),
      div("Simulated impact of each trade on projected standings", class = "scenario-subtitle")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "scenario",
        "Select a trade scenario:",
        choices  = scenario_choices,
        selected = scenario_names[1]
      ),
      hr(),
      div(
        style = "font-size: 12px; color: #6c757d;",
        strong("Column guide:"), br(),
        "\u0394 = difference vs baseline simulation.", br(), br(),
        strong("\u0394 Pts"), " \u2014 projected standing points", br(),
        strong("\u0394 Rank"), " \u2014 avg finish (negative = better)", br(),
        strong("\u0394 Wins"), " \u2014 championship wins", br(),
        strong("\u0394 Top 3"), " \u2014 top-3 finishes", br(),
        strong("\u0394 Hit / Pitch Pts"), " \u2014 hitting & pitching points"
      )
    ),

    mainPanel(
      width = 9,
      DT::DTOutput("delta_table")
    )
  )
)

server <- function(input, output, session) {

  delta_data <- reactive({
    req(input$scenario)
    tryCatch(
      load_delta(input$scenario),
      error = function(e) {
        showNotification(paste("Error loading scenario:", e$message), type = "error")
        NULL
      }
    )
  })

  output$delta_table <- DT::renderDataTable({
    df <- delta_data()
    req(df)

    dt <- DT::datatable(
      df,
      rownames = FALSE,
      options  = list(
        pageLength = 15,
        dom        = "t",
        ordering   = TRUE,
        scrollX    = TRUE
      )
    )

    dt <- DT::formatStyle(
      dt,
      "\u0394 Pts",
      color      = DT::styleInterval(0, c("#c0392b", "#27ae60")),
      fontWeight = "bold"
    )

    dt <- DT::formatStyle(
      dt,
      "\u0394 Rank",
      color      = DT::styleInterval(0, c("#27ae60", "#c0392b")),
      fontWeight = "bold"
    )

    dt
  })
}

shinyApp(ui = ui, server = server)
