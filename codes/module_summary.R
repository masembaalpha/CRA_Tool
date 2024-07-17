# Model Summary Module - Helpers ------------------------------------------------

#' Merge training and test data
#'
#' @param traindata Data frame containing the training data
#' @param testdata Data frame containing the test data
#' 
#' @return Merged data frame containing both training and test data
merge_data <- function(con) {
  scr <- scorecard()
  merged_data <- bind_rows(scr$traindata, scr$testdata)
  merged_data
}

#' Calculate summary statistics
#'
#' @param df Data frame containing the merged data
#' @param score_col Name of the score column
#' 
#' @return Data frame containing summary statistics
calculate_summary_stats <- function(df, score_col) {
  summary_stats <- df %>%
    summarise(
      count = n(),
      mean = mean(!!sym(score_col), na.rm = TRUE),
      median = median(!!sym(score_col), na.rm = TRUE),
      min = min(!!sym(score_col), na.rm = TRUE),
      max = max(!!sym(score_col), na.rm = TRUE),
      sd = sd(!!sym(score_col), na.rm = TRUE)
    )
  summary_stats
}

# Model Summary Module - UI -----------------------------------------------------

#' Interface to view the summary
#'
#' @param id character used to specify namespace,
#'     see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      9,
      uiOutput(ns("summarywindow"))
    ),
    column(3, includeMarkdown("qh_summary.md"))
  )
}

# Model Summary Module - Server -------------------------------------------------

#' Summary module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param model_data List containing the training and test data
#'
#' @return list with following components
#' \describe{
#'   \item{summary_stats}{Summary statistics}
#'   \item{merged_data}{Merged dataset}
#' }
summary_server <- function(input, output, session, model_data) {
  
  # Reactive to merge training and test data
  merged_data <- reactive({
    req(scorecard())
    merge_data(model_data$traindata, model_data$testdata)
  })
  
  # Reactive to calculate summary statistics
  summary_stats <- reactive({
    req(merged_data())
    calculate_summary_stats(merged_data(), "score")
  })
  
  # Render the summary window
  output$summarywindow <- renderUI({
    sidebarLayout(
      sidebarPanel(
        helpText("Summary of the Scores"),
        br(),
        helpText("Summary Statistics"),
        tableOutput(session$ns("summarystats"))
      ),
      mainPanel(
        div(helpText("Summary Table"), style = "font-weight:bold;"),
        DT::dataTableOutput(session$ns("summarytable")),
        br(),
        div(helpText("Score Distribution"), style = "font-weight:bold;"),
        plotOutput(session$ns("histogram")),
        br(),
        div(helpText("Score Boxplot"), style = "font-weight:bold;"),
        plotOutput(session$ns("boxplot")),
        br()
      )
    )
  })
  
  # Render the summary statistics table
  output$summarystats <- renderTable({
    req(summary_stats())
    summary_stats()
  })
  
  # Render the summary table
  output$summarytable <- DT::renderDataTable({
    req(merged_data())
    DT::datatable(merged_data())
  })
  
  # Render the score distribution histogram
  output$histogram <- renderPlot({
    req(merged_data())
    ggplot(merged_data(), aes(x = score)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "white") +
      theme_minimal() +
      labs(title = "Score Distribution", x = "Score", y = "Frequency")
  })
  
  # Render the score boxplot
  output$boxplot <- renderPlot({
    req(merged_data())
    ggplot(merged_data(), aes(y = score)) +
      geom_boxplot(fill = "blue") +
      theme_minimal() +
      labs(title = "Score Boxplot", y = "Score")
  })
  
  # Return the summary stats and merged data
  list(
    summary_stats = summary_stats,
    merged_data = merged_data
  )
}
