# Application - UI --------------------------------------------------------

ui <- shinyUI(
  navbarPage(
    "Dalberg Research Credit Scorecard Builder",
    theme = shinytheme("sandstone"),
    tags$head(
      tags$style(HTML(".navbar-default { background-color: maroon; }"))),
    id = "nav_top",
    tabPanel(
      span("Data", title = "Upload and manage data"),
      data_ui("data_module"),
      value = "data"
    ),
    tabPanel(
      span("Explore",
           title = "Descriptive statistics and distribution of variables"),
      explore_ui("explore_module"),
      value = "explore"
    ),
    tabPanel(
      span("Sample", title = "Training and test samples"),
      sample_ui("sample_module"),
      value = "sample"
    ),
    tabPanel(
      span("Binning", title = "Weight of evidence and information value"),
      binning_ui("binning_module"),
      value = "binning"
    ),
    tabPanel(
      span("Model", title = "Credit scorecard development and alignment"),
      model_ui("model_module"),
      value = "model"
    ),
    tabPanel(
      span("Validation", title = "Scorecard validation"),
      validation_ui("validation_module"),
      value = "validation"
    ),
    tabPanel(
      span("Summary", title = "Summary of Scores"),
      validation_ui("summary_module"),
      value = "summary"
    ),
    tabPanel(
      "",
      value = "download",
      download_ui("download_module"),
      icon = icon("download")
    ),
    tabPanel(
      "",
      value = "help",
      helpText("Help"),
      icon = icon("question")
    ),
    tabPanel(
      "",
      value = "stop",
      icon = icon("stop")
    )
  )
)