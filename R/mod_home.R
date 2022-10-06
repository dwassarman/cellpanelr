#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Welcome to cellpanelr"),
    h4("Identify predictive biomarkers from cell line panels. Correlate your data with mutations, gene expression, and more."),
    # h4("Correlate data from a panel of cell lines with features like mutations, gene expression, and
    # more to identify predictive biomarkers."),
    # br(),
    # fluidRow(
    #   col_6(
    #     wellPanel(
    #       h4("Step 1: Upload your data"),
    #       p("Upload response data you have collected from a panel of cell lines."),
    #       tags$img(src = "www/upload_homepage.jpeg", width = "80%")
    #     )
    #   ),
    #   col_6(
    #     wellPanel(
    #       h4("Step 2: Find correlations"),
    #       p("Correlate your data with one of several data sets and
    #     interactively plot the results."),
    #       tags$img(src = "www/mutations_example.jpeg", width = "81%")
    #     )
    #   )
    # ),
    # br(),
    actionButton(ns("button"), "Get started", class = "btn-primary btn-lg"),
    br(),
    br(),
    sidebarLayout(
      sidebarPanel(
        h4("Step 1: Upload your data"),
        p("Upload response data you have collected from a panel of cell lines."),
        br(),
        h4("Step 2: Find biomarkers"),
        p("Correlate your data with one of several data sets and interactively plot
        the results."),
        br(),
      ),
      mainPanel(
        h4("Overview of cellpanelr"),
        tags$image(src = "www/cellpanelr_overview.svg", width = "400px")
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, rv) {
  stopifnot(is.reactivevalues(rv))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      rv$active_tab <- reactive("Upload")
    }) %>% bindEvent(input$button)
  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
