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
    br(),
    h4("Correlate cell line features like mutations, gene expression, and
    more with your own data to identify predictive biomarkers."),
    br(),
    br(),
    fluidRow(
      col_6(
        wellPanel(
          h4("Step 1: Upload your data"),
          p("Upload response data you have collected from a panel of cell lines."),
          tags$img(src = "www/upload_homepage.jpeg", width = "80%")
        )
      ),
      col_6(
        wellPanel(
          h4("Step 2: Find correlations"),
          p("Correlate your data with one of several data sets and
        interactively plot the results."),
          tags$img(src = "www/mutations_example.jpeg", width = "81%")
        )
      )
    ),
    br(),
    col_12(
      align = "center",
      actionButton(ns("button"), "Get started", class = "btn-primary btn-lg")
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
