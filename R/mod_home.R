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
    p("[Introduction and description of workflow.]"),
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
