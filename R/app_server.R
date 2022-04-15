#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  rv <- reactiveValues()
  rv$active_tab <- reactive("Home")
  # Contents of rv:
  ## active_tab: name of active tab in the main navbar
  ## cell_col: name of cell line column
  ## response_col: name of response column
  ## data: uploaded data with columns cell line, response, "depmap_id"

  # # Load datasets as reactives
  # annotations_file <- system.file("extdata", "annotations.rds", package = "cellpanelr")
  # rv$expression <- reactive(readRDS(expression_file))
  # rv$annotations <- reactive(readRDS(annotations_file))

  # Module server functions
  mod_home_server("home_1", rv)
  mod_upload_single_server("upload_single_1", rv)
  mod_annotations_server("annotations_1", rv)
  mod_expression_server("expression_1", rv)
  mod_resources_server("resources_1")

  # Navigate main tabs via wizard
  observe({
    updateNavbarPage(session, "main_navbar", selected = rv$active_tab())
  })
}
