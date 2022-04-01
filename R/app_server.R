#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  rv <- reactiveValues()
  mod_upload_server("upload_ui_1", rv)
  mod_annotation_server("annotation_ui_1", rv)
  mod_expression_server("expression_ui_1", rv)

  mod_resources_server("resources_1")
}
