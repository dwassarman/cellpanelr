#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List of all reactive values shared by modules
  rv <- reactiveValues()
  
  # Update main navbar tab anytime rv$navbar_tab is changed
  rv$navbar_tab <- reactive("Home")
  observe({
    updateNavbarPage(inputId = "main_navbar", selected = rv$navbar_tab())
  })
  
  # Module servers
  mod_home_server("home_1", rv)
  
  
  
  #####################################
  ## OLD CODE ##
  ######################################
  # 
  # # Your application server logic
  # rv <- reactiveValues()
  # mod_upload_server("upload_ui_1", rv)
  # mod_annotation_server("annotation_ui_1", rv)
  # mod_expression_server("expression_ui_1", rv)
  # 
  # mod_resources_server("resources_1")
}
