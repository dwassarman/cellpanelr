#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List of all reactive values shared by modules
  rv <- reactiveValues()
  # Variable contained in rv
  # navbar_tab - active tab on top navbar
  # cell_col - name of user-selected cell line column
  # response_col - name of user-selected response column
  
  # Update main navbar tab anytime rv$navbar_tab is changed
  rv$navbar_tab <- reactive("Home")
  observe({
    updateNavbarPage(inputId = "main_navbar", selected = rv$navbar_tab())
  })
  
  # Module servers
  mod_home_server("home_1", rv)
  mod_upload_server("upload_ui_1", rv)
  
  # Debugging
  output$data <- DT::renderDT(rv$data())
  
  
  
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
