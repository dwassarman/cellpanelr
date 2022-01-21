#' annotation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_annotation_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("feature"), "Select a feature", choices = NULL),
      ),
      mainPanel(
        DT::DTOutput(ns("plot")) %>% shinycssloaders::withSpinner(),
      ),
    )
  )
}
    
#' annotation Server Functions
#'
#' @noRd 
mod_annotation_server <- function(id, rv){
  stopifnot(is.reactivevalues(rv))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_annotation_ui("annotation_ui_1")
    
## To be copied in the server
# mod_annotation_server("annotation_ui_1")
