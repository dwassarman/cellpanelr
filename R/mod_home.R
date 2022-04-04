#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel("Welcome to cellpanelr"),
    hr(),
    fluidRow(
      align = "center",
      actionButton(ns("go_to_upload"), "Get started")
    )
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id, rv){
  # make sure rv is reactive values list
  stopifnot(is.reactivevalues(rv))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Switch tab to Upload
    observe({
      rv$navbar_tab <- reactive("Upload")
    }) %>% bindEvent(input$go_to_upload)
 
  })
}
    
## To be copied in the UI
# mod_home_ui("home_1")
    
## To be copied in the server
# mod_home_server("home_1")
