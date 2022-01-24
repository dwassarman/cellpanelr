#' references UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_resources_ui <- function(id){
  ns <- NS(id)
  tagList(
    "Data sources:",
    br(a("DepMap - Broad Institute", href = "https://depmap.org/portal/")),
    hr(),
    "R shiny references:",
    br(a("\"Mastering Shiny\" by Hadley Wickham", href = "https://mastering-shiny.org")),
    a("\"Engineering Production-Grade Shiny Apps\" by Colin Fay, Sébastien Rochette, Vincent Guyader and Cervan Girard.",
      href = "https://engineering-shiny.org"),
    hr(),
    "Thank you:",
    br("Taia Wu")
  )
}
    
#' references Server Functions
#'
#' @noRd 
mod_resources_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_resources_ui("resources_1")
    
## To be copied in the server
# mod_resources_server("resources_1")
