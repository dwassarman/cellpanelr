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
    tags$b("Data:"), br(),
    a("DepMap - Broad Institute", href = "https://depmap.org/portal/"), br(),
    a("depmap R-pkg", href = "https://github.com/UCLouvain-CBIO/depmap"), br(),
    hr(),
    
    tags$b("R and shiny references:"), br(),
    a("\"R for Data Science\" by Hadley Wickham and Garrett Grolemund"), br(),
    a("\"Mastering Shiny\" by Hadley Wickham", href = "https://mastering-shiny.org"), br(),
    a("\"Engineering Production-Grade Shiny Apps\" by Colin Fay, S\u00E9bastien Rochette, Vincent Guyader and Cervan Girard.",
      href = "https://engineering-shiny.org"), br(),
    a("\"R packages\" by Hadley Wickham and Jenny Bryan", href = "https://r-pkgs.org"),
    hr(),
    
    tags$b("Thank you:"),
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
