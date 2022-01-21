#' expression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_expression_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        verbatimTextOutput(ns("message")),
        actionButton(ns("go"), "Submit"),
      ),
      mainPanel(
        DT::DTOutput(ns("table")),
      ),
    ),
  )
}
    
#' expression Server Functions
#'
#' @noRd 
mod_expression_server <- function(id, rv){
  stopifnot(is.reactivevalues(rv))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$message <- renderText({
      n_matched <- rv$data() %>%
        dplyr::semi_join(cellpanelr::expression,
                         by = "depmap_id",
                         suffix = c("", ".dapmap")) %>%
      nrow()

      paste0(n_matched, " cell lines with expression data")
    })
    
    cors <- reactive({
      rv$data() %>%
        cor_expression()
    }) %>% bindEvent(input$go)
    
    output$table <- DT::renderDT(DT::datatable(
      cors(),
      options = list("scrollX" = TRUE)))
  })
}
    
## To be copied in the UI
# mod_expression_ui("expression_ui_1")
    
## To be copied in the server
# mod_expression_server("expression_ui_1")
