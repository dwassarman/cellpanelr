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
      mainPanel(),
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
    
    # merged <- reactive({
    #   filtered_exp <- cellpanelr::expression %>%
    #     dplyr::select(.data[["depmap_id"]],
    #                   .data[["gene_name"]],
    #                   .data[["rna_expression"]])
    #   
    #   rv$data() %>%
    #     dplyr::left_join(filtered_exp,
    #                      by = "depmap_id",
    #                      suffix = c("", ".depmap"))
    # })
 
    output$message <- renderText({
      n_matched <- rv$data() %>%
        dplyr::semi_join(cellpanelr::expression,
                         by = "depmap_id",
                         suffix = c("", ".dapmap")) %>%
      nrow()

      paste0("Data available for ", n_matched, " lines")
    })
  })
}
    
## To be copied in the UI
# mod_expression_ui("expression_ui_1")
    
## To be copied in the server
# mod_expression_server("expression_ui_1")
