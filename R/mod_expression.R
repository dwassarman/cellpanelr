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
        verbatimTextOutput(ns("matched")),
        actionButton(ns("go"), "Submit"),
        h5("Note: this analysis may take several minutes"),
        downloadButton(ns("dl"), "Download tsv"),
      ),
      mainPanel(
        DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner(),
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
    
    # Let user know how many cell lines can be analyzed
    output$matched <- renderText({
      n_matched <- rv$data()$depmap_id %>%
        intersect(.exp_ids) %>%
        length()

      paste0(n_matched, " cell lines with expression data")
    })
    
    # Things that need to happen when Submit button is pushed
    observe({
      # Save nested data
      rv$exp_nested <- reactive(cor_expression(rv$data(),
                                              response = "response",
                                              return_nested = TRUE))
      # Save flat data
      rv$exp <- reactive({
        rv$exp_nested() %>%
          dplyr::select(-.data$data)
     })
     
    }) %>% bindEvent(input$go)
    
    # Display table
    output$table <- DT::renderDT({
      if (isTruthy(rv$exp)) {
        DT::datatable(rv$exp(),
                      options = list("scrollX" = TRUE))
      } else {
        NULL
      }
    })
    
    # Manage data download
    output$dl <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "-expression.tsv")
      },
      content = function(file) {
        vroom::vroom_write(rv$exp(), file)
      }
    )
    
    # Toggle state of download button
    observe({
      st <- isTruthy(rv$exp)
      shinyjs::toggleState("dl", condition = st)
    })
    
  })
}
    
## To be copied in the UI
# mod_expression_ui("expression_ui_1")
    
## To be copied in the server
# mod_expression_server("expression_ui_1")
