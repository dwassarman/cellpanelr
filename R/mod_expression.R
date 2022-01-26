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
        checkboxInput(ns("log"), "log-scale y-axis"),
        downloadButton(ns("dl"), "Download tsv"),
      ),
      mainPanel(
        DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner(),
        plotOutput(ns("plot")) %>% shinycssloaders::withSpinner(),
      ),
    ),
  )
}
    
#' expression Server Functions
#'
#' @noRd 
#' 
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth scale_y_log10 facet_wrap
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
                      options = list("scrollX" = TRUE, "scrollY" = TRUE))
      } else {
        NULL
      }
    })
    
    # Manage data download
    output$dl <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_expression.tsv")
      },
      content = function(file) {
        vroom::vroom_write(rv$exp(), file)
      }
    )
    
    # Display selected row in separate table
    output$plot <- renderPlot({
      # Get selected data
      selected_rows <- input$table_rows_selected
      req(selected_rows)
      selected <- rv$exp_nested() %>%
        dplyr::filter(dplyr::row_number() %in% selected_rows)
      
      # Plot selected data
      p <- selected %>%
        tidyr::unnest(.data$data) %>%
        ggplot(aes(x = .data$rna_expression, y = .data$response)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) +
        facet_wrap(~.data$gene_name)
      
      if (input$log) { p <- p + scale_y_log10() }
      
      p
    }, res = 96)
    
  })
}
    
## To be copied in the UI
# mod_expression_ui("expression_ui_1")
    
## To be copied in the server
# mod_expression_server("expression_ui_1")
