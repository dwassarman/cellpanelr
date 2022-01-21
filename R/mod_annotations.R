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
        selectInput(ns("feature"),
                    "Feature",
                    choices = names(cellpanelr::annotations),
                    selected = "primary_disease"),
        checkboxInput(ns("log"), "Plot in log-scale"),
      ),
      mainPanel(
        plotOutput(ns("plot")) %>% shinycssloaders::withSpinner(),
      ),
    )
  )
}
    
#' annotation Server Functions
#'
#' @noRd 
#' @importFrom ggplot2 ggplot aes geom_boxplot coord_flip xlab scale_y_log10 geom_point
#' @importFrom rlang .data
mod_annotation_server <- function(id, rv){
  stopifnot(is.reactivevalues(rv))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Merge with annotations
    annotated <- reactive({
      rv$data() %>%
        dplyr::left_join(cellpanelr::annotations,
                         by = "depmap_id")
    })
    
    # Generate boxplot or scatterplot depending on feature
    output$plot <- renderPlot({
      discreet <- annotated()[[input$feature]] %>% is.character()
      if (discreet) {
        p <- annotated() %>%
            ggplot() +
            geom_boxplot(aes(
              x = stats::reorder(.data[[input$feature]],
                                 .data[["response"]],
                                 FUN = stats::median,
                                 na.rm = TRUE),
              y = .data[["response"]])) +
            coord_flip() +
            xlab(input$feature)
      } else {
        p <- annotated() %>%
          ggplot() +
          geom_point(aes(x = .data[[input$feature]],
                         y = .data[["response"]])) +
          xlab(input$feature)
      }
      
      # Scale y axis
      if (input$log) {
        p <- p + scale_y_log10()
      }
      
      p
    })
    
    
 
  })
}
    
## To be copied in the UI
# mod_annotation_ui("annotation_ui_1")
    
## To be copied in the server
# mod_annotation_server("annotation_ui_1")
