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
                    selected = "lineage")
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
#' @importFrom ggplot2 ggplot aes geom_boxplot coord_flip xlab
#' @importFrom rlang .data
mod_annotation_server <- function(id, rv){
  stopifnot(is.reactivevalues(rv))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    annotated <- reactive({
      rv$data() %>%
        dplyr::left_join(cellpanelr::annotations,
                         by = "depmap_id")
    })
    
    output$plot <- renderPlot({
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
      p
    })
 
  })
}
    
## To be copied in the UI
# mod_annotation_ui("annotation_ui_1")
    
## To be copied in the server
# mod_annotation_server("annotation_ui_1")
