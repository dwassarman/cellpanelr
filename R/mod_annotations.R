#' annotation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_annotation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    p("[Description of \"cell line annotations\"]"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns("feature"),
          "Select a cell line annotation",
          choices = names(cellpanelr::annotations),
          selected = "primary_disease"
        ),
        checkboxInput(ns("log"), "Plot in log-scale"),
        downloadButton(ns("dl"), "Download with annotations"),
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
#' @import ggplot2
#' @importFrom rlang .data
mod_annotation_server <- function(id, rv) {
  stopifnot(is.reactivevalues(rv))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Merge with annotations
    annotated <- reactive({
      rv$data() %>%
        dplyr::left_join(cellpanelr::annotations,
          by = "depmap_id",
          suffix = c("", ".depmap")
        )
    })

    # Generate boxplot or scatterplot depending on feature
    output$plot <- renderPlot(
      {
        discreet <- annotated()[[input$feature]] %>% is.character()
        if (discreet) {
          p <- annotated() %>%
            ggplot() +
            geom_boxplot(aes(
              x = stats::reorder(.data[[input$feature]],
                .data[[rv$response_col()]],
                FUN = stats::median,
                na.rm = TRUE
              ),
              y = .data[[rv$response_col()]]
            )) +
            coord_flip() +
            xlab(input$feature)
        } else {
          p <- annotated() %>%
            ggplot() +
            geom_point(aes(
              x = .data[[input$feature]],
              y = .data[[rv$response_col]]
            )) +
            xlab(input$feature)
        }

        # Scale y axis
        if (input$log) {
          p <- p + scale_y_log10()
        }

        p
      },
      height = function() {
        0.75 * session$clientData[["output_annotation_1-plot_width"]]
      },
      res = 96
    )

    output$dl <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_annotated.tsv")
      },
      content = function(file) {
        vroom::vroom_write(annotated(), file)
      }
    )
  })
}

## To be copied in the UI
# mod_annotation_ui("annotation_ui_1")

## To be copied in the server
# mod_annotation_server("annotation_ui_1")
