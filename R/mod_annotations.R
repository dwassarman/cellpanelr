#' annotation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_annotations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    sidebarLayout(
      sidebarPanel(
        h3("Plot options"),
        selectInput(
          ns("feature"),
          "Cell line annotation",
          choices = sort(c(
            "sex", "source", "sample_collection_site", "primary_or_metastasis",
            "primary_disease", "Subtype", "age", "lineage", "lineage_subtype",
            "lineage_sub_subtype", "lineage_molecular_subtype", "culture_type"
          )),
          selected = "primary_disease"
        ),
        checkboxInput(ns("log"), "Plot response in log-scale"),
        h3("Downloads"),
        downloadButton(ns("dl"), "Download .tsv")
      ),
      mainPanel(
        h4("Right-click to save image of plot."),
        col_12(
          plotOutput(ns("plot"), hover = ns("plot_hover"), height = "100%") %>% shinycssloaders::withSpinner()
        )
      )
    )
  )
}

#' annotation Server Functions
#'
#' @noRd
#' @import ggplot2
#' @importFrom rlang .data
mod_annotations_server <- function(id, rv) {
  stopifnot(is.reactivevalues(rv))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Merge with annotations
    annotated <- reactive({
      rv$data() %>%
        dplyr::left_join(data_annotations(),
          by = "depmap_id",
          suffix = c("", ".depmap")
        )
    })

    # Generate plot for main panel
    output$plot <- renderPlot(
      {
        req(rv$data)
        p <- annotations_plot(annotated(), input$feature, rv$response_col())
        # Scale y axis
        if (input$log) {
          p <- p + scale_y_log10()
        }
        p
      },
      # Resize plot based on width of window
      height = function() {
        0.75 * session$clientData[["output_annotations_1-plot_width"]]
      },
      res = 96
    )

    # Download .tsv file with annotations
    output$dl <- downloadHandler(
      filename = function() {
        "annotated_data.tsv"
      },
      content = function(file) {
        vroom::vroom_write(annotated(), file)
      }
    )

    # Disable download button if no data has been uploaded
    observe(shinyjs::toggleState("dl", condition = !is.null(rv$data)))
  })
}

## To be copied in the UI
# mod_annotations_ui("annotations_1")

## To be copied in the server
# mod_annotations_server("annotations_1")
