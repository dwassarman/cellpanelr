#' expression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_expression_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    sidebarLayout(
      sidebarPanel(
        h3("Correlate with gene expression"),
        textOutput(ns("matched")),
        p(strong("Note: "), "Analysis will take several minutes."),
        shinyFeedback::loadingButton(ns("go"), "Go!", class = "btn-primary btn-lg", loadingLabel = "Calculating..."),
        uiOutput(ns("side"))
      ),
      mainPanel(
        uiOutput(ns("main"))
      )
    )
  )
}

#' expression Server Functions
#'
#' @noRd
#'
#' @import ggplot2 
mod_expression_server <- function(id, rv) {
  stopifnot(is.reactivevalues(rv))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Let user know how many cell lines can be analyzed
    output$matched <- renderText({
      req(rv$data)
      n_matched <- rv$data()$depmap_id %>%
        intersect(.exp_ids) %>%
        length()
      
      paste0(n_matched, " cell lines with expression data")
    })
    
    # Do correlation when button is pushed
    nested <- reactive({
      nested <- cor_expression(rv$data(), rv$response_col())
      shinyFeedback::resetLoadingButton("go")
      nested
    }) %>% bindEvent(input$go)
    
    ## Dynamic UI Elements
    # Display correlations in side panel
    output$side <- renderUI({
      req(nested())
      tagList(
        hr(),
        h3("Results"),
        h4("Select genes to plot on right"),
        br(),
        DT::DTOutput(ns("table")),
        h3("Downloads"),
        downloadButton(ns("dl_tsv"), "Download .tsv")
      )
    })
    
    # Display plot in main panel
    output$main <- renderUI({
      req(nested())
      tagList(
        h3("Correlation plot for selected genes"),
        h5("Hover mouse to identify cell lines. [TO DO]"),
        plotOutput(ns("plot")) %>% shinycssloaders::withSpinner()
      )
    })
    
    # Display results of correlation in table
    output$table <- DT::renderDT({
      req(nested)
      DT::datatable(
        data = nested() %>%
          dplyr::select(gene_name, model) %>%
          tidyr::unnest(model),
        options = list("scrollX" = TRUE, "scrollY" = TRUE)
      )
    })

    # Manage tsv download
    output$dl_tsv <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_expression.tsv")
      },
      content = function(file) {
        req(nested())
        nested() %>%
          dplyr::select(gene_name, model) %>%
          tidyr::unnest(model) %>%
          dplyr::arrange(.data[["rho"]]) %>%
          vroom::vroom_write(file)
      }
    )
    
    # Debounce selected rows to prevent plot lagging
    selected_rows_d <- reactive(input$table_rows_selected) %>% debounce(500)

    # Display selected rows in a plot
    output$plot <- renderPlot(
      {
        # Get selected data
        req(selected_rows_d())
        selected <- nested() %>%
          dplyr::filter(dplyr::row_number() %in% selected_rows_d())

        # Plot selected data
        p <- selected %>%
          tidyr::unnest(.data$data) %>%
          ggplot(aes(x = .data$rna_expression, y = .data[[rv$response_col()]])) +
          geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", se = FALSE) +
          facet_wrap(~ .data$gene_name)

        # if (input$log) {
        #   p <- p + scale_y_log10()
        # }

        p
      },
      height = function() {
        0.75 * session$clientData[["output_expression_1-plot_width"]]
      },
      res = 96
    )
  })
}

## To be copied in the UI
# mod_expression_ui("expression_1")

## To be copied in the server
# mod_expression_server("expression_1")
