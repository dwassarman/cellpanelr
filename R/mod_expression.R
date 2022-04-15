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

      paste0(n_matched, " cell lines from your data with expression data")
    })

    # Do correlation when button is pushed
    gene_cor <- reactive({
      result <- cor_expression(rv$data(), rv$response_col())
      shinyFeedback::resetLoadingButton("go")
      result
    }) %>% bindEvent(input$go)

    ## Dynamic UI Elements
    # Display correlations in side panel
    output$side <- renderUI({
      req(gene_cor())
      tagList(
        hr(),
        h3("Results"),
        h4("Select genes to plot on right"),
        br(),
        DT::DTOutput(ns("table")),
        h3("Downloads"),
        downloadButton(ns("dl_tsv"), "Download .tsv"),
        downloadButton(ns("dl_rds"), "Download .RData")
      )
    })

    # Display plot in main panel
    output$main <- renderUI({
      req(nested())
      tagList(
        h3("Correlation plot for selected genes"),
        h5("Hover mouse to identify cell lines."),
        h5("Right-click to save image of plot."),
        plotOutput(ns("plot"), hover = ns("plot_hover")) %>% shinycssloaders::withSpinner(),
        uiOutput(ns("hover_info"), style = "pointer-events: none")
      )
    })

    # Display results of correlation in table
    output$table <- DT::renderDT({
      req(gene_cor())
      # Create data table
      DT::datatable(
        data = gene_cor(),
        options = list("scrollX" = TRUE, "scrollY" = TRUE)
      ) %>%
        # Round to 3 digits
        DT::formatRound(columns = "rho", digits = 3)
    })

    # Create tooltip for hovering over points in plot
    # See here for reference: https://gitlab.com/-/snippets/16220
    output$hover_info <- renderUI({
      req(input$plot_hover)
      hover <- input$plot_hover
      # Find point near hover
      df <- nested() %>%
        dplyr::filter(.data[[hover$mapping$panelvar1]] == hover$panelvar1) %>%
        tidyr::unnest(.data[["data"]])
      point <- nearPoints(df, hover, xvar = "rna_expression", yvar = rv$response_col(), threshold = 5, maxpoints = 1, addDist = TRUE)

      if (nrow(point) == 0) {
        return(NULL)
      }

      left_px <- hover$coords_css$x
      top_px <- hover$coords_css$y

      # create style property for tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0(
        "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
        "left:", left_px, "px; top:", top_px, "px;"
      )

      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        strong(point[[rv$cell_col()]])
      )
    })


    # # Hover tool
    # output$hover_text <- renderPrint({
    #   req(input$hover)
    #   # nearPoints(nested(), input$hover)
    #   h <- input$hover
    #   df <- nested() %>%
    #     dplyr::filter(.data[[h$mapping$panelvar1]] == h$panelvar1) %>%
    #     tidyr::unnest(.data[["data"]])
    #   nearPoints(df, h, xvar = "rna_expression", yvar = rv$response_col())
    #   # df
    # })

    # Manage tsv download
    output$dl_tsv <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_expression.tsv")
      },
      content = function(file) {
        req(nested())
        nested() %>%
          dplyr::select(.data[["gene"]], .data[["model"]]) %>%
          tidyr::unnest(.data[["model"]]) %>%
          dplyr::arrange(.data[["rho"]]) %>%
          vroom::vroom_write(file)
      }
    )

    # .RData download
    output$dl_rds <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_expression.rds")
      },
      content = function(file) {
        nested() %>%
          saveRDS(file, compress = FALSE)
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
          facet_wrap(~ .data$gene)

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
