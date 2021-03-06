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
        p(strong("Note: "), "Analysis may take several minutes."),
        shinyFeedback::loadingButton(
          ns("go"),
          "Go!",
          class = "btn-primary btn-lg",
          loadingLabel = "Calculating..."
        ),
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

    ## Dynamic UI Elements
    # Display correlations in side panel
    output$side <- renderUI({
      req(gene_cor())
      tagList(
        hr(),
        h3("Select genes to plot on right"),
        br(),
        DT::DTOutput(ns("table")),
        h3("Downloads"),
        downloadButton(ns("dl_tsv"), "Download .tsv"),
        downloadButton(ns("dl_rds"), "Download .rds")
      )
    })

    # Display plot in main panel
    output$main <- renderUI({
      req(merged())
      tagList(
        fluidRow(
          h3("Correlation plot of selected genes"),
          h5("Hover mouse to identify cell lines. Right-click to save image of plot."),
        ),
        plotOutput(ns("plot"), hover = ns("plot_hover"), height = "100%") %>% shinycssloaders::withSpinner(),
        uiOutput(ns("hover_info"), style = "pointer-events: none")
      )
    })

    # Let user know how many cell lines can be analyzed
    output$matched <- renderText({
      req(rv$data)
      paste0(n_exp_matched(rv$data()), " cell lines from your data with expression data")
    })

    # Do correlation when button is pushed
    gene_cor <- reactive({
      # Show error message if user hasn't uploaded data
      if (is.null(rv$data)) {
        no_upload_error()
        shinyFeedback::resetLoadingButton("go")
        return(NULL)
      }

      result <- cor_expression(rv$data(), rv$response_col())
      shinyFeedback::resetLoadingButton("go")
      result
    }) %>% bindEvent(input$go)

    # Merged user data with expression data
    merged <- reactive({
      req(rv$data)
      rv$data() %>%
        dplyr::inner_join(
          cellpanelr::data_expression(),
          by = "depmap_id",
          suffix = c("", ".depmap")
        )
    }) %>% bindEvent(input$go)

    # Display results of correlation in table
    output$table <- DT::renderDT({
      req(gene_cor())
      # Create data table
      DT::datatable(
        data = gene_cor(),
        options = list("scrollX" = TRUE, "scrollY" = TRUE),
        rownames = FALSE
      ) %>%
        # Round to 3 digits
        DT::formatRound(columns = "rho", digits = 3)
    })

    # Debounce selected genes to prevent plot lagging
    selected_genes <- reactive({
      req(gene_cor())
      get_selected_genes(gene_cor(), input$table_rows_selected)
    }) %>% debounce(750)


    # Plot selected rows
    output$plot <- renderPlot(
      {
        # Make plot
        req(selected_genes())
        exp_plot_selected(merged(), selected_genes(), rv$response_col())
      },
      # Adjust height to maintain aspect ratio
      height = function() {
        0.75 * session$clientData[["output_expression_1-plot_width"]]
      },
      res = 96
    )

    # Create tooltip for hovering over points in plot
    output$hover_info <- renderUI({
      req(input$plot_hover)
      exp_tooltip(input$plot_hover, merged(), rv$cell_col(), rv$response_col())
    })

    # Manage tsv download
    output$dl_tsv <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_expression.tsv")
      },
      content = function(file) {
        vroom::vroom_write(gene_cor(), file)
      }
    )

    # .RData download
    output$dl_rds <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_expression.rds")
      },
      content = function(file) {
        merged() %>%
          dplyr::left_join(
            gene_cor(),
            by = "gene"
          ) %>%
          tidyr::nest(data = -c("gene", "rho")) %>%
          saveRDS(file)
      }
    )
  })
}

## To be copied in the UI
# mod_expression_ui("expression_1")

## To be copied in the server
# mod_expression_server("expression_1")
