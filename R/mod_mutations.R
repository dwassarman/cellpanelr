#' mutations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mutations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    sidebarLayout(
      sidebarPanel(
        h3("Correlate with mutations"),
        textOutput(ns("matched")),
        p(strong("Note: "), "Analysis may take several minutes."),
        shinyFeedback::loadingButton(ns("go"), "Go!", class = "btn-primary btn-lg", loadingLabel = "Calculating..."),
        uiOutput(ns("side"))
      ),
      mainPanel(
        uiOutput(ns("main"))
      )
    )
  )
}

#' mutations Server Functions
#'
#' @import ggplot2
#' @noRd
mod_mutations_server <- function(id, rv) {
  stopifnot(is.reactivevalues(rv))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Dynamic UI elements

    # UI outputs
    output$side <- renderUI({
      req(gene_cor())
      tagList(
        hr(),
        h3("Results: select genes to plot"),
        actionButton(ns("clear_rows"), "Clear selections"),
        DT::DTOutput(ns("table")),
        br(),
        downloadButton(ns("dl_tsv"), "Download .tsv"),
        # downloadButton(ns("dl_rds"), "Download .rds")
      )
    })

    output$main <- renderUI({
      req(merged())
      tagList(
        col_6(
          h3("Volcano plot: all mutations"),
          h5("Hover mouse to identify gene. Right-click to save image of plot."),
          col_12(
            plotOutput(ns("vol_plot"), hover = ns("vol_hover"), height = "100%") %>% shinycssloaders::withSpinner(),
            uiOutput(ns("vol_tip"), style = "pointer-events: none"),
          )
        ),
        col_6(
          h3("Selected genes from table"),
          h5("Right-click to save image of plot."),
          col_12(
            plotOutput(ns("selected_plot"), hover = ns("selected_hover"), height = "100%") %>% shinycssloaders::withSpinner(),
            downloadButton(ns("dl_selected"), "Download plotted data", style = "float:right"),
            uiOutput(ns("selected_tip"), style = "pointer-events: none"),
          )
        ),
      )
    })

    # Let user know how many cell lines can be analyzed
    output$matched <- renderText({
      req(rv$data)
      paste0(n_mut_matched(rv$data()), " cell lines from your data with mutation data")
    })

    # Table in side bar
    output$table <- DT::renderDataTable({
      req(gene_cor())

      gene_cor() %>%
        dplyr::select(.data$gene, .data$effect, .data$adj.p) %>%
        DT::datatable(
          options = list("scrollX" = TRUE, "scrollY" = TRUE),
          rownames = FALSE,
        ) %>%
        # Round to 3 digits
        DT::formatSignif(columns = c("effect", "adj.p"), digits = 3)
    })

    ## Data set creation

    # Do correlation when button is pushed
    gene_cor <- reactive({
      # Show error message if user hasn't uploaded data
      if (is.null(rv$data)) {
        no_upload_error()
        shinyFeedback::resetLoadingButton("go")
        return(NULL)
      }

      result <-
        cor_mutations(rv$data(), rv$response_col()) %>%
        dplyr::mutate(log.p = -log10(.data$adj.p))
      shinyFeedback::resetLoadingButton("go")
      result
    }) %>% bindEvent(input$go)

    # Merged user data with mutations data
    merged <- reactive({
      req(rv$data)
      rv$data() %>%
        dplyr::inner_join(
          cellpanelr::data_mutations(),
          by = "depmap_id",
          suffix = c("", ".depmap")
        )
    }) %>% bindEvent(input$go)

    ## Select individual genes

    # Debounce selected genes to prevent plot lagging
    merged_selected <- reactive({
      req(merged())
      selected <- get_selected_genes(gene_cor(), input$table_rows_selected)

      merged() %>%
        dplyr::filter(.data$gene %in% selected) %>%
        dplyr::mutate(genotype = ifelse(.data$mutant, "Mutant", "Wild-type")) %>%
        dplyr::mutate(genotype = factor(.data$genotype, levels = c("Wild-type", "Mutant")))
    }) %>% debounce(750)

    # Clear row selections when button is pushed
    observe({
      proxy <- DT::dataTableProxy("table")
      proxy %>% DT::selectRows(NULL)
    }) %>% bindEvent(input$clear_rows)

    # Volcano plot
    output$vol_plot <- renderPlot(
      {
        req(gene_cor())
        volcano_plot(gene_cor())
      },
      height = function() {
        0.75 * session$clientData[["output_mutations_1-vol_plot_width"]]
      },
      res = 96
    )

    output$selected_plot <- renderPlot(
      {
        req(merged_selected())
        mut_plot_selected(merged_selected(), rv$response_col())
      },
      height = function() {
        0.75 * session$clientData[["output_mutations_1-selected_plot_width"]]
      },
      res = 96
    )

    # Volcano plot tooltip
    output$vol_tip <- renderUI({
      # Get hover info from plot
      req(input$vol_hover)
      mut_vol_tip(gene_cor(), input$vol_hover)
    })

    # Download data underlying plot
    output$dl_selected <- downloadHandler(
      filename = function() {
        "selected_mutations.tsv"
      },
      content = function(file) {
        vroom::vroom_write(merged_selected(), file)
      }
    )

    # Manage downloads
    output$dl_tsv <- downloadHandler(
      filename = function() {
        "all_mutations.tsv"
      },
      content = function(file) {
        vroom::vroom_write(gene_cor(), file)
      }
    )

    # output$dl_rds <- downloadHandler(
    #   filename = function() {
    #     "all_mutations.rds"
    #   },
    #   content = function(file) {
    #     rv$data() %>%
    #       dplyr::inner_join(
    #         cellpanelr::data_mutations(),
    #         by = "depmap_id",
    #         suffix = c("", ".depmap")
    #       ) %>%
    #       tidyr::nest(data = -c("gene")) %>%
    #       dplyr::inner_join(
    #         gene_cor(),
    #         by = "gene"
    #       ) %>%
    #       saveRDS(file)
    #   }
    # )
  })
}

## To be copied in the UI
# mod_mutations_ui("mutations_1")

## To be copied in the server
# mod_mutations_server("mutations_1")
