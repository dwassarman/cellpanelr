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
        # h3("Select genes to plot individually"),
        h3("Results: select genes to plot"),
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

    # # Create tooltip for hovering over points in plot
    # # See here for reference: https://gitlab.com/-/snippets/16220
    # output$hover_info <- renderUI({
    #   req(input$plot_hover)
    #   hover <- input$plot_hover
    #
    #   # Find point near hover
    #   df <- gene_cor() %>%
    #     dplyr::mutate(
    #       adj.p = -log10(.data$adj.p)
    #     )
    #
    #   point <- nearPoints(df, xvar = "effect", yvar = "adj.p", hover, maxpoints = 1)
    #
    #
    #   if (nrow(point) == 0) {
    #     return(NULL)
    #   }
    #
    #   left_px <- hover$coords_css$x
    #   top_px <- hover$coords_css$y
    #
    #   # create style property for tooltip
    #   # background color is set so tooltip is a bit transparent
    #   # z-index is set so we are sure are tooltip will be on top
    #   style <- paste0(
    #     "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    #     "left:", left_px, "px; top:", top_px, "px;"
    #   )
    #
    #   # actual tooltip created as wellPanel
    #   wellPanel(
    #     style = style,
    #     strong(point$gene)
    #   )
    # })
    #
    # # Create tooltip for hovering over points in plot
    # # See here for reference: https://gitlab.com/-/snippets/16220
    # output$selected_hover_info <- renderUI({
    #   req(input$selected_hover)
    #   print("In selected_hover_info")
    #   hover <- input$selected_hover
    #   print("hover:")
    #   print(hover)
    #
    #   # Filter data to the correct facet
    #   filtered <- merged() %>%
    #     dplyr::mutate(genotype = ifelse(.data$mutant, "Mutant", "Wild-type")) %>%
    #     dplyr::mutate(genotype = factor(genotype, levels = c("Wild-type", "Mutant"))) %>%
    #     dplyr::filter(.data$gene == hover$panelvar1)
    #
    #   # Find the nearest point
    #   point <- nearPoints(
    #     filtered,
    #     hover,
    #     xvar = "genotype",
    #     yvar = rv$response_col(),
    #     maxpoints = 1
    #   )
    #   print("point:")
    #   print(point)
    #
    #   # Don't show tooltip if there are no nearby points
    #   if (nrow(point) == 0) {
    #     return(NULL)
    #   }
    #
    #   # Find location on the screen for the tooltip
    #   # (Bug this looks off when there are headers above the)
    #   left_px <- hover$coords_css$x
    #   top_px <- hover$coords_css$y
    #
    #   # create style property for tooltip
    #   # background color is set so tooltip is a bit transparent
    #   # z-index is set so we are sure are tooltip will be on top
    #   style <- paste0(
    #     "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    #     "left:", left_px, "px; top:", top_px, "px;"
    #   )
    #
    #   print("style:")
    #   print(style)
    #
    #   print("cell:")
    #   print(point[[rv$cell_col()]])
    #
    #   # actual tooltip created as wellPanel
    #   wellPanel(
    #     style = style,
    #     strong(point[[rv$cell_col()]])
    #   )
    #
    #   # exp_tooltip(input$selected_hover, merged(), rv$cell_col(), rv$response_col())
    #   # hover <- input$selected_hover
    #   #
    #   # # Find point near hover
    #   # df <- merged()
    #   #
    #   # point <- nearPoints(df, xvar = "mutant", yvar = rv$response_col(), hover, maxpoints = 1)
    #   #
    #   #
    #   # if (nrow(point) == 0) {
    #   #   return(NULL)
    #   # }
    #   #
    #   # left_px <- hover$coords_css$x
    #   # top_px <- hover$coords_css$y
    #   #
    #   # # create style property for tooltip
    #   # # background color is set so tooltip is a bit transparent
    #   # # z-index is set so we are sure are tooltip will be on top
    #   # style <- paste0(
    #   #   "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    #   #   "left:", left_px, "px; top:", top_px, "px;"
    #   # )
    #   #
    #   # # actual tooltip created as wellPanel
    #   # wellPanel(
    #   #   style = style,
    #   #   "Test",
    #   #   # strong(point[[rv$cell_col()]])
    # })

    # output$plot_2 <- renderPlot({
    #   req(gene_cor())
    #   gene_cor() %>%
    #     # DEBUG
    #     dplyr::filter(gene %in% c("PLXND1", "PUS7", "KRAS")) %>%
    #     dplyr::inner_join(data_mutations(), by = "gene") %>%
    #     dplyr::inner_join(rv$data(), by = "depmap_id") %>%
    #     dplyr::filter(!is.na(.data$p.value)) %>%
    #     ggplot(aes(x = .data$mutant, y = .data[[rv$response_col()]])) +
    #     geom_violin() +
    #     facet_wrap(~ .data$gene)
    # },
    # height = function() {
    #   0.75 * session$clientData[["output_mutations_1-plot_width"]]
    # },
    # res = 96
    # )

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
