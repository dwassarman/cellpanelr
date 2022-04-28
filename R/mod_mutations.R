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

    # UI outputs
    output$side <- renderUI({
      req(gene_cor())
      tagList(
        hr(),
        # h3("Select genes to plot individually"),
        h3("Results"),
        DT::DTOutput(ns("table")),
        h3("Downloads"),
        downloadButton(ns("dl_tsv"), "Download .tsv"),
        downloadButton(ns("dl_rds"), "Download .rds")
      )
    })

    output$main <- renderUI({
      req(gene_cor())
      tagList(
        h3("Volcano plot: all mutations"),
        h5("Hover mouse to identify gene. Right-click to save image of plot."),
        plotOutput(ns("plot"), hover = ns("plot_hover"), height = "100%") %>% shinycssloaders::withSpinner(),
        uiOutput(ns("hover_info"), style = "pointer-events: none"),
        # hr(),
        # h3("Individual genes: mutant vs. wild-type"),
        # plotOutput(ns("plot_2"), height = "100%") %>% shinycssloaders::withSpinner()
      )
    })

    # Let user know how many cell lines can be analyzed
    output$matched <- renderText({
      req(rv$data)
      n_matched <- rv$data()$depmap_id %>%
        intersect(.mut_ids) %>%
        length()

      paste0(n_matched, " cell lines from your data with mutation data")
    })

    # Do correlation when button is pushed
    gene_cor <- reactive({
      # Show error message if user hasn't uploaded data
      if (is.null(rv$data)) {
        no_upload_error()
        shinyFeedback::resetLoadingButton("go")
        return(NULL)
      }

      result <- cor_mutations(rv$data(), rv$response_col())
      shinyFeedback::resetLoadingButton("go")
      result
    }) %>% bindEvent(input$go)

    # Table in side bar
    output$table <- DT::renderDataTable({
      req(gene_cor())

      df <- gene_cor() %>%
        dplyr::select(.data$gene, .data$effect, .data$p.value) # %>%
      # # Change to scientific notation
      # dplyr::mutate(
      #   p.value = format(.data$p.value, scientific = TRUE, digits = 3)
      # )

      DT::datatable(
        data = df,
        options = list("scrollX" = TRUE, "scrollY" = TRUE),
        rownames = FALSE,
      ) %>%
        # Round to 3 digits
        DT::formatSignif(columns = c("effect", "p.value"), digits = 3)
    })

    # Volcano plot
    output$plot <- renderPlot(
      {
        req(gene_cor())
        gene_cor() %>%
          dplyr::filter(!is.na(.data$p.value)) %>%
          ggplot(aes(x = .data$effect, y = -log10(.data$p.value), color = .data$significant)) +
          geom_point(alpha = 0.4) +
          xlab("log2(mutant/wildtype)") +
          ylab("-log10(p.value)")
      },
      height = function() {
        0.75 * session$clientData[["output_mutations_1-plot_width"]]
      },
      res = 96
    )

    # Create tooltip for hovering over points in plot
    # See here for reference: https://gitlab.com/-/snippets/16220
    output$hover_info <- renderUI({
      req(input$plot_hover)
      hover <- input$plot_hover

      # Find point near hover
      df <- gene_cor() %>%
        dplyr::mutate(
          p.value = -log10(.data$p.value)
        )

      point <- nearPoints(df, xvar = "effect", yvar = "p.value", hover, maxpoints = 1)


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
        strong(point$gene)
      )
    })

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


    # Manage downloads
    output$dl_tsv <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_mutations.tsv")
      },
      content = function(file) {
        vroom::vroom_write(gene_cor(), file)
      }
    )

    output$dl_rds <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_expression.rds")
      },
      content = function(file) {
        rv$data() %>%
          dplyr::inner_join(
            cellpanelr::data_mutations(),
            by = "depmap_id",
            suffix = c("", ".depmap")
          ) %>%
          tidyr::nest(data = -c("gene")) %>%
          dplyr::inner_join(
            gene_cor(),
            by = "gene"
          ) %>%
          saveRDS(file)
      }
    )
  })
}

## To be copied in the UI
# mod_mutations_ui("mutations_1")

## To be copied in the server
# mod_mutations_server("mutations_1")
