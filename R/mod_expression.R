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
    p("[Description of expression analysis]"),
    br(),
    col_12(
      align = "center",
      actionButton(ns("go"), "Submit", class = "btn-primary btn-lg"),
      p(strong("Note: "), "Analysis will take several minutes."),
      textOutput(ns("matched"))
    ),
    br(),
    hr(),
    col_4(
      id = ns("table_col"),
      h3("Gene correlations"),
      h5("Select genes to plot on right."),
      DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner(),
      col_12(
        align = "right",
        downloadButton(ns("dl"), "Download .tsv", class = "btn-primary") %>%
          shinyjs::hidden()
      )
    ),
    col_8(
      id = ns("plot_col"),
      h3("Plot individual genes"),
      h5("Hover mouse to identify cell lines. [TO DO]"),
      plotOutput(ns("plot")) %>% shinycssloaders::withSpinner()
    )
    # sidebarLayout(
    #   sidebarPanel(
    #     h5("Note: this analysis may take several minutes"),
    #     checkboxInput(ns("log"), "log-scale y-axis"),
    #     downloadButton(ns("dl"), "Download tsv"),
    #   ),
    #   mainPanel(
    #     DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner(),
    #     plotOutput(ns("plot")) %>% shinycssloaders::withSpinner(),
    #   ),
    # ),
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
      shinyjs::disable("go")
      nested <- cor_expression(rv$data(), rv$response_col())
      shinyjs::enable("go")
      shinyjs::show("dl")
      nested
    }) %>%
      bindEvent(input$go)
    
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
    
    # # Things that need to happen when Submit button is pushed
    # observe({
    #   # Save nested data
    #   rv$exp_nested <- reactive(cor_expression(rv$data(),
    #     response = rv$response(),
    #     return_nested = TRUE
    #   ))
    #   # Save flat data
    #   rv$exp <- reactive({
    #     rv$exp_nested() %>%
    #       dplyr::select(-.data$data)
    #   })
    # }) %>% bindEvent(input$go)
    # 
    # # Display table
    # output$table <- DT::renderDT({
    #   if (isTruthy(rv$exp)) {
    #     DT::datatable(rv$exp(),
    #       options = list("scrollX" = TRUE, "scrollY" = TRUE)
    #     )
    #   } else {
    #     NULL
    #   }
    # })

    # Manage data download
    output$dl <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_expression.tsv")
      },
      content = function(file) {
        req(nested)
        nested() %>%
          dplyr::select(gene_name, model) %>%
          tidyr::unnest(model) %>%
          vroom::vroom_write(file)
      }
    )

    # Display selected row in separate table
    output$plot <- renderPlot(
      {
        # Get selected data
        selected_rows <- input$table_rows_selected
        req(selected_rows)
        selected <- nested() %>%
          dplyr::filter(dplyr::row_number() %in% selected_rows)

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
# mod_expression_ui("expression_ui_1")

## To be copied in the server
# mod_expression_server("expression_ui_1")
