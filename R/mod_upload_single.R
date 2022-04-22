#' upload_single UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_single_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3(
          "Upload your data",
          helpButton("upload_help", ns)
        ),
        fileInput(ns("file"), "Upload file"),
        selectInput(ns("cell"), "Cell line column", choices = NULL),
        selectInput(ns("response"), "Response column", choices = NULL),
        actionButton(ns("button"), "Submit for analysis", class = "btn-primary btn-lg") %>%
          shinyjs::disabled()
      ),
      mainPanel(
        DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner()
      )
    )
  )
}

#' upload_single Server Functions
#'
#' @noRd
#' @importFrom rlang .data
mod_upload_single_server <- function(id, rv) {
  stopifnot(is.reactivevalues(rv))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Generate Tibble from uploaded data and update column selection input choices
    uploaded <- reactive({
      df <- upload_file_with_feedback(input = input, id = "file")

      choices <- names(df)
      updateSelectInput(session, "cell", choices = choices)
      updateSelectInput(session, "response", choices = choices)

      df
    })

    # Display uploaded data in a table in the main panel
    output$table <- DT::renderDT(DT::datatable(
      uploaded(),
      options = list("scrollX" = TRUE),
      rownames = FALSE,
    ))

    # Display how many cell lines the cell line column matches to
    observe({
      n_unique <- uploaded()[[input$cell]] %>% dplyr::n_distinct()
      matched <- add_ids(uploaded(), input$cell)
      n_matched <- matched[["depmap_id"]] %>% dplyr::n_distinct()

      txt <- paste0(
        n_matched,
        "/",
        n_unique,
        " names in this column can be matched to cell lines in the DepMap database."
      )
      # Must hide and reshow feedback to update with each input$cell change
      shinyFeedback::hideFeedback("cell")
      shinyFeedback::showFeedback("cell", txt)
    }) %>% bindEvent(input$cell)


    # Response column must be numeric
    response_ok <- reactive({
      response_col <- uploaded()[[input$response]]
      ok <- is.numeric(response_col)
      shinyFeedback::feedbackDanger(
        "response",
        show = !ok,
        "This column has values that are not numbers. Please select another
        column or fix the non-number values in your data file."
      )
      ok
    }) %>% bindEvent(input$response)

    # Activate Analyze button when ready
    observe({
      shinyjs::toggleState("button", condition = response_ok())
    })

    # Button is pushed, prepare data for later modules and advance navbar
    observe({
      cell <- input$cell
      response <- input$response

      # Save column names to use again down the line
      rv$cell_col <- reactive(cell)
      rv$response_col <- reactive(response)

      # Prepare data tibble for analysis
      rv$data <- reactive(prepare_data(uploaded(), cell, response))

      # Change tab to Analyze
      rv$active_tab <- reactive("Analyze")
    }) %>% bindEvent(input$button)

    # Help button
    observe(upload_help_message()) %>% bindEvent(input$upload_help)
  })
}

## To be copied in the UI
# mod_upload_single_ui("upload_single_ui_1")

## To be copied in the server
# mod_upload_single_server("upload_single_ui_1")
