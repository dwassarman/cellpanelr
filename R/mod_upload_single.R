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
    br(),
    p("[Description of single value]"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload data"),
        selectInput(ns("cell"), "Cell line column", choices = NULL),
        selectInput(ns("response"), "Response column", choices = NULL),
        actionButton(ns("button"), "Analyze", class = "btn-primary btn-lg") %>% shinyjs::disabled()
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
#' @importFrom rlang .data :=
mod_upload_single_server <- function(id, rv) {
  # Make sure rv is a list of reactive values
  stopifnot(is.reactivevalues(rv))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Upload the file
    uploaded <- reactive(
      upload_file_with_feedback(input = input, id = "file")
    )

    # Display uploaded data
    output$table <- DT::renderDT(DT::datatable(
      uploaded(),
      options = list("scrollX" = TRUE)
    ))

    # Update column selection options when uploaded file changes
    observe({
      choices <- names(uploaded())
      updateSelectInput(session, "cell", choices = choices)
      updateSelectInput(session, "response", choices = choices)
    }) %>% bindEvent(uploaded())

    # Check how many cell lines the cell line column matches to
    observe({
      # Is this a character column?
      # ok <- is.character(uploaded()[[input$cell]])
      # shinyFeedback::feedbackDanger(
      #   "cell",
      #   show = !ok,
      #   ""
      # )
      n_unique <- uploaded()[[input$cell]] %>% dplyr::n_distinct()
      matched <- add_ids(uploaded(), input$cell)
      n_matched <- matched[["depmap_id"]] %>% dplyr::n_distinct()
      txt <- paste0(n_matched, " / ", n_unique, " cell lines identified")
      shinyFeedback::hideFeedback("cell")
      shinyFeedback::showFeedback(
        "cell",
        txt
      )
    }) %>% bindEvent(input$cell)


    # Response column must be numeric
    response_ok <- reactive({
      response_col <- uploaded()[[input$response]]
      ok <- is.numeric(response_col)
      shinyFeedback::feedbackDanger(
        "response",
        show = !ok,
        "Column contains non-numeric values."
      )
      ok
    }) %>% bindEvent(input$response)

    # Activate analyze button when ready
    observe({
      shinyjs::toggleState("button", condition = response_ok())
    })

    # When button is pushed, prepare data for later modules and advance tabPanel
    observe({
      cell <- input$cell
      response <- input$response

      # Save column names to use again down the line
      rv$cell_col <- reactive(cell)
      rv$response_col <- reactive(response)

      # Prepare data tibble for analysis
      rv$data <- reactive({
        uploaded() %>%
          # Keep only necessary columns
          dplyr::select(
            # CellLine, AUC
            .data[[cell]],
            .data[[response]]
          ) %>%
          # Average replicates
          dplyr::group_by(.data[[cell]]) %>%
          dplyr::summarise(!!response := mean(.data[[response]], na.rm = TRUE)) %>%
          # add depmap IDs in new column at end
          add_ids(cell_col = rv$cell_col())
      })

      # Print message to user if replicate averaging occurs

      # Change tab
      # Could put loading icon or slight delay for user satisfaction
      rv$navbar_tab <- reactive("Analyze")
    }) %>% bindEvent(input$button)
  })
}

## To be copied in the UI
# mod_upload_single_ui("upload_single_1")

## To be copied in the server
# mod_upload_single_server("upload_single_1")
