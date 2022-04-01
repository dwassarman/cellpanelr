#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        checkboxInput(ns("example"), "Use example data?"),
        fileInput(ns("file"), "Upload data"),
        selectInput(ns("cell"), "Cell line column", choices = NULL),
        selectInput(ns("response"), "Response column", choices = NULL),
        actionButton(ns("go"), "Submit"),
        verbatimTextOutput(ns("message")),
      ),
      mainPanel(
        DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner()
      )
    )
  )
}

#' upload Server Functions
#'
#' @noRd
#' @importFrom rlang .data
mod_upload_server <- function(id, rv) {
  stopifnot(is.reactivevalues(rv))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load data file
    rv$upload <- reactive({
      # Enable/disable file input depending on example checkbox
      shinyjs::toggleElement("file", condition = !input$example)
      # Load example data
      if (input$example) {
        return(.dasatinib_single)
      }
      # Load user file
      req(input$file)
      allowed <- c("tsv", "csv")
      datapath <- input$file$datapath
      ext <- tools::file_ext(datapath)
      ok <- isTruthy(ext %in% allowed)
      txt <- paste0(allowed, collapse = ", ")
      shinyFeedback::feedbackDanger("file",
        show = !ok,
        text = paste0("Allowed file types: ", txt)
      )
      req(ok)
      vroom::vroom(datapath)
    })

    # Display uploaded data
    output$table <- DT::renderDT(DT::datatable(
      rv$upload(),
      options = list("scrollX" = TRUE)
    ))

    # Update column selection options when upload file changes
    observe({
      choices <- names(rv$upload())
      updateSelectInput(session, "cell", choices = choices)
      updateSelectInput(session, "response", choices = choices)

      # If using example data select useful columns
      if (input$example) {
        updateSelectInput(session, "cell", selected = "cell_line")
        updateSelectInput(session, "response", selected = "log_ic50")
      }
    }) %>% bindEvent(rv$upload())

    # Wait until button is pushed to process data
    rv$data <- reactive({
      if (is.null(rv$upload())) {
        return(NULL)
      }
      rv$upload() %>%
        # Use standard column names
        dplyr::rename(
          "cell_line" = .data[[input$cell]],
          "response" = .data[[input$response]]
        ) %>%
        # Drop extra columns
        dplyr::select(
          .data[["cell_line"]],
          .data[["response"]]
        ) %>%
        # Combine replicates based on cell line name
        dplyr::group_by(.data[["cell_line"]]) %>%
        dplyr::summarise(response = mean(.data[["response"]], na.rm = TRUE)) %>%
        add_ids(name_col = "cell_line")
    }) %>% bindEvent(input$go)

    # Print message to user about number of cell lines matched
    output$message <- renderText({
      upload_unique <- rv$upload()[[input$cell]] %>% dplyr::n_distinct(na.rm = TRUE)
      ann_len <- rv$data()[["depmap_id"]] %>% dplyr::n_distinct(na.rm = TRUE)
      # See if replicates are being combines
      m1 <- if (nrow(rv$upload()) != upload_unique) {
        "Averaging cell line replicates...\n"
      } else {
        ""
      }
      paste0(m1, ann_len, "/", upload_unique, " cell lines identified")
    }) %>% bindEvent(input$go)

    # # Use example data
    # observe({
    #   if (input$example) {
    #     f <- system.file("extdata", "dasatinib_raw.tsv", package = "cellpanelr")
    #     rv$upload <- reactive(vroom::vroom(f))
    #   }
    # }) %>% bindEvent(input$example)
  })
}

## To be copied in the UI
# mod_upload_ui("upload_ui_1")

## To be copied in the server
# mod_upload_server("upload_ui_1")
