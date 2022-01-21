#' 01_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_01_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload data"),
        selectInput(ns("cell"), "Cell line column", choices = NULL),
        selectInput(ns("response"), "Response column", choices = NULL),
        actionButton(ns("go"), "Submit"),
        textOutput(ns("message")),
      ),
      mainPanel(
        DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner()
      )
    )
  )
}
    
#' 01_upload Server Functions
#'
#' @noRd 
#' @importFrom rlang .data
mod_01_upload_server <- function(id, rv){
  stopifnot(is.reactivevalues(rv))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Load data file
    rv$upload <- reactive({
      if (is.null(input$file)) {
        return(NULL)
      }
      allowed <- c("tsv", "csv")
      datapath <- input$file$datapath
      ext <- tools::file_ext(datapath)
      ok <- isTruthy(ext %in% allowed)
      txt <- paste0(allowed, collapse = ", ")
      shinyFeedback::feedbackDanger("file",
                                    show = !ok,
                                    text = paste0("Allowed file types: ", txt))
      if (ok) {
        vroom::vroom(datapath)
      } else {
        NULL
      }
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
    })
    
    # Wait until button is pushed to process data
    rv$data <- reactive({
      if (is.null(rv$upload())) { return(NULL) }
      rv$upload() %>%
        # Use standard column names
        dplyr::rename("cell_line" = .data[[input$cell]],
                      "response" = .data[[input$response]]) %>%
        # Drop extra columns
        dplyr::select(.data[["cell_line"]], 
                      .data[["response"]]) %>%
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
  })
}
    
## To be copied in the UI
# mod_01_upload_ui("01_upload_ui_1")
    
## To be copied in the server
# mod_01_upload_server("01_upload_ui_1")
