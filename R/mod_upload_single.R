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
    p("[Description of single data point]. Make note that replicates will be
      averaged automatically"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload data"),
        selectInput(ns("cell"), "Cell line column", choices = NULL),
        selectInput(ns("response"), "Response column", choices = NULL),
        actionButton(ns("button"), "Analyze", class = "btn-primary btn-lg") %>%
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
    
    # When file is entered, upload and update column selection choices
    uploaded <- reactive({
      df <- upload_file_with_feedback(input = input, id = "file")
      
      choices <- names(df)
      updateSelectInput(session, "cell", choices = choices)
      updateSelectInput(session, "response", choices = choices)
      
      df
    }) %>% bindEvent(input$file)
    
    # Display uploaded data
    output$table <- DT::renderDT(DT::datatable(
      uploaded(),
      options = list("scrollX" = TRUE)
    ))
    
    # Check how many cell lines the cell line column matches to
    observe({
      n_unique <- uploaded()[[input$cell]] %>% dplyr::n_distinct()
      matched <- add_ids(uploaded(), input$cell)
      n_matched <- matched[["depmap_id"]] %>% dplyr::n_distinct()
      
      txt <- paste0(n_matched, " / ", n_unique, " cell lines identified")
      # Must hide and reshow feedback to update with each input$cell change
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
      rv$data <- reactive({
        uploaded() %>%
          # Keep only necessary columns
          dplyr::select(
            .data[[cell]],
            .data[[response]]
          ) %>%
          # Average replicates
          dplyr::group_by(.data[[cell]]) %>%
          dplyr::summarise(!!response := mean(.data[[response]], na.rm = TRUE)) %>%
          # add depmap IDs in new column at end
          add_ids(cell_col = rv$cell_col())
      })
      
      # Change tab
      # Could put loading icon or slight delay for user satisfaction
      rv$active_tab <- reactive("Analyze")
    }) %>% bindEvent(input$button)
    
    # 
    # 
    # # Load data file
    # rv$upload <- reactive({
    #   # Enable/disable file input depending on example checkbox
    #   shinyjs::toggleElement("file", condition = !input$example)
    #   # Load example data
    #   if (input$example) {
    #     return(.dasatinib_single)
    #   }
    #   # Load user file
    #   req(input$file)
    #   allowed <- c("tsv", "csv")
    #   datapath <- input$file$datapath
    #   ext <- tools::file_ext(datapath)
    #   ok <- isTruthy(ext %in% allowed)
    #   txt <- paste0(allowed, collapse = ", ")
    #   shinyFeedback::feedbackDanger("file",
    #     show = !ok,
    #     text = paste0("Allowed file types: ", txt)
    #   )
    #   req(ok)
    #   vroom::vroom(datapath)
    # })
    # 
    # # Display uploaded data
    # output$table <- DT::renderDT(DT::datatable(
    #   rv$upload(),
    #   options = list("scrollX" = TRUE)
    # ))
    # 
    # # Update column selection options when upload file changes
    # observe({
    #   choices <- names(rv$upload())
    #   updateSelectInput(session, "cell", choices = choices)
    #   updateSelectInput(session, "response", choices = choices)
    # 
    #   # If using example data select useful columns
    #   if (input$example) {
    #     updateSelectInput(session, "cell", selected = "cell_line")
    #     updateSelectInput(session, "response", selected = "log_ic50")
    #   }
    # }) %>% bindEvent(rv$upload())
    # 
    # # Wait until button is pushed to process data
    # rv$data <- reactive({
    #   if (is.null(rv$upload())) {
    #     return(NULL)
    #   }
    #   rv$upload() %>%
    #     # Use standard column names
    #     dplyr::rename(
    #       "cell_line" = .data[[input$cell]],
    #       "response" = .data[[input$response]]
    #     ) %>%
    #     # Drop extra columns
    #     dplyr::select(
    #       .data[["cell_line"]],
    #       .data[["response"]]
    #     ) %>%
    #     # Combine replicates based on cell line name
    #     dplyr::group_by(.data[["cell_line"]]) %>%
    #     dplyr::summarise(response = mean(.data[["response"]], na.rm = TRUE)) %>%
    #     add_ids(name_col = "cell_line")
    # }) %>% bindEvent(input$go)
    # 
    # # Print message to user about number of cell lines matched
    # output$message <- renderText({
    #   upload_unique <- rv$upload()[[input$cell]] %>% dplyr::n_distinct(na.rm = TRUE)
    #   ann_len <- rv$data()[["depmap_id"]] %>% dplyr::n_distinct(na.rm = TRUE)
    #   # See if replicates are being combines
    #   m1 <- if (nrow(rv$upload()) != upload_unique) {
    #     "Averaging cell line replicates...\n"
    #   } else {
    #     ""
    #   }
    #   paste0(m1, ann_len, "/", upload_unique, " cell lines identified")
    #   
    # }) %>% bindEvent(input$go)
    # 
    # # # Use example data
    # # observe({
    # #   if (input$example) {
    # #     f <- system.file("extdata", "dasatinib_raw.tsv", package = "cellpanelr")
    # #     rv$upload <- reactive(vroom::vroom(f))
    # #   }
    # # }) %>% bindEvent(input$example)
  })
}

## To be copied in the UI
# mod_upload_single_ui("upload_single_ui_1")

## To be copied in the server
# mod_upload_single_server("upload_single_ui_1")
