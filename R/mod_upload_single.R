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
        h4("Provide a data set"),
        checkboxInput(ns("example"), tags$b("Use example data")),
        p("--or--", align = "center"),
        fileInput(ns("file"), "Upload file"),
        h4("Select data columns"),
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
    
    # Generate Tibble from uploaded data or example data
    uploaded <- reactive({
      # df <- upload_file_with_feedback(input = input, id = "file")
      if (input$example) {
        df <- .dasatinib_single
      } else {
        df <- upload_file_with_feedback(input, "file")
      }
      
      choices <- names(df)
      updateSelectInput(session, "cell", choices = choices)
      updateSelectInput(session, "response", choices = choices)
      
      df
    })
    
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
    
  })
}

## To be copied in the UI
# mod_upload_single_ui("upload_single_ui_1")

## To be copied in the server
# mod_upload_single_server("upload_single_ui_1")
