#' upload_curve UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_upload_curve_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    p("[Description of curve data]"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload data"),
        selectInput(ns("cell"), "Cell line column", choices = NULL),
        selectInput(ns("doses"), "Response columns", multiple = TRUE, choices = NULL),
        # checkboxInput(ns("long_form"), "My data is in long form"),
        actionButton(ns("fit"), "Fit Curves", class = "btn-primary btn-lg") %>% shinyjs::disabled()
      ),
      mainPanel(
        DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner()
      )
    )
  )
}
    
#' upload_curve Server Functions
#'
#' @noRd 
mod_upload_curve_server <- function(id, rv){
  # Make sure rv is a list of reactive values
  stopifnot(is.reactivevalues(rv))
  
  moduleServer( id, function(input, output, session){
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
      updateSelectInput(session, "doses", choices = choices)
    }) %>% bindEvent(uploaded())
    
    # Check how many cell lines the cell line column matches to
    observe({
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
    
    # Response columns must be numeric and have numeric titles
    doses_ok <- reactive({
      # Make sure column names can be coerced to numeric
      names_ok <- suppressWarnings(all(!is.na(as.numeric(input$doses))))

      # Make sure column contents are numeric
      cols_ok <- TRUE
      for (col in input$doses) {
        cols_ok <- cols_ok && is.numeric(uploaded()[[col]])
      }
      
      # Provide user feedback
      shinyFeedback::feedbackDanger(
        "doses",
        show = !(names_ok && cols_ok),
        "Column names are non-numeric or columns contain non-numeric values."
      )
      
      names_ok && cols_ok
      
    }) %>% bindEvent(input$doses)
    
    # Activate "Fit Curves" button when ready
    observe({
      shinyjs::toggleState("fit", doses_ok())
    }) %>% bindEvent(doses_ok())
 
  })
}
    
## To be copied in the UI
# mod_upload_curve_ui("upload_curve_1")
    
## To be copied in the server
# mod_upload_curve_server("upload_curve_1")
