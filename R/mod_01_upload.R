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
mod_01_upload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Load data file
    data <- reactive({
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
      data(),
      options = list("scrollX" = TRUE)
    ))
    
    # Update column selection options
    observe({
      choices <- names(data())
      updateSelectInput(session, "cell", choices = choices)
      updateSelectInput(session, "response", choices = choices)
    })
    
  })
}
    
## To be copied in the UI
# mod_01_upload_ui("01_upload_ui_1")
    
## To be copied in the server
# mod_01_upload_server("01_upload_ui_1")
