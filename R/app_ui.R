#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Enable packages with extra UI features
    shinyFeedback::useShinyFeedback(),
    # shinyjs::useShinyjs(),
    # Your application UI logic 
    fluidPage(
      titlePanel("cellpanelr"),
      navbarPage("Pipeline",
        navbarMenu("Upload",
          tabPanel("Single value",
            mod_01_upload_ui("01_upload_ui_1")
          ),
          tabPanel("Fit curves"),
        ),
        navbarMenu("Analyze",
          tabPanel("Annotations",
                   mod_annotation_ui("annotation_ui_1")),
          tabPanel("Gene expression",
                   mod_expression_ui("expression_ui_1")),
          tabPanel("Copy number"),
          tabPanel("Mutations"),
        ),
        tabPanel("References"),
      ),
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'cellpanelr'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

