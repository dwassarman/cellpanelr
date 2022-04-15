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
    shinyjs::useShinyjs(),
    # Pad page for fixed-top navbar
    tags$style(type = "text/css", "body {padding-top: 70px;}"),
    # Your application UI logic
    fluidPage(
      navbarPage(
        id = "main_navbar",
        title = "cellpanelr",
        inverse = TRUE,
        collapsible = TRUE,
        position = "fixed-top",
        tabPanel(
          id = "home",
          "Home",
          mod_home_ui("home_1")
        ),
        tabPanel(
          "Upload",
          mod_upload_single_ui("upload_single_1")
        ),
        tabPanel(
          "Analyze",
          # titlePanel("Correlate your data with DepMap data sets"),
          # br(),
          h3("Choose DepMap data set to correlate with your data"),
          tabsetPanel(
            type = "pills",
            tabPanel(
              "Cell line annotations",
              mod_annotations_ui("annotations_1")
            ),
            tabPanel(
              "Gene expression",
              mod_expression_ui("expression_1")
            )
          ),
        ),
        # Extra information and resources
        tabPanel(
          "Resources",
          mod_resources_ui("resources_1")
        ),
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "cellpanelr"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
