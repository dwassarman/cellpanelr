#' mutations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mutations_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    sidebarLayout(
      sidebarPanel(
        h3("Correlate with mutations"),
        textOutput(ns("matched")),
        p(strong("Note: "), "Analysis may take several minutes."),
        shinyFeedback::loadingButton(ns("go"), "Go!", class = "btn-primary btn-lg", loadingLabel = "Calculating..."),
        DT::DTOutput(ns("table"))
      ),
      mainPanel(
        plotOutput(ns("plot"), hover = ns("plot_hover")) %>% shinycssloaders::withSpinner(),
        uiOutput(ns("hover_info"), style = "pointer-events: none")
      )
    )
  )
}
    
#' mutations Server Functions
#'
#' @import ggplot2
#' @noRd 
mod_mutations_server <- function(id, rv){
  stopifnot(is.reactivevalues(rv))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Let user know how many cell lines can be analyzed
    output$matched <- renderText({
      req(rv$data)
      n_matched <- rv$data()$depmap_id %>%
        intersect(.mut_ids) %>%
        length()
      
      paste0(n_matched, " cell lines from your data with mutation data")
    })
    
    # Do correlation when button is pushed
    gene_cor <- reactive({
      result <- cor_mutations(rv$data(), rv$response_col())
      shinyFeedback::resetLoadingButton("go")
      result
    }) %>% bindEvent(input$go)
    
    # Table in side bar
    output$table <- DT::renderDataTable({
      req(gene_cor())
      
      df <- gene_cor() %>%
        dplyr::select(.data$gene, .data$effect, .data$p.value) #%>%
        # # Change to scientific notation
        # dplyr::mutate(
        #   p.value = format(.data$p.value, scientific = TRUE, digits = 3)
        # )
      
      DT::datatable(
        data = df,
        options = list("scrollX" = TRUE, "scrollY" = TRUE),
        rownames = FALSE,
      ) %>%
      # Round to 3 digits
      DT::formatSignif(columns = c("effect", "p.value"), digits = 3)
    })
    
    
    # Volcano plot
    output$plot <- renderPlot({
      req(gene_cor())
      gene_cor() %>%
        dplyr::filter(!is.na(.data$p.value)) %>%
        ggplot(aes(x = .data$effect, y = -log10(.data$p.value), color = .data$significant)) +
        geom_point(alpha = 0.4) +
        xlab("log2(mutant/wildtype)") +
        ylab("-log10(p.value)")
    },
    height = function() {
      0.75 * session$clientData[["output_mutations_1-plot_width"]]
    },
    res = 96
    )
    
    # Create tooltip for hovering over points in plot
    # See here for reference: https://gitlab.com/-/snippets/16220
    output$hover_info <- renderUI({
      req(input$plot_hover)
      hover <- input$plot_hover
      
      # Find point near hover
      df <- gene_cor() %>%
        dplyr::mutate(
          p.value = -log10(.data$p.value)
        )
      
      point <- nearPoints(df, xvar = "effect", yvar = "p.value", hover, maxpoints = 1)
      
      
      if (nrow(point) == 0) {
        return(NULL)
      }
      
      left_px <- hover$coords_css$x
      top_px <- hover$coords_css$y
      
      # create style property for tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0(
        "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
        "left:", left_px, "px; top:", top_px, "px;"
      )
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        strong(point$gene)
      )
    })
    
    
  })
}
    
## To be copied in the UI
# mod_mutations_ui("mutations_1")
    
## To be copied in the server
# mod_mutations_server("mutations_1")
