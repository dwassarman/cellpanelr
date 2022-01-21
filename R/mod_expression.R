#' expression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_expression_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        verbatimTextOutput(ns("message")),
        actionButton(ns("go"), "Submit"),
        verbatimTextOutput(ns("pb")),
      ),
      mainPanel(
        # DT::DTOutput(ns("table")),
      ),
    ),
  )
}
    
#' expression Server Functions
#'
#' @noRd 
mod_expression_server <- function(id, rv){
  stopifnot(is.reactivevalues(rv))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$pb <- renderText
    # # Let user know how many cell lines can be analyzed
    # output$message <- renderText({
    #   n_matched <- rv$data() %>%
    #     dplyr::semi_join(cellpanelr::expression,
    #                      by = "depmap_id",
    #                      suffix = c("", ".dapmap")) %>%
    #   nrow()
    # 
    #   paste0(n_matched, " cell lines with expression data")
    # })
    
    # cors <- reactive({
    #   rv$data() %>%
    #     cor_expression()
    # }) %>% bindEvent(input$go)
    
    # cors <- reactive({
    #   cor.spearman <- function(data, x, y) {
    #     incProgress(1)
    #     stats::cor.test(
    #       x = data[[x]],
    #       y = data[[y]],
    #       method = "spearman",
    #       na.action = stats::na.omit(),
    #       exact = FALSE
    #     )
    #   }
    #   
    #   withProgress({
    #     merged <- rv$data() %>%
    #       dplyr::inner_join(cellpanelr::expression,
    #                         by = "depmap_id",
    #                         suffix = c("", ".depmap"))
    #     merged %>%
    #       tidyr::nest(data = -c("gene_name", "entrez_id", "gene")) %>%
    #       dplyr::mutate(model = purrr::map(.data$data, cor.spearman, "rna_expression", "response") %>%
    #                       purrr::map(broom::tidy)) %>%
    #       tidyr::unnest(.data$model) %>%
    #       dplyr::rename(rho = .data$estimate) %>%
    #       dplyr::select(.data$gene_name,
    #                     .data$entrez_id,
    #                     .data$rho,
    #                     .data$p.value)
    #   },
    #   min = 0,
    #   max = cellpanelr::expression$gene_name %>% dplyr::n_distinct(),
    #   message = "Calculating gene correlations...")
    # }) %>% bindEvent(input$go)
    
    # Display table
    # output$table <- DT::renderDT(DT::datatable(
    #   cors(),
    #   options = list("scrollX" = TRUE)))
  })
}
    
## To be copied in the UI
# mod_expression_ui("expression_ui_1")
    
## To be copied in the server
# mod_expression_server("expression_ui_1")
