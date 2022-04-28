#' Number of cell lines present in expression data set
#'
#' @param data A tibble with "depmap_id" column
#' @param id Column containing DepMap IDs
#'
#' @return integer, number of matching cell lines
n_exp_matched <- function(data, id = "depmap_id") {
  data[[id]] %>%
    intersect(.exp_ids) %>%
    length()
}



#' Render a tooltip when user hovers mouse over plot
#'
#' @param hover Hover input from plot
#' @param data A tibble of user-uploaded data
#' @param cell Name of cell line column (to be presented)
#' @param response Name of response column (y-axis on plot)
#'
#' @return wellPanel located at the cursor containing the cell line name of
#'   the nearest point
exp_tooltip <- function(hover, data, cell, response) {
  # See here for reference: https://gitlab.com/-/snippets/16220

  # Filter data to the correct facet
  filtered <- data %>%
    dplyr::filter(.data$gene == hover$panelvar1)

  # Find the nearest point
  point <- nearPoints(
    filtered,
    hover,
    xvar = "rna_expression",
    yvar = response,
    maxpoints = 1
  )

  # Don't show tooltip if there are no nearby points
  if (nrow(point) == 0) {
    return(NULL)
  }

  # Find location on the screen for the tooltip
  # (Bug this looks off when there are headers above the)
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
    strong(point[[cell]])
  )
}



#' Get list of selected genes based on row selections from table
#'
#' @param data A tibble of data used to make the table
#' @param selected list of selected row numbers
#'
#' @return Character vector of genes
get_selected_genes <- function(data, selected) {
  data %>%
    dplyr::filter(dplyr::row_number() %in% selected) %>%
    dplyr::pull(.data$gene)
}



#' Generate scatterplot expression vs. response, faceted by selected gene
#'
#' @param data A tibble, user data merged with expression data
#' @param selected Character vector with selected gene names
#' @param response Name of response column
#'
#' @return Scatter plot
#' @import ggplot2
exp_plot_selected <- function(data, selected, response) {
  data %>%
    dplyr::filter(.data$gene %in% selected) %>%
    ggplot(aes(x = .data$rna_expression, y = .data[[response]])) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ .data$gene)
}
