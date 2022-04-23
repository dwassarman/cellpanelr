#' Plot data based on given cell line feature
#' 
#' Chooses the correct plot (boxplot or scatterplot) based on the type of
#' feature provided
#'
#' @param data Tibble
#' @param x Cell line feature
#' @param y Response value
#'
#' @return plot
#' 
#' @import ggplot2
annotations_plot <- function(data, x, y) {
  # Categorical feature
  if (is.character(data[[x]])) {
    data %>%
      # remove cell lines that don't have this annotation 
      dplyr::filter(!is.na(.data[[x]])) %>%
      ggplot(aes(
        # reorder annotation based on the median response
        x = stats::reorder(
          .data[[x]],
          .data[[y]],
          FUN = stats::median,
          na.rm = TRUE
        ),
        y = .data[[y]]
      )) +
      geom_boxplot() +
      # Flip for easier label reading
      coord_flip() +
      xlab(x)
   
  # Continuous feature   
  } else {
    data %>%
      ggplot(aes(
        x = .data[[x]],
        y = .data[[y]]
      )) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      xlab(x)
  }
}