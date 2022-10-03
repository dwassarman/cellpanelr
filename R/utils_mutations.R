#' Number of cell lines present in mutation data set
#'
#' @param data A tibble with "depmap_id" column
#' @param id Column containing DepMap IDs
#'
#' @return integer, number of matching cell lines
n_mut_matched <- function(data, id = "depmap_id") {
  data[[id]] %>%
    intersect(.mut_ids) %>%
    length()
}


#' Generate volcano plot effect vs. p value
#' 
#' @param data A tibble, output of cor_mutations()
volcano_plot <- function(data) {
  data %>%
    dplyr::filter(!is.na(.data$log.p)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$effect,
               y = .data$log.p,
               color = .data$significant)) +
    ggplot2::geom_point(alpha = 0.4, size = 4) +
    ggplot2::xlab(bquote(log[2]( mutant / wild-type ))) +
    ggplot2::ylab(bquote(-log[10]( adj.p ))) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed")
}


#' Generate boxplot genotype vs. response, faceted by selected gene
#'
#' @param data A tibble, user data merged with mutation data and filtered only for selected genes.
#'   Contains the columns "gene" and "genotype".
#' @param response Name of response column
#' @param log_scale Plot response in log-scale on y axis.
#'
#' @return Boxplot
mut_plot_selected <- function(data, response, log_scale = FALSE) {
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  p <- data %>%
      ggplot2::ggplot(ggplot2::aes(.data$genotype, .data[[response]])) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::geom_jitter(ggplot2::aes(color = .data$genotype), width = 0.2, alpha = 0.4) +
      ggplot2::facet_wrap(~ .data$gene) +
      ggplot2::scale_color_viridis_d(option = "C", end = 0.8) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::xlab("Genotype")
  
  if (log_scale) {
    p <- p + ggplot2::scale_y_log10()
  }
  
  p
}