#' Find mutations that are associated with cell line response
#'
#' @param data Tibble containing data
#' @param response Name of column containing response data
#' @param ids Name of column containing depmap_ids of cell lines
#'
#' @return Tibble with effect size and significance of each gene. Contains the
#' columns: gene, effect (log2(mutant/wildtype)), p.value (Mann-Whitney U test),
#' and significance (determined using Benjamin-Hochberg correction and alpha=
#' 0.05).
#' 
#' @export
cor_mutations <- function(data, response, ids = "depmap_id") {
  data %>%
  # Join with mutation data set
    dplyr::inner_join(
      cellpanelr::data_mutations(),
      # Note order needs to be flipped in setNames (yvar, xvar)
      by = stats::setNames("depmap_id", ids),
      suffix = c("", ".depmap")
    ) %>%
    dplyr::group_by(.data$gene) %>%
    dplyr::summarise(
      # effect is log2(mutant/wildtype)
      effect = log2(mean(.data[[response]][.data$mutant == TRUE]) /
        mean(.data[[response]][.data$mutant == FALSE])),
      p.value = ifelse(
        # For genes with no mutant in data set, let p.value = NA
        sum(.data$mutant) > 0,
        # Use Mann-Whitney (Wilcoxon) U test b/c it doesn't
        # assume normal distribution of data
        stats::wilcox.test(.data[[response]] ~ .data$mutant)[["p.value"]],
        NA
      ),
    ) %>%
    dplyr::arrange(.data$p.value) %>%
    # Adjust p-values using Benjamini-Hochberg method (https://www2.rockefeller.edu/qbio_sc/resources/l04.pdf)
    dplyr::mutate(
      p.adjusted = .data$p.value * length(.data$p.value) / dplyr::row_number(),
      significant = .data$p.adjusted < 0.05,
      significant = ifelse(
        dplyr::row_number() == 1,
        .data$significant,
        .data$p.adjusted < 0.05 & dplyr::lag(.data$significant)
      ),
      p.adjusted = NULL
    )
}