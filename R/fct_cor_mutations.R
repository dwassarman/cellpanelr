#' Correlate cell line response with gene mutations
#' 
#' For each of the 19.537 genes present in the
#' \code{\link{data_mutations}} data set, perform a Mann-Whitney test
#' \code{\link[stats]{wilcox.test}} to determine whether cell lines containing a mutant version of the gene respond
#' differently than cell lines with the wild-type version of the gene. The Mann-Whitney
#' test is chosen, rather than the Student's t-test, because it does not
#' assume that the response values in the two groups are normally distributed.
#'
#' @param data A tibble.
#' @param response Column containing response data
#' @param ids Column containing DepMap IDs of cell lines
#' @param fdr False discovery rate. Number between 0 and 1 representing the
#'   likelihood that a gene predicted to be significant is actually a false-positive.
#'
#' @return A tibble with 19,537 rows and 4 columns. Each row contains the
#'   correlation values for a single gene.
#' \describe{
#'   \item{gene}{Hugo gene symbol}
#'   \item{effect}{The relative increase or decrease in cell line response
#'     associated with mutation of the gene. Calculated as log2( mutant response / wildtype response )}
#'   \item{p.value}{Probability that the null hypothesis is true (there is
#'     no relationship between gene mutation and cell line response)}
#'   \item{significant}{Whether the correlation is deemed significant after
#'     multiple hypothesis correction with the given false discovery rate}
#' }
#'
#' @export
#' 
#' @examples 
#' # Setup example data set
#' df <- tibble::tibble(
#'   CellLine = c("LS513", "253-J", "NIH:OVCAR-3"),
#'   DepMapID = c("ACH-000007", "ACH-000011", "ACH-000001"),
#'   logIC50 = c(-2.8, -4.04, -6.23)
#' )
#'
#' cor_mutations(
#'   data = df,
#'   response = "logIC50",
#'   ids = "DepMapID",
#'   fdr = 0.01
#' )
cor_mutations <- function(data, response, ids = "depmap_id", fdr = 0.05) {
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
        # For genes with no mutant or wild-type in data set, let p.value = NA
        dplyr::n_distinct(.data$mutant) == 2,
        # Use Mann-Whitney (Wilcoxon) U test b/c it doesn't
        # assume normal distribution of data
        stats::wilcox.test(.data[[response]] ~ .data$mutant)[["p.value"]],
        NA
      ),
    ) %>%
    dplyr::arrange(.data$p.value) %>%
    # Adjust p-values using Benjamini-Hochberg method (https://www2.rockefeller.edu/qbio_sc/resources/l04.pdf)
    dplyr::mutate(significant = stats::p.adjust(.data$p.value, method = "BH") < fdr)
}
