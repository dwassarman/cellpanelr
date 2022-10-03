#' Calculate cell line response with gene expression
#'
#' For each of the 19.177 genes present in the
#' \code{\link{data_expression}} data set, perform Spearman's
#' correlation between given response data and RNA expression levels across
#' cell lines. The non-parametric Spearman's correlation is chosen, rather than
#' Pearson's correlation, because it does not assume a linear relationship
#' between the two variables.
#'
#' @param data A tibble.
#' @param response Column containing response values
#' @param ids Column containing DepMap IDs of cell lines
#' @param fdr False discovery rate. Number between 0 and 1 representing the
#'   likelihood that a gene predicted to be significant is actually a false-
#'   positive.
#'
#' @return A tibble with 19,177 rows and 4 columns. Each row contains the
#'   correlation values for a single gene.
#' \describe{
#'   \item{gene}{Hugo gene symbol}
#'   \item{rho}{Spearman's correlation coefficient}
#'   \item{p.value}{Probability that the null hypothesis is true (there is
#'     no relationship between gene expression and cell line response)}
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
#' cor_expression(
#'   data = df,
#'   response = "logIC50",
#'   ids = "DepMapID"
#' )
cor_expression <- function(data, response, ids = "depmap_id", fdr = 0.05) {
  data %>%
    # Join with gene expression data set
    dplyr::inner_join(
      cellpanelr::data_expression(),
      # Note order needs to be flipped in setNames (yvar, xvar)
      by = stats::setNames("depmap_id", ids),
      suffix = c("", ".depmap")
    ) %>%
    # Group by gene
    dplyr::group_by(.data$gene) %>%
    # Do spearman correlation for each gene
    dplyr::summarise(
      suppressWarnings(stats::cor.test(
        x = .data[[response]],
        y = .data$rna_expression,
        na.action = stats::na.omit(),
        method = "spearman"
      )) %>% broom::tidy()
    ) %>%
    dplyr::rename(rho = .data$estimate) %>%
    dplyr::select(.data$gene, .data$rho, .data$p.value) %>%
    dplyr::arrange(.data$p.value) %>%
    dplyr::mutate(significant = stats::p.adjust(.data$p.value, "BH") < fdr)
}


#' Benjamini-Hochberg correction for p-value significance
#'
#' Determine significance of p-values after repeated hypothesis testing using the
#'   Benjamini-Hochberg correction.
#'
#' @param x vector of p-values. MUST be ordered from smallest to largest!
#' @param fdr False discovery rate. Must be between 0 and 1.
#'
#' @return Logical vector of same length as \code{x} indicating whether the
#'   given p-values were sufficient to reject the null hypothesis with the
#'   given false discovery rate.
bh_correction <- function(x, fdr) {
  adjusted <- x * length(x) / dplyr::row_number(x)
  significant <- adjusted < fdr

  significant & dplyr::lag(significant, default = TRUE)
}
