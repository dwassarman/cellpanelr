#' Calculate Spearman correlation between response and gene expression
#'
#' @param data Tibble containing data
#' @param response Column containing response values
#' @param ids Column containing DepMap IDs
#'
#' @return Tibble with two columns: \code{gene} and \code{rho}
#' @export
#' 
#' @examples 
#' # Setup example data set
#' df <- tibble::tribble(
#'   ~"CellLine", ~"DepMapID", ~"logIC50",
#'   "LS513", "ACH-000007", -2.8,
#'   "253-J", "ACH-000011", -4.04,
#'   "NIH:OVCAR-3", "ACH-000001", NA
#' )
#' 
#' correlations <- cor_expression(
#'   data = df,
#'   ids = "DepMapID",
#'   response = "logIC50"
#' )
cor_expression <- function(data, response, ids = "depmap_id") {

  data %>%
    # Join with gene expression data set
    dplyr::inner_join(
      cellpanelr::data_expression(),
      by = stats::setNames(ids, "depmap_id"),
      suffix = c("", ".depmap")
    ) %>%
    # Group by gene
    dplyr::group_by(.data[["gene"]]) %>%
    # Do spearman correlation for each gene
    dplyr::summarise(
      rho = suppressWarnings(stats::cor.test(
        x = .data[[response]],
        y = .data[["rna_expression"]],
        na.action = na.omit(),
        method = "spearman"
      )) %>%
      purrr::pluck("estimate")
    ) %>%
    dplyr::arrange(dplyr::desc(.data[["rho"]]))

  # # # UNCOMMENT for progress bar
  # # # Initialize progress bar
  # # pb <- progress::progress_bar$new(total = dplyr::n_distinct(merged$gene_name),
  # #                                  format = "  correlating genes [:bar] :current/:total  eta: :eta")
  # # pb$tick(0)
  #
  # # Calculate model
  # nested <- merged %>%
  #   tidyr::nest(data = -c("gene_name")) %>%
  #   # COMMENT for removing progress bar
  #   dplyr::mutate(model = purrr::map(.data$data, cor.spearman, "rna_expression", response) %>%
  #     purrr::map(broom::tidy)) %>%
  #   # # UNCOMMENT for progress bar
  #   # dplyr::mutate(model = purrr::map(.data$data, cor.spearman_pb, "rna_expression", response, pb) %>%
  #   #                 purrr::map(broom::tidy)) %>%
  #   tidyr::unnest(.data$model) %>%
  #   dplyr::rename(rho = .data$estimate) %>%
  #   dplyr::select(
  #     .data$gene_name,
  #     .data$rho,
  #     .data$p.value,
  #     .data$data
  #   )
  #
  # # TODO: filter out bad fits (genes with no expression data)
  #
  # if (return_nested) {
  #   nested
  # } else {
  #   nested %>%
  #     dplyr::select(-.data$data)
  # }
}

#' Do a spearman correlation and clean up output
#'
#' @param data tibble of data
#' @param x Name of column containing x variable
#' @param y Name of column containing y variable
#'
#' @return Tibble with rho and p.value columns
cor_spearman <- function(data, x, y) {
  stats::cor.test(
    x = data[[x]],
    y = data[[y]],
    method = "spearman",
    na.action = stats::na.omit(),
    exact = FALSE
  ) %>%
    broom::tidy() %>%
    dplyr::select(.data[["estimate"]], .data[["p.value"]]) %>%
    dplyr::rename(rho = .data[["estimate"]])
}

# cor.spearman_pb <- function(data, x, y, pb) {
#   pb$tick()
#   stats::cor.test(
#     x = data[[x]],
#     y = data[[y]],
#     method = "spearman",
#     na.action = stats::na.omit(),
#     exact = FALSE
#   )
# }
