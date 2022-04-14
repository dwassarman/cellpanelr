#' Calculate Spearman correlation between response and gene expression
#'
#' @param df Tibble. Must have depmap_id column
#' @param response_col Name of column containing response data
#'
#' @return Nested tibble organized by gene_name with response vs. rna_expression
#' data in the "data" list-column and the rho & p.value values in the "model" 
#' list-column
#' @export
cor_expression <- function(df, response_col) {
  
  # Load expression data from DepMap
  exp <- cellpanelr::expression() %>%
    dplyr::select(
      .data[["depmap_id"]], 
      .data[["gene_name"]], 
      .data[["rna_expression"]]
    )
  
  # Pipeline
  df %>%
    # Join with expression data
    dplyr::inner_join(exp,
      by = "depmap_id",
      suffix = c("", ".depmap")
    ) %>%
    # Nest by gene_name
    tidyr::nest(data = -c("gene_name")) %>%
    # Do spearman correlation for each gene
    dplyr::mutate(
      model = purrr::map(.data$data, cor_spearman, "rna_expression", response_col)
    )

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
