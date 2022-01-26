#' Calculate Spearman correlation between response and gene expression
#'
#' @param data Tibble
#' @param return_nested bool: Whether or not the returned tibble includes the
#'   data list-column, which holds the cell-line level expression and response
#'   data
#' @param response character; Name of column containing response data
#'
#' @return Tibble
#' @export
cor_expression <- function(data,
                           response = "response",
                           return_nested = FALSE) {
  
  # Load expression data set from DepMap
  message("Loading gene expression data...")
  exp <- depmap::depmap_TPM()
  
  # Merge given data with exp data set
  message("Searching data for cell lines...")
  merged <- data %>%
    dplyr::inner_join(exp,
                     by = "depmap_id",
                     suffix = c("", ".depmap"))
  
  # Inform user how many cell lines were successfully matched
  n_matched <- merged %>%
    dplyr::pull("depmap_id") %>%
    dplyr::n_distinct()
  message(paste0(n_matched, " cell lines found"))
  
  # # UNCOMMENT for progress bar
  # # Initialize progress bar
  # pb <- progress::progress_bar$new(total = dplyr::n_distinct(merged$gene_name),
  #                                  format = "  correlating genes [:bar] :current/:total  eta: :eta")
  # pb$tick(0)
  
  # Calculate model
  nested <- merged %>%
    tidyr::nest(data = -c("gene_name")) %>%
    # COMMENT for removing progress bar
    dplyr::mutate(model = purrr::map(.data$data, cor.spearman, "rna_expression", response) %>%
                  purrr::map(broom::tidy)) %>%
    # # UNCOMMENT for progress bar
    # dplyr::mutate(model = purrr::map(.data$data, cor.spearman_pb, "rna_expression", response, pb) %>%
    #                 purrr::map(broom::tidy)) %>%
    tidyr::unnest(.data$model) %>%
    dplyr::rename(rho = .data$estimate) %>%
    dplyr::select(.data$gene_name,
                  .data$rho,
                  .data$p.value,
                  .data$data)
  
  # TODO: filter out bad fits (genes with no expression data)

  if (return_nested) {
    nested
  } else {
    nested %>%
      dplyr::select(-.data$data)
  }
}

cor.spearman <- function(data, x, y) {
  stats::cor.test(
    x = data[[x]],
    y = data[[y]],
    method = "spearman",
    na.action = stats::na.omit(),
    exact = FALSE
  )
}

cor.spearman_pb <- function(data, x, y, pb) {
  pb$tick()
  stats::cor.test(
    x = data[[x]],
    y = data[[y]],
    method = "spearman",
    na.action = stats::na.omit(),
    exact = FALSE
  )
}
