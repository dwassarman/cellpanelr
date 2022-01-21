#' Calculate Spearman correlation between response and gene expression
#'
#' @param data Tibble
#' @param non_coding bool; Whether or not to include non-protein-coding transcripts
#' @param response character; Name of column containing response data
#' @param nested_output bool; Whether to include data list-column in return tibble
#'
#' @return Tibble
#' @export
cor_expression <- function(data,
                           response = "response",
                           # by = "depmap_id",
                           non_coding = FALSE,
                           nested_output = FALSE) {
  
  # TODO: update with non-coding data set
  if (non_coding) {
    exp <- NULL
  } else {
    exp <- cellpanelr::expression
  }
  
  # Merge given data with exp data set
  merged <- data %>%
    dplyr::inner_join(exp,
                     by = "depmap_id",
                     suffix = c("", ".depmap"))
  
  # Inform user how many cell lines were successfully matched
  n_matched <- merged %>%
    dplyr::pull("cell_line") %>%
    dplyr::n_distinct()
  message(paste0(n_matched, "/", dplyr::n_distinct(data$cell_line), " cell lines found in expression data set"))

  # Calculate model
  nested <- merged %>%
    tidyr::nest(data = -c("gene_name", "entrez_id", "gene")) %>%
    dplyr::mutate(model = purrr::map(.data$data, cor.spearman, "rna_expression", response) %>%
                    purrr::map(broom::tidy)) %>%
    tidyr::unnest(.data$model) %>%
    dplyr::rename(rho = .data$estimate) %>%
    dplyr::select(.data$gene_name,
                  .data$entrez_id,
                  .data$rho,
                  .data$p.value,
                  .data$data)

  if (nested_output) {
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