#' Load expression dataset
#'
#' @return A tibble with all expression data
#' @export
expression <- function() {
  message("Loading expression dataset from DepMap")
  depmap::depmap_TPM()
}