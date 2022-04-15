#' @noRd
.expression <- function() {
  expression_file <- system.file("extdata", "expression.rds", package = "cellpanelr")
  expression <- readRDS(expression_file)
  expression %>%
    tidyr::pivot_longer(
      cols = -.data[["depmap_id"]],
      names_to = "gene_name",
      values_to = "rna_expression"
    )
}

#' Expression data set
#' @export
expression <- memoise::memoise(.expression)

#' @noRd
.annotations <- function() {
  annotations_file <- system.file("extdata", "annotations.rds", package = "cellpanelr")
  readRDS(annotations_file)
}

#' Annotations data set
#' @export
annotations <- memoise::memoise(.annotations)
