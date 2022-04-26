#' @noRd
.expression <- function() {
  readRDS(url("https://www.dropbox.com/s/9aq6vzzwvywy60k/expression.rds?dl=1")) %>%
    tidyr::pivot_longer(
      cols = -.data[["depmap_id"]],
      names_to = "gene",
      values_to = "rna_expression"
    )
}

#' Gene expression data
#'
#' Function that returns gene expression data for 19,177 genes and 1,393 cell lines.
#' First call will download the data from DropBox and cache it in memory for subsequent calls.
#'
#' @format Tibble with 26,713561 rows and 3 columns.
#' \describe{
#'   \item{depmap_id}{Unique identifier of each cell line. Formatted
#'   ACH-######.}
#'   \item{gene}{Hugo gene symbol}
#'   \item{rna_expression}{RNA expression level expressed as \code{log2(TPM + 1)}
#'   where TPM is transcripts per million.}
#' }
#'
#' @source DepMap (Broad Institute): \url{https://depmap.org/portal/}
#'
#' @export
data_expression <- memoise::memoise(.expression)


#' @noRd
.annotations <- function() {
  readRDS(url("https://www.dropbox.com/s/kzfkzruw2dbjq88/annotations.rds?dl=1"))
}


#' Cell line annotations
#'
#' Function that returns cell line annotations for 1,829 cell lines. First call
#' will download the data from DropBox and cache it in memory for subsequent
#' calls.
#'
#' @format Tibble with 1,829 rows and 15 columns. Each row represents a unique
#' DepMap cell line.
#' \describe{
#'   \item{depmap_id}{Unique identifier of each cell line. Formatted
#'   ACH-######.}
#'   \item{cell_line_name}{Common name of cell line as entered in DepMap}
#'   \item{stripped_cell_line_name}{Capitalized alpha-numeric name of cell line.
#'     Frequently used for name matching.}
#'   \item{sex}{Sex of tissue donor if known.}
#'   \item{source}{Source of cell line vial used by DepMap}
#'   \item{sample_collection_site}{Tissue collection site}
#'   \item{primary_or_metastasis}{Indicated whether tissue sample is from
#'     primary or metastatic site.}
#'   \item{primary_disease}
#'   \item{Subtype}{Cancer lineage categories}
#'   \item{age}{Age of tissue donor at time of sample collection, if known}
#'   \item{lineage}
#'   \item{lineage_subtype}
#'   \item{lineage_sub_subtype}
#'   \item{lineage_molecular_subtype}{Cancer type classifications in a standardized from}
#'   \item{culture_type}{Cell line growth conditions}
#' }
#'
#' @source DepMap (Broad Institute): \url{https://depmap.org/portal/}
#'
#' @export
data_annotations <- memoise::memoise(.annotations)


#' @noRd
.mutations <- function() {
  readRDS(url("https://www.dropbox.com/s/t4s463e0x3po9hy/mutations.rds?dl=1")) %>%
    tidyr::pivot_longer(
      cols = -.data$gene,
      names_to = "depmap_id",
      values_to = "mutant"
    )
}

#' Cell line mutations
#'
#' Function that returns mutant status for 19.537 genes in 1,759 cell lines. First call will download the
#' data from DropBox and cache it in memory for subsequent calls.
#'
#' @format Tibble with 34,365,583 rows and 3 columns.
#' \describe{
#'   \item{gene}{Hugo gene symbol}
#'   \item{depmap_id}{Unique identifier of each cell line. Formatted
#'     ACH-######}
#'   \item{mutant}{Logical indicating whether  gene is mutated in given cell line}
#' }
#'
#' @source DepMap (Broad Institute): \url{https://depmap.org/portal/}
#'
#' @export
data_mutations <- memoise::memoise(.mutations)
