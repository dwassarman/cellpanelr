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
#' Function that returns gene expression data. First call will
#' download the data from DropBox and cache it in memory for subsequent calls.
#'
#' @return Tibble with 26,713561 rows and 3 columns. The columns are
#' \code{depmap_id}, \code{gene}, and \code{rna_expression} which is
#' expressed as \code{log2(TPM + 1)} where TPM is transcripts per million. There
#' are 19,177 unique genes and 1393 cell lines included in this data set.
#'
#' @export
data_expression <- memoise::memoise(.expression)


#' @noRd
.annotations <- function() {
  readRDS(url("https://www.dropbox.com/s/kzfkzruw2dbjq88/annotations.rds?dl=1"))
}


#' Cell line annotations
#'
#' Function that returns cell line annotations. First call will download the
#' data from DropBox and cache it in memory for subsequent calls.
#'
#' @return Tibble with 1,829 rows and 15 columns. Each row represents a unique
#' DepMap cell line. Columns included are depmap_id, cell_line_name,
#' stripped_cell_line_name, sex, source, sample_collection_site,
#' primary_or_metastasis, primary_disease, Subtype, age, lineage, lineage_subtype,
#' lineage_sub_subtype, lineage_molecular_subtype, and culture_type.
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
#' Function that returns cell line annotations. First call will download the
#' data from DropBox and cache it in memory for subsequent calls.
#' 
#' @return Tibble with 34,365,583 rows and 3 columns. Columns are gene
#' <character>, depmap_id <character>, and mutant <logical>
#' @export
data_mutations <- memoise::memoise(.mutations)
