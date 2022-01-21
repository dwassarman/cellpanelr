#' RNA expression data for protein-coding genes
#' 
#' @format A tibble with 1,825 rows and 6 columns
#' \describe{
#'   \item{depmap_id}{unique cell line ID number assigned by DepMap}
#'   \item{gene}{gene name and entrez_id}
#'   \item{rna_expression}{log2(TPM) (transcripts per million)}
#'   \item{gene_name}{name of gene}
#'   \item{cell_line}{CCLE cell line name}
#' }
#' 
#' @source \href{https://depmap.org/portal/download/}{DepMap} release 21Q4.
#'   Accessed 2021-12-17.
"expression"