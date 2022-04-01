#' Annotations from DepMap cell lines.
#'
#' @format A tibble with 1,825 rows and 26 columns
#' \describe{
#'   \item{depmap_id}{unique ID number assigned by DepMap}
#'   \item{cell_line}{common name of the cell line}
#'   \item{stripped_name}{name in all caps with only alphanumeric
#'     characters, useful for name-matching}
#'   \item{lineage}{tissue of origin}
#'   \item{lineage_subtype}{cancer type classification}
#' }
#'
#' @source \href{https://depmap.org/portal/download/}{DepMap} release 21Q4.
#'   Accessed 2021-12-17.
"annotations"
