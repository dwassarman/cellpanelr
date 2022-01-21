#' Add column containing DepMap IDs
#' 
#' Matches cell lines from the given column to DepMap IDs using the
#'   \code{\link[cellpanelr]{annotations}} data set. Matching is performed after removing all non-
#'   alphanumeric characters from the provided names. \code{NA} will be produced
#'   when the cell line cannot be found in the \code{\link[cellpanelr]{annotations}} data set or when
#'   there are non-unique cell-ID pairs.
#'
#' @param data Tibble with one column that contains cell line names
#' @param name_col Name of column containing cell line names
#'
#' @return Tibble with new column appended named depmap_id
#' @export
#'
#' @examples
#' data <- tibble::tribble(
#'   ~"CellLine", ~"viability",
#'   "MCF7", 0.96,
#'   "293T", 0.58,
#'   "K562", 0.70
#' )
#' 
#' add_ids(data, "CellLine")
add_ids <- function(data, name_col) {
  id_df <- dplyr::select(cellpanelr::annotations,
                         .data[["stripped_name"]],
                         .data[["depmap_id"]])
  data %>%
    dplyr::mutate(
      stripped_name = gsub("[^[:alnum:]]", "", .data[[name_col]]) %>% toupper()
    ) %>%
    dplyr::left_join(id_df,
                     by = c("stripped_name"),
                     suffix = c("", ".depmap")) %>%
    dplyr::select(-.data[["stripped_name"]]) %>%
    remove_multi_id(name_col) %>%
    remove_multi_cell(name_col)
}

#' Remove IDs that map to the same cell line
#'
#' @param data Tibble
#' @param name_col Name of column containing cell line names
#' @param id_col Name of column containing IDs
#'
#' @return Tibble with bad IDs changed to NA
#' 
#' @importFrom rlang :=
remove_multi_id <- function(data, name_col, id_col = "depmap_id") {
  multi_id <- data %>%
    dplyr::group_by(.data[[name_col]]) %>%
    dplyr::summarise(n = dplyr::n_distinct(.data[[id_col]])) %>%
    dplyr::filter(.data[["n"]] > 1)

  data %>%
    dplyr::mutate(
      !!id_col := ifelse(.data[[name_col]] %in% multi_id[[name_col]], NA, .data[[id_col]])
    )
}

#' Remove IDs that map to more than one cell line
#'
#' @param data Tibble
#' @param name_col Name of column containing cell line names
#' @param id_col Name of column containing IDs
#'
#' @return Tibble with bad IDs changed to NA
#' 
#' @importFrom rlang :=
remove_multi_cell <- function(data, name_col, id_col = "depmap_id") {
  multi_cell <- data %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(n = dplyr::n_distinct(.data[[name_col]])) %>%
    dplyr::filter(.data[["n"]] > 1)
  
  data %>%
    dplyr::mutate(
      !!id_col := ifelse(.data[[name_col]] %in% multi_cell[[name_col]], NA, .data[[id_col]])
    )
}