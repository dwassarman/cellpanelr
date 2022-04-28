#' Append new column containing DepMap IDs
#'
#' Matches cell line names from the given column to unique DepMap IDs. Matching
#' is performed using only alpha-numeric characters from the provided
#' names. \code{NA} will be produced when the cell line cannot be found in the
#' \code{\link{data_annotations}} data set or when there is
#' non-unique cell <-> ID mapping.
#'
#' @param data A tibble.
#' @param cell Name of column containing cell line names
#'
#' @return Provided \code{data} tibble with a new \code{depmap_id}` column
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'   CellLine = c("MCF7", "293T", "K562"),
#'   viability = c(0.96, 0.58, 0.70)
#' )
#'
#' add_ids(data, cell = "CellLine")
add_ids <- function(data, cell) {
  id_df <- dplyr::select(
    data_annotations(),
    .data[["stripped_cell_line_name"]],
    .data[["depmap_id"]]
  )
  data %>%
    dplyr::mutate(
      stripped_cell_line_name = gsub("[^[:alnum:]]", "", .data[[cell]]) %>% toupper()
    ) %>%
    dplyr::left_join(id_df,
      by = c("stripped_cell_line_name"),
      suffix = c("", ".depmap")
    ) %>%
    dplyr::select(-.data[["stripped_cell_line_name"]]) %>%
    remove_multi_id(cell) %>%
    remove_multi_cell(cell)
}

#' Remove IDs that map to the same cell line
#'
#' @param data A tibble
#' @param cell_col Name of column containing cell line names
#' @param id_col Name of column containing IDs
#'
#' @return A tibble with bad IDs changed to NA
#'
#' @importFrom rlang :=
remove_multi_id <- function(data, cell_col, id_col = "depmap_id") {
  multi_id <- data %>%
    dplyr::group_by(.data[[cell_col]]) %>%
    dplyr::summarise(n = dplyr::n_distinct(.data[[id_col]])) %>%
    dplyr::filter(.data[["n"]] > 1)

  data %>%
    dplyr::mutate(
      !!id_col := ifelse(.data[[cell_col]] %in% multi_id[[cell_col]], NA, .data[[id_col]])
    )
}

#' Remove IDs that map to more than one cell line
#'
#' @param data A tibble
#' @param cell_col Name of column containing cell line names
#' @param id_col Name of column containing IDs
#'
#' @return Tibble with bad IDs changed to NA
#'
#' @importFrom rlang :=
remove_multi_cell <- function(data, cell_col, id_col = "depmap_id") {
  multi_cell <- data %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(n = dplyr::n_distinct(.data[[cell_col]])) %>%
    dplyr::filter(.data[["n"]] > 1)

  data %>%
    dplyr::mutate(
      !!id_col := ifelse(.data[[id_col]] %in% multi_cell[[id_col]], NA, .data[[id_col]])
    )
}
