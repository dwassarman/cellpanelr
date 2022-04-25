#' Upload a file
#'
#' Can handle .csv, .tsv, .xls, and .xlsx files
#'
#' @param file filepath
#'
#' @return Tibble
load_file <- function(file) {
  ext <- tools::file_ext(file)
  switch(ext,
    csv = vroom::vroom(file, delim = ","),
    tsv = vroom::vroom(file, delim = "\t"),
    xls = readxl::read_xls(file, na = c("", "NA")),
    xlsx = readxl::read_xlsx(file, na = c("", "NA")),
    validate("Invalid file type. Please upload a .csv, .tsv, .xls, or .xlsx file.")
  )
}



#' Prepare uploaded data for analysis modules
#'
#' Removes extra columns, averages the response values between repeated cell
#' line rows, and adds DepMap IDs in a new column
#'
#' @param data uploaded data tibble
#' @param cell name of column containing cell line names
#' @param response name of column containing response values
#'
#' @return Tibble with only 3 columns: cell line, response, and "depmap_id"
prepare_data <- function(data, cell, response) {
  data %>%
    # Keep only necessary columns
    dplyr::select(.data[[cell]], .data[[response]]) %>%
    # Get rid of rows with no response data
    dplyr::filter(!is.na(.data[[response]])) %>%
    # Average replicates
    dplyr::group_by(.data[[cell]]) %>%
    dplyr::summarize(!!response := mean(.data[[response]], na.rm = TRUE)) %>%
    # add depmap IDs in new column at end
    add_ids(cell_col = cell)
}
