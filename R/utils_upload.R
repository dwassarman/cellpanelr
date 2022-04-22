#' Upload a file with feedback to fileInput UI element
#'
#' @param input reactive values list of inputs from module UI
#' @param id Name of input UI element
#'
#' @return Tibble
upload_file_with_feedback <- function(input, id) {
  # Can update this to expand allowed file types
  allowed <- c("tsv", "csv", "xls", "xlsx")

  req(input[[id]])
  datapath <- input[[id]][["datapath"]]
  ext <- tools::file_ext(datapath)
  ok <- isTruthy(ext %in% allowed)
  txt <- paste0(allowed, collapse = ", ")
  shinyFeedback::feedbackDanger(id,
    show = !ok,
    text = paste0("Allowed file types: ", txt)
  )
  req(ok)
  if (ext %in% c("xls", "xlsx")) {
    readxl::read_excel(datapath)
  } else {
    vroom::vroom(datapath)
  }
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
