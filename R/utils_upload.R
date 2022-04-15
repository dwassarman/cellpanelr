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
