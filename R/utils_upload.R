#' Check whether file is allowed to be uploaded
#'
#' @param filepath Full file path (e.g. input$file$datapath)
#'
#' @description A utils function
#'
#' @return bool Whether filetype is allowed
#'
#' @noRd
upload_file_with_feedback <- function(input, id) {
  # Can update this to expand allowed file types
  allowed <- c("tsv", "csv")
  
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
  vroom::vroom(datapath)
}