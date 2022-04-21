#' Help button
#'
#' @param id inputId
#' @param ns ns function from module
#'
#' @return actionButton with the given inputId and formatted to be extra small
#' with a bold question mark inside
helpButton <- function(id, ns = NULL) {
  if (is.null(ns)) {
    actionButton(id, strong("?"), class = "btn-xs")
  } else {
    actionButton(ns(id), strong("?"), class = "btn-xs")
  }
}
