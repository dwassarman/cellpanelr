#' Pipeline of curve fitting to be called by the upload_curve module. It
#' transforms the data, adds depmap_ids, fits the curves, and simplifies output
#' for easy plotting and analysis
#'
#' @param data Tibble
#' @param cell_col Name of column containing cell line names
#' @param dose_cols Columns whose names are the doses
#'
#' @return Tibble with curves fit. Ready for downstream analysis
#' @export
#' 
#' @noRd
fit_curves_pipeline <- function(data, cell_col, dose_cols) {
  data %>%
    add_ids(cell_col) %>%
    tidyr::pivot_longer(
      cols = dose_cols,
      names_to = "dose",
      values_to = "response"
    ) %>%
    dplyr::mutate(dose = as.numeric(dose)) %>%
    fit_sigmoid("dose", "response")
}

#' Fits data to a 4-parameter sigmoid.
#' 
#' Appends new columns containing the output of the \code{stats::optim} fit and
#' the calculated AUC, IC50, and GI50.
#'
#' @param data Tibble
#' @param x Column with independent variable (e.g. dose)
#' @param y Column with dependent variable (e.g. viability)
#'
#' @return Nested tibble
#' @export
fit_sigmoid <- function(df, x, y) {
  df %>%
    # Nest data in list-column
    tidyr::nest(data = c(.data[[x]], .data[[y]])) %>%
    # Fit sigmoid and add into model column
    dplyr::mutate(
      model = purrr::map(data, sigmoid_model)
    ) %>%
    mutate_auc #%>%
    # mutate_ic50 %>%
    # mutate_gi50
}



#' Title
#'
#' @param data 
#'
#' @return
#' @noRd
sigmoid_model <- function(data) {
  stats::optim(
    par = list("top" = 1, "log_ic50" = -1, "slope" = -1, "bottom" = 0),
    fn = rmsd_sigmoid,
    data = data,
    upper = list("top" = 2, "log_ic50" = Inf, "slope" = Inf, "bottom" = 1.5),
    lower = list("top" = 0, "log_ic50" = -Inf, "slope" = -Inf, "bottom" = 0),
    method = "L-BFGS-B",
  )
}

#' Title
#'
#' @param data 
#'
#' @return
#' @noRd
rmsd_sigmoid <- function(param, data) {
  diff <- data[[2]] - sigmoid_4p(param, data[[1]])
  sqrt(mean(diff ^ 2))
}

#' Title
#'
#' @param data 
#'
#' @return
#' @noRd
sigmoid_4p <- function(param, x) {
  top <- param[1]
  log_ic50 <- param[2]
  slope <- param[3]
  bottom <- param[4]
  
  bottom + (top - bottom) / (1 + 10^((log_ic50 - log10(x))*slope))
}



#' Add AUC column
#'
#' @param df 
#' @param cell_col 
#'
#' @return
#' 
#' @importFrom rlang .data
mutate_auc <- function(df) {
  min_log_dose <- df %>%
    tidyr::unnest(data) %>%
    dplyr::pull(dose) %>%
    min() %>%
    log10()
  
  max_log_dose <- df %>%
    tidyr::unnest(data) %>%
    dplyr::pull(dose) %>%
    max() %>%
    log10()
  
  dose_range <- 10^seq(min_log_dose, max_log_dose, length.out = 1000)
  
  # for each cell line apply the fitted model to the dose range and then average
  # the predicted values to get normalized area under the curve
  df %>%
    dplyr::group_by(cell_line) %>%
    dplyr::mutate(
      auc = purrr::map(model, purrr::pluck, "par") %>%
        purrr::map(sigmoid_4p, dose_range) %>%
        purrr::map_dbl(mean)
    ) %>%
    dplyr::ungroup()
}

mutate_ic50 <- function(df) {
  df %>%
    dplyr::mutate(
      ic50 = purrr::map(model, purrr::pluck, "par") %>%
        purrr::map_dbl(purrr::pluck, "log_ic50") %>%
        10 ^ .
    )
}