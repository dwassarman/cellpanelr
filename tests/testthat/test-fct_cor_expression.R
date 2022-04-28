test_that("cor_expression() gives consistent result", {
  data <- vroom::vroom("~/Documents/r-projects/sample_data/dasatinib.tsv") %>%
    dplyr::filter(!is.na(.data$auc))
  
  expect_snapshot(cor_expression(data, response = "auc"))
})