test_that("cor_mutations() gives consistent result", {
  data <- vroom::vroom("~/Documents/r-projects/sample_data/dasatinib.tsv") %>%
    dplyr::filter(!is.na(.data$auc))
  
  expect_snapshot(cor_mutations(data, response = "auc"))
})
