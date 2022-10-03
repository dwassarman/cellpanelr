test_that("cor_mutations() gives consistent result", {
  data <- data_nutlin() %>%
    dplyr::filter(!is.na(.data$AUC)) %>%
    add_ids("Cell line")

  expect_snapshot(cor_mutations(data, response = "AUC"))
})
