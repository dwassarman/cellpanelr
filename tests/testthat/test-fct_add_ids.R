test_that("add_ids() adds IDs", {
  data <- tibble::tibble(names = c("mcf-7", "SCABER", "293-T"))
  sol <- tibble::tibble(names = c("mcf-7", "SCABER", "293-T"),
                        depmap_id = c("ACH-000019", "ACH-000839", NA))
  obj <- add_ids(data, "names")
  expect_equal(obj, sol)
})

test_that("IDs that map to the same cell line are removed", {
  data <- tibble::tibble(cells = c("ABC1", "ABC1", "GHI3", "JKL4"),
                         depmap_id = c("001", "002", "003", NA))
  sol <- tibble::tibble(cells = c("ABC1", "ABC1", "GHI3", "JKL4"),
                        depmap_id = c(NA, NA, "003", NA))
  obj <- remove_multi_id(data, "cells")
  expect_equal(obj, sol)
})

test_that("IDs that map to >1 cell line are removed", {
  data <- tibble::tibble(cells = c("ABC1", "DEF2", "GHI3", "JKL4"),
                         depmap_id = c("001", "001", "003", NA))
  sol <- tibble::tibble(cells = c("ABC1", "DEF2", "GHI3", "JKL4"),
                        depmap_id = c(NA, NA, "003", NA))
  obj <- remove_multi_cell(sol, "cells")
  expect_equal(obj, sol)
})