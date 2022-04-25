library(cellpanelr)
library(tidyverse)

test_that("prepare_data() adds depmap_ids", {
  a <- tibble(
    cells = c("CACO 2", "A101D", "253-j"),
    measure = c(20, 21, 22)
  )
  sol <- mutate(
    a,
    depmap_id = c("ACH-000003", "ACH-000008", "ACH-000011")
  )

  expect_true(all_equal(prepare_data(a, "cells", "measure"), sol))
})

test_that("prepare_data() averages rows while dealing with NA", {
  a <- tibble(
    cells = c("CACO 2", "253-J", "CACO 2", "HL60", "CACO 2"),
    y = c(10, 11, 12, 13, NA)
  )
  sol <- tibble(
    cells = c("CACO 2", "253-J", "HL60"),
    y = c(11, 11, 13),
    depmap_id = c("ACH-000003", "ACH-000011", "ACH-000002")
  )

  expect_true(all_equal(prepare_data(a, "cells", "y"), sol))
})

test_that("prepare_data() removes all NAs from response column", {
  a <- tibble(
    cells = c("NCI-H1693", "PA-TU-8988S", "253J-BV"),
    response = c(1, NA, 5)
  )
  sol <- tibble(
    cells = c("NCI-H1693", "253J-BV"),
    response = c(1, 5),
    depmap_id = c("ACH-000021", "ACH-000026")
  )

  expect_true(all_equal(prepare_data(a, "cells", "response"), sol))
})

test_that("upload_data() handles all data types", {
  # Create sample data
  df <- tibble::tibble(x = 1, y = 2)
  path_csv <- tempfile(fileext = ".csv")
  path_tsv <- tempfile(fileext = ".tsv")
  write.csv(df, path_csv, row.names = FALSE)
  write.table(df, path_tsv, sep = "\t", row.names = FALSE)

  expect_equal(load_file(path_csv), df)
  expect_equal(load_file(path_tsv), df)
  expect_error(load_file("blah.txt"), "Invalid file")
})
