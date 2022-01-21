annotations <- vroom::vroom("~/tmp/sample_info.csv") %>%
  dplyr::rename(
    depmap_id = DepMap_ID,
    cell_line = cell_line_name,
    stripped_name = stripped_cell_line_name,
  )

usethis::use_data(annotations, overwrite = TRUE)
