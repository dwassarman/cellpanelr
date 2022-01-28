# list of unique cell line IDs contained in the gene expression data set
expression <- depmap::depmap_TPM()
.exp_ids <- expression$depmap_id %>% unique()

# example data for Shiny app
# Single value
.dasatinib_single <- vroom::vroom("../sample_data/dasatinib.tsv") %>%
  # remove cell lines with no data
  dplyr::filter(!is.na(auc)) %>%
  dplyr::rename(area_under_curve = auc) %>%
  dplyr::select(cell_line, log_ic50, area_under_curve)

usethis::use_data(.exp_ids,
                  .dasatinib_single,
                  internal = TRUE,
                  overwrite = TRUE)
