library(tidyverse)
#
# list of unique cell line IDs contained in the gene expression data set
# expression <- depmap::depmap_TPM() %>%
#   dplyr::select(depmap_id, gene_name, rna_expression)
.exp_ids <- cellpanelr::data_expression()$depmap_id %>% unique()
.mut_ids <- cellpanelr::data_mutations()$depmap_id %>% unique()
# .exp_short <- head(expression, n = 20000)

# # UNCOMMENT TO USE SAMPLE DATA
# # example data for Shiny app
# # Single value
# .dasatinib_single <- vroom::vroom("../sample_data/dasatinib.tsv") %>%
#   # remove cell lines with no data
#   dplyr::filter(!is.na(auc)) %>%
#   dplyr::rename(area_under_curve = auc) %>%
#   dplyr::select(cell_line, log_ic50, area_under_curve)

usethis::use_data(
  .exp_ids,
  .mut_ids,
  # .dasatinib_single,
  # .exp_short,
  internal = TRUE,
  overwrite = TRUE
)
