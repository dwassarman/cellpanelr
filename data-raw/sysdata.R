library(tidyverse)
#
# list of unique cell line IDs contained in the gene expression data set
# expression <- depmap::depmap_TPM() %>%
#   dplyr::select(depmap_id, gene_name, rna_expression)
.exp_ids <- cellpanelr::data_expression()$depmap_id %>% unique()
.mut_ids <- cellpanelr::data_mutations()$depmap_id %>% unique()
# .exp_short <- head(expression, n = 20000)

# Example data
.nutlin <-
  vroom::vroom("../sample_data/nutlin_data.tsv") %>%
  # remove cell lines with no data
  dplyr::filter(!is.na(AUC))

usethis::use_data(
  .exp_ids,
  # .exp_short,
  .mut_ids,
  .nutlin,
  internal = TRUE,
  overwrite = TRUE
)
