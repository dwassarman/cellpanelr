expression <- depmap::depmap_TPM()
genes <- expression$gene_name %>% unique()
# UNCOMMENT to use small data
expression <- dplyr::filter(expression,
                            gene_name %in% genes[1:100])

usethis::use_data(expression, overwrite = TRUE)
