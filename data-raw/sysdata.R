expression <- depmap::depmap_TPM()
.exp_ids <- expression$depmap_id %>% unique()

usethis::use_data(.exp_ids, internal = TRUE)
