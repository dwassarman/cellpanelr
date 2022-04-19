library(tidyverse)
library(vroom)

data <- vroom("~/Documents/r-projects/DepMap raw/22Q1/CCLE_mutations.csv") %>%
  select(Hugo_Symbol, DepMap_ID) %>%
  rename(gene = Hugo_Symbol, depmap_id = DepMap_ID) %>%
  distinct(gene, depmap_id)

wide <- data %>%
  pivot_wider(
    names_from = depmap_id,
    values_from = depmap_id,
    values_fn = function(x) { !is.na(x)},
    values_fill = FALSE
  )

saveRDS(wide, "~/Documents/r-projects/cellpanelr data/22Q1/mutations.rds", compres = "gzip")
