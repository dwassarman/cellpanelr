# Last run on 04/14/2022
# Release 22Q1

library(tidyverse)
library(vroom)

# Read in from .csv file
annotations <- vroom::vroom("../DepMap data/22Q1/sample_info.csv")

# Fix column names
annotations <- annotations %>%
  rename(depmap_id = DepMap_ID) %>%
  select(-c(CCLE_Name, alias, COSMICID, RRID, WTSI_Master_Cell_ID, Sanger_Model_ID, depmap_public_comments, ))

# Save as RDS in extdata
saveRDS(annotations, "./inst/extdata/annotations.rds")

# # Save as RDS in data
# usethis::use_data(annotations, overwrite = TRUE)
