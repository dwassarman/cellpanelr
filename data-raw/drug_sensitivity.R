library(tidyverse)
library(vroom)

# Last run on 5/3/2022 with 22Q1 release

# Download data
drug_sensitivity <- vroom("../DepMap raw/22Q1/sanger-dose-response.csv") %>%
  select(ARXSPAN_ID, DRUG_NAME, IC50_PUBLISHED, AUC_PUBLISHED) %>%
  rename(
    depmap_id = ARXSPAN_ID,
    drug = DRUG_NAME,
    ic50 = IC50_PUBLISHED,
    auc = AUC_PUBLISHED
  ) %>%
  filter(!is.na(depmap_id)) %>%
  mutate(log_ic50 = log10(ic50)) %>%
  mutate(drug = tolower(drug)) %>%
  group_by(drug, depmap_id) %>%
  summarise(across(.fns = mean)) %>%
  ungroup()

# Save file in most compressed RDS format in extdata/
saveRDS(drug_sensitivity, "../cellpanelr data/22Q1/drug_sensitivity.rds")
