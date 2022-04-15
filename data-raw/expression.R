library(tidyverse)
library(vroom)

# Last run on 4/14/2022 with 22Q1 release

# Download data
expression <- vroom("../DepMap data/22Q1/CCLE_expression.csv")

# Fix column names
names <- names(expression)
names[[1]] <- "depmap_id"

fix_names <- function(name) {
  str_split(name, " ")[[1]][[1]]
}

names(expression) <- lapply(names, fix_names) %>% as.character()

# Save file in most compressed RDS format in extdata/
saveRDS(expression, "./inst/extdata/expression.rds", compress = "xz")

# # Save RDS file in data/
# usethis::use_data(expression, compress = "xz")
