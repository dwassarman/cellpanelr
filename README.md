
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cellpanelr

<!-- badges: start -->
<!-- badges: end -->

Identify predictive biomarkers from cell line panels. Correlate your
data with mutations, gene expression, and more. The goal of cellpanelr
is to make “omics” level data analysis accessible and open-source for
everyone.

If you want to know more, please see our
[preprint](https://www.biorxiv.org/content/10.1101/2022.11.02.514913v1).

cellpanelr uses data sets adapted from [DepMap (Broad
Institute)](https://depmap.org/portal/) under the [CC BY
4.0](https://creativecommons.org/licenses/by/4.0/) license. The current
version of cellpanelr uses DepMap release 22Q1.

## Web application

The interactive analysis tool is available at
<https://dwassarman.shinyapps.io/cellpanelr/>

## Package installation

To install cellpanelr from GitHub, enter the following command in an
interactive R session

``` r
# install.packages("remotes")
remotes::install_github("dwassarman/cellpanelr")
```

- Note: if you get the following error
  `Error in loadNamespace(x) : there is no package called ‘remotes’`.
  Remove the `#` character from the command above to install the
  `remotes` package.

## Usage

- `run_app()` runs a local instance of the interactive shiny app
- `add_ids()` matches cell line names with DepMap IDs for subsequent
  analysis
- `cor_expression()` and `cor_mutations()` correlate cell line response
  data with gene expression and gene mutations
- `data_annotations()`, `data_expression()`, and `data_mutations()`
  retrieve modified DepMap data sets for over 1,000 cell lines
- `data_nutlin()` provides an example data set containing cell line
  sensitivity to the drug nutlin-3. See the example in the publication
  folder for a more detailed analysis of this data set or read our
  [preprint](https://www.biorxiv.org/content/10.1101/2022.11.02.514913v1).

``` r
library(cellpanelr)
library(tidyverse) # Read in data, data joining, pipe operator

# Load data
data <- read_csv("cell_viability.csv")

# Add depmap_id column
# Example: the "Cell line" column contains the cell line names
data <- add_ids(data, cell_col = "Cell line")

# Add cell line annotations for each cell line
annotated <- data %>%
  left_join(
    data_annotations(),
    by = "depmap_id"
  )

# Correlate response column with gene expression
# Example: response values are in the "viability" column
exp_results <- cor_expression(data, response = "viability", ids = "depmap_ids")

# Correlate response column with mutations
# Example: response values are in the "viability" column
mut_results <- cor_mutations(data, response = "viability", ids = "depmap_ids")
```

## Code of Conduct

Please note that the cellpanelr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
