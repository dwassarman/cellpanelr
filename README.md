
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cellpanelr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

A Shiny app and R package for analyzing response data collected from
panels of cell lines. It uses data sets and annotations publicly
available from [DepMap](https://depmap.org/portal/) to identify features
that correlate with user-provided data. These data sets are used under
the [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) license.

## Shiny app

The shiny app is available at
<https://dwassarman.shinyapps.io/cellpanelr/>

## Installation

You can install the development version of cellpanelr like so:

``` r
# install.packages("remotes")
remotes::install_github("dwassarman/cellpanelr")
```

To open and use the app locally:

``` r
library(cellpanelr)
run_app()
```

Please note that the cellpanelr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
