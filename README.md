
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cellpanelr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

A Shiny app and R package for analyzing response data collected from
panels of cell lines.

It uses data sets and annotations publicly available from
[DepMap](https://depmap.org/portal/) to identify features that correlate
with user-provided data. These data sets are used under the [CC BY
4.0](https://creativecommons.org/licenses/by/4.0/) license.

## Shiny app

The shiny app is available for use at
<https://dwassarman.shinyapps.io/cellpanelr/>

## Local installation

You may have a better experience with the app if you run it locally.

To do so, open an interactive R session one of the following ways:

1.  open RStudio
2.  enter `R` into the command line

Then enter the following command to install the cellpanelr package from
GitHub:

``` r
# install.packages("remotes")
remotes::install_github("dwassarman/cellpanelr")
```

-   Note: if you get the following error
    `Error in loadNamespace(x) : there is no package called ‘remotes’`.
    Remove the `#` character from the command above to install the
    `remotes` package.

Once you’ve installed cellpanelr, you can load the package into R and
open the app like so:

``` r
library(cellpanelr)
run_app()
```

## R Package

Fill in additional info here about main functions and usage. Consider
creating vignettes.

Please note that the cellpanelr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
