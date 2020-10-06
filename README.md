
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datacleanr <img src="man/figures/dcr_logo.png" align="right" width = "150"/>

<!-- badges: start -->

<!-- [![CRAN status](https://www.r-pkg.org/badges/version/datacleanr)](https://CRAN.R-project.org/package=datacleanr) -->

<!-- [![Travis build status](https://travis-ci.org/the-Hull/datacleanr.svg?branch=master)](https://travis-ci.org/the-Hull/datacleanr) -->

[![CircleCI](https://circleci.com/gh/Appsilon/ci.example.svg?style=svg)](https://circleci.com/gh/the-Hull/datacleanr)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

> Currently under development.

`datacleanr` is a flexible and efficient tool for **interactive** data
cleaning, while enabling best practices in data analyses and reproducibility. 
It is designed for interoperability, and so seamlessly fits into **reproducible** analyses pipelines in `R`.

It can deal with nested **tabular**, as well as **spatial** and **time
series** data.

## Installation

You can install the development version of `datacleanr` with:

``` r
remotes::install_github("the-hull/datacleanr")
```

## Design

`datacleanr` is developed using the [shiny](https://shiny.rstudio.com/)
package, and relies on informative summaries, visual cues and
interactive data selection and anntoation. All data-altering operations
are documented, and converted to valid `R` code (**reproducible
recipe**), that can be copied, sent to an active `RStudio` script, or
saved to disk.

There are **four tabs** in the app for these tasks:

  - **Set-up & Overview**: define nesting structure based on (multiple)
    groups.
  - **Filtering**: use `R` expression to filter/subset data.
  - **Visual Cleaning and Annotating**: generate bivarirate (time
    series) plots and maps, as well as highlight and annotate individual
    observations. Cycle through nested groups to expedite exploration
    and cleaning. Histograms of original vs. ‘cleaned’ data can be
    generated.
  - **Extract**: generate reproducible recipe and define outputs.

Note, maps require columns *lon* and *lat* (X and Y) in decimal degrees
in the data set to render.

## Additional features

  - **Grouping**: the grouping defined in the “Set-up and Overview” tab
    is carried forward through the app. These groups can be used to
    cycle through nested/granular data, and considerably speed up
    exploration and cleaning. These groups are also available for
    filtering (Filtering tab), where filter expressions can be scoped to
    group level (i.e. no groups, individual, all groups).
  - **Interoperability**: when a logical (`TRUE`\\`FALSE`) column named
    `.dcrflag` is present, corresponding observations are rendered with
    different symbols in plots and maps. Use this feature to validate or
    cross-check external quality control or outlier flagging methods.

## Demonstration

Launch `datacleanr`’s interactive app with the following code:

``` r
library(datacleanr)
dcr_app(iris)
## basic example code
```

-----

Please note that the `datacleanr` project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
