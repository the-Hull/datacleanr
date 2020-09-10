
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datacleanr

<!-- badges: start -->

<!-- [![CRAN status](https://www.r-pkg.org/badges/version/datacleanr)](https://CRAN.R-project.org/package=datacleanr) -->

<!-- [![Travis build status](https://travis-ci.org/the-Hull/datacleanr.svg?branch=master)](https://travis-ci.org/the-Hull/datacleanr) -->

[![CircleCI](https://circleci.com/gh/Appsilon/ci.example.svg?style=svg)](https://circleci.com/gh/the-Hull/datacleanr)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

> Currently under development.

`datacleanr` is a general-purpose and efficient tool for data cleaning
(filtering and outlier selection / annotation). It is inherintely
interoperable, by seamlessly integrating into **reproducible** data
analyses pipelines.

## Installation

You can install the development version of `datacleanr` with:

``` r
remotes::install_github("the-hull/datacleanr")
```

## Example

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
