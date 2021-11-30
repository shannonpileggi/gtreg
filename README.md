
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gtreg

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/gtreg)](https://CRAN.R-project.org/package=gtreg)
[![Codecov test
coverage](https://codecov.io/gh/shannonpileggi/gtreg/branch/main/graph/badge.svg)](https://app.codecov.io/gh/shannonpileggi/gtreg?branch=main)
[![R-CMD-check](https://github.com/shannonpileggi/gtreg/workflows/R-CMD-check/badge.svg)](https://github.com/shannonpileggi/gtreg/actions)
<!-- badges: end -->

The {gtreg} package creates tabular data summaries appropriate for
regulatory submissions. The package builds the tables using {gtsummary}.

## Installation

You can install the development version of {gtreg} from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shannonpileggi/gtreg")
```

## Example

``` r
library(gtreg)

tbl <- 
  df_adverse_events %>%
  tbl_adverse_events(
    id = patient_id,
    adverse_event = adverse_event,
    soc = system_organ_class, 
    grade = grade, 
    strata = trt
  ) %>%
  gtsummary::bold_labels()
```

<img src="man/figures/README-example-tbl_adverse_events-1.png" width="100%" />

## Code of Conduct

Please note that the gtreg project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
