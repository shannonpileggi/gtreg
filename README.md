
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gtreg <a href="https://shannonpileggi.github.io/gtreg/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/shannonpileggi/gtreg/workflows/R-CMD-check/badge.svg)](https://github.com/shannonpileggi/gtreg/actions)
[![Codecov test
coverage](https://codecov.io/gh/shannonpileggi/gtreg/branch/main/graph/badge.svg)](https://app.codecov.io/gh/shannonpileggi/gtreg?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/gtreg)](https://CRAN.R-project.org/package=gtreg)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install {gtreg} with the following code.

``` r
install.packages("gtreg")
```

You can install the development version of {gtreg} from
[GitHub](https://github.com/shannonpileggi/gtreg) with:

``` r
# install.packages("devtools")
devtools::install_github("shannonpileggi/gtreg")
```

## Overview

The {gtreg} package creates tabular data summaries appropriate for
regulatory submissions. The package builds the tables using {gtsummary}.

Here are
[slides](https://shannonpileggi.github.io/introducing-gtreg-rmed-2022/#/title-slide)
and a [recorded talk (17 min)](https://youtu.be/DmslEfczYqM) from
“Introducing {gtreg}: an R package to produce regulatory tables for
clinical research” presented at the 2022 R in Medicine conference.

[<img src="man/figures/Introducing-gtreg-screenshot.png"
style="width:70.0%"
alt="Screenshot of YouTube recording on title slide" />](https://youtu.be/DmslEfczYqM)

## Functions for adverse event (AE) reporting

**Summarize Raw Adverse Counts**

`tbl_ae_count()` provides counts of all AEs, and omits percentage
statistics as multiple AEs can occur per subject.

``` r
library(gtreg)
tbl_ae_count <- 
  df_adverse_events |> 
  tbl_ae_count(
    ae = adverse_event,
    soc = system_organ_class, 
    by = drug_attribution
  ) |>
  add_overall(across = "by") |>
  modify_spanning_header(all_ae_cols() ~ "**Drug Attribution**") |>
  bold_labels()
```

<img src="man/figures/README-example-tbl_ae_count-1.png" width="76%" />

**Summarize Adverse Events by Grade**

`tbl_ae()` counts one AE per subject by maximum grade; percentage
statistics are provided by default with the denominators reflecting the
number of patients in the study.

``` r
library(gtreg)
gtsummary::theme_gtsummary_compact()
#> Setting theme "Compact"

tbl_ae <- 
  df_adverse_events |>
  tbl_ae(
    id_df = df_patient_characteristics,
    id = patient_id,
    ae = adverse_event,
    soc = system_organ_class, 
    by = grade, 
    strata = trt
  ) |>
  modify_header(all_ae_cols() ~ "**Grade {by}**") |> 
  bold_labels()
```

<img src="man/figures/README-example-tbl_ae-1.png" width="100%" />

**Focus on rates of high grade complications**

`tbl_ae_focus()` also counts one AE per subject by maximum grade, and is
a convenience to summarize dichotomous AE attributes.

``` r
tbl_ae_focus <- 
  df_adverse_events |>
  tbl_ae_focus(
    id_df = df_patient_characteristics,
    id = patient_id,
    ae = adverse_event,
    include = c(any_complication, grade3_complication)
  )
```

<img src="man/figures/README-example-tbl_ae_focus-1.png" width="62%" />

## Other Functions for Clinical Reporting

**Regulatory summary**

`tbl_reg_summary()` creates a data summary table often seen in
regulatory submissions.

``` r
tbl_reg_summary <-
  df_patient_characteristics |>
  tbl_reg_summary(by = trt, include = c(marker, status)) 
```

<img src="man/figures/README-example-tbl_reg_summary-1.png" width="42%" />

**Print an AE listing**

`tbl_listing()` creates a gtsummary-class listing of data to enable
grouped printing.

``` r
tbl_listing <-
  head(df_adverse_events, n = 10) |>
  select(system_organ_class, adverse_event, grade, drug_attribution, patient_id) |>
  dplyr::arrange(adverse_event, desc(grade)) |>
  tbl_listing(group_by = system_organ_class) |>
  bold_labels()
```

<img src="man/figures/README-example-tbl_listing-1.png" width="62%" />

## Code of Conduct

Please note that the gtreg project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
