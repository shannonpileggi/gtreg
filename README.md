
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gtreg

<!-- badges: start -->

[![R-CMD-check](https://github.com/shannonpileggi/gtreg/workflows/R-CMD-check/badge.svg)](https://github.com/shannonpileggi/gtreg/actions)
[![Codecov test
coverage](https://codecov.io/gh/shannonpileggi/gtreg/branch/main/graph/badge.svg)](https://app.codecov.io/gh/shannonpileggi/gtreg?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/gtreg)](https://CRAN.R-project.org/package=gtreg)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The {gtreg} package creates tabular data summaries appropriate for
regulatory submissions. The package builds the tables using {gtsummary}.

A common use-case handled by {gtreg} is the counting of adverse events
(AEs). In this setting, the programmer’s task is to tabulate adverse
events by any or all of: AE term, system organ class, grade, and
treatment arm. AE tables are most often provided to programmers in a
flat file that contains multiple rows per patient; i.e. one row per AE.
Some challenges with this structure are that study participants who have
no AEs do not appear in the AE table, and individual participants may
have several of the same AE (possibly with different severity grades).
Both of these features pose difficulties with regards to calculating
proportions; e.g. the percentage of participants in a treatment arm who
experience a specific AE. In the numerator, we need to make sure that we
do not double count the same AE within each participant, and in the
denominator we need to make sure we include all patients in the
treatment arm (not just those who experience an AE). These challenges
are amplified when AE terms need to roll up into system organ class
summaries. Functions in {gtreg} address these problems (among others) by
implementing industry recommended approaches. For further reading, the
PHUSE white paper regarding [Analysis and Displays Associated with
Adverse
events](https://phuse.s3.eu-central-1.amazonaws.com/Deliverables/Standard+Analyses+and+Code+Sharing/Analyses+and+Displays+Associated+with+Adverse+Events+Focus+on+Adverse+Events+in+Phase+2-4+Clinical+Trials+and+Integrated+Summary.pdf)
provides an excellent summary of such standards.

## Installation

You can install the development version of {gtreg} from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shannonpileggi/gtreg")
```

## Example

Summarize Adverse Events by Grade. The denominators in the table are the
number of patients in the study.

``` r
library(gtreg)
gtsummary::theme_gtsummary_compact()
#> Setting theme `Compact`

tbl_ae <- 
  df_adverse_events %>%
  tbl_ae(
    id_df = df_patient_characteristics,
    id = patient_id,
    ae = adverse_event,
    soc = system_organ_class, 
    by = grade, 
    strata = trt
  ) %>%
  modify_header(all_ae_cols() ~ "**Grade {by}**") %>% 
  bold_labels()
```

<img src="man/figures/README-example-tbl_ae-1.png" width="100%" />

Summarize Raw Adverse Counts

``` r
tbl_ae_count <- 
  df_adverse_events %>%
  tbl_ae_count(
    ae = adverse_event,
    soc = system_organ_class, 
    by = drug_attribution
  ) %>%
  add_overall(across = "by") %>%
  modify_spanning_header(all_ae_cols() ~ "**Drug Attribution**") %>%
  bold_labels()
```

<img src="man/figures/README-example-tbl_ae_count-1.png" width="76%" />

Focus on rates of high grade complications with `tbl_ae_focus()`

``` r
tbl_ae_focus <- 
  df_adverse_events %>%
  tbl_ae_focus(
    id_df = df_patient_characteristics,
    id = patient_id,
    ae = adverse_event,
    include = c(any_complication, grade3_complication)
  )
```

<img src="man/figures/README-example-tbl_ae_focus-1.png" width="62%" />

## Code of Conduct

Please note that the gtreg project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
