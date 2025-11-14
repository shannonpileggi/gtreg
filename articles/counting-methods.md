# Adverse Event Counting Methods

## Setup

``` r
# install.packages("gtreg")
library(gtreg)
```

## Overview

Reporting of adverse events to regulatory agencies typically employs
both standardized terminology and specific counting methods for adverse
events.

## Standardized Terminology

Providing standardized terminology is not in the scope of this package.
Standardized terminology adheres to a hierarchy of terms from specific
to general, as discussed in the [MedDRA
Hierarchy](https://www.meddra.org/how-to-use/basics/hierarchy)
documentation. In {gtreg}, we facilitate reporting of a single term
(i.e., `ae`, or adverse event), or a lower level term within a higher
level term (i.e., `ae` within `soc`, system organ class).

## Example Data

Data is typically recorded in a long format with multiple rows per
subject. Events can be reported over time, where both multiple time
points per subject and multiple events per time point are possible. It
is possible that grade of the event is missing.

``` r
dat <- tibble::tribble(
    ~subject, ~visit,  ~soc, ~ae, ~grade,
    # Subject 1 ----------------------------------------------------------------
    "001", 1, "Eye disorders", "Eye irritation", 1,
    "001", 1, "Gastrointestinal disorders", "Difficult digestion", NA,
    "001", 2, "Eye disorders", "Eye irritation", 1,
    "001", 3, "Eye disorders", "Eye irritation", 2,
    "001", 4, "Eye disorders", "Vision blurred", 2,
    # Subject 2 ----------------------------------------------------------------
    "002", 1, "Gastrointestinal disorders", "Difficult digestion", 2,
    "002", 1, "Gastrointestinal disorders", "Reflux", 2,
    "002", 2, "Eye disorders", "Vision blurred", 2,
    "002", 2, "Gastrointestinal disorders", "Reflux", 2,
    "002", 3, "Gastrointestinal disorders", "Reflux", NA
  )
```

``` r
dat
#> # A tibble: 10 × 5
#>    subject visit soc                        ae                  grade
#>    <chr>   <dbl> <chr>                      <chr>               <dbl>
#>  1 001         1 Eye disorders              Eye irritation          1
#>  2 001         1 Gastrointestinal disorders Difficult digestion    NA
#>  3 001         2 Eye disorders              Eye irritation          1
#>  4 001         3 Eye disorders              Eye irritation          2
#>  5 001         4 Eye disorders              Vision blurred          2
#>  6 002         1 Gastrointestinal disorders Difficult digestion     2
#>  7 002         1 Gastrointestinal disorders Reflux                  2
#>  8 002         2 Eye disorders              Vision blurred          2
#>  9 002         2 Gastrointestinal disorders Reflux                  2
#> 10 002         3 Gastrointestinal disorders Reflux                 NA
```

## Challenges Addressed

Study participants without adverse events are absent from the adverse
event raw data table. Furthermore, study participants may have multiple
adverse events, often with different severity grades.

These two features pose difficulties to computing the percentage of
participants who experience a specific adverse event.

1.  In the numerator, we need to make sure that we do not double count
    the same AE within each participant, and in the denominator we need
    to make sure we include all patients in the treatment arm (not just
    those who experience an AE). These challenges are amplified when AE
    terms need to roll up into system organ class summaries. Functions
    in {gtreg} address these problems (among others) by implementing
    industry recommended approaches. For further reading, the PHUSE
    white paper regarding [Analysis and Displays Associated with Adverse
    events](https://phuse.s3.eu-central-1.amazonaws.com/Deliverables/Standard+Analyses+and+Code+Sharing/Analyses+and+Displays+Associated+with+Adverse+Events+Focus+on+Adverse+Events+in+Phase+2-4+Clinical+Trials+and+Integrated+Summary.pdf)
    provides an excellent summary of such standards.

## Counting All Events

To count *all* instances of any event, utilize `tbl_ae_count`.

``` r
dat %>% 
  tbl_ae_count(
    ae = ae,
    soc = soc,
    by = grade
  ) %>% 
  modify_header(all_ae_cols() ~ "**Grade {by}**") %>% 
  add_overall() %>% 
  bold_labels()
```

| **Adverse Event**          | **Unknown** | **Grade 1** | **Grade 2** | **Overall** |
|----------------------------|-------------|-------------|-------------|-------------|
| Eye disorders              | —           | 2           | 3           | 5           |
|     Eye irritation         | —           | 2           | 1           | 3           |
|     Vision blurred         | —           | —           | 2           | 2           |
| Gastrointestinal disorders | 2           | —           | 3           | 5           |
|     Difficult digestion    | 1           | —           | 1           | 2           |
|     Reflux                 | 1           | —           | 2           | 3           |

In this case, we have two subjects which experienced a total of 5 eye
disorders and 4 gastrointestinal disorders over the course of multiple
time points. The only statistic allowed in
[`tbl_ae_count()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae_count.md)
is `n`.

## Counting By Highest Grade

Rather than counting all events, it is typical in regulatory reporting
to count one event per subject by the highest grade experienced. To
count the highest grade per subject, utilize
[`tbl_ae()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae.md).

The input `by` variable can be numeric (e.g., grade 1, 2, 3) or factor
(e.g. attribution unlikely, possible, definite). In the respective
cases, the highest value observed would be according to numeric order or
factor level order, with missing values considered as the lowest level.

When counting by highest grade, the count reported in any cell cannot
exceed the number of subjects. These methods apply to both the
individual adverse events and the system organ class.

### Counts Only

``` r
dat %>% 
  tbl_ae(
    id = subject,
    ae = ae,
    soc = soc,
    by = grade,
    statistic = "{n}"
  ) %>% 
  modify_header(all_ae_cols() ~ "**Grade {by}**") %>% 
  add_overall() %>% 
  bold_labels()
```

[TABLE]

#### Adverse Events

- Subject `001` experienced two instances of grade 1 eye irritation and
  one instance of grade 2 eye irritation. The maximum grade experienced
  of eye irritation experienced is 2, resulting in a count of `1` for
  eye irritation.

- Subject `001` and Subject `002` both experienced grade 2 vision
  blurred, resulting in a count of `2` of vision blurred.

- Subject `002` experienced two instances of grade 2 reflux and one
  instance of reflux with unknown grade; in a count of `1` for grade 2
  reflux.

#### System Organ Class

*Eye disorders*

- Subject `001` contributes 1 count to grade 2 eye irritation; together,
  subject `001` and `002` contribute 2 counts to grade 2 vision blurred.

- 2 subjects experienced a grade 2 eye disorder.

- 2 subjects overall experienced an eye disorder.

*Gastrointestinal disorders*

- Subject `001` experienced difficult digestion of unknown grade, and
  contributes a count of 1 to unknown grade gastrointestinal disorder.

- Subject `002` experienced both grade 2 difficult digestion and grade 2
  reflux, and contributes a count of 1 to grade 2 gastrointestinal
  disorders.

- 2 subjects overall experienced a gastrointestinal disorder.

### Counts With Percents

The default statistic reported for
[`tbl_ae()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae.md)
is `n (%)`. The percent represents the percent of all subjects present
in the data.

``` r
dat %>% 
  tbl_ae(
    id = subject,
    ae = ae,
    soc = soc,
    by = grade
  ) %>% 
  modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
  modify_spanning_header(all_ae_cols() ~ "**N = {N}**") %>% 
  add_overall() %>% 
  bold_labels()
```

[TABLE]

In adverse event reporting, it is common that not all subjects
experience an adverse event, and hence are not included in the event
reporting data set. For example, suppose there were 3 subjects of
interest in this study, but subject `003` did not experience an adverse
event. In order to get the correct subject denominator in this case,
supply
[`tbl_ae()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae.md)
with `id_df`, a data frame containing all subject ids.

``` r
dat %>% 
  tbl_ae(
    id = subject,
    id_df = tibble::tibble(subject = c("001", "002", "003")),
    ae = ae,
    soc = soc,
    by = grade
  ) %>% 
  modify_header(all_ae_cols() ~ "**Grade {by}**") %>% 
  modify_spanning_header(all_ae_cols() ~ "**N = {N}**") %>% 
  add_overall() %>% 
  bold_labels()
```

[TABLE]
