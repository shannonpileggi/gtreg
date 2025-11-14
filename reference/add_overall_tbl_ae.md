# Tabulate Overall Summary

Tabulate Overall Summary

## Usage

``` r
# S3 method for class 'tbl_ae'
add_overall(x, across = NULL, ...)

# S3 method for class 'tbl_ae_count'
add_overall(x, across = NULL, ...)

# S3 method for class 'tbl_ae_focus'
add_overall(x, across = NULL, ...)
```

## Arguments

- x:

  Object of class `"tbl_ae"`, `"tbl_ae_focus"`, or `"tbl_ae_count"`

- across:

  Specify the type of overall statistics to include.

  - `"both"` adds summaries across both the `by=` and `strata=` levels

  - `"by"` adds summaries across the `by=` levels

  - `"strata"` adds summaries across the `strata=` levels

  - `"overall-only"` adds a single overall column Default is all
    possible overall types.

- ...:

  Not used

## Value

Summary object of same input class

## Notes

If the spanning headers are modified prior to the call of
[`add_overall()`](https://www.danieldsjoberg.com/gtsummary/reference/add_overall.html),
the ordering of the columns may not be correct.

## Example Output

Example 1

![](figures/add_overall_ex1.png)

Example 2

![](figures/add_overall_ex2.png)

Example 3

![](figures/add_overall_ex3.png)

Example 4

![](figures/add_overall_ex4.png)

## Examples

``` r
# \donttest{
# Example 1 -----------------------------------------------------------------
add_overall_ex1 <-
  df_adverse_events %>%
  tbl_ae_count(
    ae = adverse_event,
    soc = system_organ_class,
    by = grade,
    strata = trt
  ) %>%
  add_overall() %>%
  modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
  bold_labels()

# Example 2 -----------------------------------------------------------------
add_overall_ex2 <-
  df_adverse_events %>%
  tbl_ae(
    id = patient_id,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade
  ) %>%
  add_overall(across = 'by') %>%
  modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
  bold_labels()

# Example 3 -----------------------------------------------------------------
add_overall_ex3 <-
  df_adverse_events %>%
  tbl_ae_focus(
    id = patient_id,
    include = c(any_complication, grade3_complication),
    ae = adverse_event,
    strata = trt
  ) %>%
  add_overall(across = 'strata')

# Example 4 -----------------------------------------------------------------
add_overall_ex4 <-
  df_adverse_events %>%
  tbl_ae(
    id = patient_id,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade,
    strata = trt
  ) %>%
  add_overall(across = 'overall-only') %>%
  modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
  bold_labels()
# }
```
