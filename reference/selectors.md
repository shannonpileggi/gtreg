# Column Selectors

See the [Table modifications
article](https://shannonpileggi.github.io/gtreg/articles/table-modifications.html)
for examples.

- `all_ae_cols(overall, unknown)` selects all columns summarizing AE
  statistics. By default, unknown and overall columns are not selected.

- `all_cols_in_strata(strata)` selects all columns from specified
  stratum.

- `all_overall_cols()` selects all overall columns

- `all_unknown_cols()` selects all unknown columns

## Usage

``` r
all_ae_cols(overall = FALSE, unknown = FALSE)

all_cols_in_strata(strata)

all_overall_cols()

all_unknown_cols()
```

## Arguments

- overall:

  logical indicating whether to include the overall columns. Default is
  FALSE

- unknown:

  logical indicating whether to include the unknown or missing columns.
  Default is FALSE

- strata:

  character vector of the selected stratum

## Value

selected columns

## Example Output

Example 1

![](figures/selectors_ex1.png)

## See also

[`gtsummary::all_stat_cols()`](https://www.danieldsjoberg.com/gtsummary/reference/select_helpers.html)

## Examples

``` r
# \donttest{
selectors_ex1 <-
  df_adverse_events %>%
  dplyr::mutate(grade = ifelse(dplyr::row_number() == 1L, NA, grade)) %>%
  tbl_ae(
    id = patient_id,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade
  ) %>%
  add_overall(across = 'by') %>%
  modify_header(
    all_ae_cols() ~ "**Grade {by}**",
    all_overall_cols() ~ "**Total**",
    all_unknown_cols() ~ "**Unknown Grade**"
  )
# }
```
