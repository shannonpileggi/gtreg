# Table modifications: headers, footnotes, and captions

``` r
library(gtreg)
library(dplyr)
library(gt)
gtsummary::theme_gtsummary_compact()
```

## Overview

In general, use
[`modify_header()`](https://www.danieldsjoberg.com/gtsummary/reference/modify.html)
and
[`modify_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/reference/modify.html)
to specify *how* headers should be modified; use in conjunction with
column selectors to specify *which* column headers to modify. Use
[`show_header_names()`](https://www.danieldsjoberg.com/gtsummary/reference/modify.html)
on a saved table object to display a usage guide for modifying headers.

To specify footnotes and captions, use
[`modify_footnote()`](https://www.danieldsjoberg.com/gtsummary/reference/deprecated_modify_footnote.html)
and
[`modify_caption()`](https://www.danieldsjoberg.com/gtsummary/reference/modify_caption.html).

For all `modify_` functions, formatting is applied via markdown and glue
syntax can be used to insert summary statistics. In addition, a return
carriage inserts a line break when preceded by two spaces: `\n`.

## AE tables

For adverse event tables functions
[`tbl_ae()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae.md),
[`tbl_ae_focus()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae_focus.md),
and
[`tbl_ae_count()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae_count.md),
use the {gtreg} column selectors
[`all_ae_cols()`](https://shannonpileggi.github.io/gtreg/reference/selectors.md),
[`all_overall_cols()`](https://shannonpileggi.github.io/gtreg/reference/selectors.md),
[`all_unknown_cols()`](https://shannonpileggi.github.io/gtreg/reference/selectors.md),
and
[`all_cols_in_strata()`](https://shannonpileggi.github.io/gtreg/reference/selectors.md)
to specify *which* columns to apply formatting.

| **{gtreg} column selectors and resulting columns selected.**                          |     |         |         |
|---------------------------------------------------------------------------------------|-----|---------|---------|
| Selector                                                                              | AE  | Overall | Unknown |
| [`all_ae_cols()`](https://shannonpileggi.github.io/gtreg/reference/selectors.md)      | ✔️  |         |         |
| [`all_overall_cols()`](https://shannonpileggi.github.io/gtreg/reference/selectors.md) |     | ✔️      |         |
| [`all_unknown_cols()`](https://shannonpileggi.github.io/gtreg/reference/selectors.md) |     |         | ✔️      |
| `all_ae_cols(overall = TRUE, unknown = TRUE)`                                         | ✔️  | ✔️      | ✔️      |

  

![Left hand side code, right hand side output table. Gif contains 8
frames that sequentially builds a table.](misc/gtreg-modify.gif)

Demonstration of `modify_` functions used with {gtreg} column selectors.

Additionally, the
[`all_cols_in_strata()`](https://shannonpileggi.github.io/gtreg/reference/selectors.md)
selector can be used with
[`modify_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/reference/modify.html)
to apply differing modifications within strata.

### tbl_ae() without strata

``` r
tbl1 <- df_adverse_events %>%
  # create a missing value to demonstrate unknown columns
  mutate(grade = ifelse(dplyr::row_number() == 1L, NA, grade)) %>%
  tbl_ae(
    id = patient_id,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade
  ) %>%
  add_overall(across = 'by') %>% 
  bold_labels()
```

#### Unmodified

``` r
# show_header_names(tbl1)
tbl1
```

[TABLE]

#### Modified

``` r
tbl1 %>%
  modify_header(
    label ~ "**Event**",
    all_ae_cols() ~ "**Grade {by}**",
    all_overall_cols() ~ "**Total**",
    all_unknown_cols() ~ "**Unknown Grade**"
  ) %>%
  modify_spanning_header(
    all_ae_cols(TRUE, TRUE) ~ "**All cohorts**, N = {N}"
  ) 
```

[TABLE]

### tbl_ae() with strata

``` r
tbl2 <-
  df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      soc = system_organ_class,
      ae = adverse_event,
      strata = trt,
      by = grade
    ) %>%
  bold_labels()
```

#### Unmodified

``` r
# show_header_names(tbl2)
tbl2
```

[TABLE]

#### Modified

``` r
tbl2 %>% 
  modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
  modify_spanning_header(all_ae_cols() ~ "**{strata}**  \n{style_percent(p)}% ({n}/{N})") %>%
  modify_caption("My caption: N = {N}")  %>%
  modify_footnote(label = "My footnote: N = {N}") 
```

[TABLE]

My caption: N = 10

``` r
tbl2 %>%
  modify_spanning_header(
      all_cols_in_strata("Drug A") ~ "**Control Group**  \n{style_percent(p)}% ({n}/{N})",
      all_cols_in_strata("Drug B") ~ "**Experimental Group**  \n{style_percent(p)}% ({n}/{N})"
    )
```

[TABLE]

## Summary tables

For
[`tbl_reg_summary()`](https://shannonpileggi.github.io/gtreg/reference/tbl_reg_summary.md),
the
[`modify_header()`](https://www.danieldsjoberg.com/gtsummary/reference/modify.html)
and
[`modify_spanning_header()`](https://www.danieldsjoberg.com/gtsummary/reference/modify.html)
functions work with the {gtsummary} column selectors like
[`all_stat_cols()`](https://www.danieldsjoberg.com/gtsummary/reference/select_helpers.html)
to specify formatting of table headers.

### tbl_reg_summary(), without overall

``` r
tbl3 <-
  df_patient_characteristics %>% 
  select(trt, marker, discontinued) %>% 
  tbl_reg_summary(
    by = trt
  ) %>% 
  bold_labels()
```

#### Unmodified

``` r
# show_header_names(tbl3)
tbl3
```

[TABLE]

#### Modified

``` r
tbl3 %>% 
  modify_header(
    all_stat_cols() ~ "**{level}**, N = {n}/{N} ({style_percent(p)}%)"
    )
```

[TABLE]

### tbl_reg_summary(), with overall

``` r
tbl4 <-
  df_patient_characteristics %>% 
  select(trt, marker, discontinued) %>% 
  tbl_reg_summary(
    by = trt
  ) %>% 
  add_overall(last = TRUE) %>% 
  bold_labels()
```

#### Unmodified

``` r
# show_header_names(tbl4)
tbl4
```

[TABLE]

#### Modified

``` r
tbl4 %>% 
  modify_header(
    list(
      stat_1 ~ "**Control Group**  \n N = {n}",
      stat_2 ~ "**Experimental Group**  \n N = {n}",
      stat_0 ~ "**Overall**  \n N = {N}"
    )) 
#> Warning: The `update` argument of `modify_header()` is deprecated as of gtsummary 2.0.0.
#> ℹ Use `modify_header(...)` input instead. Dynamic dots allow for syntax like
#>   `modify_header(!!!list(...))`.
#> ℹ The deprecated feature was likely used in the gtsummary package.
#>   Please report the issue at <https://github.com/ddsjoberg/gtsummary/issues>.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

[TABLE]
