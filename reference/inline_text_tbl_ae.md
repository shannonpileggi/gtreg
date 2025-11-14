# Report Values from gtreg tables in-line

Function allows users to report formatted and styled results from gtreg
tables in-line.

## Usage

``` r
# S3 method for class 'tbl_ae'
inline_text(x, row, column = NULL, ...)

# S3 method for class 'tbl_ae_count'
inline_text(x, row, column = NULL, ...)

# S3 method for class 'tbl_ae_focus'
inline_text(x, row, column = NULL, ...)
```

## Arguments

- x:

  an object of class
  [`tbl_ae()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae.md),
  [`tbl_ae_count()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae_count.md),
  [`tbl_ae_focus()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae_focus.md)

- row:

  string indicating the AE or SOC to report

- column:

  column name of cell to report. Use `show_header_names(x)` to print all
  column names beside the current header.

- ...:

  not used

## Value

string

## Examples

``` r
# \donttest{
tbl <-
  df_adverse_events %>%
  tbl_ae(
    id = patient_id,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade
  )
show_header_names(tbl)
#> Column Name   Header                by*       N*          
#> label         "**Adverse Event**"             10 <int>    
#> stat_1        "**1**"               1 <chr>   10 <int>    
#> stat_2        "**2**"               2 <chr>   10 <int>    
#> stat_3        "**3**"               3 <chr>   10 <int>    
#> stat_4        "**4**"               4 <chr>   10 <int>    
#> stat_5        "**5**"               5 <chr>   10 <int>    
#> 
#> * These values may be dynamically placed into headers (and other locations).
#> ℹ Review the `modify_header()` (`?gtsummary::modify_header()`) help for
#>   examples.

inline_text(tbl, "Anaemia", column = stat_5)
#> [1] "3 (30)"
# }
```
