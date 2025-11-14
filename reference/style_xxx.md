# Style numbers as x's

The purpose of `style_xxx()` is to convert numeric values in summary
tables to x's of consistent length for mock tables. See the [Table
shells
vignette](https://shannonpileggi.github.io/gtreg/articles/table-shells.html)
for detailed examples.

## Usage

``` r
style_xxx(x, width = digits + 2, digits = 0)
```

## Arguments

- x:

  a numeric or character vector

- width:

  the width of output field of x's, including the decimal place

- digits:

  the number of digits displayed after the decimal place

## Value

a character vector

## Examples

``` r
style_xxx(7:10, digits = 0)
#> [1] "xx" "xx" "xx" "xx"
style_xxx(7:10, digits = 1)
#> [1] "x.x" "x.x" "x.x" "x.x"
style_xxx(7:10, width = 2, digits = 0)
#> [1] "xx" "xx" "xx" "xx"
style_xxx(7:10, width = 5, digits = 2)
#> [1] "xx.xx" "xx.xx" "xx.xx" "xx.xx"
```
