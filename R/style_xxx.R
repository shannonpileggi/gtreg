#' Style numbers as x's
#'
#' The purpose of `style_xxx()` is to convert numeric values in
#' summary tables to x's of consistent length for mock tables.
#' See the \href{https://shannonpileggi.github.io/gtreg/articles/table-shells.html}{Table shells vignette}
#' for detailed examples.
#'
#' @param x a numeric or character vector
#' @param width the width of output field of x's, including the decimal place
#' @param digits the number of digits displayed after the decimal place
#'
#' @return a character vector
#' @export
#'
#' @examples
#' style_xxx(7:10, digits = 0)
#' style_xxx(7:10, digits = 1)
#' style_xxx(7:10, width = 2, digits = 0)
#' style_xxx(7:10, width = 5, digits = 2)
#'
style_xxx <- function(x, width = digits + 2, digits = 0) {
  after_decimal <-
    paste(rep_len("x", digits), collapse = "") %>% rep_len(length(x))

  before_decimal <-
    dplyr::case_when(
      digits == 0 ~ paste(rep_len("x", width), collapse = "") %>% rep_len(length(x)),
      TRUE ~ paste(rep_len("x", width - digits - 1), collapse = "") %>% rep_len(length(x))
    )

  if (digits == 0) return(before_decimal)

  paste(before_decimal, after_decimal, sep = ".")
}
