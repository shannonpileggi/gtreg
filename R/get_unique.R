#' Get unique values from a variable in a data frame
#'
#' Values are returned in their respective natural ordering, whether it be numeric,
#' character, or factor. If the input variable is a factor, get_unique()
#' removes factor levels and returns a character vector.
#'
#' @param data data frame
#' @param var variable from which we want unique values
#' @param drop_na whether or not to drop NA from returned values; defaults to TRUE
#' @param keep_fct_levels whether or not to keep all levels of factor (instead
#' of only values present in data); defaults to TRUE
#'
#'
#' @return a vector (either character or numeric, depending on input)
#' @noRd
#'
#' @examples
#' dat <- data.frame(
#'   w = c(NA, 1, "B"),
#'   x = c(1, 1, 2),
#'   y = c("B", "B", "A"),
#'   z = factor(x = c("cat", "cat", "dog"), levels = c("dog", "cat")),
#'   u = factor(x = c("cat", "cat", "dog"), levels = c("dog", "pig", "cat"))
#'   )
#' get_unique(dat, w)
#' get_unique(dat, x)
#' get_unique(dat, y)
#' get_unique(dat, z)
#' get_unique(dat, u)
#'
get_unique <- function(data, var, drop_na = TRUE, keep_fct_levels = TRUE){

  var_chr <- rlang::as_label(rlang::ensym(var))

  # check to see if variable is a factor and we want to return
  # all factor levels
  if (class(data[[var_chr]]) == "factor" & keep_fct_levels) {
    values <- levels(data[[var_chr]])
    return(values)
  }

  # this should run if variable is not a factor, or if variable
  # is a factor but we only want factor levels observed in data to
  # be returned
  values <- data %>%
    dplyr::distinct( {{var}} ) %>%
    arrange( {{var}} ) %>%
    {if ( drop_na ) tidyr::drop_na(., {{var}} ) else . } %>%
    dplyr::pull( {{var}} )

  if (class(values) == "factor") values <- as.character(values)

  return(values)
}

