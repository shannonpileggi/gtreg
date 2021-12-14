#' Get unique values from a variable in a data frame
#'
#' Values are returned in their respective natural ordering, whether it be numeric,
#' character, or factor. If the input variable is a factor, get_unique()
#' removes factor levels and returns a character vector.
#'
#' @param data data frame
#' @param var variable from which we want unique values
#'
#' @return a vector (either character or numeric, depending on input)
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   x = c(1, 1, 2),
#'   y = c("B", "B", "A"),
#'   z = factor(x = c("cat", "cat", "dog"), levels = c("dog", "cat"))
#'   )
#' get_unique(dat, x)
#' get_unique(dat, y)
#' get_unique(dat, z)
#'
get_unique <- function(data, var){

  values <- data %>%
    dplyr::distinct( {{var}} ) %>%
    dplyr::arrange( {{var}} ) %>%
    dplyr::pull()

  if (class(values) == "factor") values <- as.character(values)

  return(values)
}

dat <- data.frame(
  x = c(1, 1, 2),
  y = c("B", "B", "A"),
  z = factor(x = c("cat", "cat", "dog"), levels = c("dog", "cat"))
)
get_unique(dat, x)
get_unique(dat, y)
get_unique(dat, z)
