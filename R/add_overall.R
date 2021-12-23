#' Tabulate Overall Summary
#'
#' @param x Object of class `"tbl_adverse_event"`
#' @param by logical indicating whether the `by=` argument should be
#' included in the overall table. Default is `FALSE`
#' @param strata logical indicating whether the `strata=` argument should be
#' included in the overall table. Default is `FALSE`
#' @param ... Not used
#'
#' @return Summary object of class `"tbl_adverse_event"`
#' @export
#' @examples
#' # Example 1 -----------------------------------------------------------------
#' add_overall_ex1 <-
#'   df_adverse_events %>%
#'   tbl_adverse_event(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade,
#'     header = "**Grade {level}**"
#'   ) %>%
#'   add_overall()
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_overall_ex1.png}{options: width=35\%}}

add_overall.tbl_adverse_event <- function(x, by= FALSE, strata = FALSE, ...) {
  # check inputs ---------------------------------------------------------------
  if (isTRUE(by) && isTRUE(strata)) {
    stop("Both `by=` and `strata=` cannot be TRUE.", call. = FALSE)
  }
  if (is.null(x$inputs$by) && isTRUE(by)) {
    paste("Argument `by = TRUE` does not apply when no `by=`",
          "passed in original data summary call.") %>%
      stop(call. = FALSE)
  }
  if (is.null(x$inputs$strata) && isTRUE(strata)) {
    paste("Argument `strata = TRUE` does not apply when no `strata=`",
          "passed in original data summary call.") %>%
      stop(call. = FALSE)
  }

  # running data summary function ----------------------------------------------
  tbl_args <- x$inputs
  tbl_args$header <- NULL
  if (!isTRUE(by)) tbl_args$by <- NULL
  if (!isTRUE(strata)) tbl_args$strata <- NULL

  tbl_overall <- do.call(class(x)[1], tbl_args)

  # merging tbl_overall with original call -------------------------------------
  gtsummary::tbl_merge(list(x, tbl_overall), tab_spanner = FALSE) %>%
    gtsummary::modify_spanning_header(ends_with("_2") ~ "**Overall**")
}
