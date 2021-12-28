#' Tabulate Overall Summary
#'
#' @param x Object of class `"tbl_ae"`
#' @param by logical indicating whether the `by=` argument should be
#' included in the overall table. Default is `FALSE`
#' @param strata logical indicating whether the `strata=` argument should be
#' included in the overall table. Default is `FALSE`
#' @param ... Not used
#'
#' @return Summary object of class `"tbl_ae"`
#' @export
#' @examples
#' # Example 1 -----------------------------------------------------------------
#' add_overall_ex1 <-
#'   df_adverse_events %>%
#'   tbl_ae(
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
#' \if{html}{\figure{add_overall_ex1.png}{options: width=70\%}}

add_overall.tbl_ae <- function(x, by = FALSE, strata = FALSE, ...) {
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
  if (!isTRUE(by)) {
    tbl_args$by <- NULL
    tbl_args$header <- NULL
  }
  if (!isTRUE(strata)) tbl_args$strata <- NULL

  tbl_overall <- do.call(class(x)[1], tbl_args)

  # add headers ----------------------------------------------------------------
  if (isTRUE(by) && !isTRUE(strata)) {
    tbl_overall <-
      tbl_overall %>%
      gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ "**Overall**")
  }

  # merging tbl_overall with original call -------------------------------------
  tbl_final <- gtsummary::tbl_merge(list(x, tbl_overall), tab_spanner = FALSE)
  class(tbl_final) <- class(x)
  tbl_final
}
