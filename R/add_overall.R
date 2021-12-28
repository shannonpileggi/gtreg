#' Tabulate Overall Summary
#'
#' @param x Object of class `"tbl_ae"`, `"tbl_ae_focus"`, or `"tbl_ae_count"`
#' @param type Specify the type of overall statistics to include.
#' - `"by"` adds summaries across the `by=` levels
#' - `"strata"` adds summaries across the `strata=` levels
#' - `"both"` adds summaries across both the `by=` and `strata=` levels
#' Default it `"both"`
#' @param by logical indicating whether the `by=` argument should be
#' included in the overall table. Default is `FALSE`
#' @param strata logical indicating whether the `strata=` argument should be
#' included in the overall table. Default is `FALSE`
#' @param ... Not used
#'
#' @name add_overall_tbl_ae
#'
#' @section Notes:
#' If the spanning headers are modified prior to the call of `add_overall()`,
#' the ordering of the columns may not be correct.
#'
#' @return Summary object of class `"tbl_ae"`

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
#'   add_overall(type = 'by') %>%
#'   bold_labels()
#'
#' # Example 2 -----------------------------------------------------------------
#' add_overall_ex2 <-
#'   df_adverse_events %>%
#'   tbl_ae_count(
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade,
#'     strata = trt,
#'     header = "**Grade {level}**"
#'   ) %>%
#'   add_overall() %>%
#'   bold_labels()
#'
#' # Example 3 -----------------------------------------------------------------
#' add_overall_ex3 <-
#'   df_adverse_events %>%
#'   tbl_ae_focus(
#'     id = patient_id,
#'     include = c(any_complication, grade3_complication),
#'     ae = adverse_event,
#'     strata = trt
#'   ) %>%
#'   add_overall(type = 'strata')
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_overall_ex1.png}{options: width=70\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_overall_ex2.png}{options: width=70\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{add_overall_ex3.png}{options: width=70\%}}
NULL

#' @rdname add_overall_tbl_ae
#' @export
add_overall.tbl_adverse_event <- function(x, type = c("both", "by", "strata"), ...) {
  # check inputs ---------------------------------------------------------------
  type <- match.arg(type)
  if (is.null(x$inputs$by) && is.null(x$inputs$strata)) {
    paste("Cannot use `add_overall()` when neither `by=` nor `strata=`",
          "were used the in the original summary table call.") %>%
      stop(call. = FALSE)
  }
  if (type %in% c("both", "strata") && is.null(x$inputs$strata)) {
    paste("Cannot summarize {.code type = c('both', 'strata')}",
          "when original summary table call doesn't have",
          "{.code strata=} specified.") %>%
      cli::cli_alert_danger()
    cli::cli_alert_info("Using {.code type = 'by'} instead.")
    type <- 'by'
  }
  if (type %in% c("both", "by") && is.null(x$inputs$by)) {
    paste("Cannot summarize {.code type = c('both', 'by')}",
          "when original summary table call doesn't have",
          "{.code by=} specified.") %>%
      cli::cli_alert_danger()
    cli::cli_alert_info("Using {.code type = 'strata'} instead.")
    type <- 'strata'
  }

  # running data summary function ----------------------------------------------
  tbl_args <- x$inputs
  if (type %in% "both") {
    # table without by variable
    tbl_args_by <- tbl_args
    tbl_args_by$by <- NULL
    tbl_args_by$header <- NULL
    tbl_overall_by <- do.call(class(x)[1], tbl_args_by)

    # table without strata variable
    tbl_args_strata <- tbl_args
    tbl_args_strata$strata <- NULL
    tbl_overall_strata <-
      do.call(class(x)[1], tbl_args_strata) %>%
      gtsummary::modify_spanning_header(
        gtsummary::all_stat_cols() ~ "**Overall**"
      )

    # table with neither by nor strata variables
    tbl_args$by <- NULL
    tbl_args$header <- NULL
    tbl_args$strata <- NULL
    tbl_overall_neither <-
      do.call(class(x)[1], tbl_args) %>%
      gtsummary::modify_spanning_header(
        gtsummary::all_stat_cols() ~ "**Overall**"
      )

    tbl_overall <-
      list(tbl_overall_by, tbl_overall_strata, tbl_overall_neither) %>%
      gtsummary::tbl_merge(tab_spanner = FALSE)
  }
  else if (type %in% "by") {
    tbl_args$by <- NULL
    tbl_args$header <- NULL
    tbl_overall <-
      do.call(class(x)[1], tbl_args)
  }
  else if (type %in% "strata") {
    tbl_args$strata <- NULL
    tbl_overall <-
      do.call(class(x)[1], tbl_args) %>%
      gtsummary::modify_spanning_header(
        gtsummary::all_stat_cols() ~ "**Overall**"
      )
  }

  # merging tbl_overall with original call -------------------------------------
  tbl_final <- gtsummary::tbl_merge(list(x, tbl_overall), tab_spanner = FALSE)

  # grouping columns with same spanning header together ------------------------
  if (type %in% c("both", "by") && !is.null(x$inputs$strata)) {
    column_order <-
      unique(tbl_final$table_styling$header$spanning_header) %>%
      stats::na.omit() %>%
      purrr::map(
        ~ tbl_final$table_styling$header %>%
          filter(.data$spanning_header %in% .x) %>%
          dplyr::pull(.data$column)
      ) %>%
      unlist()

    tbl_final <-
      tbl_final %>%
      gtsummary::modify_table_body(
        ~dplyr::relocate(.x, !!!column_order, .after = .data$label)
      )
  }

  # return merged tables -------------------------------------------------------
  class(tbl_final) <- class(x)
  tbl_final
}

#' @rdname add_overall_tbl_ae
#' @export
add_overall.tbl_ae_count <- add_overall.tbl_adverse_event

#' @rdname add_overall_tbl_ae
#' @export
add_overall.tbl_ae_focus <- add_overall.tbl_adverse_event
