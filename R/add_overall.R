#' Tabulate Overall Summary
#'
#' @param x Object of class `"tbl_ae"`, `"tbl_ae_focus"`, or `"tbl_ae_count"`
#' @param across Specify the type of overall statistics to include.
#' - `"both"` adds summaries across both the `by=` and `strata=` levels
#' - `"by"` adds summaries across the `by=` levels
#' - `"strata"` adds summaries across the `strata=` levels
#' - `"overall-only"` adds a single overall column
#' Default is all possible overall types.
#' @param ... Not used
#'
#' @name add_overall_tbl_ae
#'
#' @section Notes:
#' If the spanning headers are modified prior to the call of `add_overall()`,
#' the ordering of the columns may not be correct.
#'
#' @return Summary object of same input class
#' @examples
#' \donttest{
#' # Example 1 -----------------------------------------------------------------
#' add_overall_ex1 <-
#'   df_adverse_events %>%
#'   tbl_ae_count(
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade,
#'     strata = trt
#'   ) %>%
#'   add_overall() %>%
#'   modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
#'   bold_labels()
#'
#' # Example 2 -----------------------------------------------------------------
#' add_overall_ex2 <-
#'   df_adverse_events %>%
#'   tbl_ae(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade
#'   ) %>%
#'   add_overall(across = 'by') %>%
#'   modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
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
#'   add_overall(across = 'strata')
#'
#' # Example 4 -----------------------------------------------------------------
#' add_overall_ex4 <-
#'   df_adverse_events %>%
#'   tbl_ae(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade,
#'     strata = trt
#'   ) %>%
#'   add_overall(across = 'overall-only') %>%
#'   modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
#'   bold_labels()
#'}
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{add_overall_ex1.png}{options: width=100\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{add_overall_ex2.png}{options: width=70\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{add_overall_ex3.png}{options: width=90\%}}
#'
#' \if{html}{Example 4}
#'
#' \if{html}{\figure{add_overall_ex4.png}{options: width=90\%}}
NULL

#' @rdname add_overall_tbl_ae
#' @export
add_overall.tbl_ae <- function(x, across = NULL, ...) {
  # check inputs ---------------------------------------------------------------
  rlang::check_dots_empty()

  # assign default value if `across=` not specified
  across <-
    across %||%
    dplyr::case_when(is.null(x$inputs$strata) ~ "by",
                     is.null(x$inputs$by) ~  "strata",
                     TRUE ~ "both")
  across <- match.arg(across, choices = c("both", "by", "strata", "overall-only"))

  if (is.null(x$inputs$by) && is.null(x$inputs$strata)) {
    paste("Cannot use `add_overall()` when neither `by=` nor `strata=`",
          "were used the in the original summary table call.") %>%
      stop(call. = FALSE)
  }
  if (across %in% c("both", "strata") && is.null(x$inputs$strata)) {
    paste("Cannot summarize {.code across = c('both', 'strata')}",
          "when original summary table call doesn't have",
          "{.code strata=} specified.") %>%
      cli::cli_alert_danger()
    cli::cli_alert_info("Using {.code across = 'by'} instead.")
    across <- 'by'
  }
  if (across %in% c("both", "by") && is.null(x$inputs$by)) {
    paste("Cannot summarize {.code across = c('both', 'by')}",
          "when original summary table call doesn't have",
          "{.code by=} specified.") %>%
      cli::cli_alert_danger()
    cli::cli_alert_info("Using {.code across = 'strata'} instead.")
    across <- 'strata'
  }

  # running data summary function ----------------------------------------------
  tbl_args <- x$inputs
  if (across %in% "both") {
    # table without by variable
    tbl_args_by <- tbl_args
    tbl_args_by$by <- tbl_args_by$by_values <- NULL
    tbl_overall_by <- do.call(class(x)[1], tbl_args_by)
    # add overall indicator
    tbl_overall_by$table_styling$header$modify_selector_overall <-
      ifelse(!is.na(tbl_overall_by$table_styling$header$modify_selector_overall), TRUE, NA)

    # table without strata variable
    tbl_args_strata <- tbl_args
    tbl_args_strata$data[[tbl_args_strata$strata]] <- "Overall"
    if (!is.null(tbl_args_strata$id_df))
      tbl_args_strata$id_df[[tbl_args_strata$strata]] <- "Overall"

    tbl_overall_strata <- do.call(class(x)[1], tbl_args_strata)

    # table with neither by nor strata variables
    tbl_args_neither <- tbl_args
    tbl_args_neither$data[[tbl_args_neither$strata]] <- "Overall"
    if (!is.null(tbl_args_neither$id_df))
      tbl_args_neither$id_df[[tbl_args_neither$strata]] <- "Overall"
    tbl_args_neither$by <- tbl_args_neither$by_values <- NULL

    tbl_overall_neither <- do.call(class(x)[1], tbl_args_neither)
    # add overall indicator
    tbl_overall_neither$table_styling$header$modify_selector_overall <-
      ifelse(!is.na(tbl_overall_neither$table_styling$header$modify_selector_overall), TRUE, NA)

    tbl_overall <-
      list(tbl_overall_by, tbl_overall_strata, tbl_overall_neither) %>%
      gtsummary::tbl_merge(tab_spanner = FALSE)
  }
  else if (across %in% "overall-only") {
    tbl_args$by <- tbl_args$by_values <- tbl_args$strata <- NULL
    tbl_overall <- do.call(class(x)[1], tbl_args)
    # add overall indicator
    tbl_overall$table_styling$header$modify_selector_overall <-
      ifelse(!is.na(tbl_overall$table_styling$header$modify_selector_overall), TRUE, NA)

  }
  else if (across %in% "by") {
    tbl_args$by <- tbl_args$by_values <- NULL
    tbl_overall <- do.call(class(x)[1], tbl_args)
    # add overall indicator
    tbl_overall$table_styling$header$modify_selector_overall <-
      ifelse(!is.na(tbl_overall$table_styling$header$modify_selector_overall), TRUE, NA)

  }
  else if (across %in% "strata") {
    tbl_args$data[[tbl_args$strata]] <- "Overall"
    if (!is.null(tbl_args$id_df))
      tbl_args$id_df[[tbl_args$strata]] <- "Overall"

    tbl_overall <- do.call(class(x)[1], tbl_args)
  }

  # merging tbl_overall with original call -------------------------------------
  tbl_final <-
    gtsummary::tbl_merge(list(x, tbl_overall), tab_spanner = FALSE) %>%
    # re-ordering rows to be the same as before the merge
    gtsummary::modify_table_body(
      ~ dplyr::left_join(
        x$table_body %>% select(variable, row_type, label),
        .x,
        by  = c("variable", "row_type", "label")
      )
    )

  # grouping columns with same spanning header together ------------------------
  if (across %in% c("both", "by") && !is.null(x$inputs$strata)) {
    df_spanning_headers <-
      dplyr::inner_join(
        tbl_final$table_styling$header %>%
          dplyr::filter(!.data$hide) %>%
          select("column"),
        tbl_final$table_styling$spanning_header,
        by = "column"
      ) %>%
      dplyr::slice_tail(by = c("level", "column"), n = 1)

    column_order <-
      unique(df_spanning_headers$spanning_header) %>%
      stats::na.omit() %>%
      purrr::map(
        ~ tbl_final$table_styling$header %>%
          dplyr::left_join(df_spanning_headers[c("column", "spanning_header")], by = "column") %>%
          filter(.data$spanning_header %in% .x) %>%
          dplyr::pull("column")
      ) %>%
      unlist()

    tbl_final <-
      tbl_final %>%
      gtsummary::modify_table_body(
        ~dplyr::relocate(.x, !!!column_order, .after = "label")
      )
  }

  # return merged tables -------------------------------------------------------
  class(tbl_final) <- class(x)
  tbl_final %>%
    gtsummary::tbl_butcher() %>%
    purrr::list_modify(
      inputs = x$inputs,
      header_info = tbl_final$header_info
    )
}

#' @rdname add_overall_tbl_ae
#' @export
add_overall.tbl_ae_count <- add_overall.tbl_ae

#' @rdname add_overall_tbl_ae
#' @export
add_overall.tbl_ae_focus <- add_overall.tbl_ae

