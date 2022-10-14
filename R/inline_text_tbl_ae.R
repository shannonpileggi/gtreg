#' Report Values from gtreg tables in-line
#'
#' Function allows users to report formatted and styled results from
#' gtreg tables in-line.
#'
#' @param x an object of class `tbl_ae()`, `tbl_ae_count()`, `tbl_ae_focus()`
#' @param row string indicating the AE or SOC to report
#' @param column column name of cell to report. Use `show_header_names(x)`
#' to print all column names beside the current header.
#' @param ... not used
#'
#' @return string
#' @name inline_text_tbl_ae
#' @examples
#' \donttest{
#' tbl <-
#'   df_adverse_events %>%
#'   tbl_ae(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade
#'   )
#' show_header_names(tbl)
#'
#' inline_text(tbl, "Anaemia", column = stat_5)
#'}
NULL

#' @rdname inline_text_tbl_ae
#' @export
inline_text.tbl_ae <- function(x, row, column = NULL, ...) {
  # check inputs ---------------------------------------------------------------
  rlang::check_dots_empty()
  column <- rlang::enquo(column)
  if (rlang::quo_is_null(column)) {
    cli::cli_alert_danger("The {.code column=} argument is required.")
    cli::cli_alert_info(
      "Run {.code show_header_names(x)} to list the column names and headers.")
    return(invisible())
  }
  if (!rlang::is_string(row)) {
    stop("Argument `row=` must be a string.", call. = FALSE)
  }

  # identify the variable name associated with `row=` --------------------------
  vct_ae_or_soc <-
    x$table_body %>%
    dplyr::pull("label")

  if (!(row %in% vct_ae_or_soc)) {
    paste0("Invalid selection in `row=`.\n",
          "Select one of\n\n",
          paste(shQuote(vct_ae_or_soc), collapse = ", ")) %>%
      stop(call. = FALSE)
  }

  variable <-
    x$table_body %>%
    filter(.data$label %in% .env$row) %>%
    dplyr::pull("variable")

  variable_is_ae <- startsWith(variable, "ae")

  # return result --------------------------------------------------------------
  gtsummary::inline_text(
    x = structure(x, class = "gtsummary"), # forcing evaluation with `gtsummary::inline_text.gtsummary()`
    variable = all_of(variable),
    level = switch(variable_is_ae, row),
    column = !!column
  )
}

#' @rdname inline_text_tbl_ae
#' @export
inline_text.tbl_ae_count <- inline_text.tbl_ae

#' @rdname inline_text_tbl_ae
#' @export
inline_text.tbl_ae_focus <- inline_text.tbl_ae
