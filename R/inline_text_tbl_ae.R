#' Report Values from `tbl_ae()` in-line
#'
#' @param x an object of class `tbl_ae()`, `tbl_ae_count()`, `tbl_ae_focus()`
#' @param ae_or_soc string indicating the AE or SOC to report
#' @param column column name of cell to report. Use `show_header_names(x)`
#' to print all column names beside the current header.
#' @param ... not used
#'
#' @return string
#' @name inline_text_tbl_ae
#'
#' @examples
#' # add example
NULL

#' @rdname inline_text_tbl_ae
#' @export
inline_text.tbl_ae <- function(x, ae_or_soc, column, ...) {
  # check inputs ---------------------------------------------------------------
  if (!rlang::is_string(ae_or_soc)) {
    stop("Argument `ae_or_soc=` must be a string.", call. = FALSE)
  }

  # identify the variable name associated with `ae_or_soc=` --------------------
  vct_ae_or_soc <-
    x$table_body %>%
    dplyr::pull(.data$label)

  if (!(ae_or_soc %in% vct_ae_or_soc)) {
    paste0("Invalid selection in `ae_or_soc=`.\n",
          "Select one of\n\n",
          paste(shQuote(vct_ae_or_soc), collapse = ", ")) %>%
      stop(call. = FALSE)
  }

  variable <-
    x$table_body %>%
    filter(.data$label %in% .env$ae_or_soc) %>%
    dplyr::pull(.data$variable)

  variable_is_ae <- startsWith(variable, "ae")

  # return result --------------------------------------------------------------
  browser()
  gtsummary::inline_text(
    x = structure(x, class = "gtsummary"), # forcing evaluation with `gtsummary::inline_text.gtsummary()`
    variable = variable,
    level = switch(variable_is_ae, ae_or_soc),
    column = {{ column }}
  )
}

#' @rdname inline_text_tbl_ae
#' @export
inline_text.tbl_ae_count <- inline_text.tbl_ae

#' @rdname inline_text_tbl_ae
#' @export
inline_text.tbl_ae_focus <- inline_text.tbl_ae
