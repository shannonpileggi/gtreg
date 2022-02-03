#' Make linebreak in LaTeX Table cells with gtsummary
#'
#' This function creates a call to `kableExtra::linebreak()`, and is meant to used in
#' `as_kable_extra(x, col.names = linebreak_gtsummary(x), escape = FALSE, format = "latex")`.
#'  - the default `align=` argument is taken from the gtsummary object
#'  - the markdown double-star bold syntax is converted to LaTeX, `\textbf{}`
#'
#' @param x a gtsummary table
#' @param align instruction passed to `kableExtra::linebreak(align=)`. Default
#' is NULL, which pulls alignment instructions from the gtsummary object.
#' @inheritParams kableExtra::linebreak
#'
#' @return string of column names in LaTeX format
#' @export
#'
#' @examples
#' # Example 1 -----------------------------------------------------------------
#' df_adverse_events %>%
#'   tbl_ae(
#'     id = patient_id,
#'     ae = adverse_event,
#'     by = grade,
#'     header_by = "**Grade {level}**"
#'   ) %>%
#'   as_kable_extra(
#'     col.names = .linebreak_gtsummary(.),
#'     escape = FALSE,
#'     format = "latex"
#'   )
.linebreak_gtsummary <- function(x,
                                 align = NULL,
                                 double_escape = FALSE,
                                 linebreaker = "\n") {
  # check inputs ---------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("'x' must be a 'gtsummary' table.", call. = FALSE)
  }
  rlang::check_installed("kableExtra", reason = "to use `linebreak_kable_extra()`.")

  # set align argument ---------------------------------------------------------
  align <-
    align %||%
    stringr::str_sub(dplyr::filter(x$table_styling$header, !.data$hide)$align, 1, 1)

  # return linebreak result ----------------------------------------------------
  kableExtra::linebreak(
    x =
      dplyr::filter(x$table_styling$header, !.data$hide) %>%
      dplyr::pull(.data$label) %>%
      stringr::str_replace_all(
        pattern = "\\*\\*(.*?)\\*\\*",
        replacement = "\\\\textbf{\\1}"
      ),
    align = align,
    double_escape = double_escape,
    linebreaker = linebreaker
  )
}
