#' Column Selectors
#'
#' See the \href{https://shannonpileggi.github.io/gtreg/articles/table-modifications.html}{Table modifications article} for examples.
#' - `all_ae_cols(overall, unknown)` selects all columns summarizing AE statistics. By default, unknown and overall columns are not selected.
#' - `all_cols_in_strata(strata)` selects all columns from specified stratum.
#' - `all_overall_cols()` selects all overall columns
#' - `all_unknown_cols()` selects all unknown columns
#'
#' @param overall logical indicating whether to include the overall columns.
#' Default is FALSE
#' @param unknown logical indicating whether to include the unknown or missing columns.
#' Default is FALSE
#' @param strata character vector of the selected stratum
#'
#' @name selectors
#' @return selected columns
#'
#' @seealso `gtsummary::all_stat_cols()`
#'
#' @examples
#' \donttest{
#' selectors_ex1 <-
#'   df_adverse_events %>%
#'   dplyr::mutate(grade = ifelse(dplyr::row_number() == 1L, NA, grade)) %>%
#'   tbl_ae(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade
#'   ) %>%
#'   add_overall(across = 'by') %>%
#'   modify_header(
#'     all_ae_cols() ~ "**Grade {by}**",
#'     all_overall_cols() ~ "**Total**",
#'     all_unknown_cols() ~ "**Unknown Grade**"
#'   )
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{selectors_ex1.png}{options: width=65\%}}
NULL

#' @export
#' @rdname selectors
all_ae_cols <- function(overall = FALSE, unknown = FALSE) {
  # construct filtering expression
  if (identical(overall, FALSE) && identical(unknown, FALSE))
    expr <- rlang::expr(.data$hide %in% FALSE & !.data$overall %in% TRUE & !.data$unknown %in% TRUE)
  else if (identical(overall, FALSE))
    expr <- rlang::expr(.data$hide %in% FALSE & !.data$overall %in% TRUE)
  else if (identical(unknown, FALSE))
    expr <- rlang::expr(.data$hide %in% FALSE & !.data$unknown %in% TRUE)
  else expr <- rlang::expr(.data$hide %in% FALSE & startsWith(.data$column, "stat_"))

  broom.helpers::.generic_selector(
    variable_column = "column",
    select_column = c("hide", "overall", "unknown"),
    select_expr = !!expr,
    fun_name = "all_ae_cols"
  )
}

#' @export
#' @rdname selectors
all_cols_in_strata <- function(strata) {
  broom.helpers::.generic_selector(
    variable_column = "column",
    select_column = c("hide", "strata"),
    select_expr = .data$hide %in% FALSE & .data$strata %in% .env$strata,
    fun_name = "all_cols_in_strata"
  )
}

#' @export
#' @rdname selectors
all_overall_cols <- function() {
  broom.helpers::.generic_selector(
    variable_column = "column",
    select_column = c("hide", "overall"),
    select_expr = .data$hide %in% FALSE & .data$overall %in% TRUE,
    fun_name = "all_overall_cols"
  )
}

#' @export
#' @rdname selectors
all_unknown_cols <- function() {
  broom.helpers::.generic_selector(
    variable_column = "column",
    select_column = c("hide", "unknown"),
    select_expr = .data$hide %in% FALSE & .data$unknown %in% TRUE,
    fun_name = "all_unknown_cols"
  )
}
