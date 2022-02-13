#' Column Selectors
#'
#' - `all_ae_cols(overall, unknown)` selects all columns summarizing AE statistics. By default, unknown and overall columns are not selected.
#' - `all_strata_cols(strata)` selects all columns from specified stratum.
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
#' @examples
#' modify_ae_header_ex1 <-
#'   df_adverse_events %>%
#'   dplyr::mutate(grade = ifelse(dplyr::row_number() == 1L, NA, grade)) %>%
#'   tbl_ae(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade
#'   ) %>%
#'   modify_ae_header(
#'     all_ae_cols() ~ "**Grade {by}**",
#'     all_overall_cols() ~ "**Total**",
#'     all_unknown_cols() ~ "**Unknown Grade**"
#'   )
NULL

#' @export
#' @rdname selectors
all_ae_cols <- function(overall = FALSE, unknown = FALSE) {
  # construct filtering expression
  if (identical(overall, FALSE) && identical(unknown, FALSE))
    expr <- rlang::expr(.data$overall %in% FALSE & .data$unknown %in% FALSE)
  else if (identical(overall, FALSE))
    expr <- rlang::expr(.data$overall %in% FALSE)
  else if (identical(unknown, FALSE))
    expr <- rlang::expr(.data$unknown %in% FALSE)
  else expr <- rlang::expr(!is.na(.data$variable))

  broom.helpers::.generic_selector(
    variable_column = "variable",
    select_column = c("overall", "unknown"),
    select_expr = !!expr,
    fun_name = "all_ae_cols"
  )
}

#' @export
#' @rdname selectors
all_strata_cols <- function(strata) {
  broom.helpers::.generic_selector(
    variable_column = "variable",
    select_column = "strata",
    select_expr = .data$strata %in% .env$strata,
    fun_name = "all_strata_cols"
  )
}

#' @export
#' @rdname selectors
all_overall_cols <- function() {
  broom.helpers::.generic_selector(
    variable_column = "variable",
    select_column = "overall",
    select_expr = .data$overall %in% TRUE,
    fun_name = "all_overall_cols"
  )
}

#' @export
#' @rdname selectors
all_unknown_cols <- function() {
  broom.helpers::.generic_selector(
    variable_column = "variable",
    select_column = "unknown",
    select_expr = .data$unknown %in% TRUE,
    fun_name = "all_unknown_cols"
  )
}
