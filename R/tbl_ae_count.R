#' Tabulate Raw AE Counts
#'
#' Create a table with the number of AE that were reported.
#'
#' @inheritParams tbl_ae
#'
#' @return a 'tbl_ae_count' object
#' @export
#'
#' @examples
#' # Example 1 -----------------------------------------------------------------
#' tbl_ae_count_ex1 <-
#'   tbl_ae_count(
#'     data = df_adverse_events,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     strata = trt,
#'     by = grade,
#'     header = "**Grade {level}**"
#'   )

tbl_ae_count <- function(data, ae,
                         soc = NULL, by = NULL, strata = NULL,
                         by_values = NULL,
                         missing_text = "Unknown",
                         header = "**{level}**") {
  # evaluate bare selectors/check inputs ---------------------------------------
  stopifnot(inherits(data, "data.frame"))
  ae <-
    .select_to_varnames({{ ae }}, data = data,
                        arg_name = "ae", select_single = TRUE)
  soc <-
    .select_to_varnames({{ soc }}, data = data,
                        arg_name = "soc", select_single = TRUE)
  by <-
    .select_to_varnames({{ by }}, data = data,
                        arg_name = "by", select_single = TRUE)
  strata <-
    .select_to_varnames({{ strata }}, data = data,
                        arg_name = "strata", select_single = TRUE)

  # will return inputs ---------------------------------------------------------
  tbl_ae_count_inputs <- as.list(environment())
  statistic <- "{n}"

  # setting structure similar to that of data after `.complete_ae_data()` ------
  lst_name_recode <-
    list(id = NULL, strata = strata, ae = ae, soc = soc, by = by) %>%
    purrr::compact()

  data <-
    data %>%
    # select and rename variables
    dplyr::select(!!!lst_name_recode) %>%
    .prepare_by_levels(
      by = by,
      by_values = by_values,
      initial_missing = missing_text,
      initial_dummy = "NOT OBSERVED"
    ) %>%
    mutate(..ae.. = TRUE, ..soc.. = TRUE) %>%
    group_by(across(any_of("soc")))

  # putting data into list of tibbles...one element per SOC --------------------
  lst_data <-
    data %>%
    dplyr::group_split() %>%
    rlang::set_names(dplyr::group_keys(data) %>% purrr::pluck(1))

  # tablulate SOC --------------------------------------------------------------
  if (!is.null(soc)) {
    lst_tbl_soc <-
      .lst_of_tbls(lst_data = lst_data,
                   variable_summary = "..soc..",
                   variable_filter = "..soc..",
                   statistic = statistic,
                   header = header,
                   remove_header_row = FALSE,
                   zero_symbol = NULL,
                   labels = names(lst_data))
  }

  # tabulate AEs ---------------------------------------------------------------
  lst_tbl_ae <-
    .lst_of_tbls(lst_data = lst_data,
                 variable_summary = "ae",
                 variable_filter = "..ae..",
                 statistic = statistic,
                 header = header,
                 remove_header_row = TRUE,
                 zero_symbol = NULL,
                 labels = NULL)

  # stacking tbls into big final AE table --------------------------------------
  if (is.null(soc)) tbl_final <- .stack_soc_ae_tbls(lst_tbl_ae)
  else tbl_final <- .stack_soc_ae_tbls(lst_tbl_ae, lst_tbl_soc)

  # return final tbl -----------------------------------------------------------
  tbl_final %>%
    # return list with function's inputs and the complete data
    purrr::list_modify(inputs = tbl_ae_count_inputs) %>%
    # add class
    structure(class = c("tbl_ae_count", "gtsummary"))
}
