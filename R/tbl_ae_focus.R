#' Tabulate AE Focused (Dichotomous) Summaries
#'
#' Summarize dichotomous AE data. For example, report the
#' rate of patients that have an AE of Grade 3 or higher.
#'
#' @inheritParams tbl_ae
#' @param include Vector of column names to summarize. Column names may be
#' quoted or unquoted. All columns must be class 'logical'.
#' @param label A named list of labels that will be applied in the
#' resulting table. Names must be those passed in `include=`. Default is
#' NULL, and either the label attribute or the column name will be used.
#'
#' @return a 'tbl_ae_focus' object
#' @export
#' @examples
#' \donttest{
#' # Example 1 -----------------------------------------------------------------
#' tbl_ae_focus_ex1 <-
#'   df_adverse_events %>%
#'   tbl_ae_focus(
#'     include = c(any_complication, grade3_complication),
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     label =
#'       list(any_complication = "Any Grade Complication",
#'            grade3_complication = "Grade 3+ Complication")
#'   ) %>%
#'   bold_labels()
#'   }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_ae_focus_ex1.png}{options: width=75\%}}

tbl_ae_focus <- function(data,
                         include,
                         id,
                         ae,
                         soc = NULL,
                         strata = NULL,
                         label = NULL,
                         id_df = NULL,
                         statistic = "{n} ({p})",
                         digits = NULL,
                         sort = NULL,
                         zero_symbol = "\U2014") {
  # evaluate bare selectors/check inputs ---------------------------------------
  if(!inherits(data, "data.frame")) {
    stop("`data=` argument must be a tibble or data frame.", call. = FALSE)
  }
  if (!is.null(sort)) {
    sort <- match.arg(sort, choices = c("ae", "soc"), several.ok = TRUE)
  }

  id <-
    .select_to_varnames({{ id }}, data = data,
                        arg_name = "id", select_single = TRUE)
  ae <-
    .select_to_varnames({{ ae }}, data = data,
                        arg_name = "ae", select_single = TRUE)
  soc <-
    .select_to_varnames({{ soc }}, data = data,
                        arg_name = "soc", select_single = TRUE)
  strata <-
    .select_to_varnames({{ strata }}, data = data,
                        arg_name = "strata", select_single = TRUE)
  include <-
    .select_to_varnames({{ include }}, data = data,
                        arg_name = "include", select_single = FALSE)
  label <-
    .formula_list_to_named_list(x = {{ label }},
                                data = data,
                                arg_name = "label",
                                type_check = rlang::is_string)
  if (is.null(include) || is.null(id) || is.null(ae)) {
    stop("Arguments `include=`, `id=`, `ae=` must be specified.", call. = FALSE)
  }

  purrr::walk(
    include,
    ~switch(!is.logical(data[[.x]]),
            stop("Columns indicated in `include=` must be class 'logical'.", call. = FALSE)))
  purrr::walk(
    include,
    ~switch(any(is.na(data[[.x]])),
            stop("Columns indicated in `include=` cannot be NA.", call. = FALSE)))

  # will return inputs ---------------------------------------------------------
  tbl_ae_focus_inputs <- as.list(environment())

  # obtain the complete data ---------------------------------------------------
  data_complete <-
    .complete_ae_data(data, id = id, ae = ae, soc = soc, by = NULL,
                      strata = strata, id_df = id_df, by_values = NULL)

  if (any(include %in% names(data_complete))) {
    stringr::str_glue(
      "'{intersect(include, names(data_complete))[1]}' is a protected name",
      "and cannot be used in `include=`.") %>%
      stop(call. = FALSE)
  }

  # merge in the `include=` variable to the complete data ----------------------
  lst_rename <- purrr::compact(list(id = id, ae = ae, soc = soc))

  data_complete <-
    purrr::map(
      include,
      ~data %>%
        select(!!!lst_rename, all_of(.x)) %>%
        group_by(across(all_of(names(lst_rename)))) %>%
        mutate(
          across(all_of(.x),
                 ~tidyr::replace_na(., FALSE) %>%
                   max() %>%
                   as.logical() %>%
                   factor(levels = c(FALSE, TRUE), ordered = TRUE))
        ) %>%
        arrange(across(all_of(.x))) %>%
        dplyr::slice_tail() %>%
        ungroup()
    ) %>%
    purrr::reduce(
      .f = ~dplyr::left_join(.x, .y, by = names(lst_rename)),
      .init = data_complete
    )  %>%
    mutate(across(all_of(include), ~tidyr::replace_na(., "FALSE")))


  # tabulate SOC ---------------------------------------------------------------
  if (!is.null(soc)) {
    tbl_soc <-
      purrr::map(
        include,
        ~.construct_summary_table(
          data = data_complete %>% filter(.data$..soc..),
          variable = "soc",
          by = .x,
          digits = digits,
          statistic = statistic,
          sort = sort,
          zero_symbol = zero_symbol,
          missing_location = "first",
          columns_to_hide = c("NOT OBSERVED", "FALSE"),
          header = glue::glue("**{label[[.x]] %||% attr(data[[.x]], 'label') %||% .x}**")
        )
      ) %>%
      gtsummary::tbl_merge(tab_spanner = FALSE)
  }

  # tabulate AE ----------------------------------------------------------------
  tbl_ae <-
    purrr::map(
      include,
      ~.construct_summary_table(
        data = data_complete %>% filter(.data$..ae..),
        variable = "ae",
        by = .x,
        digits = digits,
        statistic = statistic,
        sort = sort,
        zero_symbol = zero_symbol,
        missing_location = "first",
        columns_to_hide = c("NOT OBSERVED", "FALSE"),
        header = glue::glue("**{label[[.x]] %||% attr(data[[.x]], 'label') %||% .x}**")
      )
    ) %>%
    gtsummary::tbl_merge(tab_spanner = FALSE)

  # combine the SOC and AE tbls ------------------------------------------------
  tbl_final <-
    .combine_soc_and_ae_tbls(
      data = data_complete,
      tbl_ae = tbl_ae,
      tbl_soc = switch("soc" %in% names(data_complete), tbl_soc)
    )

  # update `modify_stat_*` columns in `tbl$table_styling$header` ---------------
  tbl_final <- .update_modify_stat_columns(tbl = tbl_final, data = data_complete)

  # return final tbl -----------------------------------------------------------
  tbl_final %>%
    purrr::compact() %>%
    # add inputs
    purrr::list_modify(inputs = tbl_ae_focus_inputs) %>%
    # add class
    structure(class = c("tbl_ae_focus", "gtsummary")) %>%
    # add default spanning headers
    purrr::when(
      !is.null(strata) ~
        modify_spanning_header(., gtsummary::all_stat_cols() ~ "**{strata}**, N = {n}"),
      TRUE ~ modify_spanning_header(., gtsummary::all_stat_cols() ~ "**N = {N}**")
    )
}


