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
#'
#' @examplesIf isTRUE(Sys.getenv("NOT_CRAN") %in% c("true", ""))
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
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_ae_focus_ex1.png}{options: width=70\%}}

tbl_ae_focus <- function(data, include, id, ae, soc = NULL, strata = NULL,
                         id_df = NULL, statistic = "{n} ({p})",
                         label = NULL, zero_symbol = "\U2014") {
  # evaluate bare selectors/check inputs ---------------------------------------
  if(!inherits(data, "data.frame")) {
    stop("`data=` argument must be a tibble or data frame.", call. = FALSE)
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
            stop("Columns indicated in `include=` must be class 'logical'.")))

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
    mutate(across(all_of(include), ~tidyr::replace_na(., "FALSE"))) %>%
    group_by(across(any_of("soc")))

  # putting data into list of tibbles...one element per SOC --------------------
  lst_data_complete <-
    data_complete %>%
    dplyr::group_split() %>%
    rlang::set_names(dplyr::group_keys(data_complete) %>% purrr::pluck(1))

  # tabulate SOC ---------------------------------------------------------------
  if (!is.null(soc)) {
    lst_tbl_soc_wide <-
      purrr::map(
        include,
        ~ .lst_of_tbls(
          lst_data =
            purrr::map(
              lst_data_complete,
              function(data) {
                group_by(data, .data$soc) %>%
                  mutate(across(any_of(.x), ~max(.))) %>%
                  dplyr::ungroup()
              }
            ),
          variable_summary = "..soc..",
          variable_filter = "..soc..",
          by = .x,
          by_level_to_hide = "FALSE",
          statistic = statistic,
          header =
            label[[.x]] %||%
            attr(data[[.x]], "label") %||%
            .x %>%
            {stringr::str_glue("**{.}**")},
          remove_header_row = FALSE,
          zero_symbol = zero_symbol,
          labels = names(lst_data_complete)
        )
      )

    # merge tbls to put `include=` variables side by side
    lst_tbl_soc <-
      purrr::pmap(
        purrr::map(seq_len(length(include)), ~lst_tbl_soc_wide[[.x]]),
        list
      ) %>%
      purrr::map(
        gtsummary::tbl_merge,
        tab_spanner = FALSE
      )
  }

  # tabulate AEs ---------------------------------------------------------------
  lst_tbl_ae_wide <-
    purrr::map(
      include,
      ~ .lst_of_tbls(
        lst_data = lst_data_complete,
        variable_summary = "ae",
        variable_filter = "..ae..",
        by = .x,
        by_level_to_hide = "FALSE",
        statistic = statistic,
        header =
          label[[.x]] %||%
          attr(data[[.x]], "label") %||%
          .x %>%
          {stringr::str_glue("**{.}**")},
        remove_header_row = TRUE,
        zero_symbol = zero_symbol
      )
    )


  # merge tbls to put `include=` variables side by side
  lst_tbl_ae <-
    purrr::pmap(
      purrr::map(seq_len(length(include)), ~lst_tbl_ae_wide[[.x]]),
      list
    ) %>%
    purrr::map(
      gtsummary::tbl_merge,
      tab_spanner = FALSE
    )

  # stacking tbls into big final AE table --------------------------------------
  if (is.null(soc)) tbl_final <- .stack_soc_ae_tbls(lst_tbl_ae)
  else tbl_final <- .stack_soc_ae_tbls(lst_tbl_ae, lst_tbl_soc)

  # return final tbl -----------------------------------------------------------
  tbl_final %>%
    # return list with function's inputs
    purrr::list_modify(inputs = tbl_ae_focus_inputs) %>%
    # add class
    structure(class = c("tbl_ae_focus", "gtsummary"))
}


