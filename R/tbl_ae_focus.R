#' Tabulate AE Focused (Dichotomous) Summaries
#'
#' Summarize dichotomous AE data. For example, report the
#' rate of patients that have an AE of Grade 3 or higher.
#'
#' @inheritParams tbl_adverse_event
#' @param include Vector of column names to summarize. Column names may be
#' quoted or unquoted. All columns must be class 'logical'.
#' @param label A named list of labels that will be applied in the
#' resulting table. Names must be those passed in `include=`. Default is
#' NULL, and either the label attribute or the column name will be used.
#'
#' @return a 'tbl_ae_focus' object
#' @export
#'
#' @examples
#' # Example 1 -----------------------------------------------------------------
#' tbl_ae_focus_ex1 <-
#'   df_adverse_events %>%
#'   dplyr::mutate(
#'     any_complication = TRUE,
#'     grade3_complication = grade >= 3
#'   ) %>%
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
                          label = NULL) {
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

  purrr::walk(
    include,
    ~switch(!is.logical(data[[.x]]),
            stop("Columns indicated in `include=` must be class 'logical'.")))

  # will return inputs ---------------------------------------------------------
  tbl_ae_focus_inputs <- as.list(environment())

  # obtain the complete data ---------------------------------------------------
  data_complete <-
    .complete_ae_data(data, id = id, ae = ae, soc = soc, by = NULL,
                      strata = strata, id_df = id_df, by_values = NULL) %>%
    group_by(across(any_of("soc")))

  if (any(include %in% names(data_complete))) {
    stringr::str_glue(
      "'{intersect(include, names(data_complete))[1]}' is a protected name",
      "and cannot be used in `include=`.") %>%
      stop(call. = FALSE)
  }

  # merge in the `include=` variable to the complete data ----------------------
  # putting data into list of tibbles...one element per SOC
  lst_data_complete <-
    data_complete %>%
    dplyr::group_split() %>%
    rlang::set_names(dplyr::group_keys(data_complete) %>% purrr::pluck(1))

  # tabulate SOC ---------------------------------------------------------------
  if (!is.null(soc)) {
    lst_tbl_soc <-
      purrr::map(
        seq_len(length(lst_data_complete)),
        function(index) {
          purrr::map(
            include,
            function(binary_varname) {
              # keep observation that will be tabulated
              df_soc <-
                lst_data_complete[[index]] %>%
                dplyr::left_join(
                  data %>%
                    select(any_of(c(id, ae, soc, binary_varname))) %>%
                    dplyr::rename(!!!purrr::compact(list(id = id, ae = ae, soc = soc))),
                  by = names(purrr::compact(list(id = id, ae = ae, soc = soc)))
                ) %>%
                group_by(.data$id, .data$soc) %>%
                mutate(
                  across(any_of(binary_varname), ~tidyr::replace_na(., FALSE)),
                  across(any_of(binary_varname), ~as.logical(max(.))),
                  across(any_of(binary_varname), ~factor(., levels = c(FALSE, TRUE)))
                ) %>%
                dplyr::ungroup() %>%
                dplyr::distinct() %>%
                filter(.data$..soc..) %>%
                dplyr::rename("..soc{index}.." := .data$..soc..)

              fn_tbl_soc <-
                purrr::partial(fn_tbl,
                               variable = stringr::str_glue("..soc{index}.."),
                               label = names(lst_data_complete[index]),
                               by = binary_varname,
                               by_level_to_hide = "FALSE",
                               statistic = statistic,
                               header =
                                 label[[binary_varname]] %||%
                                 attr(data[[binary_varname]], "label") %||%
                                 binary_varname %>%
                                 {stringr::str_glue("**{.}**")},
                               remove_header_row = FALSE,
                               zero_symbol = NULL)

              if ("strata" %in% names(df_soc)) {
                tbl <-
                  gtsummary::tbl_strata(
                    data = df_soc,
                    strata = "strata",
                    .tbl_fun = ~fn_tbl_soc(data = .x)
                  )
              }
              else {
                tbl <- fn_tbl_soc(data = df_soc)
              }

              tbl
            }
          ) %>%
            gtsummary::tbl_merge(tab_spanner = FALSE)
        }
      )
  }

  # tabulate AEs ---------------------------------------------------------------
  lst_tbl_ae <-
    purrr::map(
      seq_len(length(lst_data_complete)),
      function(index) {
        purrr::map(
          include,
          function(binary_varname) {
            # keep observation that will be tabulated
            df_ae <-
              lst_data_complete[[index]] %>%
              dplyr::left_join(
                data %>%
                  select(any_of(c(id, ae, soc, binary_varname))) %>%
                  dplyr::rename(!!!purrr::compact(list(id = id, ae = ae, soc = soc))),
                by = names(purrr::compact(list(id = id, ae = ae, soc = soc)))
              ) %>%
              group_by(.data$id, .data$ae) %>%
              mutate(
                across(any_of(binary_varname), ~tidyr::replace_na(., FALSE)),
                across(any_of(binary_varname), ~as.logical(max(.))),
                across(any_of(binary_varname), ~factor(., levels = c(FALSE, TRUE)))
              ) %>%
              dplyr::ungroup() %>%
              dplyr::distinct() %>%
              filter(.data$..ae..) %>%
              dplyr::rename("ae{index}" := .data$ae)

            fn_tbl_ae <-
              purrr::partial(fn_tbl,
                             variable = stringr::str_glue("ae{index}"),
                             by = binary_varname,
                             by_level_to_hide = "FALSE",
                             statistic = statistic,
                             header =
                               label[[binary_varname]] %||%
                               attr(data[[binary_varname]], "label") %||%
                               binary_varname %>%
                               {stringr::str_glue("**{.}**")},
                             remove_header_row = TRUE,
                             zero_symbol = NULL)

            if ("strata" %in% names(df_ae)) {
              tbl <-
                gtsummary::tbl_strata(
                  data = df_ae,
                  strata = "strata",
                  .tbl_fun = ~fn_tbl_ae(data = .x)
                )
            }
            else {
              tbl <- fn_tbl_ae(data = df_ae)
            }

            tbl
          }
        ) %>%
          gtsummary::tbl_merge(tab_spanner = FALSE)
      }
    )

  # stacking tbls into big final AE table --------------------------------------
  if (!is.null(soc)) {
    tbl_final <-
      # stack SOC with AEs within that SOC, then stack all tbls
      purrr::map2(lst_tbl_soc, lst_tbl_ae,
                  ~gtsummary::tbl_stack(list(.x, .y), quiet = TRUE)) %>%
      gtsummary::tbl_stack(quiet = TRUE)
  }
  else {
    tbl_final <-
      gtsummary::tbl_stack(lst_tbl_ae, quiet = TRUE) %>%
      # remove indentation for AEs
      gtsummary::modify_table_styling(
        columns = "label",
        text_format = "indent",
        undo_text_format = TRUE
      )
  }

  # return final tbl -----------------------------------------------------------
  tbl_final %>%
    # update labels
    gtsummary::modify_header(label = "**Adverse Event**") %>%
    # removing the no longer needed data elements saved in the individual stacked/merged tbls
    gtsummary::tbl_butcher() %>%
    # return list with function's inputs
    purrr::list_modify(inputs = tbl_ae_focus_inputs) %>%
    # add class
    structure(class = c("tbl_adverse_event", "gtsummary"))
}


