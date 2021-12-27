#' Tabulate Raw AE Counts
#'
#' Create a table with the number of AE that were reported.
#'
#' @inheritParams tbl_adverse_event
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
      purrr::map(
        seq_len(length(lst_data)),
        function(index) {
          # keep observation that will be tabulated
          df_soc <-
            filter(lst_data[[index]], .data$..soc..) %>%
            dplyr::rename("..soc{index}.." := .data$..soc..)

          fn_tbl_soc <-
            purrr::partial(.fn_tbl,
                           variable = stringr::str_glue("..soc{index}.."),
                           label = names(lst_data[index]),
                           statistic = statistic,
                           header = header,
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
      )
  }

  # tabulate AEs ---------------------------------------------------------------
  lst_tbl_ae <-
    purrr::map(
      seq_len(length(lst_data)),
      function(index) {
        # keep observation that will be tabulated
        df_ae <-
          filter(lst_data[[index]], .data$..ae..) %>%
          dplyr::rename("ae{index}" := .data$ae)

        fn_tbl_ae <-
          purrr::partial(.fn_tbl,
                         variable = stringr::str_glue("ae{index}"),
                         statistic = statistic,
                         header = header,
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
    # return list with function's inputs and the complete data
    purrr::list_modify(inputs = tbl_ae_count_inputs) %>%
    # add class
    structure(class = c("tbl_ae_count", "gtsummary"))
}
