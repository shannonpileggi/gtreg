#' Tabulate Adverse Events
#'
#' @description
#' The function tabulates adverse events. One AE per ID will be counted in the
#' resulting table. If a `by=` variable is passed and a
#' patient experienced more than one of the same AE, the AE associated with the
#' highest `by=` level will be included. For example, if a patient has two of
#' the same AE and `by = grade`, the AE with the highest grade will be
#' included.
#' Similarly, if tabulations within system organ class are requested, the
#' AE within SOC associated with the highest grade will be tabulated.
#'
#' @param data Data frame
#' @param id Variable name of the patient ID
#' @param soc Variable name of the system organ class column
#' @param ae Variable name of the adverse event column
#' @param by Variable to split results by, e.g. report AEs by grade
#' @param strata Variable to stratify results by, e.g. report AEs summaries
#' by treatment group
#' @param id_df Optional data frame of complete id values and strata to achieve correct
#' base n for the situation in which not all subjects experience adverse events
#' @param by_values Optional vector of complete by values, listed in desired order,
#' to achieve correct table structure for the situation in which an adverse
#' event of a certain grade is not observed for a given soc
#' @param missing_text String that will be shown for missing levels of `by=`,
#' Default is `"Unknown"`
#' @param statistic String indicating the statistics that will be reported.
#' The default is `"{n} ({p})"`
#' @param header String indicating the header to be placed in the table.
#' Default is `"**{level}**"`
#'
#' @export
#' @examples
#' # Example 1 -----------------------------------------------------------------
#' df_adverse_events %>%
#'   tbl_adverse_event(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade,
#'     strata = trt,
#'     header = "**Grade {level}**"
#'   ) %>%
#'   as_kable() # UPDATE THIS WITH PROPER gt image at some point.
#'
#' # Example 2 -----------------------------------------------------------------
#' df_adverse_events %>%
#'   tbl_adverse_event(
#'     id = patient_id,
#'     ae = adverse_event,
#'     by = grade,
#'     header = "**Grade {level}**"
#'   ) %>%
#'   as_kable() # UPDATE THIS WITH PROPER gt image at some point.

tbl_adverse_event <- function(data, id, ae,
                               soc = NULL, by = NULL, strata = NULL,
                               id_df = NULL, by_values = NULL,
                               missing_text = "Unknown",
                               statistic = "{n} ({p})",
                               header = "**{level}**") {
  # evaluate bare selectors/check inputs ---------------------------------------
  stopifnot(inherits(data, "data.frame"))
  id <-
    .select_to_varnames({{ id }}, data = data,
                        arg_name = "id", select_single = TRUE)
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
  tbl_adverse_event_inputs <- as.list(environment())

  # obtain the complete data ---------------------------------------------------
  data_complete <-
    .complete_ae_data(data, id = id, ae = ae, soc = soc, by = by,
                      strata = strata, id_df = id_df, by_values = by_values,
                      missing_text = missing_text) %>%
    group_by(across(any_of("soc")))

  # putting data into list of tibbles...one element per SOC --------------------
  lst_data_complete <-
    data_complete %>%
    dplyr::group_split() %>%
    rlang::set_names(dplyr::group_keys(data_complete) %>% purrr::pluck(1))

  # tablulate SOC --------------------------------------------------------------
  if (!is.null(soc)) {
    lst_tbl_soc <-
      purrr::imap(
        lst_data_complete,
        function(df_soc, soc) {
          # keep observation that will be tabulated
          df_soc <- filter(df_soc, .data$..soc..)

          fn_tbl_soc <-
            purrr::partial(fn_tbl,
                           variable = "..soc..",
                           label = soc,
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
      lst_data_complete,
      function(df_ae) {
        # keep observation that will be tabulated
        df_ae <- filter(df_ae, .data$..ae..)

        fn_tbl_ae <-
          purrr::partial(fn_tbl,
                         variable = "ae",
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
    purrr::list_modify(inputs = tbl_adverse_event_inputs,
                       data_complete = dplyr::ungroup(data_complete)) %>%
    # add class
    structure(class = c("tbl_adverse_event", "gtsummary"))
}


# define `tbl_summary()` function to tabulate SOC/AE
fn_tbl <- function(data, variable, label = NULL, statistic, header,
                   remove_header_row, zero_symbol = NULL) {
  tbl <-
    gtsummary::tbl_summary(
      data = data,
      by = "by",
      percent = "row",
      label = switch(!is.null(label), everything() ~ label),
      statistic = everything() ~ statistic,
      include = all_of(variable)
    ) %>%
    gtsummary::modify_header(gtsummary::all_stat_cols() ~ header)

  # hide the column for unobserved data
  column_to_hide <-
    tbl$df_by %>%
    filter(.data$by %in% "NOT OBSERVED") %>%
    purrr::pluck("by_col")
  if (!is.null(column_to_hide)) {
    tbl <- gtsummary::modify_column_hide(tbl, columns = column_to_hide)
  }

  # remove the header row
  if (isTRUE(remove_header_row)) {
    tbl <- gtsummary::remove_row_type(tbl)
  }

  if (!is.null(zero_symbol)) {
    # TODO: Add code to make zero count cells NA,
    # then use `modify_table_styling()` to make these columns the zero_symbol.
    # Look in `x$meta_data$df_stats` for the counts
  }

  tbl
}
