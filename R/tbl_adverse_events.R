#' Tabulate Adverse Events
#'
#' @param data Data frame
#' @param id Variable name of the patient ID
#' @param soc Variable name of the system organ class column
#' @param adverse_event Variable name of the adverse event column
#' @param grade Variable to split results by, e.g. report AEs by grade
#' @param strata Variable to stratify results by, e.g. report AEs summaries
#' by treatment group
#'
#' @export
#' @examples
#' df_adverse_events %>%
#'   tbl_adverse_events(
#'     id = patient_id,
#'     adverse_event = adverse_event,
#'     soc = system_organ_class,
#'     grade = grade,
#'     strata = trt
#'   ) %>%
#'   as_kable() # UPDATE THIS WITH PROPER gt image at some point..

tbl_adverse_events <- function(data, id, adverse_event, soc,
                               grade, strata) {
  # evaluate bare selectors/check inputs ---------------------------------------
  stopifnot(inherits(data, "data.frame"))
  id <-
    .select_to_varnames({{ id }}, data = data,
                        arg_name = "id", select_single = TRUE)
  adverse_event <-
    .select_to_varnames({{ adverse_event }}, data = data,
                        arg_name = "adverse_event", select_single = TRUE)
  soc <-
    .select_to_varnames({{ soc }}, data = data,
                        arg_name = "soc", select_single = TRUE)
  grade <-
    .select_to_varnames({{ grade }}, data = data,
                        arg_name = "grade", select_single = TRUE)
  strata <-
    .select_to_varnames({{ strata }}, data = data,
                        arg_name = "strata", select_single = TRUE)

  # create data frame where every AE is observed -------------------------------
  lst_name_recode <- list(id = id, adverse_event = adverse_event, soc = soc,
                          grade = grade, strata = strata)
  data_complete <-
    dplyr::rename(data, !!!lst_name_recode) %>%
    dplyr::select(all_of(names(lst_name_recode))) %>%
    tidyr::complete(
      tidyr::nesting(id, strata), tidyr::nesting(soc, adverse_event),
      fill = list(grade = 0)
    ) %>%
    dplyr::mutate(grade = factor(grade, levels = 0:5))

  # tabulate AEs ---------------------------------------------------------------
  df_results <-
    data_complete %>%
    tidyr::nest(data = -.data$soc) %>%
    mutate(
      # create a single line summary for each SOC
      tbl_soc =
        purrr::map2(
          .data$soc, .data$data,
          function(soc, df_soc) {
            dplyr::arrange(df_soc, .data$id, .data$grade) %>%
              # keep highest `grade` value per patient, e.g. highest grade
              dplyr::group_by(.data$id) %>%
              dplyr::slice_tail(n = 1) %>%
              dplyr::ungroup() %>%
              gtsummary::tbl_strata(
                strata = strata,
                ~ .x %>%
                  dplyr::select(.data$grade) %>%
                  dplyr::mutate(..all_true.. = TRUE) %>%
                  gtsummary::tbl_summary(
                    by = .data$grade,
                    percent = "row",
                    label = list(..all_true.. = soc)
                  ) %>%
                  gtsummary::modify_header(
                    gtsummary::all_stat_cols() ~ "**Grade {level}**") %>%
                  gtsummary::bold_labels() %>%
                  # hide Grade 0 col
                  gtsummary::modify_column_hide(all_of("stat_1"))
              )
          }
        ),
      # summarize each AE within SOC
      tbl_ae =
        purrr::map(
          .data$data,
          function(df_soc) {
            dplyr::arrange(df_soc, .data$id, .data$adverse_event, .data$grade) %>%
              # keep highest `grade` value per patient per AE, e.g. highest grade
              dplyr::group_by(.data$id, .data$adverse_event) %>%
              dplyr::slice_tail(n = 1) %>%
              dplyr::ungroup() %>%
              gtsummary::tbl_strata(
                strata = strata,
                ~ .x %>%
                  dplyr::select(.data$grade, .data$adverse_event) %>%
                  gtsummary::tbl_summary(
                    by = .data$grade,
                    percent = "row"
                  ) %>%
                  gtsummary::modify_header(
                    gtsummary::all_stat_cols() ~ "**Grade {level}**") %>%
                  gtsummary::remove_row_type(type = "header") %>%
                  # hide Grade 0 col
                  gtsummary::modify_column_hide(all_of("stat_1"))
              )
          }
        ),
      # stack SOC and AE tables
      tbl = purrr::map2(.data$tbl_soc, .data$tbl_ae, ~gtsummary::tbl_stack(list(.x, .y)))
    )

  # stack all tbls and return --------------------------------------------------
  df_results$tbl %>%
    gtsummary::tbl_stack() %>%
    gtsummary::modify_table_body(
      ~ .x %>%
        dplyr::mutate(dplyr::across(gtsummary::all_stat_cols(),
                                    ~ifelse(. == "0 (0%)", "\U2014", .)))
    )
}
