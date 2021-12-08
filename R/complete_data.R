#' Create a complete and expanded data frame for tabulating adverse events
#'
#' @param data Data frame
#' @param id Variable name of the patient ID
#' @param soc Variable name of the system organ class column
#' @param ae Variable name of the adverse event column
#' @param by Variable to split results by, e.g. report AEs by grade or attribution
#' @param strata Variable to stratify results by, e.g. report AEs summaries
#' by treatment group
#' @param id_values Optional vector of complete id values to achieve correct
#' base n for the situation in which not all subjects experience adverse events
#' @param by_values Optional vector of complete by values, listed in desired order,
#' to achieve correct table structure for the situation in which an adverse
#' event of a certain grade is not observed for a given soc
#'
#'
#' @export
#' @examples
#' df_aes %>%
#'   tbl_adverse_events(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade,
#'     strata = trt
#'   ) %>%
#'   as_kable() # UPDATE THIS WITH PROPER gt image at some point..

complete_data <- function(data, id, ae, soc, by, strata,
                          id_values, by_values) {

  # evaluate bare selectors/check inputs ---------------------------------------
  stopifnot(inherits(data, "data.frame"))

  # Does this need to be repeated here and in the main tabling function?? ----
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

  # create data frame where every AE is observed -------------------------------
  lst_name_recode <- list(id = id, ae = ae, soc = soc,
                          by = by, strata = strata)

  # some default factor levels -------------------------------------------------
  initial_missing <- "Not entered"
  initial_dummy   <- "dummy"

  # initial data renaming and trimming -----------------------------------------
  data_initial <-
    dplyr::rename(data, !!!lst_name_recode) %>%
    dplyr::select(all_of(names(lst_name_recode)))

  # if by values are not supplied ----------------------------------------------
  if (is.null(by_values)) {
    by_values <- data_initial %>%
      dplyr::distinct(by) %>%
      dplyr::pull() %>%
      c(initial_dummy, initial_missing, .)
  }

  # if by values are supplied --------------------------------------------------
  if (!is.null(by_values)) {
    by_values <- c(initial_dummy, initial_missing, by_values)
  }

  browser()

  data_complete <- data_initial %>%
    mutate(
      # prep by variable -------------------------------------------------------
      # convert by variable to factor
      by = forcats::as_factor(by),
      # expand values of factor
      by = forcats::fct_expand(by, by_values),
      # explicit NA
      by = forcats::fct_explicit_na(by, initial_missing),
      # relevel values of factor
      by = forcats::fct_relevel(by, by_values),
      # prep id variable -------------------------------------------------------
      # convert id variable to factor
      id = forcats::as_factor(id),
      # expand values of factor
      id = forcats::fct_expand(id, id_values),
      # indicator for if observation present in original data vs
      # added during expansion
      in_original = TRUE
    ) %>%
    tidyr::complete(
      tidyr::nesting(id, strata), tidyr::nesting(soc, ae),
      fill = list(by = "dummy", in_original = FALSE)
    ) %>%
    dplyr::arrange(.data$id, .data$soc,  .data$by) %>%
    # keep highest `grade` value per patient, e.g. highest grade
    dplyr::group_by(.data$id, .data$soc) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()

  # will return inputs ---------------------------------------------------------
  tbl_ae_inputs <- as.list(environment())

  out <- tibble::lst(data_complete, tbl_ae_inputs)

  return(out)

}
