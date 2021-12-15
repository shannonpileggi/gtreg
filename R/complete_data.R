#' Create a complete and expanded data frame for tabulating adverse events
#'
#' @param data Data frame
#' @param id Variable name of the patient ID
#' @param soc Variable name of the system organ class column
#' @param ae Variable name of the adverse event column
#' @param by Variable to split results by, e.g. report AEs by grade or attribution
#' @param strata Variable to stratify results by, e.g. report AEs summaries
#' by treatment group
#' @param id_df Optional data frame of complete id values and strata to achieve correct
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
                          id_df, by_values) {

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

  # list to rename variables----------------------------------------------------
  lst_name_recode <- list(id = id, strata = strata,
                          ae = ae, soc = soc, by = by)

  # some default factor levels -------------------------------------------------
  initial_missing <- "Not entered"
  initial_dummy   <- "dummy"

  # initial data renaming and trimming -----------------------------------------
  data_initial <- data
    dplyr::rename(!!!lst_name_recode) %>%
    dplyr::select(all_of(names(lst_name_recode)))

  # if by values are not supplied retrieve them from the submitted data --------
  if (is.null(by_values)) { by_values <- get_unique(data_initial, by) }

  # combine dummy and mising with by values ------------------------------------
  by_values <- c(initial_dummy, initial_missing, by_values)


  # retrieve unique values for ae and soc --------------------------------------
  soc_values <- get_unique(data_initial, soc)
  ae_values  <- get_unique(data_initial, ae)

  # if data frame of ids is supplied -------------------------------------------
  if (!is.null(id_df)) {
    id_df <- id_df %>%
      dplyr::rename(!!!lst_name_recode[1:2])
  }

  # if data frame of ids is not supplied  --------------------------------------
  if (is.null(id_df)) {
    id_df <- data_initial
    }

  # retrieve unique strata & id combinations -----------------------------------
  id_df <- id_df %>%
      dplyr::select(id, strata) %>%
      dplyr::distinct(id, strata) %>%
      dplyr::arrange(strata, id)

  # fully expanded data frame --------------------------------------------------
  data_full <- id_df %>%
    tidyr::expand_grid(
      soc = soc_values,
      ae = ae_values
      )

  data_complete <- data_initial %>%
    dplyr::mutate(
      in_original = TRUE,
      # convert to character initially -----------------------------------------
      by = as.character(by),
      # replace any missing values in by ---------------------------------------
      by = tidyr::replace_na(by, initial_missing)
    ) %>%
    # join with full data frame -----------------------------------------------
    dplyr::right_join(data_full, by = c("strata", "id", "soc", "ae")) %>%
    dplyr::mutate(
      # if data comes from expanded full data and not original data, replace
      # with  dummy value
      by = dplyr::case_when(
        in_original ~ by,
        TRUE ~ initial_dummy
      ),
      # convert by variable to factor ------------------------------------------
      by = forcats::as_factor(by),
      by = forcats::fct_expand(by, by_values),
      by = forcats::fct_relevel(by, by_values)
    ) %>%
    dplyr::arrange(.data$id, .data$soc,  .data$by) %>%
    # keep highest `grade` value per patient, e.g. highest grade
    dplyr::group_by(.data$id, .data$soc) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()

  # will return inputs ---------------------------------------------------------
  # keep all of these or just some?
  tbl_ae_inputs <- as.list(environment())

  out <- tibble::lst(data_complete, tbl_ae_inputs)

  return(out)

}

