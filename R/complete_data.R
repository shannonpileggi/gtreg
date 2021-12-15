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
#' @examples
#' df_aes %>%
#'   complete_data(
#'     id = "patient_id",
#'     ae = "adverse_event",
#'     soc = "system_organ_class",
#'     by = "grade",
#'     strata = "trt"
#'   )

complete_data <- function(data, id, ae, soc = NULL, by = NULL, strata = NULL,
                          id_df = NULL, by_values = NULL,
                          missing_text = "Unknown") {

  # evaluate bare selectors/check inputs ---------------------------------------
  stopifnot(inherits(data, "data.frame"))

  # list to rename variables----------------------------------------------------
  lst_name_recode <-
    list(id = id, strata = strata, ae = ae, soc = soc, by = by) %>%
    purrr::compact()

  # some default factor levels -------------------------------------------------
  initial_missing <- missing_text
  initial_dummy   <- "NOT OBSERVED"

  # initial data renaming and trimming -----------------------------------------
  data <- data %>% dplyr::select(!!!lst_name_recode)


  # configuring the `by=` variable ---------------------------------------------
  if (is.null(by)) {
    data$by <- factor("TRUE")
  }

  if (!inherits(data[[by]], "factor")) {
    data$by <- factor(data$by)
  }

  if (any(is.na(data$by))) {
    data$by <- forcats::fct_explicit_na(data$by, na_lavel =initial_missing )
  }

  # if by values are not supplied retrieve them from the submitted data --------
  # if (is.null(by_values)) { by_values <- get_unique(data_initial, by) }
  #
  # # combine dummy and missing with by values -----------------------------------
  # by_values <- c(initial_dummy, initial_missing, by_values)

  # retrieve unique values for ae and soc --------------------------------------
  # soc_values <- get_unique(data_initial, soc)
  # ae_values  <- get_unique(data_initial, ae)

  # if data frame of ids is supplied -------------------------------------------
  if (!is.null(id_df)) {
    data <-
      id_df %>%
      select(!!!lst_name_recode[c("id", "strata")]) %>%
      dplyr::full_join(
        data,
        by = intersect(c("id", "strata"), names(data))
      )
  }

  # fully expanded data frame --------------------------------------------------
  data_full <-
    data %>%
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(intersect(c("id", "strata"), names(data)))),
      tidyr::nesting(!!!rlang::syms(intersect(c("soc", "ae"), names(data))))
    ) %>%
    tidyr::drop_na(!!!rlang::syms(intersect(c("soc", "ae"), names(data))))

  # replace unobserved AEs with an explicit level ------------------------------
  data_full$by <- forcats::fct_explicit_na(data_full$by, initial_dummy)
  # re-level to put unobserved and missing in front
  if (any(c(initial_dummy, initial_missing) %in% levels(data_full$by))) {
    data_full$by <-
      rlang::inject(
        forcats::fct_relevel(
          data_full$by,
          !!!as.list(intersect(c(initial_dummy, initial_missing), levels(data_full$by)))
        )
      )
  }

  # identifying rows that will be used in tabulation ---------------------------
  if (!is.null(soc)) {
    data_full <-
      data_full %>%
      dplyr::arrange(dplyr::across(any_of(c("id", "strata", "soc", "by")))) %>%
      dplyr::group_by(dplyr::across(any_of(c("id", "strata", "soc")))) %>%
      dplyr::mutate(
        ..soc.. = dplyr::row_number() == dplyr::n()
      ) %>%
      dplyr::ungroup()
  }
  data_full <-
    data_full %>%
    dplyr::arrange(dplyr::across(any_of(c("id", "strata", "soc", "ae", "by")))) %>%
    dplyr::group_by(dplyr::across(any_of(c("id", "strata", "soc", "ae")))) %>%
    dplyr::mutate(
      ..ae.. = dplyr::row_number() == dplyr::n()
    ) %>%
    dplyr::ungroup()


  return(data_full)
}

