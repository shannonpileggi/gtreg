#' Create a complete and expanded data frame for tabulating adverse events
#'
#' Returns a data frame that has an observation for each patient in the study,
#' with combinations for each ID, SOC, and AE. The returned data frame includes
#' new logical columns `"..ae.."` and  `"..soc.."` indicating whether that
#' row should be included when tabulating the AE table. When multiple
#' AEs of the same type are observed, the AE with the largest `by=` value
#' is the observation to be used in the tabulation.
#'
#' @param data Data frame
#' @param id String variable name of the patient ID
#' @param ae String variable name of the adverse event column
#' @param soc Optional string variable name of the system organ class column
#' @param by Optional string variable to split results by, e.g. report AEs by grade or attribution
#' @param strata Optional string variable to stratify results by,
#' e.g. report AEs summaries by treatment group
#' @param id_df Optional data frame of complete id values and strata to achieve correct
#' base n for the situation in which not all subjects experience adverse events
#' @param by_values Optional vector of complete by values, listed in desired order,
#' to achieve correct table structure for the situation in which an adverse
#' event of a certain grade is not observed for a given soc
#' @param missing_text String that will be shown for missing levels of `by=`,
#' Default is `"Unknown"`
#' @inheritParams tbl_ae
#'
#' @export
#' @return a tibble
#'
#' @examples
#' df_adverse_events %>%
#'   .complete_ae_data(
#'     id = "patient_id",
#'     ae = "adverse_event",
#'     soc = "system_organ_class",
#'     by = "grade",
#'     strata = "trt"
#'   )

.complete_ae_data <- function(data, id, ae, soc = NULL, by = NULL, strata = NULL,
                          id_df = NULL, by_values = NULL,
                          missing_text = "Unknown", missing_location = "first") {

  # check inputs ---------------------------------------------------------------
  if (is.null(by) && !is.null(by_values))
    stop("Cannot specify `by_values=` without also specifying `by=`.", call. = FALSE)
  if (!is.null(by) && inherits(data[[by]], "factor") && !is.null(by_values))
    stop("Cannot specify `by_values=` when `by=` is a factor as it is expected
         that factor levels contain all possible `by_values`.", call. = FALSE)
  if (!rlang::is_string(missing_text)) {
    stop("The `missing_text=` argument must be a string.", call. = FALSE)
  }
  if (!is.null(by_values) && !is.character(by_values)) {
    stop("The `by_values=` argument must be a character vector.", call. = FALSE)
  }

  # check ID between data and id_df
  # Check the id and strata column names match `data=`
  if (!is.null(id_df) && !(id %in% names(id_df))) {
    stop("The `id=` column must be present in `id_df=`.", call. = FALSE)
  }
  if (!is.null(id_df) && !is.null(strata) && !(strata %in% names(id_df))) {
    stop("The `strata=` column must be present in `id_df=`.", call. = FALSE)
  }

  # Check the id and strata type matches between `data=` and `id_df`
  if (!is.null(id_df) && !identical(class(data[[id]]), class(id_df[[id]]))) {
    stop("The class of the `id=` column must match in both `data=` and `id_df=`.", call. = FALSE)
  }
  if (!is.null(id_df) && !is.null(strata) && !identical(class(data[[strata]]), class(id_df[[strata]]))) {
    stop("The class of the `strata=` column must match in both `data=` and `id_df=`.", call. = FALSE)
  }

  # Check the id and strata columns are not missing
  if (any(is.na(data[[id]])) ||
      ( !is.null(strata) && any(is.na(data[[strata]]))) ) {
    stop("Columns `id=` and `strata=` cannot be missing in `data=`", call. = FALSE)
  }
  # Check the id and strata columns are not missing
  if (!is.null(id_df) &&
      (any(is.na(id_df[[id]])) ||
      ( !is.null(strata) && any(is.na(id_df[[strata]])))) ) {
    stop("Columns `id=` and `strata=` cannot be missing in `id_df=`", call. = FALSE)
  }

  # 3. Check all ID/strata combos appear in `data=`
  if (!is.null(id_df) && any(duplicated(id_df[c(id, strata)]))) {
    stop("Disallowed duplicate `id=`/`strata=` combinations found in `id_df=`.", call. = FALSE)
  }
  if (!is.null(id_df) &&
      nrow(dplyr::anti_join(dplyr::distinct(data[c(id, strata)]),
                            id_df[c(id, strata)],
                            by = c(id, strata))) > 0) {
    stop("There are `id=`/`strata=` combinations in `data=` not found in `id_df=`.", call. = FALSE)
  }

  # some default factor levels -------------------------------------------------
  initial_missing <- missing_text
  initial_dummy   <- "NOT OBSERVED"

  # list to rename variables----------------------------------------------------
  lst_name_recode <-
    list(id = id, strata = strata, ae = ae, soc = soc, by = by) %>%
    purrr::compact()

  # initial data renaming and trimming -----------------------------------------
  data <- data %>% dplyr::select(!!!lst_name_recode)

  # configuring the `by=` variable ---------------------------------------------
  data <-
    .prepare_by_levels(
      data = data,
      by = by,
      by_values = by_values,
      initial_missing = initial_missing,
      initial_dummy = initial_dummy
    )

  # if data frame of ids is supplied, add IDs obs to data ----------------------
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
      arrange(across(any_of(c("id", "strata", "soc", "by")))) %>%
      group_by(across(any_of(c("id", "strata", "soc")))) %>%
      mutate(
        ..soc.. = dplyr::row_number() == dplyr::n(),
        soc = factor(.data$soc)
      ) %>%
      ungroup()
  }
  data_full <-
    data_full %>%
    arrange(across(any_of(c("id", "strata", "soc", "ae", "by")))) %>%
    group_by(across(any_of(c("id", "strata", "soc", "ae")))) %>%
    mutate(
      ..ae.. = dplyr::row_number() == dplyr::n()
    ) %>%
    ungroup()

  # moving missing by level to end if requested
  if (missing_location %in% "last" &&
      initial_missing %in% levels(data_full[["by"]])) {
    data_full[["by"]] <-
      forcats::fct_relevel(data_full[["by"]], initial_missing, after = Inf)
  }

  # move unobserved level to the end of the `by=` level
  if (initial_dummy %in% levels(data_full[["by"]])) {
    data_full[["by"]] <-
      forcats::fct_relevel(data_full[["by"]], initial_dummy, after = Inf)
  }

  return(data_full)
}

.prepare_by_levels <- function(data, by, by_values, initial_missing, initial_dummy) {
  if (!is.null(by) && initial_dummy %in% data[["by"]]) {
    stringr::str_glue("Level '{initial_dummy}' cannot ",
                      "appear in the levels of the `by=` variable.") %>%
      stop(call. = FALSE)
  }
  if (!is.null(by) && initial_missing %in% data[["by"]] && any(is.na(data[["by"]]))) {
    stringr::str_glue("Level '{initial_missing}' cannot appear in the levels ",
                      "of the `by=` variable when missing data present.") %>%
      stop(call. = FALSE)
  }

  if (!is.null(by_values) && initial_dummy %in% by_values) {
    stringr::str_glue("Level '{initial_dummy}' cannot ",
                      "appear in the levels of the `by_values=` argument") %>%
      stop(call. = FALSE)
  }
  if (!is.null(by_values) && initial_missing %in% by_values && any(is.na(data[["by"]]))) {
    stringr::str_glue("Level '{initial_missing}' cannot appear in the levels ",
                      "of the `by_values=` argument when missing data present.") %>%
      stop(call. = FALSE)
  }

  if (is.null(by)) {
    data$by <- factor("Overall")
  }

  if (!inherits(data$by, "factor")) {
    data$by <- factor(data$by)
  }

  if (!is.null(by_values)) {
    if (!rlang::is_empty(setdiff(levels(data$by), by_values))) {
      stop("All levels of `by=` variable must appear in  `by_values=`",
           call. = FALSE)
    }

    # expanding by factor variable
    data$by <- rlang::inject(forcats::fct_expand(data$by, !!!as.list(by_values)))

    # re-leveling by variable by_values (to order the levels in the output table)
    data$by <- rlang::inject(forcats::fct_relevel(data$by, !!!as.list(by_values)))
  }

  # adding missing level, as needed
  if (any(is.na(data$by))) {
    data$by <- forcats::fct_explicit_na(data$by, na_level = initial_missing)
  }

  data
}
