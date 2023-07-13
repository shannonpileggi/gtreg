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
#' base n for the situation in which not all subjects experience adverse events. See
#' \code{\link{df_patient_characteristics}} for an example `id_df` that pairs with
#' \code{\link{df_adverse_events}}.
#' @param by_values Optional vector of complete by values, listed in desired order,
#' to achieve correct table structure for the situation in which an adverse
#' event of a certain grade is not observed for a given soc
#' @param missing_location location where the column summarizing values with
#' missing levels `by=` will be located in the final table. Must be
#' one of `c("first", "last", "hide)`. Default is `"first"`
#' @param statistic String indicating the statistics that will be reported.
#' The default is `"{n} ({p})"`
#' @param zero_symbol String used to represent cells with zero counts. Default
#' is the em-dash (`"\U2014"`). Using `zero_symbol = NULL` will print the
#' zero count statistics, e.g. `"0 (0)"`
#' @param sort Controls order of AEs and SOCs in output table.
#' The default is `NULL`, where AEs and SOCs are sorted alphanumerically
#' (and factors sorted according to their factor level).
#' Use `sort = "ae"` to sort AEs in decreasing frequency order, `sort = "soc"`
#' to sort SOCs in decreasing order, and `sort = c("ae", "soc")` to sort both.
#' AEs are sorted within SOC.
#' @param digits Specifies the number of decimal places to round the summary statistics.
#' By default integers are shown to zero decimal places, and percentages are
#' formatted with `style_percent()`. If you would like to modify either
#' of these, pass a vector of integers indicating the number of decimal
#' places to round the statistics. For example, if the statistic being
#' calculated is `"{n} ({p}%)"` and you want the percent rounded to
#' 2 decimal places use `digits = c(0, 2)`.
#' User may also pass a styling function: `digits = style_sigfig`
#'
#' @return a 'tbl_ae' object
#' @export
#'
#' @examples
#' \donttest{
#' # Example 1 -----------------------------------------------------------------
#' tbl_ae_ex1 <-
#'   df_adverse_events %>%
#'   tbl_ae(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade,
#'     strata = trt
#'   ) %>%
#'   modify_header(all_ae_cols() ~ "**Grade {by}**")
#'
#' # Example 2 -----------------------------------------------------------------
#' tbl_ae_ex2 <-
#'   df_adverse_events %>%
#'   tbl_ae(
#'     id = patient_id,
#'     ae = adverse_event,
#'     by = grade
#'   ) %>%
#'   modify_header(all_ae_cols() ~ "**Grade {by}**")
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_ae_ex1.png}{options: width=95\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_ae_ex2.png}{options: width=65\%}}
#'
#' @usage
#' tbl_ae(
#'   data,
#'   id,
#'   ae,
#'   soc = NULL,
#'   by = NULL,
#'   strata = NULL,
#'   id_df = NULL,
#'   statistic = "{n} ({p})",
#'   by_values = NULL,
#'   digits = NULL,
#'   sort = NULL,
#'   zero_symbol = "\U2014",
#'   missing_location = c("first", "last", "hide")
#' )

tbl_ae <- function(data,
                   id,
                   ae,
                   soc = NULL,
                   by = NULL,
                   strata = NULL,
                   id_df = NULL,
                   statistic = "{n} ({p})",
                   by_values = NULL,
                   digits = NULL,
                   sort = NULL,
                   zero_symbol = "\U2014",
                   missing_location = c("first", "last", "hide")) {
  # evaluate bare selectors/check inputs ---------------------------------------
  if(!inherits(data, "data.frame")) {
    stop("`data=` argument must be a tibble or data frame.", call. = FALSE)
  }
  if (!is.null(sort)) {
    sort <- match.arg(sort, choices = c("ae", "soc"), several.ok = TRUE)
  }
  missing_location <- match.arg(missing_location)

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
  if (is.null(id) || is.null(ae)) {
    stop("Arguments `id=` and `ae=` must be specified.", call. = FALSE)
  }

  # will return inputs ---------------------------------------------------------
  tbl_ae_inputs <- as.list(environment())

  # obtain the complete data ---------------------------------------------------
  data_complete <-
    .complete_ae_data(data, id = id, ae = ae, soc = soc, by = by,
                      strata = strata, id_df = id_df, by_values = by_values,
                      missing_location = missing_location)

  # tabulate SOC ---------------------------------------------------------------
  if (!is.null(soc)) {
    tbl_soc <-
      .construct_summary_table(data = data_complete %>% filter(.data$..soc..),
                               variable = "soc",
                               digits = digits,
                               statistic = statistic,
                               sort = sort,
                               zero_symbol = zero_symbol,
                               missing_location = missing_location)
  }

  # tabulate AE ----------------------------------------------------------------
  tbl_ae <-
    .construct_summary_table(data = data_complete %>% filter(.data$..ae..),
                             variable = "ae",
                             digits = digits,
                             statistic = statistic,
                             sort = sort,
                             zero_symbol = zero_symbol,
                             missing_location = missing_location)

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
    purrr::list_modify(inputs = tbl_ae_inputs) %>%
    # add class
    structure(class = c("tbl_ae", "gtsummary")) %>%
    # add default spanning headers
    .when(
      !is.null(strata) ~
        modify_spanning_header(., gtsummary::all_stat_cols() ~ "**{strata}**, N = {n}"),
      TRUE ~ modify_spanning_header(., gtsummary::all_stat_cols() ~ "**N = {N}**")
    )
}
