#' Tabulate Raw AE Counts
#'
#' Create a table counting all AEs.
#'
#' \code{tbl_ae_count} counts all AEs (whereas \code{\link{tbl_ae}}
#' counts by maximum grade). Thus, \code{tbl_ae_count} does
#' not provide percentages as multiple AEs can be counted per subject.
#'
#' @inheritParams tbl_ae
#'
#' @return a 'tbl_ae_count' object
#' @export
#' @seealso \code{\link{tbl_ae}}
#' @examples
#' \donttest{
#' # Example 1 -----------------------------------------------------------------
#' tbl_ae_count_ex1 <-
#'   tbl_ae_count(
#'     data = df_adverse_events,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     strata = trt,
#'     by = grade
#'   ) %>%
#'   modify_header(all_ae_cols() ~ "**Grade {by}**")
#'   }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_ae_count_ex1.png}{options: width=90\%}}
#'
#' @usage
#' tbl_ae_count(
#'   data,
#'   ae,
#'   soc = NULL,
#'   by = NULL,
#'   strata = NULL,
#'   by_values = NULL,
#'   digits = NULL,
#'   sort = NULL,
#'   zero_symbol = "\U2014",
#'   missing_location = c("first", "last", "hide")
#' )

tbl_ae_count <- function(data,
                         ae,
                         soc = NULL,
                         by = NULL,
                         strata = NULL,
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

  if (is.null(ae)) {
    stop("Argument `ae=` must be specified.", call. = FALSE)
  }

  # will return inputs ---------------------------------------------------------
  tbl_ae_count_inputs <- as.list(environment())

  # adding default statistic ---------------------------------------------------
  statistic <- "{n}"
  missing_text <- "Unknown"

  # obtain the complete data ---------------------------------------------------
  data_complete <-
    data %>%
    dplyr::mutate(.......gtreg_id_for_tbl_ae_count....... = 1) %>%
    .complete_ae_data(id = ".......gtreg_id_for_tbl_ae_count.......",
                      ae = ae, soc = soc, by = by,
                      strata = strata, id_df = NULL, by_values = by_values,
                      missing_location = missing_location) %>%
    dplyr::mutate(across(any_of(c("..soc..", "..ae..")), ~TRUE))

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
    purrr::list_modify(inputs = tbl_ae_count_inputs) %>%
    # add class
    structure(class = c("tbl_ae_count", "gtsummary")) %>%
    # add default spanning headers
    .when(
      !is.null(strata) ~
        modify_spanning_header(
          ., gtsummary::all_stat_cols() ~ "**{strata}**"),
      TRUE ~ .
    )
}
