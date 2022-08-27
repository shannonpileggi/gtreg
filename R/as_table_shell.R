#' Create Table Shell
#'
#' Convert a 'gtsummary' table into a table shell by replacing the statistics
#' reported in the table with placeholders.
#'
#' @param x a 'gtsummary' object
#' @param columns columns to replace digits with placeholder
#' @param pattern regex pattern of text to replace. Default is `"[[:digit:]]+"`
#' @param replacement string placeholder. Default is `"x"`
#' @param replace_headers logical indicating whether to replace digits in column headers.
#' Default is FALSE
#' @param replace_spanning_headers logical indicating whether to replace digits
#' in column spanning headers. Default is TRUE
#'
#' @return a 'gtsummary' table
#' @export
#'
#' @examples
#' tbl <-
#'   df_adverse_events %>%
#'   tbl_ae(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade,
#'     zero_symbol = NULL
#'   ) %>%
#'   modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
#'   as_table_shell()
as_table_shell <- function(x,
                           columns = c(gtsummary::all_stat_cols(),
                                       dplyr::any_of(c("p.value", "q.value", "estimate",
                                                       "ci", "conf.low", "conf.high", "statistic"))),
                           pattern = "[[:digit:]]+",
                           replacement = "xx",
                           replace_headers = FALSE,
                           replace_spanning_headers = TRUE) {
  if (!inherits(x, "gtsummary")) {
    cli::cli_abort("Arugment {.code x} must be class {.cls gtsummary}")
  }


  # replace all digits in the table body with 'x'
  to_tibble_and_back_again <-
    x %>%
    gtsummary::modify_column_unhide(everything()) %>%
    as_tibble(col_labels = FALSE, fmt_missing = FALSE) %>%
    dplyr::mutate(
      dplyr::across({{ columns }}, ~gsub(pattern, replacement, x = .))
    ) %>%
    tbl_listing()

  # use all the same styling as the original table, but remove formatting functions
  to_tibble_and_back_again$table_styling <- x$table_styling
  to_tibble_and_back_again$table_styling$fmt_fun <-
    dplyr::filter(to_tibble_and_back_again$table_styling$fmt_fun, FALSE)

  # replace numbers in spanning header
  if (isTRUE(replace_headers)) {
    to_tibble_and_back_again$table_styling$header <-
      to_tibble_and_back_again$table_styling$header %>%
      dplyr::mutate(
        dplyr::across(dplyr::any_of("label"), ~gsub(pattern, replacement, x = .))
      )
  }

  # replace numbers in spanning header
  if (isTRUE(replace_spanning_headers)) {
    to_tibble_and_back_again$table_styling$header <-
      to_tibble_and_back_again$table_styling$header %>%
      dplyr::mutate(
        dplyr::across(dplyr::any_of("spanning_header"), ~gsub(pattern, replacement, x = .))
      )
  }


  to_tibble_and_back_again %>%
    gtsummary::tbl_butcher()
}
