#' Export AE table to .xlsx
#'
#' Quickly export a 'tbl_ae_count' object to an Excel file with
#' prespecified formatting that approximates the look of the corresponding
#' .html.
#'
#' @param ae_tbl A 'tbl_ae_count' object
#' @param file Export destination and name for the .xlsx file
#' @param bold_rows An optional numeric vector of rows to bold
#' @param header_rows An optional numeric vector of header row indices
#'
#' @return Exports a .xlsx file
#' @export
#'
#' @examples
#' df_adverse_events %>%
#' tbl_ae(
#'   id = patient_id,
#'   ae = adverse_event,
#'   soc = system_organ_class,
#'   by = grade
#' ) %>%
#'   ae_as_xlsx("ae-table.xlsx", bold_rows = 1:2, header_rows = 1:2)
ae_as_xlsx <- function(ae_tbl, file, bold_rows = NULL, header_rows = NULL) {
  ae_formatted <- ae_tbl %>%
    gtreg::format_ae_hux() %>%
    gtsummary::as_hux_table() %>%
    huxtable::set_bold(which(substr(.$label, 1, 1) != " "), 1) %>%
    huxtable::set_bottom_border(huxtable::final(1), huxtable::everywhere)

  if(!is.null(bold_rows)) {
    ae_formatted <- ae_formatted %>%
      huxtable::set_bold(bold_rows, huxtable::everywhere)
  }

  if(!is.null(header_rows)) {
    ae_formatted <- ae_formatted %>%
      huxtable::set_header_rows(header_rows, TRUE)
  }

  ae_formatted %>%
    huxtable::quick_xlsx(
      .,
      file = file,
      open = FALSE
    )
}

#' @param .data A 'tbl_ae_count' object
#'
#' @return A 'tbl_ae_count' object with indent spacing
#' @keywords internal
#' @noRd
#' @export
format_ae_hux <- function(.data) {
  .data$table_body <- .data$table_body %>%
    dplyr::mutate(
      label = dplyr::case_when(
        .data$row_type != "label" ~ paste0("     ", .data$label),
        TRUE ~ .data$label
      )
    )

  .data
}
