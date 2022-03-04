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
#'   tbl_ae(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade
#'   ) %>%
#'   ae_as_xlsx(tempfile(fileext = ".xlsx"), bold_rows = 1:2, header_rows = 1:2)
ae_as_xlsx <- function(ae_tbl, file, bold_rows = NULL, header_rows = NULL) {
  ae_formatted <- ae_tbl %>%
    # manually indent cells
    format_ae_hux() %>%
    # convert to huxtable without applying indentation styling
    gtsummary::as_hux_table(include = -dplyr::any_of(c("set_left_padding", "set_left_padding2"))) %>%
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
format_ae_hux <- function(.data) {
  # extract the indentation instructions from table_styling
  df_text_format <-
    gtsummary::.table_styling_expr_to_row_number(.data) %>%
    purrr::pluck("table_styling", "text_format") %>%
    dplyr::filter(.data$format_type %in% c("indent", "indent2"))

  # create expressions to add indentations to `x$table_body`
  indent_exprs <-
    purrr::pmap(
      list(df_text_format$column, df_text_format$row_numbers, df_text_format$format_type),
      function(column, row_numbers, format_type) {
        indent_spaces <- ifelse(format_type %in% "indent", "     ", "          ")
        rlang::expr(
          dplyr::mutate(
            dplyr::across(dplyr::all_of(!!column),
                          ~ifelse(dplyr::row_number() %in% !!row_numbers,
                                  paste0(!!indent_spaces, .), .)))
        )
      }
    )

  # indent the needed cells and return gtsummary table
  .data %>%
    gtsummary::modify_table_body(
      function(x) purrr::reduce(indent_exprs, ~rlang::inject(!!.x %>% !!.y), .init = x)
    )
}
