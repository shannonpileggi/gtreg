#' Modify AE Table Headers and Spanning Headers
#'
#' @param x object of class "tbl_ae", "tbl_ae_count", or "tbl_ae_focus"
#' @param ... Updates to header and spanning headers. Can pass either named
#' arguments (e.g. `label = "**AE**"`) or a
#' selecting formula (e.g. `all_ae_cols() ~ "**Grade {by}**"`)
#' @inheritParams gtsummary::modify_header
#'
#' @return an object of the same class as the input
#' @name modify_ae_header
#'
#' @section Details:
#' When updating the header and spanning headers, the following dynamic values
#' are available. The dynamic values are inserted using `stringr::str_glue()`
#' syntax.
#'   - `"{by}"` the level of the by variable
#'   - `"{strata}"` the level of the strata variable
#'   - `"{n}"` number of patients within a stratum
#'   - `"{N}"` total number of patients
#'   - `"{p}"` proportion of patients within a stratum (on the zero-one scale)
#'
#' The `"{n}"`, `"{N}"`, and `"{p}"` statistics are not available for
#' `tbl_ae_count()` objects.
#'
#' @examples
#' \donttest{
#' modify_ae_header_ex1 <-
#'   df_adverse_events %>%
#'   tbl_ae(
#'     id = patient_id,
#'     ae = adverse_event,
#'     soc = system_organ_class,
#'     by = grade,
#'     strata = trt
#'   ) %>%
#'   modify_ae_header(all_ae_cols() ~ "**Grade {by}**") %>%
#'   modify_ae_spanning_header(all_ae_cols() ~ "**{strata}** N = {n} / {N}")
#'   }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{modify_ae_header_ex1.png}{options: width=65\%}}
NULL


#' @export
#' @rdname modify_ae_header
modify_ae_header <- function(x, ..., text_interpret = c("md", "html")) {
  .update_headers(x = x, header_type = "column_header", ..., text_interpret = text_interpret)
}

#' @export
#' @rdname modify_ae_header
modify_ae_spanning_header <- function(x, ..., text_interpret = c("md", "html")) {
  .update_headers(x = x, header_type = "spanning_header", ..., text_interpret = text_interpret)
}

.update_headers <- function(x, header_type, ..., text_interpret) {
  # check inputs ---------------------------------------------------------------
  if (!inherits(x, c("tbl_ae", "tbl_ae_count", "tbl_ae_focus"))) {
    stop('`x=` must be class "tbl_ae", "tbl_ae_count", or "tbl_ae_focus"', call. = FALSE)
  }

  # convert headers to a named list --------------------------------------------
  lst_headers <-
    rlang::dots_list(...) %>%
    broom.helpers::.formula_list_to_named_list(
      var_info =
        dplyr::left_join(
          x$table_styling$header %>% select(.data$column),
          x$header_info,
          by = "column"
        ) %>%
        dplyr::rename(variable = .data$column),
      type_check = function(x) rlang::is_string(x) || is.na(x),
      type_check_msg = "Expecting a string or NA as the passed value."
    )

  # evaluate header in glue env ------------------------------------------------
  lst_headers <-
    lst_headers %>%
    purrr::imap(
      function(label, column) {
        header_info <- filter(x$header_info, .data$column %in% .env$column)

        purrr::when(
          header_info,
          nrow(.) > 0 ~ stringr::str_glue_data(., label),
          TRUE ~ label
        )
      }
    )

  # update headers -------------------------------------------------------------
  result <-
    switch(
      header_type,
      "column_header" =
        rlang::inject(gtsummary::modify_header(x, !!!lst_headers, text_interpret = text_interpret)),
      "spanning_header" =
        rlang::inject(gtsummary::modify_spanning_header(x, !!!lst_headers, text_interpret = text_interpret))
    ) %>%
    # removing call list
    purrr::list_modify(call_list = NULL) %>%
    purrr::compact()
  result$table_styling$header$hide <- x$table_styling$header$hide

  # return tbl -----------------------------------------------------------------
  result %>%
    # add class
    structure(class = class(x))
}
