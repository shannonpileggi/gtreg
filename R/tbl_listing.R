#' Create a Data Listing
#'
#' Function creates a gtsummary-class listing of data. Column labels are
#' used as column headers, when present.
#'
#' @param data a data frame
#' @param bold_headers logical indicating whether to bold column headers.
#' Default is `TRUE`
#' @param group_by Single variable name indicating a grouping variable.
#' Default is `NULL` for no grouping variable. When specified, a grouping
#' row will be added to the first column. Both the grouping variable and the
#' first column must be character.
#'
#' @return gtsummary data listing
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' tbl_listing_ex1 <-
#'   head(df_adverse_events, n = 10) %>%
#'   select(system_organ_class, adverse_event, grade, drug_attribution, patient_id) %>%
#'   arrange(adverse_event, desc(grade)) %>%
#'   tbl_listing(group_by = system_organ_class) %>%
#'   bold_labels()
#'
#' set.seed(11234)
#' tbl_listing_ex2 <-
#' df_patient_characteristics %>%
#'   dplyr::slice_sample(n = 10) %>%
#'   select(patient_id, status, discontinued, off_trt_ae) %>%
#'   tbl_listing() %>%
#'   as_gt() %>%
#'   gt::opt_row_striping()
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_listing_ex1.png}{options: width=75\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_listing_ex2.png}{options: width=75\%}}
tbl_listing <- function(data, group_by = NULL, bold_headers = TRUE) {
  # process inputs -------------------------------------------------------------
  stopifnot(is.data.frame(data))
  data <-
    dplyr::ungroup(data) %>%
    dplyr::mutate(row_type = "level", .before = 1L)

  group_by <-
    broom.helpers::.select_to_varnames(
      select = {{ group_by }},
      data = data,
      arg_name = "group_by",
      select_single = TRUE
    )

  # add a grouping row if specified, add the grouping rows to the data frame ---
  if (!is.null(group_by)) {
    if (!is.character(data[[group_by]])) {
      stop("The column  specified in `group_by=` must be character.", call. = FALSE)
    }

    first_column <- names(data) %>% setdiff(c(group_by, "row_type")) %>% purrr::pluck(1)
    if (!is.character(data[[first_column]])) {
      stringr::str_glue(
        "When `group_by=` is specified, the first reported column in the",
        "data frame ('{first_column}') must be character."
      ) %>%
      stop(call. = FALSE)
    }

    # adding grouped row -------------------------------------------------------
    data <-
      data %>%
      dplyr::group_by(.data[[group_by]]) %>%
      dplyr::group_map(
        ~ dplyr::bind_rows(
          .y %>% rlang::set_names(first_column) %>% mutate(row_type = "label", .before = 1L),
          .x
        )
      ) %>%
      dplyr::bind_rows() %>%
      # re-instating column labels
      purrr::imap_dfr(
        function(.x, .y) {
          attr(.x, "label") <- attr(data[[.y]], "label")
          .x
        }
      )
  }


  # convert data frame to gtsummary table --------------------------------------
  tbl <-
    gtsummary::.create_gtsummary_object(table_body = data) %>%
    gtsummary::modify_column_unhide(columns = -any_of("row_type")) %>%
    gtsummary::modify_column_alignment(columns = everything(), align = "left")

  # indenting levels if there is a grouping variable ---------------------------
  if (!is.null(group_by)) {
    tbl <-
      gtsummary::modify_table_styling(
        x = tbl,
        columns = all_of(first_column),
        rows = .data$row_type == "level",
        text_format = "indent"
      )
  }

  # add column labels  ---------------------------------------------------------
  tbl$table_styling$header <-
    tbl$table_styling$header %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      label = attr(data[[.data$column]], "label") %||% .data$column
    ) %>%
    dplyr::ungroup()


  # add markdown bold syntax ---------------------------------------------------
  if (isTRUE(bold_headers)) {
    tbl$table_styling$header$label <-
      paste0("**", tbl$table_styling$header$label, "**")
  }

  # return gtsummary tbl -------------------------------------------------------
  tbl %>%
    structure(class = c("tbl_lsiting", "gtsummary"))
}
