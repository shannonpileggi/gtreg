#' Data Listing Table
#'
#' Function creates a gtsummary-class listing of data. Column labels are
#' used as column headers, when present.
#' The listing prints observations in the order of the input data.
#'
#' @param data a data frame
#' @param bold_headers logical indicating whether to bold column headers.
#' Default is `TRUE`
#' @param group_by Single variable name indicating a grouping variable.
#' Default is `NULL` for no grouping variable. When specified, a grouping
#' row will be added to the first column. See details below.
#'
#' @section group_by:
#'
#' The grouping column and the first column in the table will be combined
#' and the type/class may be converted to common type/class for both columns.
#' However, if either the `group_by=` column or the first column are factors,
#' the factor column(s) will first be converted to character.
#'
#' The groups are ordered according to the grouping
#' variable's type (i.e.,  character, numeric, or factor).
#'
#' @section Details:
#'
#' The purpose of `tbl_listing()` is to add support for printing data frames,
#' while taking advantage of the \{gtsummary\} defaults, e.g. ability to print
#' to most output formats, using print themes to have a common style to all
#' tables in a document, etc.
#'
#' While the output of `tbl_listing()` is class `'gtsummary'`, these tables
#' are not meant to be merged with other `'gtsummary'` tables with `tbl_merge()`,
#' or reporting table contents with `inline_text()`. The reason is that a
#' proper `'gtsummary'` contains
#' [additional, hidden structure](https://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html)
#' not present in the result of `tbl_listing()`. If you do need to report
#' the results of `tbl_listing()` in-line, it's recommended to convert
#' the table to a data frame, then extract the needed cell, e.g.
#'
#' ```r
#' tbl_listing() |>
#'   as_tibble(col_names = FALSE) |>
#'   dplyr::slice(1) |>
#'   dplyr::pull(colname)`
#' ````
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
#' \if{html}{\figure{tbl_listing_ex1.png}{options: width=65\%}}
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
    .select_to_varnames(
      select = {{ group_by }},
      data = data,
      arg_name = "group_by",
      select_single = TRUE
    )

  # add a grouping row if specified, add the grouping rows to the data frame ---
  if (!is.null(group_by)) {
    first_column <- names(data) %>% setdiff(c(group_by, "row_type")) %>% purrr::pluck(1)

    # adding grouped row -------------------------------------------------------
    data <-
      data %>%
      dplyr::group_by(.data[[group_by]]) %>%
      dplyr::group_map(
        function(.x, .y) {
          rbind(
            # creating a 1 row data frame to stack with the primary data set
            rlang::set_names(.y, first_column) %>%
              mutate(
                dplyr::across(where(is.factor) & all_of(first_column), as.character), # rbind() cannot stack a factor column and numeric column, must convert to chr
                row_type = "label",
                .before = 1L
              ) %>%
              {cbind(., rlang::inject(tibble::tibble(!!!(rep_len(list(NA), length.out = length(setdiff(names(.x), c("row_type", first_column)))) %>%
                                                           stats::setNames(setdiff(names(.x), c("row_type", first_column)))))))},
            as.data.frame(.x) %>%
              mutate(dplyr::across(where(is.factor) & all_of(first_column), as.character))
          )
        }
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
    gtsummary::modify_column_alignment(columns = everything(), align = "left") %>%
    gtsummary::modify_column_indent(columns = dplyr::everything(), indent = 0)

  # indenting levels if there is a grouping variable ---------------------------
  if (!is.null(group_by)) {
    tbl <-
      gtsummary::modify_table_styling(
        x = tbl,
        columns = all_of(first_column),
        rows = .data$row_type == "level",
        indent = 4L
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
    structure(class = c("tbl_listing", "gtsummary"))
}
