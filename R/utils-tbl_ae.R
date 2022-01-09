# this function returns a list of tbls, summarizing either AEs or SOC
.lst_of_tbls <- function(lst_data,
                         variable_summary,
                         variable_filter,
                         statistic,
                         header_by,
                         header_strata = NULL,
                         remove_header_row,
                         zero_symbol = NULL,
                         labels = NULL,
                         by = "by",
                         by_level_to_hide = "NOT OBSERVED",
                         digits = NULL) {
  purrr::map(
    seq_len(length(lst_data)),
    function(index) {
      # keep observation that will be tabulated
      df_ae <-
        filter(lst_data[[index]], !!sym(variable_filter)) %>%
        dplyr::rename("{variable_summary}{index}" := !!sym(variable_summary))

      if ("ae" %in% variable_summary) {
        df_ae[[stringr::str_glue("{variable_summary}{index}")]] <-
          factor(df_ae[[stringr::str_glue("{variable_summary}{index}")]])
      }
      # ------------------------------------------------------------------------
      # TODO: Add frequency sorting codes here. Define a factor to sort AEs
      # ------------------------------------------------------------------------

      fn_tbl_ae <-
        purrr::partial(.fn_tbl,
                       variable = stringr::str_glue("{variable_summary}{index}"),
                       label = labels[index],
                       statistic = statistic,
                       header_by = header_by,
                       remove_header_row = TRUE,
                       zero_symbol = zero_symbol,
                       digits = digits)

      if ("strata" %in% names(df_ae)) {
        tbl <-
          gtsummary::tbl_strata(
            data = df_ae,
            strata = "strata",
            .tbl_fun = ~fn_tbl_ae(data = .x),
            .combine_with = "tbl_merge",
            .combine_args =
              switch(!is.null(header_strata), list(tab_spanner = header_strata))
          )
      }
      else {
        tbl <- fn_tbl_ae(data = df_ae)
      }

      tbl
    }
  )
}

# define `tbl_summary()` function to tabulate SOC/AE
.fn_tbl <- function(data, variable, label = NULL, statistic, header_by,
                    remove_header_row, zero_symbol = NULL, by = "by",
                    by_level_to_hide = "NOT OBSERVED", digits = NULL) {
  tbl <-
    gtsummary::tbl_summary(
      data = data,
      by = any_of(by),
      percent = "row",
      label = switch(!is.null(label), everything() ~ label),
      statistic = everything() ~ statistic,
      include = all_of(variable),
      digits = switch(!is.null(digits), everything() ~ digits)
    ) %>%
    gtsummary::modify_header(gtsummary::all_stat_cols() ~ header_by)

  # hide the column for unobserved data
  column_to_hide <-
    tbl$df_by %>%
    filter(.data$by %in% by_level_to_hide) %>%
    purrr::pluck("by_col")
  if (!is.null(column_to_hide)) {
    tbl <- gtsummary::modify_column_hide(tbl, columns = all_of(column_to_hide))
  }

  # remove the header row
  if (isTRUE(remove_header_row)) {
    tbl <- gtsummary::remove_row_type(tbl)
  }

  if (!is.null(zero_symbol)) {
    # data frame of zero-count cells
    df_zero_columns <-
      tbl$meta_data$df_stats[[1]] %>%
      filter(.data$n == 0L) %>%
      select(.data$label, .data$col_name) %>%
      tidyr::nest(data = .data$label)

    # if any zero count cells, then set to missing and format
    if (nrow(df_zero_columns) > 0) {
      # create expression with code to set zero count data to NA
      expr_zero_to_NA <-
        purrr::map2(
          df_zero_columns$col_name,
          purrr::map(df_zero_columns$data, ~unlist(.x) %>% unname()),
          function(col_name, labels) {
            rlang::expr(
              dplyr::across(dplyr::all_of(!!col_name),
                            ~ifelse(.data$label %in% !!labels, NA, .))
            )
          }
        )

      tbl <-
        tbl %>%
        # setting cells with zero count to missing
        gtsummary::modify_table_body(
          ~ dplyr::mutate(.x, !!!expr_zero_to_NA)
        ) %>%
        # formatting missing values with 'zero_symbol'
        gtsummary::modify_table_styling(
          columns = gtsummary::all_stat_cols(),
          missing_symbol = zero_symbol
        )
    }
  }

  tbl
}

.stack_soc_ae_tbls <- function(lst_tbl_ae, lst_tbl_soc = NULL) {
  if (!is.null(lst_tbl_soc)) {
    tbl_final <-
      # stack SOC with AEs within that SOC, then stack all tbls
      purrr::map2(lst_tbl_soc, lst_tbl_ae,
                  ~gtsummary::tbl_stack(list(.x, .y), quiet = TRUE)) %>%
      gtsummary::tbl_stack(quiet = TRUE)
  }
  else {
    tbl_final <-
      gtsummary::tbl_stack(lst_tbl_ae, quiet = TRUE) %>%
      # remove indentation for AEs
      gtsummary::modify_table_styling(
        columns = "label",
        text_format = "indent",
        undo_text_format = TRUE
      )
  }

  tbl_final %>%
    # update labels
    gtsummary::modify_header(label = "**Adverse Event**") %>%
    # removing the no longer needed data elements saved in the individual stacked/merged tbls
    gtsummary::tbl_butcher()
}


# convert the complete data df to a df with the Ns within each stratum
.complete_data_to_df_strata <- function(data) {
  if (!"strata" %in% names(data)) {
    return(NULL)
  }

  data %>%
    dplyr::ungroup() %>%
    select(all_of(c("id", "strata"))) %>%
    dplyr::distinct() %>%
    arrange(.data$strata) %>%
    mutate(N = dplyr::n()) %>%
    group_by(.data$strata) %>%
    mutate(
      strata = as.character(.data$strata),
      n = dplyr::n(),
      p = .data$n / .data$N
    ) %>%
    dplyr::ungroup() %>%
    select(level = .data$strata, .data$n, .data$N, .data$p) %>%
    dplyr::distinct()
}


