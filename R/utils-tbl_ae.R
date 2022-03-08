# this function returns a list of tbls, summarizing either AEs or SOC
.lst_of_tbls <- function(lst_data,
                         variable_summary,
                         variable_filter,
                         statistic,
                         remove_header_row,
                         zero_symbol = NULL,
                         labels = NULL,
                         header_by = "{level}",
                         by = "by",
                         by_level_to_hide = "NOT OBSERVED",
                         digits = NULL,
                         sort = NULL,
                         missing_location = "first",
                         missing_text = "Unknown") {
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

        # sorting the factor by frequency, if requested
        if ("ae" %in% sort) {
          # vector of levels in descending frequency order
          freg_levels <-
            df_ae %>%
            filter(!.data$by %in% "NOT OBSERVED") %>%
            mutate(
              "{variable_summary}{index}" :=
                forcats::fct_infreq(
                  .data[[stringr::str_glue("{variable_summary}{index}")]])
            ) %>%
            dplyr::pull(.data[[stringr::str_glue("{variable_summary}{index}")]]) %>%
            levels()

          # re-order AEs by frequency
          df_ae[[stringr::str_glue("{variable_summary}{index}")]] <-
            factor(
              df_ae[[stringr::str_glue("{variable_summary}{index}")]],
              levels = freg_levels)
        }
      }

      fn_tbl_ae <-
        purrr::partial(.fn_tbl,
                       variable = stringr::str_glue("{variable_summary}{index}"),
                       by = by,
                       by_level_to_hide = by_level_to_hide,
                       label = labels[index],
                       statistic = statistic,
                       header_by = header_by,
                       remove_header_row = TRUE,
                       zero_symbol = zero_symbol,
                       digits = digits,
                       missing_location = missing_location,
                       missing_text = missing_text)

      if ("strata" %in% names(df_ae)) {
        tbl <-
          gtsummary::tbl_strata(
            data = df_ae,
            strata = "strata",
            .tbl_fun = ~fn_tbl_ae(data = .x),
            .combine_with = "tbl_merge",
            .header = "{strata}"
          )
      }
      else {
        tbl <- fn_tbl_ae(data = df_ae)
      }

      gtsummary::tbl_butcher(tbl)
    }
  )
}

# define `tbl_summary()` function to tabulate SOC/AE
.fn_tbl <- function(data, variable, label = NULL, statistic,
                    remove_header_row, zero_symbol = NULL, by = "by",
                    header_by = "{level}",
                    by_level_to_hide = "NOT OBSERVED", digits = NULL,
                    missing_location = "first",
                    missing_text = "Unknown") {
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
  if (by_level_to_hide %in% tbl$df_by$by) {
    column_to_hide <-
      tbl$df_by %>%
      filter(.data$by %in% by_level_to_hide) %>%
      purrr::pluck("by_col")

    tbl <- gtsummary::modify_column_hide(tbl, columns = all_of(column_to_hide))
  }

  # hiding unknown column if requested
  if (missing_location %in% "hide" && missing_text %in% tbl$df_by$by) {
    missing_column_name <-
      tbl$df_by %>%
      filter(.data$by %in% missing_text) %>%
      purrr::pluck("by_col")

    tbl <-
      gtsummary::modify_column_hide(tbl, dplyr::all_of(missing_column_name))
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
      # create expression with code to set zero count data to the zero_symbol
      expr_zero_to_NA <-
        purrr::map2(
          df_zero_columns$col_name,
          purrr::map(df_zero_columns$data, ~unlist(.x) %>% unname()),
          function(col_name, labels) {
            rlang::expr(
              dplyr::across(dplyr::all_of(!!col_name),
                            ~ifelse(.data$label %in% !!labels, !!zero_symbol, .))
            )
          }
        )

      tbl <-
        tbl %>%
        # setting cells with zero count to missing
        gtsummary::modify_table_body(
          ~ dplyr::mutate(.x, !!!expr_zero_to_NA)
        )
    }
  }

  # removing footnote
  tbl$table_styling$footnote <- dplyr::filter(tbl$table_styling$footnote, FALSE)

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
    tbl_final <- gtsummary::tbl_stack(lst_tbl_ae, quiet = TRUE)
  }

  # setting indentation rules
  tbl_final$table_styling$text_format <-
    tbl_final$table_styling$text_format %>%
    filter(!(.data$column %in% "label" & .data$format_type %in% "indent"))
  if (!is.null(lst_tbl_soc)) {
    tbl_final <-
      gtsummary::modify_table_styling(
        x = tbl_final,
        columns = "label",
        rows = startsWith(.data$variable, "ae"),
        text_format = "indent"
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


.sort_lst_of_soc_tibbles <- function(lst_data, sort) {
  # if not SOC or only one SOC, and not frequency sorted return unaltered
  if (!(length(lst_data) > 1 && "soc" %in% sort)) {
    return(lst_data)
  }

  ordered_names <-
    purrr::imap(
      lst_data,
      ~ .x %>%
        filter(!.data$by %in% "NOT OBSERVED") %>%
        nrow()
    ) %>%
    unlist() %>%
    sort(decreasing = TRUE) %>%
    names()

  lst_data <- lst_data[ordered_names]
}


# calculate Ns within stratum and overall
.header_info <- function(x) {
  data <-
    x$inputs$id_df %||%
    x$inputs$data %>%
    select(any_of(c(x$inputs$id, x$inputs$strata))) %>%
    dplyr::distinct()

  # only calculate Ns when patient is defined (e.g. not `tbl_ae_count()`) ------
  if (!is.null(x$inputs$id)) {
    data <-
      data %>%
      dplyr::group_by(across(any_of(x$inputs$strata))) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        N = sum(.data$n),
        p = .data$n / .data$N
      )
  }

  if (isTRUE(x$inputs$strata %in% names(data))) {
    data <-
      data %>%
      dplyr::rename(strata = all_of(x$inputs$strata)) %>%
      dplyr::left_join(
        x$table_styling$header %>%
          dplyr::filter(startsWith(.data$column, "stat")) %>%
          dplyr::select(.data$column, by = .data$label, strata = .data$spanning_header),
        by = "strata"
      )
  }
  else {
    data <-
      data %>%
      dplyr::bind_cols(
        x$table_styling$header %>%
          dplyr::filter(startsWith(.data$column, "stat")) %>%
          dplyr::select(.data$column, by = .data$label)
      ) %>%
      select(-any_of(c("n", "p")))
  }

  # save all stats available to report in the `modify_*()` fns -----------------
  df_modify_stats <-
    data %>%
    dplyr::mutate(
      overall = is.null(x$inputs$strata) && is.null(x$inputs$by) && is.null(x$inputs$include),
      unknown = .data$by %in% "Unknown"
    ) %>%
    dplyr::select(any_of(c("overall", "unknown",
                           "column", "strata", "by",
                           "N", "n", "p"))) %>%
    dplyr::rename_with(
      .fn = ~paste0("selector_", .),
      .cols = any_of(c("overall", "unknown"))
    ) %>%
    dplyr::rename_with(
      .fn = ~paste0("modify_stat_", .),
      .cols = any_of(c("strata", "by", "N", "n", "p"))
    )

  # add rows to header df ------------------------------------------------------
  x$table_styling$header <-
    .rows_update_table_styling_header(
      x$table_styling$header,
      df_modify_stats
    )

  # return gtreg table ---------------------------------------------------------
  x
}


.rows_update_table_styling_header <- function(x, y) {
  common_columns <- intersect(names(x), names(y))

  x %>%
    # updating rows in header
    dplyr::rows_update(
      y %>% select(all_of(common_columns)),
      by = "column"
    ) %>%
    # re-adding the columns not in the original header table
    dplyr::left_join(
      y %>% select(-all_of(setdiff(common_columns, "column"))),
      by = "column"
    )
}
