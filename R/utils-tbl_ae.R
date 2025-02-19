.construct_summary_table <- function(data, variable, by = "by", digits, statistic, sort, zero_symbol, missing_location,
                                     # arguments used in `tbl_ae_focus()`
                                     columns_to_hide = "NOT OBSERVED",
                                     header = "**{level}**") {

  # this function will build a single summary table with needed modifications
  .final_summary_fun <- purrr::partial(.build_single_summary_table,
                                       variable = variable,
                                       by = by,
                                       digits = digits,
                                       statistic = statistic,
                                       zero_symbol = zero_symbol,
                                       missing_location = missing_location,
                                       columns_to_hide = columns_to_hide,
                                       header = header)

  data <- .sort_levels_as_factor(data, sort)

  # build the summary table(s)
  if ("strata" %in% names(data)) {
    tbl <-
      gtsummary::tbl_strata(
        data = data,
        strata = all_of("strata"),
        .tbl_fun = ~.final_summary_fun(data = .x),
        .combine_with = "tbl_merge",
        .header = "{strata}"
      )
  }
  else {
    tbl <- .final_summary_fun(data)
  }

  tbl
}

.sort_levels_as_factor <- function(data, sort) {
  if ("ae" %in% sort && all(c("ae", "..ae..") %in% names(data))) {
    ae_levels <-
      data %>%
      filter(!.data$by %in% "NOT OBSERVED", .data$..ae..) %>%
      mutate(fct_ae = forcats::fct_infreq(.data$ae)) %>%
      dplyr::pull("fct_ae") %>%
      levels()

    data$ae <- factor(data$ae, levels = ae_levels)
  }
  if ("soc" %in% sort && all(c("soc", "..soc..") %in% names(data))) {
    soc_levels <-
      data %>%
      filter(!.data$by %in% "NOT OBSERVED", .data$..soc..) %>%
      mutate(fct_soc = forcats::fct_infreq(.data$soc)) %>%
      dplyr::pull("fct_soc") %>%
      levels()

    data$soc <- factor(data$soc, levels = soc_levels)
  }

  data
}

.build_single_summary_table <- function(data, variable, by, digits, statistic,
                                        zero_symbol, missing_location,
                                        columns_to_hide, header) {
  tbl <-
    # build summary table
    gtsummary::tbl_summary(
      data = data,
      by = all_of(by),
      percent = "row",
      include = all_of(variable),
      type = list("categorical") %>% stats::setNames(variable),
      statistic = list(statistic) %>% stats::setNames(variable),
      digits = switch(!is.null(digits), list(digits) %>% stats::setNames(variable))
    ) %>%
    gtsummary::remove_row_type(type = "header") %>%
    gtsummary::modify_header(
      gtsummary::all_stat_cols() ~ header,
      label ~ "**Adverse Event**"
    ) %>%
    # updates with user-passed arguments
    .add_modify_stat_data() %>%
    .relocate_missing_column(missing_location = missing_location) %>%
    .hide_unobserved_columns(columns = columns_to_hide) %>%
    .replace_zero_with_NA(zero_symbol = zero_symbol) %>%
    # clean up the object
    gtsummary::tbl_butcher() %>%
    # remove default footnote
    gtsummary::remove_footnote_header(columns = everything())

  return(tbl)
}
.add_modify_stat_data <- function(tbl) {
  # add modify selector columns
  tbl$table_styling$header <-
    tbl$table_styling$header %>%
    mutate(
      modify_selector_overall = ifelse(startsWith(.data$column, "stat_"), FALSE, NA),
      modify_selector_unknown =
        dplyr::case_when(
          startsWith(.data$column, "stat_") & modify_stat_level == "Unknown" ~ TRUE,
          startsWith(.data$column, "stat_") ~ FALSE
        )
    )

  tbl
}

.hide_unobserved_columns <- function(tbl, columns) {
  column_to_hide <-
    tbl$cards[[1]] |>
    dplyr::filter(
      purrr::map_chr(.data$group1_level, ~ifelse(rlang::is_empty(.x), NA_character_, as.character(.x))) %in% .env$columns
    ) |>
    dplyr::pull("gts_column") |>
    unique()

  if (rlang::is_empty(column_to_hide)) return(tbl)
  gtsummary::modify_column_hide(tbl, columns = all_of(column_to_hide))
}

.relocate_missing_column <- function(tbl, missing_location) {
  # if first, do nothing as this is the default
  if (missing_location == "first") return(tbl)

  # identify column to move
  column_to_relocate <-
    tbl$cards[[1]] |>
    dplyr::filter(purrr::map_chr(.data$group1_level, ~ifelse(rlang::is_empty(.x), NA_character_, as.character(.x))) %in% "Unknown") |>
    dplyr::pull("gts_column") |>
    unique()

  # if no Unknown column, return tbl unmodified
  if (rlang::is_empty(column_to_relocate)) return(tbl)

  # hide column if requested
  if (missing_location == "hide")
    return(gtsummary::modify_column_hide(tbl, columns = all_of(column_to_relocate)))

  # last case is to move Unknown column to 'last'
  gtsummary::modify_table_body(
    tbl,
    ~dplyr::relocate(.x, all_of(column_to_relocate), .after = dplyr::last_col())
  )
}

.replace_zero_with_NA <- function(tbl, zero_symbol) {
  # if NULL, then show the zeros
  if (is.null(zero_symbol)) return(tbl)

  # data frame of zero-count cells
  df_zero_rows <-
    tbl$cards[[1]] |>
    dplyr::filter(.data$stat_name %in% "n", .data$stat %in% 0L, !is.na(.data$gts_column))

  if (nrow(df_zero_rows) == 0L) {
    return(tbl %>%
             gtsummary::modify_table_styling(
               columns = gtsummary::all_stat_cols(),
               rows = !is.na(.data$label),
               missing_symbol = zero_symbol
             ))
  }

  df_zero_columns <-
    df_zero_rows |>
    dplyr::select(label = "variable_level", col_name = "gts_column") |>
    dplyr::mutate(label = unlist(.data$label)) |>
    dplyr::as_tibble() |>
    tidyr::nest(data = "label")

  # create expression with code to set zero count data to the zero_symbol
  expr_zero_to_NA <-
    purrr::map2(
      df_zero_columns$col_name,
      purrr::map(df_zero_columns$data, ~unlist(.x) %>% unname()),
      function(col_name, labels) {
        rlang::expr(
          dplyr::across(dplyr::all_of(!!col_name),
                        ~ifelse(.data$label %in% !!labels, NA_character_, .))
        )
      }
    )

  tbl %>%
    # setting cells with zero count to missing
    gtsummary::modify_table_body(
      ~ dplyr::mutate(.x, !!!expr_zero_to_NA)
    ) %>%
    gtsummary::modify_table_styling(
      columns = gtsummary::all_stat_cols(),
      rows = !is.na(.data$label),
      missing_symbol = zero_symbol
    )
}

.combine_soc_and_ae_tbls <- function(data, tbl_ae, tbl_soc) {
  # first prep tables if there is no SOC table
  if (is.null(tbl_soc)) {
    tbl_final <-
      tbl_ae %>%
      gtsummary::modify_column_indent(columns = all_of("label"), indent = 0)
    return(tbl_final)
  }

  # combine SOC and AE tbls
  table_body <-
    tbl_soc$table_body$label %>%
    purrr::map_dfr(
      function(soc) {
        ae_to_stack <-
          dplyr::filter(data, .data$soc %in% .env$soc) %>%
          dplyr::pull("ae") %>%
          unique()

        tbl_soc$table_body %>%
          dplyr::filter(.data$label %in% .env$soc) %>%
          mutate(row_type = "label") %>%
          dplyr::bind_rows(
            tbl_ae$table_body %>%
              dplyr::filter(.data$label %in% .env$ae_to_stack)
          )
      }
    )

  # return stacked table
  tbl_soc$table_body <- table_body
  tbl_soc
}


.update_modify_stat_columns <- function(tbl, data) {
  data_distinct <- data %>% select(any_of(c("id", "strata"))) %>% dplyr::distinct()

  # update the modify_stat_N, and update name of modify_stat_level
  tbl$table_styling$header$modify_stat_N <- nrow(data_distinct)
  tbl$table_styling$header <-
    tbl$table_styling$header %>%
    dplyr::rename(modify_stat_by = "modify_stat_level") %>%
    select(-any_of(c("modify_stat_n", "modify_stat_p")))

  # if strata present, add little n and p, and merge them into the header
  if ("strata" %in% names(data_distinct)) {
    tbl$table_styling$header <-
      tbl$table_styling$header %>%
      select(-any_of(c("modify_stat_n", "modify_stat_p"))) %>%
      dplyr::left_join(
        data_distinct %>%
          tidyr::nest(data = -"strata") %>%
          dplyr::rowwise() %>%
          mutate(
            spanning_header = as.character(.data$strata) %>% glue::glue() %>% as.character(),
            modify_stat_strata = .data$spanning_header,
            modify_selector_strata = .data$spanning_header,
            modify_stat_n = nrow(.data$data)
          ) %>%
          dplyr::ungroup() %>%
          select(all_of(c("spanning_header", "modify_selector_strata", "modify_stat_strata", "modify_stat_n"))) %>%
          dplyr::inner_join(tbl$table_styling$spanning_header[c("column", "spanning_header")], by = "spanning_header") %>%
          select(-"spanning_header"),
        by = "column"
      ) %>%
      mutate(modify_stat_p = .data$modify_stat_n / .data$modify_stat_N)
  }

  tbl
}

# check_factor is not an exported function in forcats, copying here
# for internal use
.check_factor <- function(x) {
  if (is.character(x)) {
    factor(x)
  } else if (is.factor(x)) {
    x
  } else {
    cli::cli_abort(
      "Input must be a factor or character vector."
    )
  }
}

# keeping deprecated forcats function in order
# to not change default behavior with new functions
.fct_explicit_na <- function(f, na_level = "(Missing)") {

  f <- .check_factor(f)

  is_missing <- is.na(f)
  is_missing_level <- is.na(levels(f))

  if (any(is_missing)) {
    f <- forcats::fct_expand(f, na_level)
    f[is_missing] <- na_level

    f
  } else if (any(is_missing_level)) {
    levs <- levels(f)
    levs[is.na(levs)] <- na_level

    forcats::lvls_revalue(f, levs)
  } else {
    f
  }
}

# internal version of purrr::when() due to deprecation
.when <- function(., ...) {

  dots   <- list(...)
  names  <- names(dots)
  named  <- if (is.null(names)) rep(FALSE, length(dots)) else names != ""

  if (sum(!named) == 0)
    cli::cli_abort("At least one matching condition is needed.")

  is_formula <-
    vapply(dots,
           function(dot) identical(class(dot), "formula"),
           logical(1L))

  env <- new.env(parent = parent.frame())
  env[["."]] <- .

  if (sum(named) > 0)
    for (i in which(named))
      env[[names[i]]] <- dots[[i]]

  result <- NULL
  for (i in which(!named)) {
    if (is_formula[i]) {
      action <- length(dots[[i]])
      if (action == 2 || rlang::is_true(eval(dots[[i]][[2]], env, env))) {
        result <- eval(dots[[i]][[action]], env, env)
        break
      }
    } else {
      result <- dots[[i]]
    }
  }

  result
}
