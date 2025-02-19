# copied from the broom.helpers repo on 2025-02-19 -----------------------------
# broom.helpers has deprecated .select_to_varnames() and .formula_list_to_named_list()
#   in favor of {cards} functions. At some point we can delete this file and
#   update our code to use these functions. There is a small change in what is
#   returned when no columns are selected: .select_to_varnames(): returns NULL while cards returns character(0)
#   which will require updates, such as, changing is.null() calls to `rlang::is_empty()`

.select_to_varnames <- function(select, data = NULL, var_info = NULL,
                                arg_name = NULL, select_single = FALSE) {
  if (is.null(data) && is.null(var_info)) {
    cli::cli_abort("At least one of {.arg data} or {.arg var_info} must be specified.")
  }

  select <- rlang::enquo(select)

  # if NULL passed, return NULL
  if (rlang::quo_is_null(select)) {
    return(NULL)
  }

  # if var_info is provided, scope it
  if (!is.null(var_info)) data <- scope_tidy(var_info, data)

  # determine if selecting input begins with `var()`
  select_input_starts_var <-
    !rlang::quo_is_symbol(select) && # if not a symbol (ie name)
    tryCatch(
      identical(
        eval(as.list(rlang::quo_get_expr(select)) |> purrr::pluck(1)),
        dplyr::vars
      ),
      error = function(e) FALSE
    )

  # performing selecting
  res <-
    tryCatch(
      {
        if (select_input_starts_var) {
          # `vars()` was deprecated on June 6, 2022, gtsummary will stop
          # exporting `vars()` at some point as well.
          paste(
            "Use of {.code vars()} is now {.strong deprecated} and support will soon be removed.",
            "Please replace calls to {.code vars()} with {.code c()}."
          ) |>
            cli::cli_alert_warning()

          # `vars()` evaluates to a list of quosures; unquoting them in `select()`
          names(dplyr::select(data, !!!rlang::eval_tidy(select)))
        } else {
          names(dplyr::select(data, !!select))
        }
      },
      error = function(e) {
        if (!is.null(arg_name)) {
          error_msg <- stringr::str_glue(
            "Error in `{arg_name}=` argument input. Select from ",
            "{paste(sQuote(names(data)), collapse = ', ')}"
          )
        } else {
          error_msg <- as.character(e)
        } # nocov
        cli::cli_abort(error_msg, call = NULL)
      }
    )

  # assuring only a single column is selected
  if (select_single == TRUE && length(res) > 1) {
    .select_single_error_msg(res, arg_name = arg_name)
  }

  # if nothing is selected, return a NULL
  if (length(res) == 0) {
    return(NULL)
  }

  res
}


.formula_list_to_named_list <- function(x, data = NULL, var_info = NULL,
                                        arg_name = NULL, select_single = FALSE,
                                        type_check = NULL, type_check_msg = NULL,
                                        null_allowed = TRUE) {
  # if NULL provided, return NULL ----------------------------------------------
  if (is.null(x)) {
    return(NULL)
  }

  # converting to list if single element passed --------------------------------
  if (inherits(x, "formula")) {
    x <- list(x)
  }

  # checking the input is valid ------------------------------------------------
  .check_valid_input(x = x, arg_name = arg_name, type_check = type_check)

  # convert to a named list ----------------------------------------------------
  len_x <- length(x)
  named_list <- vector(mode = "list", length = len_x)
  for (i in seq_len(len_x)) {
    if (rlang::is_named(x[i])) {
      named_list[i] <- list(x[i])
    } else if (rlang::is_formula(x[[i]])) {
      named_list[i] <-
        .single_formula_to_list(x[[i]],
                                data = data,
                                var_info = var_info,
                                arg_name = arg_name,
                                select_single = select_single,
                                type_check = type_check,
                                type_check_msg = type_check_msg,
                                null_allowed = null_allowed
        ) |>
        list()
    } else {
      .formula_select_error(arg_name = arg_name)
    }

    .rhs_checks(
      x = named_list[i][[1]], arg_name = arg_name, type_check = type_check,
      type_check_msg = type_check_msg, null_allowed = null_allowed
    )
  }
  named_list <- purrr::flatten(named_list)

  # removing duplicates (using the last one listed if variable occurs more than once)
  rd <- function(x) {
    x <- rev(x)
    x <- !duplicated(x)
    rev(x)
  }
  tokeep <- names(named_list) |> rd()
  result <- named_list[tokeep]

  if (isTRUE(select_single) && length(result) > 1) {
    .select_single_error_msg(names(result), arg_name = arg_name)
  }
  result
}

.select_single_error_msg <- function(selected, arg_name) {
  if (!rlang::is_empty(arg_name)) {
    stringr::str_glue(
      "Error in `{arg_name}=` argument--select only a single column. ",
      "The following columns were selected, ",
      "{paste(sQuote(selected), collapse = ', ')}"
    ) |>
      cli::cli_abort(call = NULL)
  }
  stringr::str_glue(
    "Error in selector--select only a single column. ",
    "The following columns were selected, ",
    "{paste(sQuote(selected), collapse = ', ')}"
  ) |>
    cli::cli_abort(call = NULL)
}

.check_valid_input <- function(x, arg_name, type_check) {
  if (
    !rlang::is_list(x) &&
    !(rlang::is_vector(x) && rlang::is_named(x))
  ) {
    err_msg <-
      stringr::str_glue(
        "Error processing the `{arg_name %||% ''}` argument. ",
        "Expecting a list or formula.\n",
        "Review syntax details at",
        "'https://www.danieldsjoberg.com/gtsummary/reference/syntax.html'"
      )
    if (tryCatch(do.call(type_check, list(x)), error = function(e) FALSE)) {
      x_string <-
        suppressWarnings(tryCatch(
          switch(rlang::is_string(x),
                 x
          ) %||% as.character(deparse(x)),
          error = function(e) NULL
        ))
      if (!is.null(x_string) && length(x_string) == 1 && nchar(x_string) <= 50) {
        err_msg <-
          paste(
            err_msg,
            stringr::str_glue("Did you mean `everything() ~ {x_string}`?"),
            sep = "\n\n"
          )
      }
    }

    cli::cli_abort(err_msg, call = NULL)
  }

  return(invisible())
}

# checking the type/class/NULL of the RHS of formula
.rhs_checks <- function(x, arg_name,
                        type_check, type_check_msg,
                        null_allowed) {
  purrr::imap(
    x,
    function(rhs, lhs) {
      if (!null_allowed && is.null(rhs)) {
        stringr::str_glue(
          "Error processing `{arg_name %||% ''}` argument for element '{lhs[[1]]}'. ",
          "A NULL value is not allowed."
        ) |>
          cli::cli_abort(call = NULL)
      }

      # check the type of RHS ------------------------------------------------------
      if (!is.null(type_check) && !is.null(rhs) && !type_check(rhs)) {
        stringr::str_glue(
          "Error processing `{arg_name %||% ''}` argument for element '{lhs[[1]]}'. ",
          type_check_msg %||% "The value passed is not the expected type/class."
        ) |>
          cli::cli_abort(call = NULL)
      }
    }
  )

  return(invisible())
}

.single_formula_to_list <- function(x, data, var_info, arg_name,
                                    select_single, type_check, type_check_msg,
                                    null_allowed) {
  # for each formula extract lhs and rhs ---------------------------------------
  # checking the LHS is not empty
  f_lhs_quo <- .f_side_as_quo(x, "lhs")
  if (rlang::quo_is_null(f_lhs_quo)) f_lhs_quo <- rlang::expr(everything())
  # extract LHS of formula
  lhs <- .select_to_varnames(
    select = !!f_lhs_quo,
    data = data,
    var_info = var_info,
    arg_name = arg_name,
    select_single = select_single
  )

  # evaluate RHS of formula in the original formula environment
  rhs <- .f_side_as_quo(x, "rhs") |> rlang::eval_tidy()

  # checking if RHS is NULL ----------------------------------------------------


  # converting rhs and lhs into a named list
  purrr::map(
    lhs,
    ~ list(rhs) |> rlang::set_names(.x)
  ) |>
    purrr::flatten()
}

# there are a couple of places the users input may result in an error.
# this function prints an informative error msg with correct syntax example
.formula_select_error <- function(arg_name) {
  example_text <- formula_select_examples[[arg_name %||% "not_an_arg"]] %||%
    paste(c(
      "label = list(age ~ \"Age, years\")",
      "statistic = list(all_continuous() ~ \"{mean} ({sd})\")",
      "type = list(c(response, death) ~ \"categorical\")"
    ))

  # printing error for argument input
  if (!is.null(arg_name)) {
    cli_alert_danger(
      "There was a problem with the {.code {arg_name}=} argument input."
    )
  } else {
    cli_alert_danger("There was a problem with one of the function argument inputs.")
  }
  cli_alert_info("Below is an example of correct syntax.")
  cli_code(example_text)
  cli::cli_abort("Invalid argument syntax", call = NULL)
}

formula_select_examples <- list(
  labels = "labels = list(age ~ \"Age, years\", response ~ \"Tumor Response\")",
  label = "label = list(age ~ \"Age, years\", response ~ \"Tumor Response\")",
  type = "type = list(age ~ \"continuous\", where(is.integer) ~ \"categorical\")",
  statistic = c(
    paste(
      "statistic = list(all_continuous() ~ \"{mean} ({sd})\",",
      "all_categorical() ~ \"{n} / {N} ({p}%)\")"
    ),
    "statistic = list(age ~ \"{median}\")"
  ),
  digits = c("digits = list(age ~ 2)", "digits = list(all_continuous() ~ 2)"),
  value = c("value = list(grade ~ \"III\")", "value = list(all_logical() ~ FALSE)"),
  test = c("test = list(all_continuous() ~ \"t.test\")", "test = list(age ~ \"kruskal.test\")")
)

#' Scoping a tidy tibble allowing to tidy select
#'
#' This function uses the information from a model tidy tibble to generate
#' a data frame exposing the different variables of the model,
#' data frame that could be used for tidy selection. In addition, columns
#' `"var_type"`, `"var_class"` and `"contrasts_type"` are scoped and their
#' values are added as attributes to the data frame.
#' For example, if `var_type='continuous'` for variable `"age"`, then the
#' attribute `attr(.$age, 'gtsummary.var_type') <- 'continuous'` is set.
#' That attribute is then used in a selector like `all_continuous()`.
#' Note: attributes are prefixed with `"gtsummary."` to be compatible with
#' selectors provided by `{gtsummary}`.
#'
#' @param x (`data.frame`)\cr
#' A tidy tibble, with a `"variable"` column, as returned by
#' [`tidy_identify_variables()`].
#' @param data (`data.frame`)\cr
#' An optional data frame the attributes will be added to.
#' @return A data frame.
#' @keywords internal
#' @noRd
#' @examples
#' mod <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
#' tt <- mod |> tidy_and_attach() |> tidy_add_contrasts()
#'
#' scope_tidy(tt) |> str()
#' scope_tidy(tt, data = model_get_model_frame(mod)) |> str()
#'
#' scope_tidy(tt) |> dplyr::select(dplyr::starts_with("Se")) |> names()
#' scope_tidy(tt) |> dplyr::select(where(is.factor)) |> names()
#' scope_tidy(tt) |> dplyr::select(all_continuous()) |> names()
#' scope_tidy(tt) |> dplyr::select(all_contrasts()) |> names()
#' scope_tidy(tt) |> dplyr::select(all_interaction()) |> names()
#' scope_tidy(tt) |> dplyr::select(all_intercepts()) |> names()
scope_tidy <- function(x, data = NULL) {
  if (!"variable" %in% names(x)) {
    cli::cli_abort(
      "The {.code .$x} data frame does not have the required {.val variable} column."
    )
  }

  # if data not passed, use x to construct one
  if (rlang::is_empty(data)) {
    data <- dplyr::tibble(
      !!!rlang::rep_named(
        unique(as.character(x$variable)),
        logical(0L)
      )
    )

    # if var_class available in x, convert columns
    if ("var_class" %in% names(x)) {
      df_class <- x[c("variable", "var_class")] |>
        unique() |>
        tidyr::drop_na()
      for (i in seq_len(nrow(df_class))) {
        f <- switch(
          df_class$var_class[i],
          "character" = as.character,
          "factor" = as.factor,
          "ordered" = as.ordered,
          "integer" = as.integer,
          "numeric" = as.numeric,
          "complex" = as.complex,
          "Date" = as.Date,
          "POSIXlt" = as.POSIXlt,
          "POSIXct" = as.POSIXct,
          "difftime" = as.difftime,
          as.logical
        )
        data[[df_class$variable[i]]] <- f(NA)
      }
    }
  }

  # only keeping rows that have corresponding column names in data
  x <- x |> dplyr::filter(.data$variable %in% names(data))

  # if x passed, add columns as attr to data
  base_attr_cols <- c("var_type", "var_class", "contrasts_type")
  attr_cols <- x |>
    dplyr::select(any_of(base_attr_cols)) |>
    names()

  # add attributes
  for (v in attr_cols) {
    df_attr <- x[c("variable", v)] |>
      unique() |>
      tidyr::drop_na()
    for (i in seq_len(nrow(df_attr))) {
      attr(data[[df_attr$variable[i]]], paste0("gtsummary.", v)) <- df_attr[[v]][i]
    }
  }

  # return data frame with attributes
  data
}


# extract LHS/RHS of formula as quosure. attached env will be the formula env
.f_side_as_quo <- function(x, side = c("lhs", "rhs")) {
  side <- match.arg(side)
  f_expr <-
    switch(side,
           "lhs" = rlang::f_lhs(x),
           "rhs" = rlang::f_rhs(x)
    )
  f_quo <- rlang::quo(!!f_expr)
  attr(f_quo, ".Environment") <- rlang::f_env(x)
  f_quo
}
