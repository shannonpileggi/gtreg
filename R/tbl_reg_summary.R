#' Data Summary Table
#'
#' @inheritParams gtsummary::tbl_summary
#'
#' @return a gtsummary tbl
#' @export

#' @seealso See \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{vignette} for detailed tutorial#'
#' @examples
#' tbl_reg_summary_ex1 <-
#'   gtsummary::trial %>%
#'   dplyr::select(age, marker, grade, trt) %>%
#'   tbl_reg_summary(by = trt)
tbl_reg_summary <- function(data,
                            by = NULL,
                            label = NULL,
                            statistic = NULL,
                            digits = NULL,
                            type = NULL,
                            value = NULL,
                            missing = c("no", "yes", "ifany"),
                            missing_text = NULL,
                            sort = NULL,
                            percent = NULL,
                            include = everything()) {
  missing <- match.arg(missing)
  gtsummary::with_gtsummary_theme(
    x = gtreg_theme,
    expr =
      gtsummary::tbl_summary(
        data = data,
        by = {{ by }},
        label = label,
        statistic = statistic,
        digits = digits,
        type = type,
        value = value,
        missing = missing,
        missing_text = missing_text,
        sort = sort,
        percent = percent,
        include = {{ include }}
      )
  )
}

gtreg_theme <-
  list(
    "tbl_summary-str:default_con_type" = "continuous2",
    "tbl_summary-str:continuous_stat" =
      c("{N_nonmiss}", "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}", "{N_miss}"),
    "tbl_summary-fn:percent_fun" = function(x) style_percent(x, digits = 1)
  )

# tbl_reg_summary <- function(data,
#                             by = NULL,
#                             label = NULL,
#                             statistic = NULL,
#                             digits = NULL,
#                             type = NULL,
#                             value = NULL,
#                             missing = c("no", "yes", "ifany"),
#                             missing_text = NULL,
#                             sort = NULL,
#                             percent = NULL,
#                             include = everything()) {
#   # minimal checking of inputs -------------------------------------------------
#   # most check occur in `tbl_summary()`
#   stopifnot(is.data.frame(data))
#   missing <- match.arg(missing)
#
#   # evaluating inputs ----------------------------------------------------------
#   by <-
#     .select_to_varnames(select = {{ by }}, data = data,
#                         arg_name = "by", select_single = TRUE)
#   include <-
#     .select_to_varnames(select = {{ include }},
#                         data = data, arg_name = "include") %>%
#     union(by) # include by variable by default
#   value <-
#     .formula_list_to_named_list(x = value, data = data, arg_name = "value")
#
#   # assign summary type --------------------------------------------------------
#   # 1. identify a default summary type, so users can change a type with
#   # helpers like `all_dichotomous() ~ "categorical"`
#   default_type <-
#     assign_summary_type(data = data, variable = include,
#                         summary_type = NULL, value = value)
#   # evaluate the user-passed type
#   type <-
#     .formula_list_to_named_list(
#       x = type,
#       data = data,
#       var_info = tibble::enframe(unlist(default_type), "variable", "var_type"),
#       arg_name = "type"
#     )
#   # assign a final type for all variables
#   type <-
#     assign_summary_type(data = data, variable = include,
#                         summary_type = type, value = value)
#
#   # assign default summary statistics ------------------------------------------
#   statistic <-
#     .formula_list_to_named_list(x = statistic, data = data, arg_name = "statistic") %||%
#     list()
#
#   for (variable in setdiff(include, by)) {
#     if (type[[variable]] %in% "continuous2" && is.null(statistic[[variable]])) {
#       statistic[[variable]] <-
#         c("{N_nonmiss}", "{mean}", "{sd}", "{median}", "{p25}, {p75}", "{min}, {max}", "{N_miss}")
#     }
#     else if (type[[variable]] %in% c("categorical", "dichotomous") && is.null(statistic[[variable]])) {
#       statistic[[variable]] <- "{n} ({p})"
#     }
#   }
#
#   # assign default digits for continuous variables -----------------------------
#   digits <-
#     .formula_list_to_named_list(x = digits, data = data, arg_name = "digits") %||%
#     list()
#   for (variable in setdiff(include, by)) {
#     if (startsWith(type[[variable]], "continuous") && is.null(digits[[variable]])) {
#       round_to <- continuous_digits_guess(data, variable)
#       digits[[variable]] <-
#         statistic[[variable]] %>%
#         paste(collapse = " ") %>%
#         stringr::str_extract_all("\\{.*?\\}") %>%
#         purrr::map(~stringr::str_remove_all(.x, pattern = stringr::fixed("}"))) %>%
#         purrr::map(~stringr::str_remove_all(.x, pattern = stringr::fixed("{"))) %>%
#         unlist() %>%
#         {dplyr::case_when(
#           . %in% c("N_nonmiss", "N_miss", "N_obs") ~ list(gtsummary::style_number),
#           . %in% c("p_miss", "p_nonmiss") ~ list(gtsummary::style_percent),
#           TRUE ~ list(round_to)
#         )}
#     }
#   }
#
#   # run tbl_summary() ----------------------------------------------------------
#   gtsummary::tbl_summary(
#     data = data,
#     by = by,
#     label = label,
#     statistic = statistic,
#     digits = digits,
#     type = type,
#     value = value,
#     missing = missing,
#     missing_text = missing_text,
#     sort = sort,
#     percent = percent,
#     include = include
#   )
# }
#
# assign_summary_type <- function(data, variable, summary_type, value) {
#   # base classes that can be summarized as continuous
#   base_numeric_classes <- c("numeric", "integer", "difftime", "Date", "POSIXt", "double")
#
#   # assigning the summary type for each variable -------------------------------
#   assigned_summary_type <-
#     purrr::map_chr(
#       variable,
#       function(variable) {
#         # checking if user requested type = "categorical" for variable that is all missing
#         if (identical(summary_type[[variable]], "categorical") && sum(is.na(data[[variable]])) == nrow(data)) {
#           stringr::str_glue(
#             "Variable '{variable}' is `NA` for all observations and cannot be summarized as 'categorical'. ",
#             "Using `{variable} ~ \"dichotomous\"` instead."
#           ) %>%
#             stringr::str_wrap() %>%
#             rlang::inform()
#           return("dichotomous")
#         }
#
#         # return type if specified by user
#         if (!is.null(summary_type[[variable]])) {
#           return(summary_type[[variable]])
#         }
#
#         # return dichotomous if dichotomous value passed
#         if (!is.null(value[[variable]])) {
#           return("dichotomous")
#         }
#
#         # logical variables are dichotomous
#         if (inherits(data[[variable]], "logical")) {
#           return("dichotomous")
#         }
#
#         # if all missing
#         if (sum(is.na(data[[variable]])) == nrow(data)) {
#           if (inherits(data[[variable]], base_numeric_classes)) {
#             return("continuous2")
#           }
#           if (inherits(data[[variable]], "character")) {
#             return("dichotomous")
#           }
#           if (inherits(data[[variable]], "factor") &&
#               !rlang::is_empty(attr(data[[variable]], "levels"))) {
#             return("categorical")
#           }
#           if (inherits(data[[variable]], "factor") &&
#               rlang::is_empty(attr(data[[variable]], "levels"))) {
#             return("dichotomous")
#           }
#         }
#
#         # numeric variables that are 0 and 1 only, will be dichotomous
#         if (inherits(data[[variable]], c("integer", "numeric")) &&
#             length(setdiff(stats::na.omit(data[[variable]]), c(0, 1))) == 0) {
#           return("dichotomous")
#         }
#
#         # factor variables that are "No" and "Yes" only, will be dichotomous
#         if (inherits(data[[variable]], "factor") &&
#             setequal(attr(data[[variable]], "levels"), c("No", "Yes"))) {
#           return("dichotomous")
#         }
#         if (inherits(data[[variable]], "factor") &&
#             setequal(attr(data[[variable]], "levels"), c("no", "yes"))) {
#           return("dichotomous")
#         }
#         if (inherits(data[[variable]], "factor") &&
#             setequal(attr(data[[variable]], "levels"), c("NO", "YES"))) {
#           return("dichotomous")
#         }
#
#         # character variables that are "No" and "Yes" only, will be dichotomous
#         if (inherits(data[[variable]], "character") &&
#             setequal(stats::na.omit(data[[variable]]), c("No", "Yes"))) {
#           return("dichotomous")
#         }
#         if (inherits(data[[variable]], "character") &&
#             setequal(stats::na.omit(data[[variable]]), c("no", "yes"))) {
#           return("dichotomous")
#         }
#         if (inherits(data[[variable]], "character") &&
#             setequal(stats::na.omit(data[[variable]]), c("NO", "YES"))) {
#           return("dichotomous")
#         }
#
#         # factors and characters are categorical (except when all missing)
#         if (inherits(data[[variable]], c("factor", "character"))) {
#           return("categorical")
#         }
#
#         # numeric variables with fewer than 10 levels will be categorical
#         if (inherits(data[[variable]], base_numeric_classes) &&
#             length(unique(stats::na.omit(data[[variable]]))) < 10) {
#           return("categorical")
#         }
#
#         # all other numeric classes are continuous
#         if (inherits(data[[variable]], base_numeric_classes)) {
#           return("continuous2")
#         }
#
#         # otherwise return NA (which will print an informative message later)
#         return(NA_character_)
#       }
#     )
#
#   assigned_summary_type %>%
#     as.list() %>%
#     rlang::set_names(variable)
# }
#
# continuous_digits_guess <- function(data, variable) {
#   # if all values are NA, returning 0
#   if (nrow(data) == sum(is.na(data[[variable]]))) {
#     return(0)
#   }
#
#   # if class is integer, then round everything to nearest integer
#   if (inherits(data[[variable]], "integer")) {
#     return(0)
#   }
#
#   # calculate the spread of the variable
#   var_spread <-
#     stats::quantile(data[[variable]], probs = c(0.95), na.rm = TRUE) -
#     stats::quantile(data[[variable]], probs = c(0.05), na.rm = TRUE)
#
#   # otherwise guess the number of dignits to use based on the spread
#   dplyr::case_when(
#     var_spread < 0.01 ~ 4,
#     var_spread >= 0.01 & var_spread < 0.1 ~ 3,
#     var_spread >= 0.1 & var_spread < 10 ~ 2,
#     var_spread >= 10 & var_spread < 20 ~ 1,
#     var_spread >= 20 ~ 0
#   )
# }

