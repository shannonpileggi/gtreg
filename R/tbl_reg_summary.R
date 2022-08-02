#' Data Summary Table
#'
#' Function wraps `gtsummary::tbl_summary()` to create a data summary
#' table often seen in regulatory submissions. Continuous variable summaries
#' are shown on multiple lines with additional summary statistics and percentages
#' are shown for categorical variables; precision levels estimated based on values observed.
#'
#' @param by A column name (quoted or unquoted) in `data.` Summary statistics
#' will be calculated separately for each level of the by variable
#' (e.g. `by = trt`). If `NULL`, summary statistics are calculated using all observations.
#' @param statistic List of formulas specifying types of summary statistics
#' to display for each variable.
#' @param type List of formulas specifying variable types.
#' Accepted values are `c("continuous", "continuous2", "categorical", "dichotomous")`,
#' e.g. `type = list(age ~ "continuous", female ~ "dichotomous")`.
#' If type not specified for a variable, the function will default to an appropriate summary type.
#' @param value List of formulas specifying the value to display for dichotomous
#' variables. gtsummary selectors, e.g. `all_dichotomous()`, cannot be used with this argument.
#' @inheritParams gtsummary::tbl_summary
#'
#' @return a 'tbl_reg_summary' object
#' @export
#'
#' @seealso See \href{https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html}{`gtsummary::tbl_summary()`} help file
#' @seealso See \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{vignette} for detailed tutorial
#' @examples
#' tbl_reg_summary_ex1 <-
#'   df_patient_characteristics %>%
#'   tbl_reg_summary(by = trt, include = c(marker, status))
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_reg_summary_ex1.png}{options: width=65\%}}
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

  # execute `tbl_summary()` code with gtreg theme/defaults ---------------------
  gtsummary::with_gtsummary_theme(
    x = gtreg_theme,
    expr =
      gtsummary::tbl_summary(
        data = data, by = {{ by }}, label = label, statistic = statistic,
        digits = digits, type = type, value = value, missing = missing,
        missing_text = missing_text, sort = sort, percent = percent,
        include = {{ include }}
      ),
    msg_ignored_elements =
      paste("Theme element(s) {.val {elements}} utilized internally",
            "by {.code tbl_reg_summary()} and cannot be modified.\n",
            "Use {.code gtsummary::tbl_summary()} if you",
            "wish to modify these theme elements.")
  ) %>%
    structure(class = c("tbl_reg_summary", "tbl_summary", "gtsummary"))
}

# creating theme for gtreg summaries -------------------------------------------
gtreg_theme <-
  list(
    "tbl_summary-str:default_con_type" = "continuous2",
    "tbl_summary-str:continuous_stat" =
      c("{N_nonmiss}", "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}", "{N_miss}")
  )
