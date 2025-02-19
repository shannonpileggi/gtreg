#' @keywords internal
#' @importFrom purrr %||%
#' @importFrom rlang .env .data sym is_missing := is_empty
#' @importFrom dplyr mutate select filter ungroup group_by arrange across
#' @importFrom cli cli_alert_danger cli_code cli_alert_info
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(c(".", "where"))

## usethis namespace: start
## usethis namespace: end
NULL
