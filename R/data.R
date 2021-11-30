#' Simulated Adverse Event Database
#'
#' A data set containing reported AEs from a trial.
#'
#' @format A data frame with 200 rows--one row per patient
#' \describe{
#'     \item{patient_id}{Patient ID}
#'     \item{trt}{Treatment Group}
#'     \item{system_organ_class}{System Organ Class}
#'     \item{adverse_event}{Adverse Event}
#'     \item{grade}{Grade}
#'     \item{drug_attribution}{Drug Attribution}
#' }
"df_adverse_events"
