#' Simulated Adverse Event Database
#'
#' A data set containing reported AEs from a trial.
#'
#' @format A data frame with 100 rows--one row per patient per AE
#' \describe{
#'     \item{patient_id}{Patient ID}
#'     \item{trt}{Treatment Group}
#'     \item{system_organ_class}{System Organ Class}
#'     \item{adverse_event}{Adverse Event}
#'     \item{grade}{Grade}
#'     \item{drug_attribution}{Drug Attribution}
#'     \item{any_complication}{Any Grade Complication}
#'     \item{grade3_complication}{Grade 3+ Complication}
#' }
"df_adverse_events"

#' Simulated Patient Characteristics Database
#'
#' @format A data frame with 100 rows--one row per patient
#' \describe{
#'     \item{patient_id}{Patient ID}
#'     \item{trt}{Treatment Group}
#'     \item{age}{Patient Age}
#'     \item{marker}{Biological Marker}
#'     \item{status}{Study Status}
#'     \item{discontinued}{Discontinued from Study}
#'     \item{off_trt_ae}{Off Treatment Adverse Event}
#' }
"df_patient_characteristics"
