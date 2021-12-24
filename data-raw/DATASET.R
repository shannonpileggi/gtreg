set.seed(1122336654)
n <- 50
df_adverse_events <-
  tibble::tibble(
    patient_id = paste("ID", sample.int(10, n, replace = TRUE)),
    system_organ_class = "Blood and lymphatic system disorders",
    adverse_event =
      sample(c("Anaemia", "Increased tendency to bruise",
               "Iron deficiency anaemia", "Thrombocytopenia"),
             size = n, replace = TRUE),
    grade = sample.int(5, n, replace = TRUE)
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      patient_id = paste("ID", sample.int(10, n, replace = TRUE)),
      system_organ_class = "Gastrointestinal disorders",
      adverse_event =
        sample(c("Intestinal dilatation", "Myochosis", "Difficult digestion",
                 "Pancreatic enzyme abnormality", "Non-erosive reflux disease"),
               size = n, replace = TRUE),
      grade = sample.int(5, n, replace = TRUE)
    )
  ) |>
  dplyr::left_join(
    tibble::tibble(
      patient_id = paste("ID", 1:10),
      trt = sample(c("Drug A", "Drug B"), size = 10, replace = TRUE)
    ),
    by = "patient_id"
  ) |>
  dplyr::arrange(patient_id, system_organ_class, adverse_event) |>
  dplyr::mutate(
    drug_attribution =
      sample(c("Unrelated", "Unlikely", "Possible", "Probably", "Definite"),
             dplyr::n(),
             replace = TRUE) |>
      factor(levels = c("Unrelated", "Unlikely", "Possible", "Probably", "Definite")),
    any_complication = TRUE,
    grade3_complication = grade >= 3
  ) |>
  dplyr::select(patient_id, trt, dplyr::everything()) |>
  labelled::set_variable_labels(
    patient_id = "Patient ID",
    trt = "Treatment Group",
    system_organ_class = "System Organ Class",
    adverse_event = "Adverse Event",
    grade = "Grade",
    drug_attribution = "Drug Attribution",
    any_complication = "Any Grade Complication",
    grade3_complication = "Grade 3+ Complication"
  )

df_patient_characteristics <-
  tibble::tibble(
    patient_id = paste("ID", 1:100),
    trt = sample(c("Drug A", "Drug B"), size = 100, replace = TRUE),
    age = rnorm(100, 50, 10)
  ) |>
  # forcing the patients to have the same trt as in the AE dataset
  dplyr::rows_update(
    df_adverse_events |>
      dplyr::select(patient_id, trt) |>
      dplyr::distinct(),
     by = "patient_id"
  ) |>
  dplyr::select(patient_id, trt, dplyr::everything()) |>
  labelled::set_variable_labels(
    patient_id = "Patient ID",
    trt = "Treatment Group",
    age = "Patient Age"
  )

usethis::use_data(df_adverse_events, df_patient_characteristics, overwrite = TRUE)


