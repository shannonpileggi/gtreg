test_that("multiplication works", {
  df1 <- tibble::tibble(
    patient_id = paste0("ID", c(1,1,2,3)),
    system_organ_class = "Blood and lymphatic system disorders",
    adverse_event = c("Anaemia", "Anaemia", "Anaemia", "Increased tendency to bruise"),
    grade = c(1, 1, 1, 2),
    trt = rep("A", 4)
  )

  e1 <- complete_data(
    data = df1,
    id = patient_id,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade,
    strata = trt,
    id_df = data.frame(
      patient_id = paste0("ID", 1:5),
      trt = c(rep("A", 3),rep("B", 2))
      ),
    by_values = c("Unknown", 1:3)
  )
})
