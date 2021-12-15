test_that("multiplication works", {
  df1 <-
    tibble::tibble(
      patient_id = paste0("ID", c(1,1,2,3)),
      system_organ_class = "Blood and lymphatic system disorders",
      adverse_event = c("Anaemia", "Anaemia", "Anaemia", "Increased tendency to bruise"),
      grade = c(1, 1, 1, 2),
      trt = rep("A", 4)
    )

  expect_error(
    e1 <-
      .complete_ae_data(
        data = df1,
        id = "patient_id",
        ae = "adverse_event",
        soc = "system_organ_class",
        by = "grade",
        strata = "trt",
        id_df =
          data.frame(
            patient_id = paste0("ID", 1:5),
            trt = c(rep("A", 3),rep("B", 2))
          ),
        by_values = as.character(c(1:5))
      ),
    NA
  )

  expect_equal(dim(e1), c(11, 7))
  # expect_equal(as.character(e1$by), c("1", "1", "2", "dummy", "dummy"))

  # no errors when soc, by, and strata are not specified
  expect_error(
    .complete_ae_data(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      by = "grade",
      strata = "trt"
    ),
    NA
  )
  expect_error(
    .complete_ae_data(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt"
    ),
    NA
  )
  expect_error(
    .complete_ae_data(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      strata = "trt"
    ),
    NA
  )
  expect_error(
    .complete_ae_data(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade"
    ),
    NA
  )
  expect_error(
      .complete_ae_data(
        data = df1,
        id = "patient_id",
        ae = "adverse_event"
      ),
    NA
  )
})
