test_that("assess complete data single arm, single soc", {

  # no factor inputs
  df1 <-
    tibble::tibble(
      patient_id = paste0("ID", c(1,1,2,3)),
      system_organ_class = "Blood and lymphatic system disorders",
      adverse_event = c("Anaemia", "Anaemia", "Anaemia", "Increased tendency to bruise"),
      grade = c(1, 1, 1, 2),
      trt = rep("A", 4)
    )

  # introduce missing grade
  df2 <-
    tibble::tibble(
      patient_id = paste0("ID", c(1,1,2,3)),
      system_organ_class = "Blood and lymphatic system disorders",
      adverse_event = c("Anaemia", "Anaemia", "Anaemia", "Increased tendency to bruise"),
      grade = c(1, 1, NA, 2),
      trt = rep("A", 4)
    )

   # patient id submitted as factor
   df3 <-
    tibble::tibble(
      patient_id = factor(paste0("ID", c(1,1,2,3))),
      system_organ_class = "Blood and lymphatic system disorders",
      adverse_event = c("Anaemia", "Anaemia", "Anaemia", "Increased tendency to bruise"),
      grade = c(1, 1, 1, 2),
      trt = rep("A", 4)
    )

   # strata submitted as factor
   df4 <-
     tibble::tibble(
       patient_id = paste0("ID", c(1,1,2,3)),
       system_organ_class = "Blood and lymphatic system disorders",
       adverse_event = c("Anaemia", "Anaemia", "Anaemia", "Increased tendency to bruise"),
       grade = c(1, 1, 1, 2),
       trt = factor(rep("A", 4))
     )


  id_valid <-
    tibble::tibble(
      patient_id = paste0("ID", 1:5),
      trt = c(rep("A", 3),rep("B", 2))
    )


  e1 <-
    .complete_ae_data(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt",
      id_df = id_valid,
      by_values = as.character(c(1:5))
    )


  e2 <-
    .complete_ae_data(
      data = df2,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt",
      id_df = id_valid,
      by_values = as.character(c(1:5))
    )

  e2a <-
    .complete_ae_data(
      data = df2,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt",
      id_df = id_valid,
      by_values = as.character(c(1:5)),
      missing_text = "who dat"
    )

  e3 <-
    .complete_ae_data(
      data = df3,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt",
      by_values = as.character(c(1:5))
    )


  e4 <-
    .complete_ae_data(
      data = df4,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt",
      by_values = as.character(c(1:5))
    )


  #expect_equal(dim(e1), c(11, 7))
  expect_equal(levels(e1$by), c("NOT OBSERVED", "1", "2", "3", "4", "5"))
  expect_equal(levels(e2$by), c("NOT OBSERVED", "Unknown", "1", "2", "3", "4", "5"))
  expect_equal(levels(e2a$by), c("NOT OBSERVED", "who dat", "1", "2", "3", "4", "5"))

  # specifications that should result in an error ------------------------------
  # patient id variable name does not match
  expect_error(
    .complete_ae_data(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt",
      id_df = data.frame(
        patientid = paste0("ID", 1:5),
        trt = c(rep("A", 3),rep("B", 2))
      ),
      by_values = as.character(c(1:5))
    )
  )

  # patient id variable type does not match: id character, id_df factor
  expect_error(
    .complete_ae_data(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt",
      id_df = data.frame(
        patient_id = factor(paste0("ID", 1:5)),
        trt = c(rep("A", 3),rep("B", 2))
      ),
      by_values = as.character(c(1:5))
    )
  )

  # patient id variable type does not match: id factor, id_df character
  expect_error(
    .complete_ae_data(
      data = df3,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt",
      id_df = id_valid,
      by_values = as.character(c(1:5))
    )
  )

  # strata variable name does not match
  expect_error(
    .complete_ae_data(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt",
      id_df = data.frame(
        patient_id = paste0("ID", 1:5),
        arm = c(rep("A", 3),rep("B", 2))
      ),
      by_values = as.character(c(1:5))
    )
  )

  # strata type does not match; input strata is charater, id_df strata is factor
  expect_error(
    .complete_ae_data(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt",
      id_df = data.frame(
        patient_id = paste0("ID", 1:5),
        trt = factor(c(rep("A", 3),rep("B", 2)))
      ),
      by_values = as.character(c(1:5))
    )
  )


  # specifications that should not result in an error --------------------------
  expect_error(e1, NA)
  expect_error(e2, NA)
  expect_error(e2a, NA)
  expect_error(e3, NA)

  # similar to e1, but without id_df
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


 # no errors when soc, by, and strata are not specified ------------------------

  # no soc
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

  # no by
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

  # no strata
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

  # no by, strata, or soc
  expect_error(
    .complete_ae_data(
      data = df1,
      id = "patient_id",
      ae = "adverse_event"
    ),
    NA
  )
})
