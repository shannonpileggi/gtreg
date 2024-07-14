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
  expect_equal(levels(e1$by), c("1", "2", "3", "4", "5", "NOT OBSERVED"))
  expect_equal(levels(e2$by), c("Unknown", "1", "2", "3", "4", "5", "NOT OBSERVED"))
  expect_equal(levels(e2a$by), c("who dat", "1", "2", "3", "4", "5", "NOT OBSERVED"))

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

  # expect error messages
  expect_error(
    .complete_ae_data(
      data = df_adverse_events,
      id_df = tibble::tibble(id = letters),
      id = "patient_id",
      strata = "trt"
    )
  )

  expect_error(
    .complete_ae_data(
      data = df_adverse_events,
      id_df = tibble::tibble(patient_id = letters),
      id = "patient_id",
      strata = "trt"
    )
  )

  expect_error(
    .complete_ae_data(
      data = df_adverse_events,
      id_df = tibble::tibble(patient_id = factor(letters)),
      id = "patient_id"
    )
  )

  expect_error(
    .complete_ae_data(
      data = df_adverse_events,
      id_df = tibble::tibble(patient_id = letters, trt = factor(letters)),
      id = "patient_id",
      strata = "trt"
    )
  )

  expect_error(
    .complete_ae_data(
      data = df_adverse_events,
      id_df = tibble::tibble(patient_id = c(letters, letters)),
      id = "patient_id"
    )
  )

  expect_error(
    .complete_ae_data(
      data =
        df_adverse_events %>%
        dplyr::mutate(patient_id = ifelse(dplyr::row_number() == 1L, NA, patient_id)),
      id = "patient_id",
      strata = "trt"
    )
  )

  expect_error(
    .complete_ae_data(
      data = df_adverse_events,
      id = "patient_id",
      strata = "trt",
      by_levels = letters
    )
  )

  expect_error(
    .complete_ae_data(
      data = df_adverse_events %>% dplyr::mutate(trt = factor(trt)),
      id = "patient_id",
      strata = "trt",
      by_levels = letters
    )
  )

  expect_error(
    .complete_ae_data(
      data =
        df_adverse_events %>%
        dplyr::mutate(patient_id = ifelse(dplyr::row_number() == 1L, NA, patient_id)),
      id = "patient_id",
      ae = "adverse_event",
      strata = "trt"
    )
  )

  expect_error(
    .complete_ae_data(
      data = df_adverse_events,
      id = "patient_id",
      ae = "adverse_event",
      strata = "trt",
      missing_text = 1L
    )
  )

  expect_error(
    .complete_ae_data(
      data = df_adverse_events,
      ae = "adverse_event",
      id = "patient_id",
      id_df = tibble::tibble(patient_id = "adsf")
    )
  )

  expect_error(
    .complete_ae_data(
      data =
        df_adverse_events %>%
        dplyr::mutate(grade = ifelse(dplyr::row_number() == 1L, NA, grade)),
      ae = "adverse_event",
      id = "patient_id",
      by = "grade",
      missing_text = "1"
    )
  )

  expect_error(
    .complete_ae_data(
      data =
        df_adverse_events %>%
        dplyr::mutate(grade = ifelse(dplyr::row_number() == 1L, NA, grade)),
      ae = "adverse_event",
      id = "patient_id",
      by = "grade",
      missing_text = "6",
      by_values = as.character(1:6)
    )
  )

  expect_error(
    .complete_ae_data(
      data = df_adverse_events,
      ae = "adverse_event",
      id = "patient_id",
      by = "grade",
      by_values = as.character(5:6)
    )
  )
})


test_that("messaging for missing ae or soc", {

  # missing soc for same ae
  df1 <- tibble::tribble(
    ~patient_id,     ~trt,                    ~system_organ_class, ~adverse_event, ~grade,
    "ID 1", "Drug B",                                     NA,      "Anaemia",     1L,
    "ID 2", "Drug B", "Blood and lymphatic system disorders",      "Anaemia",     2L,
  )


  # missing soc for different ae
  df2 <- tibble::tribble(
    ~patient_id,     ~trt,               ~system_organ_class, ~adverse_event,                 ~grade,
    "ID 1", "Drug B",                                     NA, "Increased tendency to bruise",     1L,
    "ID 2", "Drug B", "Blood and lymphatic system disorders", "Anaemia",                          2L
  )

  # missing ae
  df3 <- tibble::tribble(
    ~patient_id,    ~trt,                    ~system_organ_class, ~adverse_event, ~grade,
    "ID 1", "Drug B", "Blood and lymphatic system disorders",      "Anaemia",     1L,
    "ID 2", "Drug B", "Blood and lymphatic system disorders",             NA,     2L
  )

  expect_error(
    .complete_ae_data(
    data = df1,
    ae = "adverse_event",
    soc = "system_organ_class",
    id = "patient_id",
    by = "grade"
    ),
    "At least one `soc` is missing."
  )

  expect_error(
  .complete_ae_data(
    data = df2,
    ae = "adverse_event",
    soc = "system_organ_class",
    id = "patient_id",
    by = "grade"
    ),
  "At least one `soc` is missing."
  )

  expect_error(
  .complete_ae_data(
    data = df3,
    ae = "adverse_event",
    soc = "system_organ_class",
    id = "patient_id",
    by = "grade"
    ),
  "At least one `ae` is missing."
  )

})
