test_that("df_adverse_events() works", {
  expect_error(
    df_adverse_events %>%
      tbl_adverse_events(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        strata = trt
      ),
    NA
  )

  expect_error(
    df_adverse_events %>%
      tbl_adverse_events(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade
      ),
    NA
  )

  expect_error(
    df_adverse_events %>%
      tbl_adverse_events(
        id = patient_id,
        ae = adverse_event,
        by = grade
      ),
    NA
  )

  expect_error(
    df_adverse_events %>%
      tbl_adverse_events(
        id = patient_id,
        ae = adverse_event
      ),
    NA
  )
})
