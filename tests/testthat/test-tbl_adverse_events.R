test_that("df_adverse_events() works", {
  expect_error(
    df_adverse_events %>%
      tbl_adverse_events(
        id = patient_id,
        adverse_event = adverse_event,
        soc = system_organ_class,
        grade = grade,
        strata = trt
      ),
    NA
  )
})
