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


test_that("counting rules", {
  ae_1 <- tibble::tribble(
    ~subject,     ~cohort, ~soc,            ~ae,             ~grade,
    "111-03-001", "A",     "Eye disorders", "Eye irritation", 1,
    "111-03-001", "A",     "Eye disorders", "Eye irritation", 2,
    "111-03-001", "A",     "Eye disorders", "Vision blurred", 2,
    "111-03-002", "A",     "Eye disorders", "Vision blurred", 2,
  )

  ae_2 <- tibble::tribble(
    ~subject,     ~cohort, ~soc,            ~ae,             ~grade,
    "111-03-001", "A",     "Eye disorders", "Eye irritation", 1,
    "111-03-001", "A",     "Eye disorders", "Eye irritation", 2,
    "111-03-001", "A",     "Eye disorders", "Vision blurred", 2,
    "111-03-002", "A",     "Eye disorders", "Vision blurred", 2,
    "111-03-002", "B",     "Gastrointestinal disorders", "Difficult digestion", 1
  )


  # this currently errors out
  # due to single soc
 #expected_ae_1 <- tbl_adverse_events(
 #  data = ae_1,
 #  id   = subject,
 #  adverse_event = ae,
 #  soc = soc,
 #  grade = grade,
 #  strata = cohort
 #)

 ## this also errors out
 #expected_ae_2 <- tbl_adverse_events(
 #  data = ae_2,
 #  id   = subject,
 #  adverse_event = ae,
 #  soc = soc,
 #  grade = grade,
 #  strata = cohort
 #)




})

