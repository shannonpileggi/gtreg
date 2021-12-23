test_that("multiplication works", {
  expect_error(
    tbl1 <-
      df_adverse_events %>%
      tbl_adverse_event(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        header = "**Grade {level}**"
      ) %>%
      add_overall() %>%
      as_tibble(col_label = FALSE),
    NA
  )

  expect_equal(
    tbl1$stat_2_2,
    c('10 (100)', '7 (70)', '8 (80)', '7 (70)', '7 (70)', '10 (100)',
      '8 (80)', '6 (60)', '7 (70)', '6 (60)', '7 (70)')
  )

  expect_error(
    tbl2 <-
      df_adverse_events %>%
      tbl_adverse_event(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        strata = trt,
        header = "**Grade {level}**"
      ) %>%
      add_overall(by = TRUE) %>%
      as_tibble(col_label = FALSE),
    NA
  )

  expect_error(
    tbl3 <-
      df_adverse_events %>%
      tbl_adverse_event(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        strata = trt,
        header = "**Grade {level}**"
      ) %>%
      add_overall(strata = TRUE) %>%
      as_tibble(col_label = FALSE),
    NA
  )
  
    expect_error(
    tbl1 <-
      df_adverse_events %>%
      tbl_adverse_event(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        strata = trt,
        header = "**Grade {level}**"
      ) %>%
      add_overall() %>%
      as_tibble(col_label = FALSE),
    NA
  )
  
})
