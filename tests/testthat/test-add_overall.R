test_that("add_overall() works", {
  expect_error(
    tbl1 <-
      df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        by = grade,
        statistic = "{n}",
        header_by = "**Grade {level}**"
      ) %>%
      add_overall(across = 'by') %>%
      as_tibble(col_label = FALSE),
    NA
  )

  expect_true(
    df_adverse_events %>%
      dplyr::select(patient_id, adverse_event) %>%
      dplyr::distinct() %>%
      dplyr::group_by(adverse_event) %>%
      dplyr::mutate(
        n = dplyr::n() %>% as.character()
      ) %>%
      dplyr::select(label = adverse_event, n) %>%
      dplyr::distinct() %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(
        tbl1 %>% dplyr::select(label, stat_1_2),
        by = "label"
      ) %>%
      dplyr::mutate(check = n == stat_1_2) %>%
      dplyr::pull(check) %>%
      all()
  )

  expect_error(
    tbl1 <-
      df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        strata = trt,
        header_by = "**Grade {level}**"
      ) %>%
      add_overall() %>%
      as_tibble(col_label = FALSE),
    NA
  )

  expect_error(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        strata = trt,
        header_by = "**Grade {level}**"
      ) %>%
      add_overall(across = 'strata'),
    NA
  )

  expect_error(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        strata = trt,
        header_by = "**Grade {level}**"
      ) %>%
      add_overall(across = 'overall-only'),
    NA
  )
})


test_that("add_overall() warns", {

  expect_message(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        statistic = "{n}",
        header_by = "**Grade {level}**"
      ) %>%
      add_overall(),
    "Using `across = 'by'` instead."
  )

  expect_message(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        statistic = "{n}",
        header_by = "**Grade {level}**"
      ) %>%
      add_overall(across = 'strata'),
    "Using `across = 'by'` instead."
  )


  expect_message(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        strata = trt,
        statistic = "{n}"
      ) %>%
      add_overall(across = 'by'),
    "Using `across = 'strata'` instead."
  )

  expect_message(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        strata = trt,
        statistic = "{n}"
      ) %>%
      add_overall(),
    "Using `across = 'strata'` instead."
  )

})

test_that("add_overall(missing_location=) works", {
  expect_error(
    tbl <-
      df_adverse_events %>%
      dplyr::mutate(
        grade = ifelse(dplyr::row_number() == 1, NA, grade)
      ) %>%
      tbl_ae(
        ae = adverse_event,
        id = patient_id,
        soc = system_organ_class,
        by = grade,
        strata = trt,
        header_by = "**Grade {level}**",
        missing_location = "first"
      ) %>%
      add_overall(),
    NA
  )
  expect_equal(
    tbl$table_styling$header %>% dplyr::filter(!hide) %>% dplyr::pull(label),
    c("**Adverse Event**", "**Grade Unknown**", "**Grade 1**", "**Grade 2**", "**Grade 3**",
      "**Grade 4**", "**Grade 5**", "**Overall**", "**Grade Unknown**", "**Grade 1**", "**Grade 2**",
      "**Grade 3**", "**Grade 4**", "**Grade 5**", "**Overall**", "**Grade Unknown**", "**Grade 1**", "**Grade 2**",
      "**Grade 3**", "**Grade 4**", "**Grade 5**", "**Overall**")
  )

  expect_error(
    tbl <-
      df_adverse_events %>%
      dplyr::mutate(
        grade = ifelse(dplyr::row_number() == 1, NA, grade)
      ) %>%
      tbl_ae(
        ae = adverse_event,
        id = patient_id,
        soc = system_organ_class,
        by = grade,
        strata = trt,
        header_by = "**Grade {level}**",
        missing_location = "last"
      ) %>%
      add_overall(),
    NA
  )
  expect_equal(
    tbl$table_styling$header %>% dplyr::filter(!hide) %>% dplyr::pull(label),
    c("**Adverse Event**", "**Grade 1**", "**Grade 2**", "**Grade 3**",
      "**Grade 4**", "**Grade 5**", "**Grade Unknown**", "**Overall**", "**Grade 1**", "**Grade 2**",
      "**Grade 3**", "**Grade 4**", "**Grade 5**", "**Grade Unknown**", "**Overall**", "**Grade 1**", "**Grade 2**",
      "**Grade 3**", "**Grade 4**", "**Grade 5**", "**Grade Unknown**", "**Overall**")
  )
})

