test_that("add_overall() works", {
  expect_error(
    tbl1 <-
      df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        statistic = "{n}",
        header = "**Grade {level}**"
      ) %>%
      add_overall(type = 'by') %>%
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
        tbl1 %>% dplyr::select(label, stat_2_2),
        by = "label"
      ) %>%
      dplyr::mutate(check = n == stat_2_2) %>%
      dplyr::pull(check) %>%
      all()
  )



  expect_error(
    tbl2 <-
      df_adverse_events %>%
      tbl_ae(
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
      tbl_ae(
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
      tbl_ae(
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

    expect_error(
      df_adverse_events %>%
        tbl_ae(
          id = patient_id,
          ae = adverse_event,
          soc = system_organ_class,
          by = grade, strata = trt,
          header = "**Grade {level}**"
        ) %>%
        add_overall(across = 'overall-only')
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
        header = "**Grade {level}**"
      ) %>%
      add_overall(),
    "Using `type = 'by'` instead."
  )

  expect_message(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        statistic = "{n}",
        header = "**Grade {level}**"
      ) %>%
      add_overall(type = 'strata'),
    "Using `type = 'by'` instead."
  )


  expect_message(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        strata = trt,
        statistic = "{n}",
        header = "**Grade {level}**"
      ) %>%
      add_overall(type = 'by'),
    "Using `type = 'strata'` instead."
  )

  expect_message(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        strata = trt,
        statistic = "{n}",
        header = "**Grade {level}**"
      ) %>%
      add_overall(),
    "Using `type = 'strata'` instead."
  )

})



