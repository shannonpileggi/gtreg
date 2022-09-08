skip_on_cran()

test_that("add_overall() works", {
  expect_error(
    tbl1 <-
      df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        by = grade,
        statistic = "{n}"
      ) %>%
      add_overall(across = 'by') %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**"),
    NA
  )
  expect_snapshot(
    tbl1 %>%
      as_gt() %>%
      gt::as_raw_html()
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
        tbl1 %>%
          as_tibble(col_label = FALSE) %>%
          dplyr::select(label, stat_1_2),
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
        strata = trt
      ) %>%
      add_overall() %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**"),
    NA
  )

  expect_snapshot(
    tbl1 %>%
      as_gt() %>%
      gt::as_raw_html()
  )

  expect_error(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        strata = trt
      ) %>%
      add_overall(across = 'strata') %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**"),
    NA
  )

  expect_error(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        strata = trt
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
        statistic = "{n}"
      ) %>%
      add_overall(across = "both") %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**"),
    "Using `across = 'by'` instead."
  )

  expect_message(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        statistic = "{n}"
      ) %>%
      add_overall(across = 'strata') %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**"),
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
      add_overall(across = "both"),
    "Using `across = 'strata'` instead."
  )

})

test_that("no errors with `by_values=`", {
  tbl <-
    df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      soc = system_organ_class,
      by = grade,
      by_values = as.character(0:5),
      strata = trt
    )

  expect_error(add_overall(tbl, across = "both"), NA)
  expect_error(add_overall(tbl, across = "by"), NA)
  expect_error(add_overall(tbl, across = "overall-only"), NA)
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
        missing_location = "first"
      ) %>%
      add_overall() %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**"),
    NA
  )
  expect_equal(
    tbl$table_styling$header %>% dplyr::filter(!hide) %>% dplyr::pull(label),
    c("**Adverse Event**", "**Unknown**", "**Grade 1**", "**Grade 2**", "**Grade 3**",
      "**Grade 4**", "**Grade 5**", "**Overall**", "**Unknown**", "**Grade 1**", "**Grade 2**",
      "**Grade 3**", "**Grade 4**", "**Grade 5**","**Overall**", "**Unknown**", "**Grade 1**", "**Grade 2**",
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
        missing_location = "last"
      ) %>%
      add_overall() %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**"),
    NA
  )
  expect_equal(
    tbl$table_styling$header %>% dplyr::filter(!hide) %>% dplyr::pull(label),
    c("**Adverse Event**", "**Grade 1**", "**Grade 2**", "**Grade 3**",
      "**Grade 4**", "**Grade 5**", "**Unknown**", "**Overall**", "**Grade 1**", "**Grade 2**",
      "**Grade 3**", "**Grade 4**", "**Grade 5**", "**Unknown**", "**Overall**", "**Grade 1**", "**Grade 2**",
      "**Grade 3**", "**Grade 4**", "**Grade 5**", "**Unknown**", "**Overall**")
  )
})

