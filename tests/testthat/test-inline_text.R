test_that("inline text works", {
  expect_equal(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        by = grade
      ) %>%
      inline_text(row = "Anaemia", column = stat_5),
    "3 (30)"
  )

  expect_equal(
    df_adverse_events %>%
      tbl_ae_count(
        ae = adverse_event,
        by = grade
      ) %>%
      inline_text(row = "Anaemia", column = stat_4),
    "3"
  )

  expect_equal(
    df_adverse_events %>%
      tbl_ae_focus(
        id = patient_id,
        ae = adverse_event,
        include = grade3_complication
      ) %>%
      inline_text(row = "Anaemia", column = stat_2_1),
    "7 (70)"
  )

  expect_equal(
    df_adverse_events %>%
      tbl_ae_focus(
        id = patient_id,
        ae = adverse_event,
        include = grade3_complication
      ) %>%
      inline_text(row = "Anaemia"),
    NULL
  )

  expect_equal(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        soc = system_organ_class,
        ae = adverse_event,
        by = grade
      ) %>%
      inline_text(row = "Blood and lymphatic system disorders", column = stat_5),
    "7 (70)"
  )

  tbl <-
    df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      soc = system_organ_class,
      ae = adverse_event,
      by = grade
    )

  expect_error(inline_text(tbl, row = 1L, column = stat_1))
  expect_error(inline_text(tbl, row = "not an AE", column = stat_1))


})
