test_that("inline text works", {
  expect_equal(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        by = grade,
        header_by = "**Grade {level}**"
      ) %>%
      inline_text(row = "Anaemia", column = stat_6),
    "3 (30)"
  )

  expect_equal(
    df_adverse_events %>%
      tbl_ae_count(
        ae = adverse_event,
        by = grade,
        header_by = "**Grade {level}**"
      ) %>%
      inline_text(row = "Anaemia", column = stat_5),
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
        by = grade,
        header_by = "**Grade {level}**"
      ) %>%
      inline_text(row = "Blood and lymphatic system disorders", column = stat_6),
    "7 (70)"
  )
})
