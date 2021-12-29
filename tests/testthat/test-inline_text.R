test_that("multiplication works", {
  expect_equal(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        by = grade,
        header = "**Grade {level}**"
      ) %>%
      inline_text(ae_or_soc = "Anaemia", column = stat_6),
    "3 (30)"
  )
})
