test_that("tbl_listing() works", {
  expect_error(
    tbl <-
      head(df_adverse_events, n = 10) %>%
      select(system_organ_class, adverse_event, grade, drug_attribution, patient_id) %>%
      arrange(adverse_event, desc(grade)) %>%
      tbl_listing() %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(
    tbl,
    head(df_adverse_events, n = 10) %>%
      select(system_organ_class, adverse_event, grade, drug_attribution, patient_id) %>%
      arrange(adverse_event, desc(grade))
  )
  # labels are correctly applied to tbl
  expect_equal(
    head(df_adverse_events, n = 10) %>%
      select(system_organ_class, adverse_event, grade, drug_attribution, patient_id) %>%
      arrange(adverse_event, desc(grade)) %>%
      tbl_listing() %>%
      as_tibble(col_labels = TRUE) %>%
      names(),
    c("**System Organ Class**", "**Adverse Event**", "**Grade**", "**Drug Attribution**", "**Patient ID**")
  )

  # check the two additional grouping rows are added to tbl
  expect_equal(
    head(df_adverse_events, n = 10) %>%
      select(system_organ_class, adverse_event, grade, drug_attribution, patient_id) %>%
      arrange(adverse_event, desc(grade)) %>%
      tbl_listing(group_by = system_organ_class) %>%
      as_tibble(col_labels = FALSE) %>%
      nrow(),
    12
  )
})
