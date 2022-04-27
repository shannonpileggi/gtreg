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

test_that("tbl_listing(group_by=) works with various column types", {
  expect_error(
    tbl <- head(df_adverse_events, n = 10) %>%
      mutate(num = rep(1:2, 5)) %>%
      select(system_organ_class, num, system_organ_class, adverse_event, grade, drug_attribution, patient_id) %>%
      tbl_listing(group_by = system_organ_class, bold_headers = TRUE) %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(tbl$num[1:3], c("Blood and lymphatic system disorders", "1", "2"))

  expect_error(
    tbl <- head(df_adverse_events, n = 10) %>%
      mutate(num = rep(1:2, 5)) %>%
      select(num, grade, system_organ_class, adverse_event, drug_attribution, patient_id) %>%
      tbl_listing(group_by = num, bold_headers = TRUE) %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(tbl$num[1:3], c("Blood and lymphatic system disorders", "1", "2"))

  expect_error(
    tbl <- head(df_adverse_events, n = 10) %>%
      select(system_organ_class, adverse_event, grade, drug_attribution, patient_id) %>%
      mutate(system_organ_class = forcats::fct_relevel(system_organ_class, "Gastrointestinal disorders")) %>%
      tbl_listing(group_by = system_organ_class, bold_headers = TRUE) %>%
      as_tibble(col_labels = FALSE),
    NA
  )
  expect_equal(tbl$adverse_event[1:3], c("Gastrointestinal disorders", "Intestinal dilatation", "Intestinal dilatation"))
})
