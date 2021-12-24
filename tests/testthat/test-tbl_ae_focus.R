
df_adverse_events_binary <-
  df_adverse_events %>%
  dplyr::mutate(
    any_complication = TRUE,
    grade3_complication = grade >= 3
  )

test_that("tbl_ae_focus() works", {
  expect_error(
    tbl1 <-
      df_adverse_events_binary %>%
      tbl_ae_focus(
        include = c(any_complication, grade3_complication),
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        statistic = "{n}",
        label =
          list(
            any_complication = "Any Grade Complication",
            grade3_complication = "Grade 3+ Complication"
          )
      ),
    NA
  )

  expect_equal(
    as_tibble(tbl1) %>% colnames(),
    c("**Adverse Event**", "**Any Grade Complication**", "**Grade 3+ Complication**")
  )
  expect_equal(
    as_tibble(tbl1, col_labels = FALSE) %>% dplyr::slice(1) %>% dplyr::pull(2),
    df_adverse_events_binary %>%
      dplyr::filter(system_organ_class %in% "Blood and lymphatic system disorders") %>%
      dplyr::select(patient_id) %>%
      dplyr::distinct() %>%
      nrow() %>%
      as.character()
  )
  expect_equal(
    as_tibble(tbl1, col_labels = FALSE) %>% dplyr::slice(2) %>% dplyr::pull(2),
    df_adverse_events_binary %>%
      dplyr::filter(adverse_event %in% "Anaemia") %>%
      dplyr::select(patient_id) %>%
      dplyr::distinct() %>%
      nrow() %>%
      as.character()
  )

  expect_error(
    tbl2 <-
      df_adverse_events_binary %>%
      tbl_ae_focus(
        include = c(any_complication, grade3_complication),
        id = patient_id,
        ae = adverse_event
      ),
    NA
  )

  expect_error(
    df_adverse_events_binary %>%
      tbl_ae_focus(
        include = c(any_complication, system_organ_class),
        id = patient_id,
        ae = adverse_event
      )
  )
})
