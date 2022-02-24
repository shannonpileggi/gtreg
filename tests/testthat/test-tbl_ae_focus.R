
test_that("tbl_ae_focus() works", {
  expect_error(
    tbl1 <-
      df_adverse_events %>%
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
    df_adverse_events %>%
      dplyr::filter(system_organ_class %in% "Blood and lymphatic system disorders") %>%
      dplyr::select(patient_id) %>%
      dplyr::distinct() %>%
      nrow() %>%
      as.character()
  )
  expect_equal(
    as_tibble(tbl1, col_labels = FALSE) %>% dplyr::slice(2) %>% dplyr::pull(2),
    df_adverse_events %>%
      dplyr::filter(adverse_event %in% "Anaemia") %>%
      dplyr::select(patient_id) %>%
      dplyr::distinct() %>%
      nrow() %>%
      as.character()
  )

  expect_error(
    tbl2 <-
      df_adverse_events %>%
      tbl_ae_focus(
        include = c(any_complication, grade3_complication),
        id = patient_id,
        ae = adverse_event,
        digits = 1
      ),
    NA
  )
  expect_equal(
    as_tibble(tbl2, col_labels = FALSE)$stat_2_1[1:2],
    c("7.0 (70.0)", "5.0 (50.0)")
  )

  expect_error(
    df_adverse_events %>%
      tbl_ae_focus(
        include = c(any_complication, system_organ_class),
        id = patient_id,
        ae = adverse_event
      )
  )


  expect_error(
    tbl_ae_focus(
      data = letters,
      include = c(any_complication, grade3_complication),
      id = patient_id,
      ae = adverse_event,
      soc = system_organ_class
    )
  )

  expect_error(
    df_adverse_events %>%
      dplyr::rename(by = any_complication) %>%
      tbl_ae_focus(
        include = by,
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class
      )
  )

  expect_error(
    tbl_ae_focus(
      data = df_adverse_events,
      include = c(any_complication, grade3_complication),
      ae = adverse_event,
      soc = system_organ_class
    )
  )

  # checking returned percentages and labels
  tbl <-
    df_adverse_events %>%
    dplyr::mutate(
      dlt = ifelse(dplyr::row_number() == 1L, TRUE, FALSE)
    ) %>%
    tbl_ae_focus(
      id = patient_id,
      id_df = df_patient_characteristics,
      ae = adverse_event,
      include = dlt,
      label = list(dlt = "DLT"),
      zero_symbol = NULL
    ) %>%
    as_tibble()

  expect_equal(
    tbl %>% dplyr::pull(2),
    c("1 (1.0)", "0 (0)", "0 (0)", "0 (0)", "0 (0)", "0 (0)", "0 (0)", "0 (0)", "0 (0)")
  )
  expect_equal(
    names(tbl),
    c("**Adverse Event**", "**DLT**")
  )

  # include vars cannot be NA
  expect_error(
    df_adverse_events %>%
      dplyr::mutate(
        any_complication = ifelse(dplyr::row_number() == 1L, NA, any_complication)
      ) %>%
      tbl_ae_focus(
        id = patient_id,
        include = c(any_complication, grade3_complication),
        ae = adverse_event,
        soc = system_organ_class
      )
  )

  # spanning header without strata ---------------------------------------------
  tbl_no_strata <- df_adverse_events %>%
    tbl_ae_focus(
      id = patient_id,
      include = c(any_complication, grade3_complication),
      ae = adverse_event
    )

  expect_equal(
    tbl_no_strata$table_styling$header %>% dplyr::filter(!hide),
    tibble::tribble(
      ~column, ~hide,   ~align, ~interpret_label,                       ~label, ~interpret_spanning_header, ~spanning_header,
      "label", FALSE,   "left",         "gt::md",          "**Adverse Event**",                   "gt::md",               NA,
      "stat_1_1", FALSE, "center",         "gt::md", "**Any Grade Complication**",                "gt::md",         "**N = 10**",
      "stat_1_2", FALSE, "center",         "gt::md",  "**Grade 3+ Complication**",                "gt::md",         "**N = 10**"
    )
  )


  # spanning header with strata ---------------------------------------------
  tbl_w_strata <- df_adverse_events %>%
    tbl_ae_focus(
      id = patient_id,
      strata = trt,
      include = c(any_complication, grade3_complication),
      ae = adverse_event
    )

  expect_equal(
    tbl_w_strata$table_styling$header %>% dplyr::filter(!hide),
    tibble::tribble(
      ~column, ~hide,   ~align, ~interpret_label,                       ~label, ~interpret_spanning_header,    ~spanning_header,
      "label", FALSE,   "left",         "gt::md",          "**Adverse Event**",                   "gt::md",                  NA,
      "stat_1_1_1", FALSE, "center",         "gt::md", "**Any Grade Complication**",                   "gt::md", "**Drug A**, N = 3",
      "stat_1_1_2", FALSE, "center",         "gt::md",  "**Grade 3+ Complication**",                   "gt::md", "**Drug A**, N = 3",
      "stat_1_2_1", FALSE, "center",         "gt::md", "**Any Grade Complication**",                   "gt::md", "**Drug B**, N = 7",
      "stat_1_2_2", FALSE, "center",         "gt::md",  "**Grade 3+ Complication**",                   "gt::md", "**Drug B**, N = 7"
    )
  )

})
