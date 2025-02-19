skip_on_cran()

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
  tbl_no_strata <-
    df_adverse_events %>%
    tbl_ae_focus(
      id = patient_id,
      include = c(any_complication, grade3_complication),
      ae = adverse_event
    )

  expect_equal(
    tbl_no_strata$table_styling$header %>% dplyr::filter(!hide) %>% select(-starts_with("modify_stat_"), -starts_with("modify_selector_")),
    tibble::tribble(
      ~column,    ~hide,   ~align, ~interpret_label,                       ~label,
      "label",    FALSE,   "left",         "gt::md",          "**Adverse Event**",
      "stat_2_1", FALSE, "center",         "gt::md", "**Any Grade Complication**",
      "stat_2_2", FALSE, "center",         "gt::md",  "**Grade 3+ Complication**"
    )
  )


  # spanning header with strata ---------------------------------------------
  tbl_w_strata <-
    df_adverse_events %>%
    tbl_ae_focus(
      id = patient_id,
      strata = trt,
      include = c(any_complication, grade3_complication),
      ae = adverse_event
    )

  expect_equal(
    tbl_w_strata$table_styling$header %>% dplyr::filter(!hide) %>% select(-starts_with("modify_stat_"), -starts_with("modify_selector_")),
    tibble::tribble(
      ~column,      ~hide,   ~align, ~interpret_label,                       ~label,
      "label",      FALSE,   "left",         "gt::md",          "**Adverse Event**",
      "stat_2_1_1", FALSE, "center",         "gt::md", "**Any Grade Complication**",
      "stat_2_1_2", FALSE, "center",         "gt::md",  "**Grade 3+ Complication**",
      "stat_2_2_1", FALSE, "center",         "gt::md", "**Any Grade Complication**",
      "stat_2_2_2", FALSE, "center",         "gt::md",  "**Grade 3+ Complication**"
    )
  )

  expect_equal(
    tbl_focs_soc <-
      df_adverse_events %>%
      tbl_ae_focus(
        id = patient_id,
        id_df = df_patient_characteristics,
        ae = adverse_event,
        soc = system_organ_class,
        include = c(any_complication, grade3_complication)
      ) %>%
      gtsummary::modify_table_body(
        ~dplyr::filter(.x, variable == "soc")
      ) %>%
      as_tibble(col_labels = FALSE),
    tibble::tribble(
      ~label, ~stat_2_1, ~stat_2_2,
      "Blood and lymphatic system disorders", "10 (10)", "9 (9.0)",
      "Gastrointestinal disorders", "10 (10)", "10 (10)"
    )
  )

  expect_equal(
    tbl_focs_soc %>%
      dplyr::select(label, stat_2_2),
    df_adverse_events %>%
      dplyr::select(patient_id, system_organ_class, grade3_complication) %>%
      dplyr::group_by(system_organ_class, patient_id) %>%
      dplyr::mutate(grade3_complication = max(grade3_complication)) %>%
      dplyr::distinct() %>%
      dplyr::full_join(
        df_patient_characteristics %>%
          dplyr::select(patient_id) %>%
          dplyr::mutate(system_organ_class = list(unique(df_adverse_events$system_organ_class))) %>%
          tidyr::unnest(system_organ_class),
        by = c("patient_id", "system_organ_class")
      ) %>%
      dplyr::mutate(grade3_complication = ifelse(is.na(grade3_complication), 0, grade3_complication)) %>%
      dplyr::group_by(system_organ_class) %>%
      dplyr::summarise(
        N = dplyr::n(),
        n = sum(grade3_complication),
        p = mean(grade3_complication)
      ) %>%
      dplyr::mutate(
        stat_2_2 = stringr::str_glue("{n} ({gtsummary::style_sigfig(p, scale = 100)})") %>% as.character()
      ) %>%
      dplyr::select(label = system_organ_class, stat_2_2),
    ignore_attr = TRUE
  )

  # test that the columns still appear when no TRUE values are observed.
  expect_equal(
    df_adverse_events %>%
      dplyr::mutate(
        all_false = FALSE
      ) %>%
      tbl_ae_focus(
        include = all_false,
        id = patient_id,
        ae = adverse_event
      ) %>%
      as_tibble(fmt_missing = FALSE, col_labels = FALSE) %>%
      dplyr::pull(2) %>%
      unique(),
    NA_character_
  )

  # test to ensure formatting missing value cells are added to AEs, when no missing are present in SOC
  expect_equal(
    df_adverse_events %>%
      dplyr::mutate(
        grade = ifelse(grade == 5 & adverse_event == "Anaemia", 4L, grade),
        grade5_complication = grade == 5) %>%
      tbl_ae_focus(
        include = grade5_complication,
        id = patient_id,
        id_df = df_patient_characteristics,
        ae = adverse_event,
        soc = system_organ_class
      ) %>%
      purrr::pluck("table_styling", "fmt_missing") %>%
      dplyr::select(column, symbol),
    structure(list(column = c("stat_1_1", "stat_2_1", "stat_3_1"),
                   symbol = c("—", "—", "—")), row.names = c(NA, -3L), class = c("tbl_df",
                                                                                 "tbl", "data.frame"))
  )

})
