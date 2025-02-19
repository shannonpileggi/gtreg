skip_on_cran()

dat <- tibble::tribble(
  ~subject, ~visit,  ~soc, ~ae, ~grade,
  # Subject 1 ----------------------------------------------------------------
  "001", 1, "Eye disorders", "Eye irritation", 1,
  "001", 1, "Gastrointestinal disorders", "Difficult digestion", 1,
  "001", 2, "Eye disorders", "Eye irritation", 1,
  "001", 3, "Eye disorders", "Eye irritation", 2,
  "001", 4, "Eye disorders", "Vision blurred", 2,
  # Subject 2 ----------------------------------------------------------------
  "002", 1, "Gastrointestinal disorders", "Difficult digestion", 2,
  "002", 1, "Gastrointestinal disorders", "Reflux", 2,
  "002", 2, "Eye disorders", "Vision blurred", 2,
  "002", 2, "Gastrointestinal disorders", "Reflux", 2,
  "002", 3, "Gastrointestinal disorders", "Reflux", NA
)

dat_unobserved_stratum <-
  tibble::tribble(
    ~id, ~cohort,                                                   ~soc,                                    ~pt, ~valxx,
    1,      "C1", "General disorders and administration site conditions",                              "Fatigue",    "1",
    1,      "C1",                                   "Vascular disorders",                            "Hot flush",    "1",
    1,      "C1",                                       "Investigations",                     "Weight increased",    "2",
    3,      "C2",                                       "Investigations",   "Alanine aminotransferase increased",    "1",
    3,      "C2",                                       "Investigations", "Aspartate aminotransferase increased",    "1",
    3,      "C2",                             "Nervous system disorders",                   "Cognitive disorder",    "1",
    16,     "C3",      "Musculoskeletal and connective tissue disorders",                           "Arthralgia",    "1",
    16,     "C3",                             "Nervous system disorders",                   "Cognitive disorder",    "1",
    16,     "C3", "General disorders and administration site conditions",                              "Fatigue",    "1",
    6,      "C4",                           "Gastrointestinal disorders",                            "Diarrhoea",    "2",
    6,      "C4",                             "Nervous system disorders",                            "Dysgeusia",    "1",
    6,      "C4",                                   "Vascular disorders",                            "Hot flush",    "1"
  )

test_that("tbl_ae_count() works", {
  expect_error(
    tbl1 <-
      tbl_ae_count(
        data = df_adverse_events,
        ae = adverse_event,
        soc = system_organ_class,
        strata = trt,
        by = grade,
        zero_symbol = NULL
      ) %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**"),
    NA
  )

  expect_equal(
    tbl1$table_body %>%
      filter(label %in% "Blood and lymphatic system disorders") %>%
      dplyr::select(gtsummary::all_stat_cols()) %>%
      c() %>%
      unname() %>%
      unlist(),
    df_adverse_events %>%
      dplyr::filter(system_organ_class %in% "Blood and lymphatic system disorders") %>%
      with(table(trt, grade)) %>%
      t() %>%
      matrix(nrow = 1) %>%
      c() %>%
      as.character()
  )

  expect_equal(
    tbl1$table_body %>%
      dplyr::filter(label %in% "Anaemia") %>%
      dplyr::select(gtsummary::all_stat_cols()) %>%
      c() %>%
      unname() %>%
      unlist(),
    c("0", "0", "1", "1", "0", "1", "0", "2", "2", "3")
  )

  expect_error(
    tbl_ae_count(
      data = df_adverse_events,
      ae = adverse_event,
      soc = system_organ_class,
      strata = trt
    ),
    NA
  )
  expect_error(
    tbl_ae_count(
      data = df_adverse_events,
      ae = adverse_event,
      soc = system_organ_class
    ),
    NA
  )
  expect_error(
    tbl <-
      tbl_ae_count(
        data = df_adverse_events,
        ae = adverse_event,
        digits = function(x) style_number(x, digits = 2)
      ),
    NA
  )
  expect_equal(as_tibble(tbl, col_labels = FALSE)$stat_1[1:3],
               c("10.00", "5.00", "9.00"))

  # bad call to missing_location= ----------------------------------------------
  expect_error(
    df_adverse_events %>%
      tbl_ae_count(
        ae = adverse_event,
        by = grade,
        missing_location = "NOPE"
      )
  )

  expect_equal(
    dat %>%
      tbl_ae_count(
        ae = ae,
        soc = soc,
        by = grade,
        missing_location = "hide"
      ) %>%
      as_tibble() %>%
      names(),
    c("**Adverse Event**", "**1**", "**2**")
  )

  expect_equal(
    dat %>%
      tbl_ae_count(
        ae = ae,
        soc = soc,
        by = grade,
        missing_location = "first"
      ) %>%
      as_tibble() %>%
      names(),
    c("**Adverse Event**", "**Unknown**", "**1**", "**2**")
  )

  expect_equal(
    dat %>%
      tbl_ae_count(
        ae = ae,
        soc = soc,
        by = grade,
        missing_location = "last"
      ) %>%
      as_tibble() %>%
      names(),
    c("**Adverse Event**", "**1**", "**2**", "**Unknown**")
  )

  # error messaging with by levels ---------------------------------------------
  expect_error(
    dat %>%
      dplyr::mutate(
        grade = ifelse(dplyr::row_number() == 1L, "Unknown", grade)
      ) %>%
      tbl_ae_count(
        ae = ae,
        soc = soc,
        by = grade
      )
  )
  expect_error( # no error when no NA present
    dat %>%
      tidyr::drop_na() %>%
      dplyr::mutate(
        grade = ifelse(dplyr::row_number() == 1L, "Unknown", grade)
      ) %>%
      tbl_ae_count(
        ae = ae,
        soc = soc,
        by = grade
      ),
    NA
  )


  expect_error(
    dat %>%
      tbl_ae_count(
        ae = ae,
        soc = soc,
        by = grade,
        by_values = c("Unknown", 1:5)
      )
  )
  expect_error( # no error when no NA present
    dat %>%
      tidyr::drop_na() %>%
      tbl_ae_count(
        ae = ae,
        soc = soc,
        by = grade,
        by_values = c("Unknown", 1:5)
      ),
    NA
  )
})

test_that("tbl_ae_count() works with unobserved data in stratum", {
  expect_error(
    tbl_unobserved_stratum <-
      dat_unobserved_stratum %>%
      tbl_ae_count(
        ae = pt,
        soc = soc,
        by = valxx,
        strata = cohort
      ),
    NA
  )

  # tabulating counts to compare against results from tbl_ae_count()
  df_count_checks_ae <-
    dat_unobserved_stratum %>%
    mutate(strata = paste(cohort, valxx)) %>%
    dplyr::mutate(across(c(soc, pt), factor)) %>%
    dplyr::count(soc, pt, strata) %>%
    select(label = pt, strata, n)

  df_count_checks_soc <-
    dat_unobserved_stratum %>%
    mutate(strata = paste(cohort, valxx)) %>%
    dplyr::count(soc, strata) %>%
    select(label = soc, strata, n)

  stat_col_rename <-
    tbl_unobserved_stratum$table_styling$header %>%
    dplyr::filter(!hide, startsWith(column, "stat_")) %>%
    dplyr::mutate_all(
      ~stringr::str_replace_all(., pattern = "\\*\\*(.*?)\\*\\*", replacement = "\\1")
    ) %>%
    dplyr::left_join(tbl_unobserved_stratum$table_styling$spanning_header[c("column", "spanning_header")], by = "column") %>%
    mutate(col_name = paste(spanning_header, label)) %>%
    select(col_name, column) %>%
    tibble::deframe()

  tbl_counts <-
    tbl_unobserved_stratum %>%
    gtsummary::modify_column_unhide(variable) %>%
    as_tibble(col_labels = FALSE) %>%
    dplyr::rename(!!!stat_col_rename) %>%
    tidyr::pivot_longer(cols = -c(variable, label)) %>%
    mutate(value = suppressWarnings(as.integer(value))) %>%
    tidyr::drop_na() %>%
    select(label, strata = name, n = value) %>%
    arrange(label, strata, n)

  expect_equal(
    dplyr::bind_rows(df_count_checks_soc, df_count_checks_ae) %>%
      arrange(label, strata, n),
    tbl_counts %>%
      mutate(strata = stringr::str_remove_all(strata, pattern = stringr::fixed("*")))
  )
})
