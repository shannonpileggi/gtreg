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
    df_adverse_events %>%
      dplyr::mutate(grade = factor(grade, levels = 1:5)) %>%
      dplyr::filter(adverse_event %in% "Anaemia") %>%
      with(table(trt, grade)) %>%
      t() %>%
      matrix(nrow = 1) %>%
      c() %>%
      as.character()
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
