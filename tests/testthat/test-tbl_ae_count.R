test_that("tbl_ae_count() works", {
  expect_error(
    tbl1 <-
      tbl_ae_count(
        data = df_adverse_events,
        ae = adverse_event,
        soc = system_organ_class,
        strata = trt,
        by = grade,
        header_by = "**Grade {level}**",
        zero_symbol = NULL
      ),
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
})
