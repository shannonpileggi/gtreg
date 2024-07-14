test_that("style_xxx works", {
  expect_error(
    style_xxx(7:10, width = 2, digits = 0),
    NA)

  expect_equal(
    style_xxx(7:10, width = 2, digits = 0),
    c("xx", "xx", "xx", "xx")
  )

  expect_equal(
    style_xxx(7:10, width = 5, digits = 2),
    c("xx.xx", "xx.xx", "xx.xx", "xx.xx")
  )

})



test_that("style_xxx works with tbl_ae family", {

  expect_error(
    t1 <- df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        zero_symbol = NULL,
        digits =
          list(purrr::partial(style_xxx, width = 2),
               purrr::partial(style_xxx, width = 4, digits = 1))
      ) %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
      modify_spanning_header(all_ae_cols() ~ "**N = xx**"),
    NA)

  expect_error(
    t2 <- df_adverse_events %>%
      tbl_ae_count(
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        zero_symbol = NULL,
        digits =
          list(purrr::partial(style_xxx, width = 2))
      ) %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
      modify_spanning_header(all_ae_cols() ~ "**N = xx**"),
    NA)

  expect_error(
    t3 <- df_adverse_events %>%
      tbl_ae_focus(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        include = c(any_complication, grade3_complication),
        digits =
          list(purrr::partial(style_xxx, width = 2),
               purrr::partial(style_xxx, width = 4, digits = 1))
      ) %>%
      modify_spanning_header(all_ae_cols() ~ "**N = xx**"),
    NA)


  expect_snapshot(as.data.frame(t1))
  expect_snapshot(as.data.frame(t2))
  expect_snapshot(as.data.frame(t3))

})


test_that("style_xxx works with tbl_reg_summary", {

  expect_error(
    t4 <- df_patient_characteristics %>%
      select(marker, trt) %>%
      tbl_reg_summary(
        digits =
          list(
            marker ~ purrr::partial(style_xxx, width = 2),
            trt ~ list(purrr::partial(style_xxx, width = 2),
                       purrr::partial(style_xxx, width = 4, digits = 1))
          )
      ) %>%
      modify_header(stat_0 ~ "**N = xx**"),
    NA)

  expect_error(
    t5 <- df_patient_characteristics %>%
      select(marker, trt) %>%
      tbl_reg_summary(
        digits =
          list(
            marker ~ list(purrr::partial(style_xxx, width = 2), # N
                          purrr::partial(style_xxx, width = 4, digits = 1), # Mean
                          purrr::partial(style_xxx, width = 4, digits = 1), # SD
                          purrr::partial(style_xxx, width = 2), # Median
                          purrr::partial(style_xxx, width = 2), # IQR
                          purrr::partial(style_xxx, width = 2), # Range
                          purrr::partial(style_xxx, width = 2)), # N missing
            trt ~ list(purrr::partial(style_xxx, width = 2),
                       purrr::partial(style_xxx, width = 4, digits = 1))
          )
      ) %>%
      modify_header(stat_0 ~ "**N = xx**"),
    NA)

  expect_snapshot(as.data.frame(t4))
  expect_snapshot(as.data.frame(t5))

})
