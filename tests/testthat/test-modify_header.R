test_that("modify_header() works", {
  tbl1 <-
    df_adverse_events %>%
    dplyr::mutate(grade = ifelse(dplyr::row_number() == 1L, NA, grade)) %>%
    tbl_ae(
      id = patient_id,
      id_df = df_patient_characteristics,
      ae = adverse_event,
      soc = system_organ_class,
      by = grade,
      strata = trt
    )
  tbl2 <- add_overall(tbl1)

  expect_equal(
    modify_header(tbl1, all_ae_cols() ~ "**Grade {by}**") %>%
      as_tibble() %>%
      names(),
    c("**Adverse Event**",
      "**Unknown**", "**Grade 1**", "**Grade 2**", "**Grade 3**", "**Grade 4**", "**Grade 5**",
      "**Unknown**", "**Grade 1**", "**Grade 2**", "**Grade 3**", "**Grade 4**", "**Grade 5**")
  )

  expect_equal(
    modify_header(tbl1, all_ae_cols(unknown = TRUE) ~ "**Grade {by}**") %>%
      as_tibble() %>%
      names(),
    c("**Adverse Event**",
      "**Grade Unknown**", "**Grade 1**", "**Grade 2**", "**Grade 3**", "**Grade 4**", "**Grade 5**",
      "**Grade Unknown**", "**Grade 1**", "**Grade 2**", "**Grade 3**", "**Grade 4**", "**Grade 5**")
  )

  expect_equal(
    modify_header(
      tbl1,
      all_unknown_cols() ~ "**Unknown Grade**",
      all_ae_cols() ~ "**Grade {by}**"
    ) %>%
      as_tibble() %>%
      names(),
    c("**Adverse Event**",
      "**Unknown Grade**", "**Grade 1**", "**Grade 2**", "**Grade 3**", "**Grade 4**", "**Grade 5**",
      "**Unknown Grade**", "**Grade 1**", "**Grade 2**", "**Grade 3**", "**Grade 4**", "**Grade 5**")
  )

  expect_equal(
    modify_spanning_header(
      tbl1,
      all_cols_in_strata("Drug A") ~ "**Control Group**, N = {n}/{N} ({style_percent(p)}%)",
      all_cols_in_strata("Drug B") ~ "**Experimental Group**, N = {n}/{N} ({style_percent(p)}%)"
    ) %>%
      purrr::pluck("table_styling", "spanning_header") %>%
      dplyr::filter(.by = c(level, column), dplyr::row_number() == dplyr::n(), startsWith(column, "stat_")) %>%
      dplyr::inner_join(
        tbl1$table_styling$header %>% dplyr::filter(!hide) %>% select(column),
        by = "column"
      ) %>%
      dplyr::pull(spanning_header) |>
      unique(),
    c("**Control Group**, N = 44/100 (44%)", "**Experimental Group**, N = 56/100 (56%)")
  )

  expect_equal(
    modify_header(
      tbl2,
      all_overall_cols() ~ "**Total**",
      all_unknown_cols() ~ "**Unknown Grade**",
      all_ae_cols() ~ "**Grade {by}**"
    ) %>%
      as_tibble() %>%
      names(),
    c("**Adverse Event**",
      "**Unknown Grade**", "**Grade 1**", "**Grade 2**", "**Grade 3**", "**Grade 4**", "**Grade 5**", "**Total**",
      "**Unknown Grade**", "**Grade 1**", "**Grade 2**", "**Grade 3**", "**Grade 4**", "**Grade 5**", "**Total**",
      "**Unknown Grade**", "**Grade 1**", "**Grade 2**", "**Grade 3**", "**Grade 4**", "**Grade 5**", "**Total**")
  )

  # modify_header works -------------------------------------------------------------------
  t1 <- df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      strata = trt,
      statistic = "{n}",
      by = grade
    )

  expect_error(
    t1_modified <-
      t1 %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**, N = {n} / {N}") %>%
      modify_spanning_header(all_ae_cols() ~ "**{strata}**, N = {n} / {N}") %>%
      modify_footnote(label = "N = {N}") %>%
      modify_caption("N = {N}"),
    NA
  )

  expect_equal(
    t1_modified$table_styling$header %>% filter(!hide) %>% dplyr::pull(label),
    c("**Adverse Event**", "**Grade 1**, N = 3 / 10", "**Grade 2**, N = 3 / 10",
      "**Grade 3**, N = 3 / 10", "**Grade 4**, N = 3 / 10", "**Grade 5**, N = 3 / 10",
      "**Grade 1**, N = 7 / 10", "**Grade 2**, N = 7 / 10", "**Grade 3**, N = 7 / 10",
      "**Grade 4**, N = 7 / 10", "**Grade 5**, N = 7 / 10")
  )
  expect_equal(
    t1_modified$table_styling$header %>%
      filter(!hide) %>%
      dplyr::inner_join(
        t1_modified$table_styling$spanning_header %>%
          dplyr::slice_tail(by = c(level, column), n = 1) %>%
          dplyr::select(column, spanning_header),
        by = "column"
      ) %>%
      dplyr::pull(spanning_header) %>%
      unique(),
    c("**Drug A**, N = 3 / 10", "**Drug B**, N = 7 / 10")
  )
  expect_equal(
    t1_modified$table_styling$caption,
    "N = 10",
    ignore_attr = TRUE
  )
})





