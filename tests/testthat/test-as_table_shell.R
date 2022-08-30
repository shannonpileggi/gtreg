skip_on_cran()
skip_on_ci()

test_that("basic as_table_shell works ", {
  expect_error(
    tbl1 <- df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        strata = trt
      ) %>%
      as_table_shell(),
    NA
  )


  expect_snapshot(as_gt(tbl1, id = "tbl1-0001"))
  expect_snapshot(as_tibble(tbl1))
  expect_snapshot(tbl1$table_styling$header$spanning_header)


})

test_that("modify spanning header as_table_shell works ", {

 tbl2 <-
   df_adverse_events %>%
   tbl_ae(
     id = patient_id,
     id_df = df_patient_characteristics,
     ae = adverse_event,
     soc = system_organ_class,
     by = grade,
     strata = trt
   ) %>%
   modify_spanning_header(
    all_cols_in_strata("Drug A") ~ "**Control Group**, N = {n}/{N} ({style_percent(p)}%)",
    all_cols_in_strata("Drug B") ~ "**Experimental Group**, N = {n}/{N} ({style_percent(p)}%)"
   ) %>%
   as_table_shell()

  expect_snapshot(as_gt(tbl2, id = "tbl2-0001"))
  expect_snapshot(as_tibble(tbl2))
  expect_snapshot(tbl2$table_styling$header$spanning_header)

 })

test_that("tbl_ae_count +  as_table_shell works ", {

  expect_error(
    tbl3 <-
      tbl_ae_count(
        data = df_adverse_events,
        ae = adverse_event,
        soc = system_organ_class,
        strata = trt,
        by = grade,
        zero_symbol = NULL
      ) %>%
      modify_header(all_ae_cols() ~ "**Grade {by}**") %>%
      as_table_shell(),
    NA
    )

  expect_snapshot(as_gt(tbl3, id = "tbl3-0001"))
})



test_that("tbl_ae_focus +  as_table_shell works ", {

  expect_error(
    tbl4 <-
      df_adverse_events %>%
        tbl_ae_focus(
          id = patient_id,
          id_df = df_patient_characteristics,
          ae = adverse_event,
          soc = system_organ_class,
          include = c(any_complication, grade3_complication)
        ) %>%
        as_table_shell(),
    NA
  )

  expect_snapshot(as_gt(tbl4, id = "tbl4-0001"))
})



test_that("tbl_reg_summary +  as_table_shell works ", {

  tbl5 <-
    df_patient_characteristics %>%
    tbl_reg_summary(by = trt, include = c(marker, status))


  expect_error(

    tbl5 |> as_table_shell(),
    NA
  )

  tbl5 |>
    modify_fmt_fun(
      update = all_categorical() ~  function(x) style_percent(x, digits = 1)
    )


  tbl5 |>
    modify_fmt_fun(
      update = all_stat_cols() ~  function(x) style_percent(x, digits = 1),
      rows = var_type == "categorical"
    )

  #expect_snapshot(as_gt(tbl5, id = "tbl5-0001"))
})
