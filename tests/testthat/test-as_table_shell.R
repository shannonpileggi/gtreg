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
   )

  tbl2_shell <-
    modify_spanning_header(
      tbl2,
      all_cols_in_strata("Drug A") ~ "**Control Group**, N = {n}/{N} ({style_percent(p)}%)",
      all_cols_in_strata("Drug B") ~ "**Experimental Group**, N = {n}/{N} ({style_percent(p)}%)"
    ) %>%
    as_table_shell()


  expect_snapshot(as_tibble(tbl2_shell))
  expect_snapshot(tbl2_shell$table_styling$header$spanning_header)

 })
