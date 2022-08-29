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

  expect_equal(
    tbl1 %>%
      as_tibble(),

    tibble::tribble(
        ~"**Adverse Event**", ~"**1**", ~"**2**", ~"**3**", ~"**4**", ~"**5**", ~"**1**", ~"**2**", ~"**3**", ~"**4**", ~ "**5**",
        "Blood and lymphatic system disorders",        NA, "xx (xx)",        NA, "xx (xx)", "xx (xx)",        NA,        NA,        NA, "xx (xx)", "xx (xx)",
        "Anaemia",        NA,        NA, "xx (xx)", "xx (xx)",        NA,        NA,        NA, "xx (xx)", "xx (xx)", "xx (xx)",
        "Increased tendency to bruise",        NA,        NA,        NA, "xx (xx)",        NA,        NA,        NA,        NA, "xx (xx)", "xx (xx)",
        "Iron deficiency anaemia",        NA,        NA,        NA, "xx (xx)", "xx (xx)", "xx (xx)", "xx (xx)",        NA, "xx (xx)", "xx (xx)",
        "Thrombocytopenia",        NA, "xx (xx)",        NA, "xx (xx)",        NA,        NA,        NA, "xx (xx)",        NA, "xx (xx)",
        "Gastrointestinal disorders",        NA,        NA,        NA, "xx (xx)", "xx (xx)",        NA,        NA,        NA, "xx (xx)", "xx (xx)",
        "Difficult digestion",        NA,        NA,        NA, "xx (xx)",        NA, "xx (xx)",        NA,        NA,        NA, "xx (xx)",
        "Intestinal dilatation", "xx (xx)",        NA,        NA,        NA,        NA, "xx (xx)", "xx (xx)",        NA,        NA, "xx (xx)",
        "Myochosis",        NA, "xx (xx)", "xx (xx)",        NA,        NA,        NA, "xx (xx)",        NA, "xx (xx)", "xx (xx)",
        "Non-erosive reflux disease", "xx (xx)",        NA,        NA,        NA,        NA, "xx (xx)",        NA,        NA, "xx (xx)", "xx (xx)",
        "Pancreatic enzyme abnormality",        NA,        NA, "xx (xx)", "xx (xx)", "xx (xx)", "xx (xx)", "xx (xx)", "xx (xx)", "xx (xx)",        NA
      )
  )

 expect_equal(
   tbl1$table_styling$header$spanning_header,
   c(NA, NA, NA, NA, "Drug A", "**Drug A**, N = xx", "**Drug A**, N = xx", "**Drug A**, N = xx", "**Drug A**, N = xx", "**Drug A**, N = xx", "**Drug A**, N = xx", "Drug B", "**Drug B**, N = xx", "**Drug B**, N = xx", "**Drug B**, N = xx", "**Drug B**, N = xx", "**Drug B**, N = xx", "**Drug B**, N = xx")

 )

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


  expect_equal(
    as_tibble(tbl2_shell),
    tibble::tribble(
      ~"**Adverse Event**", ~"**1**", ~"**2**", ~"**3**", ~"**4**", ~"**5**", ~"**1**", ~"**2**", ~"**3**", ~"**4**", ~ "**5**",
      "Blood and lymphatic system disorders",           NA, "xx (xx.xx)",           NA, "xx (xx.xx)", "xx (xx.xx)",           NA,           NA,           NA, "xx (xx.xx)",    "xx (xx)",
      "Anaemia",           NA,           NA, "xx (xx.xx)", "xx (xx.xx)",           NA,           NA,           NA, "xx (xx.xx)", "xx (xx.xx)", "xx (xx.xx)",
      "Increased tendency to bruise",           NA,           NA,           NA, "xx (xx.xx)",           NA,           NA,           NA,           NA, "xx (xx.xx)", "xx (xx.xx)",
      "Iron deficiency anaemia",           NA,           NA,           NA, "xx (xx.xx)", "xx (xx.xx)", "xx (xx.xx)", "xx (xx.xx)",           NA, "xx (xx.xx)", "xx (xx.xx)",
      "Thrombocytopenia",           NA, "xx (xx.xx)",           NA, "xx (xx.xx)",           NA,           NA,           NA, "xx (xx.xx)",           NA, "xx (xx.xx)",
      "Gastrointestinal disorders",           NA,           NA,           NA, "xx (xx.xx)", "xx (xx.xx)",           NA,           NA,           NA, "xx (xx.xx)", "xx (xx.xx)",
      "Difficult digestion",           NA,           NA,           NA, "xx (xx.xx)",           NA, "xx (xx.xx)",           NA,           NA,           NA, "xx (xx.xx)",
      "Intestinal dilatation", "xx (xx.xx)",           NA,           NA,           NA,           NA, "xx (xx.xx)", "xx (xx.xx)",           NA,           NA, "xx (xx.xx)",
      "Myochosis",           NA, "xx (xx.xx)", "xx (xx.xx)",           NA,           NA,           NA, "xx (xx.xx)",           NA, "xx (xx.xx)", "xx (xx.xx)",
      "Non-erosive reflux disease", "xx (xx.xx)",           NA,           NA,           NA,           NA, "xx (xx.xx)",           NA,           NA, "xx (xx.xx)", "xx (xx.xx)",
      "Pancreatic enzyme abnormality",           NA,           NA, "xx (xx.xx)", "xx (xx.xx)", "xx (xx.xx)", "xx (xx.xx)", "xx (xx.xx)", "xx (xx.xx)", "xx (xx.xx)",           NA
    )
  )

  expect_equal(
    tbl2_shell$table_styling$header$spanning_header,
    c(NA, NA, NA, NA, "Drug A", "**Control Group**, N = xx/xx (xx%)", "**Control Group**, N = xx/xx (xx%)", "**Control Group**, N = xx/xx (xx%)", "**Control Group**, N = xx/xx (xx%)", "**Control Group**, N = xx/xx (xx%)", "**Drug A**, N = xx", "Drug B", "**Experimental Group**, N = xx/xx (xx%)", "**Experimental Group**, N = xx/xx (xx%)", "**Experimental Group**, N = xx/xx (xx%)", "**Experimental Group**, N = xx/xx (xx%)", "**Experimental Group**, N = xx/xx (xx%)", "**Drug B**, N = xx")
  )

})
