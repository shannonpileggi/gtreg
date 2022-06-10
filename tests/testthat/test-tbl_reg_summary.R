test_that("tbl_reg_summary works", {
  expect_error(
    df_patient_characteristics %>%
      tbl_reg_summary(by = trt, include = c(marker, status)),
    NA
  )

  gtsummary::set_gtsummary_theme(list("tbl_summary-str:default_con_type" = "continuous"))
  expect_message(
    df_patient_characteristics %>%
      tbl_reg_summary(by = trt, include = c(marker, status))
  )
  gtsummary::reset_gtsummary_theme()
})
