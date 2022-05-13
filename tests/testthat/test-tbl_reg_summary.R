test_that("tbl_reg_summary works", {
  expect_error(
    df_patient_characteristics %>%
      tbl_reg_summary(by = trt, include = c(marker, status)),
    NA
  )

})
