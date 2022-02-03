test_that(".linebreak_gtsummary() works", {
  tbl <-
    df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      by = grade,
      header_by = "**Grade {level}**"
    )

  expect_equal(
    .linebreak_gtsummary(tbl),
    c("\\textbf{Adverse Event}", paste0("\\textbf{Grade ", 1:5, "}"))
  )

  expect_error(
    tbl %>%
      as_kable_extra(
        col.names = .linebreak_gtsummary(.),
        escape = FALSE,
        format = "latex"
      ),
    NA
  )
})
