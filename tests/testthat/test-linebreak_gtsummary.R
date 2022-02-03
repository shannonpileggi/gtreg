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

  # both star and underscore notation result in same latex code
  expect_equal(
    .markdown_to_latex("**bold this** not this *italia!*, and finally ***both***"),
    .markdown_to_latex("__bold this__ not this _italia!_, and finally ___both___")
  )
})
