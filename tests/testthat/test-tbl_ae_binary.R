
df_adverse_events_binary <-
  df_adverse_events %>%
  dplyr::mutate(
    any_complication = TRUE,
    grade3_complication = grade >= 3
  )

test_that("multiplication works", {
  expect_error(
    tbl <-
      df_adverse_events_binary %>%
      tbl_ae_binary(
        include = c(any_complication, grade3_complication),
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        include_label =
          list(any_complication = "Any Grade Complication",
               grade3_complication = "Grade 3+ Complication")
      ),
    NA
  )
})


