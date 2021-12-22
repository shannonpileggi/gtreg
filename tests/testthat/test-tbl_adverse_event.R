

test_that("df_adverse_events() single arm, single soc", {

  # no factor inputs
  df1 <-
    tibble::tibble(
      patient_id = paste0("ID", c(1,1,2,3)),
      system_organ_class = "Blood and lymphatic system disorders",
      adverse_event = c("Anaemia", "Anaemia", "Anaemia", "Increased tendency to bruise"),
      grade = c(1, 1, 1, 2),
      trt = rep("A", 4)
    )

  id_valid <-
    tibble::tibble(
      patient_id = paste0("ID", 1:5),
      trt = c(rep("A", 3),rep("B", 2))
    )

  e1 <-
    tbl_adverse_event(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
      strata = "trt",
      id_df = id_valid,
      by_values = as.character(c(1:5))
    )

  expect_equal(e1[["table_body"]][["stat_1_1"]][3], "2 (67)")
  expect_equal(e1[["table_body"]][["stat_1_1"]][2], "1 (33)")
  expect_equal(e1[["table_body"]][["stat_2_1"]][1], "2 (67)")
  expect_equal(e1[["table_body"]][["stat_2_1"]][2], "2 (67)")
  expect_equal(e1[["table_body"]][["stat_3_1"]][1], "1 (33)")
  expect_equal(e1[["table_body"]][["stat_3_1"]][3], "1 (33)")
})

# ------------------------------------------------------------------------------
test_that("df_adverse_events() works", {
  expect_error(
    df_adverse_events %>%
      tbl_adverse_event(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade,
        strata = trt
      ),
    NA
  )

  expect_error(
    df_adverse_events %>%
      tbl_adverse_event(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade
      ),
    NA
  )

  expect_error(
    df_adverse_events %>%
      tbl_adverse_event(
        id = patient_id,
        ae = adverse_event,
        by = grade
      ),
    NA
  )

  expect_error(
    df_adverse_events %>%
      tbl_adverse_event(
        id = patient_id,
        ae = adverse_event
      ),
    NA
  )
})
