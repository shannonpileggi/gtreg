
# no factor inputs
df1 <-
  tibble::tibble(
    patient_id = paste0("ID", c(1,1,2,3)),
    system_organ_class = "Blood and lymphatic system disorders",
    adverse_event = c("Anaemia", "Anaemia", "Anaemia", "Increased tendency to bruise"),
    grade = c(1, 1, 1, 2),
    trt = rep("A", 4)
  )

df2 <-
  tibble::tibble(
    patient_id = paste0("ID", 1:9),
    system_organ_class = c("Fruit", "Fruit", "Fruit", "Veg", "Veg", "Veg", "Veg", "Bread", "Bread"),
    adverse_event = c("apple",  "banana", "banana", "artichoke", "spinach", "spinach", "spinach", "ciabatta", "sourdough"),
    grade = rep(1, 9),
    trt = rep("A", 9)
  )

id_valid <-
  tibble::tibble(
    patient_id = paste0("ID", 1:5),
    trt = c(rep("A", 3),rep("B", 2))
  )

test_that("df_adverse_events() single arm, single soc", {
  e1 <-
    tbl_ae(
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
  expect_equal(
    e1[["table_body"]][["label"]],
    c("Blood and lymphatic system disorders", "Anaemia", "Increased tendency to bruise")
  )
})

test_that("counting rules", {
  ae_1 <- tibble::tribble(
    ~subject,     ~cohort, ~soc,            ~ae,             ~grade,
    "111-03-001", "A",     "Eye disorders", "Eye irritation", 1,
    "111-03-001", "A",     "Eye disorders", "Eye irritation", 2,
    "111-03-001", "A",     "Eye disorders", "Vision blurred", 2,
    "111-03-002", "A",     "Eye disorders", "Vision blurred", 2,
  )

  ae_2 <- tibble::tribble(
    ~subject,     ~cohort, ~soc,            ~ae,             ~grade,
    "111-03-001", "A",     "Eye disorders", "Eye irritation", 1,
    "111-03-001", "A",     "Eye disorders", "Eye irritation", 2,
    "111-03-001", "A",     "Eye disorders", "Vision blurred", 2,
    "111-03-002", "A",     "Eye disorders", "Vision blurred", 2,
    "111-03-002", "B",     "Gastrointestinal disorders", "Difficult digestion", 1
  )


  expect_error(
    expected_ae_1 <-
      tbl_ae(
        data = ae_1,
        id = subject,
        ae = ae,
        soc = soc,
        by = grade,
        strata = cohort
      ),
    NA
  )

  expect_error(
    expected_ae_2 <-
      tbl_ae(
        data = ae_2,
        id   = subject,
        ae = ae,
        soc = soc,
        by = grade,
        strata = cohort
      ),
    NA
  )

})

# ------------------------------------------------------------------------------
test_that("df_adverse_event() works", {
  expect_error(
    df_adverse_events %>%
      tbl_ae(
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
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        by = grade
      ),
    NA
  )

  expect_error(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        by = grade
      ),
    NA
  )

  expect_error(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event
      ),
    NA
  )

  # check zero_symbol output
  expect_equal(
    tbl_ae(data = df1,
           id = "patient_id",
           ae = "adverse_event",
           soc = "system_organ_class",
           by = "grade",
    ) %>%
      purrr::pluck("table_body") %>%
      dplyr::select(gtsummary::all_stat_cols()) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), is.na)),
    tbl_ae(data = df1,
           id = "patient_id",
           ae = "adverse_event",
           soc = "system_organ_class",
           statistic = "{n}",
           by = "grade",
           zero_symbol = NULL
    ) %>%
      purrr::pluck("table_body") %>%
      dplyr::select(gtsummary::all_stat_cols()) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ . == "0"))
  )

  # default statistic, default zero_symbol
  expect_error(
    tbl_ae(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      by = "grade",
    ),
    NA
  )

  # modified statistic, default zero_symbol
  expect_error(
    tbl_ae(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      statistic = "{n}",
      by = "grade",
    ),
    NA
  )

  # modified statistic, modified zero_symbol
  expect_error(
    tbl_ae(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      statistic = "{n}",
      by = "grade",
      zero_symbol = "*"
    ),
    NA
  )

  # modified statistic, NULL zero_symbol
  expect_error(
    tbl_ae(
      data = df1,
      id = "patient_id",
      ae = "adverse_event",
      soc = "system_organ_class",
      statistic = "{n} / {N} ({p}%)",
      by = "grade",
      zero_symbol = NULL
    ),
    NA
  )

  # checking digits argument
  expect_error(
    tbl <-
      tbl_ae(
        data = df1,
        id = "patient_id",
        ae = "adverse_event",
        soc = "system_organ_class",
        statistic = "{n} / {N} ({p}%)",
        by = "grade",
        zero_symbol = NULL,
        digits = c(1, 1, 2)
      ),
    NA
  )
  expect_equal(
    as_tibble(tbl, col_labels = FALSE)$stat_2,
    c("2.0 / 3.0 (66.67%)", "2.0 / 3.0 (66.67%)", "0.0 / 3.0 (0.00%)")
  )

})


test_that("tbl_ae() headers", {

  # header_by modified ---------------------------------------------------------
  h_by1 <- c("**Grade 1**", "**Grade 2**", "**Grade 3**","**Grade 4**", "**Grade 5**", "**Overall**")
  tbl_by1 <-
    df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      by = grade,
      statistic = "{n}",
      header_by = "**Grade {level}**"
    ) %>%
    add_overall(across = 'by')

  expect_equal(length(intersect(tbl_by1$table_styling$header$label, h_by1)), 6)

  # header_by default ---------------------------------------------------------
  h_by2 <- c("**1**", "**2**", "**3**","**4**", "**5**", "**Overall**")
  tbl_by2 <-
    df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      by = grade,
      statistic = "{n}"
    ) %>%
    add_overall(across = 'by')

  expect_equal(length(intersect(tbl_by2$table_styling$header$label, h_by2)), 6)

  # header_by without by -------------------------------------------------------
   expect_error(
     df_adverse_events %>%
       tbl_ae(
         id = patient_id,
         ae = adverse_event,
         statistic = "{n}",
         header_by = "**Grade {level}**"
       )
   )


  # by with header_strata ------------------------------------------------------
  expect_error(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        statistic = "{n}",
        by = grade,
        header_strata = "**Cohort {level}**"
      )
   )


  # ----------------------------------------------------------------------------
  # strata default with overall and header_by -------------------------------
  strata_by1 <- c("**Drug A**, N = 3", "**Drug B**, N = 7", "**Overall**, N = 10")
  tbl_strata1 <-
    df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      by = grade,
      strata = trt,
      statistic = "{n}",
      header_by = "**Grade {level}**"
    ) %>%
    add_overall(across = 'strata')

  expect_equal(length(intersect(tbl_strata1$table_styling$header$spanning_header, strata_by1)), 3)



  # strata_by default, no header_by no overall ---------------------------------
  strata_by2 <- c("**Drug A**, N = 3", "**Drug B**, N = 7")
  tbl_strata2 <-
    df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      strata = trt
    )
  expect_equal(length(intersect(tbl_strata2$table_styling$header$spanning_header, strata_by2)), 2)

  # strata_by default with overall ---------------------------------------------
  strata_by3 <- c("**Drug A**, N = 3", "**Drug B**, N = 7", "**Overall**, N = 10")
  tbl_strata3 <-
    df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      strata = trt
    ) %>%
    add_overall(across = 'strata')

  expect_equal(length(intersect(tbl_strata3$table_styling$header$spanning_header, strata_by3)), 3)

  # strata_by without strata----------------------------------------------------
  expect_error(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        statistic = "{n}",
        strata_by = "**Cohort {level}**"
      )
  )

  # strata with header_by ------------------------------------------------------
  expect_error(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        statistic = "{n}",
        strata = trt,
        header_by = "**Cohort {level}**"
      )
  )


  expect_equal(
    tbl %>% dplyr::slice_head(n = 2),
    tibble::tibble(
      label = c("Non-erosive reflux disease", "Thrombocytopenia"),
      stat_2 = c("10", "9")
    )
  )
})



test_that("tbl_ae() sorting", {

expect_error(
  tbl <-
    df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      statistic = "{n}",
      sort = "frequency"
    ) %>%
    as_tibble(col_labels = FALSE)
)

#  tbl_ae(
#    df2,
#    id = patient_id,
#    ae = adverse_event,
#    soc = system_organ_class,
#    statistic = "{n}"
#  )
#
#  tbl_ae(
#    df2,
#    id = patient_id,
#    ae = adverse_event,
#    soc = system_organ_class,
#    statistic = "{n}",
#    sort = "frequency"
#  )
#
#  # check ordering when soc is input as factor
#  df2 %>%
#    mutate(
#      system_organ_class = forcats::fct_rev(system_organ_class)
#    ) %>%
#    tbl_ae(
#    id = patient_id,
#    ae = adverse_event,
#    soc = system_organ_class,
#    statistic = "{n}",
#    sort = "alphanumeric"
#  )
#
#  # check ordering when ae is input as factor
#  df2 %>%
#    mutate(
#      adverse_event = forcats::fct_relevel(adverse_event, "sourdough", "banana")
#    ) %>%
#    tbl_ae(
#      id = patient_id,
#      ae = adverse_event,
#      statistic = "{n}"
#    )
#
#
#  # check ordering when ae is input as factor
#  df2 %>%
#    mutate(
#      adverse_event = forcats::fct_relevel(adverse_event, "sourdough", "banana")
#    ) %>%
#    tbl_ae(
#      id = patient_id,
#      ae = adverse_event,
#      statistic = "{n}",
#      sort = "frequency"
#    )
#
#  # check ordering when ae is input as factor
#  df2 %>%
#    mutate(
#      adverse_event = forcats::fct_relevel(adverse_event, "sourdough", "banana")
#    ) %>%
#    tbl_ae(
#      id = patient_id,
#      ae = adverse_event,
#      statistic = "{n}",
#      sort = "alphanumeric"
#    )
#


})




