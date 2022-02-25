
# no factor inputs
df1 <-
  tibble::tibble(
    patient_id = paste0("ID", c(1,1,2,3)),
    system_organ_class = "Blood and lymphatic system disorders",
    adverse_event = c("Anaemia", "Anaemia", "Anaemia", "Increased tendency to bruise"),
    grade = c(1, 1, 1, 2),
    trt = rep("A", 4)
  )

# with missing values
df1_miss <-
  tibble::tibble(
    patient_id = paste0("ID", c(1,1,2,3)),
    system_organ_class = "Blood and lymphatic system disorders",
    adverse_event = c("Anaemia", "Anaemia", "Anaemia", "Increased tendency to bruise"),
    grade = c(1, 1, NA, NA),
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

dat <- tibble::tribble(
  ~subject, ~visit,  ~soc, ~ae, ~grade,
  # Subject 1 ----------------------------------------------------------------
  "001", 1, "Eye disorders", "Eye irritation", 1,
  "001", 1, "Gastrointestinal disorders", "Difficult digestion", 1,
  "001", 2, "Eye disorders", "Eye irritation", 1,
  "001", 3, "Eye disorders", "Eye irritation", 2,
  "001", 4, "Eye disorders", "Vision blurred", 2,
  # Subject 2 ----------------------------------------------------------------
  "002", 1, "Gastrointestinal disorders", "Difficult digestion", 2,
  "002", 1, "Gastrointestinal disorders", "Reflux", 2,
  "002", 2, "Eye disorders", "Vision blurred", 2,
  "002", 2, "Gastrointestinal disorders", "Reflux", 2,
  "002", 3, "Gastrointestinal disorders", "Reflux", NA
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

  expect_equal(e1[["table_body"]][["stat_6_1"]][3], "2 (67)")
  expect_equal(e1[["table_body"]][["stat_6_1"]][2], "1 (33)")

  expect_equal(e1[["table_body"]][["stat_1_1"]][1], "2 (67)")
  expect_equal(e1[["table_body"]][["stat_1_1"]][2], "2 (67)")

  expect_equal(e1[["table_body"]][["stat_2_1"]][1], "1 (33)")
  expect_equal(e1[["table_body"]][["stat_2_1"]][3], "1 (33)")

  expect_equal(
    e1[["table_body"]][["label"]],
    c("Blood and lymphatic system disorders", "Anaemia", "Increased tendency to bruise")
  )
})

test_that("counting rules", {



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
test_that("tbl_ae() works", {
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
           zero_symbol = "."
    ) %>%
      purrr::pluck("table_body") %>%
      dplyr::select(gtsummary::all_stat_cols()) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~. == ".")),
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
    as_tibble(tbl, col_labels = FALSE)$stat_1,
    c("2.0 / 3.0 (66.67%)", "2.0 / 3.0 (66.67%)", "0.0 / 3.0 (0.00%)")
  )

  # assess all inputs as a factor ----------------------------------------------

  f1 <- df_adverse_events %>%
    dplyr::mutate(dplyr::across(c(patient_id, adverse_event, system_organ_class, grade, trt), factor)) %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      soc = system_organ_class,
      by = grade,
      strata = trt,
      statistic = "{n}"
    )

  # expected table body
  f1_tibble_exp <- tibble::tribble(
    ~label, ~stat_1_1, ~stat_2_1, ~stat_3_1, ~stat_4_1, ~stat_5_1, ~stat_1_2, ~stat_2_2, ~stat_3_2, ~stat_4_2, ~stat_5_2,
    "Blood and lymphatic system disorders",       "—",       "1",       "—",       "1",       "1",       "—",       "—",       "—",       "1",       "6",
    "Anaemia",       "—",       "—",       "1",       "1",       "—",       "—",       "—",       "1",       "1",       "3",
    "Increased tendency to bruise",       "—",       "—",       "—",       "1",       "—",       "—",       "—",       "—",       "3",       "2",
    "Iron deficiency anaemia",       "—",       "—",       "—",       "1",       "1",       "1",       "2",       "—",       "1",       "1",
    "Thrombocytopenia",       "—",       "1",       "—",       "1",       "—",       "—",       "—",       "3",       "—",       "4",
    "Gastrointestinal disorders",       "—",       "—",       "—",       "2",       "1",       "—",       "—",       "—",       "2",       "5",
    "Difficult digestion",       "—",       "—",       "—",       "3",       "—",       "1",       "—",       "—",       "—",       "1",
    "Intestinal dilatation",       "1",       "—",       "—",       "—",       "—",       "1",       "1",       "—",       "—",       "1",
    "Myochosis",       "—",       "2",       "1",       "—",       "—",       "—",       "1",       "—",       "1",       "3",
    "Non-erosive reflux disease",       "3",       "—",       "—",       "—",       "—",       "1",       "—",       "—",       "3",       "3",
    "Pancreatic enzyme abnormality",       "—",       "—",       "1",       "1",       "1",       "2",       "1",       "1",       "1",       "—"
  )

  # expected table header
  f1_header_exp <-
    tibble::tribble(
      ~column, ~hide,   ~align, ~interpret_label,              ~label, ~interpret_spanning_header,    ~spanning_header,
      "label", FALSE,   "left",         "gt::md", "**Adverse Event**",                   "gt::md",                  NA,
      "stat_1_1", FALSE, "center",         "gt::md",             "**1**",                   "gt::md", "**Drug A**, N = 3",
      "stat_2_1", FALSE, "center",         "gt::md",             "**2**",                   "gt::md", "**Drug A**, N = 3",
      "stat_3_1", FALSE, "center",         "gt::md",             "**3**",                   "gt::md", "**Drug A**, N = 3",
      "stat_4_1", FALSE, "center",         "gt::md",             "**4**",                   "gt::md", "**Drug A**, N = 3",
      "stat_5_1", FALSE, "center",         "gt::md",             "**5**",                   "gt::md", "**Drug A**, N = 3",
      "stat_1_2", FALSE, "center",         "gt::md",             "**1**",                   "gt::md", "**Drug B**, N = 7",
      "stat_2_2", FALSE, "center",         "gt::md",             "**2**",                   "gt::md", "**Drug B**, N = 7",
      "stat_3_2", FALSE, "center",         "gt::md",             "**3**",                   "gt::md", "**Drug B**, N = 7",
      "stat_4_2", FALSE, "center",         "gt::md",             "**4**",                   "gt::md", "**Drug B**, N = 7",
      "stat_5_2", FALSE, "center",         "gt::md",             "**5**",                   "gt::md", "**Drug B**, N = 7"
    )

  # assess header
  expect_equal(
    f1$table_styling$header %>% dplyr::filter(!hide),
    f1_header_exp
  )

  # assess table body
  expect_equal(
    as_tibble(f1, col_labels = FALSE),
    f1_tibble_exp
  )

  # assess all inputs as a factor when aes do not occur in both strata ---------

  f2 <- ae_2 %>%
    dplyr::mutate(dplyr::across(c(subject, cohort, soc, ae, grade), factor)) %>%
    tbl_ae(
      id = subject,
      ae = ae,
      soc = soc,
      statistic = "{n}",
      by = grade,
      strata = cohort
    )


  f2_header_exp <-
    tibble::tribble(
      ~column, ~hide,   ~align, ~interpret_label,              ~label, ~interpret_spanning_header, ~spanning_header,
      "label", FALSE,   "left",         "gt::md", "**Adverse Event**",                   "gt::md",               NA,
      "stat_1_1", FALSE, "center",         "gt::md",             "**1**",                   "gt::md",   "**A**, N = 2",
      "stat_2_1", FALSE, "center",         "gt::md",             "**2**",                   "gt::md",   "**A**, N = 2",
      "stat_1_2", FALSE, "center",         "gt::md",             "**1**",                   "gt::md",   "**B**, N = 1",
      "stat_2_2", FALSE, "center",         "gt::md",             "**2**",                   "gt::md",   "**B**, N = 1"
    )

  f2_tibble_exp <-
    tibble::tribble(
      ~label, ~stat_1_1, ~stat_2_1, ~stat_1_2, ~stat_2_2,
      "Eye disorders",       "—",       "2",       "—",       "—",
      "Eye irritation",       "—",       "1",       "—",       "—",
      "Vision blurred",       "—",       "2",       "—",       "—",
      "Gastrointestinal disorders",       "—",       "—",       "1",       "—",
      "Difficult digestion",       "—",       "—",       "1",       "—"
    )

  # assess header
  expect_equal(
    f2$table_styling$header %>% dplyr::filter(!hide),
    f2_header_exp
  )

  # assess table body
  expect_equal(
    as_tibble(f2, col_labels = FALSE),
    f2_tibble_exp
  )

})

test_that("tbl_ae() headers", {

  # spanning header without strata ---------------------------------------------
  tbl_no_strata <- df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      by = grade
    )

  expect_equal(
    tbl_no_strata$table_styling$header %>% dplyr::filter(!hide),
    tibble::tribble(
      ~column, ~hide,   ~align, ~interpret_label,              ~label, ~interpret_spanning_header, ~spanning_header,
      "label", FALSE,   "left",         "gt::md", "**Adverse Event**",                   "gt::md",               NA,
      "stat_1", FALSE, "center",         "gt::md",             "**1**",                   "gt::md",         "**N = 10**",
      "stat_2", FALSE, "center",         "gt::md",             "**2**",                   "gt::md",         "**N = 10**",
      "stat_3", FALSE, "center",         "gt::md",             "**3**",                   "gt::md",         "**N = 10**",
      "stat_4", FALSE, "center",         "gt::md",             "**4**",                   "gt::md",         "**N = 10**",
      "stat_5", FALSE, "center",         "gt::md",             "**5**",                   "gt::md",         "**N = 10**"
    )
  )

  # header_by modified ---------------------------------------------------------
  h_by1 <- c("**Grade 1**", "**Grade 2**", "**Grade 3**","**Grade 4**", "**Grade 5**", "**Overall**")
  tbl_by1 <-
    df_adverse_events %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      by = grade,
      statistic = "{n}"
    ) %>%
    modify_ae_header(gtsummary::all_stat_cols() ~ "**Grade {by}**") %>%
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


  # bad call to missing_location= ----------------------------------------------
  expect_error(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        statistic = "{n}",
        by = grade,
        missing_location = "NOPE"
      )
  )

  # missing_location FIRST------- ----------------------------------------------
  miss_first <- df1_miss %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      statistic = "{n}",
      by = grade
    )
  expect_equal(
    miss_first$table_styling$header %>% dplyr::filter(!hide) %>% dplyr::pull(label),
    c("**Adverse Event**", "**Unknown**", "**1**")
  )

  # missing_location FIRST------------------------------------------------------
  miss_last <- df1_miss %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      statistic = "{n}",
      by = grade,
      missing_location = "last"
    )
  expect_equal(
    miss_last$table_styling$header %>% dplyr::filter(!hide) %>% dplyr::pull(label),
    c("**Adverse Event**", "**1**", "**Unknown**")
  )

  # missing_location HIDE------------------------------------------------------
  miss_hide <- df1_miss %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      statistic = "{n}",
      by = grade,
      missing_location = "hide"
    )
  expect_equal(
    miss_hide$table_styling$header %>% dplyr::filter(!hide) %>% dplyr::pull(label),
    c("**Adverse Event**", "**1**")
  )

  # missing_location COMPLEX ---------------------------------------------------
  miss_complex <- df_adverse_events %>%
    dplyr::mutate(
      grade = dplyr::case_when(
        dplyr::row_number() %in% 1:5 ~ NA_integer_,
        TRUE ~ grade
      )
    ) %>%
    tbl_ae(
      id = patient_id,
      ae = adverse_event,
      statistic = "{n}",
      by = grade,
      strata = trt,
      by_values = as.character(1:6),
      missing_location = "last"
    ) %>%
    .$table_styling %>%
    .$header %>%
    dplyr::filter(!hide) %>%
    dplyr::pull(label)

    expect_equal(length(miss_complex), 15)

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
      statistic = "{n}"
    ) %>%
    modify_ae_header(gtsummary::all_stat_cols() ~ "**Grade {by}**") %>%
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

  # tests that unused argument results in error ---------------------------------
  expect_error(
    df_adverse_events %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        statistic = "{n}",
        strata_by = "**Cohort {level}**"
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
        sort = c("ae", "soc")
      ) %>%
      as_tibble(col_labels = FALSE),
    NA
  )

  expect_equal(
    tbl %>%
      dplyr::slice_head(n = 2),
      tibble::tibble(
        label = c("Non-erosive reflux disease", "Thrombocytopenia"),
        stat_1 = c("10", "9")
      )
    )

  expect_equal(
    tbl_ae(
        data = df2,
        id = patient_id,
        ae = adverse_event,
        soc = system_organ_class,
        statistic = "{n}"
      ) %>%
      as_tibble(col_labels = FALSE) %>%
      .$stat_1,
    c("2", "1", "1", "3", "1", "2", "4", "1", "3")
  )

  expect_equal(
    tbl_ae(
      df2,
      id = patient_id,
      ae = adverse_event,
      soc = system_organ_class,
      statistic = "{n}",
      sort = c("ae", "soc")
    ) %>%
    as_tibble(col_labels = FALSE) %>%
      .$stat_1,
    c("4", "3", "1", "3", "2", "1", "2", "1", "1")
  )

  # check ordering when soc is input as factor
  expect_equal(
    df2 %>%
      mutate(
        system_organ_class = forcats::fct_rev(system_organ_class)
      ) %>%
      tbl_ae(
      id = patient_id,
      ae = adverse_event,
      soc = system_organ_class,
      statistic = "{n}",
      sort = NULL
    ) %>%
    as_tibble(col_labels = FALSE) %>%
      .$stat_1,
    c("4", "1", "3", "3", "1", "2", "2", "1", "1")
  )

    # check ordering when ae is input as factor
  expect_equal(
    df2 %>%
      mutate(
        adverse_event = forcats::fct_relevel(adverse_event, "sourdough", "banana")
      ) %>%
      tbl_ae(
        id = patient_id,
        ae = adverse_event,
        statistic = "{n}"
      ) %>%
      as_tibble(col_labels = FALSE) %>%
      .$stat_1,
    c("1", "2", "1", "1", "1", "3")
  )


})


test_that("tbl_ae() unknown values messaging", {

# error messaging with by levels ---------------------------------------------
expect_error(
  dat %>%
    dplyr::mutate(
      grade = ifelse(dplyr::row_number() == 1L, "Unknown", grade)
    ) %>%
    tbl_ae(
      id = subject,
      ae = ae,
      soc = soc,
      by = grade
    )
)
expect_error( # no error when no NA present
  dat %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      grade = ifelse(dplyr::row_number() == 1L, "Unknown", grade)
    ) %>%
    tbl_ae(
      id = subject,
      ae = ae,
      soc = soc,
      by = grade
    ),
  NA
)


expect_error(
  dat %>%
    tbl_ae(
      id = subject,
      ae = ae,
      soc = soc,
      by = grade,
      by_values = c("Unknown", 1:5)
    )
)
expect_error( # no error when no NA present
  dat %>%
    tidyr::drop_na() %>%
    tbl_ae(
      id = subject,
      ae = ae,
      soc = soc,
      by = grade,
      by_values = c("Unknown", 1:5)
    ),
  NA
)

})
