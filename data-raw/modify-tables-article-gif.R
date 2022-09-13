# demonstration tables for gif
# slides here: https://docs.google.com/presentation/d/1ue-Vqls0AO6LsoN87V7oA2sRUALe_zHlp4YxMGKvX2w/edit?usp=sharing

library(gtsummary)
library(gtreg)
library(tidyverse)
library(gt)

# initial table
t0 <- df_adverse_events %>%
  # create a missing value to demonstrate unknown columns
  mutate(grade = ifelse(dplyr::row_number() == 1L, NA, grade)) %>%
  tbl_ae(
    id = patient_id,
    ae = adverse_event,
    soc = system_organ_class,
    by = grade
  ) %>%
  add_overall(across = 'by') %>%
  bold_labels()


t1 <- t0 %>%
  modify_header(
    label ~ "**Event**"
  )

t2 <- t0 %>%
  modify_header(
    label ~ "**Event**",
    all_ae_cols() ~ "**Grade {by}**"
    )

t3 <- t0 %>%
  modify_header(
    label ~ "**Event**",
    all_ae_cols() ~ "**Grade {by}**",
    all_overall_cols() ~ "**Total**"
  )

t4 <- t0 %>%
  modify_header(
    label ~ "**Event**",
    all_ae_cols() ~ "**Grade {by}**",
    all_overall_cols() ~ "**Total**",
    all_unknown_cols() ~ "**Unknown Grade**"
    )

t5 <- t0 %>%
  modify_header(
    label ~ "**Event**",
    all_ae_cols() ~ "**Grade {by}**",
    all_overall_cols() ~ "**Total**",
    all_unknown_cols() ~ "**Unknown Grade**"
  ) |>
  modify_spanning_header(
    all_ae_cols(TRUE, TRUE) ~ "**All cohorts**, N = {N}"
  )

t6 <- t0 %>%
  modify_header(
    label ~ "**Event**",
    all_ae_cols() ~ "**Grade {by}**",
    all_overall_cols() ~ "**Total**",
    all_unknown_cols() ~ "**Unknown Grade**"
  ) |>
  modify_spanning_header(
    all_ae_cols(TRUE, TRUE) ~ "**All cohorts**, N = {N}"
  ) |>
  modify_caption("My caption: N = {N}")

t7 <- t0 %>%
  modify_header(
    label ~ "**Event**",
    all_ae_cols() ~ "**Grade {by}**",
    all_overall_cols() ~ "**Total**",
    all_unknown_cols() ~ "**Unknown Grade**"
  ) %>%
  modify_spanning_header(
    all_ae_cols(TRUE, TRUE) ~ "**All cohorts**, N = {N}"
  ) %>%
  modify_caption("My caption: N = {N}") %>%
  modify_footnote(label = "My footnote: N = {N}")



gtsave(as_gt(t0), filename =  "modify-t0.png", path = here::here("data-raw", "img"))
gtsave(as_gt(t1), filename =  "modify-t1.png", path = here::here("data-raw", "img"))
gtsave(as_gt(t2), filename =  "modify-t2.png", path = here::here("data-raw", "img"))
gtsave(as_gt(t3), filename =  "modify-t3.png", path = here::here("data-raw", "img"))
gtsave(as_gt(t4), filename =  "modify-t4.png", path = here::here("data-raw", "img"))
gtsave(as_gt(t5), filename =  "modify-t5.png", path = here::here("data-raw", "img"))
gtsave(as_gt(t6), filename =  "modify-t6.png", path = here::here("data-raw", "img"))
gtsave(as_gt(t7), filename =  "modify-t7.png", path = here::here("data-raw", "img"))
