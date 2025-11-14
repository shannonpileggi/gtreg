# Changelog

## gtreg 0.4.1

CRAN release: 2025-02-27

- Update internals to be compatible with {gtsummary} 2.1.0 release.

- Removed {broom.helpers} dependency.

## gtreg 0.4.0

CRAN release: 2024-07-25

- Update internals to be compatible with {gtsummary} 2.0.0 release.

## gtreg 0.3.0

CRAN release: 2023-11-17

- Add errors for missing `soc` and `ae` in `complete_ae_data()`
  ([\#204](https://github.com/shannonpileggi/gtreg/issues/204)).

- Fix GitHub link on `pkgdown` site
  ([\#207](https://github.com/shannonpileggi/gtreg/issues/207)).

- Address deprecated
  [`purrr::when()`](https://purrr.tidyverse.org/reference/when.html) and
  [`forcats::fct_explicit_na()`](https://forcats.tidyverse.org/reference/fct_explicit_na.html)
  ([\#210](https://github.com/shannonpileggi/gtreg/issues/210)).

- Update package versions for dependencies.

## gtreg 0.2.0

CRAN release: 2022-10-18

Documentation:

- Added R in Medicine “Introducing {gtreg}” recording to readme.

- Migrated “Adverse Event Counting Methods” vignette to an article.
  ([\#186](https://github.com/shannonpileggi/gtreg/issues/186))

- Add a vignette to show how to modify table headers, footnotes, and
  captions. ([\#96](https://github.com/shannonpileggi/gtreg/issues/96))

- Added new Table Shells article outlining how to create shells with the
  {gtreg} package.
  ([\#85](https://github.com/shannonpileggi/gtreg/issues/85))

- Added vignette highlighting the options to export {gtreg} tables.
  ([\#108](https://github.com/shannonpileggi/gtreg/issues/108))

Functionality:

- Updated structure of package to no longer use tbl_stack() internally,
  improving speed.
  ([\#126](https://github.com/shannonpileggi/gtreg/issues/126))

- Fix in
  [`add_overall()`](https://www.danieldsjoberg.com/gtsummary/reference/add_overall.html)
  to correctly order adverse events within system organ classes.
  ([\#183](https://github.com/shannonpileggi/gtreg/issues/183))

- Added function
  [`style_xxx()`](https://shannonpileggi.github.io/gtreg/reference/style_xxx.md)
  to assist in creating table shells.

- Re-exporting additional functions from {gtsummary}.
  ([\#172](https://github.com/shannonpileggi/gtreg/issues/172))

- Allow for special characters, like “” to be followed by a space.
  ([\#174](https://github.com/shannonpileggi/gtreg/issues/174))

## gtreg 0.1.1

CRAN release: 2022-08-17

- Fix in
  [`tbl_ae_count()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae_count.md)
  when individual stratum had complete unobserved data for the SOC and
  AE. ([\#165](https://github.com/shannonpileggi/gtreg/issues/165))

- Fix in the denominator of
  [`tbl_ae_focus()`](https://shannonpileggi.github.io/gtreg/reference/tbl_ae_focus.md)
  for the system organ class level.
  ([\#163](https://github.com/shannonpileggi/gtreg/issues/163))

## gtreg 0.1.0

CRAN release: 2022-08-10

- Initial release.
