# gtreg 0.2.0

Documentation:

* Added R in Medicine "Introducing {gtreg}" recording to readme.

* Migrated "Adverse Event Counting Methods" vignette to an article. (#186)

* Add a vignette to show how to modify table headers, footnotes, and captions. (#96)

* Added new Table Shells article outlining how to create shells with the {gtreg} package. (#85)

* Added vignette highlighting the options to export {gtreg} tables. (#108)


Functionality:

* Updated structure of package to no longer use tbl_stack() internally, improving speed. (#126)

* Fix in `add_overall()` to correctly order adverse events within system organ classes. (#183)

* Added function `style_xxx()` to assist in creating table shells.

* Re-exporting additional functions from {gtsummary}. (#172)

* Allow for special characters, like "\n " to be followed by a space. (#174)
 
# gtreg 0.1.1

* Fix in `tbl_ae_count()` when individual stratum had complete unobserved data for the SOC and AE. (#165)

* Fix in the denominator of `tbl_ae_focus()` for the system organ class level. (#163)

# gtreg 0.1.0

* Initial release.
