# 1. install pkg that will run and save the documentation images
install.packages(
  'gt.doc.images',
  repos = c(ddsjoberg = 'https://ddsjoberg.r-universe.dev',
            CRAN = 'https://cloud.r-project.org')
)

# 2. Install the most recent version of gtreg

# 3. Restart R to have a fresh R session.
#    No packages should be loaded, not even gtreg.
#    Any object that is in the global environment may be written over!

# 4. Run the function below to save the images created in the help files.
#    Only objects whose named in `_ex` or `_ex[:digit:]+` are saved.
#    Files will be saved to "~/man/figures/<filename>.png", where the filename
#    is the object name, i.e.'tbl_ae_count_ex1.png'. No example object
#    may overlap throughout the entire package.
gt.doc.images::save_help_file_images(pkg = "gtreg")

gt.doc.images::save_help_file_images(
  pkg = "gtreg",
  rd_files = c("selectors.Rd", "modify_ae_header.Rd")
)

# 5. Shrink png files
gt.doc.images::shrink_help_file_images(pkg = "gtreg")
