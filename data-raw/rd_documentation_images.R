# 1. install pkg that will run and save the documentation images
remotes::install_github("ddsjoberg/gt.doc.images")

# 2. Install the most recent version of gtreg

# 3. Restart R to have a fresh R session.
#    No packages should be loaded, not even gtreg.
#    Any object that is in the global environment may be written over!

# 4. Run the function below to save the images created in the help files
#    Files will be saved to "~/man/figures/<filename>.png", where the filename
#    is the object name, i.e.'tbl_ae_count_ex1.png'. No example object
#    may overlap throughout the entire package.
gt.doc.images::save_help_file_images(pkg = "gtreg")
