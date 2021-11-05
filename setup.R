# Run this script to install all the dependencies needed.

  # You might need to run it twice and restart the RStudio session afterwards: Control + Shift + F10


# Install targets and tarchetypes ------------------------------------------

  if (!require('targets')) install.packages('targets'); library('targets')
  if (!require('tarchetypes')) install.packages('tarchetypes'); library('tarchetypes')
  

# Make sure all packages are present --------------------------------------

  source("_targets.R")
  missing_packages = packages_to_load[!packages_to_load %in% installed.packages()[,1]]
  
  if (length(missing_packages) > 0) {
    cat("The following packages are missing and will be installed: ", packages_to_load[!packages_to_load %in% installed.packages()[,1]])
    install.packages(packages_to_load[!packages_to_load %in% installed.packages()[,1]])
  } else {
    cat(crayon::green("All the necessary packages are present\n"))
  }


# Make sure all the necessary folders exist -----------------------------
  
  necessary_folders = c(".vault", "outputs/DF", "outputs/log", "outputs/screenshots", "outputs/source")
  if (all(necessary_folders %in% dir(recursive = TRUE, include.dirs = TRUE, all.files = TRUE))) {
    cat(crayon::green("All the necessary folders are present\n"))
  } else {
    cat(crayon::yellow("Creating necessary folders: "), paste(necessary_folders, collapse = ", "), "\n")
    invisible(purrr::map(necessary_folders, dir.create, recursive = TRUE, showWarnings = FALSE))
  }
