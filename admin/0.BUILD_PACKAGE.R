# Step by step instructions to build and test package
  # https://r-pkgs.org/structure.html

# Make sure we have the minimum dependencies ------------------------------

  # To have a minimal renv cache:

  # .Rprofile: make sure source("renv/activate.R") is UNCOMMENTED
  rstudioapi::navigateToFile(".Rprofile") # If this fails, it is uncommented

    # 1) Delete renv/cache and renv/lib folders
    # 2) Only install the packages explicitly mentionened in _targets_options.R main_packages
      # - Open _targets_options.R, load main_packages
      # renv::restore(packages = main_packages)
    # 3) Recreate _targets_packages.R:
      # targets::tar_renv(extras = c("markdown", "rstudioapi","visNetwork"))
    # 4) Create lockfile with the installed subset
      # renv::snapshot()

    # Make sure project 999 runs: targets::tar_make()


# Prepare files -----------------------------------------------------------

  # .Rprofile: make sure source("renv/activate.R") is UNCOMMENTED
  rstudioapi::navigateToFile(".Rprofile")

  # DO THIS ALWAYS so jsPsychHelpeR.zip is updated!
  # Create jsPsychHelpeR.zip
  source("admin/helper-scripts-admin.R")

  # add_renv_cache = TRUE creates a zip file with the renv cache (initially ~ 230MB package, after cleaning up a loot, 75.6MB)
  # Very useful to avoid downloading all renv cache again, to build the docker image much faster...
  # MAX Github uploads 100MB
  create_jsPsychHelpeR_zip(add_renv_cache = FALSE)



# Build package -----------------------------------------------------------

  # .Rprofile: make sure source("renv/activate.R") is COMMENTED
  rstudioapi::navigateToFile(".Rprofile")

  # Build and install
  devtools::document()
  devtools::load_all()


  # Documentation
  devtools::spell_check() # Spell check
  pkgdown::build_site() # Create documentation! # pkgdown::build_news()

  # Only one time?
  # usethis::use_pkgdown() # build-ignore the docs directory and _pkgdown.yml file to avoid NOTE in CRAN checks
  # pkgdown::deploy_site_github() # CHECK HOWTO (only first time?)


# Install package ---------------------------------------------------------

  # Build
  devtools::build()

  # devtools::install()
  renv::install("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychMonkeys_0.2.5.tar.gz") # Install package from file


# CHECK functions ----------------------------------------------------------

  # CHECK Main function
  jsPsychHelpeR::run_initial_setup(pid = 999, data_location = "~/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychHelpeR/data/999", dont_ask = TRUE)

  # CHECK project works
  targets::tar_make()

  # Check docker works
  # rstudioapi::navigateToFile("admin/create_docker.R")
  jsPsychHelpeR::create_docker_container()
  PID = 999
  file.remove(list.files(paste0("~/Downloads/jsPsychHelpeR", PID, "/outputs"), recursive = TRUE, full.names = TRUE))
  system(paste0("docker run --rm -d --name pid", PID, " -v ~/Downloads/jsPsychHelpeR", PID, "/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid", PID))

  # ***DEBUG***
  # docker run --rm -ti -v ~/Downloads/jsPsychHelpeR999/outputs:/home/project/jsPsychHelpeR/outputs:rw gorkang/jspsychhelper:pid999 /bin/bash


# CHECK package -----------------------------------------------------------

devtools::check() # Check package (~30s)
devtools::test() # Run all tests for the package (This includes test-0run_initial_setup.R, which will create a full project and run the protocol tests in the tmp/project) (~47s): [ FAIL 0 | WARN 0 | SKIP 0 | PASS 115 ]
  # Package Tests are in tests/testthat/
  # Protocol tests are in inst/templates/tests/testthat/
  # Snapshots in inst/templates/tests/testthat/_snaps/snapshots/


devtools::test_coverage()
# Not necesary because we have a use_github_action???
# covr::codecov(token = "6c8a8848-9175-446c-9cb8-131378f96356") # UPLOAD coverage reports to https://codecov.io/gh/gorkang/jsPsychMaker/



# Code coverage -----------------------------------------------------------

# FIRST TIME
# 1) usethis::use_coverage(type = c("codecov"))
# 2) usethis::use_github_action("test-coverage") # To continuosly monitor code coverage
# 3) Go to website: codecov.io using the GitHub account and setup the repo

COV_REPORT = covr::package_coverage(); COV_REPORT # Test coverage report. If a testthat tests fails, this FAILS!
covr::report(COV_REPORT) # Check local shiny app with Coverage report
# TOKEN FROM step 3)
covr::codecov(token = "6c8a8848-9175-446c-9cb8-131378f96356") # UPLOAD coverage reports to https://codecov.io/gh/gorkang/jsPsychMaker/


# REMEMBER ----------------------------------------------------------------

# If warning about non-ASCII characters. Find character and replace
# https://altcodeunicode.com/alt-codes-letter-o-with-accents/
# tools::showNonASCIIfile(file = "R/create_task.R")
# stringi::stri_escape_unicode("ó")
# e.g. ó -> \u00F3




# READ files in package
# system.file("extdata", "999.zip", package = "jsPsychHelpeR")
# system.file("templates", "jsPsychHelpeR.zip", package = "jsPsychHelpeR")
