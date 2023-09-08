# Step by step instructions to build and test package
  # https://r-pkgs.org/structure.html

# Make sure we have the minimum dependencies ------------------------------

  # To have a minimal renv cache:

  # .Rprofile: make sure source("renv/activate.R") is UNCOMMENTED
  # rstudioapi::navigateToFile(".Rprofile") # If this fails, it is uncommented

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
  # rstudioapi::navigateToFile(".Rprofile")

  # DO THIS ALWAYS so jsPsychHelpeR.zip is updated!
  # Create jsPsychHelpeR.zip
  # source("admin/helper-scripts-admin.R")
  jsPsychAdmin::create_jsPsychMonkeys_zip(add_renv_cache = FALSE)
  # 20230308: 47KB
  # 20230809: 51KB


# Build package -----------------------------------------------------------

  # .Rprofile: make sure source("renv/activate.R") is COMMENTED
  # rstudioapi::navigateToFile(".Rprofile")

  # Build and install
  devtools::document()
  devtools::load_all()


  # Documentation
  devtools::spell_check() # Spell check
  pkgdown::build_site() # Create documentation!

  # Only one time?
  # usethis::use_pkgdown() # build-ignore the docs directory and _pkgdown.yml file to avoid NOTE in CRAN checks
  # pkgdown::deploy_site_github() # CHECK HOWTO (only first time?)


# CHECK -------------------------------------------------------------------

  testthat::test_local() # [ FAIL 0 | WARN 0 | SKIP 0 | PASS 5 ]
  # test_check("jsPsychMonkeys")

  devtools::check() # Check package (Duration: 5m 43.2s)
  # 20230308: 0 errors ✔ | 1 warning ✖ | 4 notes ✖
  # 20230730: 0 errors ✔ | 1 warnings ✖ | 3 notes ✖


# Install package ---------------------------------------------------------

  # Build
  devtools::build()
  devtools::install()
  # renv::install(BUILT) # Install package from file


  # QUICK
  # source("admin/helper-scripts-admin.R")
  jsPsychAdmin::create_jsPsychMonkeys_zip(add_renv_cache = FALSE)
  devtools::document()
  devtools::load_all()
  pkgdown::build_site() # Create documentation!
  devtools::build()
  devtools::install()

  # renv::install("/home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychMonkeys_0.2.6.tar.gz") # Install package from file

  devtools::check() # Check package (~230s)
  # system.file("templates", package = "jsPsychMonkeys")
  # devtools::test() # Packages tests  (~180s) [ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]



# CHECK functions ----------------------------------------------------------

  # Same as tests in test-release_the_monkeys

  # Create local protocol
  jsPsychMaker::create_protocol(canonical_tasks = "AIM", folder_output = "~/Downloads/new_protocol_999/", launch_browser = FALSE)

  # Test local protocol
  jsPsychMonkeys::release_the_monkeys(uid = "1", times_repeat_protocol = 1, time_to_sleep_before_repeating_protocol = 10, local_folder_tasks = "~/Downloads/new_protocol_999/", DEBUG = TRUE, open_VNC = FALSE)

  # Test parallel monkeys
  jsPsychMonkeys::release_the_monkeys(uid = "18:20", open_VNC = FALSE,
                                      local_folder_tasks = "~/Downloads/new_protocol_999/",
                                      DEBUG = TRUE, sequential_parallel = "parallel", # number_of_cores = 2,
                                      clean_up_targets = TRUE)

  if (!require('remotes')) utils::install.packages('remotes'); remotes::install_github('gorkang/jsPsychMonkeys')

  # Test online protocol "test/protocols_DEV/test9999" is a simple protocol with AIM

  uid_random = round(stats::runif(1, 1, 10000), 0)
  jsPsychMonkeys::release_the_monkeys(uid = uid_random, open_VNC = TRUE,
                                      server_folder_tasks = "test/protocols_DEV/test9999",
                                      credentials_folder = "~/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychMonkeys/.vault/")


  uid_random = round(stats::runif(1, 1, 10000), 0)
  # Test online protocol "test/protocols_DEV/test9999" is a simple protocol with AIM
  jsPsychMonkeys::release_the_monkeys(uid = uid_random, open_VNC = TRUE,
                                      server_folder_tasks = "test/protocols_DEV/999new",
                                      clean_up_targets = TRUE,
                                      credentials_folder = "~/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychMonkeys/.vault/")


# CHECK package -----------------------------------------------------------


# WARNING: some of the tests KILL ALL THE DOCKER CONTAINERS
  active_containers = system('docker ps -q', intern = TRUE)
  active_containers |> purrr::walk(~system(paste0('docker stop ', .x)))
  system("docker system prune -f") # Cleans up system (stopped containers, etc.)

devtools::check() # Check package (~200s)
devtools::test() # Run all tests for the package (183.2 s): [ FAIL 1 | WARN 0 | SKIP 0 | PASS 2 ]
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
covr::codecov(token = "asdasdasdasd") # UPLOAD coverage reports to https://codecov.io/gh/gorkang/jsPsychMaker/


# REMEMBER ----------------------------------------------------------------

# If warning about non-ASCII characters. Find character and replace
# https://altcodeunicode.com/alt-codes-letter-o-with-accents/
# tools::showNonASCIIfile(file = "R/process_elements.R")
# stringi::stri_escape_unicode("ó")
# e.g. ó -> \u00F3




# READ files in package
# system.file("extdata", "999.zip", package = "jsPsychHelpeR")
# system.file("templates", "jsPsychHelpeR.zip", package = "jsPsychHelpeR")
