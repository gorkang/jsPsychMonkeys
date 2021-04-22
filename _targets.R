# Parameters --------------------------------------------------------------

participants = list(uid = 1:20)

parameters = list(pid = 999,
                  image_browser = "chrome",
                  big_container = FALSE,
                  open_VNC = TRUE,
                  DEBUG = TRUE,
                  debug_file = FALSE)

parameters_local_server = list(local_or_server = "local",
                               folder_downloads = "~/Downloads",
                               local_folder_tasks = "Downloads/test_prototol")




# Libraries ---------------------------------------------------------------

suppressMessages(suppressWarnings(library(targets)))
suppressMessages(suppressWarnings(library(tarchetypes)))

  packages_to_load = c("targets", "tarchetypes", "dplyr", "glue", "purrr", "RSelenium", "XML")
  
  # target options (packages, errors...)
  tar_option_set(packages = packages_to_load, # Load packages for all targets
                 error = "workspace") # Needed to load workspace on error to debug

# Functions ---------------------------------------------------------------

# Source all /R files
lapply(list.files("./R_simple", full.names = TRUE, pattern = ".R"), source)

# source("R/helper_functions.R")
# source("R_simple/only_docker.R")
# source("R_simple/only_create_remDr.R")
# source("R_simple/complete_task_simple.R")
# source("R_simple/clean_up_docker.R")

# Restore output to console
  suppressWarnings(sink())
  sink(type = "message")


# Targets -----------------------------------------------------------------

list(
  
  tarchetypes::tar_map(
    values = participants,
    
    # Create docker container
    tar_target(
      container,
      only_docker(
        container_name = paste0("container", uid),
        image_browser = parameters$image_browser,
        DEBUG = parameters$DEBUG,
        big_container = parameters$big_container,
        folder_downloads = parameters_local_server$folder_downloads
      )
    ),
    
    # Open browser
    tar_target(
      remDr,
      only_create_remDr(
        container_port = container$container_port,
        browserName = parameters$image_browser,
        container_name = container$container_name
      )
    ),
    
    # Complete task
    tar_target(
      task,
      complete_task_simple(
        parameters_local_server = parameters_local_server,
        uid = uid,
        DEBUG = parameters$DEBUG,
        container_name = container$container_name,
        remDr = remDr
      )
    ),
    
    # Create docker container
    tar_target(
      clean_container,
      clean_up_docker(
        container_name = task,
        DEBUG = parameters$DEBUG
      )
    )
  )
  
)
