# DEBUG -------------------------------------------------------------------

# BENCHMARK: invalidate initial container
# if (file.exists("_targets/objects/container_24000") == TRUE ) targets::tar_invalidate(matches("container_24000"))
# targets::tar_destroy()

# Parameters --------------------------------------------------------------

  participants = list(uid = 24000) # e.g. [1:100]
  
  parameters = list(pid = "test/TEST-ALL-ITEMS", # ["test/TEST-ALL-ITEMS", 999, "test/1x"]
                    browserName = "chrome",
                    big_container = FALSE,
                    initial_wait = 2,
                    screenshot = FALSE,
                    DEBUG = TRUE,
                    debug_file = FALSE,
                    open_VNC = FALSE)
  
  parameters_local_server = list(local_or_server = "server", # [server, local]
                                 folder_downloads = "~/Downloads",
                                 local_folder_tasks = "Downloads/tests/1x") #["Downloads/tests/test_prototol", "Downloads/tests/1x"]


# Libraries ---------------------------------------------------------------

  suppressMessages(suppressWarnings(library(targets)))
  suppressMessages(suppressWarnings(library(tarchetypes)))
  suppressMessages(suppressWarnings(library(future)))
  suppressMessages(suppressWarnings(library(future.callr)))
  
  # Needed here if we run make_future()
  plan(callr)
  Sys.setenv(R_CLI_NUM_COLORS = crayon::num_ansi_colors()) # So crayon colors work
  
  # List of packages to use
  packages_to_load = c("targets", "tarchetypes", "dplyr", "glue", "purrr", "readr", "RSelenium", "rvest" ,"XML")
    
    
# Functions ---------------------------------------------------------------

# Source all /R files
lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)
  

# Maintenance -------------------------------------------------------------

  # target options (packages, errors...)
  tar_option_set(packages = packages_to_load, # Load packages for all targets
                 error = "workspace") # Needed to load workspace on error to debug
  
  # Restore output to console (in case it was left hanging...)
    suppressWarnings(sink())
    sink(type = "message")
    

# Targets -----------------------------------------------------------------

list(
  # Maybe add parameters to a target?
  # tar_target(configuration, ),
  
  tarchetypes::tar_map(
    values = participants,
    
    # Create docker container
    tar_target(
      container,
      only_docker(
        container_name = paste0("container", uid),
        browserName = parameters$browserName,
        DEBUG = parameters$DEBUG,
        big_container = parameters$big_container,
        folder_downloads = parameters_local_server$folder_downloads
      )
    ),
    
    # Open browser
    tar_target(
      remoteDriver,
      only_create_remDr(
        container_port = container$container_port,
        browserName = parameters$browserName,
        container_name = container$container_name
      )
    ),
    
    # Complete task
    tar_target(
      task,
      complete_task(
        parameters_local_server = parameters_local_server,
        uid = uid,
        initial_wait = parameters$initial_wait,
        screenshot = parameters$screenshot,
        DEBUG = parameters$DEBUG,
        open_VNC = parameters$open_VNC,
        container_name = remoteDriver$container_name,
        remDr = remoteDriver$remDr
      )
    ),
    
    # Clean up after participants finish
    tar_target(
      clean_container,
      clean_up_docker(
        container_name = task,
        keep_alive = TRUE,
        DEBUG = parameters$DEBUG
      )
    )
  )
)
