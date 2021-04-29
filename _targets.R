# Parameters --------------------------------------------------------------
  
  parameters_monkeys = list(
    participants = list(uid = 24000),
    
    docker = list(
      browserName = "chrome",
      big_container = FALSE,
      keep_alive = TRUE,
      folder_downloads = "~/Downloads"
    ),
    
    debug = list(
      DEBUG = TRUE,
      screenshot = FALSE,
      debug_file = FALSE,
      open_VNC = TRUE
    ),
    
    task_params = list(
      pid = 999,
      local_or_server = "server", # ["local", "server", "test"]
      local_folder_tasks = "Downloads/tests/test_prototol",
      server_folder_tasks = "test/1x",
      initial_wait = 2,
      wait_retry = 5
    )
  )
  
# Libraries ---------------------------------------------------------------

  suppressMessages(suppressWarnings(library(targets)))
  suppressMessages(suppressWarnings(library(tarchetypes)))
  suppressMessages(suppressWarnings(library(future)))
  suppressMessages(suppressWarnings(library(future.callr)))

  # List of packages to use
  packages_to_load = c("targets", "tarchetypes", "dplyr", "glue", "purrr", "readr", "RSelenium", "rvest" ,"XML")

  
  # Needed here if we run make_future()
    # https://github.com/HenrikBengtsson/future/#controlling-how-futures-are-resolved
    future::plan(callr)
    future::tweak(strategy = "multisession")# REVIEW: while (NUMBER_dockers >= NUMBER_dockers_LOW)  of only_docker.R
    # future::tweak(strategy = "multicore") # NOT working on Rstudio?
    # future::tweak(strategy = "transparent")
    
    
# Functions ---------------------------------------------------------------

  # Source all /R files
  lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)
  

# Maintenance -------------------------------------------------------------

  # So crayon colors work when using future
  Sys.setenv(R_CLI_NUM_COLORS = crayon::num_ansi_colors()) 
  
  # target options (packages, errors...)
  tar_option_set(
    # Load packages for all targets
    packages = packages_to_load,
    # to load workspace on error to debug
    error = "workspace",
    memory = "transient",
    garbage_collection = TRUE
    ) 
  
  
  # Restore output to console (in case it was left hanging...)
    suppressWarnings(sink())
    sink(type = "message")
    

# Targets -----------------------------------------------------------------

  list(
    # Maybe add parameters to a target?
    # tar_target(parameters_monkeys, ),
    
    tarchetypes::tar_map(
      values = parameters_monkeys$participants,
      
      # Create docker container
      tar_target(
        container,
        only_docker(
          container_name = paste0("container", uid),
          browserName = parameters_monkeys$docker$browserName,
          DEBUG = parameters_monkeys$debug$DEBUG,
          big_container = parameters_monkeys$docker$big_container,
          folder_downloads = parameters_monkeys$docker$folder_downloads,
          parameters_docker = parameters_monkeys
        )
      ),
      
      # Open browser
      tar_target(
        remoteDriver,
        only_create_remDr(
          container_port = container$container_port,
          browserName = container$browserName,
          container_name = container$container_name
        )
      ),
      
      # Complete task
      tar_target(
        task,
        complete_task(
          parameters_task = parameters_monkeys,
          uid = uid,
          initial_wait = parameters_monkeys$task_params$initial_wait,
          wait_retry = parameters_monkeys$task_params$wait_retry,
          screenshot = parameters_monkeys$debug$screenshot,
          DEBUG = parameters_monkeys$debug$DEBUG,
          open_VNC = parameters_monkeys$debug$open_VNC,
          container_name = remoteDriver$container_name,
          remDr = remoteDriver$remDr
        )
      ),
      
      # Clean up after participants finish
      tar_target(
        clean_container,
        clean_up_docker(
          container_name = task,
          keep_alive = parameters_monkeys$docker$keep_alive,
          DEBUG = parameters_monkeys$debug$DEBUG
        )
      )
    )
  )
