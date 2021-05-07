# Parameters ----- [EDIT ONLY THIS SECTION] --------------------------------
  
  # Minimal parameters for your simulation. 
  # For a complete list of possible parameters, see set_parameters()
  parameters_monkeys_minimal = list(
    uid = 1:2,
    random_id = FALSE,
    keep_alive = TRUE,
    # DEBUG = TRUE,
    open_VNC = TRUE,
    local_folder_tasks = c("Downloads/tests/test_prototol")#, "Downloads/tests/994")
    # server_folder_tasks = "test/1x"
  )
  

  
# Libraries ---------------------------------------------------------------

  suppressMessages(suppressWarnings(library(targets)))
  suppressMessages(suppressWarnings(library(tarchetypes)))
  suppressMessages(suppressWarnings(library(future)))
  suppressMessages(suppressWarnings(library(future.callr)))
  suppressMessages(suppressWarnings(library(purrr)))
  suppressMessages(suppressWarnings(library(dplyr)))
  

  # List of packages to use
  packages_to_load = c("targets", "tarchetypes", "dplyr", "glue", "purrr", "readr", "RSelenium", "rvest" ,"XML")

  
  # For tar_make_future() [https://github.com/HenrikBengtsson/future/#controlling-how-futures-are-resolved]
  future::plan(callr)
  future::tweak(strategy = "multisession")
    
    
# Functions ---------------------------------------------------------------

  # Source all /R files
  lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)
  

# Maintenance -------------------------------------------------------------

  # So crayon colors work when using future
  Sys.setenv(R_CLI_NUM_COLORS = crayon::num_ansi_colors()) 
  
  # target options (packages, errors...)
  tar_option_set(
    packages = packages_to_load, # Load packages for all targets
    error = "workspace", # to load workspace on error to debug
    memory = "transient", # Memory management
    garbage_collection = TRUE # Memory management
    ) 
  
  
  # Restore output to console (in case it was left hanging...)
    suppressWarnings(sink())
    sink(type = "message")
    

# Targets -----------------------------------------------------------------

TARGETS =  list(
    
  tar_target(
    parameters_monkeys,
    set_parameters(parameters_input = parameters_monkeys_minimal), 
    priority = 1
  ),
  
    tarchetypes::tar_map(
      # values = parameters_monkeys$participants,
      values = list(uid = parameters_monkeys_minimal$uid),
      
      # Create docker container
      tar_target(
        container,
        create_docker(
          container_name = paste0("container", uid),
          browserName = parameters_monkeys$docker$browserName,
          DEBUG = parameters_monkeys$debug$DEBUG,
          big_container = parameters_monkeys$docker$big_container,
          folder_downloads = parameters_monkeys$docker$folder_downloads,
          parameters_docker = parameters_monkeys
        ), priority = .5
      ),
      
      # Open remote Driver and browser
      tar_target(
        remoteDriver,
        create_remDr(
          container_port = container$container_port,
          browserName = container$browserName,
          container_name = container$container_name
        ), priority = .5
      ),
      
      
      # Create links
      tar_target(
        links_tar,
        create_links(
          parameters_task = parameters_monkeys,
          uid = uid,
          uid_URL = parameters_monkeys$task_params$uid_URL,
          DEBUG = parameters_monkeys$debug$DEBUG,
          container_name = remoteDriver$container_name,
          remDr = remoteDriver$remDr
        ), priority = .5
      ),
      
      
      # Complete task
      tar_target(
        task,
        complete_task(
          parameters_task = parameters_monkeys,
          uid = uid,
          links = links_tar$links,
          wait_retry = parameters_monkeys$task_params$wait_retry,
          screenshot = parameters_monkeys$debug$screenshot,
          DEBUG = parameters_monkeys$debug$DEBUG,
          open_VNC = parameters_monkeys$debug$open_VNC,
          container_name = remoteDriver$container_name,
          remDr = remoteDriver$remDr
        ), priority = .5
      ),
      
      # Clean up after participants finish
      tar_target(
        clean_container,
        clean_up_docker(
          container_name = task,
          keep_alive = parameters_monkeys$docker$keep_alive,
          DEBUG = parameters_monkeys$debug$DEBUG
        ), priority = .5
      )
    )
  )

    
    

  # Change priorities  ------------------------------------------------------
    # We assign priorities so the target's progress is row-wise 
    # This way, a participant should finish before starting a new one, avoiding memory issues
    tar_make_future_rowwise(TARGETS = TARGETS, uids = parameters_monkeys_minimal$uid)

TARGETS
