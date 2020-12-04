# Release a bunch of Monkeys


# Libraries and functions -------------------------------------------------

  source("R/run_participants.R")
  options(scipen=999)


# See and kill containers -------------------------------------------------

  system('docker ps -a', intern = TRUE) # List containers
  # system('docker stop $(docker ps -q)') # KILL all docker instances


# START ------------------------------------------------------------------

  source(".vault/SERVER_PATH.R")
  
  run_participants(starting_worker_id = 80000, 
                   number_workers = 200,
                   cpus_to_use = 10,
                   project_id = 3,
                   big_container = FALSE,
  
                   type_protocol = "single_link",
                   DEBUG = FALSE,
                   debug_file = FALSE,
                   open_VNC = FALSE,
                   local_or_server = "server", #"local", "server"
                   method = "parallel", # "single, "multiple", "parallel
                   local_folder_tasks = "Downloads/TASKS",
                   server_path = SERVER_PATH)
