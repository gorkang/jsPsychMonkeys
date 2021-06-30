
# SETUP -------------------------------------------------------------------

  # First time
  source("setup.R") 
  # If it fails, open _targets_packages.R, install all the packages there and rerun setup.R

  # On Ubuntu, install dependencies:
  # system("sudo apt install docker.io libssl-dev libcurl4-openssl-dev libxml2-dev")

  # targets::tar_renv()


  # Do you have Docker installed?
    # https://docs.docker.com/docker-for-windows/install/ [500 MB Download]

  # Define protocol parameters in _targets.R
    rstudioapi::navigateToFile("_targets.R")
    # IMPORTANT: `local_folder_tasks` # WHERE is the local protocol?


# Launch  -----------------------------------------------------------------

  targets::tar_watch(seconds = 5, outdated = FALSE, targets_only = TRUE) #, exclude = "parameters_monkeys"
  
  system('docker stop $(docker ps -q)') # KILL all docker instances
  targets::tar_destroy()
  targets::tar_make()


# Parallel ----------------------------------------------------------------

  targets::tar_watch(seconds = 5, outdated = FALSE, targets_only = TRUE, exclude = "parameters_monkeys")
  
  system('docker stop $(docker ps -q)') # KILL all docker instances
  targets::tar_destroy()
  targets::tar_make_future(workers = future::availableCores() - 2)

  