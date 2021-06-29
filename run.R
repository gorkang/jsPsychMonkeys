
# SETUP -------------------------------------------------------------------

  # First time
  targets::tar_renv()
  source("setup.R")

  # Do you have Docker installed?
    # https://docs.docker.com/docker-for-windows/install/ [500 MB Download]

  # Define protocol parameters in _targets.R
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

  