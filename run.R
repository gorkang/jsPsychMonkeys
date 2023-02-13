# ADMIN -------------------------------------------------------------------

# See jsPsychHelper/admin/000_CHECK_canonical.R
# Check trialids, repeated names, etc.


# SETUP -------------------------------------------------------------------

  # First time
  source("setup.R")
  # If it fails, open _targets_packages.R, install all the packages there and rerun setup.R

  # See the jsPsychR-manual for details
  # https://gorkang.github.io/jsPsychR-manual/qmd/02-QuickGuide.html#setup
  


# SET PARAMETERS ----------------------------------------------------------

  # Define protocol parameters in _targets.R: 
    rstudioapi::navigateToFile("_targets.R")
    # e.g. `local_folder_tasks` = WHERE is the local protocol?


# Clean up ----------------------------------------------------------------

  # Stop all docker containers, etc.
  system('docker stop $(docker ps -q)') # KILL all docker instances
  system('docker ps -aq | xargs docker stop | xargs docker rm') # Alternative for Windows?
  system("docker system prune -f") # Cleans up system (stopped containers, etc.)
  # WINDOWS (delete all docker containers): 
  # FOR /f "tokens=*" %i IN ('docker ps -q') DO docker stop %i
  
  # Delete targets cache
  targets::tar_destroy(ask = FALSE)



# Launch  -----------------------------------------------------------------

  targets::tar_make() # Single CPU
  
  targets::tar_make_future(workers = future::availableCores() - 2) # Parallel. All minus 2 CPU's available

  
# Watch -------------------------------------------------------------------
  
  targets::tar_watch(seconds = 5, outdated = FALSE, targets_only = TRUE) #, exclude = "parameters_monkeys"
  
  