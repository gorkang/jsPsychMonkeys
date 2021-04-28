
# SETUP -------------------------------------------------------------------

  # First time
  # targets::tar_renv()


# CLEAN UP ----------------------------------------------------------------

  targets::tar_invalidate(matches("task_24000"))
  targets::tar_destroy()
  system('docker stop $(docker ps -q)') # KILL all docker instances
  # system('docker ps -a', intern = TRUE) # List containers


# Visualize ---------------------------------------------------------------

  targets::tar_visnetwork(targets_only = TRUE, label = "time")
  targets::tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)
  
  
# Launch  -----------------------------------------------------------------

  targets::tar_destroy()
  system('docker stop $(docker ps -q)') # KILL all docker instances
  
  targets::tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)
  targets::tar_make()


# Parallel ----------------------------------------------------------------

  targets::tar_destroy()
  system('docker stop $(docker ps -q)') # KILL all docker instances
  
  targets::tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)
  targets::tar_make_future(workers = future::availableCores() - 2) 

  
# GET Containers ----------------------------------------------------------

  # debug_docker(24000)
  reconnect_to_VNC()
  reconnect_to_VNC("container24000", DEBUG = TRUE)
  

# CHECK META --------------------------------------------------------------

  targets::tar_meta(fields = c(name, warnings))
  targets::tar_meta(fields = c(name, seconds)) %>% tidyr::drop_na()
  
  