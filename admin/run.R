
# Clean up ----------------------------------------------------------------

  # Stop all docker containers, etc.
  active_containers = system('docker ps -q', intern = TRUE)
  active_containers |> purrr::walk(~system(paste0('docker stop ', .x)))
  system("docker system prune -f") # Cleans up system (stopped containers, etc.)

  # Delete targets cache
  targets::tar_destroy(ask = FALSE)


# Launch  -----------------------------------------------------------------

  targets::tar_make() # Single CPU

  # targets::tar_make_future(workers = future::availableCores() - 2) # Parallel. All minus 2 CPU's available


# Watch -------------------------------------------------------------------

  targets::tar_watch(seconds = 5, outdated = FALSE, targets_only = TRUE) #, exclude = "parameters_monkeys"

  # targets::tar_visnetwork(targets_only = TRUE)
