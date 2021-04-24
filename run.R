# TODO: 
# -SCRIPT]: In START [time]: 2021-04-22 10:18:23
# Usted ya ha completado todas las tareas de este protocolo.

# WARNINGS: 1: the condition has length > 1 and only the first element will be used 
# complete_task.R


# SETUP -------------------------------------------------------------------


# First time
# targets::tar_renv()


# CLEAN UP ----------------------------------------------------------------

targets::tar_destroy()
system('docker stop $(docker ps -q)') # KILL all docker instances

# system('docker ps -a', intern = TRUE) # List containers

# Launch  -----------------------------------------------------------------

  # PROTOCOL 999
  # complete_task_new 149.408 sec elapsed
  # complete_task_old 
  
  # PROTOCOL Downloads/test_prototol
  # complete_task_new 15.099 sec elapsed
  # complete_task_old 26.724 sec elapsed

  targets::tar_visnetwork(targets_only = TRUE, label = "time")
  
targets::tar_destroy()
system('docker stop $(docker ps -q)') # KILL all docker instances
targets::tar_make()

  tictoc::tic()
    targets::tar_destroy()
    targets::tar_make()
  tictoc::toc()
  
  debug_docker(24000)
  reconnect_to_VNC("container24000", DEBUG = TRUE)

  targets::tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)
  targets::tar_meta(fields = c(name, warnings))
  # targets::tar_meta(fields = c(name, seconds)) %>% tidyr::drop_na() %>% write.csv("NEW.csv")


# Parallel ----------------------------------------------------------------

  # FURRR inside get_elements2: 106.756 sec elapsed
  
  # PROTOCOL Downloads/test_prototol [11 workers]
    # complete_task_new: 54.998 sec elapsed / 58.946 sec elapsed / 55.065 sec elapsed
    # complete_task_old: 72.931 sec elapsed / 76.674 sec elapsed || 118.8 sec elapsed [con click inicial sin insertar contenido]
  
  targets::tar_destroy()
  system('docker stop $(docker ps -q)') # KILL all docker instances
  targets::tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)
  tictoc::tic()
  targets::tar_make_future(workers = 11) 
  tictoc::toc()

  
  
  
  #future::availableCores() - 1

# GET Containers ----------------------------------------------------------

  reconnect_to_VNC()
  reconnect_to_VNC("container24000", DEBUG = TRUE)
  
  