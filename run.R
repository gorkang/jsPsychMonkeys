# TODO: 
# -SCRIPT]: In START [time]: 2021-04-22 10:18:23
# Usted ya ha completado todas las tareas de este protocolo.

# WARNINGS: 1: the condition has length > 1 and only the first element will be used 
# complete_task.R


# CLEAN UP ----------------------------------------------------------------

targets::tar_destroy()
system('docker stop $(docker ps -q)') # KILL all docker instances



# Launch  -----------------------------------------------------------------

# tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)

tictoc::tic()
  # future::plan(future.callr::callr, workers = 10)
  future::plan(future::multisession, workers = 10)
  targets::tar_make_future(workers = 10)
tictoc::toc()

targets::tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)
targets::tar_visnetwork(targets_only = TRUE)

targets::tar_make()

# source("R/helper_functions.R")
reconnect_to_VNC()
reconnect_to_VNC("container20", DEBUG = TRUE)



targets::tar_destroy()

# system('docker ps -a', intern = TRUE) # List containers
# system('docker stop $(docker ps -q)') # KILL all docker instances


# TODO --------------------------------------------------------------------

# KILL docker when participant finishes
# Implement solution in regular monkeys!
# min(container_port)


# GET Containers ----------------------------------------------------------

source("R/helper_functions.R")
reconnect_to_VNC("container_20")

NAMES_CONTAINERS = system('docker ps -a --format "{{.Names}}"', intern = TRUE); NAMES_CONTAINERS
reconnect_to_VNC(NAMES_CONTAINERS[5])

container_name = NAMES_CONTAINERS[1]
container_port_raw <- system(sprintf('docker port %s', container_name), intern = TRUE)
container_port <- min(as.integer(gsub('.*:(.*)$', '\\1', container_port_raw)))
container_port
