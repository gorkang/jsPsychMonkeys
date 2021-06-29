
# GET Containers ----------------------------------------------------------

# debug_docker(1)
reconnect_to_VNC()
reconnect_to_VNC("container10", DEBUG = TRUE)


# CHECK META --------------------------------------------------------------

targets::tar_meta(fields = c(name, warnings)) %>% tidyr::drop_na(warnings)
targets::tar_meta(fields = c(name, seconds)) %>% tidyr::drop_na()
targets::tar_manifest()




# Visualize ---------------------------------------------------------------

# targets::tar_visnetwork(targets_only = TRUE, label = "time", exclude = "parameters_monkeys")
targets::tar_visnetwork(targets_only = TRUE, label = "time")
targets::tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE, label = "time") #, exclude = "parameters_monkeys"


# CLEAN UP ----------------------------------------------------------------

system('docker ps -a', intern = TRUE) # List containers

system('docker stop $(docker ps -q)') # KILL all docker instances
system('docker rm -v $(docker ps -a --format "{{.Names}}")') # KILL all docker images

targets::tar_destroy() # Destroy _targets folder
targets::tar_invalidate(matches("task_1")) # Invalidate specific target
