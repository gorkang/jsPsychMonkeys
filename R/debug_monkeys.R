# 
# debug_monkeys <- function() {
#   
# 
#   source("_targets.R") # parameters_monkeys_minimal
#   targets::tar_load(parameters_monkeys)
#   
#   uid = parameters_monkeys_minimal$uid[1]
#   
#   remoteDriver_UID = paste0("remoteDriver_", uid)
#   targets::tar_load(dplyr::all_of(remoteDriver_UID))
#   
#   
#   container_name = get(remoteDriver_UID)$container_name
#   remDr = get(remoteDriver_UID)$remDr
#   
#   assign("container_name", container_name, envir = .GlobalEnv)
#   assign("remDr", remDr, envir = .GlobalEnv)
# 
# }
