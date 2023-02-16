# new -----------------------------------------------------------------
targets::tar_load_globals()
debug_function("complete_task")
debug_docker(uid_participant = uid)
reconnect_to_VNC(container_name = container_name)

# If No browser opened:
# remoteDriver = create_remDr(container_port = container_port, browserName = browserName, container_name = container_name, parameters_monkeys = parameters_monkeys)
# remDr = remoteDriver$remDr

# THIS IS A WIP, but seems to work
list_get_elements = get_elements(remDr = remDr, DEBUG = DEBUG); list_get_elements
# 
# 
# # In complete_task.R
rstudioapi::navigateToFile("R/helper_select_input.R")
# 
source("R/interact_with_element.R")
rstudioapi::navigateToFile("R/interact_with_element.R")
interact_with_element(list_get_elements = list_get_elements, index = 1, seed = 11)






# old DEBUG TASK --------------------------------

targets::tar_load_globals()
targets::tar_load("parameters_monkeys")
debug_function("complete_task")

# If single participant in _targets.R
participant_to_debug = parameters_monkeys$participants$uid
# If more than one, select manually
# participant_to_debug = 1

debug_docker(uid_participant = participant_to_debug, parameters_debug = parameters_monkeys)
reconnect_to_VNC(container_name = paste0("container", participant_to_debug))


list_get_elements = get_elements(remDr = remDr, DEBUG = DEBUG); list_get_elements


# In complete_task.R
rstudioapi::navigateToFile("R/helper_select_input.R")

source("R/interact_with_element.R")
rstudioapi::navigateToFile("R/interact_with_element.R")
interact_with_element(list_get_elements = list_get_elements, index = 1, seed = 11)
