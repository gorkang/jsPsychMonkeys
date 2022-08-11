# DEBUG TASK

targets::tar_load_globals()
debug_function("complete_task")
targets::tar_load("parameters_monkeys")

# If single participant in _targets.R
participant_to_debug = parameters_monkeys$participants$uid
# If more than one, select manually
# participant_to_debug = 1

debug_docker(uid_participant = participant_to_debug, parameters_debug = parameters_monkeys)
reconnect_to_VNC(container_name = paste0("container", participant_to_debug))


list_get_elements = get_elements(remDr = remDr, DEBUG = DEBUG); list_get_elements


# In complete_task.R
rstudioapi::navigateToFile("R/helper_select_input.R")

rstudioapi::navigateToFile("R/interact_with_element.R")
