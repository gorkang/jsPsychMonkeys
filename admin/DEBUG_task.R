# Some of the plugins in Maker/admin/example_tasks fail
  # - audio-button-response
  # - html-keyboard-response
# SEE helper_select_input 396
# Maybe we need to send index to select_input() (from  interact_with_element), and
# get the choices available for keyboard-response plugins with questions[index].choices


# Clean -------------------------------------------------------------------
jsPsychMonkeys::clean_monkeys_containers()
targets::tar_destroy(ask = FALSE)


# new -----------------------------------------------------------------
# jsPsychMonkeys::clean_monkeys_containers()
devtools::load_all()
release_the_monkeys(uid = 1, local_folder_tasks = "/home/emrys/Downloads/protocolALL999", open_VNC = TRUE, disable_web_security = TRUE)



targets::tar_load_globals()
debug_function("complete_task")
debug_docker(uid_participant = uid)
# jsPsychMonkeys::reconnect_to_VNC()

reconnect_to_VNC(container_name = container_name)

# If No browser opened:
# remoteDriver = create_remDr(container_port = container_port, browserName = browserName, container_name = container_name, parameters_monkeys = parameters_monkeys)
# remDr = remoteDriver$remDr

# THIS IS A WIP, but seems to work
list_get_elements = get_elements(remDr = remDr, DEBUG = DEBUG); list_get_elements

# list_get_elements$DF_elements_options |> View()
# list_get_elements$name_contents |> View()
# list_get_elements$name_buttons |> View()


# FINALIZAR ESTUDIO >

# # In complete_task.R
# rstudioapi::navigateToFile("R/helper_select_input.R")
# rstudioapi::navigateToFile("R/interact_with_element.R")
# interact_with_element(list_get_elements = list_get_elements, index = 1, seed = 11)


targets::tar_load_globals()
tictoc::tic(msg = "GET")
list_get_elements = get_elements(remDr = remDr, try_number = 1, DEBUG = DEBUG)
tictoc::toc()
process_elements(list_get_elements = list_get_elements, try_number = 1, DEBUG = DEBUG)
tictoc::tic(msg = "INTERACT")
interact_with_element(list_get_elements = list_get_elements, index = 1, seed = 11, DEBUG = DEBUG)
tictoc::toc()


# old DEBUG TASK --------------------------------

targets::tar_load_globals()
targets::tar_load("parameters_monkeys")
debug_function("complete_task")

# If single participant in _targets.R
participant_to_debug = parameters_monkeys$participants$uid
# If more than one, select manually
# participant_to_debug = 1

debug_docker(uid_participant = participant_to_debug, parameters_debug = parameters_monkeys)
reconnect_to_VNC(container_name = paste0("monkey_", participant_to_debug))


list_get_elements = get_elements(remDr = remDr, DEBUG = DEBUG); list_get_elements


# In complete_task.R
rstudioapi::navigateToFile("R/helper_select_input.R")

source("R/interact_with_element.R")
rstudioapi::navigateToFile("R/interact_with_element.R")
interact_with_element(list_get_elements = list_get_elements, index = 1, seed = 11)
