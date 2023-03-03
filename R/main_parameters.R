# If we give the input in parameters_monkeys_minimal, use that, else the function's default should be fine
# Thanks to this, we can just pass parameters_monkeys to the functions, instead of passing all the individual parameters

# Unnested, for set_parameters
if (exists("parameters_monkeys_minimal")) {

  # participants
  # if (!is.null(parameters_monkeys_minimal$uid)) uid = parameters_monkeys_minimal$uid

  # docker
  if (!is.null(parameters_monkeys_minimal$browserName)) browserName = parameters_monkeys_minimal$browserName
  if (!is.null(parameters_monkeys_minimal$big_container)) big_container = parameters_monkeys_minimal$big_container
  if (!is.null(parameters_monkeys_minimal$keep_alive)) keep_alive = parameters_monkeys_minimal$keep_alive
  if (!is.null(parameters_monkeys_minimal$folder_downloads)) folder_downloads = parameters_monkeys_minimal$folder_downloads
  if (!is.null(parameters_monkeys_minimal$credentials_folder)) credentials_folder = parameters_monkeys_minimal$credentials_folder

  # debug
  if (!is.null(parameters_monkeys_minimal$DEBUG)) DEBUG = parameters_monkeys_minimal$DEBUG
  if (!is.null(parameters_monkeys_minimal$screenshot)) screenshot = parameters_monkeys_minimal$screenshot
  if (!is.null(parameters_monkeys_minimal$debug_file)) debug_file = parameters_monkeys_minimal$debug_file
  if (!is.null(parameters_monkeys_minimal$console_logs)) console_logs = parameters_monkeys_minimal$console_logs
  if (!is.null(parameters_monkeys_minimal$open_VNC)) open_VNC = parameters_monkeys_minimal$open_VNC

  # remDr_params
  if (!is.null(parameters_monkeys_minimal$disable_web_security)) disable_web_security = parameters_monkeys_minimal$disable_web_security

  # task_params
  if (!is.null(parameters_monkeys_minimal$pid)) pid = parameters_monkeys_minimal$pid
  if (!is.null(parameters_monkeys_minimal$uid_URL)) uid_URL = parameters_monkeys_minimal$uid_URL
  if (!is.null(parameters_monkeys_minimal$initial_wait)) initial_wait = parameters_monkeys_minimal$initial_wait
  if (!is.null(parameters_monkeys_minimal$wait_retry)) wait_retry = parameters_monkeys_minimal$wait_retry
  if (!is.null(parameters_monkeys_minimal$forced_random_wait)) forced_random_wait = parameters_monkeys_minimal$forced_random_wait
  if (!is.null(parameters_monkeys_minimal$forced_refresh)) forced_refresh = parameters_monkeys_minimal$forced_refresh
  if (!is.null(parameters_monkeys_minimal$forced_seed)) forced_seed = parameters_monkeys_minimal$forced_seed
  if (!is.null(parameters_monkeys_minimal$local_or_server)) local_or_server = parameters_monkeys_minimal$local_or_server
  if (!is.null(parameters_monkeys_minimal$local_folder_tasks)) local_folder_tasks = parameters_monkeys_minimal$local_folder_tasks
  if (!is.null(parameters_monkeys_minimal$server_folder_tasks)) server_folder_tasks = parameters_monkeys_minimal$server_folder_tasks

}

# Nested, for all the other functions
if (exists("parameters_monkeys")) {

  # participants
  # if (!is.null(parameters_monkeys$participants$uid)) uid = parameters_monkeys$participants$uid

  # docker
  if (!is.null(parameters_monkeys$docker$browserName)) browserName = parameters_monkeys$docker$browserName
  if (!is.null(parameters_monkeys$docker$big_container)) big_container = parameters_monkeys$docker$big_container
  if (!is.null(parameters_monkeys$docker$keep_alive)) keep_alive = parameters_monkeys$docker$keep_alive
  if (!is.null(parameters_monkeys$docker$folder_downloads)) folder_downloads = parameters_monkeys$docker$folder_downloads
  if (!is.null(parameters_monkeys$docker$credentials_folder)) credentials_folder = parameters_monkeys$docker$credentials_folder

  # debug
  if (!is.null(parameters_monkeys$debug$DEBUG)) DEBUG = parameters_monkeys$debug$DEBUG
  if (!is.null(parameters_monkeys$debug$screenshot)) screenshot = parameters_monkeys$debug$screenshot
  if (!is.null(parameters_monkeys$debug$debug_file)) debug_file = parameters_monkeys$debug$debug_file
  if (!is.null(parameters_monkeys$debug$console_logs)) console_logs = parameters_monkeys$debug$console_logs
  if (!is.null(parameters_monkeys$debug$open_VNC)) open_VNC = parameters_monkeys$debug$open_VNC

  # remDr_params
  if (!is.null(parameters_monkeys$remDr_params$disable_web_security)) disable_web_security = parameters_monkeys$remDr_params$disable_web_security

  # task_params
  if (!is.null(parameters_monkeys$task_params$pid)) pid = parameters_monkeys$task_params$pid
  if (!is.null(parameters_monkeys$task_params$uid_URL)) uid_URL = parameters_monkeys$task_params$uid_URL
  if (!is.null(parameters_monkeys$task_params$initial_wait)) initial_wait = parameters_monkeys$task_params$initial_wait
  if (!is.null(parameters_monkeys$task_params$wait_retry)) wait_retry = parameters_monkeys$task_params$wait_retry
  if (!is.null(parameters_monkeys$task_params$forced_random_wait)) forced_random_wait = parameters_monkeys$task_params$forced_random_wait
  if (!is.null(parameters_monkeys$task_params$forced_refresh)) forced_refresh = parameters_monkeys$task_params$forced_refresh
  if (!is.null(parameters_monkeys$task_params$forced_seed)) forced_seed = parameters_monkeys$task_params$forced_seed
  if (!is.null(parameters_monkeys$task_params$local_or_server)) local_or_server = parameters_monkeys$task_params$local_or_server
  if (!is.null(parameters_monkeys$task_params$local_folder_tasks)) local_folder_tasks = parameters_monkeys$task_params$local_folder_tasks
  if (!is.null(parameters_monkeys$task_params$server_folder_tasks)) server_folder_tasks = parameters_monkeys$task_params$server_folder_tasks


}
