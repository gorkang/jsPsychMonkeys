#' check_trialids
#'
#'Checks that trialid's of an experiment in a folder follow the standard expected rules
#' @param local_folder_tasks local folders where the tasks are
#'
#' @return
#' @export
#'
#' @examples
check_trialids <- function(local_folder_protocol) {

  # suppressMessages(suppressWarnings(library(dplyr)))
  # suppressMessages(suppressWarnings(library(purrr)))
  # suppressMessages(suppressWarnings(library(readr)))

  scripts = dir(path = paste0(local_folder_protocol, "/tasks"), pattern = ".js", recursive = TRUE, full.names = TRUE)
  if (length(scripts) == 0) stop(paste("Can't find anything in ", local_folder_protocol))

  find_trialids <- function(file_name) {

    script = read_file(file_name)
    expres = ".*?trialid: '(.*?)'.*?"
    trialid = gsub(expres, "\\1; \n", script) %>% gsub("^(.*; \n).*", "\\1", .) %>% gsub(";", "", .) %>% gsub(" number \n", "", .)
    if (grepl("This document was made with test_maker", trialid)) trialid = ""
    strsplit(trialid, " \n")[[1]] %>% as_tibble() %>%
      mutate(file = file_name) %>%
      rename(trialid = value) %>%
      filter(!grepl("^Instructions|^Instructions_[0-9]{2}|^Fullscreen", trialid))

  }


  DF_all_trialids = map_df(scripts, find_trialids)

  rule_check_trialids = "^[a-zA-Z0-9]{1,100}_[0-9]{2,3}$|^[a-zA-Z0-9]{1,100}_[0-9]{2,3}_[0-9]{1,3}$" # NAME_001, NAMEexperiment_001_1
  DF_problematic_trialids =
    DF_all_trialids %>%
    filter(!grepl(rule_check_trialids, trialid)) %>%
    mutate(experiment = basename(file)) %>%
    select(-file)

  if (nrow(DF_problematic_trialids) > 0) {

    cat(crayon::red(nrow(DF_problematic_trialids), "ISSUES:\n"),
        "- experiment:", paste(DF_problematic_trialids %>% pull(experiment), collapse = ", "), "\n",
        "- trialid:   ", paste(DF_problematic_trialids %>% pull(trialid), collapse = ", "), "\n")

  } else {
    cat(crayon::green("All trialid's look great!\n"))
  }
}


#' check_accept_alert
#' Safely checks if there in an alert and accepts it
#'
#' @param wait_retry In seconds, how much to wait before retrying
#' @param remDr remDr object
#' @param DEBUG TRUE/FALSE
#'
#' @return
#' @export
check_accept_alert <- function(wait_retry = .5, remDr, DEBUG) {

  get_alert <- function(variables) {
    MESSAGE = remDr$getAlertText()
    remDr$acceptAlert()
    return(MESSAGE)
  }

  get_alert_safely = purrr::safely(get_alert)
  RESP = suppressMessages(get_alert_safely())
  while (!is.null(RESP$result)) {
    if (DEBUG == TRUE) withr::with_options(list(crayon.enabled = FALSE), cat(crayon::yellow("[Alert] found, waiting", wait_retry, "seconds: ", crayon::silver(RESP[[1]]), "\n")))
    Sys.sleep(wait_retry)

    RESP = suppressMessages(get_alert_safely())
  }


}


#' reconnect_to_VNC
#'
#' @param container_name if empty, show available containers
#' @param just_check FALSE / TRUE
#' @param port No need to specify one
#' @param DEBUG TRUE/FALSE
#'
#' @return
#' @export
#'
#' @examples reconnect_to_VNC()
reconnect_to_VNC <- function(container_name = NULL, just_check = FALSE, port = NULL, DEBUG = FALSE) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function("complete_task")
  # container_name = paste0("container", uid)
  # just_check = TRUE
  # port = NULL
  # DEBUG = TRUE

  if (is.null(container_name)) {
    # List containers

    system('docker ps -a --format "{{.Names}}"', intern = TRUE) # Print container names

  } else {

    if (just_check == TRUE) {
      # Check if container exists

      # Outputs the name of the container if it exists, NULL otherwise
      containers_available = system('docker ps -a --format "{{.Names}}"', intern = TRUE) # Print container names

      if (!container_name %in% containers_available) {
        NULL
      } else {
        container_name
      }


    } else {
      # Connect to container
      # If container is a debug container, we can connect
      if (any(grepl("debug", system('docker ps -a --format "{{.Image}}"', intern = TRUE)))) {

        if (!is.null(port)) {

          container_port = port

        } else {

          # Get container port
          container_port_raw <- system(sprintf('docker port %s', container_name), intern = TRUE)
          container_port_raw_clean = container_port_raw[grepl(".*0.0.0.0:([0-9]{5})$", container_port_raw)]

          container_ports_found = as.integer(gsub('.*:(.*)$', '\\1', container_port_raw_clean))
          if (length(container_ports_found) > 2) cli::cli_alert_warning("Multiple ports found: {container_ports_found}. \n - If the VNC command suplied below does not work, try the other ports.")
          container_port <- min(container_ports_found)
          if (is.na(container_port[1])) cat(crayon::red("Port not found?"))

        }

        # Open VNC, using second port in container_port, the password is 'secret'
        if (Sys.info()["sysname"] == "Windows") {
          vnc_program = '"C:/Program Files/RealVNC/VNC Viewer/vncviewer.exe"'
          end_command = ""
        } else {
          vnc_program = "vncviewer"
          end_command = " &"

        }

        vnc_command = paste0(vnc_program, ' 127.0.0.1:', container_port)


        # cat(crayon::yellow(paste0("\nOpen VNC | localhost:", container_port, " | pwd: secret\n"), crayon::black(vnc_command, "\n")))
        cli::cli_alert_info("Open VNC | localhost: {container_port} | pwd: secret")
        cli::cli_alert_info("In a terminal: {.code {vnc_command}}")

        # if (DEBUG == TRUE) cat(crayon::silver(" \nDEBUG:", container_port_raw), "\n\n")
        if (DEBUG == TRUE) cli::cli_alert_info("DEBUG: {paste(container_port_raw, sep = '\n')}")

        # If Using callr::r_bg() or callr::r() ERROR in task target
        # TODO: This could run with Windows if VNCviewer is installed
        if (Sys.info()["sysname"] != "Windows") system(paste0(vnc_command, end_command))


      } else {

        cat(crayon::bgRed("Docker container NOT in debug mode.\n"), crayon::silver("To connect, a container needs to be launched with the DEBUG=TRUE in _targets.R\n\n"))

      }
    }
  }

}


#' debug_docker
#'
#' @param uid_participant uid of participant
#'
#' @return
#' @export
#'
#' @examples debug_docker(24000)
debug_docker <- function(uid_participant) {

  # DEBUG
  # uid_participant = 92
  # parameters_monkeys = parameters_monkeys

  # suppressMessages(source(shrtcts::locate_shortcuts_source()))
  suppressMessages(source(here::here("_targets.R")))

  targets::tar_load(parameters_monkeys)
  source("R/main_parameters.R")

  # Extract parameters from parameters_monkeys
  # DEBUG <<- parameters_debug$debug$DEBUG
  # screenshot <<- parameters_debug$debug$screenshot
  # debug_file <<- parameters_debug$debug$debug_file
  # open_VNC <<- parameters_debug$debug$open_VNC
  # parameters_task <<- parameters_debug
  # uid <<- uid_participant


  # targets::tar_manifest()
  # targets::tar_meta()

  container_name_tar <- paste0("container_", uid_participant)
  container_name <<- paste0("container", uid_participant)
  driver_name <<- paste0("remote_driver_", uid_participant)
  targets::tar_load(eval(container_name_tar))
  targets::tar_load(eval(driver_name))
  remDr <<- get(driver_name)$remDr

  cli::cli_alert_info("Loaded {container_name_tar}")

}



#' debug_function
#'
#' Loads the parameters used in the functions present in _targets.R to make debugging easier
#'
#' @param name_function Name of function to debug
#'
#' @return
#' @export
#'
#' @examples
debug_function <- function(name_function, uid = NULL) {

  # DEBUG
  # name_function = "create_docker"

  # Function to tar_load or assign the parameters
  load_parameters <- function(parameters_function_separated, NUM) {

    # NUM = 4
    parameter = parameters_function_separated[[NUM]]
    parameter_1 = gsub("\\n", "", parameter[1])
    parameter_2 = gsub("\\n", "", parameter[2])

    if (is.na(parameter_2)) {
      cli::cli_alert_info("processing {.code {parameter}}")
    } else {
      cli::cli_alert_info("processing {.code {parameter_1} = {parameter_2}}")
    }


    # A single parameter e.g. parameters_monkeys (must be an existing object)
    if (length(parameter) == 1) {

      targets::tar_load(all_of(existing_object), envir = .GlobalEnv)

    # A parameter with "=" e.g: pid = 999
    } else if (length(parameter) == 2) {

      # If parameter_2 is an existing object, load
      existing_object = parameter_2[parameter_2 %in% targets::tar_objects()]

      # If it's in tar_objects(), load
      if (length(existing_object) == 1) {

        targets::tar_load(all_of(existing_object), envir = .GlobalEnv)
        assign(parameter_1, eval(parse(text = parameter_2)), envir = .GlobalEnv)

      } else {

      # If the variable contains $, it is a list, so load the actual value stored
      if (grepl("\\$", parameter_2)) {

        assign(parameter_1, eval(parse(text = parameter_2)), envir = .GlobalEnv)

      # Else, just assign it
      } else {

        assign(parameter_1, parameter_2, inherits = TRUE, envir = .GlobalEnv)

      }


      }
    }
  }


  # Makes possible to use prepare_TASK or "prepare_TASK"
  if (substitute(name_function) != "name_function") name_function = substitute(name_function) #if (!interactive()) is so substitute do not overwrite name_function when in interactive mode

  # Parses _targets.R
  # code <- parse("_targets.R")
  # if (file.exists("targets/targets_main.R")) code <- c(code, parse("targets/targets_main.R"))

  # Finds the chunk where name_function is, and cleans the "\"
  # text_targets = grep(name_function, code, value = TRUE) %>% gsub("[^A-Za-z0-9\\(\\),_= ]", "", .)

  # Gets and separates then parameters of the function
  # parameters_function_raw = gsub(paste0(".*", name_function, "\\((.*?)).*"), "\\1", text_targets) %>% gsub(" ", "", .)

  # if (length(parameters_function_raw) > 0) {

    # parameters_function_separated = strsplit(parameters_function_raw, ",") %>% unlist() %>% strsplit(., "=")

  # If there are multiple uid's we need a way to filter
  if (is.null(uid)) uid = "*"

  DF_function = targets::tar_manifest() %>%
    dplyr::filter(grepl(name_function, command)) %>%
    dplyr::filter(grepl(uid, name)) |>
    dplyr::pull(command) %>% dplyr::last()

  if (!is.na(DF_function)) {

    string_function = gsub(name_function, "", DF_function) %>% gsub(" |\\\\n|\\(|\\)", "", .)
    LOADME = stringr::str_extract_all(string_function, "=.*?[,\\$]", simplify = TRUE) %>% gsub("\\$|,|=", "", .) %>% as.vector() %>% unique()
    existing_objects = LOADME[LOADME %in% targets::tar_objects()]
    if (length(existing_objects) > 0) targets::tar_load(all_of(existing_objects))

    parameters_function_separated = strsplit(string_function, ",") %>% unlist() %>% strsplit(., "=")

    # For each of the parameters, applies the load_parameters() function
    seq_along(parameters_function_separated) %>%
      purrr::walk(~
             load_parameters(parameters_function_separated, NUM = .x)
           )

    cat(crayon::green("Loaded: "), gsub(",", ", ", name_function), "\n")

  } else {

    cat(crayon::red(paste0("'", name_function, "'", "not found in _targets.R")), "\n")

  }
}




#' Makes tar_make_future() work row-wise by assigning priorities to targets using their uid. This allows you to run as many participants as you want, avoiding memory issues.
#'
#' @param TARGETS targets list
#' @param uids user id's
#'
#' @return a list of targets
tar_make_future_rowwise <- function(TARGETS, uids) {

  # TODO: The number in TARGETS[[2]] depends on the position of the tar_map() target!

  assign_priority <- function(uid, target_name) {
    # browser()
    uid_container = gsub(".*_(.*)", "\\1", TARGETS[[2]][[target_name]][[uid]][["settings"]][["name"]])
    prioriry_container = as.numeric(uid_container) / max(uids)
    TARGETS[[2]][[target_name]][[uid]][["settings"]][["priority"]] = prioriry_container

  }

  1:length(uids) %>%
    purrr::walk(~{
      index_uid = .x
      1:length(names(TARGETS[[2]])) %>%
        purrr::walk(~{
          index_target = .x
          # cat("uid = ", uid_walk, "name = ", .x)
          assign_priority(uid = index_uid, target_name = index_target)
        })
    })

  # TARGETS[[1]]$container[[1]]$settings$priority

}


#' Check csv files in a folder
#'
#' @param pid project id
#' @param download_folder
#'
#' @return
#' @export
#'
#' @examples
check_Downloads <- function(parameters_monkeys, uid = "", links_tar = "") {

  # DEBUG
  # parameters_monkeys
  # uid = "3"

  pid = parameters_monkeys$task_params$pid
  download_folder = parameters_monkeys$docker$folder_downloads
  local_or_server = parameters_monkeys$task_params$local_or_server
  DEBUG = parameters_monkeys$debug$DEBUG


  # Do this only in local protocols
  if (local_or_server == "local") {

    folder_protocol = basename(parameters_monkeys$task_params$local_folder_tasks)

    # uid is used in copy_files_to_data so only that participant's data is copyied
    if (DEBUG == TRUE) cli::cli_alert_info(paste0("Looking for files for pid = {pid} (/{folder_protocol}) and uid = {uid}"))
    files_downloaded = list.files(download_folder, pattern = paste0("^", pid, ".*", uid, "\\.csv"))

    # If no files are found when looking for files for a specific uid, try harder
    if (length(files_downloaded) == 0 & uid != "") {

      all_csv_found = list.files(download_folder, pattern = paste0("^.*", uid, "\\.csv"))

      if (length(all_csv_found) > 0) {
        pid_found = unique(gsub("([0-9]{1,10})_.*", "\\1", all_csv_found))

        cli::cli_h1("No csv files found using pid = {pid} and uid = {uid}")
        cli::cli_alert_info("Ignoring pid = {pid}, I found {length(all_csv_found)} files: \n{all_csv_found}\n")

        cli::cli_alert_danger("You should rename the folder_protocol {.code {folder_protocol}} so the numeric part ({parameters_monkeys$task_params$pid}) matches the `pid` in config.js!")
      } else {
        cli::cli_alert_warning("No csv files found using uid = {uid}")
      }

    }

    return(files_downloaded)
  }

}

#' In local protocols, move the Downloaded csv to the data folder of the protocol
#'
#' @param pre_existing_CSV
#' @param parameters_monkeys
#' @param uid
#' @param task
#'
#' @return
#' @export
#'
#' @examples
copy_files_to_data <- function(pre_existing_CSV, parameters_monkeys, uid, task = task) {

  if (parameters_monkeys$task_params$local_or_server == "local") {

    # Create data folder
    local_folder_tasks_data = paste0(parameters_monkeys$task_params$local_folder_tasks, "/.data")
    if(!dir.exists(local_folder_tasks_data)) dir.create(local_folder_tasks_data)

    # Check final files in data folder
    final_files = check_Downloads(parameters_monkeys = parameters_monkeys, uid = uid)

    # Prepare variables
    final_files_clean = final_files[!final_files %in% pre_existing_CSV]

    if (length(final_files_clean) == 0) {

      cli::cli_alert_warning("NO files to copy:
                             - pid is {parameters_monkeys$task_params$pid}
                             - uid is {uid}
                             - local folder is: {parameters_monkeys$task_params$local_folder_tasks}")
    }

    from_files = paste0(parameters_monkeys$docker$folder_downloads, "/", final_files_clean)
    to_files = paste0(local_folder_tasks_data, "/", final_files_clean)

    # Rename/Move to final location
    # cli::cli_alert_info("{from_files}, to {to_files}")

    file.rename(from = from_files,
                to = to_files)

    cli::cli_alert_info("{length(final_files_clean)} files moved to {.code {local_folder_tasks_data}}")

  }
}



#' Parse website elements to extract and tabulate all the properties
#'
#' @param what a html tag such as c("p", "textarea", "input", "div", ...)
#'
#' @return
#' @export
#'
#' @examples
parse_elements <- function(what, page_source_rvest) {
  # what = "p"
  page_source_temp = page_source_rvest |> rvest::html_elements(what)

  if (what == "textarea") what = "input" # We consider textarea a type of input

  if (length(page_source_temp) > 0) {

    1:length(page_source_temp)  |>
      purrr::map_df(~ page_source_temp[[.x]]  |>
                      rvest::html_attrs() |>
                      dplyr::bind_rows() |>
                      dplyr::mutate(tag_name = what,
                                    content =
                                      page_source_temp[[.x]] |>
                                      rvest::html_text2() # Here the <big>TITLE</big> is lost
                                    )
                    )

  } else {
    # cli::cli_alert_info("No {.code {what}} found")
    NULL
  }

}

# Navigate to link
launch_task <- function(links, wait_retry, remDr, DEBUG) {
  if (length(links) != 1) stop("links passed to remDr$navigate are != 1")
  Sys.sleep(wait_retry)
  remDr$navigate(links)
}



#' Create _targets.R file from a protocol folder
#'
#' @param pid project id
#' @param folder protocol folder
#' @param dont_ask do everything without asking for user input
#' @param uid
#' @param browserName
#' @param big_container
#' @param keep_alive
#' @param folder_downloads
#' @param DEBUG
#' @param screenshot
#' @param debug_file
#' @param console_logs
#' @param open_VNC
#' @param uid_URL
#' @param local_or_server
#' @param local_folder_tasks
#' @param server_folder_tasks
#' @param disable_web_security
#' @param initial_wait
#' @param wait_retry
#' @param forced_random_wait
#' @param forced_refresh
#' @param forced_seed
#'
#' @return Creates a _targets.R file
#' @export
create_targets_file <- function(folder = "~/Downloads/",
                                uid = 1,
                                browserName = "chrome",
                                big_container = FALSE,
                                keep_alive = FALSE,
                                folder_downloads = NULL,
                                DEBUG = FALSE,
                                screenshot = FALSE,
                                debug_file = FALSE,
                                console_logs = FALSE,
                                open_VNC = FALSE,
                                pid = NULL,
                                uid_URL = TRUE,
                                local_or_server = NULL, # ["local", "server", "test"]
                                local_folder_tasks = NULL, # ["Downloads/tests/test_prototol", "Downloads/tests/2"]
                                server_folder_tasks = NULL,
                                disable_web_security = FALSE,
                                initial_wait = 2,
                                wait_retry = .1,
                                forced_random_wait = FALSE,
                                forced_refresh = NULL,
                                forced_seed = NULL,
                                dont_ask = FALSE) {


  # folder = "~/Downloads/"
  # uid = 1
  # browserName = "chrome"
  # big_container = FALSE
  # keep_alive = FALSE
  # folder_downloads = NULL
  # DEBUG = FALSE
  # screenshot = FALSE
  # debug_file = FALSE
  # console_logs = FALSE
  # open_VNC = FALSE
  # pid = NULL
  # uid_URL = TRUE
  # local_folder_tasks = NULL
  # server_folder_tasks = "999"
  # disable_web_security = FALSE
  # initial_wait = 2
  # wait_retry = .1
  # forced_random_wait = FALSE
  # forced_refresh = NULL
  # forced_seed = NULL
  # folder = NULL
  # dont_ask = FALSE


  # Prepare all parameters
  if (!is.null(local_folder_tasks)) FOLDER_TASKS = glue::glue('local_folder_tasks = "{local_folder_tasks}"') # No comma at the end because will be the last element
  if (!is.null(server_folder_tasks)) FOLDER_TASKS = glue::glue('server_folder_tasks = "{server_folder_tasks}"')# No comma at the end because will be the last element

  if (!is.null(uid)) string_uid = glue::glue('uid = {dput(uid)},')
  if (!is.null(browserName)) string_browserName = glue::glue('browserName = "{browserName}",')
  if (!is.null(big_container)) string_big_container = glue::glue('big_container = {big_container},')
  if (!is.null(keep_alive)) string_keep_alive = glue::glue('keep_alive = {keep_alive},')
  if (!is.null(folder_downloads)) string_folder_downloads = glue::glue('folder_downloads = "{folder_downloads}",')
  if (!is.null(DEBUG)) string_DEBUG = glue::glue('DEBUG = {DEBUG},')
  if (!is.null(screenshot)) string_screenshot = glue::glue('screenshot = {screenshot},')
  if (!is.null(debug_file)) string_debug_file = glue::glue('debug_file = {debug_file},')
  if (!is.null(console_logs)) string_console_logs = glue::glue('console_logs = {console_logs},')
  if (!is.null(open_VNC)) string_open_VNC = glue::glue('open_VNC = {open_VNC},')
  if (!is.null(pid)) string_pid = glue::glue('pid = "{pid}",')
  if (!is.null(uid_URL)) string_uid_URL = glue::glue('uid_URL = {uid_URL},')
  if (!is.null(disable_web_security)) string_disable_web_security = glue::glue('disable_web_security = {disable_web_security},')
  if (!is.null(initial_wait)) string_initial_wait = glue::glue('initial_wait = {initial_wait},')
  if (!is.null(wait_retry)) string_wait_retry = glue::glue('wait_retry = {wait_retry},')
  if (!is.null(forced_random_wait)) string_forced_random_wait = glue::glue('forced_random_wait = {forced_random_wait},')
  if (!is.null(forced_refresh)) string_forced_refresh = glue::glue('forced_refresh = {forced_refresh},')
  if (!is.null(forced_seed)) string_forced_seed = glue::glue('forced_seed = {forced_seed},')
  if (!is.null(dont_ask)) string_dont_ask = glue::glue('dont_ask = {dont_ask},')


  # Make sure not parameter is empty
  string_uid = ifelse(exists("string_uid"), string_uid, "")
  string_browserName = ifelse(exists("string_browserName"), string_browserName, "")
  string_big_container = ifelse(exists("string_big_container"), string_big_container, "")
  string_keep_alive = ifelse(exists("string_keep_alive"), string_keep_alive, "")
  string_folder_downloads = ifelse(exists("string_folder_downloads"), string_folder_downloads, "")
  string_DEBUG = ifelse(exists("string_DEBUG"), string_DEBUG, "")
  string_screenshot = ifelse(exists("string_screenshot"), string_screenshot, "")
  string_debug_file = ifelse(exists("string_debug_file"), string_debug_file, "")
  string_console_logs = ifelse(exists("string_console_logs"), string_console_logs, "")
  string_open_VNC = ifelse(exists("string_open_VNC"), string_open_VNC, "")
  string_pid = ifelse(exists("string_pid"), string_pid, "")
  string_uid_URL = ifelse(exists("string_uid_URL"), string_uid_URL, "")
  string_disable_web_security = ifelse(exists("string_disable_web_security"), string_disable_web_security, "")
  string_initial_wait = ifelse(exists("string_initial_wait"), string_initial_wait, "")
  string_wait_retry = ifelse(exists("string_wait_retry"), string_wait_retry, "")
  string_forced_random_wait = ifelse(exists("string_forced_random_wait"), string_forced_random_wait, "")
  string_forced_refresh = ifelse(exists("string_forced_refresh"), string_forced_refresh, "")
  string_forced_seed = ifelse(exists("string_forced_seed"), string_forced_seed, "")
  string_dont_ask = ifelse(exists("string_dont_ask"), string_dont_ask, "")


    # Read template
    template = readLines(paste0(folder, "/inst/templates/_targets_TEMPLATE.R"))

    # Create parameters_monkeys_minimal code
    new_parameters_monkeys_minimal =
      glue::glue('
    parameters_monkeys_minimal = list(
               {string_uid}
               {string_browserName}
               {string_big_container}
               {string_keep_alive}
               {string_folder_downloads}
               {string_DEBUG}
               {string_screenshot}
               {string_debug_file}
               {string_console_logs}
               {string_open_VNC}
               {string_pid}
               {string_uid_URL}
               {string_disable_web_security}
               {string_initial_wait}
               {string_wait_retry}
               {string_forced_random_wait}
               {string_forced_refresh}
               {string_forced_seed}
               {string_dont_ask}
               {FOLDER_TASKS})
    ')


    # Include parameters_monkeys_minimal
    final_file = gsub("#PARAMETERS_HERE", new_parameters_monkeys_minimal, template)


    # Create final file
    cat(final_file, file = paste0(folder, "/_targets_automatic_file.R"), sep = "\n")


  # If previous step was successful
  if (file.exists(paste0(folder, "/_targets_automatic_file.R"))) {

    if (dont_ask == FALSE) {

      response_prompt = menu(choices = c("YES", "No"),
                             title =  jsPsychHelpeR::cli_message(var_used = new_parameters_monkeys_minimal,
                                                 info = "{cli::style_bold((cli::col_yellow('Overwrite')))} file '_targets.R' to include the following parameters?",
                                                 details = "{new_parameters_monkeys_minimal}"))
    } else {

      jsPsychHelpeR::cli_message(var_used = new_parameters_monkeys_minimal,
                  info = "{cli::style_bold((cli::col_yellow('Overwriten')))} file '_targets.R' to include the following parameters",
                  details = "{new_parameters_monkeys_minimal}")

      response_prompt = 1

    }

    if (response_prompt == 1) {

      # Create Backup file
      if (file.exists(paste0(folder, "/_targets.R"))) file.rename(from = paste0(folder, "/_targets.R"), to = paste0(folder, "/_targets_old.R"))

      # RENAME _targets_automatic_file.R as _targets.R. _targets_automatic_file.R was created in the previous step
      file.rename(from = paste0(folder, "/_targets_automatic_file.R"), to = paste0(folder, "/_targets.R"))



      # END Message
      jsPsychHelpeR::cli_message(h2_title = "All done",
                  success = "NEW '_targets.R' created"
      )


    } else {
      cli::cli_alert_warning("OK, nothing done\n")
    }

  }

}


#' setup_folders
#' Extract jsPsychMonkeys.zip and make sure we have all necessary folders in a specific location
#'
#' @param folder destination folder for the project
#' @param extract_zip If TRUE, extracts jsPsychMonkeys.zip to folder
#'
#' @return NULL
#' @export
#'
#' @examples setup_folders(tempdir())
setup_folders <- function(folder, extract_zip = FALSE) {

  # TODO: ADD check about empty folder and ASK user if we should delete contents

  # Avoid spaces in folder path because other functions (e.g. update_data) won't work if there are spaces
  if (grepl(" ", folder)) cli::cli_abort("The folder path should NOT have spaces. You can replace {.code {folder}} for {.code {gsub(' ', '', folder)}}")

  if (extract_zip == TRUE) {
    # Make sure folder exists and extract jsPsychMonkeys.zip there
    if (!dir.exists(folder)) dir.create(folder)

    # Location of jsPsychMonkeys.zip
    if ("jsPsychMonkeys" %in% utils::installed.packages()) {
      jsPsychMonkeys_zip = system.file("templates", "jsPsychMonkeys.zip", package = "jsPsychMonkeys")
    } else {
      jsPsychMonkeys_zip = "inst/templates/jsPsychMonkeys.zip"
    }

    utils::unzip(jsPsychMonkeys_zip, exdir = folder)
    cli::cli_alert_success("jsPsychMonkeys project extracted to {.code {folder}}\n")

  }

  # Necessary folders
  # necessary_folders = c(paste0("data/", pid), # data/manual_correction
  #                       "outputs/backup", "outputs/data", "outputs/plots", "outputs/reliability", "outputs/reports", "outputs/tables", "outputs/tests_outputs",
  #                       ".vault/data_vault", ".vault/Rmd", ".vault/outputs/data", ".vault/outputs/reports")
  #
  necessary_folders = c(".vault", "outputs/DF", "outputs/errors", "outputs/log", "outputs/screenshots", "outputs/source")


  if (all(necessary_folders %in% dir(folder, recursive = TRUE, include.dirs = TRUE, all.files = TRUE))) {
    cli::cli_alert_success("All the necessary folders are present\n")
  } else {
    invisible(purrr::map(paste0(folder, "/", necessary_folders), dir.create, recursive = TRUE, showWarnings = FALSE))
    system(paste0("chmod 700 -R ", folder, "/.vault/"))
    cli::cli_alert_success("Created necessary folders: {.pkg {necessary_folders}}\n")
  }

}

#' Create a jsPsychMonkeys project for your data
#'
#' run_initial_setup() will read your data and create a jsPsychMonkeys project
#' tailoring the _targets.R file to the tasks included in the data.
#'
#' @param pid project id
#' @param dont_ask answer YES to all questions so the process runs uninterrupted. This will:
#' @param folder location for the project
#' @param uid
#' @param browserName
#' @param big_container
#' @param keep_alive
#' @param folder_downloads
#' @param DEBUG
#' @param screenshot
#' @param debug_file
#' @param console_logs
#' @param open_VNC
#' @param uid_URL
#' @param local_or_server
#' @param local_folder_tasks
#' @param server_folder_tasks
#' @param disable_web_security
#' @param initial_wait
#' @param wait_retry In seconds, how much to wait before retrying
#' @param forced_random_wait
#' @param forced_refresh
#' @param forced_seed
#' @param open_rstudio Open RStudio with the new project TRUE / FALSE
#' @param credentials_folder
#'
#' @return Opens a new RStudio project
#' @export
#' @examples
#' run_initial_setup(pid = 999, download_files = FALSE,
#' data_location = system.file("extdata", package = "jsPsychMonkeys"),
#' download_task_script = FALSE,
#' folder = tempdir(),
#' sensitive_tasks = c(""), dont_ask = TRUE, open_rstudio = FALSE)
create_monkeys_project <- function(folder = "~/Downloads/",
                                   credentials_folder = NULL,
                                   uid = 1,
                                   browserName = "chrome",
                                   big_container = FALSE,
                                   keep_alive = FALSE,
                                   folder_downloads = NULL,
                                   DEBUG = FALSE,
                                   screenshot = FALSE,
                                   debug_file = FALSE,
                                   console_logs = FALSE,
                                   open_VNC = FALSE,
                                   pid = NULL,
                                   uid_URL = TRUE,
                                   local_or_server = NULL, # ["local", "server", "test"]
                                   local_folder_tasks = NULL, # ["Downloads/tests/test_prototol", "Downloads/tests/2"]
                                   server_folder_tasks = NULL,
                                   disable_web_security = FALSE,
                                   initial_wait = 2,
                                   wait_retry = .1,
                                   forced_random_wait = FALSE,
                                   forced_refresh = NULL,
                                   forced_seed = NULL,
                                   dont_ask = FALSE,
                                   open_rstudio = TRUE) {

  # CHECKS
  if (dont_ask == TRUE) response_prompt = 1


  # ASK FOR USER PERMISSION
  if (dont_ask == FALSE)  response_prompt = menu(choices = c("Yes", "No"),
                                                 title =
                                                   jsPsychHelpeR::cli_message(h1_title = "Initial SETUP",
                                                               info = "Do you want to run the {.pkg initial setup}?",
                                                               details = "This will {cli::style_bold((cli::col_green('install')))} necessary packages,
                                                                         {cli::style_bold((cli::col_green('copy')))} configuration files,
                                                                         {cli::style_bold((cli::col_yellow('replace')))} the _targets.R, etc."))



  if (response_prompt == 1) {

    # 1) Run to make sure you have all the necessary packages and folders -------

    jsPsychHelpeR::cli_message(h1_title = "Setup")
    # setup_folders(pid = pid, folder = folder, extract_zip = TRUE)
    setup_folders(folder = folder, extract_zip = TRUE)

    # Copy credentials files to new project
    if (!is.null(credentials_folder)) {

      cli::cli_h1("Copying credentials")
      FILES_temp =  c("SERVER_PATH.R", ".credentials")
      FILES_to_COPY = c(paste0(credentials_folder, FILES_temp))
      FILES_DESTINATION = c(paste0(folder, "/.vault/", FILES_temp))

      cli::cli_inform("FILES_to_COPY: {FILES_to_COPY} \n
                      FILES_DESTINATION: {FILES_DESTINATION}")

      file.copy(from = FILES_to_COPY, to = FILES_DESTINATION)
    }


    # 2) Create a _targets.R file for your data -------------------------------

    jsPsychHelpeR::cli_message(var_used = folder, h1_title = "Create _targets.R file in {.code {folder}}")

    create_targets_file(folder = folder,
                        uid = uid,
                        browserName = browserName,
                        big_container = big_container,
                        keep_alive = keep_alive,
                        folder_downloads = folder_downloads,
                        DEBUG = DEBUG,
                        screenshot = screenshot,
                        debug_file = debug_file,
                        console_logs = console_logs,
                        open_VNC = open_VNC,
                        pid = pid,
                        uid_URL = uid_URL,
                        local_or_server = local_or_server,
                        local_folder_tasks = local_folder_tasks,
                        server_folder_tasks = server_folder_tasks,
                        disable_web_security = disable_web_security,
                        initial_wait = initial_wait,
                        wait_retry = wait_retry,
                        forced_random_wait = forced_random_wait,
                        forced_refresh = forced_refresh,
                        forced_seed = forced_seed,
                        dont_ask = dont_ask)




    # Copy tests to tests/testthat/
    # tests_templates_origin = list.files(paste0(folder, "/inst/templates/tests"), full.names = TRUE, recursive = TRUE)
    # tests_templates_destination = gsub("inst/templates/", "", tests_templates_origin)
    # # folder_destination_snaps = paste0(folder, "/tests/testthat/_snaps/snapshots/") # Create needed folders
    # if(!dir.exists(folder_destination_snaps)) dir.create(folder_destination_snaps, recursive = TRUE)
    # file.copy(tests_templates_origin, tests_templates_destination, overwrite = TRUE)

    # Open RStudio project
    if (Sys.getenv("RSTUDIO") == "1" & open_rstudio == TRUE) {
      jsPsychHelpeR::cli_message(var_used = folder, info = "Opening new RStudio project {.code {folder}}")
      rstudioapi::openProject(folder, newSession = TRUE)
    } else {
      cli::cli_alert_success("The new RStudio project is in {.code {folder}}")
    }


  } else {

    cli::cli_alert_warning("OK, nothing done")

  }
}





#' List files in the .data folder in the server
#'
#' @param pid protocol id
#' @param list_credentials list with credentials
#'
#' @return A tibble with a list of files
#' @export
list_data_server <- function(pid, list_credentials = NULL) {

  # pid = "999"

  # list_credentials = NULL

  # Parameters ---
  dry_run = " --dry-run "

  # Check and prepare local folder and path
  # if (!file.exists(local_folder)) dir.create(local_folder)
  local_folder = normalizePath(here::here("."))
  local_folder_terminal = gsub(" ", "\\\\ ", local_folder)


  # CHECKS we have credentials and necessary software ---

  # Get server credentials
  if (is.null(list_credentials)) {
    list_credentials = source(here::here(".vault/.credentials"))
    credentials_exist = file.exists(here::here(".vault/.credentials"))
  } else {
    credentials_exist = TRUE
  }


  # credentials_exist = file.exists(here::here(".vault/.credentials"))
  SSHPASS = Sys.which("sshpass") # Check if sshpass is installed
  RSYNC = Sys.which("rsync") # Check if rsync is installed


  if (credentials_exist) {
    # sshpass and rsync installed (?)
    if (SSHPASS != "" & RSYNC != "") {
      # cli::cli_text(cli::col_green("{cli::symbol$tick} "), "rsync installed and credentials exist")
    } else {
      cli::cli_abort("'sshpass' or 'rsync' not installed. Can't use `sync_server_local()`")
    }
  } else {
    cli::cli_abort("Can't find server credentials in '.vault/.credentials'")
  }

  # GET ---

  # DOWNLOAD server to local
  OUT =
    suppressWarnings(
      system(
        paste0('sshpass -p ', list_credentials$value$password, ' rsync -av ', dry_run, ' --rsh=ssh ',
               list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, pid, '/.data/ ',
               here::here(local_folder_terminal), '/ '
        ), intern = TRUE
      )
    )

  clean_OUT =
    tibble::tibble(files = OUT) |>
      dplyr::filter(grepl("^[0-9]{1,20}_.*\\.csv$", files))

  return(clean_OUT)

}


# SAFELY functions ----

# In z_safely_helper_functions.R so it's the last function to load
