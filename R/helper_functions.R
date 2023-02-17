#' check_trialids
#'
#'Checks that trialid's of an experiment in a folder follow the stantard expected rules
#' @param local_folder_tasks 
#'
#' @return
#' @export
#'
#' @examples
check_trialids <- function(local_folder_protocol) {
  
  suppressMessages(suppressWarnings(library(dplyr)))
  suppressMessages(suppressWarnings(library(purrr)))
  suppressMessages(suppressWarnings(library(readr)))
  
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
#' @return
#' @export
#'
#' @examples
check_accept_alert <- function(wait_retry, remDr) {
  
  get_alert <- function(variables) {
    MESSAGE = remDr$getAlertText()  
    remDr$acceptAlert()
    return(MESSAGE)
  }
  
  get_alert_safely = purrr::safely(get_alert)
  RESP = suppressMessages(get_alert_safely())
  while (!is.null(RESP$result)) {
    withr::with_options(list(crayon.enabled = FALSE), cat(crayon::yellow("[Alert] found, waiting", wait_retry, "seconds: ", crayon::silver(RESP[[1]]), "\n")))
    Sys.sleep(wait_retry)
    
    RESP = suppressMessages(get_alert_safely())
  }
  
  
}


#' reconnect_to_VNC
#'
#' @param container_name if empty, show available containers
#' @param port No need to specify one
#' @param DEBUG TRUE/FALSE
#'
#' @return
#' @export
#'
#' @examples reconnect_to_VNC("test1")
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
#' @param uid_participant 
#'
#' @return
#' @export
#'
#' @examples debug_docker(24000)
debug_docker <- function(uid_participant) {
  
  # DEBUG
  # uid_participant = 888
  # parameters_monkeys = parameters_monkeys
  
  suppressMessages(source(shrtcts::locate_shortcuts_source()))
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
  
  
  container_name_tar <- paste0("container_", uid)
  container_name <<- paste0("container", uid)
  driver_name <<- paste0("remoteDriver_", uid)
  targets::tar_load(eval(container_name_tar))
  targets::tar_load(eval(driver_name))
  remDr <<- get(driver_name)$remDr
  
  cli::cli_alert_info("Loaded {container_name_tar}")
  
}



#' debug_function
#' 
#' Loads the parameters used in the functions present in _targets.R to make debugging easier
#'
#' @param name_function 
#'
#' @return
#' @export
#'
#' @examples
debug_function <- function(name_function) {
  
  # DEBUG
  # name_function = "create_docker"
  
  # Function to tar_load or assign the parameters
  load_parameters <- function(parameters_function_separated, NUM) {
    
    # NUM = 2
    parameter = parameters_function_separated[[NUM]]
    parameter_1 = gsub("\\n", "", parameter[1])
    parameter_2 = gsub("\\n", "", parameter[2])
    
    if (is.na(parameter_2)) {
      cli::cli_alert_info("processing {.code {parameter}}")
    } else {
      cli::cli_alert_info("processing {.code {parameter_1} = {parameter_2}}")
    }

    
    # A single parameter e.g. parameters_monkeys
    if (length(parameter) == 1) {
      
      targets::tar_load(parameter, envir = .GlobalEnv)
      
    # A parameter with "=" e.g: pid = 999
    } else if (length(parameter) == 2) {
      
      # If parameter_2 is an existing object, load
      existing_object = parameter_2[parameter_2 %in% tar_objects()]
      
      # If it's in tar_objects(), load
      if (length(existing_object) == 1) {
        
        targets::tar_load(all_of(existing_object), envir = .GlobalEnv)
        
      } else {
        
      # assign(parameters_function_separated[[NUM]][1], parameters_function_separated[[NUM]][2], envir = .GlobalEnv)

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
    
  DF_function = targets::tar_manifest() %>% filter(grepl(name_function, command)) %>% pull(command) %>% last()
  
  if (!is.na(DF_function)) {
  
    string_function = gsub(name_function, "", DF_function) %>% gsub(" |\\\\n|\\(|\\)", "", .)
    LOADME = stringr::str_extract_all(string_function, "=.*?[,\\$]", simplify = TRUE) %>% gsub("\\$|,|=", "", .) %>% as.vector() %>% unique()
    existing_objects = LOADME[LOADME %in% tar_objects()]
    if (length(existing_objects) > 0) tar_load(all_of(existing_objects))

    parameters_function_separated = strsplit(string_function, ",") %>% unlist() %>% strsplit(., "=")
    
    # For each of the parameters, applies the load_parameters() function
    seq_along(parameters_function_separated) %>% 
      walk(~ 
             load_parameters(parameters_function_separated, NUM = .x)
           )
    
    cat(crayon::green("Loaded: "), gsub(",", ", ", name_function), "\n")
    
  } else {
    
    cat(crayon::red(paste0("'", name_function, "'", "not found in _targets.R")), "\n")
    
  }
}




#' tar_make_future_rowwise
#' 
#' Makes tar_make_future() work row-wise by assigning priorities to targets using their uid. This allows you to run as many participants as you want, avoiding memory issues.
#'
#' @param TARGETS 
#' @param parameters_monkeys 
tar_make_future_rowwise <- function(TARGETS, uids) {
  
  # TODO: The number in TARGETS[[2]] depends on the position of the tar_map() target!
  
  assign_priority <- function(uid, target_name) {
    # browser()
    uid_container = gsub(".*_(.*)", "\\1", TARGETS[[2]][[target_name]][[uid]][["settings"]][["name"]])
    prioriry_container = as.numeric(uid_container) / max(uids)
    TARGETS[[2]][[target_name]][[uid]][["settings"]][["priority"]] = prioriry_container
    
  }
  
  1:length(uids) %>%
    walk(~{
      index_uid = .x
      1:length(names(TARGETS[[2]])) %>%
        walk(~{
          index_target = .x
          # cat("uid = ", uid_walk, "name = ", .x)
          assign_priority(uid = index_uid, target_name = index_target)
        })
    })

  # TARGETS[[1]]$container[[1]]$settings$priority
  
}


#' Check csv files in a folder
#'
#' @param pid 
#' @param download_folder 
#'
#' @return
#' @export
#'
#' @examples
check_Downloads <- function(parameters_monkeys, uid = "", links_tar = "") {

  pid = parameters_monkeys$task_params$pid
  download_folder = parameters_monkeys$docker$folder_downloads
  local_or_server = parameters_monkeys$task_params$local_or_server
  
  # Do this only in local protocols
  if (local_or_server == "local") {
      
    # uid is used in copy_files_to_data so only that participant's data is copyied
    cli::cli_alert_info(paste0("Looking for files for pid = {pid} and uid = {uid}"))
    files_downloaded = list.files(download_folder, pattern = paste0("^", pid, ".*", uid, "\\.csv"))
    
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
      map_df(~ page_source_temp[[.x]]  |>   
               html_attrs() |> 
               bind_rows() |> 
               mutate(tag_name = what, 
                      content = page_source_temp[[.x]] |> rvest::html_text2()))      
    
    
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


# SAFER functions ---------------------------------------------------------

launch_task_safely = safely(launch_task)


