#' check_trialids
#'
#'Checks that trialid's of an experiment in a folder follow the stantard expected rules
#' @param local_folder_tasks 
#'
#' @return
#' @export
#'
#' @examples
check_trialids <- function(local_folder_tasks) {
  
  suppressMessages(suppressWarnings(library(dplyr)))
  suppressMessages(suppressWarnings(library(purrr)))
  suppressMessages(suppressWarnings(library(readr)))
  
  scripts = dir(path = paste0("~/", local_folder_tasks), pattern = "experiment.js", recursive = TRUE, full.names = TRUE)
  if (length(scripts) == 0) stop(paste("Can't find anything in ", local_folder_tasks))

  find_trialids <- function(file_name) {
    
    script = read_file(file_name) 
    expres = ".*?trialid: '(.*?)'.*?"
    trialid = gsub(expres, "\\1; \n", script) %>% gsub("^(.*; \n).*", "\\1", .) %>% gsub(";", "", .) %>% gsub(" number \n", "", .)
    if (grepl("This document was made with test_maker", trialid)) trialid = ""
    strsplit(trialid, " \n")[[1]] %>% as_tibble() %>% mutate(file = file_name) %>% rename(trialid = value) %>% filter(!grepl("^Instructions|^Instructions_[0-9]{2}", trialid))
    
  }
  
  
  DF_all_trialids = map_df(scripts, find_trialids)
  
  rule_check_trialids = "^[a-zA-Z0-9]{1,100}_[0-9]{2}$"
  DF_problematic_trialids = DF_all_trialids %>% 
    filter(!grepl(rule_check_trialids, trialid))
  
  if (nrow(DF_problematic_trialids) > 0) {
    
    message(cat(crayon::red("ISSUES in the following trialid: "), paste(DF_problematic_trialids %>% pull(trialid), collapse = ", ")))

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
  # container_name = container24000
  # just_check = TRUE
  # port = NULL
  
  if (!is.null(port)) container_port = port
  
  if (is.null(container_name)) {
    
    system('docker ps -a --format "{{.Names}}"', intern = TRUE) # Print container names
    
  } else {
    
    if (just_check == TRUE) {
    
      # Outputs the name of the container if it exists, NULL otherwise  
      containers_available = system('docker ps -a --format "{{.Names}}"', intern = TRUE) # Print container names
      if (!container_name %in% containers_available) {
        NULL
      } else {
        container_name
      }
      
      
    } else {

      container_port_raw <- system(sprintf('docker port %s', container_name), intern = TRUE)
      container_port <- min(as.integer(gsub('.*:(.*)$', '\\1', container_port_raw)))
      if (is.na(container_port[1])) cat(crayon::red("Port not found?"))
      
      
      # Open VNC, using second port in container_port, the password is 'secret'
      vnc_command = paste0('vncviewer 127.0.0.1:', container_port)
      cat(crayon::yellow(paste0("\nOpen VNC - localhost:", container_port, " pwd: secret\n"), crayon::black(vnc_command, "\n")))
      if (DEBUG == TRUE) cat(crayon::silver(" DEBUG:", container_port_raw))
      # system(paste0('echo "', MESSAGE, '"'))
      
      system(paste0(vnc_command, ' &'))      
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
  # uid_participant = 24000
  
  suppressMessages(source(shrtcts::locate_shortcuts_source()))
  suppressMessages(source(here::here("_targets.R")))
  
  DEBUG = TRUE
  uid = uid_participant
  container_name = paste0("container_", uid)
  driver_name = paste0("remoteDriver_", uid)
  targets::tar_load(eval(container_name))
  targets::tar_load(eval(driver_name))
  remDr <<- get(driver_name)$remDr
  
  cat(crayon::green(glue("Loaded {container_name} \n")))
  
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
  # name_function = "prepare_CRS"
  
  # Function to tar_load or assign the parameters
  load_parameters <- function(parameters_function_separated, NUM) {
    if (length(parameters_function_separated[[NUM]]) == 1) {
      targets::tar_load(parameters_function_separated[[NUM]], envir = .GlobalEnv)
    } else if (length(parameters_function_separated[[NUM]]) == 2) {
      assign(parameters_function_separated[[NUM]][1], parameters_function_separated[[NUM]][2], envir = .GlobalEnv)
    }
  }
  
  
  # Makes possible to use prepare_TASK or "prepare_TASK"
  if (substitute(name_function) != "name_function") name_function = substitute(name_function) #if (!interactive()) is so substitute do not overwrite name_function when in interactive mode
  
  # Parses _targets.R
  code <- parse("_targets.R")
  if (file.exists("targets/targets_main.R")) code <- c(code, parse("targets/targets_main.R"))
  # code <- parse("_targets.R")
  
  # Finds the chunk where name_function is, and cleans the "\"
  text_targets = grep(name_function, code, value = TRUE) %>% gsub("[^A-Za-z0-9\\(\\),_= ]", "", .)
  
  # Gets and separates then parameters of the function
  parameters_function_raw = gsub(paste0(".*", name_function, "\\((.*?)).*"), "\\1", text_targets) %>% gsub(" ", "", .)
  
  if (length(parameters_function_raw) > 0) {
    
    parameters_function_separated = strsplit(parameters_function_raw, ",") %>% unlist() %>% strsplit(., "=")
    
    # For each of the parameters, applies the load_parameters() function
    TEMP = seq_along(parameters_function_separated) %>% map(~ load_parameters(parameters_function_separated, NUM = .x))
    cat(crayon::green("Loaded: "), gsub(",", ", ", parameters_function_raw), "\n")
    
  } else {
    
    cat(crayon::red(paste0("'", name_function, "'", "not found in _targets.R")), "\n")
    
  }
}

