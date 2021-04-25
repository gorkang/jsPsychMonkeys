# Function to complete a task
complete_task <-
  function(parameters_local_server,
           uid,
           initial_wait = 2,
           screenshot = FALSE,
           DEBUG = FALSE,
           open_VNC = FALSE,
           container_name = NULL,
           remDr = NULL) {
    
  # DEBUG
  # debug_function(complete_task_simple) # TODO: Make it work with lists
    # DEBUG = TRUE
    # debug_docker(24000)
    # reconnect_to_VNC("container24000", DEBUG = TRUE)
    
  
    
  # CHECKS --------------------------------------------------------------
  
    if (length(reconnect_to_VNC()) == 0) {
      cat(crayon::bgYellow("NO DOCKER IMAGE available.\n"), crayon::silver("Probably need to do:\n targets::tar_destroy() & targets::tar_make()\n\n"))
      stop()
    } else {
      if (open_VNC == TRUE) reconnect_to_VNC(container_name, DEBUG = TRUE)
    }
    
    if (!exists("uid")) uid = 0
    
    # Maybe necessary (?)
    remDr <<- remDr
    

  # Create link -------------------------------------------------------------

    if (parameters_local_server$local_or_server == "local") {
      links_tasks = paste0("file:///home/seluser/", parameters_local_server$local_folder_tasks, "/index.html?uid=", uid, "&pid=", parameters$pid)
    } else {
      source(".vault/SERVER_PATH.R") # server:path
      parameters_local_server$server_path = server_path
      links_tasks = paste0(parameters_local_server$server_path, parameters$pid, "/?uid=", uid, "&pid=", parameters$pid)
    }
    

  # START LOG ----------------------------------------------------------------

    # Create log for each worker
    if (parameters$debug_file == TRUE) {
      con <- file(paste0("log/pid_", gsub("/", "_", parameters$pid), "_uid_", uid, ".log"))
      sink(con, append = TRUE)
      sink(con, append = TRUE, type = "message")
    }    
  
  
  # Message -----------------------------------------------------------------
  
    if (DEBUG == TRUE) cat(crayon::underline(crayon::green(uid, "- ", links_tasks), "\n"))
    # if (DEBUG == TRUE) cli::cli_h2("{uid} - {links_tasks}\n")
    
    if (!exists("time_wait")) time_wait = 1
    
    
  # Go to task --------------------------------------------------------------
  
    # If fails, tries again, waiting a bit longer
    launch_task <- function(links_tasks, time_wait) {
      Sys.sleep(time_wait)
      if (DEBUG == TRUE) cat(crayon::yellow("Opening link:", links_tasks, "\n"))
      remDr$navigate(links_tasks)
    }
    launch_task_safely = safely(launch_task)
    LAUNCH_TASK = launch_task_safely(links_tasks, time_wait = 1)
    
    # INITIAL WAIT FOR PAGE TO LOAD
    if (DEBUG == TRUE) cat(crayon::bgGreen(paste0("\n  START OF EXPERIMENT. waiting ", initial_wait, "s [If it fails, increase wait]  \n")))
    Sys.sleep(initial_wait)
    
    if (length(LAUNCH_TASK$error) > 0) cat(crayon::red("ERROR: launching task [launch_task()]\n"))

  
  # Loop through items of a task --------------------------------------------
    
    # Condition to stop while
    continue = TRUE
    index = 1
    
    while (continue) {
    
      if (!exists("index")) index = 1
      if (screenshot == TRUE) remDr$screenshot(file = paste0("outputs/screenshots/", uid, "_screenshot_", sprintf("%03d", index), "_", as.Date(Sys.Date(), format = "%Y-%m-%d"), ".png"))
      
      # Get elements of website
      list_get_elements = get_elements(remDr = remDr, DEBUG = DEBUG)
      
      # Interact with the elements we found
      continue_interact = interact_with_element(list_get_elements, DEBUG = DEBUG)    
      
      if (DEBUG == TRUE) cat(crayon::bgYellow("\n  get_elements$continue:", list_get_elements$continue, "interact$continue: ", continue_interact, "\n"))
      
      continue = list_get_elements$continue & continue_interact
      
      # If we are in DEBUG mode and continue is FALSE, load the debug_docker to help DEBUG and stop targets
      if (DEBUG == TRUE & continue == FALSE) {
        # debug_docker(uid)
        cat(crayon::bgCyan("\ncomplete_task_new.R == END OF EXPERIMENT!. IF WE stop() is because we are in DEBUG MODE\n"))
        # stop()
      }
      
      index = index + 1
    }
  

  # END LOG -----------------------------------------------------------------

  # Restore output to console
  if (parameters$debug_file == TRUE) {
    sink() 
    sink(type = "message")
  }
  
  return(container_name)
  
}
