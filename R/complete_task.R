# Function to complete a task
complete_task <-
  function(parameters_task,
           uid,
           initial_wait = 2,
           wait_retry = 2,
           screenshot = FALSE,
           DEBUG = FALSE,
           open_VNC = FALSE,
           container_name = NULL,
           remDr = NULL) {
    
    
  # DEBUG
    # debug_function(complete_task_simple) # TODO: Make it work with lists
    # reconnect_to_VNC("container24000", DEBUG = TRUE)
    
    
    # parameters_task = parameters_monkeys
    # uid = 24000
    # initial_wait = parameters_monkeys$task_params$initial_wait
    # wait_retry = parameters_monkeys$task_params$wait_retry
    # screenshot = parameters_monkeys$debug$screenshot
    # DEBUG = parameters_monkeys$debug$DEBUG
    # open_VNC = parameters_monkeys$debug$open_VNC
    # tar_load(paste0("remoteDriver_", uid))
    # container_name = get(paste0("remoteDriver_", uid))$container_name
    # remDr = get(paste0("remoteDriver_", uid))$remDr
    
    # open_VNC = FALSE
    # initial_wait = 0
    # wait_retry = 2
    # screenshot = FALSE
    # DEBUG = TRUE
    # debug_docker(24000)
    
    
  # CHECKS --------------------------------------------------------------
  
    # If Docker container does not exist, stop execution.
    if (length(reconnect_to_VNC(container_name = container_name, just_check = TRUE)) == 0) {
      cat(crayon::bgRed("NO DOCKER IMAGE available.\n"), crayon::silver("Maybe need to do:\n targets::tar_destroy() & targets::tar_make()\n\n"))
      targets::tar_invalidate(paste0("container_", parameters_task$participants$uid))
      cat(crayon::bgYellow("Invalidated ", container_name, " to restart process\n\n"))
      stop()
      
    } else {
      
      if (DEBUG == TRUE & open_VNC == TRUE) {
        cat(crayon::bgWhite("\n\nVNC launching\n\n"))
        reconnect_to_VNC(container_name, DEBUG = TRUE)
      }
    }
    
    # Critical variables exist?
    if (!exists("uid")) uid = 0
    if (!exists("wait_retry")) wait_retry = 1
    
    
    # Maybe necessary (?)
    remDr <<- remDr
    
    
  # SAFER functions ---------------------------------------------------------
    
    # Get elements
    get_elements_safely = safely(get_elements)
    

  # START LOG ----------------------------------------------------------------

    # Create log for each worker
    if (parameters_task$debug$debug_file == TRUE) {
      con <- file(paste0("outputs/log/pid_", gsub("/", "_", parameters_task$task$pid), "_uid_", uid, ".log"))
      sink(con, append = TRUE)
      sink(con, append = TRUE, type = "message")
    }

  
  # Loop through items of a task --------------------------------------------
    
    # Condition to stop while
    continue = TRUE
    index = 1
    
    while (continue) {
    
      if (DEBUG == TRUE) cat(crayon::bgMagenta("  --- SCREEN: ", index, " ---\n"))
      if (!exists("index")) index = 1
      if (screenshot == TRUE) remDr$screenshot(file = paste0("outputs/screenshots/", uid, "_screenshot_", sprintf("%03d", index), "_", as.Date(Sys.Date(), format = "%Y-%m-%d"), ".png"))
      
      
      ## Get elements of website ----------------------------
      
        list_get_elements = get_elements_safely(remDr = remDr, index = index, try_number = 1, DEBUG = DEBUG)
        
        # If we don't get any elements on out first try, wait wait_retry and try again (important when loading images, htmls, etc.)
        if (!is.null(list_get_elements$error)) { 
            Sys.sleep(wait_retry)
            list_get_elements = get_elements_safely(remDr = remDr, index = index, try_number = 2, DEBUG = DEBUG)
            }
          
        # When there is an error, usually we will have some content here (we "cause" the error with a stop())
        list_get_elements = list_get_elements$result
  
        
      # Interact with the elements we found ------------------
      if (list_get_elements$continue == TRUE) interact_with_element(list_get_elements, DEBUG = DEBUG)
      
      
      # Output of while
      continue = list_get_elements$continue
      index = index + 1
      
    }
  

  # END LOG -----------------------------------------------------------------

  # Restore output to console
  if (parameters_task$debug$debug_file == TRUE) {
    sink() 
    sink(type = "message")
  }

  return(container_name)

}
