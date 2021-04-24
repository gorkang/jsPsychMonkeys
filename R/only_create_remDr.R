only_create_remDr <- function(container_port, browserName, container_name = NULL, DEBUG = FALSE) {
  
  # DEBUG
  # targets::tar_load(container_24000)
  # browserName = parameters$browserName
  # container_name = container_24000$container_name
  # container_port = container_24000$container_port
  # DEBUG = parameters$DEBUG
  

  # CHECK -------------------------------------------------------------------
  
  if (length(reconnect_to_VNC()) == 0) {
    cat(crayon::bgYellow("NO DOCKER IMAGE available.\n"), crayon::silver("Probably need to do:\n targets::tar_destroy() & targets::tar_make()\n\n"))
    stop()
  }
  
  
  # Create browser instance -------------------------------------------------
  
    if (DEBUG == TRUE) cat(crayon::silver("Create remoteDriver: [", container_port, "] [", browserName, "] [", container_name, "] [", "\n"))
      
    create_remDr <- function(container_port, browserName, time_wait = 5, errored = FALSE) {
      if (errored == FALSE) {
        if (DEBUG == TRUE) cat(crayon::yellow("Creating remoteDriver in", paste0(time_wait, "s\n")))
        Sys.sleep(time_wait)
      } else if (errored == TRUE) {
        if (DEBUG == TRUE) cat(crayon::red("Error creating remoteDriver. Retrying after", paste0(time_wait, "s\n")))
            Sys.sleep(time_wait)
      }
      
      # Create remote driver
      remDr <- remoteDriver(remoteServerAddr = "localhost", port = container_port, browserName = browserName)
      return(remDr)
    }
    create_remDr_safely = safely(create_remDr)
    
    CREATE_REMOTE_DRIVER = create_remDr_safely(container_port = container_port, browserName = browserName, time_wait = 1)
    # while (!is.null(CREATE_REMOTE_DRIVER$error)) {
    #   CREATE_REMOTE_DRIVER = create_remDr_safely(container_port = container_port, browserName = browserName, time_wait = 5, errored == TRUE)
    # }
    if (length(CREATE_REMOTE_DRIVER$error) > 0) cat(crayon::red("ERROR: creating remote driver [create_remDr()]"))
    
    remDr = CREATE_REMOTE_DRIVER$result
    
  

  # Close all existing browsers ---------------------------------------------
    
    if (DEBUG == TRUE) cat(crayon::yellow("\n-Closing browsers... \n"))
    
    # Wait for driver to be created
    Sys.sleep(3)
    
    # Close all browser instances
    remDr$closeall()
    
    
  # Open new browser -------------------------------------------------------
    
    if (DEBUG == TRUE) cat(crayon::yellow("\n-About to open a browser... \n"))
    # Sys.sleep(1)
    remDr$open(silent = !DEBUG)
    

  # OUTPUT ------------------------------------------------------------------

    output_list = list(remDr = remDr,
                       container_name = container_name)
    
    return(output_list)

}

