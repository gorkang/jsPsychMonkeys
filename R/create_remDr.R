create_remDr <-
  function(container_port,
           browserName,
           container_name = NULL,
           disable_web_security = FALSE,
           DEBUG = FALSE) {
    
  # DEBUG
    # debug_function("create_remDr")
    # targets::tar_load("parameters_monkeys")
    # debug_docker(uid_participant = 2, parameters_debug = parameters_monkeys)
    
  # targets::tar_load(container_24001)
  # browserName = parameters_monkeys$docker$browserName
  # container_name = container_24001$container_name
  # container_port = container_24001$container_port
  # DEBUG = parameters_monkeys$debug$DEBUG
  

  # CHECK -------------------------------------------------------------------
  
  # If Docker container does not exist, stop execution.
  if (length(reconnect_to_VNC(container_name = container_name, just_check = TRUE)) == 0) {
    cat(crayon::bgRed("NO DOCKER IMAGE available.\n"), crayon::silver("Maybe need to do:\n targets::tar_destroy() & targets::tar_make()\n\n"))
    targets::tar_invalidate(paste0("container_", parameters_task$participants$uid))
    cat(crayon::bgYellow("Invalidated ", container_name, " to restart process\n\n"))
    stop()
  }
  
  
    

  # Create browser instance -------------------------------------------------
  
    if (DEBUG == TRUE) cat(crayon::silver("Create remoteDriver: [", container_port, "] [", browserName, "] [", container_name, "] [", "\n"))
      
    create_remDr <- function(container_port, browserName, time_wait = 1, errored = FALSE) {
      if (errored == FALSE) {
        if (DEBUG == TRUE) cat(crayon::yellow("Creating remoteDriver in", paste0(time_wait, "s\n")))
        Sys.sleep(time_wait)
      } else if (errored == TRUE) {
        if (DEBUG == TRUE) cat(crayon::bgRed("Error creating remoteDriver. Retrying after", paste0(time_wait, "s\n")))
        Sys.sleep(time_wait)
      }

      # extraCapabilities for Chrome
        # See full list: https://peter.sh/experiments/chromium-command-line-switches/ e.g. --incognito or --disable-web-security
        # http://www.chromium.org/for-testers/enable-logging # "--enable-logging"
      chrome_extra_arguments = list(args = list("--start-maximized", "--incognito"))
      if (disable_web_security == TRUE & browserName == "chrome") chrome_extra_arguments$args[[length(chrome_extra_arguments$args) + 1]] = "--disable-web-security"
      
      # Create remote driver
      remDr <- remoteDriver(remoteServerAddr = "localhost", port = container_port, browserName = browserName, 
                            extraCapabilities = list(chromeOptions = chrome_extra_arguments))

      return(remDr)
    }
    create_remDr_safely = safely(create_remDr)
    
    CREATE_REMOTE_DRIVER = create_remDr_safely(container_port = container_port, browserName = browserName, time_wait = 1)

    if (length(CREATE_REMOTE_DRIVER$error) > 0) {
      cat(crayon::bgRed("ERROR: creating remote driver [create_remDr()]\n"))
      CREATE_REMOTE_DRIVER = create_remDr_safely(container_port = container_port, browserName = browserName, time_wait = 2)
      
    }
    
    remDr = CREATE_REMOTE_DRIVER$result
    
  

  # Close all existing browsers ---------------------------------------------
    
    if (DEBUG == TRUE) cat(crayon::yellow("\n-Closing browsers... \n"))
    
    
    # Close all browser instances
    clean_open <- function(time_wait = 1) {
      
      # Wait for driver to be created
      Sys.sleep(time_wait)
      
      remDr$closeall()
      
      if (DEBUG == TRUE) cat(crayon::yellow("\n-About to open a browser... \n"))
      
      # OPEN browser
      remDr$open(silent = !DEBUG)
      
    }
    
    clean_open_safely = safely(clean_open)
    
    
    # Open new browser -------------------------------------------------------
    
    CLEAN_OPEN_SAFELY = clean_open_safely(time_wait = 1)
    
    if (length(CLEAN_OPEN_SAFELY$error) > 0) {
      cat(crayon::bgRed("ERROR: creating closing browsers [create_remDr()]\n"))
      CLEAN_OPEN_SAFELY = clean_open_safely(time_wait = 2)
    }
    # remDr$closeall()
  


  # OUTPUT ------------------------------------------------------------------

    output_list = list(remDr = remDr,
                       container_name = container_name)
    
    return(output_list)

}

