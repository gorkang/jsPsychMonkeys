only_docker <-
  function(container_name = "test",
           browserName = "chrome",
           DEBUG = FALSE,
           big_container = FALSE,
           folder_downloads = "~/Downloads") {
    
    
    # DEBUG
    # debug_function(only_docker)
    
    # container_name = "testX"
    # browserName = "chrome"
    # DEBUG = TRUE
    # big_container = FALSE
    # folder_downloads = "~/Downloads"
    
    
    # Packages -------------------------------------------------------------
    suppressMessages(suppressWarnings(library(RSelenium)))
    suppressMessages(suppressWarnings(library(dplyr)))
    suppressMessages(suppressWarnings(library(purrr)))
    
    
    # CHECKS ------------------------------------------------------------------
    if (!browserName %in% c("chrome", "firefox")) message("Use 'firefox' or 'chrome' as browserName parameter")

    available_RAM = as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern = TRUE))
    
    if (available_RAM < 8000000) {
      
      NUMBER_dockers_LOW = length(reconnect_to_VNC())
      
      if (!exists("NUMBER_dockers")) NUMBER_dockers = NUMBER_dockers_LOW
      cat(crayon::bgRed("\n\n --- LOW RAM --- dockers: ", NUMBER_dockers_LOW, "\n\n"))
      
      while (NUMBER_dockers >= NUMBER_dockers_LOW) {
        Sys.sleep(10)
        NUMBER_dockers = length(reconnect_to_VNC()) 
        cat(crayon::bgWhite("\n\n --- LOW RAM --- dockers: ", NUMBER_dockers_LOW, "/", NUMBER_dockers, "\n\n"))
      }
      
    }
    
    
    # Check if exists
    if (is_empty(system(sprintf('docker ps -q -f name=%s', container_name), intern = TRUE))) {
      
      # Container does not exist
      if (DEBUG == TRUE) cli::cli_alert_info("Container {container_name} does not exist. Creating it...\n") #cat(crayon::yellow("Container", container_name, "does not exist. Creating it...\n"))
    
    } else {
      
      # Container already exists
      if (DEBUG == TRUE) cli::cli_alert_info("Container {container_name} exists. Reusing it...\n")
      
    }
    
    
    # Parameters --------------------------------------------------------------
    
    # Sometimes we need a browser instance with more available memory
    if (big_container == TRUE) {
      big_container_str = '--shm-size=2g'
    } else {
      big_container_str = ''
    }
    
    
    # Start docker session ----------------------------------------------------
    
    # Get image if we don't have it already (to use VNC use DEBUG = TRUE)
    # TODO: CHECK if we have the "latest"
    if (DEBUG == TRUE) {
      debug_label = '-debug'
    } else {
      debug_label = ''
    }
    
    
    # Look for the "latest" version of the docker container 
    if (system(paste0('docker images -a |  grep "', browserName, debug_label, '"'), intern = TRUE) %>% grepl("latest", .) %>% any(.) == FALSE) {
      
      if (DEBUG == TRUE) {
        # if (DEBUG == TRUE) message("* Pull docker image - DEBUG mode: ")
        cat(crayon::silver("\n-docker pull\n"))
        # debug_label = '-debug'
        system(paste0('docker pull --quiet selenium/standalone-', browserName, debug_label))  
      } else {
        # if (DEBUG == TRUE) message("* Pull docker image - normal mode: ")
        # debug_label = ''
        system(paste0('docker pull --quiet selenium/standalone-', browserName))  
      }
      
      # Wait
      Sys.sleep(5)
      
      
    } else {
      if (DEBUG == TRUE) cat(crayon::silver("Not pulling docker for ", paste0(browserName, debug_label), ", already have it\n"))
    }
    
    
    # Run docker session. Map home directory to download docker container
    
    if (length(system(paste0('docker ps --filter "name=', container_name, '"'), intern = TRUE)) == 1) {
      
      if (DEBUG == TRUE) cat(crayon::yellow("Docker image", container_name, " not running. Launching...\n"))
      
      if (folder_downloads == "") {
        system(paste0('docker run --rm -t -d ', big_container_str,' --name ', container_name, ' -v /dev/shm:/dev/shm -P selenium/standalone-', browserName, debug_label)) # NO Mapeando Downloads
      } else {
        system(paste0('docker run --rm -t -d ', big_container_str,' --name ', container_name, ' -v ', folder_downloads, ':/home/seluser/Downloads -v /dev/shm:/dev/shm -P selenium/standalone-', browserName, debug_label)) # Mapeando Downloads
      }
    } else {
      if (DEBUG == TRUE) cat(crayon::green("Docker image", container_name, " already running.\n"))
    } 
    


    # Get port
    container_port_raw <- system(sprintf('docker port %s', container_name), intern = TRUE)
    container_port <- max(as.integer(gsub('.*:(.*)$', '\\1', container_port_raw)))
    if (is.na(container_port)) cat(crayon::red("Port not found?"))
    
    list(container_name = container_name,
         container_port = container_port, # THIS REVIEW
         browserName = browserName
         )
    
    
      }
