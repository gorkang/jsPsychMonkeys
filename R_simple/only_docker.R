only_docker <-
  function(container_name = "test",
           image_browser = "chrome",
           DEBUG = FALSE,
           big_container = FALSE,
           folder_downloads = "~/Downloads") {
    
    
    # DEBUG
    # debug_function(only_docker)
    
    # container_name = "testX"
    # image_browser = "chrome"
    # DEBUG = TRUE
    # big_container = FALSE
    # folder_downloads = "~/Downloads"
    
    
    # Packages -------------------------------------------------------------
    suppressMessages(suppressWarnings(library(RSelenium)))
    suppressMessages(suppressWarnings(library(dplyr)))
    suppressMessages(suppressWarnings(library(purrr)))
    
    
    # CHECKS ------------------------------------------------------------------
    if (!image_browser %in% c("chrome", "firefox")) message("Use 'firefox' or 'chrome' as image_browser parameter")
    
    # Check if exists
    if (is_empty(system(sprintf('docker ps -q -f name=%s', container_name), intern = TRUE))) {
      
      # Container does not exist
      if (DEBUG == TRUE) cat(crayon::yellow("Container", container_name, "does not exist. Creating it...\n"))
      
    } else {
      
      # Container already exists
      if (DEBUG == TRUE) cat(crayon::green("Container", container_name, "exists. Reusing it...\n"))
                                           
      #Stop and remove containers:
      # TODO: SHOULD DO THIS WHEN PARTICIPANT ENDS
      # If container is running stop it
      # if (!is_empty(system(sprintf('docker ps -q -f name=%s', container_name), intern = TRUE))) {
      #   # Stop and remove container
      #   system(sprintf('docker stop %s', container_name)) # kill container_name
      # }
      
      # If container exists remove it
      # if (length(system(sprintf('docker container ls -a -f name=%s', container_name), intern = TRUE)) > 1) {
      #   system(sprintf('docker container rm %s', container_name))
      # }
      
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
    if (system(paste0('docker images -a |  grep "', image_browser, debug_label, '"'), intern = TRUE) %>% grepl("latest", .) %>% any(.) == FALSE) {
      
      if (DEBUG == TRUE) {
        # if (DEBUG == TRUE) message("* Pull docker image - DEBUG mode: ")
        cat(crayon::silver("\n-docker pull\n"))
        # debug_label = '-debug'
        system(paste0('docker pull --quiet selenium/standalone-', image_browser, debug_label))  
      } else {
        # if (DEBUG == TRUE) message("* Pull docker image - normal mode: ")
        # debug_label = ''
        system(paste0('docker pull --quiet selenium/standalone-', image_browser))  
      }
      
      # Wait
      Sys.sleep(5)
      
      
    } else {
      if (DEBUG == TRUE) cat(crayon::silver("Not pulling docker for ", paste0(image_browser, debug_label), ", already have it\n"))
    }
    
    
    # Run docker session. Map home directory to download docker container
    
    if (length(system(paste0('docker ps --filter "name=', container_name, '"'), intern = TRUE)) == 1) {
      
      if (DEBUG == TRUE) cat(crayon::yellow("Docker image", container_name, " not running. Launching...\n"))
      
      if (folder_downloads == "") {
        system(paste0('docker run --rm -t -d ', big_container_str,' --name ', container_name, ' -v /dev/shm:/dev/shm -P selenium/standalone-', image_browser, debug_label)) # NO Mapeando Downloads
      } else {
        system(paste0('docker run --rm -t -d ', big_container_str,' --name ', container_name, ' -v ', folder_downloads, ':/home/seluser/Downloads -v /dev/shm:/dev/shm -P selenium/standalone-', image_browser, debug_label)) # Mapeando Downloads
      }
    } else {
      if (DEBUG == TRUE) cat(crayon::green("Docker image", container_name, " already running.\n"))
    } 
    
    # Wait
    # Sys.sleep(5)

    # Get port
    container_port_raw <- system(sprintf('docker port %s', container_name), intern = TRUE)
    container_port <- max(as.integer(gsub('.*:(.*)$', '\\1', container_port_raw)))
    if (is.na(container_port)) cat(crayon::red("Port not found?"))
    
    list(container_name = container_name,
         container_port = container_port, # THIS REVIEW
         image_browser = image_browser
         )
    
    
      }
