start_docker <-
  function(container_name = "test",
           image_browser = "firefox",
           DEBUG = FALSE,
           open_VNC = FALSE,
           big_container = FALSE,
           folder_downloads = "~/Downloads") {
    
  
  # Packages -------------------------------------------------------------
    suppressMessages(suppressWarnings(library(RSelenium)))
    suppressMessages(suppressWarnings(library(dplyr)))
    suppressMessages(suppressWarnings(library(purrr)))


  # CHECKS ------------------------------------------------------------------
    if(!image_browser %in% c("chrome", "firefox")) message("Use 'firefox' or 'chrome' as image_browser parameter")
    
    # Check if exists
    if(is_empty(system(sprintf('docker ps -q -f name=%s', container_name), intern = TRUE))) {
      
      message("Container '", container_name, "' does not exist. Creating...")

    } else {
      
      # Start docker session ----------------------------------------------------
      message("* Stop and remove containers: ")
      
      # If container is running stop it
      if (!is_empty(system(sprintf('docker ps -q -f name=%s', container_name), intern = TRUE))) {
        # Stop and remove container
        system(sprintf('docker stop %s', container_name)) # kill container_name
      }
      
      # If container exists remove it
      if (length(system(sprintf('docker container ls -a -f name=%s', container_name), intern = TRUE)) > 1) {
        system(sprintf('docker container rm %s', container_name))
      }

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
        debug_label = '-debug'
        system(paste0('docker pull --quiet selenium/standalone-', image_browser, debug_label))  
      } else {
        # if (DEBUG == TRUE) message("* Pull docker image - normal mode: ")
        debug_label = ''
        system(paste0('docker pull --quiet selenium/standalone-', image_browser))  
      }
      
    } else {
      cat(crayon::silver("\nNot pulling docker for ", paste0(image_browser, debug_label), ", already have it\n"))
    }
    
    # Wait
    Sys.sleep(5)
    
      # Run docker session. Map home directory to download docker container
      if (folder_downloads == "") {
        system(paste0('docker run --rm -t -d ', big_container_str,' --name ', container_name, ' -v /dev/shm:/dev/shm -P selenium/standalone-', image_browser, debug_label)) # NO Mapeando Downloads
      } else {
        system(paste0('docker run --rm -t -d ', big_container_str,' --name ', container_name, ' -v ', folder_downloads, ':/home/seluser/Downloads -v /dev/shm:/dev/shm -P selenium/standalone-', image_browser, debug_label)) # Mapeando Downloads    
      }
    
    # Wait
    Sys.sleep(5)

  
  # Selenium ----------------------------------------------------------------

    # Get port
    container_port_raw <- system(sprintf('docker port %s', container_name), intern = TRUE)
    container_port <<- as.integer(gsub('.*:(.*)$', '\\1', container_port_raw))
    if (is.na(container_port[1])) cat(crayon::red("Port not found?"))
    
    # Wait
    # Sys.sleep(5)
  
    # Create browser instance. This should be reflected in the VNC session
    if (DEBUG == TRUE) cat(crayon::silver("\n-Open remoteDriver\n"))
    create_remDr <- function(port, browserName, time_wait = 3,  time_wait2 = 0) {
      if(time_wait2 == 0) {
        cat(crayon::red("Creating remoteDriver in", paste0(time_wait, "s\n")))
        Sys.sleep(time_wait)
      } else {
        cat(crayon::red("Error creating remoteDriver. Retrying after", paste0(time_wait2, "s\n")))
        Sys.sleep(time_wait2)
      }
      
      remDr <<- remoteDriver(remoteServerAddr = "localhost", port = port, browserName = browserName)
    }
    
    tryCatch(create_remDr(port = container_port[1], browserName = image_browser, time_wait = 5),
             error = function(e) e,
             finally = create_remDr(port = container_port[1], browserName = image_browser, time_wait2 = 5))
    
    # remDr <<- remoteDriver(remoteServerAddr = "localhost", port = container_port[1], browserName = image_browser)


    # TODO: IF this fails, should restart the container! ----------------------
    # if (DEBUG == TRUE) message("* Open connection (2s wait)")
    

    if (DEBUG == TRUE) cat(crayon::silver("\n-Open connection\n"))
    
    # IF "Undefined error in httr call. httr output: Recv failure: Connection reset by peer" error, increase wait! [was 2, increased to 5]    
    
    open_remDr <- function(DEBUG, time_wait = 3, time_wait2 = 0) {
      
      if(time_wait2 == 0) {
        cat(crayon::red("Opening remDr in", paste0(time_wait, "s\n")))
        Sys.sleep(time_wait)
      } else {
        cat(crayon::red("Error opening remDr. Retrying after", paste0(time_wait2, "s\n")))
        Sys.sleep(time_wait2)
      }
      
      remDr$open(silent = !DEBUG)
    }
    
    tryCatch(open_remDr(DEBUG, time_wait = 5),
             error = function(e) e,
             finally = open_remDr(DEBUG, time_wait2 = 5))

    
    if (open_VNC == TRUE) {
      
      # Open VNC, using second port in container_port, the password is 'secret'
      vnc_command = paste0('vncviewer 127.0.0.1:', container_port[2])
      MESSAGE <<- paste0("\n* Open VNC (e.g.remmina) localhost:", container_port[2], " pwd: secret\n", "", vnc_command)
      message(MESSAGE)
      system(paste0('echo "', MESSAGE, '"'))
      
      # system('remmina --server=localhost &')
      system(paste0(vnc_command, ' &'))
    }

}
