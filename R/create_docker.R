create_docker <-
  function(container_name = "test",
           browserName = "chrome",
           DEBUG = FALSE,
           big_container = FALSE,
           folder_downloads = "~/Downloads",
           parameters_docker = NULL) {
    
    
    # DEBUG
    # debug_function(create_docker)
    
    # container_name = "container1"
    # browserName = "chrome"
    # DEBUG = TRUE
    # big_container = FALSE
    # folder_downloads = "~/Downloads/"
    # parameters_docker = parameters_monkeys
    
    
    # Packages -------------------------------------------------------------
    suppressMessages(suppressWarnings(library(RSelenium)))
    suppressMessages(suppressWarnings(library(dplyr)))
    suppressMessages(suppressWarnings(library(purrr)))
    

# FOLDER -----------------------------------------------------

   # Operative system detection
    OS = Sys.info()["sysname"]

    # The way we build the folder_downloads depends on the OS
    # With folder_protocol_local I am testing a way to separate the protocol folder mount from the data mount in the docker container
    # It works, but encounter permission issues, as the local Download folder need to have 777
    # A simpler way to do this can be seen in create_remDr(), where we set the Download folder in Chrome.
    # We encounter the same permission issues, as the local Download folder need to have 777
    if (OS == "Linux") {
      
      grep_command = "grep"
      # folder_downloads = folder_downloads
      
      # folder_protocol_local   folder_protocol_docker
      # # -v ~/Downloads/protocol999:/home/seluser/protocol999 
      
      # folder_protocol_local = paste0(folder_downloads, "/", basename(parameters_docker$task_params$local_folder_tasks))
      folder_protocol_local = parameters_docker$task_params$local_folder_tasks
      
                                      
    } else if (OS == "Windows") {

      grep_command = "findstr"
      # DEBUG
      # parameters_docker$task_params$local_folder_tasks = "C:\\\\Users\\\\emrys\\\\Downloads\\\\protocol999"
      
      # OLD Working
      if (parameters_docker$task_params$local_or_server == "local") {
        folder_downloads_temp = paste0(normalizePath(parameters_docker$task_params$local_folder_tasks), "")
        # # if (!dir.exists(folder_downloads)) dir.create(folder_downloads)
        if (grepl("C:", folder_downloads_temp)) folder_downloads = gsub("C:", "c", paste0("//",folder_downloads_temp)) %>% dirname() %>% gsub("\\\\", "/", .)
        if (grepl("D:", folder_downloads_temp)) folder_downloads = gsub("D:", "d", paste0("//",folder_downloads_temp)) %>% dirname() %>% gsub("\\\\", "/", .)
        folder_protocol_local = basename(parameters_docker$task_params$local_folder_tasks) %>% gsub("C:", "c", paste0("//",folder_downloads))  %>% gsub("\\\\", "/", .)
        
      }
      # folder_downloads = paste0(normalizePath(parameters_docker$task_params$local_folder_tasks), "")
      # folder_protocol_local = gsub("C:", "c", paste0("//",folder_downloads))  %>% gsub("\\\\", "/", .)
      

    } else if (OS == "Darwin" | OS == "macOS") {

      grep_command = "grep"
      
      # folder_downloads = folder_downloads
      folder_protocol_local = paste0(folder_downloads, "/", basename(parameters_docker$task_params$local_folder_tasks))
      
    } else {
      stop("Not sure about your operative system.")
    }

    # Common to all OS
    folder_protocol_docker = paste0("/home/seluser/", basename(parameters_docker$task_params$local_folder_tasks))


    
    # CHECKS ------------------------------------------------------------------
    if (!browserName %in% c("chrome", "firefox")) message("Use 'firefox' or 'chrome' as browserName parameter")
    if (is.null(parameters_docker)) parameters_docker = list(task = list(local_or_server = "server"))

    # If we are in testing mode, use the test/ folder in the project
    if (parameters_docker$task$local_or_server == "test") folder_downloads = here::here("tests/jspsych-6_3_1/")
    
    
    if (Sys.info()["sysname"] == "Linux") {
      
      # Available RAM
      available_RAM = as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern = TRUE))
      
      if (available_RAM < 80000) {
  
        NUMBER_dockers_LOW = length(reconnect_to_VNC())
  
        if (!exists("NUMBER_dockers")) NUMBER_dockers = NUMBER_dockers_LOW
        # cat(crayon::bgRed("\n\n --- LOW RAM --- dockers: ", NUMBER_dockers_LOW, "\n\n"))
  
        # while (NUMBER_dockers >= NUMBER_dockers_LOW) {
        if (NUMBER_dockers >= NUMBER_dockers_LOW) {
          cat(crayon::bgWhite("\n\n --- LOW RAM --- dockers: ", NUMBER_dockers_LOW, "/", NUMBER_dockers, "Pause for 60s...\n\n"))
          Sys.sleep(60)
          NUMBER_dockers = length(reconnect_to_VNC())
  
        }
  
      }
    }
    
    # Check if exists
    # if (is_empty(system(sprintf('docker ps -q -f name=%s', container_name), intern = TRUE))) { # WITH THIS SINTAX, container1 == container10
      
    if (container_name %in% system('docker ps -a --format "{{.Names}}"', intern = TRUE) == FALSE) {
      
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
    if (DEBUG == TRUE) {
      debug_label = '-debug'
    } else {
      # BAD workaround for https://github.com/gorkang/jsPsychMonkeys/issues/8
      debug_label = '-debug'
    }
    
    

    # Get docker images -------------------------------------------------------
    # browserName = "chrome"
    # debug_label = "-debug"
    DOCKER_images = system("docker images -a", intern = TRUE) |> tibble::as_tibble()
    LATEST_image = DOCKER_images |> 
      dplyr::filter(grepl(paste0(browserName, debug_label), value)) |> 
      dplyr::filter(grepl("latest", value))
    
    # REVIEW: Look for the "latest" version of the docker image 
    # if (system(paste0('docker images -a |  ', grep_command, ' "', browserName, debug_label, '"'), intern = TRUE) %>% grepl("latest", .) %>% any(.) == FALSE) {
    if (nrow(LATEST_image == 0)) {
      if (DEBUG == TRUE) cat(crayon::silver("Pulling docker for ", paste0(browserName, debug_label), " as we don't have the latest\n"))
      
      DOCKER_image = paste0('docker pull --quiet selenium/standalone-', browserName, debug_label)
      
      cli::cli_h1("Pulling docker image")
      cli::cli_alert(DOCKER_image)
      
      system(DOCKER_image)
      
      
      Sys.sleep(5)
    } else {
      if (DEBUG == TRUE) cat(crayon::silver("Not pulling docker for ", paste0(browserName, debug_label), ", already have it\n"))
    }
    

    # Run docker container ----------------------------------------------------

    
    # Run docker session. Map home directory to download docker container
    if (container_name %in% system('docker ps -a --format "{{.Names}}"', intern = TRUE) == FALSE) {
      
      if (DEBUG == TRUE) cat(crayon::yellow("Docker image", container_name, " not running. Launching...\n"))
      
      if (parameters_docker$task_params$local_or_server == "server") {
        
        # REVIEW: https://docs.docker.com/engine/reference/run/ || -e "ENABLE_CORS=true" 
        DOCKER_run = paste0('docker run --rm -t -d ', big_container_str,' --name ', container_name, ' -v /dev/shm:/dev/shm -P selenium/standalone-', browserName, debug_label) # Not mapping local folder
      } else {
        
        
        DOCKER_run = paste0('docker run --rm -t -d ', big_container_str,' --name ', container_name,
                            ' -v ', folder_downloads, ':/home/seluser/Downloads',
                            ' -v /dev/shm:/dev/shm -P selenium/standalone-', browserName, debug_label) # Mapping local folder
        
        # THIS WORKS, BUT ~/Downloads/protocol999/.data NEEDS write permissions 777
        # folder_protocol_local   folder_protocol_docker
        # # -v ~/Downloads/protocol999:/home/seluser/protocol999 
        # Downloads
        # # -v ~/Downloads/protocol999/.data:/home/seluser/Downloads 
        
        # DOCKER_run = paste0(
        #   'docker run --rm -t -d ', big_container_str,' --name ', container_name,
        #   ' -v ', folder_protocol_local, '/.data:/home/seluser/Downloads', # Downloads
        #   ' -v ', folder_protocol_local, ':', folder_protocol_docker, # Protocol
        #   ' -v /dev/shm:/dev/shm',
        #   ' -P selenium/standalone-', browserName, debug_label # Mapping local folder
        #   )
        
  

        
        
      }
      
      
      cli::cli_h1("Running DOCKER")
      cli::cli_alert(DOCKER_run)
      
      system(DOCKER_run)



    } else {
      if (DEBUG == TRUE) cat(crayon::green("Docker image", container_name, " already running.\n"))
    } 
    

    # Get port ---------------------------------------------------------------
    
    container_port_raw <- system(sprintf('docker port %s', container_name), intern = TRUE)
    container_port <- max(as.integer(gsub('.*:(.*)$', '\\1', container_port_raw)))
    if (is.na(container_port)) cat(crayon::red("Port not found?"))
    
    

  # OUTPUT ------------------------------------------------------------------

    list(container_name = container_name,
         container_port = container_port, # THIS REVIEW
         browserName = browserName
         )
    
    
    }
