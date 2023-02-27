#' Creates a docker container
#'
#' @param container_name Container name
#' @param parameters_monkeys parameters_monkeys list of parameters
#'
#' @return A list with the parameters of the newly created docker container
#' @export
create_docker <-
  function(uid = "test",
           parameters_monkeys = NULL) {

    # DEBUG
    # targets::tar_load_globals()
    # debug_function("create_docker")

# Packages -------------------------------------------------------------
  suppressMessages(suppressWarnings(library(RSelenium)))
  suppressMessages(suppressWarnings(library(dplyr)))
  suppressMessages(suppressWarnings(library(purrr)))



# Check which parameters were entered in parameters_monkeys -----------------

  # If the parameter was entered in the parameters_monkeys list, use it
  source("R/main_parameters.R", local = TRUE)

  if (DEBUG == TRUE) cli::cli_h1("UID: {uid}")

  # VARS
  container_name = paste0("container", uid)




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

      # folder_protocol_local = paste0(folder_downloads, "/", basename(parameters_monkeys$task_params$local_folder_tasks))
      folder_protocol_local = parameters_monkeys$task_params$local_folder_tasks


    } else if (OS == "Windows") {

      grep_command = "findstr"
      # DEBUG
      # parameters_monkeys$task_params$local_folder_tasks = "C:\\\\Users\\\\emrys\\\\Downloads\\\\protocol999"

      # OLD Working
      if (parameters_monkeys$task_params$local_or_server == "local") {
        folder_downloads_temp = paste0(normalizePath(parameters_monkeys$task_params$local_folder_tasks), "")
        # # if (!dir.exists(folder_downloads)) dir.create(folder_downloads)
        if (grepl("C:", folder_downloads_temp)) folder_downloads = gsub("C:", "c", paste0("//",folder_downloads_temp)) %>% dirname() %>% gsub("\\\\", "/", .)
        if (grepl("D:", folder_downloads_temp)) folder_downloads = gsub("D:", "d", paste0("//",folder_downloads_temp)) %>% dirname() %>% gsub("\\\\", "/", .)
        folder_protocol_local = basename(parameters_monkeys$task_params$local_folder_tasks) %>% gsub("C:", "c", paste0("//",folder_downloads))  %>% gsub("\\\\", "/", .)

      }
      # folder_downloads = paste0(normalizePath(parameters_monkeys$task_params$local_folder_tasks), "")
      # folder_protocol_local = gsub("C:", "c", paste0("//",folder_downloads))  %>% gsub("\\\\", "/", .)


    } else if (OS == "Darwin" | OS == "macOS") {

      grep_command = "grep"

      # folder_downloads = folder_downloads
      folder_protocol_local = paste0(folder_downloads, "/", basename(parameters_monkeys$task_params$local_folder_tasks))

    } else {
      stop("Not sure about your operative system.")
    }

    # Common to all OS
    # folder_protocol_docker = paste0("/home/seluser/", basename(parameters_monkeys$task_params$local_folder_tasks))



    # CHECKS ------------------------------------------------------------------
    if (!browserName %in% c("chrome", "firefox")) message("Use 'firefox' or 'chrome' as browserName parameter")
    if (is.null(parameters_monkeys)) parameters_monkeys = list(task = list(local_or_server = "server"))

    # If we are in testing mode, use the test/ folder in the project
    if (parameters_monkeys$task$local_or_server == "test") folder_downloads = here::here("tests/jspsych-6_3_1/")


    if (Sys.info()["sysname"] == "Linux") {

      # Available RAM
      available_RAM = as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern = TRUE))

      if (available_RAM < 80000) {

        NUMBER_dockers_LOW = length(reconnect_to_VNC())

        if (!exists("NUMBER_dockers")) NUMBER_dockers = NUMBER_dockers_LOW
        # cat(crayon::bgRed("\n\n --- LOW RAM --- dockers: ", NUMBER_dockers_LOW, "\n\n"))

        if (NUMBER_dockers >= NUMBER_dockers_LOW) {
          cat(crayon::bgWhite("\n\n --- LOW RAM --- dockers: ", NUMBER_dockers_LOW, "/", NUMBER_dockers, "Pause for 60s...\n\n"))
          Sys.sleep(60)
          NUMBER_dockers = length(reconnect_to_VNC())

        }
      }
    }

    # Check if exists
    if (container_name %in% system('docker ps -a --format "{{.Names}}"', intern = TRUE) == FALSE) {

      # Container does not exist
      if (DEBUG == TRUE) cli::cli_alert_info("Container {container_name} does not exist. Creating it...\n")

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

    # TODO: This is not necessary, as docker run pulls the image if does not exist locally

    DOCKER_images = system("docker images -a", intern = TRUE) |> tibble::as_tibble()
    LATEST_image = DOCKER_images |>
      dplyr::filter(grepl(paste0(browserName, debug_label), value)) |>
      dplyr::filter(grepl("latest", value))

    # Look for the "latest" version of the docker image
    if (nrow(LATEST_image) == 0) {
      if (DEBUG == TRUE) cli::cli_alert_info("Pulling docker image for {.code {paste0(browserName, debug_label)}} as we don't have the latest\n")

      DOCKER_image = paste0('docker pull --quiet selenium/standalone-', browserName, debug_label)

      cli::cli_h1("Pulling docker image")
      cli::cli_alert(DOCKER_image)

      system(DOCKER_image)

      Sys.sleep(5)

    } else {
      if (DEBUG == TRUE) cli::cli_alert_info("Not pulling docker image for {.code {paste0(browserName, debug_label)}}, already have it")
    }


    # Run docker container ----------------------------------------------------

    # https://github.com/SeleniumHQ/docker-selenium#debugging
    # TODO: Redo this code. Avoid the selenium/standalone-chrome-debug, as it is outdated.
      # Use selenium/standalone-chrome

    # Can make the ports more predictable like this:

      # uid = 5:10
      # port1 = 4444:(4444 + length(uid))
      # port2 = 5900:(5900 + length(uid))
      #
      # paste0('docker run -d -p ', port1, ':4444 -p ', port2, ':5900 --shm-size="2g" selenium/standalone-chrome:latest')

    # The VNC port is port2

    # CHANGE reconnect_to_VNC, etc to avoid all the "-debug" related code



    # Run docker session. Map home directory to download docker container
    if (container_name %in% system('docker ps -a --format "{{.Names}}"', intern = TRUE) == FALSE) {

      if (DEBUG == TRUE) cli::cli_alert_info("Docker container {.code {container_name}} not running. Launching...")

      if (parameters_monkeys$task_params$local_or_server == "server") {

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

      if (DEBUG == TRUE) cli::cli_h1("Running Docker")
      if (DEBUG == TRUE) cli::cli_alert(DOCKER_run)

      system(DOCKER_run)


    } else {
      if (DEBUG == TRUE) cat(crayon::green("Docker container", container_name, " already running.\n"))
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
