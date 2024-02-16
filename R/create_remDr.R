#' Creates remote driver in docker container (opens browser)
#'
#' @param container_port port of container
#' @param browserName c("chrome", "firefox")
#' @param container_name name of container
#' @param parameters_monkeys parameters_monkeys list of parameters
#'
#' @return A remDr object
#' @export
create_remDr <-
  function(container_port,
           browserName,
           container_name = NULL,
           parameters_monkeys = parameters_monkeys
           ) {

  # DEBUG
    # targets::tar_load_globals()
    # debug_function("create_remDr")
    # debug_docker(uid_participant = uid)
    # reconnect_to_VNC(container_name = container_name)

    # DEBUG = TRUE
    # targets::tar_load("parameters_monkeys")
    # debug_docker(uid_participant = 2, parameters_debug = parameters_monkeys)


  # Check which parameters were entered in parameters_monkeys -----------------

    # If the parameter was entered in the parameters_monkeys list, use it
    source(here::here("R/main_parameters.R"), local = TRUE)



  # Create destination folder in local computer when running a local protocol
  if (parameters_monkeys$task_params$local_or_server == "local") {

    # Attempt to directly download the csv's to a .data folder inside the project's folder
    # local_download_folder = paste0(parameters_monkeys$task_params$local_folder_tasks, "/.data")
    # chrome_download_folder = paste0("/home/seluser/Downloads/", basename(parameters_monkeys$task_params$local_folder_tasks), "/.data")
    #
    # if (!dir.exists(local_download_folder)) dir.create(local_download_folder, mode =  "777", showWarnings = TRUE)
    # Sys.chmod(local_download_folder, mode = "0777", use_umask = TRUE)

    # system("ls -la ~/Downloads/protocol999/")
    # unlink(local_download_folder, force = TRUE, recursive = TRUE)

  }

  # CHECK -------------------------------------------------------------------

  # If Docker container does not exist, stop execution.
  # if (length(reconnect_to_VNC(container_name = container_name, just_check = TRUE)) == 0) {
  #   cat(crayon::bgRed("NO DOCKER IMAGE available.\n"), crayon::silver("Maybe need to do:\n targets::tar_destroy() & targets::tar_make()\n\n"))
  #   targets::tar_invalidate(paste0("container_", parameters_task$participants$uid))
  #   cat(crayon::bgYellow("Invalidated ", container_name, " to restart process\n\n"))
  #   stop()
  # }




  # Create browser instance -------------------------------------------------

    if (DEBUG == TRUE) cli::cli_h1("Create remoteDriver: [ {container_port} ] [ {browserName} ] [ {container_name} ]" )

    create_remDr <- function(container_port, browserName, time_wait = 1, errored = FALSE) {
      if (errored == FALSE) {
        if (DEBUG == TRUE) cli::cli_alert_info("Creating remoteDriver in {time_wait}s")
        Sys.sleep(time_wait)
      } else if (errored == TRUE) {
        if (DEBUG == TRUE) cli::cli_alert_warning("Error creating remoteDriver. Retrying in {time_wait}s")
        Sys.sleep(time_wait)
      }

      # extraCapabilities for Chrome
        # See full list: https://peter.sh/experiments/chromium-command-line-switches/ e.g. --incognito or --disable-web-security
        # http://www.chromium.org/for-testers/enable-logging # "--enable-logging"
      chrome_extra_arguments = list(args = list("--start-maximized",
                                                "--incognito",
                                                # "--user-data-dir=/home/seluser/Downloads/ChromeSelenium/",
                                                "--disable-extensions",
                                                "--disable-gpu",
                                                "--disable-dev-shm-usage",
                                                "--no-sandbox") #

                                    # Works to choose a different Download folder.
                                    # But that folder needs 777 permissions in the local computer
                                    # prefs = list(
                                    #   "profile.default_content_settings.popups" = 0L,
                                    #   "download.prompt_for_download" = FALSE,
                                    #   "download.default_directory" = chrome_download_folder #"/home/seluser/Downloads/protocol999/.data"
                                    # )
                                    )


      if (disable_web_security == TRUE & browserName == "chrome") chrome_extra_arguments$args[[length(chrome_extra_arguments$args) + 1]] = "--disable-web-security"

      # Create remote driver
      remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost", port = container_port, browserName = browserName,
                                       extraCapabilities = list(chromeOptions = chrome_extra_arguments))

      return(remDr)
    }
    create_remDr_safely = purrr::safely(create_remDr)

    CREATE_REMOTE_DRIVER = create_remDr_safely(container_port = container_port, browserName = browserName, time_wait = 1)

    if (length(CREATE_REMOTE_DRIVER$error) > 0) {
      cli::cli_alert_danger("Could not create remote driver `create_remDr()`, retrying: {.code {CREATE_REMOTE_DRIVER$error}}")
      CREATE_REMOTE_DRIVER = create_remDr_safely(container_port = container_port, browserName = browserName, time_wait = 2)

    }

    remDr = CREATE_REMOTE_DRIVER$result



  # Close all existing browsers ---------------------------------------------

    if (DEBUG == TRUE) cli::cli_alert_info("Opening browser in a clean session...")

    # Close all browser instances
    clean_open <- function(time_wait = 1) {

      # Wait for driver to be created
      Sys.sleep(time_wait)

      # REMEMBER: if open_VNC == FALSE, THIS FAILS. NO IDEA WHY
      # https://github.com/gorkang/jsPsychMonkeys/issues/8
      remDr$closeall()

      # OPEN browser
      remDr$open(silent = TRUE) #!DEBUG

    }

    clean_open_safely = purrr::safely(clean_open)


    # Open new browser -------------------------------------------------------

    CLEAN_OPEN_SAFELY = clean_open_safely(time_wait = 1)

    if (length(CLEAN_OPEN_SAFELY$error) > 0) {
      cli::cli_alert_warning("Warning: creating closing browsers failed: {CLEAN_OPEN_SAFELY$error}")
      CLEAN_OPEN_SAFELY = clean_open_safely(time_wait = 2)
      if (length(CLEAN_OPEN_SAFELY$error) > 0) {
        cli::cli_alert_danger("Warning: creating closing browsers failed: {CLEAN_OPEN_SAFELY$error}")
        stop()
      }
    }


  # OUTPUT ------------------------------------------------------------------

    output_list = list(remDr = remDr,
                       container_name = container_name)

    return(output_list)

}

