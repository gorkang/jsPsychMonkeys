#'Process the parameters_monkeys_minimal parameters so a minimal set of input parameters can be used. Also, tries to take into account dependencies between parameters.
#'
#' @param parameters_monkeys_minimal A list with the main input parameters. See parameters_monkeys_minimal in _targets.R.
#' If the parameter is entered in this list, use it. You can also enter the parameter directly
#' @param uid User id for the monkey
#' @param browserName In which browser should the monkey run the protocol? c("chrome", "firefox")
#' @param big_container Big containers are needed for big protocols: FALSE / TRUE
#' @param keep_alive Keep the docker container alive after finishing the protocol?
#' This is useful to debug.
#' @param folder_downloads Local folder where csv's will be downloaded:
#' - On linux, can be something like ~/Download
#' - On windows, can be something like: C:/Users/myusername/Downloads/protocol999
#' If Monkeys are running OK but no csv's are donwloaded, make sure the docker username has write access to the folder
#' @param DEBUG Show debug messages: FALSE / TRUE
#' @param screenshot Should the monkey's take screenshots of each screen they see?
#' - The images will be in outputs/screenshots
#' @param debug_file Store the debug info in a file in outputs/debug
#' @param open_VNC Show the info to open VNC viewer to see what the monkey's are up to
#' @param pid Protocol id
#' @param local_or_server Run the protocol locally or on server c("local", "server)
#' This variable will be inferred from whichever of local_folder_tasks or server_folder_tasks
#' has information.
#' See https://gorkang.github.io/jsPsychR-manual/qmd/04-jsPsychMonkeys.html#launch-monkeys-on-a-server for more information
#' @param local_folder_tasks Local folder where the protocol is stored
#' @param server_folder_tasks Location of the protocol in the server
#' @param initial_wait Initial wait (in seconds) after entering the main protol page, before
#' the monkeys start to complete the protocol. If the protocol has lots of images to pre-load
#' should be a number big enough for them to download.
#' @param wait_retry How many seconds to wait before retrying, after no elements are found in a page.
#' @param forced_random_wait At some random point in the protocol, the monkey should wait for a random period of time? FALSE / TRUE
#' @param forced_seed Random seed
#' @param console_logs Store console logs of browser? Logs are stored in outputs/logs
#' @param uid_URL Include user id in the protocol URL? If true, the uid are predefined
#' @param disable_web_security Run with CORS disabled? Needed for local protocols that load videos: FALSE / TRUE
#' @param forced_refresh Force a full refresh for some of the monkeys?
#' This is useful to test if the monkey's can continue a protocol after they exit.
#'
#' @return a list of parameters for the rest of the jsPsychMonkeys functions
#' @export
#'
#' @examples
#'
#' set_parameters(parameters_monkeys = list(uid = 888, uid_URL = TRUE, forced_seed = 11,
#'                                        local_folder_tasks = "~/Downloads/protocol999",
#'                                        screenshot = FALSE, forced_refresh = FALSE,
#'                                        debug = TRUE, open_VNC = TRUE))
set_parameters <- function(parameters_monkeys_minimal = parameters_monkeys_minimal,
                           uid = 1,
                           browserName = "chrome",
                           big_container = FALSE,
                           keep_alive = FALSE,
                           folder_downloads = NULL,
                           DEBUG = FALSE,
                           screenshot = FALSE,
                           debug_file = FALSE,
                           console_logs = FALSE,
                           open_VNC = FALSE,
                           pid = NULL,
                           uid_URL = TRUE,
                           local_or_server = NULL, # ["local", "server", "test"]
                           local_folder_tasks = NULL, # ["Downloads/tests/test_prototol", "Downloads/tests/2"]
                           server_folder_tasks = NULL,
                           disable_web_security = FALSE,
                           initial_wait = 2,
                           wait_retry = .1,
                           forced_random_wait = FALSE,
                           forced_refresh = FALSE,
                           forced_seed = 11) {

  # DEBUG
  # parameters_monkeys = parameters_monkeys_minimal
  # uid = 1
  # browserName = "chrome"
  # big_container = FALSE
  # keep_alive = FALSE
  # folder_downloads = "~/Downloads"
  # DEBUG = FALSE
  # screenshot = FALSE
  # debug_file = FALSE
  # open_VNC = FALSE
  # pid = 999
  # local_or_server = "test"
  # local_folder_tasks = NULL
  # server_folder_tasks = NULL
  # disable_web_security = FALSE
  # initial_wait = 2
  # wait_retry = 2



# CHECK all folders exist -------------------------------------------------

  necessary_folders = c(".vault", "outputs/DF", "outputs/log", "outputs/screenshots", "outputs/source")
  if (all(necessary_folders %in% dir(recursive = TRUE, include.dirs = TRUE, all.files = TRUE))) {
    if (DEBUG == TRUE) cat(crayon::green("All the necessary folders are present\n"))
  } else {
    if (DEBUG == TRUE) cat(crayon::yellow("Creating necessary folders: "), paste(necessary_folders, collapse = ", "), "\n")
    invisible(purrr::map(necessary_folders, dir.create, recursive = TRUE, showWarnings = FALSE))
  }



  # Check which parameters were entered in parameters_monkeys -----------------

  # If the parameter was entered in the parameters_monkeys list, use it
   source("R/main_parameters.R", local = TRUE)


  # Parameters with dependencies --------------------------------------------

    # local_folder_tasks, server_folder_tasks, local_or_server
    if (!is.null(local_folder_tasks) & !is.null(server_folder_tasks)) cli::cli_abort("Only one of 'local_folder_tasks' or 'server_folder_tasks' must be set in parameters_monkeys_minimal \n")

      if (!is.null(local_folder_tasks)) {
        local_or_server = "local"
        if (is.null(folder_downloads)) folder_downloads = dirname(local_folder_tasks)
      } else if (!is.null(server_folder_tasks)) {
        local_or_server = "server"
      } else {
        cli::cli_abort("You need to set either 'local_folder_tasks' or 'server_folder_tasks' in parameters_monkeys_minimal \n")
      }

      # if (!is.null(parameters_monkeys$local_folder_tasks)) local_folder_tasks = parameters_monkeys$local_folder_tasks
      # if (!is.null(parameters_monkeys$server_folder_tasks)) server_folder_tasks = parameters_monkeys$server_folder_tasks
      # if (!is.null(parameters_monkeys$local_or_server)) local_or_server = parameters_monkeys$local_or_server

      # Disable parameters not compatible with non-chrome browsers
      if (browserName != "chrome") console_logs = FALSE
      if (browserName != "chrome") disable_web_security = FALSE


    # Enable DEBUG if debug_file or open_VNC are TRUE
      if (debug_file == TRUE) DEBUG = TRUE
      if (open_VNC == TRUE) DEBUG = TRUE

      if (debug_file == TRUE) options(cli.num_colors = 1L) # Do not use cli and crayon colors to avoid weird chars in log file



    # CHECK pid ---------------------------------------------------------------

    # If pid not explicit, get from protocol folder
    if (is.null(pid)) {

      if (!is.null(local_folder_tasks)) {
        FOLDER = local_folder_tasks
      } else if (!is.null(server_folder_tasks)) {
        FOLDER = server_folder_tasks
      }
      pid = stringr::str_extract_all(basename(FOLDER), pattern = "[0-9]{1,10}", simplify = TRUE) |> dplyr::last()
      if (length(pid) != 1) cli::cli_alert_warning("Something weird with the pid: {pid}")
    }

  # Create parameters_monkeys list -------------------------------------------

    parameters_monkeys = list(

      participants = list(uid = uid),

      docker = list(
        browserName = browserName,
        big_container = big_container,
        keep_alive = keep_alive,
        folder_downloads = folder_downloads
      ),

      debug = list(
        DEBUG = DEBUG,
        screenshot = screenshot,
        debug_file = debug_file,
        console_logs = console_logs,
        open_VNC = open_VNC
      ),

      remDr_params = list(
        disable_web_security = disable_web_security
      ),

      task_params = list(
        pid = pid,
        uid_URL = uid_URL,
        local_or_server = local_or_server,
        local_folder_tasks = local_folder_tasks,
        server_folder_tasks = server_folder_tasks,
        initial_wait = initial_wait,
        wait_retry = wait_retry,
        forced_random_wait = forced_random_wait,
        forced_refresh = forced_refresh,
        forced_seed = forced_seed
      )
    )


  # OUTPUT -----------------------------------------------------------------

  return(parameters_monkeys)

}
