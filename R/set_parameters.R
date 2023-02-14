#' set_parameters
#'
#'Process the parameters_monkeys_minimal parameters so a minimal set of input parameters can be used. Also, tries to take into account dependencies between parameters.
#' @param parameters_input 
#' @param uid 
#' @param browserName 
#' @param big_container 
#' @param keep_alive 
#' @param folder_downloads 
#' @param DEBUG 
#' @param screenshot 
#' @param debug_file 
#' @param open_VNC 
#' @param pid 
#' @param local_or_server 
#' @param local_folder_tasks 
#' @param server_folder_tasks 
#' @param initial_wait 
#' @param wait_retry 
#' @param forced_random_wait
#' @param forced_seed
#'
#' @return
#' @export
#'
#' @examples
set_parameters <- function(parameters_input = parameters_monkeys_minimal,
                           uid = 1, 
                           browserName = "chrome", 
                           big_container = FALSE, 
                           keep_alive = FALSE, 
                           folder_downloads = dirname(local_folder_tasks),
                           DEBUG = FALSE, 
                           screenshot = FALSE,
                           debug_file = FALSE,
                           console_logs = FALSE,
                           open_VNC = FALSE, 
                           pid = NULL,
                           uid_URL = TRUE,
                           local_or_server = "test", # ["local", "server", "test"]
                           local_folder_tasks = "", # ["Downloads/tests/test_prototol", "Downloads/tests/2"]
                           server_folder_tasks = "",
                           disable_web_security = FALSE,
                           initial_wait = 2,
                           wait_retry = 5,
                           forced_random_wait = FALSE,
                           forced_refresh = NULL,
                           forced_seed = NULL) {
  
  # DEBUG
  # parameters_input = parameters_monkeys_minimal
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
  # local_folder_tasks = ""
  # server_folder_tasks = ""
  # disable_web_security = FALSE
  # initial_wait = 2
  # wait_retry = 2
  

  
# CHECK all folders exist -------------------------------------------------

  necessary_folders = c(".vault", "outputs/DF", "outputs/log", "outputs/screenshots", "outputs/source")
  if (all(necessary_folders %in% dir(recursive = TRUE, include.dirs = TRUE, all.files = TRUE))) {
    cat(crayon::green("All the necessary folders are present\n"))
  } else {
    cat(crayon::yellow("Creating necessary folders: "), paste(necessary_folders, collapse = ", "), "\n")
    invisible(purrr::map(necessary_folders, dir.create, recursive = TRUE, showWarnings = FALSE))
  }
  
    

  # Check which parameters were entered -------------------------------------

    # If we give the input in parameters_monkeys_minimal, use that, else the function's default should be fine
    if (!is.null(parameters_input$uid)) uid = parameters_input$uid
    if (!is.null(parameters_input$browserName)) browserName = parameters_input$browserName
    if (!is.null(parameters_input$big_container)) big_container = parameters_input$big_container
    if (!is.null(parameters_input$keep_alive)) keep_alive = parameters_input$keep_alive
    if (!is.null(parameters_input$folder_downloads)) folder_downloads = parameters_input$folder_downloads
    if (!is.null(parameters_input$DEBUG)) DEBUG = parameters_input$DEBUG
    if (!is.null(parameters_input$screenshot)) screenshot = parameters_input$screenshot
    if (!is.null(parameters_input$debug_file)) debug_file = parameters_input$debug_file
    if (!is.null(parameters_input$console_logs)) console_logs = parameters_input$console_logs
    if (!is.null(parameters_input$open_VNC)) open_VNC = parameters_input$open_VNC
    if (!is.null(parameters_input$pid)) pid = parameters_input$pid
    if (!is.null(parameters_input$uid_URL)) uid_URL = parameters_input$uid_URL
    if (!is.null(parameters_input$disable_web_security)) disable_web_security = parameters_input$disable_web_security
    if (!is.null(parameters_input$initial_wait)) initial_wait = parameters_input$initial_wait
    if (!is.null(parameters_input$wait_retry)) wait_retry = parameters_input$wait_retry
    if (!is.null(parameters_input$forced_random_wait)) forced_random_wait = parameters_input$forced_random_wait
    if (!is.null(parameters_input$forced_refresh)) forced_refresh = parameters_input$forced_refresh
    if (!is.null(parameters_input$forced_seed)) forced_seed = parameters_input$forced_seed
    

  # Parameters with dependencies --------------------------------------------
  
    # local_folder_tasks, server_folder_tasks, local_or_server
    
      if (!is.null(parameters_input$local_folder_tasks)) {
        parameters_input$local_or_server = "local"
      } else if (!is.null(parameters_input$server_folder_tasks)) {
        parameters_input$local_or_server = "server"
      } else {
        cat(crayon::bgRed(" ERROR: you need to set either 'local_folder_tasks' or 'server_folder_tasks' in parameters_monkeys_minimal \n"))
      }
      
      if (!is.null(parameters_input$local_folder_tasks)) local_folder_tasks = parameters_input$local_folder_tasks
      if (!is.null(parameters_input$server_folder_tasks)) server_folder_tasks = parameters_input$server_folder_tasks
      if (!is.null(parameters_input$local_or_server)) local_or_server = parameters_input$local_or_server
      
      # Disable parameters not compatible with non-chrome browsers
      if (browserName != "chrome") console_logs = FALSE
      if (browserName != "chrome") disable_web_security = FALSE
      
      
    # Enable DEBUG if debug_file or open_VNC are TRUE
      if (debug_file == TRUE) DEBUG = TRUE
      if (open_VNC == TRUE) DEBUG = TRUE
  
      
      
      
      
    # CHECK pid ---------------------------------------------------------------
    
    # If pid not explicit, get from protocol folder
    if (is.null(pid)) {
      
      if (!is.null(parameters_input$local_folder_tasks)) {
        FOLDER = parameters_input$local_folder_tasks
      } else if (!is.null(parameters_input$server_folder_tasks)) {
        FOLDER = parameters_input$server_folder_tasks
      }
      pid = stringr::str_extract_all(FOLDER, pattern = "[0-9]{1,10}", simplify = TRUE) |> last()
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