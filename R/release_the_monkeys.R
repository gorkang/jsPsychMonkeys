#' release a horde of Monkeys to complete a jsPsychMakeR protocol
#'
#' @param uid user id's for the monkeys. Can be a single number (e.g. 55) or a numeric vector (e.g. 1:10). Determines the number of monkeys that will be released
#' @param times_repeat_protocol if different than 1, creates an URL parameter ID and multiple links changing the uid
#' @param time_to_sleep_before_repeating_protocol In seconds, how long to sleep before repeating protocol
#' @param browserName In which browser should the monkey run the protocol? c("chrome", "firefox")
#' @param big_container Big containers are needed for big protocols: FALSE / TRUE
#' @param keep_alive Keep the docker container alive after finishing the protocol?
#' This is useful to debug.
#' @param folder_downloads Local folder where csv's will be downloaded:
#' - On linux, can be something like ~/Download
#' - On windows, can be something like: C:/Users/myusername/Downloads/protocol999
#' If Monkeys are running OK but no csv's are downloaded, make sure the docker username has write access to the folder
#' @param DEBUG Show debug messages: FALSE / TRUE
#' @param screenshot Should the monkey's take screenshots of each screen they see?
#' - The images will be in outputs/screenshots
#' @param debug_file Store the debug info in a file in outputs/debug
#' @param console_logs Store console logs of browser? Logs are stored in outputs/logs
#' @param open_VNC Show the info to open VNC viewer to see what the monkey's are up to
#' @param pid Protocol id
#' @param uid_URL Include user id in the protocol URL? If true, the uid are predefined
#' @param local_or_server Run the protocol locally or on server c("local", "server)
#' This variable will be inferred from whichever of local_folder_tasks or server_folder_tasks
#' has information.
#' @param local_folder_tasks Local folder where the protocol is stored
#' @param server_folder_tasks Location of the protocol in the server
#' @param disable_web_security Run with CORS disabled? Needed for local protocols that load videos: FALSE / TRUE
#' @param initial_wait Initial wait (in seconds) after entering the main protol page, before
#' the monkeys start to complete the protocol. If the protocol has lots of images to pre-load
#' should be a number big enough for them to download.
#' @param wait_retry How many seconds to wait before retrying, after no elements are found in a page.
#' @param forced_random_wait At some random point in the protocol, the monkey should wait for a random period of time? FALSE / TRUE
#' @param forced_refresh Force a full refresh for some of the monkeys?
#' This is useful to test if the monkey's can continue a protocol after they exit.
#' @param forced_seed Random seed
#' @param dont_ask Assume the responses to all questions are 'Yes'. Not recommended if you don't know what you are doing.
#' @param open_rstudio Open the RStudio project
#' @param credentials_folder folder where files "SERVER_PATH.R" and ".credentials" are. Usually .vault/
#' @param sequential_parallel Run monkeys sequentially or in parallel
#' @param number_of_cores Number of cores to use then running in parallel
#' @param clean_up_targets Clean up (i.e. targets::tar_destroy()) TRUE/FALSE
#'
#' @return Releases monkeys to complete a jsPsychMaker protocol
#' @export
release_the_monkeys <- function(uid = 1,
                                times_repeat_protocol = 1,
                                time_to_sleep_before_repeating_protocol = 1,
                                credentials_folder = NULL,
                                sequential_parallel = "sequential",
                                number_of_cores = ceiling(future::availableCores()/2), # Half of available cores
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
                                forced_refresh = NULL,
                                forced_seed = NULL,
                                dont_ask = TRUE,
                                open_rstudio = FALSE,
                                clean_up_targets = FALSE) {


  # CHECKS ---
  if(is.null(server_folder_tasks) & is.null(local_folder_tasks)) cli::cli_abort("You need to input either `server_folder_tasks` or `local_folder_tasks`")

  if(!is.null(server_folder_tasks) & is.null(credentials_folder)) cli::cli_abort("To run tasks on server I need `credentials_folder`")

  if(sequential_parallel == "parallel" & open_VNC == TRUE) cli::cli_abort("Running monkeys in parallel with `open_VNC = TRUE` is not allowed")





  current_WD = getwd()
  RND_int = round(stats::runif(1, 0, 10000), 0)
  FOLDER = paste0(tempdir(), "/Monkeys_", RND_int)

  jsPsychMonkeys::create_monkeys_project(folder = FOLDER,
                                         credentials_folder = credentials_folder,
                                         uid = uid,
                                         times_repeat_protocol = times_repeat_protocol,
                                         time_to_sleep_before_repeating_protocol = time_to_sleep_before_repeating_protocol,
                                         browserName = browserName,
                                         big_container = big_container,
                                         keep_alive = keep_alive,
                                         folder_downloads = folder_downloads,
                                         DEBUG = DEBUG,
                                         screenshot = screenshot,
                                         debug_file = debug_file,
                                         console_logs = console_logs,
                                         open_VNC = open_VNC,
                                         pid = pid,
                                         uid_URL = uid_URL,
                                         local_or_server = local_or_server,
                                         local_folder_tasks = local_folder_tasks,
                                         server_folder_tasks = server_folder_tasks,
                                         disable_web_security = disable_web_security,
                                         initial_wait = initial_wait,
                                         wait_retry = wait_retry,
                                         forced_random_wait = forced_random_wait,
                                         forced_refresh = forced_refresh,
                                         forced_seed = forced_seed,
                                         dont_ask = dont_ask,
                                         open_rstudio = open_rstudio)


  make_and_clean_monkeys <- function(FOLDER, clean_up_targets, credentials_folder, sequential_parallel, number_of_cores, current_WD) {

    cli::cli_alert_info("Changing WD to {.code {FOLDER}}")
    setwd(dir = FOLDER)

    # list_data_server() does not work on Windows because rsync is not available
    if(!is.null(server_folder_tasks)) {

      # Get credentials
      CREDENTIALS <- get_credentials(credentials_folder)


      pid = paste0(server_folder_tasks)
      cli::cli_alert_info("Checking files for pid = {pid}")
      INITIAL_files = list_data_server(pid = pid, list_credentials = CREDENTIALS$credentials$value) |> dplyr::pull(files)

    } else {
      INITIAL_files = list.files(paste0(local_folder_tasks, "/.data"), pattern = "csv")
    }

    cli::cli_h1("Releasing monkeys")

    if (sequential_parallel == "parallel") {

      cli::cli_alert_info("Using {number_of_cores} CPU cores")

      targets::tar_make_future(workers = number_of_cores)

    } else {

      targets::tar_make()

    }

    # Files in .data after the Monkeys finish
    if(!is.null(server_folder_tasks)) {
      FINAL_files = list_data_server(pid = pid, list_credentials = CREDENTIALS$credentials$value) |> dplyr::pull(files)
    } else {
      FINAL_files = list.files(paste0(local_folder_tasks, "/.data"), pattern = "csv")
    }


    # REMOVE _targets.R
    if (clean_up_targets == TRUE) {
      cli::cli_h1("Monkeys tidying up")
      targets::tar_destroy(ask = FALSE)
      FILES = list.files(paste0(FOLDER, "/_targets/"), recursive = TRUE, full.names = TRUE)
      file.remove(FILES)
    }

    cli::cli_alert_info("Changing WD back to {.code {current_WD}}")
    setwd(dir = current_WD)

    # New files
    NEW_files = FINAL_files[!FINAL_files %in% INITIAL_files]

    return(NEW_files)
  }

  make_and_clean_monkeys_safely = purrr::safely(make_and_clean_monkeys)

  OUTPUT = make_and_clean_monkeys_safely(FOLDER, clean_up_targets, credentials_folder, sequential_parallel, number_of_cores, current_WD)

  # Output message
  if (!is.null(OUTPUT$error)) {
    message_out = paste0("Something went wrong", OUTPUT$error)
    cli::cli_alert_info("Restoring WD back to {.code {current_WD}}")
    setwd(dir = current_WD)
  } else if (!is.null(OUTPUT$result)) {
    message_out = paste0("The Monkeys completed ", length(OUTPUT$result), " tasks.")
  }

  cli::cli_alert_info(message_out)

  OUTPUT_fun = list(message_out = message_out,
                    output = OUTPUT$result,
                    error = OUTPUT$error)

  return(OUTPUT_fun)

}
