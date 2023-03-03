#' release a horde of Monkeys to complete a jsPsychMakeR protocol
#'
#' @param uid .
#' @param browserName .
#' @param big_container .
#' @param keep_alive .
#' @param folder_downloads .
#' @param DEBUG .
#' @param screenshot .
#' @param debug_file .
#' @param console_logs .
#' @param open_VNC .
#' @param pid project id
#' @param uid_URL .
#' @param local_or_server .
#' @param local_folder_tasks .
#' @param server_folder_tasks .
#' @param disable_web_security .
#' @param initial_wait .
#' @param wait_retry In seconds, how much to wait before retrying
#' @param forced_random_wait .
#' @param forced_refresh .
#' @param forced_seed .
#' @param dont_ask .
#' @param open_rstudio .
#' @param credentials_folder folder where files "SERVER_PATH.R" and ".credentials" are. Usually .vault/
#' @param sequential_parallel .
#' @param number_of_cores .
#' @param clean_up_targets .
#'
#' @return Releases monkeys to complete a jsPsychMaker protocol
#' @export
release_the_monkeys <- function(uid = 1,
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
  if(!is.null(server_folder_tasks) & is.null(credentials_folder)) cli::cli_abort("To run tasks on server I need `credentials_folder`")



  current_WD = getwd()
  RND_int = round(stats::runif(1, 0, 10000), 0)
  FOLDER = paste0(tempdir(), "/Monkeys_", RND_int)

  jsPsychMonkeys::create_monkeys_project(folder = FOLDER,
                                         credentials_folder = credentials_folder,
                                         uid = uid,
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

      cli::cli_alert_info("{number_of_cores} monkeys released")

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
