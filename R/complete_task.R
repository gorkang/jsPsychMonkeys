#' Completes a jsPsychMaker task
#'
#' @param parameters_monkeys The parameters_monkeys list
#' @param uid User id
#' @param links Links to complete
#' @param remote_driver remote_driver output
#'
#' @return
#' @export
#'
#' @examples
complete_task <-
  function(parameters_monkeys,
           uid,
           links,
           # container_name = NULL,
           remote_driver = NULL) {

  # DEBUG
    # targets::tar_load_globals()
    # debug_function("complete_task")
    # debug_docker(uid_participant = uid)
    # reconnect_to_VNC(container_name = container_name)

    # If No browser opened:
    # debug_function("create_remDr")
    # remote_driver = create_remDr(container_port = container_port, browserName = browserName, container_name = container_name, parameters_monkeys = parameters_monkeys)
    # remDr = remote_driver$remDr


  # Check which parameters were entered in parameters_monkeys -----------------

    # If the parameter was entered in the parameters_monkeys list, use it
    source("R/main_parameters.R", local = TRUE)

    container_name = remote_driver$container_name
    remDr = remote_driver$remDr

    if (DEBUG == TRUE) cli::cli_h1("Completing tasks")


  # CHECKS --------------------------------------------------------------

    # If Docker container does not exist, stop execution.
    if (length(reconnect_to_VNC(container_name = container_name, just_check = TRUE)) == 0) {
      cli::cli_alert_danger("ERROR: No docker image available. Maybe need to do:\n `targets::tar_destroy() & targets::tar_make()`\n\n")
      targets::tar_invalidate(paste0("container_", parameters_monkeys$participants$uid))
      cli::cli_alert_info("Invalidated {.code {container_name}} to restart process")
      stop()

    } else {

      if (DEBUG == TRUE & open_VNC == TRUE) {
        cli::cli_h2("Launching VNC")
        reconnect_to_VNC(container_name, DEBUG = TRUE)
      }
    }

    # Critical variables exist?
    if (!exists("uid")) uid = 0
    if (!exists("wait_retry")) wait_retry = 0.5


  # START LOG ----------------------------------------------------------------

    # Create log for each worker
    if (parameters_monkeys$debug$debug_file == TRUE) {
      con <- file(paste0("outputs/log/pid_", gsub("/", "_", parameters_monkeys$task$pid), "_uid_", uid, ".log"))
      sink(con, append = TRUE)
      sink(con, append = TRUE, type = "message")
    }


  # Loop through links in a specific container and browser ---------------------

  # Condition to stop while
  continue_links = TRUE
  index_links = 1

  while (continue_links) {

    # Go to task --------------------------------------------------------------

    if (DEBUG == TRUE) cli::cli_alert_info("Opening link: {links}")
    LAUNCH_TASK = launch_task_safely(links[index_links], wait_retry = wait_retry, remDr = remDr, DEBUG = DEBUG)

    # INITIAL WAIT FOR PAGE TO LOAD
    if (DEBUG == TRUE) withr::with_options(list(crayon.enabled = FALSE), cat(crayon::bgGreen(paste0("[[START OF EXPERIMENT]] ", Sys.time(), " Waiting ", initial_wait, "s")), crayon::yellow("[If it fails, increase initial_wait or wait_retry]\n")))
    Sys.sleep(initial_wait)

    if (length(LAUNCH_TASK$error) > 0) cli::cli_alert_danger("ERROR: opening link [launch_task()] \n {LAUNCH_TASK$error}")


    ## Loop through items of a task --------------------------------------------

      # Condition to stop while
      continue = TRUE
      index = 1
      index_task = 1
      try_number = 1


      console_logs_list = list()

      # Which screen inside the task. jsPsychMaker creates a single big "task"
      while (continue) {

        # If there is an alert, accept
        check_accept_alert(wait_retry, remDr, DEBUG)

        # Take screenshots
        if (screenshot == TRUE) {
          output_folder = paste0("outputs/screenshots/", parameters_monkeys$task$pid, "/", uid, "/")
          if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
          remDr$screenshot(file = paste0(output_folder, parameters_monkeys$task$pid, "_", uid, "_screenshot_", sprintf("%03d", index), "_", as.Date(Sys.Date(), format = "%Y-%m-%d"), ".png"))
        }

        if (console_logs == TRUE) console_logs_list[[index]] = remDr$log(type = "browser")


        ### Get elements of website ----------------------------

          if (DEBUG == TRUE) cli::cli_h2("Getting elements")
          try_number = 1
          wait_retry_loop = wait_retry # Reset to initial value
          continue_elements = FALSE


          while (!continue_elements) {

            # Increase wait with each retry
            wait_retry_loop = wait_retry_loop + (try_number/10)
            if (try_number > 1) Sys.sleep(wait_retry_loop)

            # Last chance, take 3 extra seconds to give things time to load
            if (try_number == 9) Sys.sleep(wait_retry_loop + 3)

            # Make sure there are no alerts before retrying
            check_accept_alert(wait_retry_loop, remDr, DEBUG)
            list_get_elements = get_elements_safely(remDr = remDr, index = index, try_number = try_number, DEBUG = DEBUG)

            # When there is not an error, the content is in result
            list_get_elements = list_get_elements$result

            # Check list_get_elements, show messages, and determine if we should continue getting elements and/or move to the next task
            OUTPUT_process_elements = process_elements(list_get_elements = list_get_elements, try_number = try_number, DEBUG = DEBUG)
            continue_elements = OUTPUT_process_elements$continue_elements
            continue = OUTPUT_process_elements$continue

            # CHECKS
            if (!is.null(list_get_elements$error)) cli::cli_alert_danger("ERROR on get_elements_safely()")
            if (DEBUG == TRUE & continue_elements == FALSE & try_number < 10) cli::cli_alert_warning("WARNING: No input|button elements extracted on try {try_number}/10. Retrying in {wait_retry_loop} sec.")

            # Last try, show console errors
            if (continue_elements == FALSE & try_number == 10) {
              if (DEBUG == TRUE) cli::cli_alert_danger("Tried {try_number} times but could not find elements. Stopping")
              browser_console = remDr$log(type = "browser")
              if (length(browser_console) > 0) browser_console_clean = browser_console |> bind_rows() |> filter(level == "SEVERE") |> pull(message)
              if (length(browser_console_clean) > 0) cli::cli_alert_danger("Browser console: \n{.code {browser_console_clean}}")
              continue_elements = TRUE # Get out of the while loop # TODO: Shouldn't this be FALSE?: while (!continue_elements) seems to be reversed
              continue = FALSE # Stop the task
              }

            try_number = try_number + 1
          }


          # If we must continue
          if (continue_elements == TRUE) {


              ### Interact with the elements we found ------------------

                if (DEBUG == TRUE) cli::cli_h2("Interacting with elements")
                # seed = (forced_seed_final + index) is critical so there is some variation in case a specific response is needed to continue (e.g. BART)

                  # SEED ---------

                  # Restart seed numbering in each INSTRUCTIONS page
                  if (length(list_get_elements$name_buttons$id) == 1 & all(list_get_elements$name_buttons$id == "jspsych-instructions-next")) {

                    index_task = 1

                    # We can force a seed based on forced_seed + uid
                    if (!is.null(forced_seed) & is.numeric(uid)) {
                      forced_seed_final = forced_seed + uid
                      # set.seed(forced_seed_final)
                    } else if (is.numeric(uid)) {
                      forced_seed_final = uid
                      # set.seed(forced_seed_final)
                    } else {
                      forced_seed_final = 1
                      # set.seed(forced_seed_final)
                    }

                  } else {

                    if(!exists("forced_seed_final")) forced_seed_final = 1

                  }
                  # If we are in consent form, reset forced_seed_final to 1
                  # if (grepl("Consentimiento informado", list_get_elements$name_contents$content)) forced_seed_final = 1

              interact_with_element_safely(list_get_elements, DEBUG = DEBUG, index = index, seed = (forced_seed_final + index_task + index)) #interact_with_element

              # FORCED WAIT ---
                if (forced_random_wait == TRUE) {
                  if (index == 30) {
                    time_wait = sample(c(.2, 1, 5, 10), 1)
                    # time_wait = sample(c(1, 10, 100, 200, 400, 500), 1)
                    cat("[MONKEY]", paste0("[", index, "]"), "uid", uid,"waiting", time_wait, "seconds... \n")
                    Sys.sleep(time_wait)
                  }
                }

              # FORCED REFRESH ---
                # forced_refresh = TRUE
                if (!is.null(forced_refresh)) {
                  if (forced_refresh == TRUE) forced_refresh = sample(c(20, 30, 200, 500, 1000, 10000, 20000), 1)
                  if (index == forced_refresh) {
                    cat("\n[MONKEY]", paste0("[", index, "]"), "uid", uid,"REFRESHING PAGE... \n")
                    remDr$refresh()
                    Sys.sleep(5)
                  }
                }


          # } else {
          #   already_completed_strings = c("El participante ya completó el protocolo|The participant already completed the protocol")
          #   already_completed = any(grepl(already_completed_strings, list_get_elements$DF_elements_options |> as_tibble() |> pull(content)))
          #   if (DEBUG == TRUE & already_completed == TRUE) cli::cli_h1(cli::col_green("[[FINISH PROTOCOL]]: {DF_elements_options |> as_tibble() |> pull(content)}"))

          }

        # Output of while
        # continue
        index = index + 1
        index_task = index_task + 1

      }
      ## END of while task items

      # Store console logs of browser
      if (console_logs == TRUE & length(console_logs_list) > 0) {

        # Store browser console logs
        numbered_console_logs = console_logs_list %>% setNames(seq_along(.))
        DF_console_logs = 1:length(numbered_console_logs) %>% map_df(~ numbered_console_logs[[.x]] %>% bind_rows %>% mutate(page_number = .x)) %>% tidyr::drop_na(message)
        write_csv(DF_console_logs, paste0("outputs/log/", uid, "_console_logs", "_", gsub(":", "-", Sys.time()), ".csv"))
        }

      # links while loop
      index_links = index_links + 1

      # Exit condition for links while loop
      if (is.na(links[index_links])) continue_links = FALSE

  }
  ## END of while links


  # END LOG -----------------------------------------------------------------

    # Restore output to console
    if (parameters_monkeys$debug$debug_file == TRUE) {
      sink()
      sink(type = "message")
    }

  return(container_name)

}
