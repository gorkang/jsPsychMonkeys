#' Completes a jsPsychMaker task
#'
#' @param parameters_monkeys The parameters_monkeys list
#' @param uid User id
#' @param links Links to complete
#' @param remote_driver remote_driver output
#' @param previous_target targets object dependency to control when this target runs
#'
#' @return Completes a task and returns a container name.
#' @export
complete_task <-
  function(parameters_monkeys,
           uid,
           links,
           # container_name = NULL,
           remote_driver = NULL,
           previous_target = "") {

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
    source(here::here("R/main_parameters.R"), local = TRUE)

    container_name = remote_driver$container_name
    remDr = remote_driver$remDr
    time_to_sleep_before_repeating_protocol = as.numeric(parameters_monkeys$links_tar$time_to_sleep_before_repeating_protocol)


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

    # How much the participant should wait after completing the task and before proceeding to repeat it?
    if (index_links > 1 & parameters_monkeys$links_tar$times_repeat_protocol > 1) {
      cli::cli_h1("Sleeping for {time_to_sleep_before_repeating_protocol} s. before repeating protocol")

      # We do it in a loop with 30s naps, because if the nap is too long, the monkeys die
      # time_to_sleep_before_repeating_protocol = 100
      seconds_each_nap = 30
      times_to_do_loop = ceiling(time_to_sleep_before_repeating_protocol/seconds_each_nap)

      for(i in 1:times_to_do_loop) {
        if (DEBUG == TRUE) cli::cli_alert_info("loop {i} / {times_to_do_loop}")
        check_accept_alert(remDr = remDr, DEBUG = FALSE)
        remDr$refresh()
        Sys.sleep(seconds_each_nap)
      }

      # Sys.sleep(parameters_monkeys$links_tar$time_to_sleep_before_repeating_protocol)
    }


    # Go to task --------------------------------------------------------------

    if (DEBUG == TRUE) cli::cli_h1("[[START OF EXPERIMENT]] ")
    if (DEBUG == TRUE) cli::cli_alert_info("{Sys.time()} Waiting  {initial_wait}s. | If it fails, increase initial_wait or wait_retry")
    if (DEBUG == TRUE) cli::cli_alert_info("continue_links = {continue_links} | index_links = {index_links}")

    if (DEBUG == TRUE) cli::cli_alert_info("Opening link: {links[index_links]}\n")
    LAUNCH_TASK = launch_task_safely(links[index_links], wait_retry = wait_retry, remDr = remDr, DEBUG = DEBUG)

    # INITIAL WAIT FOR PAGE TO LOAD


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

        if (DEBUG == TRUE) cli::cli_h1("SCREEN [{index}]")
        if (DEBUG == TRUE) cli::cli_alert_info("continue = {continue} | index = {index} | index_task = {index_task} | try_number = {try_number}")

        # If there is an alert, accept
        check_accept_alert(wait_retry, remDr, DEBUG)

        # Take screenshots
        if (screenshot == TRUE) {

          output_folder = paste0("outputs/screenshots/", parameters_monkeys$task$pid, "/", uid, "/")
          if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

          # Image screenshot
          remDr$screenshot(file = paste0(output_folder, parameters_monkeys$task$pid, "_", uid, "_screenshot_", sprintf("%03d", index), "_", as.Date(Sys.Date(), format = "%Y-%m-%d"), ".png"))

          # HTML screenshot
          writeLines(text = remDr$getPageSource()[[1]], con = paste0(output_folder, parameters_monkeys$task$pid, "_", uid, "_screenshot_", sprintf("%03d", index), "_", as.Date(Sys.Date(), format = "%Y-%m-%d"), ".html"))

        }


        if (console_logs == TRUE) console_logs_list[[index]] = remDr$log(type = "browser")


        ### Get elements of website ----------------------------

          try_number = 1
          wait_retry_loop = wait_retry # Reset to initial value
          continue_elements = FALSE # we don't have an element to interact with yet

          # Continue and go to interact with elements of a website
          # Default is FALSE, because we need to first found the elements and then interact with them
          if (DEBUG == TRUE) cli::cli_h2("\nLOOP GET/PROCESS ELEMENTS | !continue_elements")
          while (!continue_elements) {

            # Wait - Increase wait with each retry
            wait_retry_loop = wait_retry_loop + (try_number/20)
            if (DEBUG == TRUE) cli::cli_alert_info("try_number = {try_number} | wait_retry_loop = {wait_retry_loop} | continue_elements = {continue_elements}")
            if (try_number > 1) Sys.sleep(wait_retry_loop)


            # Get elements
            if (DEBUG == TRUE) cli::cli_h3("GET ELEMENTS")
            # Make sure there are no alerts before retrying
            check_accept_alert(wait_retry_loop, remDr, DEBUG)
            list_get_elements = get_elements_safely(remDr = remDr, index = index, try_number = try_number, DEBUG = DEBUG)

            # When there is not an error, the content is in result
            list_get_elements = list_get_elements$result

            if (DEBUG == TRUE) cli::cli_alert_info("{length(list_get_elements$ID_names)} elements found")
            if (DEBUG == TRUE) cli::cli_h3("PROCESS ELEMENTS [{list_get_elements$percentage_completed}%]")

            # Check list_get_elements, show messages, and determine if we should continue trying to get elements and/or move to the next task
            OUTPUT_process_elements = process_elements(list_get_elements = list_get_elements, try_number = try_number, DEBUG = DEBUG)
            continue_elements = OUTPUT_process_elements$continue_elements # Go to interact with element?
            continue = OUTPUT_process_elements$continue # Go to next screen?

            if (DEBUG == TRUE) cli::cli_alert_info("continue_elements = {continue_elements} | continue = {continue}")

            # CHECKS
            if (!is.null(list_get_elements$error)) cli::cli_alert_danger("ERROR on get_elements_safely()")

            # Tries 1 to 9
            if (DEBUG == TRUE & continue_elements == FALSE & try_number < 10) cli::cli_alert_warning("WARNING: No input|button elements extracted on try {try_number}/10. Retrying in {wait_retry_loop} sec.")

            # Last try, show console errors
            if (continue_elements == FALSE & try_number == 10) {
              if (DEBUG == TRUE) cli::cli_alert_danger("Tried {try_number} times but could not find elements. Stopping.\n
                                                       - If the protocol is slow and this should not happen, you can increase `wait_retry` to make the monkeys more patient.")

              # TODO: DEBUG WHY FAILS WITH PARALLEL MONKEYS
              # browser_console = remDr$log(type = "browser")
              # if (length(browser_console) > 0) browser_console_clean = browser_console |> dplyr::bind_rows() |> dplyr::filter(level == "SEVERE") |> dplyr::pull(message)
              # if (length(browser_console_clean) > 0) cli::cli_alert_danger("Browser console: \n{.code {browser_console_clean}}")

              continue_elements = TRUE # Gets out of the while loop if TRUE. while(!continue_elements) is reversed ()
              continue = FALSE # Stop the task
              }

            try_number = try_number + 1
          }


          # If we must continue
          if (continue_elements == TRUE) {


              ### Interact with the elements we found ------------------

                if (DEBUG == TRUE) cli::cli_h2("INTERACTING WITH ELEMENTS | continue_elements {continue_elements}")
                # seed = (forced_seed_final + index) is critical so there is some variation in case a specific response is needed to continue (e.g. BART)

                  # SEED ---------

                  # Restart seed numbering in each INSTRUCTIONS page
                  if (length(list_get_elements$name_buttons$id) == 1 & all(list_get_elements$name_buttons$id == "jspsych-instructions-next")) {

                    index_task = 1

                    # We can force a seed based on forced_seed + uid
                    if (!is.null(forced_seed) & is.numeric(uid)) {
                      forced_seed_final = forced_seed + uid
                    } else if (is.numeric(uid)) {
                      forced_seed_final = uid
                    } else {
                      forced_seed_final = 1
                    }

                  } else {

                    if(!exists("forced_seed_final")) forced_seed_final = 1

                  }


                  # If we are in consent form, reset forced_seed_final to 1
                  # if (grepl("Consentimiento informado", list_get_elements$name_contents$content)) forced_seed_final = 1

              if (DEBUG == TRUE) cli::cli_alert_info("forced_seed_final: {forced_seed_final} | interact_with_element_safely() seed = {(forced_seed_final + index_task + index)}")

              check_accept_alert(0.2, remDr, DEBUG)
              interact_with_element_safely(list_get_elements, remDr = remDr, DEBUG = DEBUG, index = index, seed = (forced_seed_final + index_task + index))

              # FORCED WAIT ---
                if (forced_random_wait == TRUE) {
                  if (index == 30) {
                    time_wait = sample(c(.2, 1, 5, 10), 1)
                    if (DEBUG == TRUE) cli::cli_alert_info("forced_seed_final: {forced_seed_final}")

                    cli::cli_alert_info("[MONKEY][{index}] uid {uid} forced to wait {time_wait} seconds | forced_random_wait = {forced_random_wait} \n")
                    Sys.sleep(time_wait)
                  }
                }

              # FORCED REFRESH ---
                # forced_refresh = TRUE
                if (!is.null(forced_refresh)) {
                  forced_refresh_n = ifelse(forced_refresh == TRUE, ceiling(runif(1, 1, 1000)), 0)
                  if (index == forced_refresh_n) {
                    cli::cli_alert_info("[MONKEY][{index}] uid {uid} forced to refresh page | forced_refresh = {forced_refresh} \n")
                    jsPsychMonkeys::check_accept_alert(wait_retry = 5, remDr = remDr, DEBUG = FALSE) # To avoid errors
                    remDr$refresh()
                    Sys.sleep(5)
                  }
                }

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
        numbered_console_logs = console_logs_list %>% stats::setNames(seq_along(.))
        DF_console_logs = 1:length(numbered_console_logs) %>% purrr::map_df(~ numbered_console_logs[[.x]] %>% dplyr::bind_rows() %>% dplyr::mutate(page_number = .x)) %>% tidyr::drop_na(message)
        readr::write_csv(DF_console_logs, paste0("outputs/log/", uid, "_console_logs", "_", gsub(":", "-", Sys.time()), ".csv"))
        }

      # links while loop
      index_links = index_links + 1
      if (DEBUG == TRUE) cli::cli_alert_info("index_links end while loop = {index_links} ")

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
