#' Cleans up docker container
#'
#' @param container_name name of the container
#' @param parameters_monkeys parameters_monkeys list
#'
#' @return NULL
#' @export
#'
clean_up_docker <-
  function(container_name = "test",
           parameters_monkeys = parameters_monkeys
           ) {

    # targets::tar_load_globals()
    # debug_function("clean_up_docker")
    # uid = stringr::str_extract_all(reconnect_to_VNC()[1], pattern = "[0-9]{1,10}") |> unlist()
    # debug_docker(uid_participant = uid)
    # reconnect_to_VNC(container_name = container_name)

  # Check which parameters were entered in parameters_monkeys -----------------

    # If the parameter was entered in the parameters_monkeys list, use it
    source(here::here("R/main_parameters.R"), local = TRUE)


  # CHECK -------------------------------------------------------------------

    if (length(reconnect_to_VNC()) == 0) {
      cli::cli_alert_warning(cli::col_yellow("NO DOCKER IMAGE available\n", cli::col_silver("You probably need to do:\n targets::tar_destroy() & targets::tar_make()\n\n")))
      stop()
    }



  # Keep alive? -------------------------------------------------------------

    if (keep_alive == FALSE) {

      # Container already exists
      if (DEBUG == TRUE) cli::cli_alert_info("Removing container {.code {container_name}} | [keep_alive = {keep_alive}]")


      # Stop and remove containers:
      # If container is running stop it
      if (!purrr::is_empty(system(sprintf('docker ps -q -f name=%s', container_name), intern = TRUE))) {
        # Stop and remove container
        if (DEBUG == TRUE) cli::cli_alert_info("Stopping container")
        OUT = system(sprintf('docker stop %s', container_name), intern = TRUE) # kill container_name
      }

      # # If container exists remove it
      # if (length(system(sprintf('docker container ls -a -f name=%s', container_name), intern = TRUE)) > 1) {
      #   if (DEBUG == TRUE) cli::cli_alert_info("Removing container")
      #   OUT = system(sprintf('docker container rm %s', container_name), intern = TRUE)
      # }

    } else {

      if (DEBUG == TRUE) cli::cli_alert_info("NOT removing container {.code {container_name}} | [keep_alive = TRUE]")

    }

}
