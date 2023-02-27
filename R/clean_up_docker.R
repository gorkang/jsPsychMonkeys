#' Cleans up docker container
#'
#' @param container_name name of the container
#' @param parameters_monkeys parameters_monkeys list
#'
#' @return
#' @export
#'
#' @examples
clean_up_docker <-
  function(container_name = "test",
           parameters_monkeys = parameters_monkeys
           ) {


  # Check which parameters were entered in parameters_monkeys -----------------

    # If the parameter was entered in the parameters_monkeys list, use it
    source("R/main_parameters.R", local = TRUE)


  # CHECK -------------------------------------------------------------------

    if (length(reconnect_to_VNC()) == 0) {
      cat(crayon::bgYellow("NO DOCKER IMAGE available.\n"), crayon::silver("Probably need to do:\n targets::tar_destroy() & targets::tar_make()\n\n"))
      stop()
    }



  # Keep alive? -------------------------------------------------------------

    if (keep_alive == FALSE) {

      # Container already exists
      if (DEBUG == TRUE) cli::cli_alert_info("Removing container {.code {container_name}}")

      # Stop and remove containers:
      # If container is running stop it
      if (!is_empty(system(sprintf('docker ps -q -f name=%s', container_name), intern = TRUE))) {
        # Stop and remove container
        system(sprintf('docker stop %s', container_name)) # kill container_name
      }

      # If container exists remove it
      if (length(system(sprintf('docker container ls -a -f name=%s', container_name), intern = TRUE)) > 1) {
        system(sprintf('docker container rm %s', container_name))
      }

    } else {

      if (DEBUG == TRUE) cli::cli_alert_info("NOT removing container {.code {container_name}} | [keep_alive = TRUE]")

    }

}
