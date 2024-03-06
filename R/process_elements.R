process_elements <- function(list_get_elements, try_number = 1, DEBUG = FALSE) {

  continue = TRUE # By default, should continue in the task while loop
  name_buttons = list_get_elements$name_buttons
  ID_names = list_get_elements$ID_names
  # DIV_names = list_get_elements$DIV_names
  DF_elements_options = list_get_elements$DF_elements_options
  list_elements = list_get_elements$list_elements
  content_screen = DF_elements_options |> tibble::as_tibble() |> dplyr::pull(content)
  percentage_completed = list_get_elements$percentage_completed


  # cli::cli_alert_info("CONTENT: {DF_elements_options$content}")


  # Detect last screen  -----------------------------------------------------

  # Last screen in last task. After this, still a blank page, or a page with a final message
  finish_study_strings = c("FINALIZAR ESTUDIO|FINISH STUDY")
  last_screen = any(grepl(finish_study_strings, name_buttons$content))

  if(length(last_screen) > 0){
    if (DEBUG == TRUE & last_screen == TRUE) cli::cli_h1(cli::col_green("[[FINISH STUDY screen]]"))
  }



  # Already completed -------------------------------------------------------

  # TODO: We have this here, "Detect last screen" above join all this in a single chunk
  already_completed_strings = c("El participante ya completó el protocolo|The participant already completed the protocol|Ya has completado todas las tareas de este protocolo")
  already_completed = any(grepl(already_completed_strings, content_screen))


  # CHECK if we found any elements. The parameter try_number is set in complete_task.
  # if(already_completed){
  #
  #   if (DEBUG == TRUE) cli::cli_h1(cli::col_green("[[Protocol already completed]]"))
  #   list_elements = list(completed = "protocol already completed")
  #   ID_names = ""
  #
  # } else {
    if (DEBUG == TRUE) cli::cli_alert_info("{length(list_elements)} elements extracted: {.code {names(list_elements)}}")
  # }



  # DETECT status ---------------------------------------------------------

  # continue_elements = TRUE: proceed to interact with element
  # continue_elements = FALSE: keep on trying to extract elements!


  if(already_completed){

    if (DEBUG == TRUE) cli::cli_h1(cli::col_green("[[Protocol already completed]]"))
    continue_elements = TRUE
    continue = FALSE

  } else if (percentage_completed == 100){

    if (DEBUG == TRUE) cli::cli_h1(cli::col_magenta("[[END OF EXPERIMENT - 100%]]"))

    continue_elements = FALSE
    # continue = FALSE # With the new system, after consent they reach 100%

  # Initial FULLSCREEN
  } else if (length(ID_names) == 1 & all(ID_names == c("jspsych-fullscreen-btn"))) {

    if (DEBUG == TRUE) withr::with_options(list(crayon.enabled = FALSE), cat(crayon::yellow("[SCREEN]", paste0("[-]"), ":"), crayon::silver(paste0(paste("Fullscreen button")), "\n")))

    DF_elements_options$status = "start"
    continue_elements = TRUE

  } else if (try_number < 10 & length(ID_names) == 0 & all(names(list_elements) == "jspsych-content")) {

    if (DEBUG == TRUE & any(grepl("Cargando protocolo", content_screen))) cli::cli_alert_info("Loading protocol")
    continue_elements = FALSE # try again to get content!

  # END of experiment
  } else if (try_number == 10 & (length(list_elements) == 0 | length(ID_names) == 0)) {

    if (DEBUG == TRUE) cli::cli_h1(cli::col_green("[[END OF EXPERIMENT - try 10]]"))
    continue_elements = TRUE # Go to interact with elements, next step towards finishing protocol
    continue = FALSE

  } else if (length(ID_names) == 1 & "jspsych-html-keyboard-response-stimulus" %in% DF_elements_options$id & !"button" %in% DF_elements_options$type_extracted) {
    # No button, need to press a specific key
    continue_elements = TRUE
  } else if (length(ID_names) == 1 & "jspsych-content" %in% DF_elements_options$id & !"button" %in% DF_elements_options$type_extracted) {

    if (DEBUG == TRUE) cli::cli_h1(cli::col_magenta("[[END OF EXPERIMENT - B]]"))
    continue_elements = TRUE # Go to interact with elements, next step towards finishing protocol
    continue = FALSE

  } else {

    # Keep going and go to interact with the elements!
    continue_elements = TRUE

  }



  # Output ---

  OUTPUT = list(continue_elements = continue_elements,
                continue = continue)

  return(OUTPUT)

}
