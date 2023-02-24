process_elements <- function(list_get_elements, try_number = 1, DEBUG = FALSE) {
  
  name_buttons = list_get_elements$name_buttons
  ID_names = list_get_elements$ID_names
  # DIV_names = list_get_elements$DIV_names
  DF_elements_options = list_get_elements$DF_elements_options
  list_elements = list_get_elements$list_elements
  content_screen = DF_elements_options |> as_tibble() |> pull(content)
  
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
  already_completed_strings = c("El participante ya completó el protocolo|The participant already completed the protocol")
  already_completed = any(grepl(already_completed_strings, content_screen))

  
  # CHECK if we found any elements. The parameter try_number is set in complete_task.
  if(already_completed){
    
    if (DEBUG == TRUE) cli::cli_h1(cli::col_green("[[Protocol already completed]]"))
    list_elements = list(completed = "protocol already completed")
    ID_names = ""
    
  } else {
    if (DEBUG == TRUE) cli::cli_alert_info("{length(list_elements)} elements extracted: {.code {names(list_elements)}}")
  }
  
  

  # DETECT status. CONTINUE or NOT -------------------------------------------
  
  

  if (length(ID_names) == 1 & all(ID_names == c("jspsych-fullscreen-btn"))) {
    # Initial FULLSCREEN
    if (DEBUG == TRUE) withr::with_options(list(crayon.enabled = FALSE), cat(crayon::yellow("[SCREEN]", paste0("[-]"), ":"), crayon::silver(paste0(paste("Fullscreen button")), "\n")))
    
    DF_elements_options$status = "start"
    continue_elements = TRUE
    
  } else if (try_number < 10 & length(ID_names) == 0 & all(names(list_elements) == "jspsych-content")) {
    
    if (any(grepl("Cargando protocolo", content_screen))) cli::cli_alert_info("Loading protocol")
    continue_elements = FALSE # try again to get content!
    
  } else if (try_number == 10 & (length(list_elements) == 0 | length(ID_names) == 0)) {
    
    if (DEBUG == TRUE) cli::cli_h1(cli::col_green("[[END OF EXPERIMENT]]"))  
    continue_elements = FALSE
    
  } else if (length(ID_names) == 1 & "jspsych-html-keyboard-response-stimulus" %in% DF_elements_options$id & !"button" %in% DF_elements_options$type_extracted) {
    # No button, need to press a specific key
    continue_elements = TRUE
  } else if (length(ID_names) == 1 & "jspsych-content" %in% DF_elements_options$id & !"button" %in% DF_elements_options$type_extracted) {
    
    if (DEBUG == TRUE) cli::cli_h1(cli::col_magenta("[[END OF EXPERIMENT - B]]"))  
    continue_elements = FALSE
    
  } else {
    
    # Keep going!
    continue_elements = TRUE
    
  }
  
  return(continue_elements)
  
}