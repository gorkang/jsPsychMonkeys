interact_with_element <- function(list_get_elements, DEBUG = FALSE) {
  
  # DEBUG
    # remDr$screenshot(display = TRUE)
  
    ## CONTROL + P
    # DEBUG = TRUE
    # debug_docker(24000)
    # list_get_elements = get_elements(remDr = remDr, DEBUG = DEBUG); list_get_elements
    # list_get_elements = list_get_elements$result
  
  

  # INPUTS ------------------------------------------------------------------
  
    if (length(list_get_elements$name_inputs$id) > 0) {
    
      # source("R/helper_select_input.R")  
      output_select_input = select_input(list_get_elements, DEBUG = DEBUG)
  
    } else {
      
      # if (DEBUG == TRUE) cat(crayon::red("No input elements found\n"))
      output_select_input = list(selected_input = tibble(name = "NO input element found"),
                                 input_text = "''")
      
    }
  
  content_str = substr(gsub(".*?(\\n.*$)", "\\1", list_get_elements$name_contents$content),  1, 120) %>% gsub("\\\n", "", .) %>% paste0(., "...")#<BR>
  
  
  # BUTTONS -----------------------------------------------------------------
  
    if (length(list_get_elements$name_buttons$id) == 1) {
      
      selected_button_name = list_get_elements$name_buttons$id
      list_get_elements$list_elements[[selected_button_name]]$clickElement()
      
      # If button is Fullscreen, wait a couple seconds so the interface is responsive
      if (selected_button_name == "jspsych-fullscreen-btn") Sys.sleep(3)
      
    } else if (length(list_get_elements$name_buttons$id) > 1) {
      
        # IF WE ARE IN CONSENT and debugging: always start
        # if (DEBUG == TRUE & all(list_get_elements$name_buttons$id == c("start", "end"))) {
        if (all(list_get_elements$name_buttons$id == c("start", "end"))) {

          selected_button_name = c("start")
          list_get_elements$list_elements[[selected_button_name]]$clickElement()
          
        # If we are in instructions with Back/Forward buttons, always Forward
        } else if (all(list_get_elements$name_buttons$id == c("jspsych-instructions-back", "jspsych-instructions-next"))) {
          
          selected_button_name = c("jspsych-instructions-next")
          list_get_elements$list_elements[[selected_button_name]]$clickElement()
          
        } else {
          
          selected_button_name = list_get_elements$name_buttons[sample(1:nrow(list_get_elements$name_buttons), 1),]$id
          list_get_elements$list_elements[[selected_button_name]]$clickElement()
          
        }
      
    } else {
      
      selected_button_name = "No buttons found"

    }
    

  # MESSAGE -----------------------------------------------------------------

  if (DEBUG == TRUE & length(list_get_elements$name_buttons$id) == 1 & all(list_get_elements$name_buttons$id == "jspsych-instructions-next")) cat(crayon::bold("\n[STARTING TASK]:", gsub("\n", "", list_get_elements$name_contents$content), "\n"))

  if (DEBUG == TRUE) cat(crayon::yellow("\n[SCREEN]:"), crayon::silver(paste0(paste(output_select_input$selected_input$name, collapse = ", "), "|", paste(selected_button_name, collapse = ", "), ":")),  content_str, crayon::yellow("[response]:"), crayon::white( paste(output_select_input$input_text_human_readable, collapse = ", ")), "\n")


  # Output ------------------------------------------------------------------

  output_interact = 
    list(input = output_select_input$selected_input,
         button = selected_button_name,
         response = output_select_input$input_text)

    # return(output_interact)
  
}
