interact_with_element <- function(list_get_elements, DEBUG = FALSE) {
  
  # DEBUG
    # remDr$screenshot(display = TRUE)
  
    ## CONTROL + P
    # DEBUG = TRUE
    # debug_docker(24000)
    # list_get_elements = get_elements(remDr = remDr, DEBUG = DEBUG); list_get_elements
    
  

  # INPUTS ------------------------------------------------------------------
  
    if (length(list_get_elements$name_inputs$id) > 0) {
    
      # source("R/helper_select_input.R")  
      output_select_input = select_input(list_get_elements, DEBUG = DEBUG)
  
    } else {
      
      # if (DEBUG == TRUE) cat(crayon::red("No input elements found\n"))
      output_select_input = list(selected_input = tibble(name = "NO input element found"),
                                 input_text = "''")
      
    }

  
  # BUTTONS -----------------------------------------------------------------
  
    if (length(list_get_elements$name_buttons$id) == 1) {
      
      selected_button_name = list_get_elements$name_buttons$id
      list_get_elements$list_elements[[selected_button_name]]$clickElement()
      
    } else if (length(list_get_elements$name_buttons$id) > 1) {
      
        # IF WE ARE IN CONSENT and debugging: always start
        if (DEBUG == TRUE & all(list_get_elements$name_buttons$id == c("start", "end"))) {

          selected_button_name = c("start", "end")
          list_get_elements$list_elements[["start"]]$clickElement()
          
        } else {
          
          selected_button_name = list_get_elements$name_buttons[sample(1:nrow(list_get_elements$name_buttons), 1),]$id
          list_get_elements$list_elements[[selected_button_name]]$clickElement()
          
        }
      
    } else {
      
      selected_button_name = "No buttons found"

    }
    

  # MESSAGE -----------------------------------------------------------------

  if (DEBUG == TRUE & length(list_get_elements$name_buttons$id) == 1 & list_get_elements$name_buttons$id == "jspsych-instructions-next") cat(crayon::bold("\n[STARTING TASK]:", gsub("\n", "", list_get_elements$name_contents$content), "\n"))

  if (DEBUG == TRUE) cat(crayon::yellow("\n[SCREEN]:", paste(output_select_input$selected_input$name, collapse = ", "), "|", paste(selected_button_name, collapse = ", "), "[response]:", output_select_input$input_text), "\n")


  # Output ------------------------------------------------------------------

  output_interact = 
    list(input = output_select_input$selected_input,
         button = selected_button_name,
         response = output_select_input$input_text)

    # return(output_interact)
  
}
