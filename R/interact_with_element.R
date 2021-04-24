interact_with_element <- function(list_get_elements, DEBUG = FALSE) {
  
  # DEBUG
  # DEBUG = TRUE
  # debug_docker(24000)
  # list_get_elements = get_elements2(remDr = remDr, DEBUG = DEBUG); list_get_elements
  # remDr$screenshot(display = TRUE)
   
  
  select_input <- function(list_get_elements) {
    
    # REVIEW
    # In html-form items, we do not have (yet) a standard way to name items. BUT we do have one for buttons
    if ("jspsych-btn jspsych-survey-html-form" %in% list_get_elements$name_buttons$class) list_get_elements$name_inputs$type = "html-form"
    
    selected_input = list_get_elements$name_inputs[sample(1:nrow(list_get_elements$name_inputs), 1),]
    selected_input_name = selected_input$name
    
    if (DEBUG == TRUE) cat(crayon::yellow("Selected", selected_input_name, "from", nrow(list_get_elements$name_inputs), "elements \n"))
    
    
    
    # Date --------------------------------------------------------------------  
    
      if (selected_input$type %in% c("date")) {
      
      min_date = clock::year_month_day(1900, 1, 30) %>% clock::as_date()
      max_date = clock::year_month_day(2021, 1, 30) %>% clock::as_date()
      
      input_text = format(sample(seq(min_date, max_date, by = "day"), 1), "%m/%d/%Y")
      
      list_get_elements$list_elements[[selected_input_name]]$clearElement()
      list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))
      
      
    # html-form -------------------------------------------------------------------
      
    } else if (selected_input$type %in% c("html-form")) {
      
      number_textboxes = length(list_get_elements$name_inputs$name)
      if (number_textboxes == 1) {
      
        # One textbox. Probably a searchable list
        selected_input = list_get_elements$name_inputs[sample(1:nrow(list_get_elements$name_inputs), 1),]
        
        name_list = list_get_elements$list_elements[[selected_input_name]]$getElementAttribute("list") %>% unlist()
        options_list = remDr$findElements(using = 'xpath', paste0("//datalist[@id='", name_list, "']/option"))
        num_selected = sample(length(options_list), 1)
        input_text = options_list[[num_selected]]$getElementAttribute("value") %>% unlist()
          
        list_get_elements$list_elements[[selected_input_name]]$clearElement()
        list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))
        
      } else {
      
        # Multiple textboxes  
        input_text = as.character(stringi::stri_rand_strings(n = number_textboxes, length = 10))
        
        1:number_textboxes %>% 
          purrr::map(~ {
            selected_input_name = list_get_elements$name_inputs$name[.x]
            list_get_elements$list_elements[[selected_input_name]]$clearElement()
            list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text[.x]))
            })
        
      }
      

    # Radio -------------------------------------------------------------------

    } else if (selected_input$type %in% c("radio")) {
      
      input_text = selected_input_name
      
      list_get_elements$list_elements[[selected_input_name]]$clickElement()


    # Number ------------------------------------------------------------------
        
    } else if (selected_input$type %in% c("number")) {
      
      input_text = as.character(sample(1:100, 1))
      
      list_get_elements$list_elements[[selected_input_name]]$clearElement()
      list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))


    # Text --------------------------------------------------------------------
        
    } else if (selected_input$type %in% c("text")) {
      
      input_text = as.character(stringi::stri_rand_strings(n = 1, length = 10))
      
      list_get_elements$list_elements[[selected_input_name]]$clearElement()
      list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))


    
    # Slider ------------------------------------------------------------------
        
    } else if (selected_input$type %in% c("slider")) {
      
      # Check min, max and initial value of slider to choose a path that is possible
      min_slider = as.integer(selected_input$min)
      max_slider = as.integer(selected_input$max)
      initial_value_slider = as.integer(selected_input$value) #present_elements$SLIDR[[1]]$getElementAttribute("value")
      destination_slider = sample(min_slider:max_slider, 1)
      trip_slider = destination_slider - initial_value_slider
      path_slider = rep(trip_slider / abs(trip_slider), abs(trip_slider))
      # If we will stay in the same position, need to choose a path that does not go beyond min or max
      if (length(path_slider) == 0) {
        if (initial_value_slider == min_slider) {
          path_slider = c(1, -1)
        } else {
          path_slider = c(-1, 1)
        }
      }
      
      replacement = c("left_arrow", "b", "right_arrow")
      patterns = c(-1, 0, 1)
      final_vector = stringr::str_replace_all(path_slider, setNames(replacement, patterns))
      
      input_text = 1:length(final_vector) %>% purrr::map(~ list(key = final_vector[.x])) %>% flatten()
      
      list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(sendKeys = input_text)
      

    # Multi select ------------------------------------------------------------

    } else if (selected_input$type %in% c("multiselect")) {
      
      all_checkboxers = list_get_elements$name_inputs$name
      n_selected = sample(1:length(list_get_elements$name_inputs$name), 1)
      selected_checkboxes = sample(all_checkboxers, n_selected)
      
      input_text = selected_checkboxes
      
      1:length(selected_checkboxes) %>% 
        walk(~ list_get_elements$list_elements[[selected_checkboxes[.x]]]$clickElement())


    } else {
      
      input_text = "No input elements "
      
      # if (DEBUG == TRUE) cat(crayon::red("No input elements found\n"))
    }
  
    output_select_input = list(selected_input = selected_input,
                               input_text = input_text)
    return(output_select_input)
    
  }
  
  

  # STATUS ------------------------------------------------------------------

  #length is faster than nrow
  if (length(list_get_elements$name_status$name) == 1) {
    
    if (DEBUG == TRUE) cat(crayon::bgGreen("\n  START OF EXPERIMENT interact  \n"))
    # Sys.sleep(2)
    # list_get_elements$name_status
     
  }
  
  # INPUTS ------------------------------------------------------------------
  
  if (length(list_get_elements$name_inputs$name) == 1) {
    
    output_select_input = select_input(list_get_elements)

    
  } else if (nrow(list_get_elements$name_inputs) > 1) {
    
    output_select_input = select_input(list_get_elements)
    
  } else {
    
    # if (DEBUG == TRUE) cat(crayon::red("No input elements found\n"))
    output_select_input = list(selected_input = 
                                 tibble(name = "NO input element found"),
                               input_text = "")
    
  }
  
  
  
  # BUTTONS -----------------------------------------------------------------
  
  if (length(list_get_elements$name_buttons$name) == 1) {
    
    selected_button_name = list_get_elements$name_buttons$name
    list_get_elements$list_elements[[selected_button_name]]$clickElement()
    
  } else if (nrow(list_get_elements$name_buttons) > 1) {
    
    # WE ARE IN CONSENT
    if (DEBUG == TRUE & all(list_get_elements$name_buttons$name == c("end", "start"))) list_get_elements$list_elements[["start"]]$clickElement()
    
    selected_button_name = list_get_elements$name_buttons[sample(1:nrow(list_get_elements$name_buttons), 1),]$name
    # selected_button_name = selected_button$name
    list_get_elements$list_elements[[selected_button_name]]$clickElement()
    
  } else {
    
    selected_button_name = "No buttons found"
    # No buttons found
    # cat(crayon::red("No buttons found\n"))
  }
  
  

  # MESSAGE -----------------------------------------------------------------

  if (DEBUG == TRUE) cat(crayon::yellow("\n[SCREEN]:", paste(output_select_input$selected_input$name, collapse = ", "), "|", paste(selected_button_name, collapse = ", "), "[response]:", output_select_input$input_text), "\n")
  # if (DEBUG == TRUE) cat(crayon::yellow("\n[SCREEN]:", paste(output_select_input$selected_input$type, collapse = ", "), "|", paste(selected_button$name, collapse = ", "), "[response]:", output_select_input$input_text), "")
  

  
  # Check end ---------------------------------------------------------------
  
    # THIS IS THE END OF THE EXPERIMENT
  if ("end" %in% list_get_elements$name_status$status) {
    continue_interact = FALSE 
  } else {
    continue_interact = TRUE
  }
  
  return(continue_interact)
  
}