select_input <- function(list_get_elements, DEBUG = FALSE) {
  
  # DEBUG
    # remDr$screenshot(display = TRUE)
    
    ## CONTROL + P
    # DEBUG = TRUE
    # debug_docker(24000)
    # source("R/complete_task.R")
    # list_get_elements = get_elements_safely(remDr = remDr, DEBUG = DEBUG, try_number = 2); list_get_elements
    # list_get_elements$result
    
  # SELECTED ----------------------------------------------------------------
  
    # Randomly select one of the inputs
    selected_input = list_get_elements$name_inputs[sample(1:nrow(list_get_elements$name_inputs), 1),]
    selected_input_name = selected_input$id
    
    # if (DEBUG == TRUE) cat(crayon::yellow("Selected", selected_input_name, "from", nrow(list_get_elements$name_inputs), "elements \n"))
    

  # CHECK -------------------------------------------------------------------

    # REVIEW
    # html-form: In html-form items, we do not have (yet) a standard way to name items. BUT we do have one for buttons
    if ("jspsych-btn jspsych-survey-html-form" %in% list_get_elements$name_buttons$class) list_get_elements$name_inputs$type_extracted = "html-form"
  
    # CATCH ALL condition: When we can't extract an input type, try everything (?)
    # TODO: CREATE SAFE ClearElement helper function to be able to include ALL types in the ALL
    if (list_get_elements$name_inputs %>% filter(id == selected_input_name) %>% pull(type_extracted) == "ALL") {
      
      cat(crayon::bgYellow(" WARNING: Unknown type of input· Trying ALL\n"))
      
      # Anything with ClearElement errors. 
      # ERRORS: "date", "html-form" "number", "text",  "slider",
      types_of_input = c("radio", "multi-select")
      selected_input = 1:length(types_of_input) %>% 
        map_df(~ list_get_elements$name_inputs %>% filter(id == selected_input_name) %>% 
                 mutate(type_extracted = types_of_input[.x]))
    }
  
    

  # TYPES OF INPUTS ------------------------------------------------------------
    

  # checkbox -------------------------------------------------------------------
  
  if (any(selected_input$type_extracted %in% c("checkbox"))) {
    
    input_text = selected_input_name
    
    list_get_elements$list_elements[[selected_input_name]]$clickElement()
    
    
  # date --------------------------------------------------------------------  
    
  } else if (any(selected_input$type_extracted %in% c("date"))) {
    
    min_date = clock::year_month_day(1900, 1, 30) %>% clock::as_date()
    max_date = clock::year_month_day(2021, 1, 30) %>% clock::as_date()
    
    input_text = format(sample(seq(min_date, max_date, by = "day"), 1), "%m/%d/%Y")
    
    list_get_elements$list_elements[[selected_input_name]]$clearElement()
    list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))
    
    
  # email --------------------------------------------------------------------  
      
  } else if (any(selected_input$type_extracted %in% c("email"))) {
    
    input_text = paste0(as.character(stringi::stri_rand_strings(n = 1, length = sample(5:10, 1))), "@gmail.com")
    
    list_get_elements$list_elements[[selected_input_name]]$clearElement()
    list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))
    
    
  # html-form -------------------------------------------------------------------
    
  } else if (any(selected_input$type_extracted %in% c("html-form"))) {
      
      number_textboxes = length(list_get_elements$name_inputs$name)
      type = list_get_elements$name_inputs$type
      type[is.na(type)] = "" # If there are NA's, replace
      # if (is.na(type)) type = ""
      
      if (number_textboxes == 1) {
        
        if (type == "number") {
          
          # Default Limits
          min_num = 0
          max_num = 100
          
          # If max and min exist, replace default limits
          if (!is.na(list_get_elements$name_inputs$min)) min_num = as.numeric(list_get_elements$name_inputs$min)
          if (!is.na(list_get_elements$name_inputs$max)) max_num = as.numeric(list_get_elements$name_inputs$max)
          
          # Create random number
          input_text = as.character(sample(min_num:max_num, 1))
          
          list_get_elements$list_elements[[selected_input_name]]$clearElement()
          list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))
          
        } else if (type == "email") {
          
          input_text = paste0(as.character(stringi::stri_rand_strings(n = 1, length = sample(5:10, 1))), "@gmail.com")
          
          list_get_elements$list_elements[[selected_input_name]]$clearElement()
          list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))
          
        } else if (type == "") {
          
          # One textbox. Probably a searchable list
          selected_input = list_get_elements$name_inputs[sample(1:nrow(list_get_elements$name_inputs), 1),]
          
          name_list = list_get_elements$list_elements[[selected_input_name]]$getElementAttribute("list") %>% unlist()
          options_list = remDr$findElements(using = 'xpath', paste0("//datalist[@id='", name_list, "']/option"))
          num_selected = sample(length(options_list), 1)
          input_text = options_list[[num_selected]]$getElementAttribute("value") %>% unlist()
          
          list_get_elements$list_elements[[selected_input_name]]$clearElement()
          list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))
          
        } else {
          if (DEBUG == TRUE) cat(crayon::bgRed(" UNKNOWN html-form \n"))
        }
        
      # Multiple textboxes    
      } else {
        
        input_text = as.character(stringi::stri_rand_strings(n = number_textboxes, length = 10))
        
        1:number_textboxes %>% 
          purrr::walk(~ {
            selected_input_name = list_get_elements$name_inputs$name[.x]
            list_get_elements$list_elements[[selected_input_name]]$clearElement()
            list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text[.x]))
          })
      }
      
    
  # radio -------------------------------------------------------------------
    
  } else if (any(selected_input$type_extracted %in% c("radio"))) {
    
    input_text = selected_input_name
    
    list_get_elements$list_elements[[selected_input_name]]$clickElement()
    
    
  # number ------------------------------------------------------------------
    
  } else if (any(selected_input$type_extracted %in% c("number"))) {
    
    # Default Limits
    min_num = 0
    max_num = 100
    
    content_text = list_get_elements$name_contents$content
    if (length(content_text) == 0) content_text = ""
    
    if (grepl("Rut Completo| rut |celular", content_text)) {
      min_num = 100000000
      max_num = 999999999
    }

    
    # If max and min exist, replace default limits
    if (!is.na(list_get_elements$name_inputs$min)) min_num = as.numeric(list_get_elements$name_inputs$min)
    if (!is.na(list_get_elements$name_inputs$max)) max_num = as.numeric(list_get_elements$name_inputs$max)
    
    # Create random number
    input_text = as.character(sample(min_num:max_num, 1))
    
    list_get_elements$list_elements[[selected_input_name]]$clearElement()
    list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))
    
    
  # text --------------------------------------------------------------------
    
  } else if (any(selected_input$type_extracted %in% c("text"))) {
    
    # Default characters limits
    min_chars = 0
    max_chars = 100
    
    # If minlength and maxlength exist, replace default limits
    if (!is.na(list_get_elements$name_inputs$minlength)) min_chars = as.numeric(list_get_elements$name_inputs$minlength)
    if (!is.na(list_get_elements$name_inputs$maxlength)) max_chars = as.numeric(list_get_elements$name_inputs$maxlength)
    
    input_text = as.character(stringi::stri_rand_strings(n = 1, length = sample(min_chars:max_chars, 1)))
    
    list_get_elements$list_elements[[selected_input_name]]$clearElement()
    list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))
    
    
    
  # slider ------------------------------------------------------------------
    
  } else if (any(selected_input$type_extracted %in% c("slider"))) {
    
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
    
    final_vector = stringr::str_replace_all(path_slider, setNames(c("left_arrow", "b", "right_arrow"), c(-1, 0, 1)))
    input_text = 1:length(final_vector) %>% purrr::map(~ list(key = final_vector[.x])) %>% flatten()
    list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(sendKeys = input_text)
    
    input_text_human_readable = destination_slider
    
    
    # multi-select ------------------------------------------------------------
    
  } else if (any(selected_input$type_extracted %in% c("multi-select"))) {
    
    all_checkboxers = list_get_elements$name_inputs$id
    n_selected = sample(1:length(list_get_elements$name_inputs$name), 1)
    selected_checkboxes = sample(all_checkboxers, n_selected)
    
    input_text = selected_checkboxes
    
    1:length(selected_checkboxes) %>% 
      walk(~ list_get_elements$list_elements[[selected_checkboxes[.x]]]$clickElement())
    
    
  } else {
    
    input_text = "No input elements "
    
  }
  
    

  # OUTPUT ------------------------------------------------------------------

  if (!exists("input_text_human_readable")) input_text_human_readable = input_text
    
  output_select_input = list(selected_input = selected_input,
                             input_text = input_text,
                             input_text_human_readable = input_text_human_readable)
  
  return(output_select_input)
  
}
