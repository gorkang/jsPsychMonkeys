
create_text <- function(text_question, element, DEBUG = FALSE) {
  
  # DEBUG
  # text_question = present_content[[1]]
  # element = present_elements[[1]][[1]]
  
  if (exists("element")) {
    type_field = present_elements[[1]][[1]]$getElementAttribute("type")
  } else {
    type_field = ""
  }
  
  # Create exceptions for things like age, cellphone number, National ID numbers (RUT in Chile), as they should have a specific number of digits
  if (grepl("Rut Completo", present_content[[1]][[1]], ignore.case = TRUE)) {
    if (DEBUG == TRUE) cat(crayon::yellow(" [RUT] "))
    text_to_send = stringi::stri_rand_strings(n = 1, length = sample(c(2, 7, 8, 9, 9, 9, 10), 1), pattern = "[0-9]")
    
  } else if (grepl("edad", present_content[[1]][[1]], ignore.case = TRUE)) {
    if (DEBUG == TRUE) cat(crayon::yellow(" [edad] "))
    text_to_send = stringi::stri_rand_strings(n = 1, length = sample(c(1, 2, 2, 3), 1), pattern = "[0-9]")
    
  } else if (grepl("celular", present_content[[1]][[1]], ignore.case = TRUE)) {
    if (DEBUG == TRUE) cat(crayon::yellow(" [celular] "))
    text_to_send = stringi::stri_rand_strings(n = 1, length = sample(c(1, 8, 9, 9, 9, 10, 20), 1), pattern = "[0-9]")
    
  } else if (type_field == "number") {
  
      max_num = as.numeric(present_elements[[1]][[1]]$getElementAttribute("max"))
      min_num = as.numeric(present_elements[[1]][[1]]$getElementAttribute("min"))
      if (is.na(max_num)) max_num = 10^20
      if (is.na(min_num)) min_num = 0
      text_to_send = as.character(round(runif(1, min = min_num, max = max_num), 0))
      
  } else {
  
  length_text = sample(c(1, 2, 2, 2, 1:10, 100, 1000), 1)  
  text_to_send =
    sample(
      c(gsub("_|;|:", " ", stringi::stri_rand_strings(n = 1, length = length_text, pattern = "[a-zA-Z_;:]")),
        "example1@gmail.com",
        stringi::stri_rand_strings(n = 1, length = length_text, pattern = "[0-9]")),
      1)
  
  }
  return(text_to_send)
}


get_elements <- function(variables) {
  
  # Response elements ---------------------------------------------
  
  # ID
  # start_id = remDr$findElements(using = 'xpath', "/html/body/div[1]/input[1]")
  start_id = remDr$findElements(using = 'id', "input_uid")
  
  # Radio buttons
  radioButton_vertical <- remDr$findElements(using = 'name', "jspsych-survey-multi-choice-vertical-response-0")
  radioButton_horizontal <- remDr$findElements(using = 'name', "jspsych-survey-multi-choice-horizontal-response-0")
  
  # Checkboxes
  checkbox <- remDr$findElements(using = 'class', "jspsych-survey-multi-select-option")
    
  # Sliders
  slider_input <- remDr$findElements(using = 'xpath', "/html/body/div/div/div/div[2]/input")
  
  # Textboxes
  textbox <- remDr$findElements(using = 'name', "#jspsych-survey-number-response-0")
  textbox2 <- remDr$findElements(using = 'name', "#jspsych-survey-text-response-0")
  
  # Date input
  date_input <- remDr$findElements(using = 'name', "#jspsych-survey-date-response-0")
  
  # Buttongroup
  # image_buttons = remDr$findElements(using = 'id', "jspsych-image-button-response-btngroup")
  image_buttons = remDr$findElements(using = 'class', "jspsych-image-button-response-button")
  
  # FORM
  # form <- remDr$findElements(using = 'id', "jspsych-survey-html-form")
  form_text <- remDr$findElements(using = 'name', "Q0")
  # form <- remDr$findElements(using = 'xpath', "/html/body/div/div/form/input[1]")
  
  # Consent checkbox
  consent_checkbox <- remDr$findElements(using = 'id', "consent_checkbox")
  

  
  # Buttons --------------------------------------------------------
  
  startbutton = remDr$findElements(using = 'id', "check")
  startbutton_old <- remDr$findElements(using = 'id', "start")
  
  redobutton = remDr$findElements(using = 'id', "redo")
  # remDr$findElements(using = 'id', "start_and_skip_to_last")
  # remDr$findElements(using = 'id', "start_and_skip_lastast")
  
  continueButton1v <- remDr$findElements(using = 'id', "jspsych-survey-multi-choice-vertical-next")
  continueButton1h <- remDr$findElements(using = 'id', "jspsych-survey-multi-choice-horizontal-next")
  continueButton1cb <- remDr$findElements(using = 'id', "jspsych-survey-multi-select-next")
  
  # continueButton1 <- remDr$findElements(using = 'class name', "jspsych-survey-multi-choice-vertical")
  continueButton2 <- remDr$findElements(using = 'id', "jspsych-fullscreen-btn")  
  # continueButton3 <- remDr$findElements(using = 'xpath', "/html/body/div/div/form/input")
  continueButton4 <- remDr$findElements(using = 'id', "jspsych-survey-text-next") 
  greyed_button <- remDr$findElements(using = 'xpath', "/html/body/div/div/button")
  next_button <- remDr$findElements(using = 'xpath', value = '/html/body/div/div/div/button')
  form_next <- remDr$findElements(using = 'id', "jspsych-survey-html-form-next")
  html_button <- remDr$findElements(using = 'class', "jspsych-btn")
  
  
  next_inst_button <- remDr$findElements(using = 'id', "jspsych-instructions-next")
  # back_inst_button <- remDr$findElements(using = 'id', "jspsych-instructions-back
  

  # Content -----------------------------------------------------------------
  
  # Capture screen content
  content = remDr$findElements(using = 'class', "justified")
  if (length(content) == 0)  content = remDr$findElements(using = 'class', "jspsych-survey-text")
  if (length(content) == 0)  content = remDr$findElements(using = 'id', "text_input_uid")
  # ADD MORE WAYS TO LOOK FOR CONTENT
  
  if (length(content) > 0) {
    content_str = substr(content[[1]]$getElementText(),  1, 120) %>% gsub("\\\n", "<BR>", .)
  } else {
    content_str = ""
  }
  
  

  # Validation errors
  validation = remDr$findElements(using = 'class name', "required")
  if (length(validation) > 0) validation_text <<- validation[[1]]$getElementText()
  
  
  present_content <<- list(content = content_str)
  
  # Keep only present elements and buttons
  present_elements <<- list(TEXid = start_id,
                            RADBv = radioButton_vertical, 
                            RADBh = radioButton_horizontal,
                            CHBOX = checkbox,
                            SLIDR = slider_input,
                            TEXda = date_input,
                            TEXte = textbox,
                            TEXnu = textbox2,
                            IMGbt = image_buttons,
                            FORte = form_text,
                            CONSc = consent_checkbox)[lengths(list(start_id, radioButton_vertical, radioButton_horizontal, checkbox, slider_input, date_input, textbox, textbox2, image_buttons, form_text, consent_checkbox)) > 0]
  
  present_buttons <<- list(BTsta = startbutton,
                           BTstaold = startbutton_old,
                           BTred = redobutton,
                           BTco1 = continueButton1v,
                           BTco2 = continueButton1h,
                           BTco3 = continueButton1cb,
                           BTco4 = continueButton2,
                           # BTco5 = continueButton3,
                           BTco6 = continueButton4,
                           BTgre = greyed_button,
                           BTnex = next_button,
                           BTfne = form_next,
                           BThtm = html_button,
                           BTinex = next_inst_button)[lengths(list(startbutton, startbutton_old, redobutton, continueButton1v, continueButton1h, continueButton1cb, continueButton2, continueButton4, greyed_button, next_button, form_next, html_button, next_inst_button)) == 1]
  
  # continueButton3, 
  
  # If there are no buttons (e.g. image buttons), fill present_buttons with "
  if (length(present_buttons) == 0) present_buttons <<- NULL
  if (length(present_elements) == 0) present_elements <<- NULL
  
  # lapply(present_elements, function(x){x[[1]]$highlightElement()})
  # lapply(present_buttons, function(x){x[[1]]$highlightElement()})
  
}


check_trialids <- function(local_folder_tasks) {
  
  suppressMessages(suppressWarnings(library(dplyr)))
  suppressMessages(suppressWarnings(library(purrr)))
  suppressMessages(suppressWarnings(library(readr)))
  
  scripts = dir(path = paste0("~/", local_folder_tasks), pattern = "experiment.js", recursive = TRUE, full.names = TRUE)
  if (length(scripts) == 0) stop(paste("Can't find anything in ", local_folder_tasks))

  find_trialids <- function(file_name) {
    
    script = read_file(file_name) 
    expres = ".*?trialid: '(.*?)'.*?"
    trialid = gsub(expres, "\\1; \n", script) %>% gsub("^(.*; \n).*", "\\1", .) %>% gsub(";", "", .) %>% gsub(" number \n", "", .)
    if (grepl("This document was made with test_maker", trialid)) trialid = ""
    strsplit(trialid, " \n")[[1]] %>% as_tibble() %>% mutate(file = file_name) %>% rename(trialid = value) %>% filter(!grepl("^Instructions|^Instructions_[0-9]{2}", trialid))
    
  }
  
  
  DF_all_trialids = map_df(scripts, find_trialids)
  
  rule_check_trialids = "^[a-zA-Z0-9]{1,100}_[0-9]{2}$"
  DF_problematic_trialids = DF_all_trialids %>% 
    filter(!grepl(rule_check_trialids, trialid))
  
  if (nrow(DF_problematic_trialids) > 0) {
    
    message(cat(crayon::red("ISSUES in the following trialid: "), paste(DF_problematic_trialids %>% pull(trialid), collapse = ", ")))

  }
}



#' reconnect_to_VNC
#'
#' @param container_name if empty, show available containers
#' @param port No need to specify one
#' @param DEBUG TRUE/FALSE
#'
#' @return
#' @export
#'
#' @examples reconnect_to_VNC("test1")
reconnect_to_VNC <- function(container_name = NULL, port = NULL, DEBUG = FALSE) {
  
  if (!is.null(port)) container_port = port
  
  if (is.null(container_name)) {
    
    system('docker ps -a --format "{{.Names}}"', intern = TRUE) # Print container names
    
  } else {
    
    container_port_raw <- system(sprintf('docker port %s', container_name), intern = TRUE)
    container_port <- min(as.integer(gsub('.*:(.*)$', '\\1', container_port_raw)))
    if (is.na(container_port[1])) cat(crayon::red("Port not found?"))
    
    
    # Open VNC, using second port in container_port, the password is 'secret'
    vnc_command = paste0('vncviewer 127.0.0.1:', container_port)
    cat(crayon::yellow(paste0("\nOpen VNC - localhost:", container_port, " pwd: secret\n"), crayon::black(vnc_command, "\n")))
    if (DEBUG == TRUE) cat(crayon::silver(" DEBUG:", container_port_raw))
    # system(paste0('echo "', MESSAGE, '"'))

    system(paste0(vnc_command, ' &'))
  }

}





#' debug_function
#' 
#' Loads the parameters used in the functions present in _targets.R to make debugging easier
#'
#' @param name_function 
#'
#' @return
#' @export
#'
#' @examples
debug_function <- function(name_function) {
  
  # DEBUG
  # name_function = "prepare_CRS"
  
  # Function to tar_load or assign the parameters
  load_parameters <- function(parameters_function_separated, NUM) {
    if (length(parameters_function_separated[[NUM]]) == 1) {
      targets::tar_load(parameters_function_separated[[NUM]], envir = .GlobalEnv)
    } else if (length(parameters_function_separated[[NUM]]) == 2) {
      assign(parameters_function_separated[[NUM]][1], parameters_function_separated[[NUM]][2], envir = .GlobalEnv)
    }
  }
  
  
  # Makes possible to use prepare_TASK or "prepare_TASK"
  if (substitute(name_function) != "name_function") name_function = substitute(name_function) #if (!interactive()) is so substitute do not overwrite name_function when in interactive mode
  
  # Parses _targets.R
  code <- parse("_targets.R")
  if (file.exists("targets/targets_main.R")) code <- c(code, parse("targets/targets_main.R"))
  # code <- parse("_targets.R")
  
  # Finds the chunk where name_function is, and cleans the "\"
  text_targets = grep(name_function, code, value = TRUE) %>% gsub("[^A-Za-z0-9\\(\\),_= ]", "", .)
  
  # Gets and separates then parameters of the function
  parameters_function_raw = gsub(paste0(".*", name_function, "\\((.*?)).*"), "\\1", text_targets) %>% gsub(" ", "", .)
  
  if (length(parameters_function_raw) > 0) {
    
    parameters_function_separated = strsplit(parameters_function_raw, ",") %>% unlist() %>% strsplit(., "=")
    
    # For each of the parameters, applies the load_parameters() function
    TEMP = seq_along(parameters_function_separated) %>% map(~ load_parameters(parameters_function_separated, NUM = .x))
    cat(crayon::green("Loaded: "), gsub(",", ", ", parameters_function_raw), "\n")
    
  } else {
    
    cat(crayon::red(paste0("'", name_function, "'", "not found in _targets.R")), "\n")
    
  }
}

