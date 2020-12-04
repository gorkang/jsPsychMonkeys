
create_text <- function() {
  
  length_text = sample(c(1, 2, 1:10, 100, 1000), 1)  
  text_to_send =
    sample(
    c(gsub("_|;|:", " ", stringi::stri_rand_strings(n = 1, length = length_text, pattern = "[a-zA-Z_;:]")),
      stringi::stri_rand_strings(n = 1, length = length_text, pattern = "[0-9]")), 1)
  return(text_to_send )
}


get_elements <- function(variables) {
  
  # Response elements ---------------------------------------------
  
  # ID
  start_id = remDr$findElements(using = 'xpath', "/html/body/div[1]/input[1]")
  
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
  
  startbutton <- remDr$findElements(using = 'id', "start")
  redobutton = remDr$findElements(using = 'id', "redo")
  # remDr$findElements(using = 'id', "start_and_skip_to_last")
  # remDr$findElements(using = 'id', "start_and_skip_lastast")
  
  continueButton1v <- remDr$findElements(using = 'id', "jspsych-survey-multi-choice-vertical-next")
  continueButton1h <- remDr$findElements(using = 'id', "jspsych-survey-multi-choice-horizontal-next")
  continueButton1cb <- remDr$findElements(using = 'id', "jspsych-survey-multi-select-next")
  
  # continueButton1 <- remDr$findElements(using = 'class name', "jspsych-survey-multi-choice-vertical")
  continueButton2 <- remDr$findElements(using = 'id', "jspsych-fullscreen-btn")  
  continueButton3 <- remDr$findElements(using = 'xpath', "/html/body/div/div/form/input")
  continueButton4 <- remDr$findElements(using = 'id', "jspsych-survey-text-next") 
  greyed_button <- remDr$findElements(using = 'xpath', "/html/body/div/div/button")
  next_button <- remDr$findElements(using = 'xpath', value = '/html/body/div/div/div/button')
  form_next <- remDr$findElements(using = 'id', "jspsych-survey-html-form-next")
  
  

  # Content -----------------------------------------------------------------
  content = remDr$findElements(using = 'class', "justified")
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
                           BTred = redobutton,
                           BTco1 = continueButton1v,
                           BTco2 = continueButton1h,
                           BTco3 = continueButton1cb,
                           BTco4 = continueButton2,
                           BTco5 = continueButton3,
                           BTco6 = continueButton4,
                           BTgre = greyed_button,
                           BTnex = next_button,
                           BTfne = form_next)[lengths(list(startbutton, redobutton, continueButton1v, continueButton1h, continueButton1cb, continueButton2, continueButton3, continueButton4, greyed_button, next_button, form_next)) == 1]
  
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
