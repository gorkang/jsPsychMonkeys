
# Get all id's, classes... ------------------------------------------------
get_elements <- function(remDr, index = 1, try_number = 1, DEBUG = FALSE) {
  
  # DEBUG -------------------------------------------------------------------
  
    # targets::tar_load_globals()
    # debug_function("complete_task")
    # debug_docker(uid_participant = uid)
    # reconnect_to_VNC(container_name = container_name)
    # DEBUG = TRUE
    # index = 1
    # try_number = 1
    # container_name = remote_driver$container_name
    # remDr = remote_driver$remDr
  

  # CHECKS ------------------------------------------------------------------
  
    if (exists("list_elements_ids")) remove("list_elements_ids")
    if (exists("list_elements_names")) remove("list_elements_names")
    if (exists("list_elements_class")) remove("list_elements_class")
  
  
  # iframes -----------------------------------------------------------------
  
  # This is for jsPsychMaker v0.1. Should be safe to delete now. CHECK
  find_elements <- function() {
    check_accept_alert()
    remDr$findElements(using = "tag name", "iframe")
  }
  # If we find an iframe after page 1, enter in it
  if (index > 1) {
     webElems <- find_elements() #remDr$findElements(using = "tag name", "iframe")
     iframes = sapply(webElems, function(x){x$getElementAttribute("src")}) %>% unlist(); iframes
     if (length(webElems) > 0) remDr$switchToFrame(webElems[[1]])
  }

  # GET source and elements -------------------------------------------------

  # Before getting source, make sure there are no alerts
  get_page_source <- function() {
    check_accept_alert()
    remDr$getPageSource()
  }
  
    page_source = get_page_source()
    page_source_rvest <- rvest::read_html(page_source[[1]])


    # Get html elements -------------------------------------------------------
    
    # Gets html elements for div's, buttons, etc. using parse_elements() and joins all in a DF
    DF_elements_options_raw =
      tibble::tibble(id = NA_character_, name = NA_character_, class = NA_character_, type = NA_character_, status = NA_character_,
             required = NA_character_, hidden = NA_character_,
             min = NA_character_, max = NA_character_, minlength = NA_character_, maxlength = NA_character_) |>
      dplyr::bind_rows(parse_elements("p", page_source_rvest)) |>
      dplyr::bind_rows(parse_elements("div", page_source_rvest)) |>
      dplyr::bind_rows(parse_elements("input", page_source_rvest)) |>
      dplyr::bind_rows(parse_elements("textarea", page_source_rvest)) |>
      dplyr::bind_rows(parse_elements("button", page_source_rvest)) |> 
      
      # Filter out invisible things
      filter(is.na(style) | style != "display: none;")
    
    if (nrow(DF_elements_options_raw) == 0) cli::cli_alert_danger(" ERROR: No elements found in source")
    

    # Columns that we don't have in all the items but that are used in the case_when() below
    cols <- c(list = NA_character_)
    
    
    DF_elements_options = 
      DF_elements_options_raw %>% 
      
      # If columns in cols do not exist, create them. Avoids warnings in case_when() below
      tibble::add_column(., !!!cols[setdiff(names(cols), names(.))]) %>%
      
      # Use data.table to speed up things
      dtplyr::lazy_dt() %>%
    
      # Some hand-made plugins are missing the tag_name. ADD HERE
      mutate(tag_name = 
               case_when(
                 id == "jspsych-html-keyboard-response-stimulus" ~ "input",
                 id == "jspsych-audio-button-response-button-0" ~ "input",
                 
                 TRUE ~ tag_name
               )) %>% 
      
      # FILTERING
      filter(tag_name %in% c("input", "button") | class == "jspsych-content") %>%
      filter(is.na(hidden)) %>%  # Avoid hidden elements
      filter(is.na(type) | type != "hidden") %>% # Avoid hidden elements
      
      # REQUIRED
      mutate(required = ifelse(grepl("", required), TRUE, FALSE)) %>% 
      
      # TRY TO GET NON-STANDARD INPUTS (inputs without id) and assign an id and class
      mutate(id = 
               case_when(
                 tag_name == "input" & is.na(id)  & !is.na(name) ~ name,
                 tag_name == "button" & is.na(id)  & !is.na(class) ~ class,
                 TRUE ~ id),
             class = 
               case_when(
                 tag_name == "input" & is.na(class) & "jspsych-survey-text-0" %in% DF_elements_options_raw$id ~ "jspsych-survey-text-question",
                 TRUE ~ class)
      ) %>% 
      
      # TODO: if required == FALSE: sample between assigning a type_extracted or not. This way we can detect inputs without validation
      
      # EXHAUSTIVE LIST PLUGINS
      mutate(type_extracted = 
               case_when(
                 
                 # No buttons or inputs, needs an keyboard input 
                 id == "jspsych-html-keyboard-response-stimulus" ~ "keyboard_response",
                 
                 # CONTENT
                 class == "jspsych-content" ~ "content",
                 
                 # TYPES (more robust than grepl: e.g. jspsych-survey-text can be a number)
                 type == "button" ~ "button",
                 type == "checkbox" ~ "checkbox",
                 type == "email" ~ "email",
                 type == "number" ~ "number",
                 type == "radio" ~ "radio",
                 type == "range" ~ "slider",
                 type == "text" ~ "text",
                 !is.na(list) ~ "list",
                 
                 # CLASS
                 grepl("jspsych-btn$|jspsych-btn ", class) ~ "button",
                 grepl("jspsych-survey-text$|jspsych-survey-text |jspsych-survey-text-question", class) & type == "date" ~ "date",
                 grepl("jspsych-survey-text$|jspsych-survey-text |jspsych-survey-text-question", class) ~ "text",
                 
                 #NAME
                 grepl("jspsych-survey-multi-choice-vertical-response-0", name) ~ "radio",
                 grepl("jspsych-survey-multi-select-response-0", name) ~ "multi-select",
                 grepl("jspsych-survey-multi-select-option", name) ~ "multi-select",
                 grepl("jspsych-html-slider-response-response", name) ~ "slider", # THIS IS ALSO type == range. WHIS IS FASTER?
                 
                 type == "submit" ~ "button", # Start experiment button
                 
                 
                 # THIS SHOULD NOT EXIST (?): no type, no class, but required. Try with "text"
                 is.na(type) & is.na(class) & required == TRUE ~ "text",
                 
                 TRUE ~ NA_character_)
      ) %>% 
      
      mutate(type_extracted = 
               case_when(
                 # If they are record buttons, are inputs, not buttons
                 class == "jspsych-btn start-recording-button" ~ "input_button",
                 class == "jspsych-btn stop-recording-button" ~ "input_button",
                 
                 # Maybe we should implement a way to try ALL the type_extracted when we have an imput we don't recognize (WITH A WARNING)
                 is.na(type_extracted) & tag_name == "input" & required == FALSE ~ "ALL",
                 TRUE ~ type_extracted
               )) 
      
    # Store table for DEBUG
    # if (DEBUG == TRUE) write_csv(DF_elements_options, paste0("outputs/DF/EXTRACTED_", index, "_NEW.csv"))
    
    
  # Extract remDr elements --------------------------------------------------

    # Get only IDs of inputs and buttons
    ID_names = DF_elements_options %>% filter(tag_name %in% c("input", "button")) %>% tidyr::drop_na(id) %>% pull(id) 
    DIV_names = DF_elements_options %>% filter(tag_name %in% c("div")) %>% tidyr::drop_na(id) %>% pull(id) 
    # Now we are getting id, name and class using DF_elements_options$id. Maybe try to actually use name and class
    # This could be problematic for Consent, and because of some empty elements, elements with repated id's etc.
      # Name_names = DF_elements_options %>% filter(tag_name %in% c("input", "button")) %>% tidyr::drop_na(name) %>% pull(name)
      # Class_names = DF_elements_options %>% filter(tag_name %in% c("input", "button")) %>% tidyr::drop_na(class) %>% pull(class)
      
    
    # Extract all elements with an id. We look for it in the "id", "name" and "class"
    if (length(ID_names) != 0) list_elements_ids = 1:length(ID_names) %>% map(~ remDr$findElements(using = 'id', value = ID_names[.x])) %>% setNames(ID_names) %>% unlist()
    if (length(ID_names) != 0) list_elements_names = 1:length(ID_names) %>% map(~ remDr$findElements(using = 'name', value = ID_names[.x])) %>% setNames(ID_names) %>% unlist()
    if (length(ID_names) != 0) list_elements_class = 1:length(ID_names) %>% map(~ remDr$findElements(using = 'class', value = ID_names[.x])) %>% setNames(ID_names) %>% unlist()

    
    # Combine all elements in a single list: list_elements
    if (any(exists("list_elements_ids") | exists("list_elements_names") | exists("list_elements_class"))) {
      
      list_elements = c(list_elements_ids, list_elements_names, list_elements_class) 
      
    # Does not find anything other than a DIV
    } else if (length(DIV_names) > 0 & !any(exists("list_elements_ids") | exists("list_elements_names") | exists("list_elements_class"))) {
      
      # cli::cli_alert_info("Only a DIV element found: {.code {DIV_names}}\nThis usually means the protocol is starting or has finished")
      list_elements = 1:length(DIV_names) %>% map(~ remDr$findElements(using = 'id', value = DIV_names[.x])) %>% setNames(DIV_names) %>% unlist()

    } else {
      
      list_elements = NULL  
      
    }
      
    


  
  # Inputs, buttons, status -------------------------------------------------
    
    # Filter inputs, buttons and status to end up with a clean list of known names we know how to interact with.
    name_contents = DF_elements_options %>% filter(type_extracted %in% c("content")) |> as_tibble()
    name_inputs = DF_elements_options %>% filter(tag_name == "input" & type_extracted %in% c("checkbox", "date", "email", "html-form", "keyboard_response", "input_button", "list", "multi-select", "text", "number", "radio", "slider", "ALL")) |> as_tibble()
    name_buttons = DF_elements_options %>% filter(tag_name == "button" | type_extracted %in% c("button")) |> as_tibble()
    
    # Back to tibble so we can look inside
    DF_elements_options = DF_elements_options |> as_tibble()
    
   
    
  
  # Create output list -----------------------------------------------------
  list_get_elements = list(list_elements = list_elements,
                           DF_elements_options = DF_elements_options,
                           name_contents = name_contents,
                           name_inputs = name_inputs,
                           name_buttons = name_buttons,
                           # continue = continue,
                           ID_names = ID_names,
                           DIV_names = DIV_names)

  return(list_get_elements)

}
