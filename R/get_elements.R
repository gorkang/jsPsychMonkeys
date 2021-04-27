
# Get all id's, classes... ------------------------------------------------

get_elements <- function(remDr, index = 1, DEBUG = FALSE) {
  
  # DEBUG -------------------------------------------------------------------
  
  # if (index == 1) stop()
  # if (DEBUG == TRUE) cat(crayon::red("SCREEN:", crayon::silver(index), "\n\n"))
  
  # reconnect_to_VNC(container_name = "container24000")
  # remDr$screenshot(display = TRUE)
  
  # DEBUG = TRUE
  # index = 2
  # debug_docker(uid_participant = 24000)

  
  
  # iframes -----------------------------------------------------------------
  
  # If we find an iframe after page 1, enter in it
  if (index > 1) {
     webElems <- remDr$findElements(using = "tag name", "iframe")
     iframes = sapply(webElems, function(x){x$getElementAttribute("src")}) %>% unlist(); iframes
     if (length(webElems) > 0) remDr$switchToFrame(webElems[[1]])
  }

  # GET source and elements -------------------------------------------------

    page_source = remDr$getPageSource()
    page_source_rvest <- read_html(page_source[[1]])



  # Get html elements -------------------------------------------------------

    # Gets html elements for div's, buttons, etc.
    page = page_source_rvest %>% html_elements("p")
    inputs = page_source_rvest %>% html_elements("input")
    buttons = page_source_rvest %>% html_elements("button")
    div = page_source_rvest %>% html_elements("div")
    
    # CHECK
    if (DEBUG == TRUE & (length(page) == 0 & length(inputs) == 0 & length(buttons) == 0 & length(div) == 0)) cat(crayon::bgRed(" ERROR: No elements found in source \n"))
    
    
  # Builds table with all attributes of elements -----------------------------
    
    # IMPORTANT: all input and button elements SHOULD have an id
    
    # DF_attributes
    DF_elements_options_raw = 
      # Elements that should be in the df (we look for them below)
      tibble(id = NA_character_, name = NA_character_, class = NA_character_, type = NA_character_, status = NA_character_, 
             required = NA_character_, hidden = NA_character_, 
             min = NA_character_, max = NA_character_, minlength = NA_character_, maxlength = NA_character_) %>% 
      
      # Bind all types of html elements
      bind_rows(if (length(page) > 0) {1:length(page) %>% map_df(~ page[[.x]] %>%  html_attrs()  %>% bind_rows()%>% mutate(tag_name = "p", content = page[[.x]] %>% html_text2()))}) %>%
      bind_rows(if (length(div) > 0) {1:length(div) %>% map_df(~ div[[.x]] %>%  html_attrs()  %>% bind_rows() %>% mutate(tag_name = "div", content = div[[.x]] %>% html_text2()))}) %>%
      bind_rows(if (length(inputs) > 0) {1:length(inputs) %>% map_df(~ inputs[[.x]] %>%  html_attrs()  %>% bind_rows()%>% mutate(tag_name = "input", content = inputs[[.x]] %>% html_text2()))}) %>% 
      bind_rows(if (length(buttons) > 0) {1:length(buttons) %>% map_df(~ buttons[[.x]] %>%  html_attrs()  %>% bind_rows() %>% mutate(tag_name = "button", content = buttons[[.x]] %>% html_text2()))})

    # DF_elements_options_raw #%>% select(id, tag_name, class, content)
    
    DF_elements_options = 
      DF_elements_options_raw %>% 
      
      # FILTERING
      filter(tag_name %in% c("input", "button") | class == "jspsych-content") %>%
      filter(is.na(hidden)) %>% # Avoid hidden elements
      
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
                 
                 # CONTENT
                 class == "jspsych-content" ~ "content",
                 
                 # TYPES (more robust than grepl: e.g. jspsych-survey-text can be a number)
                 type == "button" ~ "button",
                 type == "number" ~ "number",
                 type == "radio" ~ "radio",
                 type == "checkbox" ~ "checkbox",
                 type == "range" ~ "slider",
                 type == "text" ~ "text",
                 
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
                 # Maybe we should implement a way to try ALL the type_extracted when we have an imput we don't recognize (WITH A WARNING)
                 is.na(type_extracted) & tag_name == "input" & required == FALSE ~ "ALL",
                 TRUE ~ type_extracted
               ))
      

    # DF_elements_options

    # Store table for DEBUG
    if (DEBUG == TRUE) write_csv(DF_elements_options, paste0("outputs/DF/EXTRACTED_", index, "_NEW.csv"))
    

    
  # Extract remDr elements --------------------------------------------------

    # Get only IDs of inputs and buttons
    # SHOULD NOT BE POSSIBLE TO HAVE AN EMPTY id %>% tidyr::drop_na(id) 
    ID_names = DF_elements_options %>% filter(tag_name %in% c("input", "button")) %>% pull(id)
    
    # Extract all elements with an id. We look for it in the "id", "name" and "class"
    if (length(ID_names) != 0) list_elements_ids = 1:length(ID_names) %>% map(~ remDr$findElements(using = 'id', value = ID_names[.x])) %>% setNames(ID_names) %>% unlist()
    if (length(ID_names) != 0) list_elements_names = 1:length(ID_names) %>% map(~ remDr$findElements(using = 'name', value = ID_names[.x])) %>% setNames(ID_names) %>% unlist()
    if (length(ID_names) != 0) list_elements_class = 1:length(ID_names) %>% map(~ remDr$findElements(using = 'class', value = ID_names[.x])) %>% setNames(ID_names) %>% unlist()
    
    if (any(exists("list_elements_ids") | exists("list_elements_names") | exists("list_elements_class"))) {
      list_elements = c(list_elements_ids, list_elements_names, list_elements_class) 
    } else {
      list_elements = NULL
    }
    
    
    # list_elements[[1]]$highlightElement()
    # if (DEBUG == TRUE & length(list_elements) == 0) stop()
    # CHECK
    if (DEBUG == TRUE & length(list_elements) == 0) cat(crayon::bgYellow(" WARNING: No elements extracted [get_elements] \n"))

  
  # Inputs, buttons, status -------------------------------------------------
    
    # Filter inputs, buttons and status to end up with a clean list of known names we know how to interact with.
    name_contents = DF_elements_options %>% filter(type_extracted %in% c("content"))
    name_inputs = DF_elements_options %>% filter(tag_name == "input" & type_extracted %in% c("checkbox", "date", "html-form", "multi-select", "text", "number", "radio", "slider", "ALL"))
    name_buttons = DF_elements_options %>% filter(tag_name == "button" | type_extracted %in% c("button"))
    
    
  # DETECT status. CONTINUE or NOT -------------------------------------------
    
    # content == "Usted ya ha completado todas las tareas de este protocolo." ~ "end",
    # name == "jspsych-download-as-text-link" ~ "end",
    
    # Initial FULLSCREEN
    if (length(ID_names) == 1 & all(ID_names == c("jspsych-fullscreen-btn"))) {
      
      if (DEBUG == TRUE) cat(crayon::bgYellow("\n  START of experiment \n"))
      DF_elements_options$status = "start"
      continue = TRUE
  
    # TODO: use name_buttons and name_inputs
    } else if (length(ID_names) == 1 & "jspsych-content" %in% DF_elements_options$id & !"button" %in% DF_elements_options$type_extracted) {
      
      if (DEBUG == TRUE) cat(crayon::bgYellow("\n  END of experiment \n"))
      # DF_elements_options$status = "end"
      continue = FALSE
      
    } else if (length(list_elements) == 0 | length(ID_names) == 0) {
      
      if (DEBUG == TRUE) cat(crayon::bgGreen("\n  END OF EXPERIMENT. NO elements found. CHECK: outputs/END.png  \n"))
      if (DEBUG == TRUE) remDr$screenshot(file = "outputs/END-get_elements-good-end.png")
      continue = FALSE

    # Keep going!
    } else  {
      
      continue = TRUE
    }
    

  
  # Create output list -----------------------------------------------------
  list_get_elements = list(list_elements = list_elements,
                           DF_elements_options = DF_elements_options,
                           name_contents = name_contents,
                           name_inputs = name_inputs,
                           name_buttons = name_buttons,
                           # name_status = name_status,
                           continue = continue)
  
  return(list_get_elements)
  
}
