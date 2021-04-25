
# Get all id's, classes... ------------------------------------------------

get_elements <- function(remDr, DEBUG = FALSE) {
  
  # DEBUG -------------------------------------------------------------------
  ## CONTROL + P
  # DEBUG = TRUE
  # # IN _targets.R: parameters_local_server
  # uid = participants$uid[1]
  # targets::tar_load(remoteDriver_24000)
  # remDr <<- remoteDriver_24000$remDr
  # remDr$screenshot(display = TRUE)
  
  # If loaded two times, fails 
    # Sometimes you need to manually run the code of the funtion to work
  # DEBUG = TRUE
  # debug_docker(uid_participant = 2400)
  
  
  # CLEAN UP ----------------------------------------------------------------
  suppressWarnings(remove(screen_elements_id))
  suppressWarnings(remove(screen_elements_name))
  ID_names = NULL
  NAME_names = NULL
  
  
  # Get page source ---------------------------------------------------------
  
  page_source = remDr$getPageSource()
  
  # cat(crayon::bgYellow("SOURCE; ", page_source))
  # stop()
  

  # GET IDs and NAMEs -------------------------------------------------------

    # We use stringr::str_extract_all(page_source, pattern = "id=\".*?\"") to extract all id's and name's from source code of website. We could add also class, or other elements.

  
    # IDs ---------------------------------------------------------------------
    
      # Clean up id's
  
      # REVIEW proper names of jspsych plugins. ***WE SHOULD USE STANDARD WAY TO NAME ITEMS*** (e.g. consent forms... html plugin!)
      # This is to get rid of MOST of the id's found. PRO: map below is faster. CON: the system is not UNIVERSAL. 
      # USE COMPLETE LIST OF PLUGINS IMPLEMENTED: jspsych-html-slider-response-response, jspsych-fullscreen-btn, etc.
      # TODO: SHOULD LOOK FOR WAY TO FIND ONLY INPUTS AND BUTTONS
      IDs = stringr::str_extract_all(page_source, pattern = "id=\".*?\"")
      ID_names_raw = gsub("id=\"|\"| ", "", IDs[[1]]) %>% unique(.) #; cat("ID_names_raw: ", crayon::silver(paste(ID_names_raw, collapse = "; "), "\n"))
      ID_names = grep("input_uid|check$|start|end|response-response|response-0-[0-99]|next$|jspsych-fullscreen-btn|jspsych-image-button-response-button|jspsych-survey-text-0|jspsych-download-as-text-link", ID_names_raw, value = TRUE)
      
      if (DEBUG == TRUE & length(ID_names) != 0) cat("ID_names: ", crayon::silver(paste(ID_names, collapse = "; "), "\n"))
      
  
    # NAMES -------------------------------------------------------------------
    
      # Clean up names
      NAMEs = stringr::str_extract_all(page_source, pattern = " name=\".*?\"") # Added space so "data-name=" is not catched
      NAME_names_raw = gsub("name=\"|\"| ", "", NAMEs[[1]]) %>% unique(.)
      NAME_names = discard(gsub("generator|author|created|changedby|robots", NA, NAME_names_raw), is.na)
      
      if (DEBUG == TRUE & length(NAME_names) != 0)  cat("NAME_names: ", crayon::silver(paste(NAME_names, collapse = "; "), "\n"))
   
      
    

    # END OF EXPERIMENT (?) ---------------------------------------------------

      if (length(ID_names) == 0 & length(NAME_names) == 0) {
        
        if (DEBUG == TRUE) cat(crayon::red("No ID's or NAMEs found: "), "\n\nIDs: ", crayon::silver(paste(IDs, collapse = "; "), "\nNAMEs", crayon::silver(paste(NAMEs, collapse = "; "))), "\n\n")
        
        if (ID_names_raw == "jspsych-content") {
          
          # If it's last page, can be empty. Wait to see if the "jspsych-download-as-text-link" id appears    
          if (DEBUG == TRUE) cat(crayon::bgGreen("\n  END OF EXPERIMENT (?). waiting 3s  \n"))
          Sys.sleep(3)
          page_source = remDr$getPageSource()
          IDs = stringr::str_extract_all(page_source, pattern = "id=\".*?\"")
          ID_names_raw = gsub("id=\"|\"| ", "", IDs[[1]]) %>% unique(.) #; cat("ID_names_raw: ", crayon::silver(paste(ID_names_raw, collapse = "; "), "\n"))
          ID_names = grep("input_uid|check$|start|end|response-response|response-0-[0-99]|next$|jspsych-fullscreen-btn|jspsych-image-button-response-button|jspsych-survey-text-0|jspsych-download-as-text-link", ID_names_raw, value = TRUE)
        }
        
      }

  # Combine IDs and NAMEs for names and remDr elements -----------------------------

    # Extract all elements with an id
    if (length(ID_names) != 0) screen_elements_id = 1:length(ID_names) %>% map(~ remDr$findElements(using = 'id', value = ID_names[.x])) %>% setNames(ID_names)
    if (length(NAME_names) != 0) screen_elements_name = 1:length(NAME_names) %>% map(~ remDr$findElements(using = 'name', value = NAME_names[.x])) %>% setNames(NAME_names)
    
    if (!exists("screen_elements_id")) screen_elements_id = NULL
    if (!exists("screen_elements_name")) screen_elements_name = NULL
    
    
    # COMBINE names and elements
    names_ALL = c(ID_names, NAME_names)
    screen_elements_all = c(screen_elements_id, screen_elements_name)
  

  # Create big list with everything ------------------------------------------
    
    # This is a very costly step. Can take a couple seconds in a page with multiple inputs and buttons
    # REVIEW: How to make it faster
  
    # If we found something on the website
    if (!is.null(screen_elements_all)) {
        
        # Create a list with remDr elements (can interact with them) and a nested tibble with options/parameters of those elements 
        LIST_screen_elements =
          1:length(screen_elements_all) %>% 
          # furrr::future_map(~ list(element = screen_elements_all[[.x]][[1]],
          map(~ list(element = screen_elements_all[[.x]][[1]],
                     options = tibble(
                       name = names(screen_elements_all)[[.x]],
                       tag_name =  as.character(purrr::pluck(screen_elements_all[[.x]][[1]]$getElementTagName(), .default = NA)),
                       content = as.character(purrr::pluck(screen_elements_all[[.x]][[1]]$getElementText(), .default = NA)),
                       class = as.character(purrr::pluck(screen_elements_all[[.x]][[1]]$getElementAttribute("class"), .default = NA)),
                       max = as.character(purrr::pluck(screen_elements_all[[.x]][[1]]$getElementAttribute("max"), .default = NA)),
                       min = as.character(purrr::pluck(screen_elements_all[[.x]][[1]]$getElementAttribute("min"), .default = NA)),
                       value = as.character(purrr::pluck(screen_elements_all[[.x]][[1]]$getElementAttribute("value"), .default = NA)),
                       required = as.character(purrr::pluck(screen_elements_all[[.x]][[1]]$getElementAttribute("required"), .default = NA)),
                       number = length(screen_elements_all[[.x]][[1]]$findElements(using = "xpath", value = paste0('//*[@name="', names_ALL[.x],'"][@type="number"] | //*[@id="', names_ALL[.x],'"][@type="number"]'))) == 1,
                       text = length(screen_elements_all[[.x]][[1]]$findElements(using = "xpath", value = paste0('//*[@name="', names_ALL[.x],'"][@type="text"] | //*[@id="', names_ALL[.x],'"][@type="text"]'))) == 1,
                       radio = length(screen_elements_all[[.x]][[1]]$findElements(using = "xpath", value = paste0('//*[@id="', names_ALL[.x],'"][@type="radio"]'))) == 1,
                       submit = length(screen_elements_all[[.x]][[1]]$findElements(using = "xpath", value = paste0('//*[@id="', names_ALL[.x],'"][@type="submit"]'))) == 1
                     )
          )
          ) %>% setNames(names_ALL)
        

      # Separate only the remDr elements
      list_elements = 
        1:length(LIST_screen_elements) %>% map(~ LIST_screen_elements[[.x]]$element) %>% setNames(names_ALL)

      
      # DF_elements_options -----------------------------------------------------

      DF_elements_options = 
        
        1:length(LIST_screen_elements) %>% 
        map_df(~ LIST_screen_elements[[.x]]$options) %>% 
        mutate_if(is.list, unlist) %>% 
        
        # CLASSIFY elements. REVIEW; SHOULD USE LIST OF NAMES BY PLUGIN?
        mutate(type = 
                 case_when(
                   
                   # PLUGINS
                   name == "jspsych-html-slider-response-response" ~ "slider",
                   grepl("jspsych-survey-multi-select-response-0-[0-999]", name) ~ "multiselect",
                   grepl("jspsych-survey-date-response-0", name) ~ "date",
                   
                   # BUTTONS
                   name == "jspsych-image-button-response-button-0" ~ "button",
                   
                   
                   # grepl("jspsych-survey-html-form", name) ~ "html-form", # NEED TO have a standard for html-form
                   
                   
                   # MANUAL (!!!)
                   radio == TRUE ~ "radio",
                   number == TRUE ~ "number",
                   text == TRUE ~ "text",
                   
                   # MANUAL BUTTONS (!!!)
                   submit == TRUE ~ "button",
                   tag_name == "button" ~ "button",
                   content == "acepto participar" ~ "button",
                   content == "rechazo participar" ~ "button",
                   
                   name == "Q0" ~ "html-form", # NEED TO have a standard for html-form
                   TRUE ~ NA_character_
                 ),
               
               status = 
                 case_when(
                   content == "Usted ya ha completado todas las tareas de este protocolo." ~ "end",
                   name == "input_uid" ~ "start",
                   name == "jspsych-download-as-text-link" ~ "end",
                   TRUE ~ "item"
                 )
               ) %>% 
        
        arrange(name) %>% 
        
        # REVIEW: gets rid of duplicates. Can cause issues (?)
        distinct(name, .keep_all = TRUE)
      

      # Inputs, buttons, status -------------------------------------------------

      # Filter inputs, buttons and status to end up with a clean list of known names we know how to interact with.
        # Could use a more universal approach (?): filter(tag_name == "input" & tag_name != "button" & submit == FALSE) OR (tag_name == "button" | submit == TRUE)  
      name_inputs = DF_elements_options %>% filter(type %in% c("date", "html-form", "multiselect", "text", "number", "radio", "slider"))
      name_buttons = DF_elements_options %>% filter(type %in% c("button"))
      name_status = DF_elements_options %>% filter(status %in% c("end", "end", "item", "start"))
      
      # If end it should mean the end of the experiment 
      if ("end" %in% name_status$status) {
        
        if (DEBUG == TRUE) cat(crayon::bgGreen("\n  GOOD END OF EXPERIMENT. CHECK: outputs/END.png  \n"))
        if (DEBUG == TRUE) remDr$screenshot(file = "outputs/END-get_elements-good-end.png")
        continue = FALSE
        
      } else {
        
        continue = TRUE
        
      }
      
    
    # If the website is empty
    } else {
      
      if (DEBUG == TRUE) cat(crayon::bgRed("\n  NO ELEMENTS FOUND ON WEBSITE. CHECK: outputs/END.png  \n"))
      if (DEBUG == TRUE) remDr$screenshot(file = "outputs/END-get_elements-empty.png")
      
      list_elements = NULL
      DF_elements_options = NULL
      name_inputs = NULL
      name_buttons = NULL
      name_status = NULL
      continue = FALSE
      
    } 
  
  # Create output list  
  list_get_elements = list(list_elements = list_elements,
                           DF_elements_options = DF_elements_options,
                           name_inputs = name_inputs,
                           name_buttons = name_buttons,
                           name_status = name_status,
                           continue = continue)
  
  return(list_get_elements)
  
}
