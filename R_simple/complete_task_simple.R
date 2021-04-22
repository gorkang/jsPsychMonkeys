# Function to complete a task
complete_task_simple <- function(parameters_local_server, uid, DEBUG = FALSE, container_name = NULL, remDr = NULL) { 
  
  # DEBUG
  # DEBUG = TRUE
  # links_tasks = links_tasks[9]
  # links_tasks = "file:///home/seluser/Downloads/test_prototol/index.html?uid=1&pid=999"
  # i = 1
  # DEBUG = FALSE
  # container = 1
  
  # CHECKS --------------------------------------------------------------
  
  if (!exists("uid")) uid = 0
  # if (!exists("DEBUG")) DEBUG = FALSE
  remDr <<- remDr
  
  

  # PARAMETERS --------------------------------------------------------------
  
    source(".vault/SERVER_PATH.R") # server:path
    parameters_local_server$server_path = server_path

  # Create link -------------------------------------------------------------

    if (parameters_local_server$local_or_server == "local") {
      links_tasks = paste0("file:///home/seluser/", parameters_local_server$local_folder_tasks, "/index.html?uid=", uid, "&pid=", parameters$pid)
    } else {
      links_tasks = paste0(parameters_local_server$server_path, parameters$pid, "/?uid=", uid, "&pid=", parameters$pid)
    }
    

  # START LOG ----------------------------------------------------------------

    # Create log for each worker
    if (parameters$debug_file == TRUE) {
      con <- file(paste0("log/pid_", parameters$pid, "_uid_", uid, ".log"))
      sink(con, append = TRUE)
      sink(con, append = TRUE, type = "message")
    }    
  
  
  # Message -----------------------------------------------------------------
  
  cat("\n\n", crayon::green(uid, "- ", links_tasks), "\n")
  if (!exists("time_wait")) time_wait = 1
  
    
  # Go to task --------------------------------------------------------------
  
  # If fails, tries again, waiting a bit longer
  launch_task <- function(links_tasks, time_wait) {
    Sys.sleep(time_wait)
    remDr$navigate(links_tasks)
  }
    
  launch_task_safely = safely(launch_task)
  
  LAUNCH_TASK = launch_task_safely(links_tasks, time_wait = 1)
  while (!is.null(LAUNCH_TASK$error)) {
    if (DEBUG == TRUE) cat(crayon::red("Error navigating to link: "), crayon::silver(LAUNCH_TASK$error), crayon::yellow("Retrying after", paste0(time_wait, "s\n")))
    LAUNCH_TASK = launch_task_safely(links_tasks, time_wait = 5)
  }
  
  
  # Loop through items of a task --------------------------------------------
  
  # Condition to stop while
  continue = TRUE
  
  while (continue) {
    
    get_elements()
    
    # DEBUG - GET NAMES OF THINGS FOUND:
    # cat(crayon::yellow("\n[SCREEN]:", paste(names(present_elements), collapse = ", "), "|", paste(names(present_buttons), collapse = ", "), "[item]:", present_content[[1]]), "")
    
    
    # length(present_elements)
    if (length(present_elements) > 0) {
      
      
      # Store item content ----------------------
      # Will use this to check if we are in the same item when checking if an item does not have validation
      item_content = present_content[[1]]
      
      # Show item content -----------------------
      if (DEBUG == TRUE) {
        
        if (names(present_elements) == "TEXid" & "BTsta" %in% names(present_buttons)) {
          # Starting experiment
          cat(crayon::silver("\n[SCREEN]: START EXPERIMENT  "))
        } else {
          cat(crayon::yellow("\n[SCREEN]:", paste(names(present_elements), collapse = ", "), "|", paste(names(present_buttons), collapse = ", "), "[item]:", present_content[[1]]), "")
        }
        
      }        
      
      
      # START ID BOX -----------------------------------------------------------
      
      if (names(present_elements) == "TEXid" & "BTsta" %in% names(present_buttons)) {
        if (DEBUG == TRUE) cat(crayon::yellow("[SCRIPT]: In START"), "[time]:", as.character(Sys.time()))
        
        get_elements()
        
        # Try to continue without answering
        # present_buttons[[1]][[1]]$clickElement()
        
        max_id = as.numeric(present_elements[[1]][[1]]$getElementAttribute("max"))
        min_id = as.numeric(present_elements[[1]][[1]]$getElementAttribute("min"))
        if (is.na(max_id)) max_id = 10^20
        ID_input = round(runif(1, min = min_id, max = max_id), 0)
        
        
        present_elements[[1]][[1]]$sendKeysToElement(list(as.character(ID_input)))
        # present_elements[[1]][[1]]$highlightElement()
        
        # Continue after answering
        # present_buttons[[1]][[1]]$highlightElement()
        present_buttons[[1]][[1]]$clickElement()
        
        
        if ("BTred" %in% names(present_buttons)) {
          present_buttons[[2]][[1]]$clickElement()
          # SHOULD BE: IF ALERT PRESENT, remDr$acceptAlert()
        }
        # remDr$switchToFrame(0) # Changes frame. In single link protocols, the start button remains hidden...
        
      }
      
      # Image buttons -----------------------------------
      
      if (names(present_elements) == "IMGbt") {
        if (DEBUG == TRUE) cat(crayon::yellow("[SCRIPT]: In Image buttons"))
        
        get_elements()
        
        # Click one of the buttons at random
        button_to_click = sample(1:length(present_elements[[1]]), 1)
        # present_elements[[1]][[button_to_click]]$highlightElement()
        present_elements[[1]][[button_to_click]]$clickElement()
        LAST_TIME = Sys.time()
      }
      
      
      
      # Radio buttons -----------------------------------
      
      if (names(present_elements) == "RADBh" | names(present_elements) ==  "RADBv") {
        if (DEBUG == TRUE) cat(crayon::yellow("[SCRIPT]: In radio"))
        
        get_elements()
        # #cat(crayon::yellow("\n\n[elements]: ", present_content[[1]]), " - ")
        # cat(crayon::green("[button]:", present_buttons[[1]][[1]]$getElementText()), "")
        
        # Try to continue without answering
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
        
        
        # Click radio button at random
        if (length(present_elements$RADBv) > 0) present_elements$RADBv[[sample(1:length(present_elements$RADBv), 1)]]$clickElement()
        if (length(present_elements$RADBh) > 0) present_elements$RADBh[[sample(1:length(present_elements$RADBh), 1)]]$clickElement()
        
        # present_elements$RADBv$getElementAttribute("value")
        
        # Continue after answernig
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
      }
      
      
      # Checkboxes --------------------------------------------------------------
      
      if (names(present_elements) == "CHBOX") {
        if (DEBUG == TRUE) cat(crayon::yellow("[SCRIPT]: In CHBOX"))
        
        get_elements()
        #cat(crayon::yellow("\n\n[elements]: ", present_content[[1]]), " - ")
        
        # Try to continue without answering
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
        
        # Click CHBOX at random
        if (length(present_elements$CHBOX) > 0) {
          
          var = 1:sample(1:length(present_elements$CHBOX), 1) %>% 
            map(~ paste0("x[[", .x, "]]$clickElement()")) %>% 
            unlist() %>% paste(., collapse = ", ") %>% paste0("c(", . ,")")
          
          present_elements %>% map(., function(x) eval(str2expression(((var)))))
          
        }
        
        # Continue after answernig
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
      }
      
      
      # Consent checkbox --------------------------------------------------------
      
      if (names(present_elements) == "CONSc") { # & names(present_buttons) == "BTsta"
        if (DEBUG == TRUE) cat(crayon::yellow("[SCRIPT]: In CONSc"))
        
        get_elements()
        #cat(crayon::yellow("\n\n[elements]: ", present_content[[1]]), " - ")
        
        # Try to continue without answering
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
        
        # Click CHBOX at random
        present_elements[[1]][[1]]$clickElement()
        
        # Continue after answernig
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
        
      }
      
      
      
      # Date --------------------------------------------------------------------
      
      if (names(present_elements) == "TEXda") {
        if (DEBUG == TRUE) cat(crayon::yellow("[SCRIPT]: In date"))
        
        get_elements()
        #cat(crayon::yellow("\n\n[elements]: ", present_content[[1]]), " - ")
        
        # Try to continue without answering
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
        
        present_elements$TEXda[[1]]$clearElement()
        present_elements$TEXda[[1]]$sendKeysToElement(sendKeys = list("20/10/2020"))
        
        # Continue after answernig
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
      }
      
      
      # Slider ------------------------------------------------------------------
      
      if (names(present_elements) == "SLIDR") {
        if (DEBUG == TRUE) cat(crayon::yellow("[SCRIPT]: Slider"))
        
        get_elements()
        
        # Try to continue without answering
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
        
        # [TODO]: If there is no validation, gets here. Not sure how to catch validation in SLIDR to check if it's new...
        get_elements()
        
        
        # Check min, max and initial value of slider to choose a path that is possible
        min_slider = as.integer(present_elements$SLIDR[[1]]$getElementAttribute("min"))
        max_slider = as.integer(present_elements$SLIDR[[1]]$getElementAttribute("max"))
        initial_value_slider = as.integer(present_elements$SLIDR[[1]]$getElementAttribute("value"))
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
        keys_to_send = 1:length(final_vector) %>% purrr::map(~ list(key = final_vector[.x])) %>% flatten()
        present_elements$SLIDR[[1]]$sendKeysToElement(sendKeys = keys_to_send)
        
        
        # Jump to next
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
        
      }
      
      
      # FORM ------------------------------------------------------------------
      
      if (names(present_elements) == "FORte") {
        if (DEBUG == TRUE) cat(crayon::yellow("[SCRIPT]: FORM text"))
        
        get_elements()
        
        # Try to continue without answering
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
        
        # [TODO]: If there is no validation, gets here.
        get_elements()
        
        # If still in FORte 
        if (names(present_elements) == "FORte") {
          
          if (length(present_elements$FORte[[1]]$getElementAttribute("list")) > 0) {
            # This is probably a searchable list 
            
            name_list = present_elements$FORte[[1]]$getElementAttribute("list") %>% unlist()
            options_list = remDr$findElements(using = 'xpath', paste0("//datalist[@id='", name_list, "']/option"))
            num_selected = sample(length(options_list), 1)
            text_selected = options_list[[num_selected]]$getElementAttribute("value") %>% unlist()
            
            present_elements$FORte[[1]]$clearElement() 
            present_elements$FORte[[1]]$sendKeysToElement(list(text_selected))
            
          } else {
            
            text_created = create_text(text_question = present_content[[1]], element = present_elements[[1]][[1]], DEBUG = DEBUG)  
            
            text_created_debug = text_created
            if (nchar(text_created) > 20) text_created_debug = paste(strtrim(text_created, 20), "... and", nchar(text_created) - 20, "more characters")
            
            present_elements[[1]][[1]]$clearElement() 
            present_elements[[1]][[1]]$sendKeysToElement(list(text_created))
            if (DEBUG == TRUE) cat(crayon::silver("\n[Validation]:", crayon::yellow("trying"), "->", text_created_debug))
            
          }
          
          # Jump to next
          present_buttons[[1]][[1]]$clickElement()
          LAST_TIME = Sys.time()
        }          
      }
      
      
      # Text --------------------------------------------------------------------
      
      if (names(present_elements) == "TEXte" | names(present_elements) == "TEXnu") {
        if (DEBUG == TRUE) cat(crayon::yellow("[SCRIPT]: TEXte/nu"))
        
        get_elements()
        #cat(crayon::yellow("\n\n[elements]: ", present_content[[1]]), " - ")
        
        # Try to continue without answering
        present_buttons[[1]][[1]]$clickElement()
        LAST_TIME = Sys.time()
        
        
        get_elements()
        # item_content_validation = present_content[[1]]
        validation_exists = present_elements[[1]][[1]]$getElementAttribute("required")
        
        # If there is no validation, jumps this
        # if (item_content == item_content_validation) {
        if (length(validation_exists) > 0) {
          
          
          # Fill textbox
          # Create an exception for ID numbers (Rut in Chile), as it should have a specific number of digits
          if (grepl("Rut Completo", present_content[[1]][[1]])) {
            if (DEBUG == TRUE) cat(crayon::yellow(" [RUT] "))
            text_created = stringi::stri_rand_strings(n = 1, length = sample(c(2, 7, 8, 9, 9, 9, 10), 1), pattern = "[0-9]")
          } else {
            text_created = create_text(present_content[[1]], DEBUG)  
          }
          
          text_created_debug = text_created
          if (nchar(text_created) > 20) text_created_debug = paste(strtrim(text_created, 20), "... and", nchar(text_created) - 20, "more characters")
          
          present_elements[[1]][[1]]$clearElement() 
          present_elements[[1]][[1]]$sendKeysToElement(list(text_created))
          
          
          # Jump to next
          present_buttons[[1]][[1]]$clickElement()
          
          # Validation errors
          validation = remDr$findElements(using = 'class name', "required")
          if (length(validation) > 0) {
            
            validation_text = validation[[1]]$getElementText()
            # if (DEBUG == TRUE) cat(crayon::silver("\n[Validation]: ", crayon::red(validation_text), "->", text_created_debug))
            if (DEBUG == TRUE) cat(crayon::silver("\n[Validation]:", crayon::yellow("trying"), "->", text_created_debug))
            if (DEBUG == TRUE) cat("->", crayon::red(validation_text))
            
            while (length(validation_text) > 0) {
              
              get_elements()
              
              # Fill textbox
              text_created = create_text(present_content[[1]], DEBUG)
              text_created_debug = text_created
              if (nchar(text_created) > 20) text_created_debug = paste(strtrim(text_created, 20), "... and", nchar(text_created) - 20, "more characters")
              
              present_elements[[1]][[1]]$clearElement() 
              present_elements[[1]][[1]]$sendKeysToElement(list(text_created))
              
              if (DEBUG == TRUE) cat(crayon::silver("\n[Validation]:", crayon::yellow("trying"), "->", text_created_debug))
              
              # Jump to next
              present_buttons[[1]][[1]]$clickElement()
              LAST_TIME = Sys.time()
              
              # Validation errors
              validation = remDr$findElements(using = 'class name', "required")
              if (length(validation) > 0) {
                
                validation_text = validation[[1]]$getElementText()
                if (nchar(text_created) > 20) text_created_debug = paste(strtrim(text_created, 20), "... and", nchar(text_created) - 20, "more characters")
                if (DEBUG == TRUE) cat("->", crayon::red(validation_text))
                
              } else {
                
                validation_text = NULL
                if (nchar(text_created) > 20) text_created_debug = strtrim(text_created, 20)
                if (DEBUG == TRUE) cat(crayon::silver("->", crayon::green("OK")))
                
              }
              
            }
          }
        } else {
          
          if (DEBUG == TRUE) cat(crayon::silver("\n[Validation]: ", crayon::red(crayon::underline("Item does NOT have validation"))))
          
        }
      }
      
      # if present_elements > 0
    } else if (length(present_elements) == 0 & length(present_buttons) > 0) {
      
      # Jump to next --------------------------
      
      # First screen of new task. Print time of last item of the task
      if (length(present_elements) == 0 & names(present_buttons)  == "BTnex") {
        if (exists("LAST_TIME") & DEBUG == TRUE) cat(crayon::yellow("\n[SCREEN]: EMPTY  [SCRIPT]: END TASK [time]:", as.character(LAST_TIME)))
      }
      
      
      
      # Regular New task screen --------------------
      if (DEBUG == TRUE) {
        title_task = remDr$findElements(using = 'tag', "big")
        
        if (length(title_task) > 0) {
          cat(crayon::green("\n\n[SCREEN]: Title Task:", title_task[[1]]$getElementText(), " "))
        } else {
          cat(crayon::green("\n[SCREEN]: EMPTY "))
        }
        
        cat(crayon::yellow("[SCRIPT]: Next trial"), "[time]:", as.character(Sys.time()))
      }
      
      present_buttons[[1]][[1]]$clickElement()
      
      
      # END --------------------------------------------------------------------
    } else if (length(present_elements) == 0 & length(present_buttons) == 0) {
      
      # Try to get elements once more, just in case
      if (DEBUG == TRUE) cat(crayon::black("\n\n[SCREEN]: WAITING 3s"))
      Sys.sleep(5)
      get_elements()
      
      if (length(present_elements) == 0 & length(present_buttons) == 0) {
        continue = FALSE
        if (DEBUG == TRUE) cat(crayon::yellow("[SCRIPT]: [[END task]]", "[time]:", as.character(Sys.time())))
      }
    }
  }
  

  # END LOG -----------------------------------------------------------------

  # Restore output to console
  if (parameters$debug_file == TRUE) {
    sink() 
    sink(type = "message")
  }
  
  return(container_name)
  
}
