# Function to complete a task
complete_task <-
  function(parameters_task,
           uid,
           links,
           initial_wait = 2,
           wait_retry = 2,
           forced_random_wait = FALSE,
           screenshot = FALSE,
           DEBUG = FALSE,
           console_logs = TRUE,
           open_VNC = FALSE,
           container_name = NULL,
           remDr = NULL) {
    
  # DEBUG
    # debug_function("complete_task")
    # targets::tar_load("parameters_monkeys")
    # debug_docker(uid_participant = 2, parameters_debug = parameters_monkeys)

    
  # CHECKS --------------------------------------------------------------
  
    # If Docker container does not exist, stop execution.
    if (length(reconnect_to_VNC(container_name = container_name, just_check = TRUE)) == 0) {
      cat(crayon::bgRed("NO DOCKER IMAGE available.\n"), crayon::silver("Maybe need to do:\n targets::tar_destroy() & targets::tar_make()\n\n"))
      targets::tar_invalidate(paste0("container_", parameters_task$participants$uid))
      cat(crayon::bgYellow("Invalidated ", container_name, " to restart process\n\n"))
      stop()
      
    } else {
      
      if (DEBUG == TRUE & open_VNC == TRUE) {
        cat(crayon::bgWhite("\n\nVNC launching\n\n"))
        reconnect_to_VNC(container_name, DEBUG = TRUE)
      }
    }
    
    # Critical variables exist?
    if (!exists("uid")) uid = 0
    if (!exists("wait_retry")) wait_retry = 1
    
    
    # Maybe necessary (?)
    remDr <<- remDr
    
    
  # SAFER functions ---------------------------------------------------------

    # Launch task
    launch_task <- function(links, wait_retry) {
      if (length(links) != 1) stop("links passed to remDr$navigate are != 1")
      Sys.sleep(wait_retry) 
      if (DEBUG == TRUE) withr::with_options(list(crayon.enabled = !parameters_task$debug$debug_file), cat(crayon::yellow("\n\nOpening link:", links, "\n")))
      remDr$navigate(links)
    }
    
    launch_task_safely = safely(launch_task)
    

  # START LOG ----------------------------------------------------------------

    # Create log for each worker
    if (parameters_task$debug$debug_file == TRUE) {
      con <- file(paste0("outputs/log/pid_", gsub("/", "_", parameters_task$task$pid), "_uid_", uid, ".log"))
      sink(con, append = TRUE)
      sink(con, append = TRUE, type = "message")
    }
    
    
  # Loop through links in a specific container and browser ---------------------
  
  # Condition to stop while
  continue_links = TRUE
  index_links = 1
  
  while (continue_links) {    
    
    # Go to task --------------------------------------------------------------
    
    LAUNCH_TASK = launch_task_safely(links[index_links], wait_retry = 1)
    
    # INITIAL WAIT FOR PAGE TO LOAD
    if (DEBUG == TRUE) withr::with_options(list(crayon.enabled = FALSE), cat(crayon::bgGreen(paste0("[[START OF EXPERIMENT]] ", Sys.time(), " Waiting ", initial_wait, "s")), crayon::yellow("[If it fails, increase initial_wait or wait_retry]\n")))
    Sys.sleep(initial_wait)
    
    if (length(LAUNCH_TASK$error) > 0) cat(crayon::bgRed(" ERROR: launching task [launch_task()] \n"), crayon::black(LAUNCH_TASK$error))
    
    
    # Loop through items of a task --------------------------------------------
      
      # Condition to stop while
      continue = TRUE
      index = 1
      console_logs_list = list()
      
      while (continue) {
        
        # If there is an alert, accept
        check_accept_alert(wait_retry)
        
        
        if (!exists("index")) index = 1
        if (screenshot == TRUE) remDr$screenshot(file = paste0("outputs/screenshots/", uid, "_screenshot_", sprintf("%03d", index), "_", as.Date(Sys.Date(), format = "%Y-%m-%d"), ".png"))
        
        if (console_logs == TRUE) console_logs_list[[index]] = remDr$log(type = "browser")
        
        ## Get elements of website ----------------------------
        
          list_get_elements = get_elements_safely(remDr = remDr, index = index, try_number = 1, DEBUG = DEBUG)
          
          # If we don't get any elements on out first try, wait wait_retry and try again (important when loading images, htmls, etc.)
          if (!is.null(list_get_elements$error)) {
              Sys.sleep(wait_retry)
              # Make sure there are no alerts before retrying
              check_accept_alert(wait_retry)
              list_get_elements = get_elements_safely(remDr = remDr, index = index, try_number = 2, DEBUG = DEBUG)
          }
            
          # When there is an error, usually we will have some content here (we "cause" the error with a stop())
          list_get_elements = list_get_elements$result
    
          
        # Interact with the elements we found ------------------
        if (list_get_elements$continue == TRUE) interact_with_element_safely(list_get_elements, DEBUG = DEBUG, index = index) #interact_with_element

        # FORCED WAIT ---
          if (forced_random_wait == TRUE) {
            if (index == 5) {
              #set.seed(index_links)
              set.seed(uid)
              time_wait = sample(c(1, 10, 20, 30, 30), 1)
              cat("[MONKEY]", paste0("[", index, "]"), "uid", uid,"waiting", time_wait, "seconds... \n")
              Sys.sleep(time_wait)
            }
          }
          
          
        # Output of while
        continue = list_get_elements$continue
        index = index + 1
        
      }
      ## END of while task items
      
      # Store console logs of browser
      if (console_logs == TRUE) {
        # Store browser console logs
        numbered_console_logs = console_logs_list %>% setNames(seq_along(.))  %>% .[lengths(.) != 0]
        DF_console_logs = console_logs_list %>% bind_rows() %>% mutate(page_number = names(numbered_console_logs))
        write_csv(DF_console_logs, paste0("outputs/log/", uid, "_console_logs", "_", Sys.time(), ".csv"))}
      
      # links while loop
      index_links = index_links + 1
      
      # Exit condition for links while loop
      if (is.na(links[index_links])) continue_links = FALSE
  
  }
  ## END of while links
  
  
  # END LOG -----------------------------------------------------------------

    # Restore output to console
    if (parameters_task$debug$debug_file == TRUE) {
      sink() 
      sink(type = "message")
    }

  return(container_name)

}
