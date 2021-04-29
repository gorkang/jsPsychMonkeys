# Function to complete a task
create_links <-
  function(parameters_task,
           uid,
           # initial_wait = 2,
           # wait_retry = 2,
           # screenshot = FALSE,
           DEBUG = FALSE,
           # open_VNC = FALSE,
           container_name = NULL,
           remDr = NULL) {
    
    
    
    
    # CHECKS -----------------------------------------------------------------
    
    # If Docker container does not exist, stop execution.
    if (length(reconnect_to_VNC(container_name = container_name, just_check = TRUE)) == 0) {
      cat(crayon::bgRed("NO DOCKER IMAGE available.\n"), crayon::silver("Maybe need to do:\n targets::tar_destroy() & targets::tar_make()\n\n"))
      targets::tar_invalidate(paste0("container_", parameters_task$participants$uid))
      cat(crayon::bgYellow("Invalidated ", container_name, " to restart process\n\n"))
      stop()
    }
    
    
    # Critical variables exist?
    if (!exists("uid")) uid = 0
    
    
    
    # Maybe necessary (?)
    remDr <<- remDr
    
    

    # Create link -------------------------------------------------------------
    
    if (parameters_task$task$local_or_server == "server") {
      
      source(".vault/SERVER_PATH.R") # server:path
      parameters_task$task$server_path = server_path
      links_tasks = paste0(parameters_task$task$server_path, parameters_task$task$server_folder_tasks, "/?uid=", uid, "&pid=", parameters_task$task$pid)
      
    } else if (parameters_task$task$local_or_server == "test") {
      
      path_tests = gsub("tests/jspsych-6_3_1/", "", dir(path = "tests/jspsych-6_3_1/examples", pattern = "jspsych.*.html", full.names = TRUE))
      links_tasks = paste0("file:///home/seluser/Downloads/", path_tests[1], "?uid=", uid, "&pid=", parameters_task$task$pid)
      
    } else {
      # By default, use local
      random_uid = round(runif(n = 1, min = 1, max = 25000), 0)
      
      links_tasks = paste0("file:///home/seluser/", parameters_task$task$local_folder_tasks, "/index.html?uid=", random_uid, "&pid=", parameters_task$task$pid)
      # links_tasks = paste0("file:///home/seluser/", parameters_task$task$local_folder_tasks, "/index.html?uid=", uid, "&pid=", parameters_task$task$pid)
      
    }
    
    
    # START LOG ----------------------------------------------------------------
    
    # Create log for each worker
    if (parameters_task$debug$debug_file == TRUE) {
      con <- file(paste0("outputs/log/links_pid_", gsub("/", "_", parameters_task$task$pid), "_uid_", uid, ".log"))
      sink(con, append = TRUE)
      sink(con, append = TRUE, type = "message")
    }    
    
    
    # Message -----------------------------------------------------------------
    
    if (DEBUG == TRUE) cat(crayon::underline(crayon::green(uid, "- ", links_tasks), "\n"))
    
    

    
    # END LOG -----------------------------------------------------------------
    
    # Restore output to console
    if (parameters_task$debug$debug_file == TRUE) {
      sink() 
      sink(type = "message")
    }
    
    
    
    # OUTPUT ------------------------------------------------------------------
    
    
    output_list = list(remDr = remDr,
                       container_name = container_name,
                       links = links_tasks)
    
    return(output_list)
    
  }
