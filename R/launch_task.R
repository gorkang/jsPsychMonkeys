# Function to complete a task
launch_task <-
  function(parameters_task,
           uid,
           links_tasks = NULL,
           initial_wait = 2,
           # wait_retry = 2,
           # screenshot = FALSE,
           DEBUG = FALSE,
           open_VNC = FALSE,
           container_name = NULL,
           remDr = NULL) {
    
    
    # DEBUG
    # debug_function(complete_task_simple) # TODO: Make it work with lists
    # reconnect_to_VNC("container24000", DEBUG = TRUE)
    
    # targets::tar_load(links_24000)
    # parameters_task = parameters_monkeys
    # uid = 24000
    # links_tasks = links_24000$links
    # initial_wait = 2
    # 
    # DEBUG = TRUE
    # open_VNC = TRUE
    # container_name = links_24000$container_name
    # remDr = links_24000$remDr

    
    
    
    # remDr = launch_24000_dea570ab$remDr
    # container_name = launch_24000_dea570ab$container_name
    
    # parameters_task = parameters_monkeys
    # uid = 24000
    # initial_wait = parameters_monkeys$task_params$initial_wait
    # wait_retry = parameters_monkeys$task_params$wait_retry
    # screenshot = parameters_monkeys$debug$screenshot
    # DEBUG = parameters_monkeys$debug$DEBUG
    # open_VNC = parameters_monkeys$debug$open_VNC
    # tar_load(paste0("remoteDriver_", uid))
    # container_name = get(paste0("remoteDriver_", uid))$container_name
    # remDr = get(paste0("remoteDriver_", uid))$remDr
    
    # open_VNC = FALSE
    # initial_wait = 0
    # wait_retry = 2
    # screenshot = FALSE
    # DEBUG = TRUE
    # debug_docker(24000)
    
    
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
    
    # If fails, tries again, waiting a bit longer
    launch_task <- function(links_tasks, wait_retry) {
      Sys.sleep(wait_retry)
      if (DEBUG == TRUE) cat(crayon::yellow("Opening link:", links_tasks, "\n"))
      remDr$navigate(links_tasks)
    }
    
    launch_task_safely = safely(launch_task)

    
    # Create link -------------------------------------------------------------
    
    # if (parameters_task$task$local_or_server == "server") {
    #   
    #   source(".vault/SERVER_PATH.R") # server:path
    #   parameters_task$task$server_path = server_path
    #   links_tasks = paste0(parameters_task$task$server_path, parameters_task$task$server_folder_tasks, "/?uid=", uid, "&pid=", parameters_task$task$pid)
    #   
    # } else if (parameters_task$task$local_or_server == "test") {
    #   
    #   path_tests = gsub("tests/jspsych-6_3_1/", "", dir(path = "tests/jspsych-6_3_1/examples", pattern = "jspsych.*.html", full.names = TRUE))
    #   links_tasks = paste0("file:///home/seluser/Downloads/", path_tests[1], "?uid=", uid, "&pid=", parameters_task$task$pid)
    #   
    # } else {
    #   # By default, use local
    #   links_tasks = paste0("file:///home/seluser/", parameters_task$task$local_folder_tasks, "/index.html?uid=", uid, "&pid=", parameters_task$task$pid)
    #   
    # }
    # 
    
    # START LOG ----------------------------------------------------------------
    
    # Create log for each worker
    if (parameters_task$debug$debug_file == TRUE) {
      con <- file(paste0("outputs/log/launch_pid_", gsub("/", "_", parameters_task$task$pid), "_uid_", uid, ".log"))
      sink(con, append = TRUE)
      sink(con, append = TRUE, type = "message")
    }    
    
    
    # Message -----------------------------------------------------------------
    
    if (DEBUG == TRUE) cat(crayon::underline(crayon::green(uid, "- ", links_tasks), "\n"))
    
    
    # Go to task --------------------------------------------------------------
    
    LAUNCH_TASK = launch_task_safely(links_tasks, wait_retry = 1)
    
    # INITIAL WAIT FOR PAGE TO LOAD
    if (DEBUG == TRUE) cat(crayon::bgGreen(paste0("\n  START OF EXPERIMENT. waiting ", initial_wait, "s")), crayon::yellow("[If it fails, increase wait]  \n"))
    Sys.sleep(initial_wait)
    
    if (length(LAUNCH_TASK$error) > 0) cat(crayon::bgRed(" ERROR: launching task [launch_task()] \n"))
    
    
    
    # END LOG -----------------------------------------------------------------
    
    # Restore output to console
    if (parameters_task$debug$debug_file == TRUE) {
      sink() 
      sink(type = "message")
    }
    
    

  # OUTPUT ------------------------------------------------------------------

    
    output_list = list(remDr = remDr,
                       container_name = container_name)
    
    return(output_list)
    
  }
