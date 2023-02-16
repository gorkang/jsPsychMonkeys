# Creates the URL link for the monkey to complete a protocol
#'
#' @param parameters_monkeys 
#'
#' @param uid 
#' @param container_name 
#' @param remDr 
#'
#' @return
#' @export
#'
#' @examples
create_links <-
  function(parameters_monkeys,
           uid,
           remoteDriver = NULL) {
    
    
    # DEBUG
    # targets::tar_load_globals()
    # debug_function("create_links")
    # debug_docker(uid_participant = uid)
    
    
    # Check which parameters were entered in parameters_monkeys -----------------
    
      # If the parameter was entered in the parameters_monkeys list, use it
      source("R/main_parameters.R", local = TRUE)
    
      
    container_name = remoteDriver$container_name
    remDr = remoteDriver$remDr
    
    
    # CHECKS -----------------------------------------------------------------
    
    # If Docker container does not exist, stop execution.
    if (length(reconnect_to_VNC(container_name = container_name, just_check = TRUE)) == 0) {
      cat(crayon::bgRed("NO DOCKER IMAGE available.\n"), crayon::silver("Maybe need to do:\n targets::tar_destroy() & targets::tar_make()\n\n"))
      targets::tar_invalidate(paste0("container_", parameters_monkeys$participants$uid))
      cat(crayon::bgYellow("Invalidated ", container_name, " to restart process\n\n"))
      stop()
    }
    
    
    # Critical variables exist?
    if (!exists("uid")) uid = 0
    
    
    
    # Maybe necessary (?)
    # remDr <<- remDr
    
    

    # Create link -------------------------------------------------------------
    
    if (uid_URL == FALSE) {
      uid_string = ""
    } else {
      uid_string = paste0("&uid=", uid)
    }
    
    if (parameters_monkeys$task$local_or_server == "server") {
      
      source(".vault/SERVER_PATH.R") # server:path
      parameters_monkeys$task$server_path = server_path
      links_tasks = paste0(parameters_monkeys$task$server_path, parameters_monkeys$task$server_folder_tasks, "/index.html?pid=", parameters_monkeys$task$pid,uid_string)
      
    } else if (parameters_monkeys$task$local_or_server == "test") {
      
      path_tests = gsub("tests/jspsych-6_3_1/", "", dir(path = "tests/jspsych-6_3_1/examples", pattern = "jspsych.*.html", full.names = TRUE))
      # links_tasks = paste0("file:///home/seluser/Downloads/", path_tests[1], "?uid=", uid, "&pid=", parameters_monkeys$task$pid)
      links_tasks = paste0("file:///home/seluser/", path_tests[1], "?uid=", uid, "&pid=", parameters_monkeys$task$pid)
      
      
    # LOCAL
    } else {
      
      # If folder NOT in Downloads, make a copy (Selenium can only access ~/Downloads)
      if (!grepl("Downloads", parameters_monkeys$task$local_folder_tasks)) {
        source_folder = parameters_monkeys$task$local_folder_tasks
        destination_folder = paste0("~/Downloads/JSPSYCH/")
        
        # CHECK: if can't find given folder, try as if it is an absolute path
        if (length(dir(source_folder)) == 0) source_folder = paste0("/", source_folder)
        
        final_complete_folder = paste0(gsub("~/", "", destination_folder), basename(source_folder), "/")
        cat("", 
            crayon::yellow("Folder does not exist or is not accesible:"), source_folder, "\n",
            crayon::green("Copying to:"), final_complete_folder, "\n")
        if (!dir.exists(destination_folder)) dir.create(destination_folder)
        file.copy(source_folder, destination_folder, recursive=TRUE, copy.mode = TRUE)
        
        # Change local_folder_tasks parameter to accessible folder
        parameters_monkeys$task$local_folder_tasks = final_complete_folder
      }
      
      # Get folder inside downloads
      post_downloads_folder = gsub(".*Downloads(.*)", "\\1", parameters_monkeys$task$local_folder_tasks)
      # By default, use local
      # OLD: links_tasks = paste0("file:///home/seluser/", parameters_monkeys$task$local_folder_tasks, "/index.html?pid=", parameters_monkeys$task$pid, uid_string)
      
      # Working
      links_tasks = paste0("file:///home/seluser/Downloads/", post_downloads_folder, "/index.html?pid=", parameters_monkeys$task$pid, uid_string)
      
      # Trying to implement (see create_docker(), and also create_remDr())
      # links_tasks = paste0("file:///home/seluser/", post_downloads_folder, "/index.html?pid=", parameters_monkeys$task$pid, uid_string)
      
      # paste0(gsub("~/", "", destination_folder), basename(source_folder), "/")
    }
    
    
    # START LOG ----------------------------------------------------------------
    
    # Create log for each worker
    if (parameters_monkeys$debug$debug_file == TRUE) {
      con <- file(paste0("outputs/log/links_pid_", gsub("/", "_", parameters_monkeys$task$pid), "_uid_", uid, ".log"))
      sink(con, append = TRUE)
      sink(con, append = TRUE, type = "message")
    }    
    
    
    # Message -----------------------------------------------------------------
    
    if (DEBUG == TRUE) withr::with_options(list(crayon.enabled = FALSE), cat(crayon::underline(crayon::green(uid, "- ", links_tasks), "\n")))
    
    
    # END LOG -----------------------------------------------------------------
    
    # Restore output to console
    if (parameters_monkeys$debug$debug_file == TRUE) {
      sink() 
      sink(type = "message")
    }
    
    
    # OUTPUT ------------------------------------------------------------------
    
    output_list = list(
      # remDr = remDr,
      # container_name = container_name,
      links = links_tasks)
    
    return(output_list)
    
  }
