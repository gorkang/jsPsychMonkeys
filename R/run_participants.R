#' Run participants
#'
#' @param starting_worker_id
#' @param number_workers
#' @param cpus_to_use
#' @param project_id
#' @param DEBUG TRUE/FALSE
#' @param local_or_server c("local", "server)
#' @param method c("single", "multiple", "parallel")
#' @param local_folder_tasks
#' @param task_number
#' @param server_path
#'
#' @return
#' @export
#'
#' @examples
run_participants <- function(starting_worker_id = 1,
                             number_workers = 1,
                             cpus_to_use = 2,
                             project_id = 9999,
                             big_container = FALSE,
                             DEBUG = TRUE,
                             debug_file = FALSE,
                             open_VNC = FALSE,
                             type_protocol = "single_link",
                             local_or_server = "server",
                             method = "single", # "multiple" "parallel"
                             local_folder_tasks = "Downloads/TASKS",
                             task_number = 1,
                             server_path) {


  
  cat("\n", crayon::green("Launching ", number_workers, "workers in project ", project_id, " in ", local_or_server, " using the ", method, "method. Starting from", starting_worker_id, ". We will use ", cpus_to_use, " cpu/processes if in parallel."), "\n")
  

  # Libraries ---------------------------------------------------------------

  suppressMessages(suppressWarnings(library(dplyr)))
  suppressMessages(suppressWarnings(library(purrr)))
  suppressMessages(suppressWarnings(library(furrr)))
  suppressMessages(suppressWarnings(library(RSelenium)))
  suppressMessages(suppressWarnings(library(XML)))

  source("R/start_docker.R")
  source("R/helper_functions.R")
  source("R/complete_task.R")



  # Checks ------------------------------------------------------------------
  if (!exists("type_protocol")) type_protocol = ""



  # Tasks -------------------------------------------------------------------

  if (type_protocol != "single_link") {
    # Get tasks from a specific local folder
    list_tasks = gsub("/experiment.js", "", list.files(path = paste0("~/",local_folder_tasks), pattern = "experiment.js", full.names = FALSE, recursive = TRUE))
  }


  # Run all tasks in parallel -----------------------------------------------

  if (method == "parallel") {

    future::plan(multisession, workers = cpus_to_use)
    # furrr::furrr_options(scheduling = 10) # scheduling: This argument controls the average number of futures ("chunks") per worker.

    # Launch all workers (one docker per worker_id)
    starting_worker_id:(number_workers + (starting_worker_id - 1)) %>%

      # furrr::future_map(~ {
      furrr::future_walk(~ {
        
        # Create log for each worker
        if (debug_file == TRUE) {
          con <- file(paste0("log/pid_", project_id, "_uid_", .x, ".log"))
          sink(con, append=TRUE)
          sink(con, append=TRUE, type="message")
        }

        if (DEBUG == TRUE) system(paste0('echo "\nuid ', .x, ' INITIATING..."'))
        
        # Launch container
        container_name = paste0("test", .x)
        if (DEBUG == TRUE) system(paste0('echo "\nuid ', .x, ' STARTING DOCKER"'))
        
        
        # Launch locally or on server
        if (local_or_server == "server") {
          
          if (type_protocol == "single_link") {
            # Single link
            links_tasks = paste0(server_path, project_id, "/?uid=", .x, "&pid=", project_id)
          } else {
            # Multiple links
            links_tasks = paste0(server_path, project_id, "/", list_tasks, "?uid=", .x, "&pid=", project_id)
          }
          
          # If fails, tries again. folder_downloads = "" so we use the -v /dev/shm:/dev/shm option in start_docker
          tryCatch(start_docker(container_name = container_name, image_browser = "chrome", open_VNC = open_VNC, DEBUG = DEBUG, big_container = FALSE, folder_downloads = ""),
                   error = function(e) e,
                   finally = start_docker(container_name = container_name, image_browser = "chrome", open_VNC = open_VNC, DEBUG = DEBUG, big_container = FALSE, folder_downloads = ""))
          
          
        } else {
          links_tasks = paste0("file:///home/seluser/",local_folder_tasks, "/", list_tasks, "/index.html")
          
          # If fails, tries again
          tryCatch(start_docker(container_name = container_name, image_browser = "chrome", open_VNC = open_VNC, DEBUG = DEBUG, big_container = FALSE, folder_downloads = "~/Downloads"),
                   error = function(e) e,
                   finally = start_docker(container_name = container_name, image_browser = "chrome", open_VNC = open_VNC, DEBUG = DEBUG, big_container = FALSE, folder_downloads = "~/Downloads"))
          
        }
   
        
        # Complete task
        if (DEBUG == TRUE) system(paste0('echo "\nuid ', .x, ' STARTING TASK"'))
        Sys.sleep(10)
        complete_task(links_tasks, .x, DEBUG = DEBUG)
        if (DEBUG == TRUE) system(paste0('echo "\nuid ', .x, ' FINISHED TASK"'))

        # When a participant finishes, stop container
        Sys.sleep(10)
        remDr$close()
        system(paste0('docker stop "', container_name, '"'))
        
        if (DEBUG == TRUE) system(paste0('echo "\nuid ', .x, ' FINISHED ALL"'))
        
        
        # Restore output to console
        if (debug_file == TRUE) {
          sink() 
          sink(type="message")
        }
    })
    

  # Run 1 worker --------------------------------------------------------------    
        
  } else if (method == "single") {

    system(paste0('echo "\nuid ', starting_worker_id, ' LAUNCHED"'))
    
    # Create log for each worker
    if (debug_file == TRUE) {
      con <- file(paste0("log/test_id", starting_worker_id, ".log"))
      sink(con, append=TRUE)
      sink(con, append=TRUE, type="message")
    }
                
    # Launch locally or on server
    if (local_or_server == "server") {
      if (type_protocol == "single_link") {
        # Single link
        links_tasks = paste0(server_path, project_id, "/?uid=", starting_worker_id, "&pid=", project_id)
      } else {
        # Multiple links
        links_tasks = paste0(server_path, project_id, "/", list_tasks, "?uid=", starting_worker_id, "&pid=", project_id)
      }
      
    } else {
      links_tasks = paste0("file:///home/seluser/",local_folder_tasks, "/", list_tasks, "/index.html")
    }


    # Launch container
    # restart = FALSE, 
    container_name = paste0("test", starting_worker_id)
    if (DEBUG == TRUE) message("\nNAME: ", container_name, "")
    # suppressPackageStartupMessages(
      start_docker(container_name = container_name, image_browser = "chrome", open_VNC = open_VNC, DEBUG = DEBUG, folder_downloads = "~/Downloads")
    # )

    # Complete task
    complete_task(links_tasks[task_number], i = starting_worker_id, DEBUG = DEBUG)

    # When a participant finishes, stop container
    # Sys.sleep(10)
    # remDr$close()
    # system(paste0('docker stop "', container_name, '"'))
    
    # Restore output to console
    if (debug_file == TRUE) {
      sink() 
      sink(type="message")
    }
    

        
  # Run all tasks serially --------------------------------------------------

  } else if (method == "multiple") {

    system(paste0('echo "\nuid ', starting_worker_id, ' LAUNCHED"'))
    
    # Create log for each worker
    if (debug_file == TRUE) {
      con <- file(paste0("log/test_id", starting_worker_id, ".log"))
      sink(con, append=TRUE)
      sink(con, append=TRUE, type="message")
    }
    
    
    # Launch locally or on server
    if (local_or_server == "server") {
      if (type_protocol == "single_link") {
        # Single link
        links_tasks = paste0(server_path, project_id, "/?uid=", paste0(starting_worker_id:(number_workers + starting_worker_id)), "&pid=", project_id)
      } else {
        # Multiple links
        links_tasks = paste0(server_path, project_id, "/", paste0(list_tasks), "?uid=", paste0(starting_worker_id:(number_workers + starting_worker_id)), "&pid=", project_id)
        
      }
      
    } else {
      links_tasks = paste0("file:///home/seluser/", local_folder_tasks, "/", list_tasks, "/index.html")
    }

    # Launch container
    container_name = paste0("test", 1)
    if (DEBUG == TRUE) message("\nNAME: ", container_name, "")
    # suppressPackageStartupMessages(
      start_docker(container_name = container_name, image_browser = "chrome", open_VNC = open_VNC, DEBUG = DEBUG, folder_downloads = "~/Downloads")
    # )

    Sys.sleep(10)
      
    # Launch task!
    if (DEBUG == TRUE) tictoc::tic()
    1:length(links_tasks) %>% map(~ complete_task(links_tasks[.x], .x, DEBUG = DEBUG))
    if (DEBUG == TRUE) tictoc::toc()      

    Sys.sleep(10)
    
    # When finished, stop container
    remDr$close()
    system(paste0('docker stop "', container_name, '"'))
    
    
    # Restore output to console
    if (debug_file == TRUE) {
      sink() 
      sink(type="message")
    }
    
  }

}
  
