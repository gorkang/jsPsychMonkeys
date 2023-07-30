#' create_links
#'
#' @param parameters_monkeys parameters_monkeys list
#' @param uid user id
#' @param remote_driver remDr
#'
#' @return A list of links
create_links <-
  function(parameters_monkeys,
           uid,
           remote_driver = NULL) {


    # DEBUG
    # targets::tar_load_globals()
    # debug_function("create_links")
    # debug_docker(uid_participant = uid)


    # Check which parameters were entered in parameters_monkeys -----------------

      # If the parameter was entered in the parameters_monkeys list, use it
      source(here::here("R/main_parameters.R"), local = TRUE)


    container_name = remote_driver$container_name
    times_repeat_protocol = as.numeric(parameters_monkeys$links_tar$times_repeat_protocol)
    # remDr = remote_driver$remDr


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


    # Create link -------------------------------------------------------------

    if (times_repeat_protocol > 1) {

      # ID with 3 numbers and 3 letters
      ID_char = rep(paste0(sample.int(999, 1),
                           sample(LETTERS, size = 1), sample(LETTERS, size = 1), sample(LETTERS, size = 1)),
                    times_repeat_protocol)

      # Create a uid modifier so the same uid does not repeat the experiment
        # We create A1, B1,... A2, B2,... uid modifiers
        repetitions = ceiling(times_repeat_protocol/26)
        LETTERS_ALL = rep(LETTERS[1:26], repetitions)
        numeric_SUFFIX = rep(1:repetitions, each  = 26)
        uid_modifier = paste0(LETTERS_ALL, numeric_SUFFIX)[1:times_repeat_protocol]


      if (uid_URL == FALSE) {
        uid_string = paste0("", "&ID=", ID_char)
      } else {
        uid_string = paste0("&uid=", uid, uid_modifier,  "&ID=", ID_char)
      }

    } else {

      if (uid_URL == FALSE) {
        uid_string = ""
      } else {
        uid_string = paste0("&uid=", uid)
      }

      uid_modifier = ""

    }

    if (parameters_monkeys$task$local_or_server == "server") {

      credentials_location = paste0(parameters_monkeys$docker$credentials_folder, "/SERVER_PATH.R")
      source(credentials_location) # loads variable: server_path

      parameters_monkeys$task$server_path = server_path
      links_tasks = paste0(parameters_monkeys$task$server_path, parameters_monkeys$task$server_folder_tasks, "/index.html?pid=", parameters_monkeys$task$pid,uid_string)

    } else if (parameters_monkeys$task$local_or_server == "test") {

      path_tests = gsub("tests/jspsych-6_3_1/", "", dir(path = "tests/jspsych-6_3_1/examples", pattern = "jspsych.*.html", full.names = TRUE))
      # links_tasks = paste0("file:///home/seluser/Downloads/", path_tests[1], "?uid=", uid, "&pid=", parameters_monkeys$task$pid)
      links_tasks = paste0("file:///home/seluser/", path_tests[1], "?uid=", uid, "&pid=", parameters_monkeys$task$pid)


    # LOCAL
    } else {

      # TODO: should check if folder has write permissions and leave it as is if possible

      # # Check if we have write permission
      # write_permission = file.access(parameters_monkeys$task$local_folder_tasks, 2) == 0
      #
      # # Give permissions [TODO: review if this works on Windows]
      # if (!write_permission) system(paste0("chmod 777 ", parameters_monkeys$task$local_folder_tasks, " * -R"), intern = TRUE)


      # If folder NOT in Downloads, make a copy (Selenium can only access ~/Downloads)
      # if (!grepl("Downloads", parameters_monkeys$task$local_folder_tasks)) {
      #   source_folder = parameters_monkeys$task$local_folder_tasks
      #   destination_folder = paste0("~/Downloads/JSPSYCH/")
      #
      #   # CHECK: if can't find given folder, try as if it is an absolute path
      #   if (length(dir(source_folder)) == 0) source_folder = paste0("/", source_folder)
      #
      #   final_complete_folder = paste0(gsub("~/", "", destination_folder), basename(source_folder), "/")
      #
      #   cat("",
      #       crayon::yellow("Folder does not exist or is not accesible:"), source_folder, "\n",
      #       crayon::green("Copying to:"), final_complete_folder, "\n")
      #   if (!dir.exists(destination_folder)) dir.create(destination_folder)
      #   file.copy(source_folder, destination_folder, recursive=TRUE, copy.mode = TRUE)
      #
      #   # Change local_folder_tasks parameter to accessible folder
      #   # REVIEW: DOES THIS IS EXPORTED to other targets?
      #   parameters_monkeys$task$local_folder_tasks = final_complete_folder
      # }

      # Get folder inside downloads
      if (!grepl("Downloads", parameters_monkeys$task$local_folder_tasks)) {
        if (DEBUG == TRUE) cli::cli_alert_info("Folder NOT inside ~/Downloads")
        post_downloads_folder = basename(parameters_monkeys$task$local_folder_tasks)
      } else {
        post_downloads_folder = gsub(".*Downloads(.*)", "\\1", parameters_monkeys$task$local_folder_tasks)
      }


      # By default, use local
      links_tasks = paste0("file:///home/seluser/Downloads/", post_downloads_folder, "/index.html?pid=", parameters_monkeys$task$pid, uid_string)

      # Trying to implement (see create_docker(), and also create_remDr())
      # links_tasks = paste0("file:///home/seluser/", post_downloads_folder, "/index.html?pid=", parameters_monkeys$task$pid, uid_string)

      # paste0(gsub("~/", "", destination_folder), basename(source_folder), "/")
    }



    # Message -----------------------------------------------------------------

    if (DEBUG == TRUE) cli::cli_alert_info("{uid} - {.url {links_tasks}} \n")


    # OUTPUT ------------------------------------------------------------------

    output_list = list(links = links_tasks,
                       uid_modifier = uid_modifier)

    return(output_list)

  }
