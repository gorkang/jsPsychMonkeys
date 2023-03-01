test_that("local and server protocol runs", {

  # Clean up containers
  active_containers = system('docker ps -q', intern = TRUE)
  active_containers |> purrr::walk(~system(paste0('docker stop ', .x)))
  system("docker system prune -f") # Cleans up system (stopped containers, etc.)




  # Simple protocol ---------------------------------------------------------

  temp_folder = tempdir(check = TRUE)
  FOLDER = paste0(temp_folder, "/test_protocol999")
  write_permission = file.access(FOLDER, 2) == TRUE


  # Create simple protocol
  jsPsychMaker::create_protocol(canonical_tasks = "AIM", folder_output = FOLDER, launch_browser = FALSE)

  # Give permissions
  if (!write_permission) system(paste0("chmod 777 ", temp_folder, " * -R"))


  # Launch monkey
  OUTPUT_simple = jsPsychMonkeys::release_the_monkeys(uid = "1", local_folder_tasks = FOLDER, clean_up_targets = TRUE,
                                      open_VNC = FALSE, DEBUG = FALSE, keep_alive = FALSE)


  testthat::expect_equal(object = OUTPUT_simple$message_out, expected = "The Monkeys completed 3 tasks.")


  # Simple protocol parallel -----------------------------------------------

  OUTPUT_simple_parallel = jsPsychMonkeys::release_the_monkeys(uid = "2:3", local_folder_tasks = FOLDER, clean_up_targets = TRUE,
                                               sequential_parallel = "parallel", # number_of_cores = 2,
                                               open_VNC = FALSE, DEBUG = FALSE, keep_alive = FALSE)


  testthat::expect_equal(object = OUTPUT_simple_parallel$message_out, expected = "The Monkeys completed 6 tasks.")



  # Online protocol ---------------------------------------------------------

  # "test/protocols_DEV/test9999" is a simple protocol with AIM
  uid_random = round(runif(1, 1, 10000), 0)
  OUTPUT_simple_online = jsPsychMonkeys::release_the_monkeys(uid = uid_random, open_VNC = FALSE,
                                                server_folder_tasks = "test/protocols_DEV/test9999",
                                                clean_up_targets = TRUE,
                                                credentials_folder = "~/gorkang@gmail.com/RESEARCH/PROYECTOS-Code/jsPsychR/jsPsychMonkeys/.vault/")


  testthat::expect_equal(object = OUTPUT_simple_online$message_out, expected = "The Monkeys completed 3 tasks.")


})
