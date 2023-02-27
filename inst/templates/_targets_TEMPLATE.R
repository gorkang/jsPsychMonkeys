# [EDIT ONLY THIS SECTION] ---------------------------------------------------

  # Parameters ---------------------------------------------------------------
  
    # For a complete list of possible parameters, see set_parameters()  
    # Number of workers is defined in run.R: targets::tar_make_future(workers = future::availableCores() - 2)
  


#PARAMETERS_HERE

# [DO NOT EDIT BELOW] -----------------------------------------------------



# Load main targets parameters --------------------------------------------

# Source all /R files
lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source) # Including "R/_targets_parameters.R"

    
# Targets -----------------------------------------------------------------

TARGETS =  list(
   
  # 1) Prepares the parameters_monkeys list with all the necessary parameters  
  tar_target(
    parameters_monkeys,
    set_parameters(parameters_monkeys_minimal = parameters_monkeys_minimal), 
    priority = 1
  ),
  
  
  # Creates as many branches as uid's
  tarchetypes::tar_map(
    values = list(uid = parameters_monkeys_minimal$uid),
    
      
      # 2) Create docker container
      tar_target(
        container,
        create_docker(
          uid = uid,
          parameters_monkeys = parameters_monkeys
        ), priority = .5
      ),
    
      
      # 3) Open remote Driver and browser
      tar_target(
        remote_driver,
        create_remDr(
          container_port = container$container_port,
          browserName = container$browserName,
          container_name = container$container_name,
          parameters_monkeys = parameters_monkeys
        ), priority = .5
      ),
      
      
      # 4) Create links
      tar_target(
        links_tar,
        create_links(
          parameters_monkeys = parameters_monkeys,
          uid = uid,
          remote_driver = remote_driver
        ), priority = .5
      ),
      
      
      # 5) For local protocols, check the csv in downloads
      tar_target(
        existing_CSVs,
        check_Downloads(
          parameters_monkeys = parameters_monkeys,
          links_tar = remote_driver # This is so this runs before task
          ), priority = .5
      ),
      
      
      # 6) Complete task
      tar_target(
        task,
        complete_task(
          parameters_monkeys = parameters_monkeys,
          uid = uid,
          links = links_tar$links,
          remote_driver = remote_driver
        ), priority = .5
      ),
      
    
      # 7) Copy newly downloaded csv's to the protocols data folder
      tar_target(
        data_moved,
        copy_files_to_data(
          pre_existing_CSV = existing_CSVs, 
          parameters_monkeys = parameters_monkeys, 
          uid = uid,
          task = task
          ), priority = .5
        ),
    
      
      # 8) Clean up after participants finish
      tar_target(
        clean_container,
        clean_up_docker(
          container_name = task,
          parameters_monkeys = parameters_monkeys
          ), priority = .5
        )
    
    
    ) # End of tar_map()
  )


  # Change priorities  ------------------------------------------------------
    # We assign priorities so the target's progress is row-wise 
    # This way, a participant should finish before starting a new one, avoiding memory issues
    tar_make_future_rowwise(TARGETS = TARGETS, uids = parameters_monkeys_minimal$uid)

TARGETS
