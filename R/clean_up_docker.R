clean_up_docker <-
  function(container_name = "test",
           DEBUG = FALSE) {
    
    
      # Container already exists
      if (DEBUG == TRUE) cat(crayon::green("Removing container", container_name, "...\n"))
      
      # Stop and remove containers:
      # TODO: SHOULD DO THIS WHEN PARTICIPANT ENDS
      # If container is running stop it
      if (!is_empty(system(sprintf('docker ps -q -f name=%s', container_name), intern = TRUE))) {
        # Stop and remove container
        system(sprintf('docker stop %s', container_name)) # kill container_name
      }

      # If container exists remove it
      if (length(system(sprintf('docker container ls -a -f name=%s', container_name), intern = TRUE)) > 1) {
        system(sprintf('docker container rm %s', container_name))
      }
      
    }
