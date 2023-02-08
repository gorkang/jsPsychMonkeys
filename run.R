# ADMIN -------------------------------------------------------------------

# See jsPsychHelper/admin/000_CHECK_canonical.R
# Check trialids, repeated names, etc.


# SETUP -------------------------------------------------------------------

  # First time
  source("setup.R")
  # If it fails, open _targets_packages.R, install all the packages there and rerun setup.R

  # Install docker
  # https://gorkang.github.io/jsPsychR-manual/qmd/06-jsPsychRadmins.html#install-docker
  
    # On Ubuntu, install dependencies:
      # system("sudo apt install docker.io libssl-dev libcurl4-openssl-dev libxml2-dev")

    # On windows:
      # Install docker desktop
      # Update wsl (in a command prompt): wsl - update

  # Install VNC viewer
  # https://www.realvnc.com/download/file/viewer.files/VNC-Viewer-6.21.406-Linux-x64.deb

  # Make sure docker works:
    # [In terminal]:
      # sudo usermod -aG docker ${USER}
      # sudo chmod 666 /var/run/docker.sock
      # sudo systemctl restart docker
    ## logout and login
      # docker run hello-world



# SET PARAMETERS ----------------------------------------------------------

  # Define protocol parameters in _targets.R
    rstudioapi::navigateToFile("_targets.R")
    # IMPORTANT: `local_folder_tasks` # WHERE is the local protocol?

    
    # The folder .vault needs two files:
      # + SERVER_PATH.R
      #     server_path = "http://YOUR SERVER DOMAIN/SUBFOLDER/WHERE/PROTOCOLS/ARE/"
      # + .credentials file
          # list(user = "SERVER_USERNAME",
          #  password = "SERVER_PASSWORD")
    
    
    


# Launch  -----------------------------------------------------------------

  targets::tar_watch(seconds = 5, outdated = FALSE, targets_only = TRUE) #, exclude = "parameters_monkeys"
  
  system('docker stop $(docker ps -q)') # KILL all docker instances
  targets::tar_destroy(ask = FALSE)
  targets::tar_make()
  

# Parallel ----------------------------------------------------------------

  # If you want monkeys to work in parallel, use this
  
  targets::tar_watch(seconds = 5, outdated = FALSE, targets_only = TRUE, exclude = "parameters_monkeys")
  
  system('docker stop $(docker ps -q)') # KILL all docker instances
  targets::tar_destroy(ask = FALSE)
  # targets::tar_make_future(workers = 4) # When testing the discarding / assigning / re-assigning process
  targets::tar_make_future(workers = future::availableCores() - 2)
  
