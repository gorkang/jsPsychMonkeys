
# SETUP -------------------------------------------------------------------

  # First time
  source("setup.R") 
  # If it fails, open _targets_packages.R, install all the packages there and rerun setup.R

  # On Ubuntu, install dependencies:
  # system("sudo apt install docker.io libssl-dev libcurl4-openssl-dev libxml2-dev")

  # Install VNC viewer
  # https://www.realvnc.com/download/file/viewer.files/VNC-Viewer-6.21.406-Linux-x64.deb

  # Make sure docker works:
    # [In terminal]:
      # sudo usermod -aG docker ${USER}
      # sudo chmod 666 /var/run/docker.sock
      # sudo systemctl restart docker
    ## logout and login
      # docker run hello-world


  # targets::tar_renv()


  # Do you have Docker installed?
    # https://docs.docker.com/docker-for-windows/install/ [500 MB Download]

  # Define protocol parameters in _targets.R
    rstudioapi::navigateToFile("_targets.R")
    # IMPORTANT: `local_folder_tasks` # WHERE is the local protocol?


# Launch  -----------------------------------------------------------------

  targets::tar_watch(seconds = 5, outdated = FALSE, targets_only = TRUE) #, exclude = "parameters_monkeys"
  
  system('docker stop $(docker ps -q)') # KILL all docker instances
  targets::tar_destroy()
  targets::tar_make()


# Parallel ----------------------------------------------------------------

  targets::tar_watch(seconds = 5, outdated = FALSE, targets_only = TRUE, exclude = "parameters_monkeys")
  
  system('docker stop $(docker ps -q)') # KILL all docker instances
  targets::tar_destroy()
  targets::tar_make_future(workers = future::availableCores() - 2)

  