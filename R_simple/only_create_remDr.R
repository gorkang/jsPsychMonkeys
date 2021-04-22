only_create_remDr <- function(container_port, browserName, container_name = NULL, DEBUG = FALSE) {
  
  # DEBUG
  # container_port = 49216
  # browserName = "chrome"
  # container_name = "container2004"
  # DEBUG = TRUE
  
  # container_port = container_port[1]
  
  # browserName = "chrome"
  # container_name = "container2004"
  # DEBUG = TRUE
  # container_port = 49261
  
  # RSelenium::remoteDriver()
  

# Create browser instance. This should be reflected in the VNC session
if (DEBUG == TRUE) cat(crayon::silver("Create remoteDriver: [", container_port, "] [", browserName, "] [", container_name, "] [", "\n"))
  
create_remDr <- function(container_port, browserName, time_wait = 5, errored = FALSE) {
  if (errored == FALSE) {
    if (DEBUG == TRUE) cat(crayon::red("Creating remoteDriver in", paste0(time_wait, "s\n")))
    Sys.sleep(time_wait)
  } else if (errored == TRUE) {
    if (DEBUG == TRUE) cat(crayon::red("Error creating remoteDriver. Retrying after", paste0(time_wait, "s\n")))
        Sys.sleep(time_wait)
  }
  
  # Create remote driver
  remDr <- remoteDriver(remoteServerAddr = "localhost", port = container_port, browserName = browserName)
  return(remDr)
}
create_remDr_safely = safely(create_remDr)

CREATE_REMOTE_DRIVER = create_remDr_safely(container_port = container_port, browserName = browserName, time_wait = 5)
while (!is.null(CREATE_REMOTE_DRIVER$error)) {
  CREATE_REMOTE_DRIVER = create_remDr_safely(container_port = container_port, browserName = browserName, time_wait = 5, errored == TRUE)
}

remDr = CREATE_REMOTE_DRIVER$result


if (DEBUG == TRUE) cat(crayon::yellow("\n-Closing browsers... \n"))

Sys.sleep(5)

# Close all browser instances
# ESTO TB DA ERROR
remDr$closeall()

# Open new browser

if (DEBUG == TRUE) cat(crayon::yellow("\n-About to open a browser... \n"))
Sys.sleep(1)
# EL ERROR ES AQUI
# https://stackoverflow.com/questions/57682082/rselenium-with-docker-fails-in-rscript-but-works-in-rstudio
  # Error : Undefined error in httr call. httr output: Empty reply from server
  # Error: callr subprocess failed: Undefined error in httr call. httr output: Empty reply from server


remDr$open(silent = !DEBUG)



# only_open_remDr(remDr = remDr, DEBUG = TRUE)

return(remDr)
# list(name_driver = remDr)

}

