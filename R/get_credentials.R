#' Get credentials from folder
#'
#' @param credentials_folder folder where files "SERVER_PATH.R" and ".credentials" are. Usually .vault/
#'
#' @return A list with credentials$value and server_path$value
#' @export
get_credentials <- function(credentials_folder) {

  # Get credentials
  if (!is.null(credentials_folder)) {

    FILES_temp =  c("SERVER_PATH.R", ".credentials")
    FILES_to_COPY = here::here(c(paste0(credentials_folder, "/", FILES_temp)))

    server_path = source(FILES_to_COPY[1])
    credentials = source(FILES_to_COPY[2])

    OUT = list(server_path = server_path,
         credentials = credentials)

  } else {

    cli::cli_alert_danger("No credentials found in {credentials_folder}")
    OUT = NULL
  }

  return(OUT)

}
