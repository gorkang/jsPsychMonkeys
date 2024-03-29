% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{create_monkeys_project}
\alias{create_monkeys_project}
\title{Create a jsPsychMonkeys project for your data}
\usage{
create_monkeys_project(
  folder = "~/Downloads/",
  credentials_folder = ".vault/",
  uid = 1,
  times_repeat_protocol = 1,
  time_to_sleep_before_repeating_protocol = 1,
  browserName = "chrome",
  big_container = FALSE,
  keep_alive = FALSE,
  folder_downloads = NULL,
  DEBUG = FALSE,
  screenshot = FALSE,
  debug_file = FALSE,
  console_logs = FALSE,
  open_VNC = FALSE,
  pid = NULL,
  uid_URL = TRUE,
  local_or_server = NULL,
  local_folder_tasks = NULL,
  server_folder_tasks = NULL,
  disable_web_security = FALSE,
  initial_wait = 2,
  wait_retry = 0.1,
  forced_random_wait = FALSE,
  forced_refresh = NULL,
  forced_seed = NULL,
  dont_ask = FALSE,
  open_rstudio = TRUE
)
}
\arguments{
\item{folder}{location for the project}

\item{credentials_folder}{folder where files "SERVER_PATH.R" and ".credentials" are. Usually .vault/}

\item{uid}{.}

\item{times_repeat_protocol}{if different than 1, creates an URL parameter ID and multiple links changing the uid}

\item{time_to_sleep_before_repeating_protocol}{In seconds, how long to sleep before repeating protocol}

\item{browserName}{.}

\item{big_container}{.}

\item{keep_alive}{.}

\item{folder_downloads}{.}

\item{DEBUG}{.}

\item{screenshot}{.}

\item{debug_file}{.}

\item{console_logs}{.}

\item{open_VNC}{.}

\item{pid}{project id}

\item{uid_URL}{.}

\item{local_or_server}{.}

\item{local_folder_tasks}{.}

\item{server_folder_tasks}{.}

\item{disable_web_security}{.}

\item{initial_wait}{.}

\item{wait_retry}{In seconds, how much to wait before retrying}

\item{forced_random_wait}{.}

\item{forced_refresh}{.}

\item{forced_seed}{.}

\item{dont_ask}{answer YES to all questions so the process runs uninterrupted. This will:}

\item{open_rstudio}{Open RStudio with the new project TRUE / FALSE}
}
\value{
Creates a new project
}
\description{
create_monkeys_project() will create a jsPsychMonkeys project adapted to
the parameters you entered
}
