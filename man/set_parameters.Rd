% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_parameters.R
\name{set_parameters}
\alias{set_parameters}
\title{Process the parameters_monkeys_minimal parameters so a minimal set of input parameters can be used. Also, tries to take into account dependencies between parameters.}
\usage{
set_parameters(
  parameters_monkeys_minimal = parameters_monkeys_minimal,
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
  forced_refresh = FALSE,
  forced_seed = 11
)
}
\arguments{
\item{parameters_monkeys_minimal}{A list with the main input parameters. See parameters_monkeys_minimal in _targets.R.
If the parameter is entered in this list, use it. You can also enter the parameter directly}

\item{credentials_folder}{folder where files "SERVER_PATH.R" and ".credentials" are. Usually .vault/}

\item{uid}{user id's for the monkeys. Can be a single number (e.g. 55) or a numeric vector (e.g. 1:10). Determines the number of monkeys that will be released}

\item{times_repeat_protocol}{if different than 1, creates an URL parameter ID and multiple links changing the uid}

\item{time_to_sleep_before_repeating_protocol}{In seconds, how long to sleep before repeating protocol}

\item{browserName}{In which browser should the monkey run the protocol? c("chrome", "firefox")}

\item{big_container}{Big containers are needed for big protocols: FALSE / TRUE}

\item{keep_alive}{Keep the docker container alive after finishing the protocol?
This is useful to debug.}

\item{folder_downloads}{Local folder where csv's will be downloaded:
\itemize{
\item On linux, can be something like ~/Download
\item On windows, can be something like: C:/Users/myusername/Downloads/protocol999
If Monkeys are running OK but no csv's are downloaded, make sure the docker username has write access to the folder
}}

\item{DEBUG}{Show debug messages: FALSE / TRUE}

\item{screenshot}{Should the monkey's take screenshots of each screen they see?
\itemize{
\item The images will be in outputs/screenshots
}}

\item{debug_file}{Store the debug info in a file in outputs/debug}

\item{console_logs}{Store console logs of browser? Logs are stored in outputs/logs}

\item{open_VNC}{Show the info to open VNC viewer to see what the monkey's are up to}

\item{pid}{Protocol id}

\item{uid_URL}{Include user id in the protocol URL? If true, the uid are predefined}

\item{local_or_server}{Run the protocol locally or on server c("local", "server)
This variable will be inferred from whichever of local_folder_tasks or server_folder_tasks
has information.
See https://gorkang.github.io/jsPsychR-manual/qmd/04-jsPsychMonkeys.html#launch-monkeys-on-a-server for more information}

\item{local_folder_tasks}{Local folder where the protocol is stored}

\item{server_folder_tasks}{Location of the protocol in the server}

\item{disable_web_security}{Run with CORS disabled? Needed for local protocols that load videos: FALSE / TRUE}

\item{initial_wait}{Initial wait (in seconds) after entering the main protol page, before
the monkeys start to complete the protocol. If the protocol has lots of images to pre-load
should be a number big enough for them to download.}

\item{wait_retry}{How many seconds to wait before retrying, after no elements are found in a page.}

\item{forced_random_wait}{At some random point in the protocol, the monkey should wait for a random period of time? FALSE / TRUE}

\item{forced_refresh}{Force a full refresh for some of the monkeys?
This is useful to test if the monkey's can continue a protocol after they exit.}

\item{forced_seed}{Random seed}
}
\value{
a list of parameters for the rest of the jsPsychMonkeys functions
}
\description{
Process the parameters_monkeys_minimal parameters so a minimal set of input parameters can be used. Also, tries to take into account dependencies between parameters.
}
\examples{
try(set_parameters(parameters_monkeys = list(uid = 888, uid_URL = TRUE, forced_seed = 11,
                                       local_folder_tasks = "~/Downloads/protocol999",
                                       screenshot = FALSE, forced_refresh = FALSE,
                                       debug = TRUE, open_VNC = TRUE)))
}
