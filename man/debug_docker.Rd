% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{debug_docker}
\alias{debug_docker}
\title{debug_docker}
\usage{
debug_docker(uid_participant)
}
\arguments{
\item{uid_participant}{uid of participant}
}
\value{
Loads in GlobalEnv the vars necessary to debug a docker container
}
\description{
debug_docker
}
\examples{
try(debug_docker(24000))
}
