
# Libraries ---------------------------------------------------------------

suppressMessages(suppressWarnings({
  if (!require('targets')) install.packages('targets'); library('targets')
  if (!require('tarchetypes')) install.packages('tarchetypes'); library('tarchetypes')
  if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
  if (!require("purrr")) install.packages("purrr"); library("purrr")
#   if (!require('dtplyr')) install.packages('dtplyr'); library('dtplyr')
#   if (!require('future')) install.packages('future'); library('future')
#   if (!require('future.callr')) install.packages('future.callr'); library('future.callr')
#   if (!require("glue")) install.packages("glue"); library("glue")
#   if (!require("assertthat")) install.packages("assertthat"); library("assertthat")
#   if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
#   if (!require("RSelenium")) install.packages("RSelenium", dependencies = TRUE); library("RSelenium")
}))

# List of packages to use
packages_to_load = c("callr", "dtplyr", "dplyr", "future", "future.callr", "glue", "purrr", "readr", "RSelenium", "rvest", "shinyWidgets", "targets", "tarchetypes", "tidyr","XML")

# For tar_make_future() [https://github.com/HenrikBengtsson/future/#controlling-how-futures-are-resolved]
future::plan(future.callr::callr)
future::tweak(strategy = "multisession")


# Functions ---------------------------------------------------------------

# Safely functions
# source("R/.safely_helper_functions.R")


# Maintenance -------------------------------------------------------------

# So crayon colors work when using future and when in interactive mode
Sys.setenv(R_CLI_NUM_COLORS = crayon::num_ansi_colors())


# target options (packages, errors...)
targets::tar_option_set(
  packages = packages_to_load, # Load packages for all targets
  workspace_on_error = TRUE, # to load workspace on error to debug
  memory = "transient", # Memory management
  garbage_collection = TRUE # Memory management
)

# Restore output to console (in case it was left hanging...)
suppressWarnings(sink())
sink(type = "message")
