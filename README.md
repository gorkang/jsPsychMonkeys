# jsPsychMonkeys <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->


Release monkeys to a jsPsych experiment using the R package [{targets}](https://github.com/wlandau/targets), docker and [{RSelenium}](https://github.com/ropensci/RSelenium).  

The goal of this package is to simplify testing a jsPysch experiment, generating synthetic responses. With its sister package [jsPsychHelpeR](https://github.com/gorkang/jsPsychHelpeR) you can process the synthetic responses to help catch issues before any actual humans complete the experiment.  

jsPsychMonkeys can run local or online experiments. You can launch as many participants as you want (laws of Physics and time constraints for virtual Monkey's labour still apply), including a parallel horde (your number of CPU cores are the limit). You can also ask the monkeys to take a screenshot of every screen they see, etc.


## How to use this

### 1) Download the jsMonkeys project

To download this project to your computer:  

```
if (!require('usethis')) install.packages('usethis'); library('usethis')
usethis::use_course("https://github.com/gorkang/jsPsychMonkeys")
```

### 2) Setup

Install the dependencies (see packages used in `_targets_packages.R`) and make sure you have all the necessary folders:  

`source("setup.R")`  


### 3) Parameters

Edit the `Parameters` section of the `_targets.R` file. The minimal set of parameters needed are:  

```
parameters_monkeys_minimal = list(
  uid = 1:10, # User id's for the participants. 
  local_folder_tasks = "Downloads/tests/2" # Where is your jsPsych protocol located
)
````

### 4) Release the Monkeys! 

If you want a sequential process: 

- `targets::tar_make()`  

If you want a parallel horde of monkeys: 

- `targets::tar_future_make(workers = 2)`

You can set as many parallel workers as you want. With `targets::tar_make_future(workers = future::availableCores() - 1)` you can have as many workers as your computer cores minus 1.

In `run.R` you can see the typical set of commands needed for a variety of situations.  

---  

10 Monkeys completing a protocol in parallel. You can use `targets::tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)` to see the live progress:  


![](man/figures/jsMonkeys_parallel.gif)
