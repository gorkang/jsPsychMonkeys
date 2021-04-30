# jsPsychMonkeys <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->


Release monkeys to a jsPsych experiment using the R package [{targets}](https://github.com/wlandau/targets), docker and [{RSelenium}](https://github.com/ropensci/RSelenium).  

The goal of this package is to simplify testing a jsPysch experiment, generating synthetic responses. With its sister package [jsPsychHelpeR](https://github.com/gorkang/jsPsychHelpeR) you can process the synthetic responses to help catch issues before any actual humans complete the experiment.  

jsPsychMonkeys can run local or online experiments. You can launch as many participants as you want (laws of Physics and time constraints for virtual Monkey's labour still apply), including a parallel horde (your number of CPU cores are the limit). You can also ask the monkeys to take a screenshot of every screen they see, etc.

