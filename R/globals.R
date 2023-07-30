# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
utils::globalVariables(
  c("value", "trialid", "experiment", "keep_alive", "DEBUG", "open_VNC",
    "initial_wait", "screenshot", "console_logs", "level", "forced_seed",
    ".", "browserName", "big_container", "uid_URL", "server_path",
    "driver_name", "command", "name", "style", "tag_name", "hidden",
    "type", "required", "id", "type_extracted", "files", "content",
    "remDr", "forced_random_wait", "disable_web_security", "remoteDriver",
    "parameters_monkeys", "container_name"))
