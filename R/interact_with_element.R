interact_with_element <- function(list_get_elements, DEBUG = FALSE, index = NULL, seed = 1) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function("complete_task")
  # debug_docker(uid_participant = uid)
  # reconnect_to_VNC(container_name = container_name)
  # DEBUG = TRUE
  # index = 1
  # seed = 1
  # container_name = remote_driver$container_name
  # remDr = remote_driver$remDr
  # list_get_elements = get_elements(remDr = remDr, DEBUG = DEBUG); list_get_elements
    # IF it comes from list_get_elements_safer list_get_elements = list_get_elements$result


  # SET SEED ----------------------------------------------------------------

    set.seed(seed)
    # cat(crayon::bgGreen("\n seed - interact: ", seed, "\n"))

  # INPUTS ------------------------------------------------------------------

    if (length(list_get_elements$name_inputs$id) > 0) {

      # source("R/helper_select_input.R")
      output_select_input = select_input(list_get_elements = list_get_elements, DEBUG = DEBUG, seed = seed)

    } else {

      output_select_input = list(selected_input = tibble(name = "NO input element found"),
                                 input_text = "''",
                                 input_text_human_readable = "''")

    }

  # The <big>TITLE</big> is lost in rvest::html_text2() in parse_elements()
  content_str =
    # substr(gsub(".*?(\\n.*$)", "\\1", list_get_elements$name_contents$content),  1, 120) %>% # Only first 120 chars
    substr(gsub("(\\.jspsych.*\\n)(.*?)(.*$)", "\\2\\3", list_get_elements$name_contents$content),  1, 120) %>% # Only first 120 chars. Do not get all the Initial .jspsych-***
    gsub("\\\n\\\n", "\\\n", .) %>% # Double \n by single \n
    gsub("\\\n", " | ", .) %>% # Use | as a mark for line jump
    gsub("^ \\| (.*)$", "\\1", .) %>%
    paste0(., "...")


  # BUTTONS -----------------------------------------------------------------

    if (length(list_get_elements$name_buttons$id) == 1) {

      selected_button_id = list_get_elements$name_buttons$id

      # Get button's value
      selected_button_value = "" # Default
      button_value_columns = c("value", "content")
      button_value_columns_n = which(button_value_columns %in% colnames(list_get_elements$name_buttons))
      if (length(button_value_columns_n) > 0) {
        temp_list = button_value_columns[button_value_columns_n] |> purrr::map(~list_get_elements$name_buttons |> pull(.x))
        selected_button_value = temp_list[temp_list != ""] |> unlist()
      }

      # Click!
      list_get_elements$list_elements[[selected_button_id]]$clickElement()

      # If button was Fullscreen, wait a couple seconds so the interface is responsive
      if (selected_button_id == "jspsych-fullscreen-btn") Sys.sleep(3)

    } else if (length(list_get_elements$name_buttons$id) > 1) {

        # IF WE ARE IN consentHTML: always start
        if (all(list_get_elements$name_buttons$id == c("start", "end"))) {
          selected_button_id = c("start")
          list_get_elements$list_elements[[selected_button_id]]$clickElement()

        # IF WE ARE IN consentJS: always start
        } else if (all(list_get_elements$name_buttons$content == c("acepto participar", "rechazo participar"))) {
          selected_button_id = c("jspsych-btn1")
          list_get_elements$list_elements[[selected_button_id]]$clickElement()

          # If we are in instructions with Back/Forward buttons, always Forward

        } else if (all(list_get_elements$name_buttons$content == c("I agree to participate", "I refuse to participate"))) {
          selected_button_id = c("jspsych-btn1")
          list_get_elements$list_elements[[selected_button_id]]$clickElement()

          # If we are in instructions with Back/Forward buttons, always Forward

        # If we are in instructions with Back/Forward buttons, always Forward
        } else if (all(list_get_elements$name_buttons$id == c("jspsych-instructions-back", "jspsych-instructions-next"))) {

          selected_button_id = c("jspsych-instructions-next")
          list_get_elements$list_elements[[selected_button_id]]$clickElement()

          # If we are in BART, make it more likely to push inflate
        } else if (all(list_get_elements$name_buttons$id == c("inflate_button", "collect_button"))) {

          selected_button_id = sample(c("inflate_button", "collect_button"), size = 1, prob = c(.8, .2))
          list_get_elements$list_elements[[selected_button_id]]$clickElement()

        } else {

          selected_button_id = list_get_elements$name_buttons[sample(1:nrow(list_get_elements$name_buttons), 1),]$id
          list_get_elements$list_elements[[selected_button_id]]$clickElement()

        }

    } else {

      selected_button_id = "No buttons found"

    }


  # MESSAGE -----------------------------------------------------------------

  # Instructions screen
  # if (DEBUG == TRUE & length(list_get_elements$name_buttons$id) %in% c(1,2) & all(list_get_elements$name_buttons$id %in% c("jspsych-instructions-back", "jspsych-instructions-next"))) withr::with_options(list(crayon.enabled = FALSE), cat(crayon::bold("[Instructions]:", gsub("\n", "", list_get_elements$name_contents$content), "\n")))

  if (DEBUG == TRUE & length(list_get_elements$name_buttons$id) %in% c(1,2) & all(list_get_elements$name_buttons$id %in% c("jspsych-instructions-back", "jspsych-instructions-next"))) content_str = paste0("[Instructions]: ", content_str)


  if (exists("output_select_input$selected_input$value")) {
    if (is.na(output_select_input$selected_input$value)) output_select_input$selected_input$value = ""
  }

  if (exists("output_select_input$input_text_human_readable")) {
    if (is.na(output_select_input$input_text_human_readable)) output_select_input$input_text_human_readable = ""
  }

  # Selected response
  if (DEBUG == TRUE) withr::with_options(list(crayon.enabled = FALSE),
                                         cat(
                                           # Screen
                                           cli::style_bold("[SCREEN]"), glue::glue("[{index}]"), "\n",

                                           # Content
                                           cli::col_silver("[content]:"), glue::glue("{content_str}"), "\n",

                                           # Response
                                           cli::col_blue("[response]:"), glue::glue("{output_select_input$input_text_human_readable}"),
                                                                      cli::col_white(paste(output_select_input$input_text, collapse = ", ")), "\n",

                                           # Button
                                           cli::col_magenta("[button]:"), glue::glue("{selected_button_value}"),
                                                                                cli::col_white(paste(selected_button_id, collapse = ", ")), "\n"
                                         )
                                        )

  if (grepl("FINALIZAR ESTUDIO", selected_button_value)) {
    cli::cli_h1("-- ENDING -- ")
    Sys.sleep(1)
  }


  # Output ------------------------------------------------------------------

  output_interact =
    list(input = output_select_input$selected_input,
         button = selected_button_id,
         response = output_select_input$input_text)

    # return(output_interact)

}
