#' Helper to select and interact with input fields
#'
#' @param list_get_elements list_get_elements
#' @param DEBUG TRUE / FALSE
#' @param seed numeric random seed
#' @param remDr .
#'
#' @return list with selected input and the text of the selected element
#' @export
select_input <- function(list_get_elements, remDr = NULL, DEBUG = FALSE, seed = 1) {

  # DEBUG
  # targets::tar_load_globals()
  # debug_function("complete_task")
  # debug_docker(uid_participant = uid)
  # reconnect_to_VNC(container_name = container_name)
  # list_get_elements = get_elements(remDr = remDr, DEBUG = DEBUG); list_get_elements
  # DEBUG = TRUE
  # seed = 1

  # SET SEED ----------------------------------------------------------------

    set.seed(seed)
    # cat(crayon::bgRed("\n seed - select_input: ", seed, "\n"))


  # SELECTED ----------------------------------------------------------------

    # Randomly select one of the inputs
    selected_input = list_get_elements$name_inputs[sample(1:nrow(list_get_elements$name_inputs), 1),]
    selected_input_name = selected_input$id

    # Add columns if they don't exist
    cols <- c(pattern = NA_character_, value = NA_character_)
    selected_input = selected_input %>% tibble::add_column(!!!cols[!names(cols) %in% names(.)])

    # if (DEBUG == TRUE) cat(crayon::yellow("Selected", selected_input_name, "from", nrow(list_get_elements$name_inputs), "elements \n"))


  # CHECK -------------------------------------------------------------------

    # REVIEW
    # html-form: In html-form items, we do not have (yet) a standard way to name items. BUT we do have one for buttons
    if ("jspsych-btn jspsych-survey-html-form" %in% list_get_elements$name_buttons$class) list_get_elements$name_inputs$type_extracted = "html-form"

    # CATCH ALL condition: When we can't extract an input type, try everything (?)
    # TODO: CREATE SAFE ClearElement helper function to be able to include ALL types in the ALL
    if (all(list_get_elements$name_inputs %>% dplyr::filter(id == selected_input_name) %>% dplyr::pull(type_extracted) == "ALL")) {

      cat(crayon::bgYellow(" WARNING: Unknown type of input. Trying ALL\n"))

      # Anything with ClearElement errors (?)
      # ERRORS: "date", "html-form" "number", "text",  "slider",
      types_of_input = c("radio", "multi-select")
      selected_input = 1:length(types_of_input) %>%
        purrr::map_df(~
                        list_get_elements$name_inputs %>%
                        dplyr::filter(id == selected_input_name) %>%
                        dplyr::mutate(type_extracted = types_of_input[.x])
                      )
    }



  # TYPES OF INPUTS ------------------------------------------------------------

  if (DEBUG == TRUE) cli::cli_alert_info("Type extracted: {.code {selected_input$type_extracted}}")

  if (any(selected_input$type_extracted %in% c("list"))) {

    if (!is.na(selected_input$pattern)) {

      validated_inputs = gsub("\\^\\(|\\)\\$", "", selected_input$pattern) %>% strsplit(split = "\\|") %>% unlist()
      input_text = sample(x = validated_inputs, size = 1)
      list_get_elements$list_elements[[selected_input_name]]$clearElement()
      list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))

    } else {

      # Extract all elements of the list
      option <- remDr$findElements(using = 'xpath', "//*/option")
      OPTIONS = 1:length(option) |> map(~ { option[[.x]]$getElementAttribute(attrName = "value")}) |> unlist()

      # Create a random string, to include amongst the possible options (checks if you can write something out of the list)
        # Default characters limits
        min_chars = 0
        max_chars = 100

        # If minlength and maxlength exist, replace default limits
        if (!is.na(list_get_elements$name_inputs$minlength)) min_chars = as.numeric(list_get_elements$name_inputs$minlength)
        if (!is.na(list_get_elements$name_inputs$maxlength)) max_chars = as.numeric(list_get_elements$name_inputs$maxlength)

        input_text = as.character(stringi::stri_rand_strings(n = 1, length = sample(min_chars:max_chars, 1)))


      # Choose one at random
      selected_option = sample(c(OPTIONS, input_text), 1)

      # Send it
      list_get_elements$list_elements[[selected_input_name]]$clearElement()
      list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(selected_option))

    }


  # checkbox -------------------------------------------------------------------

  } else if (any(selected_input$type_extracted %in% c("checkbox")) & any(!grepl("multi-select", selected_input$id))) {

    input_text = selected_input_name
    # input_text_human_readable = destination_slider


    list_get_elements$list_elements[[selected_input_name]]$clickElement()


  # date --------------------------------------------------------------------

  } else if (any(selected_input$type_extracted %in% c("date"))) {

    min_date = as.Date("1900-01-01")
    max_date = as.Date(Sys.Date())

    input_text = format(sample(seq(min_date, max_date, by = "day"), 1), "%m/%d/%Y")

    list_get_elements$list_elements[[selected_input_name]]$clearElement()
    list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))


  # email --------------------------------------------------------------------

  } else if (any(selected_input$type_extracted %in% c("email"))) {

    input_text = paste0(as.character(stringi::stri_rand_strings(n = 1, length = sample(5:10, 1))), "@gmail.com")

    list_get_elements$list_elements[[selected_input_name]]$clearElement()
    list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))


  # html-form -------------------------------------------------------------------

  } else if (any(selected_input$type_extracted %in% c("html-form"))) {

      number_textboxes = length(list_get_elements$name_inputs$name)
      type = list_get_elements$name_inputs$type
      type[is.na(type)] = "" # If there are NA's, replace
      # if (is.na(type)) type = ""

      if (number_textboxes == 1) {

        if (type == "number") {

          # Default Limits
          min_num = 0
          max_num = 100

          # If max and min exist, replace default limits
          if (!is.na(list_get_elements$name_inputs$min)) min_num = as.numeric(list_get_elements$name_inputs$min)
          if (!is.na(list_get_elements$name_inputs$max)) max_num = as.numeric(list_get_elements$name_inputs$max)

          # Create random number
          input_text = as.character(sample(min_num:max_num, 1))

          list_get_elements$list_elements[[selected_input_name]]$clearElement()
          list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))

        } else if (type == "email") {

          input_text = paste0(as.character(stringi::stri_rand_strings(n = 1, length = sample(5:10, 1))), "@gmail.com")

          list_get_elements$list_elements[[selected_input_name]]$clearElement()
          list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))

        } else if (type == "") {

          # One textbox. Probably a searchable list
          selected_input = list_get_elements$name_inputs[sample(1:nrow(list_get_elements$name_inputs), 1),]

          name_list = list_get_elements$list_elements[[selected_input_name]]$getElementAttribute("list") %>% unlist()
          options_list = remDr$findElements(using = 'xpath', paste0("//datalist[@id='", name_list, "']/option"))
          num_selected = sample(length(options_list), 1)
          input_text = options_list[[num_selected]]$getElementAttribute("value") %>% unlist()

          list_get_elements$list_elements[[selected_input_name]]$clearElement()
          list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text))

        } else {
          if (DEBUG == TRUE) cat(crayon::bgRed(" UNKNOWN html-form \n"))
        }

      # Multiple textboxes
      } else {

        input_text = as.character(stringi::stri_rand_strings(n = number_textboxes, length = 10))

        1:number_textboxes %>%
          purrr::walk(~ {
            selected_input_name = list_get_elements$name_inputs$name[.x]
            list_get_elements$list_elements[[selected_input_name]]$clearElement()
            list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text[.x]))
          })
      }


  # radio -------------------------------------------------------------------

  } else if (any(selected_input$type_extracted %in% c("radio"))) {

    radio_groups = unique(list_get_elements$name_inputs$name)
    # list_get_elements$name_inputs

    selected_input = 1:length(radio_groups) %>%
      purrr::map_df( ~ {
        temp_list_get_elements = list_get_elements$name_inputs %>% dplyr::filter(name == radio_groups[.x])

        selected_input = temp_list_get_elements[sample(1:nrow(temp_list_get_elements), 1),]
        selected_input_name = selected_input$id
        input_text_human_readable = selected_input$value
        list_get_elements$list_elements[[selected_input_name]]$clickElement()

        selected_input |> dplyr::mutate(input_text = selected_input_name,
                                 input_text_human_readable = input_text_human_readable)

      })

    input_text = selected_input |> dplyr::pull(input_text)
    input_text_human_readable = selected_input |> dplyr::pull(input_text_human_readable)



  # number ------------------------------------------------------------------

  } else if (any(selected_input$type_extracted %in% c("number"))) {

    number_textboxes = length(list_get_elements$name_inputs$name)

    # Default Limits
    min_num = 0
    max_num = 100

    content_text = list_get_elements$name_contents$content
    if (length(content_text) == 0) content_text = ""

    # Hardcoded conditions
    if (grepl("Rut Completo| rut ", content_text)) input_text = random_number_trimmed(1, sd = 100000000 * 20, min = 100000000, max = 999999999)
    if (grepl("celular", content_text)) input_text = random_number_trimmed(1, min = 900000000, max = 999999999, distrib = "uniform")
    if (grepl("How old are you|edad", content_text)) input_text = random_number_trimmed(1, mean = 20, sd = 10, min = 17, max = 90)

    # If max and min exist, replace default limits
    if (all(!is.na(list_get_elements$name_inputs$min))) min_num = as.numeric(list_get_elements$name_inputs$min)
    if (all(!is.na(list_get_elements$name_inputs$max))) max_num = as.numeric(list_get_elements$name_inputs$max)

    # input_text = as.character(sample(min_num:max_num, number_textboxes))
    if (!exists("input_text")) {
      input_text = as.character(random_number_trimmed(number_textboxes, min = min_num, max = max_num))
    } else {
      input_text = as.character(input_text)
    }

    if (DEBUG == TRUE) cli::cli_alert_info("NUMBER: {format(as.numeric(input_text), nsmall = 0, big.mark = ',')}")

    1:number_textboxes %>%
      purrr::walk(~ {
        # .x = 1
        selected_input_name = list_get_elements$name_inputs$name[.x]

        # The above is null in the 999new protocol with jsPsych 7.3
        if (is.null(list_get_elements$list_elements[[selected_input_name]])) selected_input_name = list_get_elements$name_inputs$id[.x]

        list_get_elements$list_elements[[selected_input_name]]$clearElement()
        list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text[.x]))

      })


  # text --------------------------------------------------------------------

  } else if (any(selected_input$type_extracted %in% c("text"))) {

    number_textboxes = length(list_get_elements$name_inputs$name)
    # if (DEBUG == TRUE) cli::cli_alert_info("number_textboxes: {.code {number_textboxes}}")

    # Get content
    content_text = list_get_elements$name_contents$content
    if (length(content_text) == 0) content_text = ""
    if (DEBUG == TRUE) cli::cli_alert_info("content_text: {.code {content_text}}")

    # Default characters limits
    min_chars = 0
    max_chars = 100

    # If minlength and maxlength exist, replace default limits
    if (all(!is.na(list_get_elements$name_inputs$minlength))) min_chars = as.numeric(list_get_elements$name_inputs$minlength)
    if (all(!is.na(list_get_elements$name_inputs$maxlength))) max_chars = as.numeric(list_get_elements$name_inputs$maxlength)

    input_text = as.character(stringi::stri_rand_strings(n = number_textboxes, length = sample(min_chars:max_chars, number_textboxes)))


    # IF there is an ID in the URL, and the input asked is an ID ---

      # This is used to get participants to repeat the same protocol multiple times matching them by the ID
      # We use the times_repeat_protocol parameter in create_links() to build URLs with ID in them

      # Use the URL ID to fill the input
      ID_input = grepl("Ingrese el identificador que se le ha asignado:", content_text)

      CURRENT_URL = remDr$getCurrentUrl()
      ID_url = stringr::str_extract(CURRENT_URL, "ID=.*")
      if (DEBUG == TRUE) cli::cli_alert_info("ID_input = {.code {ID_input}}")

      if (!is.na(ID_url) & ID_input) {
        input_text = gsub("ID=(.*)", "\\1", ID_url)
        if (DEBUG == TRUE) cli::cli_alert_info("Using ID in URL as input_text | ID_url = {.code {ID_url}}")
      } else {
        if (DEBUG == TRUE) cli::cli_alert_info("ID not found in URL")
      }


    if (DEBUG == TRUE) cli::cli_alert_info("input_text: {.code {input_text}}")

    # Fill all the textboxes

    1:number_textboxes %>%
      purrr::walk(~ {
        # .x = 1
        selected_input_name = list_get_elements$name_inputs$name[.x]

        # The above is null in the 999new protocol with jsPsych 7.3
        if (is.null(list_get_elements$list_elements[[selected_input_name]])) selected_input_name = list_get_elements$name_inputs$id[.x]

        list_get_elements$list_elements[[selected_input_name]]$clearElement()
        list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(list(input_text[.x]))
      })


  # slider ------------------------------------------------------------------

  } else if (any(selected_input$type_extracted %in% c("slider"))) {

    # Check min, max and initial value of slider to choose a path that is possible
    min_slider = as.integer(selected_input$min)
    max_slider = as.integer(selected_input$max)
    initial_value_slider = as.integer(selected_input$value) #present_elements$SLIDR[[1]]$getElementAttribute("value")
    destination_slider = sample(min_slider:max_slider, 1)
    trip_slider = destination_slider - initial_value_slider
    path_slider = rep(trip_slider / abs(trip_slider), abs(trip_slider))

    # If we will stay in the same position, need to choose a path that does not go beyond min or max
    if (length(path_slider) == 0) {
      if (initial_value_slider == min_slider) {
        path_slider = c(1, -1)
      } else {
        path_slider = c(-1, 1)
      }
    }

    final_vector = stringr::str_replace_all(path_slider, stats::setNames(c("left_arrow", "b", "right_arrow"), c(-1, 0, 1)))
    input_text = 1:length(final_vector) %>% purrr::map(~ list(key = final_vector[.x])) %>% purrr::flatten()
    list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(sendKeys = input_text)

    input_table = table(input_text |> unlist())
    input_text = paste0(names(input_table), ": ", input_table)
    input_text_human_readable = destination_slider


    # multi-select ------------------------------------------------------------

  } else if (any(selected_input$type_extracted %in% c("multi-select")) | grepl("multi-select", selected_input$id)) {

    all_checkboxers = list_get_elements$name_inputs$id
    n_selected = sample(1:length(list_get_elements$name_inputs$name), 1)
    selected_checkboxes = sample(all_checkboxers, n_selected)

    input_text = selected_checkboxes

    1:length(selected_checkboxes) %>%
      purrr::walk(~ list_get_elements$list_elements[[selected_checkboxes[.x]]]$clickElement())



    # keyboard_response -------------------------------------------------------

  } else if (any(selected_input$type_extracted %in% c("keyboard_response"))) {

    content_responses = list_get_elements$name_inputs$content
    response_options = stringr::str_extract_all(content_responses, '\\"[:alnum:]\\"')[[1]] %>% gsub('\\"', "", .)

    # In html-keyboard-response (https://www.jspsych.org/6.3/plugins/jspsych-html-keyboard-response/) and others,
    # the allowed keys are stored in jsPsych.ALL_KEYS
    if (length(response_options) == 0) {
      # list_get_elements$DF_elements_options$id

      # Get allowed keys
      raw_ALL_KEYS = remDr$executeScript("return jsPsych.ALL_KEYS;")

      # jsPsych.ALL_KEYS does not work? ALL_KEYS from jsPsych7.0. but it does not work?
      # WE CAN acces the allowed choices if we know the questions index:
            # questions[55].choices

      # If we found them
      if (length(raw_ALL_KEYS) > 0) {

        cli::cli_alert_info("keyboard-response type of plugin detected: \n\n jsPsych.ALL_KEYS: {raw_ALL_KEYS[[1]]}")

        ALL_KEYS = raw_ALL_KEYS[[1]]

        # If the value is allkeys, any key should work
        if (ALL_KEYS == "allkeys") {
          selected_response = as.character(stringi::stri_rand_strings(n = 1, length = 1, pattern = "[0-9a-z]"))
        # Else, try only the key indicated by ALL_KEYS
        } else {
          selected_response = ALL_KEYS
        }

        # If nothing in response_options and nothing in ALL_KEYS, not sure what is happening
      } else {
        cli::cli_alert_danger("Not sure what to do with plugin/s: \n\n {list_get_elements$DF_elements_options$id}")
        # We try to send a single character, just in case
        selected_response = as.character(stringi::stri_rand_strings(n = 1, length = 1))
      }

    } else {
      selected_response = tolower(sample(response_options, 1))
    }



    # list_get_elements$list_elements[[selected_input_name]]$sendKeysToElement(sendKeys = as.list(selected_response))

    cli::cli_alert_info("Selected response: {selected_response}")

    webElem <- remDr$findElement("css", "html")
    webElem$sendKeysToElement(sendKeys = as.list(selected_response))


    # remDr$screenshot(display = TRUE)
    # sendsendKeysToActiveElement(as.list(selected_response))


    # input_button ------------------------------------------------------------

  } else if (any(selected_input$type_extracted %in% c("input_button"))) {

    # NOT FOUND
    cli::cli_alert_danger("Can't access mic in Docker container... See Console in Chrome:(")
    list_get_elements$list_elements[[1]]$highlightElement()
    list_get_elements$list_elements[[1]]$clickElement()


  } else {

    input_text = "No input elements "

  }



  # OUTPUT ------------------------------------------------------------------

  if (!exists("input_text")) input_text = ""
  if (!exists("input_text_human_readable")) input_text_human_readable = input_text

  output_select_input = list(selected_input = selected_input,
                             input_text = input_text,
                             input_text_human_readable = input_text_human_readable)

  return(output_select_input)

}
