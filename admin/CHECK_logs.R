
CHECK_logs <- function(FOLDER = "outputs/log/") {

  # DEBUG
  # FOLDER = "outputs/log/"

library(tidyverse)


FILES = list.files(FOLDER, pattern = "csv", full.names = TRUE)
DF_raw = purrr::map_dfr(FILES %>% set_names(basename(.)), data.table::fread, .id = "filename", colClasses = 'character', encoding = 'UTF-8', nThread = 1) %>% tibble::as_tibble() %>% 
  tidyr::separate(filename, into = c("uid"), sep = "_", extra = "drop", remove = FALSE) %>% 
  select(-level, -source, -timestamp) %>% 
  dplyr::mutate(message = gsub(" [0-9]{3}:[0-9]{2} ", "", message))

DF = 
DF_raw %>% 
  dplyr::mutate(completed_task = dplyr::case_when(grepl(pattern = "completed_task_storage()", x = message, fixed = TRUE) ~ "completed_task_storage()", TRUE ~ NA_character_),
         counter_PLUS = dplyr::case_when(grepl(pattern = "counter + 1", x = message, fixed = TRUE) ~ "counter + 1", TRUE ~ NA_character_),
         counter_MINUS = dplyr::case_when(grepl(pattern = "counter - 1", x = message, fixed = TRUE) ~ "counter - 1", TRUE ~ NA_character_),
         assigned_PLUS = dplyr::case_when(grepl(pattern = "assigned_task + 1", x = message, fixed = TRUE) ~ "assigned_task + 1", TRUE ~ NA_character_),
         assigned_MINUS = dplyr::case_when(grepl(pattern = "assigned_task - 1", x = message, fixed = TRUE) ~ "assigned_task - 1", TRUE ~ NA_character_),
         discarded = dplyr::case_when(grepl(pattern = "uid_external", x = message, fixed = TRUE) ~ "clean_mysql DISCARDED", TRUE ~ NA_character_),
         discarded2 = dplyr::case_when(grepl(pattern = "completed_task_storage() || User discarded", x = message, fixed = TRUE) ~ "completed_task_storage DISCARDED", TRUE ~ NA_character_),
         blocked = dplyr::case_when(grepl(pattern = "condition_selection() | Final check | Participante bloqueado por límite en condiciones", x = message, fixed = TRUE) ~ "condition_selection BLOCKED", TRUE ~ NA_character_),
         selected_condition = dplyr::case_when(grepl(pattern = "Selected condition:", x = message, fixed = TRUE) ~ gsub(".*Selected condition: (.*)\"\"", "\\1", message), TRUE ~ NA_character_),
         all_conditions = dplyr::case_when(grepl(pattern = "All conditions:", x = message, fixed = TRUE) ~ gsub(".*All conditions: (.*).*", "\\1", message), TRUE ~ NA_character_),
         available_conditions = dplyr::case_when(grepl(pattern = "Available conditions:", x = message, fixed = TRUE) ~ gsub(".*Available conditions: (.*).*", "\\1", message), TRUE ~ NA_character_),
         
           
         
  ) %>% 
  dplyr::mutate(OUTPUT = 
           dplyr::case_when(
             !is.na(discarded) ~ gsub(".*uid_external ([0-9]{1,10}).*", "uid: \\1", message),
             !is.na(discarded2) ~ paste0("uid: ", uid),
             TRUE ~ NA_character_)
  )
  

check_what <- function(what = NULL) {
  
  # what = "completed_task"
  
  if (is.null(what)) {
    
    names(DF)
    
  } else {
    
    DF %>% 
      group_by(uid, get(what)) %>% 
      rename(get_what = `get(what)`) %>% 
      drop_na(what) %>% 
      summarise(N = n(), OUTPUT = paste(OUTPUT, collapse = " // "), MESSAGE = paste(message, collapse = "// ")) %>% 
      dplyr::mutate(MESSAGE = gsub("\\\\n", "", MESSAGE),
             MESSAGE = gsub("[^[:alnum:] |():,_+-]", "", MESSAGE),
             MESSAGE = gsub("   ", "", MESSAGE)) %>% 
      dplyr::mutate(get_what = gsub("\\\\n", "", get_what),
             get_what = gsub("[^[:alnum:] |():,_+-]", "", get_what),
             get_what = gsub("   ", "", get_what))
      
      # count(what = get(what))
  }
  
}

check_what("all_conditions") %>% arrange(desc(uid)) 

X1 = check_what("available_conditions") %>% arrange(desc(uid)) %>% select(uid, get_what) %>% rename(available_conditions = get_what)
X2 = check_what("selected_condition") %>% arrange(desc(uid)) %>% select(uid, get_what) %>% rename(selected_condition = get_what)
X1 %>% full_join(X2, by = "uid") %>% dplyr::mutate(available_conditions = gsub("\\\\", "", available_conditions))

X1 = check_what("all_conditions") %>% arrange(desc(uid)) %>% select(uid, get_what) %>% rename(all_conditions = get_what)
X2 = check_what("selected_condition") %>% arrange(desc(uid)) %>% select(uid, get_what) %>% rename(selected_condition = get_what)
X1 %>% full_join(X2, by = "uid") %>% dplyr::mutate(all_conditions = gsub("\\\\", "", all_conditions)) %>% View



check_what("completed_task") %>% dplyr::filter(N == 115) %>% arrange(uid) %>% dplyr::pull(uid)
check_what("counter_PLUS") 
check_what("counter_MINUS") 

check_what("assigned_PLUS") 
check_what("assigned_MINUS") 


check_what("discarded")
check_what("discarded2")
check_what("blocked")


DF %>% 
  dplyr::mutate(message = gsub("\\\\n", "", message)) %>%
  dplyr::mutate(message = gsub("[^[:alnum:] |():,_+-]", "", message)) %>%
  dplyr::mutate(message = gsub("   ", "", message)) %>% 
  dplyr::filter(grepl("UPDATE|DISCARD", message)) %>% select(uid, message) %>% 
  View
  # dplyr::pull(message) %>% .[900:910]

}

