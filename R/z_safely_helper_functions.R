# Include here safely versions of functions

launch_task_safely = safely(launch_task)

# Get elements safely
get_elements_safely <- purrr::safely(get_elements)

# Interact with elements safely
interact_with_element_safely <- purrr::safely(interact_with_element)
