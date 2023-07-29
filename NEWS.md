# jsPsychMonkeys 0.2.6

Major updates  

* 2 new parameters times_repeat_protocol and time_to_sleep_before_repeating_protocol so monkeys can complete more than 1 time the same task
  - This can be used to speed up the monkeys (no need to recreate docker containers, etc.) and for longitudinal protocols

**************
en existing_CSV(links_tar = links_tar)
y en copy_files_to_data(links_tar = links_tar)
Necesitamos pasarle links_tar para poder extraer los uid reales usados
De lo contrario no se rescatan los CSVs

HAY QUE MODIFICAR el _targets_TEMPLATE.R!!!
**************

Minor updates

* Fixed some issues with finishing tasks
* implemented WAIS WM response

# jsPsychMonkeys 0.2.5

Major updates  

* Now jsPsychMonkeys is an R package
* release_the_monkeys() creates the RProject and launches the monkeys from a tempdir()
* Included several tests to check release_the_monkeys()
* More robust, capturing error conditions better.
* Change the internal logic of complete_tasks to capture end conditions better
* Monkeys are now 2x faster
* DEBUG messages improved
* Works in Windows
* ~90 tasks run fine
* random_number_trimmed() to sample from a trimmed normal or uniform distribution in numeric responses
* lots of small improvements



# jsPsychMonkeys 0.2.0

Major updates  

* Works with jsPsychMaker 0.2.0
* Working with ~60 tasks
* Added new parameter `forced_random_wait` so the Monkeys wait a random amount (to get over max_time)
* Added new parameter `forced_seed` so the Monkeys' behavior is reproducible
* Added new parameter `forced_refresh` so the Monkeys restart the protocol (to check they continue in the task where they left off)
