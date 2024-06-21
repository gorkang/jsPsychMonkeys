# jsPsychMonkeys 0.3.5.901

Minor updates

* Sync version number with admin, maker, monkeys and manual
* All non-mysql credentials parameters are now credentials_file instead of list_credentials
* Works with Likert scales


# jsPsychMonkeys 0.2.7

Minor updates

* Support for fancy datalist input
* Checks for uid. As it is used to set the random seed, we need it to be a number less than 2147483647
* Adapt to last _targets version for error printing

# jsPsychMonkeys 0.2.6

Major updates  

* 2 new parameters times_repeat_protocol and time_to_sleep_before_repeating_protocol so monkeys can complete more than 1 time the same task
  - This can be used to speed up the monkeys (no need to recreate docker containers, etc.) and for longitudinal protocols

Minor updates

* Fixed some issues with finishing tasks
* implemented WAIS WM response
* Working with lists now (selects one of the alternatives, and includes in the sample pool a random string)
* New clean_monkeys_containers() function


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
