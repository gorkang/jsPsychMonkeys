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
