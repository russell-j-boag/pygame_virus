# Clear workspace
rm(list = ls())

# Source helper functions
source("0-helpers.R")

# Run pre-task instructions
run_instructions()

# Run task
run_task()

# # You can also run one block at a time by selecting a specific block:
# # Calibration always uses the 4 s LP deadline.
# run_task(block = "CALIBRATION", deadline_s = 4)
# run_task(block = "MANUAL", deadline_s = 2)
# run_task(block = "MANUAL", deadline_s = 4)
# run_task(block = "AUTOMATION", deadline_s = 2, reliability_group = "high")
# run_task(block = "AUTOMATION", deadline_s = 4, reliability_group = "low")
