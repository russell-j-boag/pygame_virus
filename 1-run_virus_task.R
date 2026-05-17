# Clear workspace
rm(list = ls())

# Source helper functions
source("0-helpers.R")

# Run pre-task instructions
run_instructions()

# Run task
run_task()

# # You can also run one block at a time by selecting a specific block:
# run_task(block = "CALIBRATION")
# run_task(block = "MANUAL", deadline_s = 3)
# run_task(block = "MANUAL", deadline_s = 6)
# run_task(block = "AUTOMATION", deadline_s = 3, reliability_group = "high")
# run_task(block = "AUTOMATION", deadline_s = 6, reliability_group = "high")
# run_task(block = "AUTOMATION", deadline_s = 3, reliability_group = "low")
# run_task(block = "AUTOMATION", deadline_s = 6, reliability_group = "low")
