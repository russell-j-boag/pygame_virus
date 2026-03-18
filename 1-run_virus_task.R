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
# run_task(block = "MANUAL")
# run_task(block = "AUTOMATION1")
# run_task(block = "AUTOMATION2")
