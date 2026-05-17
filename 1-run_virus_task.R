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
# run_task(block = "AUTOMATION", aid_onset_ms = -500, reliability_group = "high")
# run_task(block = "AUTOMATION", aid_onset_ms = 0, reliability_group = "high")
# run_task(block = "AUTOMATION", aid_onset_ms = 500, reliability_group = "high")
# run_task(block = "AUTOMATION", aid_onset_ms = -500, reliability_group = "low")
# run_task(block = "AUTOMATION", aid_onset_ms = 0, reliability_group = "low")
# run_task(block = "AUTOMATION", aid_onset_ms = 500, reliability_group = "low")
