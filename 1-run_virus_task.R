# Clear workspace
rm(list = ls())

# Source helper functions
source("0-helpers.R")

# Run pre-task instructions
run_instructions()

# Run task
run_task()

# You can also run one condition at a time:
# run_task(aid_condition = "practice")
# run_task(aid_condition = "manual")
# run_task(aid_condition = "aid_first")
# run_task(aid_condition = "stimulus_first")
