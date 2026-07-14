# Clear workspace
rm(list = ls())

# Source helper functions
source("0-helpers.R")

# Run pre-task instructions
run_instructions()

# Run task
run_task()

# You can also run one block at a time by selecting a specific block:
# run_task(block = "CAL")
# run_task(block = "MAN/PRE_AUTOMATION")
# run_task(block = "REL_DROP")
# run_task(block = "MAN/POST_AUTOMATION")
