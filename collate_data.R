# Clear workspace
rm(list = ls())

# Load libraries
library("dplyr")
library("readr")
library("stringr")
library("purrr")

TRIAL_COLUMNS <- c(
  "participant_id",
  "run_timestamp",
  "key_black",
  "key_white",
  "keymap_flip",
  "block",
  "block_idx",
  "condition_deadline_code",
  "time_pressure_condition",
  "trial_deadline_s",
  "automation_reliability_pattern",
  "automation_reliability_group",
  "trial",
  "global_trial",
  "difficulty_mode",
  "delta_fixed_mean",
  "delta_fixed_sd",
  "delta_stair_realised",
  "delta_stair_mean",
  "delta_step_down_used",
  "delta_step_up_used",
  "vblack_prop",
  "n_vblack",
  "n_vwhite",
  "auto_on",
  "aid_accuracy_setting",
  "aid_transparency_level",
  "stimulus",
  "aid_label",
  "aid_correct",
  "aid_onset_ms",
  "aid_onset_ms_rel",
  "response",
  "correct",
  "feedback",
  "rt_s"
)

TRIAL_SUMMARY_COLUMNS <- c(
  "participant_id",
  "block",
  "block_idx",
  "condition_deadline_code",
  "time_pressure_condition",
  "automation_reliability_pattern",
  "automation_reliability_group",
  "trial_deadline_s",
  "trial",
  "vblack_prop",
  "stimulus",
  "auto_on",
  "aid_accuracy_setting",
  "aid_label",
  "aid_correct",
  "response",
  "rt_s",
  "correct",
  "feedback"
)

POSTBLOCK_COLUMNS <- c(
  "participant_id",
  "block",
  "block_idx",
  "condition_deadline_code",
  "time_pressure_condition",
  "trial_deadline_s",
  "automation_reliability_pattern",
  "automation_reliability_group",
  "question_idx",
  "question",
  "left_anchor",
  "right_anchor",
  "response",
  "scale_min",
  "scale_max"
)

SLIDER_COLUMNS <- c(
  "participant_id",
  "run_timestamp",
  "block",
  "block_idx",
  "condition_deadline_code",
  "time_pressure_condition",
  "trial_deadline_s",
  "automation_reliability_pattern",
  "automation_reliability_group",
  "question_idx",
  "question_key",
  "question",
  "response_percent"
)

# 1) First collate all choice-RT results files ----------------------------

# Find all complete results files
files <- list.files(
  "output",
  pattern = "^results_.*_b00_ALL\\.csv$",
  full.names = TRUE
)

# Make a file table
file_tbl <- tibble(path = files) %>%
  mutate(
    file_name = basename(path),
    participant_id = str_extract(file_name, "(?<=results_)[^_]+"),
    mtime = file.info(path)$mtime
  )

# For each participant, keep only the most recent b00_ALL file
latest_per_participant <- file_tbl %>%
  group_by(participant_id) %>%
  slice_max(order_by = mtime, n = 1, with_ties = FALSE) %>%
  ungroup()

# Show which files were selected
print(latest_per_participant %>% select(participant_id, path, mtime))
nrow(latest_per_participant)

# Read and bind all selected files
dat <- latest_per_participant %>%
  mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
  pull(data) %>%
  bind_rows() %>%
  select(all_of(TRIAL_COLUMNS))

# Inspect combined data
head(dat)
tail(dat)
str(dat)
length(unique(dat$participant_id))

# Save master CSV
write_csv(dat, "data/data_virus_all.csv")

# To make the data more manageable, take a subset of relevant columns 
dat <- latest_per_participant %>%
  mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
  pull(data) %>%
  bind_rows() %>%
  select(all_of(TRIAL_SUMMARY_COLUMNS))

dat <- data.frame(dat)

# Inspect combined data
head(dat)
tail(dat)
str(dat)
# View(dat)

# Save master CSV
write_csv(dat, "data/data_virus.csv")


# 2) Collate post-block questionnaire files -------------------------------

# Find all complete post-block results files
files <- list.files(
  "output",
  pattern = "^results_.*_b00_POSTBLOCK_ALL\\.csv$",
  full.names = TRUE
)

# Make a file table
file_tbl <- tibble(path = files) %>%
  mutate(
    file_name = basename(path),
    participant_id = str_extract(file_name, "(?<=results_)[^_]+"),
    mtime = file.info(path)$mtime
  )

# For each participant, keep only the most recent POSTBLOCK_ALL file
latest_per_participant <- file_tbl %>%
  group_by(participant_id) %>%
  slice_max(order_by = mtime, n = 1, with_ties = FALSE) %>%
  ungroup()

# Show which files were selected
print(latest_per_participant %>% select(participant_id, path, mtime))

# Read and bind all selected files
dat <- latest_per_participant %>%
  mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
  pull(data) %>%
  bind_rows() %>%
  select(all_of(POSTBLOCK_COLUMNS))

# Inspect combined data
head(dat)
tail(dat)
str(dat)
# View(dat)

# Save master CSV
write_csv(dat, "data/data_virus_postblock_all.csv")


# 3) Collate post-block accuracy slider files -----------------------------

# Find all complete post-block results files
files <- list.files(
  "output",
  pattern = "^results_.*_b00_POSTBLOCK_SLIDERS_ALL\\.csv$",
  full.names = TRUE
)

# Make a file table
file_tbl <- tibble(path = files) %>%
  mutate(
    file_name = basename(path),
    participant_id = str_extract(file_name, "(?<=results_)[^_]+"),
    mtime = file.info(path)$mtime
  )

# For each participant, keep only the most recent POSTBLOCK_SLIDERS_ALL file
latest_per_participant <- file_tbl %>%
  group_by(participant_id) %>%
  slice_max(order_by = mtime, n = 1, with_ties = FALSE) %>%
  ungroup()

# Show which files were selected
print(latest_per_participant %>% select(participant_id, path, mtime))

# Read and bind all selected files
dat <- latest_per_participant %>%
  mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
  pull(data) %>%
  bind_rows() %>%
  select(all_of(SLIDER_COLUMNS))

# Inspect combined data
head(dat)
tail(dat)
str(dat)
# View(dat)

# Save master CSV
write_csv(dat, "data/data_virus_sliders_all.csv")
