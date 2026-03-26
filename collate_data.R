# Clear workspace
rm(list = ls())

# Load libraries
library("dplyr")
library("readr")
library("stringr")
library("tidyverse")
library("zoo")
library("patchwork")


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

# Read and bind all selected files
dat <- latest_per_participant %>%
  mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
  pull(data) %>%
  bind_rows()

# Inspect combined data
head(dat)
tail(dat)
str(dat)

# Save master CSV
write_csv(dat, "data/data_virus_all.csv")

# To make the data more manageable, take a subset of relevant columns 
dat <- latest_per_participant %>%
  mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
  pull(data) %>%
  bind_rows() %>%
  select(
    participant_id,
    block,
    block_idx,
    trial,
    vblack_prop,
    stimulus,
    auto_on,
    aid_accuracy_setting,
    aid_label,
    aid_correct,
    response,
    rt_s,
    correct,
    feedback
  )

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
  bind_rows()

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
  bind_rows()

# Inspect combined data
head(dat)
tail(dat)
str(dat)
# View(dat)

# Save master CSV
write_csv(dat, "data/data_virus_sliders_all.csv")
