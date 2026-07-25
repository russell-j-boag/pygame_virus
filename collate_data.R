# Clear workspace
rm(list = ls())

# Load libraries
library("dplyr")
library("purrr")
library("readr")
library("stringr")
library("tibble")

# Usage:
#   Rscript collate_data.R [input_dir] [output_dir]

args <- commandArgs(trailingOnly = TRUE)

INPUT_DIR <- if (length(args) >= 1) args[[1]] else "output"
OUTPUT_DIR <- if (length(args) >= 2) args[[2]] else "data"

if (!dir.exists(INPUT_DIR)) {
  stop("Input directory does not exist: ", INPUT_DIR, call. = FALSE)
}

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

latest_complete_files <- function(pattern, label) {
  files <- list.files(
    INPUT_DIR,
    pattern = pattern,
    full.names = TRUE
  )

  if (!length(files)) {
    stop("No ", label, " files found in ", INPUT_DIR, call. = FALSE)
  }

  file_tbl <- tibble(path = files) %>%
    mutate(
      file_name = basename(path),
      participant_id = str_extract(file_name, "(?<=results_)[^_]+"),
      mtime = file.info(path)$mtime
    )

  if (any(is.na(file_tbl$participant_id))) {
    stop("Could not extract participant IDs from all ", label, " filenames.", call. = FALSE)
  }

  latest <- file_tbl %>%
    group_by(participant_id) %>%
    slice_max(order_by = mtime, n = 1, with_ties = FALSE) %>%
    ungroup()

  print(latest %>% select(participant_id, path, mtime))
  latest
}

postblock_single_phase_cols <- c(
  "reliability_phase_idx",
  "trial_in_reliability_phase",
  "aid_reliability_level",
  "automation_reliability_group"
)

# 1) First collate all choice-RT results files ----------------------------

# Find all complete results files
latest_per_participant <- latest_complete_files(
  "^results_.*_b00_ALL\\.csv$",
  "complete trial"
)
nrow(latest_per_participant)

# Read and bind all selected files
dat <- latest_per_participant %>%
  mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
  pull(data) %>%
  bind_rows()

# Normalise trial index naming across exports.
if (!"trial_idx" %in% names(dat) && "trial" %in% names(dat)) {
  dat <- dat %>%
    mutate(trial_idx = trial)
}

# Inspect combined data
head(dat)
tail(dat)
str(dat)
length(unique(dat$participant_id))

# Save master CSV
write_csv(dat, file.path(OUTPUT_DIR, "data_virus_all.csv"))

# To make the data more manageable, take a subset of relevant columns 
dat <- latest_per_participant %>%
  mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
  pull(data) %>%
  bind_rows()

if (!"trial" %in% names(dat) && "trial_idx" %in% names(dat)) {
  dat <- dat %>%
    mutate(trial = trial_idx)
}

dat <- dat %>%
  select(
    participant_id,
    block,
    block_idx,
    any_of(c(
      "condition_code",
      "calibration_target_group",
      "calibration_target_accuracy",
      "main_block_order",
      "manual_segment",
      "reliability_phase_idx",
      "trial_in_reliability_phase",
      "reliability_phase_label",
      "aid_reliability_level",
      "automation_reliability_group",
      "trial_deadline_s"
    )),
    trial,
    vblack_prop,
    stimulus,
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
write_csv(dat, file.path(OUTPUT_DIR, "data_virus.csv"))


# 2) Collate post-block questionnaire files -------------------------------

# Find all complete post-block results files
latest_per_participant <- latest_complete_files(
  "^results_.*_b00_POSTBLOCK_ALL\\.csv$",
  "complete post-block questionnaire"
)

# Read and bind all selected files
dat <- latest_per_participant %>%
  mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
  pull(data) %>%
  bind_rows() %>%
  select(-any_of(postblock_single_phase_cols))

# Inspect combined data
head(dat)
tail(dat)
str(dat)
# View(dat)

# Save master CSV
write_csv(dat, file.path(OUTPUT_DIR, "data_virus_postblock_all.csv"))


# 3) Collate post-block accuracy slider files -----------------------------

# Find all complete post-block results files
latest_per_participant <- latest_complete_files(
  "^results_.*_b00_POSTBLOCK_SLIDERS_ALL\\.csv$",
  "complete post-block slider"
)

# Read and bind all selected files
dat <- latest_per_participant %>%
  mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
  pull(data) %>%
  bind_rows()

# Normalise slider column names across exports (both legacy and current names).
if (!"question_key" %in% names(dat) && "slider_key" %in% names(dat)) {
  dat <- dat %>%
    mutate(question_key = slider_key)
}

if (!"slider_key" %in% names(dat) && "question_key" %in% names(dat)) {
  dat <- dat %>%
    mutate(slider_key = question_key)
}

if (!"response_percent" %in% names(dat) && "response" %in% names(dat)) {
  dat <- dat %>%
    mutate(response_percent = response)
}

if (!"response" %in% names(dat) && "response_percent" %in% names(dat)) {
  dat <- dat %>%
    mutate(response = response_percent)
}

dat <- dat %>%
  select(-any_of(postblock_single_phase_cols))

# Inspect combined data
head(dat)
tail(dat)
str(dat)
# View(dat)

# Save master CSV
write_csv(dat, file.path(OUTPUT_DIR, "data_virus_sliders_all.csv"))
