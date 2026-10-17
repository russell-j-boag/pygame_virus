# Clear workspace
rm(list = ls())

# Load libraries
library("dplyr")
library("readr")
library("stringr")
library("purrr")

# Usage:
#   Rscript collate_data.R [input_dir] [output_dir]

script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
root <- dirname(normalizePath(script))
source(file.path(root, "R/participant_identity.R"))
args <- commandArgs(trailingOnly = TRUE)

INPUT_DIR <- if (length(args) >= 1) args[[1]] else "output"
OUTPUT_DIR <- if (length(args) >= 2) args[[2]] else "data"

if (!dir.exists(INPUT_DIR)) {
  stop("Input directory does not exist: ", INPUT_DIR, call. = FALSE)
}

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

TRIAL_COLUMNS <- c(
  "participant_id",
  "source_participant_id",
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
  "source_participant_id",
  "run_timestamp",
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

# Timing fields are optional for historical exports. Keep existing required
# columns strict and preserve missing values rather than labelling old trials.
OPTIONAL_TIMING_COLUMNS <- c(
  "rt_timing_version", "rt_backend_version", "stimulus_onset_s",
  "response_timestamp_s", "n_ignored_pre_onset_keydowns",
  "response_readout_delay_s"
)

POSTBLOCK_COLUMNS <- c(
  "participant_id",
  "source_participant_id",
  "run_timestamp",
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
  "source_participant_id",
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

# Select matching complete exports by run, applying identity corrections before
# checking participant uniqueness. Unexplained duplicates require an explicit decision.
files <- sort(list.files(INPUT_DIR, pattern = "^results_.*_b00_ALL\\.csv$", full.names = TRUE))
if (!length(files)) stop("No complete trial exports found")
trials <- apply_participant_identity(bind_rows(lapply(files, read_identity_csv)))
runs <- distinct(trials, participant_id, source_participant_id, run_timestamp)
manifest <- list()
for (kind in c("trials", "trust", "sliders")) {
  paths <- switch(kind, trials = files,
    trust = sub("_ALL.csv$", "_POSTBLOCK_ALL.csv", files),
    sliders = sub("_ALL.csv$", "_POSTBLOCK_SLIDERS_ALL.csv", files))
  if (!all(file.exists(paths))) stop("Missing same-run ", kind, " exports")
  parts <- lapply(paths, read_identity_csv)
  for (i in seq_along(parts)) {
    ids <- distinct(parts[[i]], participant_id, source_participant_id, run_timestamp)
    expected <- distinct(read_identity_csv(files[i]), participant_id, source_participant_id, run_timestamp)
    if (nrow(ids) != 1 || !identical(ids, expected)) stop("Mismatched run: ", paths[i])
    manifest[[length(manifest) + 1L]] <- mutate(ids, kind = kind,
      path = normalizePath(paths[i]), md5 = unname(tools::md5sum(paths[i])), n_rows = nrow(parts[[i]]))
  }
  dat <- apply_participant_identity(bind_rows(parts))
  if (kind == "trials") {
    write_csv(select(dat, all_of(TRIAL_COLUMNS), any_of(OPTIONAL_TIMING_COLUMNS)), file.path(OUTPUT_DIR, "data_virus_all.csv"))
    write_csv(select(dat, all_of(TRIAL_SUMMARY_COLUMNS), any_of(OPTIONAL_TIMING_COLUMNS)), file.path(OUTPUT_DIR, "data_virus.csv"))
  } else {
    columns <- if (kind == "trust") POSTBLOCK_COLUMNS else SLIDER_COLUMNS
    filename <- if (kind == "trust") "data_virus_postblock_all.csv" else "data_virus_sliders_all.csv"
    write_csv(select(dat, all_of(columns)), file.path(OUTPUT_DIR, filename))
  }
  message(kind, ": ", n_distinct(dat$participant_id), " participants, ", nrow(dat), " rows")
}
write_csv(bind_rows(manifest), file.path(OUTPUT_DIR, "input_manifest.csv"))
sources <- normalizePath(c(script, file.path(root, "R/participant_identity.R"), IDENTITY_CORRECTIONS_FILE))
write_csv(tibble(path = sources, md5 = unname(tools::md5sum(sources))), file.path(OUTPUT_DIR, "source_checksums.csv"))
