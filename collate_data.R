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


# Select a single run per participant before reading any export family. File
# modification times change during copying and are not evidence of run order.
export_patterns <- c(
  trials = "^results_.*_b00_ALL\\.csv$",
  questionnaire = "^results_.*_b00_POSTBLOCK_ALL\\.csv$",
  sliders = "^results_.*_b00_POSTBLOCK_SLIDERS_ALL\\.csv$"
)
source_files <- bind_rows(lapply(names(export_patterns), function(kind) {
  paths <- list.files(INPUT_DIR, pattern = export_patterns[[kind]], full.names = TRUE)
  tibble(path = paths, export_type = rep(kind, length(paths)))
})) %>%
  mutate(
    file_name = basename(path),
    participant_key = str_match(file_name, "^results_(p[0-9]+)_")[, 2],
    participant_id = as.integer(sub("^p", "", participant_key)),
    run_timestamp = str_match(file_name, "_([0-9]{8}_[0-9]{6})_b00_")[, 2]
  )
if (!nrow(source_files) || anyNA(source_files$participant_id) ||
    anyNA(source_files$run_timestamp)) {
  stop("Missing exports or invalid participant/run filename.", call. = FALSE)
}
selected_runs <- source_files %>%
  group_by(participant_id) %>%
  summarise(run_timestamp = max(run_timestamp), .groups = "drop")
selected_files <- source_files %>%
  inner_join(selected_runs, by = c("participant_id", "run_timestamp"))
run_counts <- selected_files %>% count(participant_id, export_type)
if (nrow(run_counts) != nrow(selected_runs) * 3L || any(run_counts$n != 1L)) {
  stop("Each latest run must have exactly one trial, questionnaire and slider export; no fallback to older runs.", call. = FALSE)
}
if (any(selected_runs$participant_id == 59L &
        selected_runs$run_timestamp == "20260903_120309")) {
  stop("The earlier p59 run was excluded for chance performance; supply replacement 20260924_110831.", call. = FALSE)
}
collation_manifest <- selected_files %>%
  transmute(participant_id, run_timestamp, export_type,
            source_file = normalizePath(path),
            source_md5 = unname(tools::md5sum(path))) %>%
  arrange(participant_id, export_type)

read_latest_participant_csvs <- function(pattern, label) {
  selected <- selected_files %>% filter(grepl(pattern, file_name))
  if (nrow(selected) != nrow(selected_runs)) {
    stop("Missing selected-run ", label, " export.", call. = FALSE)
  }
  bind_rows(lapply(seq_len(nrow(selected)), function(i) {
    row <- selected[i, ]
    data <- read_csv(row$path, show_col_types = FALSE)
    if (!"participant_id" %in% names(data) || !nrow(data) ||
        anyNA(data$participant_id) || any(data$participant_id != row$participant_id)) {
      stop("Participant mismatch in ", row$path, call. = FALSE)
    }
    if ("run_timestamp" %in% names(data) &&
        (anyNA(data$run_timestamp) || any(data$run_timestamp != row$run_timestamp))) {
      stop("Run timestamp mismatch in ", row$path, call. = FALSE)
    }
    data$run_timestamp <- row$run_timestamp
    data
  }))
}


ensure_design_current_columns <- function(dat) {
  if (!"condition_code" %in% names(dat) && "condition_deadline_code" %in% names(dat)) {
    dat <- dat %>%
      mutate(condition_code = condition_deadline_code)
  }

  if (!"trial_deadline_s" %in% names(dat) && "trial_deadline_ms" %in% names(dat)) {
    dat <- dat %>%
      mutate(trial_deadline_s = trial_deadline_ms / 1000)
  }

  dat
}


ensure_trial_current_columns <- function(dat) {
  if (!"trial" %in% names(dat) && "trial_idx" %in% names(dat)) {
    dat <- dat %>%
      mutate(trial = trial_idx)
  }

  if (!"decision1_response" %in% names(dat) && "initial_response" %in% names(dat)) {
    dat <- dat %>%
      mutate(decision1_response = initial_response)
  }

  if (!"decision1_correct" %in% names(dat) && "initial_correct" %in% names(dat)) {
    dat <- dat %>%
      mutate(decision1_correct = initial_correct)
  }

  if (!"decision1_rt_s" %in% names(dat) && "initial_rt_s" %in% names(dat)) {
    dat <- dat %>%
      mutate(decision1_rt_s = initial_rt_s)
  }

  if (!"decision2_response" %in% names(dat) && "final_response" %in% names(dat)) {
    dat <- dat %>%
      mutate(decision2_response = final_response)
  }

  if (!"decision2_correct" %in% names(dat) && "final_correct" %in% names(dat)) {
    dat <- dat %>%
      mutate(decision2_correct = final_correct)
  }

  if (!"decision2_correct" %in% names(dat) && "correct" %in% names(dat)) {
    dat <- dat %>%
      mutate(decision2_correct = correct)
  }

  if (!"decision2_rt_s" %in% names(dat) && "final_rt_s" %in% names(dat)) {
    dat <- dat %>%
      mutate(decision2_rt_s = final_rt_s)
  }

  if (!"decision2_rt_s" %in% names(dat) && "rt_s" %in% names(dat)) {
    dat <- dat %>%
      mutate(decision2_rt_s = rt_s)
  }

  dat
}


ensure_slider_current_columns <- function(dat) {
  if (!"question_key" %in% names(dat) && "slider_key" %in% names(dat)) {
    dat <- dat %>%
      mutate(question_key = slider_key)
  }

  if (!"response_percent" %in% names(dat) && "response" %in% names(dat)) {
    dat <- dat %>%
      mutate(response_percent = response)
  }

  dat
}


select_current_cols <- function(dat, cols) {
  for (col in setdiff(cols, names(dat))) {
    dat[[col]] <- rep(NA, nrow(dat))
  }

  dat %>%
    select(all_of(cols))
}


current_trial_all_cols <- c(
  "participant_id",
  "run_timestamp",
  "key_black",
  "key_white",
  "keymap_flip",
  "block_idx",
  "condition_code",
  "aid_condition",
  "trial_deadline_s",
  "trial",
  "global_trial",
  "difficulty_mode",
  "staircase_target_accuracy",
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
  "stimulus",
  "aid_label",
  "aid_correct",
  "preview_display",
  "preview_label",
  "decision1_display",
  "decision1_response",
  "decision1_correct",
  "decision1_rt_s",
  "decision1_matches_aid",
  "decision2_preview_display",
  "decision2_preview_label",
  "decision2_display",
  "decision2_label",
  "decision2_response",
  "decision2_correct",
  "decision2_rt_s",
  "decision2_matches_aid",
  "changed_response",
  "feedback"
)

current_trial_summary_cols <- c(
  "participant_id",
  "key_black",
  "key_white",
  "keymap_flip",
  "block_idx",
  "condition_code",
  "aid_condition",
  "trial_deadline_s",
  "trial",
  "vblack_prop",
  "stimulus",
  "auto_on",
  "aid_accuracy_setting",
  "aid_label",
  "aid_correct",
  "preview_display",
  "preview_label",
  "decision1_display",
  "decision1_response",
  "decision1_correct",
  "decision1_rt_s",
  "decision1_matches_aid",
  "decision2_preview_display",
  "decision2_preview_label",
  "decision2_display",
  "decision2_label",
  "decision2_response",
  "decision2_correct",
  "decision2_rt_s",
  "decision2_matches_aid",
  "changed_response",
  "feedback"
)

current_postblock_cols <- c(
  "participant_id",
  "run_timestamp",
  "block_idx",
  "condition_code",
  "aid_condition",
  "trial_deadline_s",
  "question_idx",
  "question",
  "left_anchor",
  "right_anchor",
  "response",
  "scale_min",
  "scale_max"
)

current_slider_cols <- c(
  "participant_id",
  "run_timestamp",
  "block_idx",
  "condition_code",
  "aid_condition",
  "trial_deadline_s",
  "question_idx",
  "question_key",
  "question",
  "response_percent"
)


# 1) Collate all choice-RT results files ----------------------------------

dat <- read_latest_participant_csvs(
  "^results_.*_b00_ALL\\.csv$",
  "complete trial"
) %>%
  ensure_design_current_columns() %>%
  ensure_trial_current_columns()

trial_all <- dat %>%
  select_current_cols(current_trial_all_cols)

head(trial_all)
tail(trial_all)
str(trial_all)
length(unique(trial_all$participant_id))


trial_summary <- dat %>%
  select_current_cols(current_trial_summary_cols)

head(trial_summary)
tail(trial_summary)
str(trial_summary)


# 2) Collate post-block questionnaire files --------------------------------

postblock_all <- read_latest_participant_csvs(
  "^results_.*_b00_POSTBLOCK_ALL\\.csv$",
  "complete post-block questionnaire"
) %>%
  ensure_design_current_columns() %>%
  select_current_cols(current_postblock_cols)

head(postblock_all)
tail(postblock_all)
str(postblock_all)


# 3) Collate post-block accuracy slider files ------------------------------

slider_all <- read_latest_participant_csvs(
  "^results_.*_b00_POSTBLOCK_SLIDERS_ALL\\.csv$",
  "complete post-block slider"
) %>%
  ensure_design_current_columns() %>%
  ensure_slider_current_columns() %>%
  select_current_cols(current_slider_cols)

head(slider_all)
tail(slider_all)
str(slider_all)


# All source families have now passed participant/run checks.
write_csv(trial_all, file.path(OUTPUT_DIR, "data_virus_all.csv"))
write_csv(trial_summary, file.path(OUTPUT_DIR, "data_virus.csv"))
write_csv(postblock_all, file.path(OUTPUT_DIR, "data_virus_postblock_all.csv"))
write_csv(slider_all, file.path(OUTPUT_DIR, "data_virus_sliders_all.csv"))
write_csv(collation_manifest, file.path(OUTPUT_DIR, "collation_manifest.csv"))
