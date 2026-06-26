# Clear workspace
rm(list = ls())

# Load libraries
library("dplyr")
library("purrr")
library("readr")
library("stringr")
library("tibble")


read_latest_participant_csvs <- function(pattern) {
  files <- list.files(
    "output",
    pattern = pattern,
    full.names = TRUE
  )

  file_tbl <- tibble(path = files) %>%
    mutate(
      file_name = basename(path),
      participant_id = str_extract(file_name, "(?<=results_)[^_]+"),
      mtime = file.info(path)$mtime
    )

  latest_per_participant <- file_tbl %>%
    group_by(participant_id) %>%
    slice_max(order_by = mtime, n = 1, with_ties = FALSE) %>%
    ungroup()

  print(latest_per_participant %>% select(participant_id, path, mtime))

  latest_per_participant %>%
    mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
    pull(data) %>%
    bind_rows()
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
  "block",
  "block_idx",
  "condition_code",
  "aid_condition",
  "trial_deadline_s",
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
  "block",
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
  "block",
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
  "block",
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

dat <- read_latest_participant_csvs("^results_.*_b00_ALL\\.csv$") %>%
  ensure_design_current_columns() %>%
  ensure_trial_current_columns()

trial_all <- dat %>%
  select_current_cols(current_trial_all_cols)

head(trial_all)
tail(trial_all)
str(trial_all)
length(unique(trial_all$participant_id))

write_csv(trial_all, "data/data_virus_all.csv")

trial_summary <- dat %>%
  select_current_cols(current_trial_summary_cols)

head(trial_summary)
tail(trial_summary)
str(trial_summary)

write_csv(trial_summary, "data/data_virus.csv")


# 2) Collate post-block questionnaire files --------------------------------

postblock_all <- read_latest_participant_csvs("^results_.*_b00_POSTBLOCK_ALL\\.csv$") %>%
  ensure_design_current_columns() %>%
  select_current_cols(current_postblock_cols)

head(postblock_all)
tail(postblock_all)
str(postblock_all)

write_csv(postblock_all, "data/data_virus_postblock_all.csv")


# 3) Collate post-block accuracy slider files ------------------------------

slider_all <- read_latest_participant_csvs("^results_.*_b00_POSTBLOCK_SLIDERS_ALL\\.csv$") %>%
  ensure_design_current_columns() %>%
  ensure_slider_current_columns() %>%
  select_current_cols(current_slider_cols)

head(slider_all)
tail(slider_all)
str(slider_all)

write_csv(slider_all, "data/data_virus_sliders_all.csv")
