# Create a hypothesis-ready dataset with one row per participant.
#
# Usage:
#   Rscript create_averaged_within_participants.R [input_dir] [output_csv]
#
# Defaults:
#   input_dir  = data
#   output_csv = data/semester2_2026_averaged_within_participants.csv

rm(list = ls())

library("dplyr")
library("readr")
library("tidyr")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 2) {
  stop(
    "Usage: Rscript create_averaged_within_participants.R [input_dir] [output_csv]",
    call. = FALSE
  )
}

INPUT_DIR <- if (length(args) >= 1) args[[1]] else "data"
OUTPUT_CSV <- if (length(args) >= 2) {
  args[[2]]
} else {
  file.path(INPUT_DIR, "semester2_2026_averaged_within_participants.csv")
}

TRIAL_FILE <- file.path(INPUT_DIR, "data_virus_all.csv")
POSTBLOCK_FILE <- file.path(INPUT_DIR, "data_virus_postblock_all.csv")
SLIDER_FILE <- file.path(INPUT_DIR, "data_virus_sliders_all.csv")

required_files <- c(TRIAL_FILE, POSTBLOCK_FILE, SLIDER_FILE)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop(
    "Required input file(s) not found: ",
    paste(missing_files, collapse = ", "),
    call. = FALSE
  )
}

require_columns <- function(data, columns, label) {
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns)) {
    stop(
      label,
      " is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

as_binary <- function(x, label) {
  if (is.logical(x)) {
    out <- as.integer(x)
  } else if (is.numeric(x)) {
    out <- as.numeric(x)
  } else {
    values <- toupper(trimws(as.character(x)))
    out <- dplyr::case_when(
      values %in% c("TRUE", "T", "1") ~ 1,
      values %in% c("FALSE", "F", "0") ~ 0,
      is.na(values) | values == "" ~ NA_real_,
      TRUE ~ NA_real_
    )
  }

  invalid <- !is.na(x) & (is.na(out) | !out %in% c(0, 1))
  if (any(invalid)) {
    stop(label, " contains values other than TRUE/FALSE or 1/0.", call. = FALSE)
  }

  out
}

assert_id_sets_match <- function(reference_ids, candidate_ids, label) {
  if (!setequal(reference_ids, candidate_ids)) {
    missing_from_candidate <- setdiff(reference_ids, candidate_ids)
    extra_in_candidate <- setdiff(candidate_ids, reference_ids)
    stop(
      "Participant IDs in ", label, " do not match the trial data. ",
      "Missing: ", paste(missing_from_candidate, collapse = ", "), "; ",
      "extra: ", paste(extra_in_candidate, collapse = ", "), ".",
      call. = FALSE
    )
  }
}

assert_range <- function(data, columns, lower, upper, label) {
  bad <- vapply(
    columns,
    function(column) {
      values <- data[[column]]
      any(is.na(values) | !is.finite(values) | values < lower | values > upper)
    },
    logical(1)
  )

  if (any(bad)) {
    stop(
      label,
      " column(s) contain missing or out-of-range values: ",
      paste(columns[bad], collapse = ", "),
      call. = FALSE
    )
  }
}

trials <- read_csv(TRIAL_FILE, show_col_types = FALSE)
postblock <- read_csv(POSTBLOCK_FILE, show_col_types = FALSE)
sliders <- read_csv(SLIDER_FILE, show_col_types = FALSE)

require_columns(
  trials,
  c(
    "participant_id",
    "aid_condition",
    "aid_correct",
    "decision2_correct",
    "changed_response"
  ),
  "Trial data"
)
require_columns(
  postblock,
  c("participant_id", "aid_condition", "question", "response", "scale_min", "scale_max"),
  "Post-block questionnaire data"
)
require_columns(
  sliders,
  c("participant_id", "aid_condition", "question_key", "response_percent"),
  "Post-block slider data"
)

expected_conditions <- c("manual", "aid_first", "stimulus_first")
observed_conditions <- sort(unique(trials$aid_condition))
if (!setequal(observed_conditions, expected_conditions)) {
  stop(
    "Trial data must contain exactly these aid conditions: ",
    paste(expected_conditions, collapse = ", "),
    ". Observed: ", paste(observed_conditions, collapse = ", "), ".",
    call. = FALSE
  )
}

participant_ids <- sort(unique(trials$participant_id))
if (!length(participant_ids) || any(is.na(participant_ids))) {
  stop("Trial data contain no participants or missing participant IDs.", call. = FALSE)
}

assert_id_sets_match(participant_ids, unique(postblock$participant_id), "post-block questionnaire data")
assert_id_sets_match(participant_ids, unique(sliders$participant_id), "post-block slider data")

trial_counts <- trials %>%
  count(participant_id, aid_condition, name = "n_trials")

if (
  nrow(trial_counts) != length(participant_ids) * length(expected_conditions) ||
    any(trial_counts$n_trials != 260)
) {
  bad_counts <- trial_counts %>%
    filter(n_trials != 260)
  stop(
    "Expected exactly 260 trials per participant and main condition. ",
    "Nonconforming cells: ",
    if (nrow(bad_counts)) paste0(nrow(bad_counts)) else "missing condition cells",
    ".",
    call. = FALSE
  )
}

trials <- trials %>%
  mutate(
    changed_response_num = as_binary(changed_response, "changed_response"),
    decision2_correct_num = as_binary(decision2_correct, "decision2_correct"),
    aid_correct_num = as_binary(aid_correct, "aid_correct"),
    condition_suffix = recode(
      aid_condition,
      manual = "manual",
      aid_first = "aid_first",
      stimulus_first = "stimulus_first"
    )
  )

if (any(is.na(trials$changed_response_num)) || any(is.na(trials$decision2_correct_num))) {
  stop("Switching and Decision 2 accuracy must be observed on every trial.", call. = FALSE)
}

if (
  any(!is.na(trials$aid_correct_num[trials$aid_condition == "manual"])) ||
    any(is.na(trials$aid_correct_num[trials$aid_condition != "manual"]))
) {
  stop(
    "aid_correct must be missing for Manual trials and observed for all aided trials.",
    call. = FALSE
  )
}

aid_outcome_counts <- trials %>%
  filter(aid_condition != "manual") %>%
  count(participant_id, aid_condition, aid_correct_num)

if (
  nrow(aid_outcome_counts) != length(participant_ids) * 2 * 2 ||
    any(aid_outcome_counts$n < 1)
) {
  stop(
    "Every participant must have both correct- and incorrect-aid trials in each aided condition.",
    call. = FALSE
  )
}

switch_wide <- trials %>%
  group_by(participant_id, condition_suffix) %>%
  summarise(value = mean(changed_response_num), .groups = "drop") %>%
  pivot_wider(
    names_from = condition_suffix,
    values_from = value,
    names_glue = "switch_proportion_{condition_suffix}"
  )

overall_accuracy_wide <- trials %>%
  group_by(participant_id, condition_suffix) %>%
  summarise(value = mean(decision2_correct_num), .groups = "drop") %>%
  mutate(
    metric = case_when(
      condition_suffix == "manual" ~ "decision2_accuracy_proportion_manual",
      TRUE ~ paste0("decision2_accuracy_proportion_", condition_suffix, "_overall")
    )
  ) %>%
  select(participant_id, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value)

aid_outcome_accuracy_wide <- trials %>%
  filter(aid_condition != "manual") %>%
  mutate(
    aid_outcome_suffix = if_else(
      aid_correct_num == 1,
      "aid_correct",
      "aid_incorrect"
    )
  ) %>%
  group_by(participant_id, condition_suffix, aid_outcome_suffix) %>%
  summarise(value = mean(decision2_correct_num), .groups = "drop") %>%
  pivot_wider(
    names_from = c(condition_suffix, aid_outcome_suffix),
    values_from = value,
    names_glue = "decision2_accuracy_proportion_{condition_suffix}_{aid_outcome_suffix}"
  )

slider_cells <- sliders %>%
  filter(
    (aid_condition == "manual" & question_key == "perc_self_correct") |
      (aid_condition %in% c("aid_first", "stimulus_first") &
        question_key == "perc_auto_correct")
  ) %>%
  mutate(
    metric = case_when(
      aid_condition == "manual" ~ "manual_reliability_rating_pct",
      aid_condition == "aid_first" ~ "aid_first_reliability_rating_pct",
      aid_condition == "stimulus_first" ~ "stimulus_first_reliability_rating_pct"
    )
  ) %>%
  count(participant_id, metric, name = "n_responses")

if (
  nrow(slider_cells) != length(participant_ids) * 3 ||
    any(slider_cells$n_responses != 1)
) {
  stop(
    "Expected one Manual self-reliability rating and one aid-reliability rating ",
    "in each aided condition per participant.",
    call. = FALSE
  )
}

reliability_wide <- sliders %>%
  filter(
    (aid_condition == "manual" & question_key == "perc_self_correct") |
      (aid_condition %in% c("aid_first", "stimulus_first") &
        question_key == "perc_auto_correct")
  ) %>%
  mutate(
    metric = case_when(
      aid_condition == "manual" ~ "manual_reliability_rating_pct",
      aid_condition == "aid_first" ~ "aid_first_reliability_rating_pct",
      aid_condition == "stimulus_first" ~ "stimulus_first_reliability_rating_pct"
    )
  ) %>%
  select(participant_id, metric, response_percent) %>%
  pivot_wider(names_from = metric, values_from = response_percent) %>%
  mutate(
    aid_reliability_rating_mean_pct = rowMeans(
      pick(
        aid_first_reliability_rating_pct,
        stimulus_first_reliability_rating_pct
      )
    )
  )

trust_rows <- postblock %>%
  filter(aid_condition %in% c("aid_first", "stimulus_first"))

trust_counts <- trust_rows %>%
  group_by(participant_id, aid_condition) %>%
  summarise(
    n_rows = n(),
    n_questions = n_distinct(question),
    n_observed = sum(!is.na(response)),
    scale_minimum = min(scale_min, na.rm = TRUE),
    scale_maximum = max(scale_max, na.rm = TRUE),
    .groups = "drop"
  )

if (
  nrow(trust_counts) != length(participant_ids) * 2 ||
    any(trust_counts$n_rows != 6) ||
    any(trust_counts$n_questions != 6) ||
    any(trust_counts$n_observed != 6) ||
    any(trust_counts$scale_minimum != 1) ||
    any(trust_counts$scale_maximum != 5)
) {
  stop(
    "Expected six distinct, observed 1-to-5 trust items in each aided condition per participant.",
    call. = FALSE
  )
}

trust_wide <- trust_rows %>%
  mutate(
    condition_suffix = recode(
      aid_condition,
      aid_first = "aid_first",
      stimulus_first = "stimulus_first"
    )
  ) %>%
  group_by(participant_id, condition_suffix) %>%
  summarise(value = mean(response), .groups = "drop") %>%
  pivot_wider(
    names_from = condition_suffix,
    values_from = value,
    names_glue = "trust_mean_{condition_suffix}_1to5"
  )

participant_output <- tibble(participant_id = participant_ids) %>%
  left_join(reliability_wide, by = "participant_id") %>%
  left_join(switch_wide, by = "participant_id") %>%
  left_join(overall_accuracy_wide, by = "participant_id") %>%
  left_join(aid_outcome_accuracy_wide, by = "participant_id") %>%
  left_join(trust_wide, by = "participant_id") %>%
  mutate(subject_no = suppressWarnings(as.integer(participant_id)), .before = 1)

if (
  any(is.na(participant_output$subject_no)) ||
    n_distinct(participant_output$subject_no) != length(participant_ids)
) {
  stop("participant_id values must map uniquely to integer subject numbers.", call. = FALSE)
}

participant_output <- participant_output %>%
  select(
    subject_no,
    manual_reliability_rating_pct,
    aid_first_reliability_rating_pct,
    stimulus_first_reliability_rating_pct,
    aid_reliability_rating_mean_pct,
    switch_proportion_manual,
    switch_proportion_aid_first,
    switch_proportion_stimulus_first,
    decision2_accuracy_proportion_manual,
    decision2_accuracy_proportion_aid_first_aid_correct,
    decision2_accuracy_proportion_stimulus_first_aid_correct,
    decision2_accuracy_proportion_aid_first_aid_incorrect,
    decision2_accuracy_proportion_stimulus_first_aid_incorrect,
    decision2_accuracy_proportion_aid_first_overall,
    decision2_accuracy_proportion_stimulus_first_overall,
    trust_mean_aid_first_1to5,
    trust_mean_stimulus_first_1to5
  ) %>%
  arrange(subject_no)

if (
  nrow(participant_output) != length(participant_ids) ||
    n_distinct(participant_output$subject_no) != nrow(participant_output) ||
    ncol(participant_output) != 17
) {
  stop("The participant-level output does not have the expected dimensions.", call. = FALSE)
}

assert_range(
  participant_output,
  c(
    "manual_reliability_rating_pct",
    "aid_first_reliability_rating_pct",
    "stimulus_first_reliability_rating_pct",
    "aid_reliability_rating_mean_pct"
  ),
  0,
  100,
  "Reliability rating"
)

assert_range(
  participant_output,
  grep("^(switch|decision2_accuracy)_proportion", names(participant_output), value = TRUE),
  0,
  1,
  "Behavioural proportion"
)

assert_range(
  participant_output,
  c("trust_mean_aid_first_1to5", "trust_mean_stimulus_first_1to5"),
  1,
  5,
  "Trust mean"
)

dir.create(dirname(OUTPUT_CSV), recursive = TRUE, showWarnings = FALSE)
write_csv(participant_output, OUTPUT_CSV)

message(
  "Wrote ", nrow(participant_output), " participants and ",
  ncol(participant_output), " columns to ", OUTPUT_CSV
)
