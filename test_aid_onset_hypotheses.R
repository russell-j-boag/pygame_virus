# Test the preregistered aid-onset hypotheses using primary trial-level
# logistic mixed models and participant-level paired-test sensitivity checks.
#
# Usage:
#   Rscript test_aid_onset_hypotheses.R \
#     [participant_csv] [trial_csv] [output_dir]
#
# Defaults:
#   participant_csv = data/semester2_2026_averaged_within_participants.csv
#   trial_csv       = data/data_virus_all.csv
#   output_dir      = analysis_outputs/semester2_2026_hypotheses

rm(list = ls())

required_packages <- c("dplyr", "emmeans", "lme4", "readr", "tidyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Required R package(s) not installed: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library("dplyr")
library("emmeans")
library("lme4")
library("readr")
library("tidyr")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 3) {
  stop(
    paste(
      "Usage: Rscript test_aid_onset_hypotheses.R",
      "[participant_csv] [trial_csv] [output_dir]"
    ),
    call. = FALSE
  )
}

PARTICIPANT_CSV <- if (length(args) >= 1) {
  args[[1]]
} else {
  "data/semester2_2026_averaged_within_participants.csv"
}

TRIAL_CSV <- if (length(args) >= 2) {
  args[[2]]
} else {
  "data/data_virus_all.csv"
}

OUTPUT_DIR <- if (length(args) >= 3) {
  args[[3]]
} else {
  "analysis_outputs/semester2_2026_hypotheses"
}

ALPHA <- 0.05
EXPECTED_INPUT_PARTICIPANTS <- 60L
EXCLUDED_SUBJECTS <- 59L
EXPECTED_PARTICIPANTS <- EXPECTED_INPUT_PARTICIPANTS - length(EXCLUDED_SUBJECTS)
EXPECTED_TRIALS_PER_CONDITION <- 260L
CONDITION_LEVELS <- c("Manual", "Aid first", "Stimulus first")
TRUST_BOOTSTRAP_REPS <- 10000L
TRUST_BOOTSTRAP_SEED <- 20260908L
TRUST_SWITCH_BOOTSTRAP_SEED <- 20260909L
TRUST_HARMFUL_SWITCH_BOOTSTRAP_SEED <- 20260910L

if (!file.exists(PARTICIPANT_CSV)) {
  stop("Participant-level CSV not found: ", PARTICIPANT_CSV, call. = FALSE)
}
if (!file.exists(TRIAL_CSV)) {
  stop("Trial-level CSV not found: ", TRIAL_CSV, call. = FALSE)
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
    out <- case_when(
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

assert_range <- function(data, columns, lower, upper, label) {
  invalid <- vapply(
    columns,
    function(column) {
      values <- data[[column]]
      any(is.na(values) | !is.finite(values) | values < lower | values > upper)
    },
    logical(1)
  )

  if (any(invalid)) {
    stop(
      label,
      " column(s) contain missing or out-of-range values: ",
      paste(columns[invalid], collapse = ", "),
      call. = FALSE
    )
  }
}

first_existing_column <- function(data, candidates, label) {
  selected <- candidates[candidates %in% names(data)]
  if (!length(selected)) {
    stop(
      "Could not find ", label, ". Tried: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  selected[[1]]
}

format_p <- function(p) {
  if (!is.finite(p)) {
    return("NA")
  }
  if (p < 0.001) {
    return("< .001")
  }
  paste0("= ", formatC(p, format = "f", digits = 3))
}

participant_columns <- c(
  "subject_no",
  "manual_reliability_rating_pct",
  "aid_first_reliability_rating_pct",
  "stimulus_first_reliability_rating_pct",
  "aid_reliability_rating_mean_pct",
  "switch_proportion_manual",
  "switch_proportion_aid_first",
  "switch_proportion_stimulus_first",
  "decision2_accuracy_proportion_manual",
  "decision2_accuracy_proportion_aid_first_aid_correct",
  "decision2_accuracy_proportion_stimulus_first_aid_correct",
  "decision2_accuracy_proportion_aid_first_aid_incorrect",
  "decision2_accuracy_proportion_stimulus_first_aid_incorrect",
  "decision2_accuracy_proportion_aid_first_overall",
  "decision2_accuracy_proportion_stimulus_first_overall",
  "trust_mean_aid_first_1to5",
  "trust_mean_stimulus_first_1to5"
)

participant_data <- read_csv(PARTICIPANT_CSV, show_col_types = FALSE)
trial_data_raw <- read_csv(TRIAL_CSV, show_col_types = FALSE)

require_columns(participant_data, participant_columns, "Participant-level data")
if (ncol(participant_data) != length(participant_columns)) {
  stop(
    "Participant-level data must contain exactly the expected 17 columns.",
    call. = FALSE
  )
}

trial_columns <- c(
  "participant_id",
  "aid_condition",
  "aid_correct",
  "decision1_correct",
  "decision2_correct",
  "changed_response"
)
require_columns(trial_data_raw, trial_columns, "Trial-level data")

if (
  nrow(participant_data) != EXPECTED_INPUT_PARTICIPANTS ||
    n_distinct(participant_data$subject_no) != EXPECTED_INPUT_PARTICIPANTS ||
    any(is.na(participant_data$subject_no))
) {
  stop(
    "Expected exactly 60 unique, observed participants in the participant-level data.",
    call. = FALSE
  )
}

expected_input_subjects <- seq_len(EXPECTED_INPUT_PARTICIPANTS)
if (!identical(sort(as.integer(participant_data$subject_no)), expected_input_subjects)) {
  stop("Expected subject_no to contain the integers 1 through 60.", call. = FALSE)
}

assert_range(
  participant_data,
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

participant_proportion_columns <- grep(
  "^(switch|decision2_accuracy)_proportion",
  names(participant_data),
  value = TRUE
)
assert_range(
  participant_data,
  participant_proportion_columns,
  0,
  1,
  "Participant-level behavioural proportion"
)
assert_range(
  participant_data,
  c("trust_mean_aid_first_1to5", "trust_mean_stimulus_first_1to5"),
  1,
  5,
  "Trust mean"
)

if (
  !isTRUE(all.equal(
    participant_data$aid_reliability_rating_mean_pct,
    rowMeans(participant_data[
      c(
        "aid_first_reliability_rating_pct",
        "stimulus_first_reliability_rating_pct"
      )
    ]),
    tolerance = 1e-12,
    check.attributes = FALSE
  ))
) {
  stop(
    "aid_reliability_rating_mean_pct is not the equal mean of the two aided ratings.",
    call. = FALSE
  )
}

if (!all(EXCLUDED_SUBJECTS %in% participant_data$subject_no)) {
  stop("Requested excluded participant(s) are absent from the input data.", call. = FALSE)
}

participant_data <- participant_data %>%
  filter(!subject_no %in% EXCLUDED_SUBJECTS)
expected_subjects <- setdiff(expected_input_subjects, EXCLUDED_SUBJECTS)

if (
  nrow(participant_data) != EXPECTED_PARTICIPANTS ||
    !identical(sort(as.integer(participant_data$subject_no)), expected_subjects)
) {
  stop("Participant exclusion did not produce the expected analysis sample.", call. = FALSE)
}

analysis_exclusions <- tibble(
  excluded_subject_no = EXCLUDED_SUBJECTS,
  reason = "Excluded at user request",
  input_participants = EXPECTED_INPUT_PARTICIPANTS,
  analysed_participants = EXPECTED_PARTICIPANTS
)

condition_labels <- c(
  manual = "Manual",
  aid_first = "Aid first",
  stimulus_first = "Stimulus first"
)

if (
  nrow(trial_data_raw) !=
    EXPECTED_INPUT_PARTICIPANTS * 3L * EXPECTED_TRIALS_PER_CONDITION
) {
  stop("Expected exactly 46,800 trials in the unfiltered trial input.", call. = FALSE)
}

trial_data <- trial_data_raw %>%
  mutate(
    subject_no = suppressWarnings(as.integer(participant_id))
  ) %>%
  filter(!subject_no %in% EXCLUDED_SUBJECTS) %>%
  mutate(
    subject = factor(subject_no, levels = expected_subjects),
    condition = factor(
      unname(condition_labels[aid_condition]),
      levels = CONDITION_LEVELS
    ),
    changed = as_binary(changed_response, "changed_response"),
    decision1_accuracy = as_binary(decision1_correct, "decision1_correct"),
    accuracy = as_binary(decision2_correct, "decision2_correct"),
    aid_correct_num = as_binary(aid_correct, "aid_correct")
  )

if (
  nrow(trial_data) != EXPECTED_PARTICIPANTS * 3L * EXPECTED_TRIALS_PER_CONDITION ||
    any(is.na(trial_data$subject_no)) ||
    any(is.na(trial_data$subject)) ||
    any(is.na(trial_data$condition)) ||
    any(is.na(trial_data$changed)) ||
    any(is.na(trial_data$decision1_accuracy)) ||
    any(is.na(trial_data$accuracy))
) {
  stop(
    "Filtered trial-level data must contain complete trials for the expected analysis sample ",
    "in all three conditions.",
    call. = FALSE
  )
}

harmful_switch_participant_long <- trial_data %>%
  filter(condition != "Manual", aid_correct_num == 0) %>%
  group_by(subject_no, condition) %>%
  summarise(
    n_incorrect_advice_trials = n(),
    n_initially_correct = sum(decision1_accuracy),
    n_harmful_switches = sum(decision1_accuracy == 1 & accuracy == 0),
    harmful_switch_proportion = n_harmful_switches / n_initially_correct,
    .groups = "drop"
  )

if (
  nrow(harmful_switch_participant_long) != EXPECTED_PARTICIPANTS * 2L ||
    any(harmful_switch_participant_long$n_initially_correct < 1L) ||
    any(!is.finite(harmful_switch_participant_long$harmful_switch_proportion)) ||
    any(harmful_switch_participant_long$harmful_switch_proportion < 0) ||
    any(harmful_switch_participant_long$harmful_switch_proportion > 1)
) {
  stop(
    paste(
      "Conditional harmful-switch rates must be finite for every participant",
      "in both aided conditions."
    ),
    call. = FALSE
  )
}

harmful_switch_participant_wide <- harmful_switch_participant_long %>%
  mutate(
    condition_suffix = recode(
      as.character(condition),
      "Aid first" = "aid_first",
      "Stimulus first" = "stimulus_first"
    )
  ) %>%
  select(
    subject_no,
    condition_suffix,
    n_incorrect_advice_trials,
    n_initially_correct,
    n_harmful_switches,
    harmful_switch_proportion
  ) %>%
  pivot_wider(
    names_from = condition_suffix,
    values_from = c(
      n_incorrect_advice_trials,
      n_initially_correct,
      n_harmful_switches,
      harmful_switch_proportion
    ),
    names_glue = "{.value}_{condition_suffix}"
  )

participant_data <- participant_data %>%
  left_join(harmful_switch_participant_wide, by = "subject_no")

if (!setequal(unique(trial_data$subject_no), participant_data$subject_no)) {
  stop("Participant IDs do not align across the two input files.", call. = FALSE)
}

trial_counts <- trial_data %>%
  count(subject_no, condition, name = "n_trials")
if (
  nrow(trial_counts) != EXPECTED_PARTICIPANTS * 3L ||
    any(trial_counts$n_trials != EXPECTED_TRIALS_PER_CONDITION)
) {
  stop("Expected exactly 260 trials per participant and condition.", call. = FALSE)
}

if (
  any(!is.na(trial_data$aid_correct_num[trial_data$condition == "Manual"])) ||
    any(is.na(trial_data$aid_correct_num[trial_data$condition != "Manual"]))
) {
  stop(
    "aid_correct must be missing in Manual and observed in both aided conditions.",
    call. = FALSE
  )
}

aid_outcome_cells <- trial_data %>%
  filter(condition != "Manual") %>%
  count(subject_no, condition, aid_correct_num)
if (
  nrow(aid_outcome_cells) != EXPECTED_PARTICIPANTS * 2L * 2L ||
    any(aid_outcome_cells$n < 1L)
) {
  stop(
    "Every participant must have correct- and incorrect-aid trials in both aided conditions.",
    call. = FALSE
  )
}

make_paired_test <- function(
  data,
  hypothesis,
  outcome,
  contrast,
  first_column,
  second_column,
  estimate_scale,
  predicted_direction = "first > second",
  include_percentage_points = FALSE
) {
  first <- data[[first_column]]
  second <- data[[second_column]]
  complete <- is.finite(first) & is.finite(second)
  first <- first[complete]
  second <- second[complete]
  difference <- first - second

  if (length(difference) < 2L || !is.finite(sd(difference)) || sd(difference) == 0) {
    stop("Cannot run paired t-test for ", hypothesis, ": ", contrast, call. = FALSE)
  }

  test <- t.test(first, second, paired = TRUE, alternative = "two.sided")
  direction_matches <- if (predicted_direction == "first > second") {
    mean(difference) > 0
  } else {
    NA
  }

  tibble(
    hypothesis = hypothesis,
    analysis = "Paired t-test",
    outcome = outcome,
    contrast = contrast,
    first_variable = first_column,
    second_variable = second_column,
    n_participants = length(difference),
    mean_first = mean(first),
    mean_second = mean(second),
    estimate_scale = estimate_scale,
    estimate = mean(difference),
    estimate_percentage_points = if (include_percentage_points) {
      mean(difference) * 100
    } else {
      NA_real_
    },
    conf_low = unname(test$conf.int[[1]]),
    conf_high = unname(test$conf.int[[2]]),
    statistic = unname(test$statistic),
    df = unname(test$parameter),
    cohens_dz = mean(difference) / sd(difference),
    p_value_raw = test$p.value,
    p_value_adjusted = test$p.value,
    p_adjustment = "none",
    predicted_direction = predicted_direction,
    direction_matches = direction_matches,
    significant_adjusted = test$p.value < ALPHA,
    contrast_result = case_when(
      is.na(direction_matches) & test$p.value < ALPHA & mean(difference) > 0 ~
        "Significant difference; first condition higher",
      is.na(direction_matches) & test$p.value < ALPHA & mean(difference) < 0 ~
        "Significant difference; first condition lower",
      is.na(direction_matches) ~ "No significant difference",
      direction_matches & test$p.value < ALPHA ~ "Supports prediction",
      !direction_matches & test$p.value < ALPHA ~ "Significant opposite direction",
      TRUE ~ "Not significant"
    )
  )
}

fit_binary_hypothesis <- function(
  data,
  hypothesis,
  response,
  outcome,
  subset_description,
  contrast_definitions
) {
  formula <- as.formula(paste0(response, " ~ condition + (1 | subject)"))
  model <- glmer(
    formula,
    data = data,
    family = binomial(link = "logit"),
    nAGQ = 1,
    control = glmerControl(
      optimizer = "bobyqa",
      calc.derivs = TRUE,
      optCtrl = list(maxfun = 200000)
    )
  )

  convergence_messages <- model@optinfo$conv$lme4$messages
  optimizer_code <- model@optinfo$conv$opt
  converged <- is.null(convergence_messages) &&
    isTRUE(optimizer_code == 0) &&
    all(is.finite(fixef(model)))
  singular <- isSingular(model, tol = 1e-4)

  gradient <- model@optinfo$derivs$gradient
  max_abs_gradient <- if (is.null(gradient)) {
    NA_real_
  } else {
    max(abs(gradient))
  }

  pearson_residuals <- residuals(model, type = "pearson")
  pearson_chisq <- sum(pearson_residuals^2)
  pearson_df <- df.residual(model)
  pearson_ratio <- pearson_chisq / pearson_df
  pearson_p <- pchisq(pearson_chisq, df = pearson_df, lower.tail = FALSE)

  diagnostics <- tibble(
    hypothesis = hypothesis,
    outcome = outcome,
    subset = subset_description,
    n_trials = nrow(data),
    n_participants = n_distinct(data$subject),
    converged = converged,
    singular = singular,
    optimizer_code = optimizer_code,
    convergence_message = if (is.null(convergence_messages)) {
      ""
    } else {
      paste(convergence_messages, collapse = " | ")
    },
    max_absolute_gradient = max_abs_gradient,
    pearson_chisq = pearson_chisq,
    pearson_df = pearson_df,
    pearson_dispersion_ratio = pearson_ratio,
    pearson_overdispersion_p = pearson_p
  )

  if (!converged || singular) {
    stop(
      hypothesis,
      " model failed diagnostics: converged=", converged,
      ", singular=", singular,
      ".",
      call. = FALSE
    )
  }

  emmeans_link <- emmeans(model, ~ condition)
  emmeans_response <- as.data.frame(
    summary(
      emmeans_link,
      type = "response",
      infer = c(TRUE, TRUE),
      adjust = "none"
    )
  )

  probability_column <- first_existing_column(
    emmeans_response,
    c("prob", "response"),
    "model-estimated probability column"
  )
  lower_column <- first_existing_column(
    emmeans_response,
    c("asymp.LCL", "lower.CL"),
    "probability confidence-limit column"
  )
  upper_column <- first_existing_column(
    emmeans_response,
    c("asymp.UCL", "upper.CL"),
    "probability confidence-limit column"
  )

  observed_means <- data %>%
    group_by(condition) %>%
    summarise(observed_proportion = mean(.data[[response]]), .groups = "drop")

  probabilities <- emmeans_response %>%
    transmute(
      hypothesis = hypothesis,
      outcome = outcome,
      subset = subset_description,
      condition = as.character(condition),
      n_trials = nrow(data),
      n_participants = n_distinct(data$subject),
      model_estimated_probability = .data[[probability_column]],
      standard_error = SE,
      conf_low = .data[[lower_column]],
      conf_high = .data[[upper_column]]
    ) %>%
    left_join(observed_means, by = "condition")

  emmeans_contrasts <- contrast(
    emmeans_link,
    method = contrast_definitions,
    adjust = "none"
  )
  contrast_response <- as.data.frame(
    summary(
      emmeans_contrasts,
      type = "response",
      infer = c(TRUE, TRUE),
      adjust = "none"
    )
  )

  estimate_column <- first_existing_column(
    contrast_response,
    c("odds.ratio", "ratio"),
    "odds-ratio column"
  )
  lower_column <- first_existing_column(
    contrast_response,
    c("asymp.LCL", "lower.CL"),
    "contrast confidence-limit column"
  )
  upper_column <- first_existing_column(
    contrast_response,
    c("asymp.UCL", "upper.CL"),
    "contrast confidence-limit column"
  )
  statistic_column <- first_existing_column(
    contrast_response,
    c("z.ratio", "t.ratio"),
    "contrast test-statistic column"
  )

  tests <- contrast_response %>%
    transmute(
      hypothesis = hypothesis,
      analysis = "Trial-level logistic mixed model",
      outcome = outcome,
      contrast = as.character(contrast),
      first_variable = NA_character_,
      second_variable = NA_character_,
      n_participants = n_distinct(data$subject),
      mean_first = NA_real_,
      mean_second = NA_real_,
      estimate_scale = "Odds ratio",
      estimate = .data[[estimate_column]],
      estimate_percentage_points = NA_real_,
      conf_low = .data[[lower_column]],
      conf_high = .data[[upper_column]],
      statistic = .data[[statistic_column]],
      df = df,
      cohens_dz = NA_real_,
      p_value_raw = p.value,
      p_value_adjusted = p.adjust(p.value, method = "holm"),
      p_adjustment = "Holm within hypothesis",
      predicted_direction = "first > second",
      direction_matches = .data[[estimate_column]] > 1,
      significant_adjusted = p.adjust(p.value, method = "holm") < ALPHA,
      contrast_result = case_when(
        .data[[estimate_column]] > 1 &
          p.adjust(p.value, method = "holm") < ALPHA ~ "Supports prediction",
        .data[[estimate_column]] < 1 &
          p.adjust(p.value, method = "holm") < ALPHA ~
            "Significant opposite direction",
        TRUE ~ "Not significant"
      )
    )

  list(
    model = model,
    diagnostics = diagnostics,
    probabilities = probabilities,
    tests = tests
  )
}

common_contrasts <- list(
  "Stimulus first - Aid first" = c(0, -1, 1),
  "Stimulus first - Manual" = c(-1, 0, 1),
  "Aid first - Manual" = c(-1, 1, 0)
)

h4_contrasts <- list(
  "Manual - Stimulus first" = c(1, 0, -1),
  "Manual - Aid first" = c(1, -1, 0),
  "Stimulus first - Aid first" = c(0, -1, 1)
)

h1_test <- make_paired_test(
  participant_data,
  hypothesis = "H1",
  outcome = "Perceived reliability rating",
  contrast = "Mean aid reliability - Manual reliability",
  first_column = "aid_reliability_rating_mean_pct",
  second_column = "manual_reliability_rating_pct",
  estimate_scale = "Rating percentage-point difference"
)

h2 <- fit_binary_hypothesis(
  trial_data,
  hypothesis = "H2",
  response = "changed",
  outcome = "Changed response from Decision 1 to Decision 2",
  subset_description = "All trials",
  contrast_definitions = common_contrasts
)

h3_data <- trial_data %>%
  filter(condition == "Manual" | aid_correct_num == 1)
h3 <- fit_binary_hypothesis(
  h3_data,
  hypothesis = "H3",
  response = "accuracy",
  outcome = "Decision 2 accuracy",
  subset_description = "All Manual trials plus correct-advice aided trials",
  contrast_definitions = common_contrasts
)

h4_data <- trial_data %>%
  filter(condition == "Manual" | aid_correct_num == 0)
h4 <- fit_binary_hypothesis(
  h4_data,
  hypothesis = "H4",
  response = "accuracy",
  outcome = "Decision 2 accuracy",
  subset_description = "All Manual trials plus incorrect-advice aided trials",
  contrast_definitions = h4_contrasts
)

h5 <- fit_binary_hypothesis(
  trial_data,
  hypothesis = "H5",
  response = "accuracy",
  outcome = "Decision 2 accuracy",
  subset_description = "All trials",
  contrast_definitions = common_contrasts
)

exploratory_test <- make_paired_test(
  participant_data,
  hypothesis = "Exploratory",
  outcome = "Six-item mean trust rating",
  contrast = "Stimulus first - Aid first",
  first_column = "trust_mean_stimulus_first_1to5",
  second_column = "trust_mean_aid_first_1to5",
  estimate_scale = "Trust-scale point difference",
  predicted_direction = "two-sided difference"
)

harmful_switch_condition_test <- make_paired_test(
  participant_data,
  hypothesis = "Exploratory follow-up",
  outcome = paste(
    "Harmful-switch proportion among initially correct,",
    "incorrect-advice trials"
  ),
  contrast = "Stimulus first - Aid first",
  first_column = "harmful_switch_proportion_stimulus_first",
  second_column = "harmful_switch_proportion_aid_first",
  estimate_scale = "Conditional proportion difference",
  predicted_direction = "two-sided difference",
  include_percentage_points = TRUE
)

primary_tests <- bind_rows(
  h1_test,
  h2$tests,
  h3$tests,
  h4$tests,
  h5$tests,
  exploratory_test
) %>%
  mutate(
    hypothesis = factor(
      hypothesis,
      levels = c("H1", "H2", "H3", "H4", "H5", "Exploratory")
    )
  ) %>%
  arrange(hypothesis) %>%
  mutate(hypothesis = as.character(hypothesis))

sensitivity_specs <- tribble(
  ~hypothesis, ~outcome, ~contrast, ~first_column, ~second_column,
  "H2", "Switch proportion", "Stimulus first - Aid first",
  "switch_proportion_stimulus_first", "switch_proportion_aid_first",
  "H2", "Switch proportion", "Stimulus first - Manual",
  "switch_proportion_stimulus_first", "switch_proportion_manual",
  "H2", "Switch proportion", "Aid first - Manual",
  "switch_proportion_aid_first", "switch_proportion_manual",
  "H3", "Decision 2 accuracy proportion; aid correct",
  "Stimulus first - Aid first",
  "decision2_accuracy_proportion_stimulus_first_aid_correct",
  "decision2_accuracy_proportion_aid_first_aid_correct",
  "H3", "Decision 2 accuracy proportion; aid correct",
  "Stimulus first - Manual",
  "decision2_accuracy_proportion_stimulus_first_aid_correct",
  "decision2_accuracy_proportion_manual",
  "H3", "Decision 2 accuracy proportion; aid correct",
  "Aid first - Manual",
  "decision2_accuracy_proportion_aid_first_aid_correct",
  "decision2_accuracy_proportion_manual",
  "H4", "Decision 2 accuracy proportion; aid incorrect",
  "Manual - Stimulus first",
  "decision2_accuracy_proportion_manual",
  "decision2_accuracy_proportion_stimulus_first_aid_incorrect",
  "H4", "Decision 2 accuracy proportion; aid incorrect",
  "Manual - Aid first",
  "decision2_accuracy_proportion_manual",
  "decision2_accuracy_proportion_aid_first_aid_incorrect",
  "H4", "Decision 2 accuracy proportion; aid incorrect",
  "Stimulus first - Aid first",
  "decision2_accuracy_proportion_stimulus_first_aid_incorrect",
  "decision2_accuracy_proportion_aid_first_aid_incorrect",
  "H5", "Overall Decision 2 accuracy proportion",
  "Stimulus first - Aid first",
  "decision2_accuracy_proportion_stimulus_first_overall",
  "decision2_accuracy_proportion_aid_first_overall",
  "H5", "Overall Decision 2 accuracy proportion",
  "Stimulus first - Manual",
  "decision2_accuracy_proportion_stimulus_first_overall",
  "decision2_accuracy_proportion_manual",
  "H5", "Overall Decision 2 accuracy proportion",
  "Aid first - Manual",
  "decision2_accuracy_proportion_aid_first_overall",
  "decision2_accuracy_proportion_manual"
)

sensitivity_tests <- bind_rows(lapply(
  seq_len(nrow(sensitivity_specs)),
  function(index) {
    spec <- sensitivity_specs[index, ]
    make_paired_test(
      participant_data,
      hypothesis = spec$hypothesis,
      outcome = spec$outcome,
      contrast = spec$contrast,
      first_column = spec$first_column,
      second_column = spec$second_column,
      estimate_scale = "Proportion difference",
      include_percentage_points = TRUE
    )
  }
)) %>%
  group_by(hypothesis) %>%
  mutate(
    p_value_adjusted = p.adjust(p_value_raw, method = "holm"),
    p_adjustment = "Holm within hypothesis",
    significant_adjusted = p_value_adjusted < ALPHA,
    contrast_result = case_when(
      direction_matches & significant_adjusted ~ "Supports prediction",
      !direction_matches & significant_adjusted ~ "Significant opposite direction",
      TRUE ~ "Not significant"
    )
  ) %>%
  ungroup()

model_probabilities <- bind_rows(
  h2$probabilities,
  h3$probabilities,
  h4$probabilities,
  h5$probabilities
)

model_diagnostics <- bind_rows(
  h2$diagnostics,
  h3$diagnostics,
  h4$diagnostics,
  h5$diagnostics
)

descriptive_lookup <- tribble(
  ~variable, ~hypothesis, ~measure, ~condition_or_subset,
  "manual_reliability_rating_pct", "H1", "Perceived reliability (%)", "Manual",
  "aid_first_reliability_rating_pct", "H1", "Perceived aid reliability (%)", "Aid first",
  "stimulus_first_reliability_rating_pct", "H1", "Perceived aid reliability (%)", "Stimulus first",
  "aid_reliability_rating_mean_pct", "H1", "Perceived aid reliability (%)", "Mean of aided conditions",
  "switch_proportion_manual", "H2", "Switch proportion", "Manual",
  "switch_proportion_aid_first", "H2", "Switch proportion", "Aid first",
  "switch_proportion_stimulus_first", "H2", "Switch proportion", "Stimulus first",
  "decision2_accuracy_proportion_manual", "H3/H4/H5", "Decision 2 accuracy proportion", "Manual overall",
  "decision2_accuracy_proportion_aid_first_aid_correct", "H3", "Decision 2 accuracy proportion", "Aid first; aid correct",
  "decision2_accuracy_proportion_stimulus_first_aid_correct", "H3", "Decision 2 accuracy proportion", "Stimulus first; aid correct",
  "decision2_accuracy_proportion_aid_first_aid_incorrect", "H4", "Decision 2 accuracy proportion", "Aid first; aid incorrect",
  "decision2_accuracy_proportion_stimulus_first_aid_incorrect", "H4", "Decision 2 accuracy proportion", "Stimulus first; aid incorrect",
  "decision2_accuracy_proportion_aid_first_overall", "H5", "Decision 2 accuracy proportion", "Aid first overall",
  "decision2_accuracy_proportion_stimulus_first_overall", "H5", "Decision 2 accuracy proportion", "Stimulus first overall",
  "trust_mean_aid_first_1to5", "Exploratory", "Six-item mean trust (1-5)", "Aid first",
  "trust_mean_stimulus_first_1to5", "Exploratory", "Six-item mean trust (1-5)", "Stimulus first",
  "harmful_switch_proportion_aid_first", "Exploratory follow-up",
  "Conditional harmful-switch proportion", "Aid first; aid incorrect and Decision 1 correct",
  "harmful_switch_proportion_stimulus_first", "Exploratory follow-up",
  "Conditional harmful-switch proportion", "Stimulus first; aid incorrect and Decision 1 correct"
)

participant_descriptives <- bind_rows(lapply(
  seq_len(nrow(descriptive_lookup)),
  function(index) {
    metadata <- descriptive_lookup[index, ]
    values <- participant_data[[metadata$variable]]
    tibble(
      variable = metadata$variable,
      hypothesis = metadata$hypothesis,
      measure = metadata$measure,
      condition_or_subset = metadata$condition_or_subset,
      n = sum(is.finite(values)),
      mean = mean(values, na.rm = TRUE),
      sd = sd(values, na.rm = TRUE),
      median = median(values, na.rm = TRUE),
      minimum = min(values, na.rm = TRUE),
      maximum = max(values, na.rm = TRUE)
    )
  }
))

make_trust_performance_correlation <- function(
  data,
  condition,
  trust_column,
  performance_column,
  performance_definition,
  method
) {
  trust <- data[[trust_column]]
  performance <- data[[performance_column]]
  complete <- is.finite(trust) & is.finite(performance)
  trust <- trust[complete]
  performance <- performance[complete]

  if (method == "pearson") {
    test <- cor.test(
      trust,
      performance,
      method = "pearson",
      alternative = "two.sided"
    )
    conf_low <- unname(test$conf.int[[1]])
    conf_high <- unname(test$conf.int[[2]])
  } else if (method == "spearman") {
    test <- cor.test(
      trust,
      performance,
      method = "spearman",
      alternative = "two.sided",
      exact = FALSE
    )
    conf_low <- NA_real_
    conf_high <- NA_real_
  } else {
    stop("Unsupported correlation method: ", method, call. = FALSE)
  }

  tibble(
    analysis = "Across-participant trust-performance correlation",
    condition = condition,
    trust_variable = trust_column,
    performance_variable = performance_column,
    performance_definition = performance_definition,
    method = method,
    n_participants = length(trust),
    correlation = unname(test$estimate),
    conf_low = conf_low,
    conf_high = conf_high,
    p_value = test$p.value,
    p_adjustment = "none; exploratory",
    significant = test$p.value < ALPHA
  )
}

trust_performance_correlations <- bind_rows(
  make_trust_performance_correlation(
    participant_data,
    condition = "Aid first",
    trust_column = "trust_mean_aid_first_1to5",
    performance_column = "decision2_accuracy_proportion_aid_first_overall",
    performance_definition = "Overall Decision 2 accuracy proportion",
    method = "pearson"
  ),
  make_trust_performance_correlation(
    participant_data,
    condition = "Stimulus first",
    trust_column = "trust_mean_stimulus_first_1to5",
    performance_column = "decision2_accuracy_proportion_stimulus_first_overall",
    performance_definition = "Overall Decision 2 accuracy proportion",
    method = "pearson"
  ),
  make_trust_performance_correlation(
    participant_data,
    condition = "Aid first",
    trust_column = "trust_mean_aid_first_1to5",
    performance_column = "decision2_accuracy_proportion_aid_first_overall",
    performance_definition = "Overall Decision 2 accuracy proportion",
    method = "spearman"
  ),
  make_trust_performance_correlation(
    participant_data,
    condition = "Stimulus first",
    trust_column = "trust_mean_stimulus_first_1to5",
    performance_column = "decision2_accuracy_proportion_stimulus_first_overall",
    performance_definition = "Overall Decision 2 accuracy proportion",
    method = "spearman"
  ),
  make_trust_performance_correlation(
    participant_data,
    condition = "Aid first",
    trust_column = "trust_mean_aid_first_1to5",
    performance_column = "switch_proportion_aid_first",
    performance_definition = "Decision-switch proportion",
    method = "pearson"
  ),
  make_trust_performance_correlation(
    participant_data,
    condition = "Stimulus first",
    trust_column = "trust_mean_stimulus_first_1to5",
    performance_column = "switch_proportion_stimulus_first",
    performance_definition = "Decision-switch proportion",
    method = "pearson"
  ),
  make_trust_performance_correlation(
    participant_data,
    condition = "Aid first",
    trust_column = "trust_mean_aid_first_1to5",
    performance_column = "switch_proportion_aid_first",
    performance_definition = "Decision-switch proportion",
    method = "spearman"
  ),
  make_trust_performance_correlation(
    participant_data,
    condition = "Stimulus first",
    trust_column = "trust_mean_stimulus_first_1to5",
    performance_column = "switch_proportion_stimulus_first",
    performance_definition = "Decision-switch proportion",
    method = "spearman"
  ),
  make_trust_performance_correlation(
    participant_data,
    condition = "Aid first",
    trust_column = "trust_mean_aid_first_1to5",
    performance_column = "harmful_switch_proportion_aid_first",
    performance_definition = paste(
      "Conditional harmful-switch proportion;",
      "aid incorrect and Decision 1 correct"
    ),
    method = "pearson"
  ),
  make_trust_performance_correlation(
    participant_data,
    condition = "Stimulus first",
    trust_column = "trust_mean_stimulus_first_1to5",
    performance_column = "harmful_switch_proportion_stimulus_first",
    performance_definition = paste(
      "Conditional harmful-switch proportion;",
      "aid incorrect and Decision 1 correct"
    ),
    method = "pearson"
  ),
  make_trust_performance_correlation(
    participant_data,
    condition = "Aid first",
    trust_column = "trust_mean_aid_first_1to5",
    performance_column = "harmful_switch_proportion_aid_first",
    performance_definition = paste(
      "Conditional harmful-switch proportion;",
      "aid incorrect and Decision 1 correct"
    ),
    method = "spearman"
  ),
  make_trust_performance_correlation(
    participant_data,
    condition = "Stimulus first",
    trust_column = "trust_mean_stimulus_first_1to5",
    performance_column = "harmful_switch_proportion_stimulus_first",
    performance_definition = paste(
      "Conditional harmful-switch proportion;",
      "aid incorrect and Decision 1 correct"
    ),
    method = "spearman"
  )
)

aid_first_trust <- participant_data$trust_mean_aid_first_1to5
aid_first_performance <-
  participant_data$decision2_accuracy_proportion_aid_first_overall
stimulus_first_trust <- participant_data$trust_mean_stimulus_first_1to5
stimulus_first_performance <-
  participant_data$decision2_accuracy_proportion_stimulus_first_overall

observed_aid_first_correlation <- cor(
  aid_first_trust,
  aid_first_performance,
  method = "pearson"
)
observed_stimulus_first_correlation <- cor(
  stimulus_first_trust,
  stimulus_first_performance,
  method = "pearson"
)
observed_correlation_difference <-
  observed_stimulus_first_correlation - observed_aid_first_correlation

set.seed(TRUST_BOOTSTRAP_SEED)
bootstrap_correlation_differences <- replicate(
  TRUST_BOOTSTRAP_REPS,
  {
    sampled_rows <- sample.int(
      nrow(participant_data),
      nrow(participant_data),
      replace = TRUE
    )
    cor(
      stimulus_first_trust[sampled_rows],
      stimulus_first_performance[sampled_rows],
      method = "pearson"
    ) -
      cor(
        aid_first_trust[sampled_rows],
        aid_first_performance[sampled_rows],
        method = "pearson"
      )
  }
)
bootstrap_correlation_differences <- bootstrap_correlation_differences[
  is.finite(bootstrap_correlation_differences)
]

if (length(bootstrap_correlation_differences) < TRUST_BOOTSTRAP_REPS * 0.99) {
  stop(
    "Fewer than 99% of trust-performance bootstrap samples produced finite correlations.",
    call. = FALSE
  )
}

bootstrap_confidence_interval <- quantile(
  bootstrap_correlation_differences,
  probs = c(0.025, 0.975),
  names = FALSE
)
bootstrap_p_value <- min(
  1,
  2 * min(
    mean(bootstrap_correlation_differences <= 0),
    mean(bootstrap_correlation_differences >= 0)
  )
)

trust_accuracy_correlation_difference <- tibble(
  analysis = "Difference between dependent Pearson correlations",
  performance_definition = "Overall Decision 2 accuracy proportion",
  contrast = "Stimulus first correlation - Aid first correlation",
  n_participants = nrow(participant_data),
  stimulus_first_correlation = observed_stimulus_first_correlation,
  aid_first_correlation = observed_aid_first_correlation,
  correlation_difference = observed_correlation_difference,
  bootstrap_conf_low = bootstrap_confidence_interval[[1]],
  bootstrap_conf_high = bootstrap_confidence_interval[[2]],
  bootstrap_p_value = bootstrap_p_value,
  bootstrap_repetitions_requested = TRUST_BOOTSTRAP_REPS,
  bootstrap_repetitions_valid = length(bootstrap_correlation_differences),
  bootstrap_seed = TRUST_BOOTSTRAP_SEED,
  p_adjustment = "none; exploratory",
  significant = bootstrap_p_value < ALPHA
)

aid_first_switch <- participant_data$switch_proportion_aid_first
stimulus_first_switch <- participant_data$switch_proportion_stimulus_first

observed_aid_first_switch_correlation <- cor(
  aid_first_trust,
  aid_first_switch,
  method = "pearson"
)
observed_stimulus_first_switch_correlation <- cor(
  stimulus_first_trust,
  stimulus_first_switch,
  method = "pearson"
)
observed_switch_correlation_difference <-
  observed_stimulus_first_switch_correlation -
  observed_aid_first_switch_correlation

set.seed(TRUST_SWITCH_BOOTSTRAP_SEED)
bootstrap_switch_correlation_differences <- replicate(
  TRUST_BOOTSTRAP_REPS,
  {
    sampled_rows <- sample.int(
      nrow(participant_data),
      nrow(participant_data),
      replace = TRUE
    )
    cor(
      stimulus_first_trust[sampled_rows],
      stimulus_first_switch[sampled_rows],
      method = "pearson"
    ) -
      cor(
        aid_first_trust[sampled_rows],
        aid_first_switch[sampled_rows],
        method = "pearson"
      )
  }
)
bootstrap_switch_correlation_differences <-
  bootstrap_switch_correlation_differences[
    is.finite(bootstrap_switch_correlation_differences)
  ]

if (
  length(bootstrap_switch_correlation_differences) <
    TRUST_BOOTSTRAP_REPS * 0.99
) {
  stop(
    "Fewer than 99% of trust-switch bootstrap samples produced finite correlations.",
    call. = FALSE
  )
}

bootstrap_switch_confidence_interval <- quantile(
  bootstrap_switch_correlation_differences,
  probs = c(0.025, 0.975),
  names = FALSE
)
bootstrap_switch_p_value <- min(
  1,
  2 * min(
    mean(bootstrap_switch_correlation_differences <= 0),
    mean(bootstrap_switch_correlation_differences >= 0)
  )
)

trust_switch_correlation_difference <- tibble(
  analysis = "Difference between dependent Pearson correlations",
  performance_definition = "Decision-switch proportion",
  contrast = "Stimulus first correlation - Aid first correlation",
  n_participants = nrow(participant_data),
  stimulus_first_correlation = observed_stimulus_first_switch_correlation,
  aid_first_correlation = observed_aid_first_switch_correlation,
  correlation_difference = observed_switch_correlation_difference,
  bootstrap_conf_low = bootstrap_switch_confidence_interval[[1]],
  bootstrap_conf_high = bootstrap_switch_confidence_interval[[2]],
  bootstrap_p_value = bootstrap_switch_p_value,
  bootstrap_repetitions_requested = TRUST_BOOTSTRAP_REPS,
  bootstrap_repetitions_valid =
    length(bootstrap_switch_correlation_differences),
  bootstrap_seed = TRUST_SWITCH_BOOTSTRAP_SEED,
  p_adjustment = "none; exploratory",
  significant = bootstrap_switch_p_value < ALPHA
)

aid_first_harmful_switch <-
  participant_data$harmful_switch_proportion_aid_first
stimulus_first_harmful_switch <-
  participant_data$harmful_switch_proportion_stimulus_first

observed_aid_first_harmful_switch_correlation <- cor(
  aid_first_trust,
  aid_first_harmful_switch,
  method = "pearson"
)
observed_stimulus_first_harmful_switch_correlation <- cor(
  stimulus_first_trust,
  stimulus_first_harmful_switch,
  method = "pearson"
)
observed_harmful_switch_correlation_difference <-
  observed_stimulus_first_harmful_switch_correlation -
  observed_aid_first_harmful_switch_correlation

set.seed(TRUST_HARMFUL_SWITCH_BOOTSTRAP_SEED)
bootstrap_harmful_switch_correlation_differences <- replicate(
  TRUST_BOOTSTRAP_REPS,
  {
    sampled_rows <- sample.int(
      nrow(participant_data),
      nrow(participant_data),
      replace = TRUE
    )
    cor(
      stimulus_first_trust[sampled_rows],
      stimulus_first_harmful_switch[sampled_rows],
      method = "pearson"
    ) -
      cor(
        aid_first_trust[sampled_rows],
        aid_first_harmful_switch[sampled_rows],
        method = "pearson"
      )
  }
)
bootstrap_harmful_switch_correlation_differences <-
  bootstrap_harmful_switch_correlation_differences[
    is.finite(bootstrap_harmful_switch_correlation_differences)
  ]

if (
  length(bootstrap_harmful_switch_correlation_differences) <
    TRUST_BOOTSTRAP_REPS * 0.99
) {
  stop(
    paste(
      "Fewer than 99% of trust-harmful-switch bootstrap samples",
      "produced finite correlations."
    ),
    call. = FALSE
  )
}

bootstrap_harmful_switch_confidence_interval <- quantile(
  bootstrap_harmful_switch_correlation_differences,
  probs = c(0.025, 0.975),
  names = FALSE
)
bootstrap_harmful_switch_p_value <- min(
  1,
  2 * min(
    mean(bootstrap_harmful_switch_correlation_differences <= 0),
    mean(bootstrap_harmful_switch_correlation_differences >= 0)
  )
)

trust_harmful_switch_correlation_difference <- tibble(
  analysis = "Difference between dependent Pearson correlations",
  performance_definition = paste(
    "Conditional harmful-switch proportion;",
    "aid incorrect and Decision 1 correct"
  ),
  contrast = "Stimulus first correlation - Aid first correlation",
  n_participants = nrow(participant_data),
  stimulus_first_correlation =
    observed_stimulus_first_harmful_switch_correlation,
  aid_first_correlation = observed_aid_first_harmful_switch_correlation,
  correlation_difference = observed_harmful_switch_correlation_difference,
  bootstrap_conf_low =
    bootstrap_harmful_switch_confidence_interval[[1]],
  bootstrap_conf_high =
    bootstrap_harmful_switch_confidence_interval[[2]],
  bootstrap_p_value = bootstrap_harmful_switch_p_value,
  bootstrap_repetitions_requested = TRUST_BOOTSTRAP_REPS,
  bootstrap_repetitions_valid =
    length(bootstrap_harmful_switch_correlation_differences),
  bootstrap_seed = TRUST_HARMFUL_SWITCH_BOOTSTRAP_SEED,
  p_adjustment = "none; exploratory",
  significant = bootstrap_harmful_switch_p_value < ALPHA
)

trust_performance_correlation_difference <- bind_rows(
  trust_accuracy_correlation_difference,
  trust_switch_correlation_difference,
  trust_harmful_switch_correlation_difference
)

harmful_switch_trust_long <- participant_data %>%
  select(
    subject_no,
    trust_mean_aid_first_1to5,
    trust_mean_stimulus_first_1to5
  ) %>%
  pivot_longer(
    cols = starts_with("trust_mean_"),
    names_to = "trust_variable",
    values_to = "trust_mean"
  ) %>%
  mutate(
    condition = factor(
      if_else(
        trust_variable == "trust_mean_aid_first_1to5",
        "Aid first",
        "Stimulus first"
      ),
      levels = c("Aid first", "Stimulus first")
    )
  ) %>%
  group_by(condition) %>%
  mutate(trust_z_within_condition = as.numeric(scale(trust_mean))) %>%
  ungroup()

harmful_switch_model_data <- harmful_switch_participant_long %>%
  mutate(
    condition = factor(
      as.character(condition),
      levels = c("Aid first", "Stimulus first")
    ),
    subject = factor(subject_no, levels = expected_subjects)
  ) %>%
  left_join(
    harmful_switch_trust_long,
    by = c("subject_no", "condition")
  )

if (
  nrow(harmful_switch_model_data) != EXPECTED_PARTICIPANTS * 2L ||
    any(is.na(harmful_switch_model_data$trust_mean)) ||
    any(!is.finite(harmful_switch_model_data$trust_z_within_condition))
) {
  stop("Harmful-switch model data are incomplete.", call. = FALSE)
}

harmful_switch_model <- glmer(
  cbind(
    n_harmful_switches,
    n_initially_correct - n_harmful_switches
  ) ~ condition * trust_z_within_condition + (1 | subject),
  data = harmful_switch_model_data,
  family = binomial(link = "logit"),
  nAGQ = 1,
  control = glmerControl(
    optimizer = "bobyqa",
    calc.derivs = TRUE,
    optCtrl = list(maxfun = 200000)
  )
)

harmful_switch_convergence_messages <-
  harmful_switch_model@optinfo$conv$lme4$messages
harmful_switch_optimizer_code <- harmful_switch_model@optinfo$conv$opt
harmful_switch_converged <-
  is.null(harmful_switch_convergence_messages) &&
  isTRUE(harmful_switch_optimizer_code == 0) &&
  all(is.finite(fixef(harmful_switch_model)))
harmful_switch_singular <- isSingular(harmful_switch_model, tol = 1e-4)
harmful_switch_gradient <- harmful_switch_model@optinfo$derivs$gradient
harmful_switch_max_abs_gradient <- if (is.null(harmful_switch_gradient)) {
  NA_real_
} else {
  max(abs(harmful_switch_gradient))
}

harmful_switch_model_diagnostics <- tibble(
  analysis = "Binomial mixed-model sensitivity analysis",
  outcome = paste(
    "Harmful switch among initially correct,",
    "incorrect-advice trials"
  ),
  n_participant_condition_cells = nrow(harmful_switch_model_data),
  n_eligible_trials = sum(harmful_switch_model_data$n_initially_correct),
  n_harmful_switches = sum(harmful_switch_model_data$n_harmful_switches),
  n_participants = n_distinct(harmful_switch_model_data$subject),
  converged = harmful_switch_converged,
  singular = harmful_switch_singular,
  optimizer_code = harmful_switch_optimizer_code,
  convergence_message = if (is.null(harmful_switch_convergence_messages)) {
    ""
  } else {
    paste(harmful_switch_convergence_messages, collapse = " | ")
  },
  max_absolute_gradient = harmful_switch_max_abs_gradient
)

if (!harmful_switch_converged || harmful_switch_singular) {
  stop(
    "Harmful-switch mixed model failed convergence or singularity checks.",
    call. = FALSE
  )
}

make_harmful_switch_model_result <- function(
  term,
  interpretation,
  coefficient_weights
) {
  coefficients <- fixef(harmful_switch_model)
  covariance <- as.matrix(vcov(harmful_switch_model))
  weights <- setNames(rep(0, length(coefficients)), names(coefficients))
  missing_coefficients <- setdiff(names(coefficient_weights), names(coefficients))

  if (length(missing_coefficients)) {
    stop(
      "Missing harmful-switch coefficient(s): ",
      paste(missing_coefficients, collapse = ", "),
      call. = FALSE
    )
  }

  weights[names(coefficient_weights)] <- coefficient_weights
  estimate <- sum(weights * coefficients)
  standard_error <- sqrt(as.numeric(t(weights) %*% covariance %*% weights))
  statistic <- estimate / standard_error
  p_value <- 2 * pnorm(-abs(statistic))
  critical_value <- qnorm(0.975)
  conf_low_log_odds <- estimate - critical_value * standard_error
  conf_high_log_odds <- estimate + critical_value * standard_error

  tibble(
    analysis = "Binomial mixed-model sensitivity analysis",
    term = term,
    interpretation = interpretation,
    trust_scaling = "Z score calculated separately within each condition",
    estimate_log_odds = estimate,
    standard_error = standard_error,
    statistic = statistic,
    p_value = p_value,
    p_adjustment = "none; exploratory",
    odds_ratio = exp(estimate),
    odds_ratio_conf_low = exp(conf_low_log_odds),
    odds_ratio_conf_high = exp(conf_high_log_odds),
    significant = p_value < ALPHA
  )
}

harmful_switch_model_results <- bind_rows(
  make_harmful_switch_model_result(
    term = "intercept",
    interpretation = "Aid-first harmful-switch odds at mean Aid-first trust",
    coefficient_weights = c("(Intercept)" = 1)
  ),
  make_harmful_switch_model_result(
    term = "condition_stimulus_first",
    interpretation = paste(
      "Stimulus-first versus Aid-first harmful-switch odds",
      "at each condition's mean trust"
    ),
    coefficient_weights = c("conditionStimulus first" = 1)
  ),
  make_harmful_switch_model_result(
    term = "trust_slope_aid_first",
    interpretation = "Aid-first trust slope per within-condition SD",
    coefficient_weights = c("trust_z_within_condition" = 1)
  ),
  make_harmful_switch_model_result(
    term = "trust_slope_stimulus_first",
    interpretation = "Stimulus-first trust slope per within-condition SD",
    coefficient_weights = c(
      "trust_z_within_condition" = 1,
      "conditionStimulus first:trust_z_within_condition" = 1
    )
  ),
  make_harmful_switch_model_result(
    term = "condition_by_trust_interaction",
    interpretation = "Stimulus-first trust slope minus Aid-first trust slope",
    coefficient_weights = c(
      "conditionStimulus first:trust_z_within_condition" = 1
    )
  )
)

make_directional_conclusion <- function(hypothesis_id, tests) {
  hypothesis_tests <- tests %>%
    filter(.data$hypothesis == .env$hypothesis_id)
  supported <- sum(
    hypothesis_tests$significant_adjusted & hypothesis_tests$direction_matches
  )
  opposite <- sum(
    hypothesis_tests$significant_adjusted & !hypothesis_tests$direction_matches
  )
  required <- nrow(hypothesis_tests)

  classification <- case_when(
    supported == required ~ "Fully supported",
    supported > 0 & opposite > 0 ~ "Partially supported with contrary evidence",
    supported > 0 ~ "Partially supported",
    opposite > 0 ~ "Contradicted",
    TRUE ~ "Not supported"
  )

  tibble(
    hypothesis = hypothesis_id,
    classification = classification,
    supported_contrasts = supported,
    required_contrasts = required,
    significant_opposite_contrasts = opposite,
    inferential_basis = if (hypothesis_id == "H1") {
      "Paired t-test"
    } else {
      "Trial-level logistic mixed model"
    },
    details = paste(
      paste0(hypothesis_tests$contrast, ": ", hypothesis_tests$contrast_result),
      collapse = "; "
    )
  )
}

hypothesis_conclusions <- bind_rows(
  make_directional_conclusion("H1", primary_tests),
  make_directional_conclusion("H2", primary_tests),
  make_directional_conclusion("H3", primary_tests),
  make_directional_conclusion("H4", primary_tests),
  make_directional_conclusion("H5", primary_tests)
)

exploratory_conclusion <- exploratory_test %>%
  transmute(
    hypothesis = "Exploratory",
    classification = case_when(
      significant_adjusted & estimate > 0 ~
        "Significant difference; Stimulus first higher",
      significant_adjusted & estimate < 0 ~
        "Significant difference; Stimulus first lower",
      TRUE ~ "No significant difference"
    ),
    supported_contrasts = NA_integer_,
    required_contrasts = 1L,
    significant_opposite_contrasts = NA_integer_,
    inferential_basis = "Paired t-test",
    details = paste0(contrast, ": ", contrast_result)
  )

hypothesis_conclusions <- bind_rows(
  hypothesis_conclusions,
  exploratory_conclusion
)

if (
  nrow(primary_tests) != 14L ||
    nrow(sensitivity_tests) != 12L ||
    nrow(model_probabilities) != 12L ||
    nrow(model_diagnostics) != 4L ||
    nrow(participant_descriptives) != 18L ||
    nrow(hypothesis_conclusions) != 6L ||
    nrow(trust_performance_correlations) != 12L ||
    nrow(trust_performance_correlation_difference) != 3L ||
    nrow(harmful_switch_condition_test) != 1L ||
    nrow(harmful_switch_model_results) != 5L ||
    nrow(harmful_switch_model_diagnostics) != 1L
) {
  stop("One or more result tables have unexpected dimensions.", call. = FALSE)
}

if (
  any(!model_diagnostics$converged) ||
    any(model_diagnostics$singular) ||
    any(!is.finite(primary_tests$p_value_raw)) ||
    any(!is.finite(primary_tests$p_value_adjusted)) ||
    any(!is.finite(sensitivity_tests$p_value_raw)) ||
    any(!is.finite(sensitivity_tests$p_value_adjusted)) ||
    any(!is.finite(trust_performance_correlations$correlation)) ||
    any(!is.finite(trust_performance_correlations$p_value)) ||
    any(!is.finite(trust_performance_correlation_difference$bootstrap_p_value)) ||
    any(!is.finite(harmful_switch_condition_test$p_value_raw)) ||
    any(!is.finite(harmful_switch_model_results$p_value)) ||
    any(!is.finite(harmful_switch_model_results$odds_ratio)) ||
    !harmful_switch_model_diagnostics$converged ||
    harmful_switch_model_diagnostics$singular
) {
  stop("Final model or result-table validation failed.", call. = FALSE)
}

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

write_csv(
  participant_descriptives,
  file.path(OUTPUT_DIR, "participant_descriptives.csv")
)
write_csv(
  model_probabilities,
  file.path(OUTPUT_DIR, "model_estimated_probabilities.csv")
)
write_csv(
  primary_tests,
  file.path(OUTPUT_DIR, "primary_hypothesis_tests.csv")
)
write_csv(
  sensitivity_tests,
  file.path(OUTPUT_DIR, "participant_level_sensitivity_tests.csv")
)
write_csv(
  model_diagnostics,
  file.path(OUTPUT_DIR, "model_diagnostics.csv")
)
write_csv(
  hypothesis_conclusions,
  file.path(OUTPUT_DIR, "hypothesis_conclusions.csv")
)
write_csv(
  trust_performance_correlations,
  file.path(OUTPUT_DIR, "trust_performance_correlations.csv")
)
write_csv(
  trust_performance_correlation_difference,
  file.path(OUTPUT_DIR, "trust_performance_correlation_difference.csv")
)
write_csv(
  harmful_switch_condition_test,
  file.path(OUTPUT_DIR, "harmful_switch_condition_test.csv")
)
write_csv(
  harmful_switch_model_results,
  file.path(OUTPUT_DIR, "harmful_switch_model_results.csv")
)
write_csv(
  harmful_switch_model_diagnostics,
  file.path(OUTPUT_DIR, "harmful_switch_model_diagnostics.csv")
)
write_csv(
  analysis_exclusions,
  file.path(OUTPUT_DIR, "analysis_exclusions.csv")
)

cat("\nAid-onset hypothesis analysis\n")
cat("Participants:", EXPECTED_PARTICIPANTS, "\n")
cat("Excluded participant(s):", paste(EXCLUDED_SUBJECTS, collapse = ", "), "\n")
cat("Trials:", nrow(trial_data), "\n\n")

h1_row <- primary_tests %>% filter(hypothesis == "H1")
cat(
  "H1:", hypothesis_conclusions$classification[hypothesis_conclusions$hypothesis == "H1"],
  "- aid reliability was",
  formatC(h1_row$estimate, format = "f", digits = 2),
  "percentage points higher than Manual; p", format_p(h1_row$p_value_adjusted),
  ".\n"
)

for (hypothesis_id in c("H2", "H3", "H4", "H5")) {
  classification <- hypothesis_conclusions$classification[
    hypothesis_conclusions$hypothesis == hypothesis_id
  ]
  cat(hypothesis_id, ": ", classification, ".\n", sep = "")
  hypothesis_rows <- primary_tests %>%
    filter(.data$hypothesis == .env$hypothesis_id)
  for (index in seq_len(nrow(hypothesis_rows))) {
    row <- hypothesis_rows[index, ]
    cat(
      "  ", row$contrast,
      ": OR = ", formatC(row$estimate, format = "f", digits = 3),
      ", Holm p ", format_p(row$p_value_adjusted),
      " (", row$contrast_result, ").\n",
      sep = ""
    )
  }
}

exploratory_row <- primary_tests %>% filter(hypothesis == "Exploratory")
cat(
  "Exploratory trust:", exploratory_conclusion$classification,
  "- Stimulus-first minus Aid-first =",
  formatC(exploratory_row$estimate, format = "f", digits = 2),
  "points; p", format_p(exploratory_row$p_value_adjusted),
  ".\n\n"
)

cat(
  "Harmful-switch follow-up: Stimulus-first mean =",
  formatC(harmful_switch_condition_test$mean_first, format = "f", digits = 3),
  ", Aid-first mean =",
  formatC(harmful_switch_condition_test$mean_second, format = "f", digits = 3),
  ", paired difference =",
  formatC(
    harmful_switch_condition_test$estimate_percentage_points,
    format = "f",
    digits = 2
  ),
  "percentage points; p",
  format_p(harmful_switch_condition_test$p_value_raw),
  ".\n\n"
)

cat("Trust-performance correlations across participants:\n")
performance_labels <- c(
  "Overall Decision 2 accuracy proportion" = "Overall Decision 2 accuracy",
  "Decision-switch proportion" = "Decision-switch proportion"
)
performance_labels <- c(
  performance_labels,
  setNames(
    "Conditional harmful-switch proportion",
    paste(
    "Conditional harmful-switch proportion;",
    "aid incorrect and Decision 1 correct"
    )
  )
)

for (performance_id in names(performance_labels)) {
  cat(" ", performance_labels[[performance_id]], ":\n", sep = "")
  pearson_rows <- trust_performance_correlations %>%
    filter(
      method == "pearson",
      .data$performance_definition == .env$performance_id
    )
  spearman_rows <- trust_performance_correlations %>%
    filter(
      method == "spearman",
      .data$performance_definition == .env$performance_id
    )
  difference_row <- trust_performance_correlation_difference %>%
    filter(.data$performance_definition == .env$performance_id)

  for (index in seq_len(nrow(pearson_rows))) {
    row <- pearson_rows[index, ]
    cat(
      "   ", row$condition,
      ": Pearson r = ", formatC(row$correlation, format = "f", digits = 3),
      ", 95% CI [", formatC(row$conf_low, format = "f", digits = 3),
      ", ", formatC(row$conf_high, format = "f", digits = 3),
      "], p ", format_p(row$p_value), ".\n",
      sep = ""
    )
  }
  cat(
    "   Correlation difference (Stimulus first - Aid first) = ",
    formatC(difference_row$correlation_difference, format = "f", digits = 3),
    ", bootstrap 95% CI [",
    formatC(difference_row$bootstrap_conf_low, format = "f", digits = 3),
    ", ", formatC(difference_row$bootstrap_conf_high, format = "f", digits = 3),
    "], p ", format_p(difference_row$bootstrap_p_value), ".\n",
    sep = ""
  )
  for (index in seq_len(nrow(spearman_rows))) {
    row <- spearman_rows[index, ]
    cat(
      "   ", row$condition,
      ": Spearman rho = ", formatC(row$correlation, format = "f", digits = 3),
      ", p ", format_p(row$p_value), ".\n",
      sep = ""
    )
  }
}
cat("\n")

condition_effect_row <- harmful_switch_model_results %>%
  filter(term == "condition_stimulus_first")
aid_first_trust_slope_row <- harmful_switch_model_results %>%
  filter(term == "trust_slope_aid_first")
stimulus_first_trust_slope_row <- harmful_switch_model_results %>%
  filter(term == "trust_slope_stimulus_first")
interaction_row <- harmful_switch_model_results %>%
  filter(term == "condition_by_trust_interaction")

cat("Harmful-switch binomial mixed-model sensitivity analysis:\n")
cat(
  "  Stimulus-first versus Aid-first: OR = ",
  formatC(condition_effect_row$odds_ratio, format = "f", digits = 3),
  ", p ", format_p(condition_effect_row$p_value), ".\n",
  sep = ""
)
cat(
  "  Aid-first trust slope: OR = ",
  formatC(aid_first_trust_slope_row$odds_ratio, format = "f", digits = 3),
  ", p ", format_p(aid_first_trust_slope_row$p_value), ".\n",
  sep = ""
)
cat(
  "  Stimulus-first trust slope: OR = ",
  formatC(stimulus_first_trust_slope_row$odds_ratio, format = "f", digits = 3),
  ", p ", format_p(stimulus_first_trust_slope_row$p_value), ".\n",
  sep = ""
)
cat(
  "  Condition-by-trust interaction: OR = ",
  formatC(interaction_row$odds_ratio, format = "f", digits = 3),
  ", p ", format_p(interaction_row$p_value), ".\n\n",
  sep = ""
)

cat("Wrote result tables to", OUTPUT_DIR, "\n")
