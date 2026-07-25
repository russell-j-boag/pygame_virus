# Plot individual and cohort decision accuracy, self-ratings, and correct RT.
#
# Usage:
#   Rscript plot_condition_decision_means.R [input_dir] [output_dir] \
#     [output_prefix] [plot_label] [mode]
#
# Modes:
#   single  - plot one participant
#   cohort  - plot every participant plus the group summaries
#   group   - plot only the group summaries

rm(list = ls())

library("dplyr")
library("ggplot2")
library("patchwork")
library("purrr")
library("readr")
library("stringr")
library("tibble")
library("tidyr")
library("zoo")

font_cache_dir <- file.path(tempdir(), "fontconfig-cache")
dir.create(font_cache_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(XDG_CACHE_HOME = font_cache_dir)

args <- commandArgs(trailingOnly = TRUE)

INPUT_DIR <- if (length(args) >= 1) args[[1]] else "output"
OUTPUT_DIR <- if (length(args) >= 2) args[[2]] else "plots"
OUTPUT_PREFIX <- if (length(args) >= 3) args[[3]] else "condition"
PLOT_LABEL <- if (length(args) >= 4) args[[4]] else "Cohort"
PLOT_MODE <- if (length(args) >= 5) args[[5]] else "cohort"

if (!PLOT_MODE %in% c("single", "cohort", "group")) {
  stop("mode must be 'single', 'cohort', or 'group'", call. = FALSE)
}

if (!dir.exists(INPUT_DIR)) {
  stop("Input directory does not exist: ", INPUT_DIR, call. = FALSE)
}

PRACTICE_BURN_IN_N <- 20
PRACTICE_KEEP_N <- 40
PRACTICE_TOTAL_N <- PRACTICE_BURN_IN_N + PRACTICE_KEEP_N
CALIBRATION_ROLLING_N <- 10
DELTA_SD <- 0.014615991726
PRACTICE_TARGET <- 0.75
PRACTICE_PLOT_LABEL <- "Practice = last 40 calibration trials"
STUDY_TITLE <- "Aid Onset Study"
GLOBAL_AID_ACCURACY <- 0.85
CONDITION_CODES <- c("PRACTICE", "MANUAL", "AIDFIRST", "STIMFIRST")
CONDITION_LABELS <- c("Practice", "Manual", "Aid first", "Stimulus first")
AID_OUTCOME_LEVELS <- c("Aid correct", "Manual", "Aid incorrect")
AID_OUTCOME_FACET_LEVELS <- c("Aid first", "Stimulus first")
DECISION_LEVELS <- c("Decision 1", "Decision 2")
DECISION_COLOURS <- c(
  "Decision 1" = "#0072B2",
  "Decision 2" = "#D55E00"
)
DECISION_SHAPES <- c("Decision 1" = 16, "Decision 2" = 17)
DECISION_OFFSETS <- c("Decision 1" = -0.07, "Decision 2" = 0.07)
CHANGE_STATUS_LEVELS <- c("No change of mind", "Change of mind")
CHANGE_STATUS_COLOURS <- c(
  "No change of mind" = "#4D4D4D",
  "Change of mind" = "#CC79A7"
)
CHANGE_STATUS_SHAPES <- c("No change of mind" = 16, "Change of mind" = 18)
CHANGE_STATUS_OFFSETS <- c("No change of mind" = -0.07, "Change of mind" = 0.07)
RATING_LEVELS <- c("Self-rated own accuracy", "Self-rated aid accuracy")
RATING_COLOURS <- c(
  "Self-rated own accuracy" = "#009E73",
  "Self-rated aid accuracy" = "#CC79A7"
)
RATING_SHAPES <- c(
  "Self-rated own accuracy" = 15,
  "Self-rated aid accuracy" = 18
)

required_trial_cols <- c(
  "participant_id",
  "condition_code",
  "trial",
  "staircase_target_accuracy",
  "aid_accuracy_setting",
  "aid_correct",
  "decision1_correct",
  "decision1_rt_s",
  "decision2_correct",
  "decision2_rt_s",
  "changed_response"
)

required_calibration_cols <- c(
  "participant_id",
  "condition_code",
  "trial",
  "difficulty_mode",
  "staircase_target_accuracy",
  "delta_stair_realised",
  "delta_stair_mean",
  "decision2_correct"
)

as_binary <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  case_when(
    is.na(x_chr) | x_chr == "na" | x_chr == "" ~ NA_real_,
    x_chr %in% c("true", "t", "1") ~ 1,
    x_chr %in% c("false", "f", "0") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

mean_finite <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

latest_participant_files <- function(pattern, label) {
  files <- list.files(INPUT_DIR, pattern = pattern, full.names = TRUE)
  if (!length(files)) {
    stop("No ", label, " files found in ", INPUT_DIR, call. = FALSE)
  }

  file_tbl <- tibble(
    path = files,
    file_name = basename(files),
    participant_key = str_extract(basename(files), "(?<=results_)[^_]+"),
    mtime = file.info(files)$mtime
  )

  if (any(is.na(file_tbl$participant_key))) {
    stop("Could not extract participant IDs from all ", label, " filenames.", call. = FALSE)
  }

  file_tbl %>%
    group_by(participant_key) %>%
    slice_max(order_by = mtime, n = 1, with_ties = FALSE) %>%
    ungroup()
}

read_latest_combined <- function(pattern, label) {
  selected <- latest_participant_files(pattern, label)
  message("Selected ", nrow(selected), " ", label, " file(s):")
  print(selected %>% select(participant_key, path, mtime))

  selected %>%
    mutate(data = map(path, read_csv, show_col_types = FALSE)) %>%
    pull(data) %>%
    bind_rows()
}

validate_required_columns <- function(dat, required, label) {
  missing_cols <- setdiff(required, names(dat))
  if (length(missing_cols)) {
    stop(
      label,
      " input is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

condition_factor <- function(x) {
  factor(x, levels = CONDITION_CODES, labels = CONDITION_LABELS)
}

prepare_trials <- function(main_dat, practice_dat) {
  validate_required_columns(main_dat, required_trial_cols, "Main trial")
  validate_required_columns(practice_dat, required_trial_cols, "Practice trial")

  main_dat <- main_dat %>%
    filter(condition_code %in% CONDITION_CODES[-1])

  practice_counts <- practice_dat %>%
    count(participant_id, name = "n_practice")
  incomplete_practice <- practice_counts %>%
    filter(n_practice < PRACTICE_KEEP_N)
  if (nrow(incomplete_practice)) {
    stop(
      "Practice files have fewer than ",
      PRACTICE_KEEP_N,
      " trials for participant(s): ",
      paste(incomplete_practice$participant_id, collapse = ", "),
      call. = FALSE
    )
  }

  practice_dat <- practice_dat %>%
    filter(condition_code == "PRACTICE") %>%
    group_by(participant_id) %>%
    arrange(trial, .by_group = TRUE) %>%
    slice_tail(n = PRACTICE_KEEP_N) %>%
    ungroup()

  bind_rows(practice_dat, main_dat) %>%
    mutate(
      participant_id = suppressWarnings(as.integer(participant_id)),
      trial = suppressWarnings(as.integer(trial)),
      condition = condition_factor(condition_code),
      condition_order = as.integer(condition),
      decision1_correct_num = as_binary(decision1_correct),
      decision2_correct_num = as_binary(decision2_correct),
      changed_response_num = as_binary(changed_response),
      aid_correct_num = as_binary(aid_correct),
      decision1_rt_valid = suppressWarnings(as.numeric(decision1_rt_s)),
      decision2_rt_valid = suppressWarnings(as.numeric(decision2_rt_s)),
      staircase_target_accuracy = suppressWarnings(
        as.numeric(staircase_target_accuracy)
      ),
      aid_accuracy_setting = suppressWarnings(as.numeric(aid_accuracy_setting))
    ) %>%
    mutate(
      decision1_rt_valid = if_else(
        is.finite(decision1_rt_valid) & decision1_rt_valid > 0,
        decision1_rt_valid,
        NA_real_
      ),
      decision2_rt_valid = if_else(
        is.finite(decision2_rt_valid) & decision2_rt_valid > 0,
        decision2_rt_valid,
        NA_real_
      )
    ) %>%
    filter(!is.na(participant_id), !is.na(condition))
}

prepare_calibration_trials <- function(practice_dat) {
  validate_required_columns(
    practice_dat,
    required_calibration_cols,
    "Practice calibration"
  )

  out <- practice_dat %>%
    filter(condition_code == "PRACTICE", difficulty_mode == "staircase") %>%
    mutate(
      participant_id = suppressWarnings(as.integer(participant_id)),
      trial = suppressWarnings(as.integer(trial)),
      staircase_target_accuracy = suppressWarnings(
        as.numeric(staircase_target_accuracy)
      ),
      delta_stair_realised = suppressWarnings(as.numeric(delta_stair_realised)),
      delta_stair_mean = suppressWarnings(as.numeric(delta_stair_mean)),
      decision2_correct_num = as_binary(decision2_correct)
    ) %>%
    filter(!is.na(participant_id), !is.na(trial)) %>%
    group_by(participant_id) %>%
    arrange(trial, .by_group = TRUE) %>%
    mutate(staircase_trial = row_number()) %>%
    ungroup()

  coverage <- out %>% count(participant_id, name = "n_calibration_trials")
  incomplete <- coverage %>% filter(n_calibration_trials < PRACTICE_TOTAL_N)
  if (nrow(coverage) == 0 || nrow(incomplete)) {
    stop(
      "Practice calibration files must have at least ",
      PRACTICE_TOTAL_N,
      " staircase trials. Incomplete: ",
      paste(incomplete$participant_id, collapse = ", "),
      call. = FALSE
    )
  }

  out
}

validate_trial_coverage <- function(dat) {
  coverage <- dat %>%
    count(participant_id, condition, name = "n_trials") %>%
    complete(
      participant_id,
      condition = factor(CONDITION_LABELS, levels = CONDITION_LABELS),
      fill = list(n_trials = 0)
    )

  incomplete <- coverage %>% filter(n_trials == 0)
  if (nrow(incomplete)) {
    details <- incomplete %>%
      transmute(label = paste0(participant_id, ":", condition)) %>%
      pull(label)
    stop(
      "Every participant must have all four conditions. Missing: ",
      paste(details, collapse = ", "),
      call. = FALSE
    )
  }

  coverage
}

make_participant_summary <- function(dat) {
  dat %>%
    group_by(participant_id, condition, condition_order) %>%
    summarise(
      n_trials = n(),
      decision1_accuracy = mean(decision1_correct_num, na.rm = TRUE),
      decision2_accuracy = mean(decision2_correct_num, na.rm = TRUE),
      decision1_rt = mean_finite(
        decision1_rt_valid[decision1_correct_num == 1]
      ),
      decision2_rt = mean_finite(
        decision2_rt_valid[decision2_correct_num == 1]
      ),
      changed_response_prop = mean(changed_response_num, na.rm = TRUE),
      empirical_aid_accuracy = if (all(is.na(aid_correct_num))) {
        NA_real_
      } else {
        mean(aid_correct_num, na.rm = TRUE)
      },
      target_accuracy = case_when(
        as.character(first(condition)) == "Practice" ~ {
          target <- mean(staircase_target_accuracy, na.rm = TRUE)
          if (is.finite(target)) target else PRACTICE_TARGET
        },
        as.character(first(condition)) %in% c("Aid first", "Stimulus first") ~ {
          target <- mean(aid_accuracy_setting, na.rm = TRUE)
          if (is.finite(target)) target else GLOBAL_AID_ACCURACY
        },
        TRUE ~ NA_real_
      ),
      .groups = "drop"
    ) %>%
    mutate(condition = factor(condition, levels = CONDITION_LABELS))
}

make_participant_change_rt_summary <- function(dat) {
  dat %>%
    filter(changed_response_num %in% c(0, 1)) %>%
    mutate(
      change_status = if_else(
        changed_response_num == 1,
        "Change of mind",
        "No change of mind"
      ),
      change_status = factor(change_status, levels = CHANGE_STATUS_LEVELS)
    ) %>%
    group_by(
      participant_id,
      condition,
      condition_order,
      change_status
    ) %>%
    summarise(
      n_trials = n(),
      n_decision1_correct_rt = sum(
        decision1_correct_num == 1 & is.finite(decision1_rt_valid),
        na.rm = TRUE
      ),
      n_decision2_correct_rt = sum(
        decision2_correct_num == 1 & is.finite(decision2_rt_valid),
        na.rm = TRUE
      ),
      decision1_rt = mean_finite(
        decision1_rt_valid[decision1_correct_num == 1]
      ),
      decision2_rt = mean_finite(
        decision2_rt_valid[decision2_correct_num == 1]
      ),
      .groups = "drop"
    ) %>%
    mutate(
      mean_rt_difference = decision2_rt - decision1_rt,
      condition = factor(condition, levels = CONDITION_LABELS),
      change_status = factor(change_status, levels = CHANGE_STATUS_LEVELS)
    )
}

make_participant_accuracy_difference <- function(participant_summary) {
  participant_summary %>%
    transmute(
      participant_id,
      condition,
      condition_order,
      metric = "Accuracy change",
      mean_accuracy_difference = decision2_accuracy - decision1_accuracy
    )
}

prepare_ratings <- function(dat) {
  if (!"question_key" %in% names(dat) && "slider_key" %in% names(dat)) {
    dat$question_key <- dat$slider_key
  }
  if (!"response_percent" %in% names(dat) && "response" %in% names(dat)) {
    dat$response_percent <- dat$response
  }

  validate_required_columns(
    dat,
    c("participant_id", "condition_code", "question_key", "response_percent"),
    "Slider"
  )

  out <- dat %>%
    mutate(
      participant_id = suppressWarnings(as.integer(participant_id)),
      condition = condition_factor(condition_code),
      condition_order = as.integer(condition),
      rating_measure = case_when(
        question_key == "perc_self_correct" & condition_code == "MANUAL" ~
          "Self-rated own accuracy",
        question_key == "perc_auto_correct" &
          condition_code %in% c("AIDFIRST", "STIMFIRST") ~
          "Self-rated aid accuracy",
        TRUE ~ NA_character_
      ),
      rating_measure = factor(rating_measure, levels = RATING_LEVELS),
      rated_accuracy = suppressWarnings(as.numeric(response_percent)) / 100
    ) %>%
    filter(
      !is.na(participant_id),
      !is.na(condition),
      !is.na(rating_measure),
      is.finite(rated_accuracy),
      rated_accuracy >= 0,
      rated_accuracy <= 1
    ) %>%
    group_by(participant_id, condition, condition_order, rating_measure) %>%
    summarise(rated_accuracy = mean(rated_accuracy), .groups = "drop")

  coverage <- out %>% count(participant_id, name = "n_ratings")
  incomplete <- coverage %>% filter(n_ratings != 3)
  if (nrow(coverage) == 0 || nrow(incomplete)) {
    stop(
      "Each participant must have the three expected ratings. Incomplete: ",
      paste(incomplete$participant_id, collapse = ", "),
      call. = FALSE
    )
  }

  out
}

make_rating_references <- function(participant_summary) {
  participant_summary %>%
    filter(condition %in% c("Manual", "Aid first", "Stimulus first")) %>%
    transmute(
      participant_id,
      condition,
      condition_order,
      observed_reference = if_else(
        as.character(condition) == "Manual",
        decision2_accuracy,
        empirical_aid_accuracy
      ),
      target_reference = if_else(
        as.character(condition) %in% c("Aid first", "Stimulus first"),
        target_accuracy,
        NA_real_
      )
    )
}

to_accuracy_long <- function(participant_summary) {
  participant_summary %>%
    select(
      participant_id,
      condition,
      condition_order,
      target_accuracy,
      changed_response_prop,
      `Decision 1` = decision1_accuracy,
      `Decision 2` = decision2_accuracy
    ) %>%
    pivot_longer(
      cols = all_of(DECISION_LEVELS),
      names_to = "decision",
      values_to = "mean_accuracy"
    ) %>%
    mutate(
      decision = factor(decision, levels = DECISION_LEVELS),
      x_plot = condition_order + unname(DECISION_OFFSETS[as.character(decision)])
    )
}

to_rt_long <- function(participant_summary) {
  participant_summary %>%
    select(
      participant_id,
      condition,
      condition_order,
      `Decision 1` = decision1_rt,
      `Decision 2` = decision2_rt
    ) %>%
    pivot_longer(
      cols = all_of(DECISION_LEVELS),
      names_to = "decision",
      values_to = "mean_rt"
    ) %>%
    mutate(
      decision = factor(decision, levels = DECISION_LEVELS),
      x_plot = condition_order + unname(DECISION_OFFSETS[as.character(decision)])
    )
}

to_change_rt_long <- function(participant_change_rt_summary) {
  participant_change_rt_summary %>%
    select(
      participant_id,
      condition,
      condition_order,
      change_status,
      n_trials,
      `Decision 1` = decision1_rt,
      `Decision 2` = decision2_rt
    ) %>%
    pivot_longer(
      cols = all_of(DECISION_LEVELS),
      names_to = "decision",
      values_to = "mean_rt"
    ) %>%
    mutate(
      decision = factor(decision, levels = DECISION_LEVELS),
      change_status = factor(change_status, levels = CHANGE_STATUS_LEVELS),
      x_plot = condition_order + unname(DECISION_OFFSETS[as.character(decision)])
    )
}

summarise_repeated <- function(data, value_col, condition_col, facet_cols) {
  value_sym <- rlang::sym(value_col)
  condition_sym <- rlang::sym(condition_col)

  data %>%
    filter(
      !is.na(participant_id),
      !is.na(!!condition_sym),
      is.finite(!!value_sym)
    ) %>%
    group_by(across(all_of(facet_cols))) %>%
    group_modify(function(.x, .y) {
      n_conditions <- n_distinct(.x[[condition_col]])
      n_subjects <- n_distinct(.x$participant_id)

      if (n_conditions > 1) {
        grand_mean <- mean(.x[[value_col]])
        normalized <- .x %>%
          group_by(participant_id) %>%
          mutate(
            .subject_mean = mean(!!value_sym),
            .normalized_value = !!value_sym - .subject_mean + grand_mean
          ) %>%
          ungroup()
        correction <- sqrt(n_conditions / (n_conditions - 1))
      } else {
        normalized <- .x %>% mutate(.normalized_value = !!value_sym)
        correction <- 1
      }

      normalized %>%
        group_by(!!condition_sym) %>%
        summarise(
          mean = mean(!!value_sym),
          se = if (n_subjects > 1) {
            sd(.normalized_value) /
              sqrt(n_distinct(participant_id)) * correction
          } else {
            NA_real_
          },
          n_participants = n_distinct(participant_id),
          .groups = "drop"
        )
    }) %>%
    ungroup()
}

summarise_repeated_complete <- function(
  data,
  value_col,
  condition_col,
  facet_cols,
  expected_conditions = CONDITION_LABELS
) {
  value_sym <- rlang::sym(value_col)
  condition_sym <- rlang::sym(condition_col)

  complete_data <- data %>%
    filter(
      !is.na(participant_id),
      !is.na(!!condition_sym),
      is.finite(!!value_sym)
    ) %>%
    group_by(across(all_of(c(facet_cols, "participant_id")))) %>%
    filter(
      n_distinct(!!condition_sym) == length(expected_conditions),
      all(expected_conditions %in% as.character(!!condition_sym))
    ) %>%
    ungroup()

  if (nrow(complete_data) == 0) {
    stop(
      "No participants have complete condition coverage for ",
      value_col,
      ".",
      call. = FALSE
    )
  }

  summarise_repeated(
    complete_data,
    value_col,
    condition_col,
    facet_cols
  )
}

make_target_references <- function(participant_summary) {
  participant_summary %>%
    filter(!is.na(target_accuracy)) %>%
    group_by(condition, condition_order) %>%
    summarise(target_accuracy = mean(target_accuracy), .groups = "drop")
}

decision_scales <- function() {
  list(
    scale_colour_manual(values = DECISION_COLOURS, name = NULL),
    scale_shape_manual(values = DECISION_SHAPES, name = NULL)
  )
}

change_status_scales <- function() {
  list(
    scale_colour_manual(values = CHANGE_STATUS_COLOURS, name = NULL),
    scale_shape_manual(values = CHANGE_STATUS_SHAPES, name = NULL)
  )
}

accuracy_scales <- function() {
  series_levels <- c(DECISION_LEVELS, RATING_LEVELS)
  list(
    scale_colour_manual(
      values = c(DECISION_COLOURS, RATING_COLOURS),
      breaks = series_levels,
      name = NULL
    ),
    scale_shape_manual(
      values = c(DECISION_SHAPES, RATING_SHAPES),
      breaks = series_levels,
      name = NULL
    )
  )
}

condition_axis <- function(include_practice = TRUE) {
  if (include_practice) {
    breaks <- seq_along(CONDITION_LABELS)
    labels <- CONDITION_LABELS
  } else {
    breaks <- seq_along(CONDITION_LABELS)[-1]
    labels <- CONDITION_LABELS[-1]
  }

  scale_x_continuous(
    breaks = breaks,
    labels = labels,
    limits = range(breaks) + c(-0.35, 0.35)
  )
}

get_axis_limits <- function(values, ses = NULL, bounds = c(-Inf, Inf), pad = 0.18) {
  if (is.null(ses)) ses <- rep(0, length(values))
  lo <- suppressWarnings(min(values - ses, na.rm = TRUE))
  hi <- suppressWarnings(max(values + ses, na.rm = TRUE))

  if (!is.finite(lo) || !is.finite(hi)) return(bounds)

  span <- hi - lo
  extra <- if (span > 0) span * pad else max(abs(lo) * 0.05, 0.05)
  c(max(bounds[[1]], lo - extra), min(bounds[[2]], hi + extra))
}

make_calibration_plot <- function(calibration_dat, participant_label) {
  calibration_dat <- calibration_dat %>%
    arrange(staircase_trial) %>%
    mutate(
      observed_n = cumsum(!is.na(decision2_correct_num)),
      accuracy_running = if_else(
        observed_n > 0,
        cumsum(replace_na(decision2_correct_num, 0)) / observed_n,
        NA_real_
      ),
      accuracy_rolling = zoo::rollapply(
        decision2_correct_num,
        width = CALIBRATION_ROLLING_N,
        FUN = function(x) mean(x, na.rm = TRUE),
        align = "right",
        fill = NA_real_,
        partial = TRUE
      )
    )

  final_trials <- calibration_dat %>%
    slice_tail(n = PRACTICE_KEEP_N)
  final_start <- min(final_trials$staircase_trial)
  target_values <- unique(
    calibration_dat$staircase_target_accuracy[
      is.finite(calibration_dat$staircase_target_accuracy)
    ]
  )
  if (length(target_values) != 1) {
    stop(
      participant_label,
      " must have exactly one finite staircase target.",
      call. = FALSE
    )
  }

  target_accuracy <- target_values[[1]]
  delta_mean <- mean(final_trials$delta_stair_realised, na.rm = TRUE)
  delta_sd <- sd(final_trials$delta_stair_realised, na.rm = TRUE)
  if (!is.finite(delta_sd)) {
    delta_sd <- 0
  }
  whole_accuracy <- mean(calibration_dat$decision2_correct_num, na.rm = TRUE)
  final_accuracy <- mean(final_trials$decision2_correct_num, na.rm = TRUE)
  delta_upper <- max(
    c(
      calibration_dat$delta_stair_realised,
      calibration_dat$delta_stair_mean,
      calibration_dat$delta_stair_mean + DELTA_SD,
      delta_mean + delta_sd
    ),
    na.rm = TRUE
  )
  delta_y_max <- max(0.01, ceiling(delta_upper * 100) / 100)
  participant_id_label <- sub("^Participant 0*", "", participant_label)

  p_delta <- ggplot(calibration_dat, aes(x = staircase_trial)) +
    geom_ribbon(
      data = calibration_dat %>% filter(staircase_trial >= final_start),
      aes(
        ymin = pmax(0, delta_mean - delta_sd),
        ymax = delta_mean + delta_sd
      ),
      fill = "orange",
      alpha = 0.35
    ) +
    geom_ribbon(
      aes(
        ymin = pmax(0, delta_stair_mean - DELTA_SD),
        ymax = delta_stair_mean + DELTA_SD
      ),
      fill = "purple",
      alpha = 0.20
    ) +
    geom_point(
      aes(y = delta_stair_realised),
      colour = "orange",
      size = 1
    ) +
    geom_line(
      aes(y = delta_stair_mean),
      colour = "purple",
      linewidth = 0.75
    ) +
    geom_hline(
      yintercept = delta_mean,
      colour = "orange",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    coord_cartesian(ylim = c(0, delta_y_max)) +
    labs(
      x = NULL,
      y = "Delta",
      title = paste0(
        "Calibration dynamics; participant ",
        participant_id_label
      ),
      subtitle = paste0(
        "Target accuracy: ", sprintf("%.2f", target_accuracy),
        "; last ", PRACTICE_KEEP_N,
        " post-burn-in trials delta mean = ", sprintf("%.3f", delta_mean)
      )
    ) +
    theme_classic()

  p_accuracy <- ggplot(calibration_dat, aes(x = staircase_trial)) +
    geom_point(
      aes(y = decision2_correct_num),
      shape = 4,
      size = 1,
      alpha = 0.45,
      na.rm = TRUE
    ) +
    geom_line(
      aes(y = accuracy_rolling),
      linewidth = 0.65,
      colour = "steelblue"
    ) +
    geom_line(
      aes(y = accuracy_running),
      linewidth = 0.75,
      colour = "orange"
    ) +
    geom_hline(
      yintercept = target_accuracy,
      colour = "purple",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    geom_hline(
      yintercept = final_accuracy,
      colour = "orange",
      linewidth = 0.5,
      alpha = 0.5
    ) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      hjust = 1.05,
      vjust = -7,
      size = 3.5,
      label = sprintf("Target acc = %.2f", target_accuracy)
    ) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      hjust = 1.05,
      vjust = -5,
      size = 3.5,
      label = sprintf("Whole-block acc = %.2f", whole_accuracy)
    ) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      hjust = 1.05,
      vjust = -3,
      size = 3.5,
      label = sprintf(
        "Last %d acc = %.2f",
        PRACTICE_KEEP_N,
        final_accuracy
      )
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1)
    ) +
    labs(
      x = "Trial",
      y = "Accuracy"
    ) +
    theme_classic()

  p_delta / p_accuracy
}

make_accuracy_plot <- function(acc_dat, rating_dat, references, title, subtitle) {
  acc_ses <- if ("se" %in% names(acc_dat)) replace_na(acc_dat$se, 0) else {
    rep(0, nrow(acc_dat))
  }
  rating_ses <- if ("se" %in% names(rating_dat)) {
    replace_na(rating_dat$se, 0)
  } else {
    rep(0, nrow(rating_dat))
  }
  accuracy_limits <- get_axis_limits(
    c(
      acc_dat$mean_accuracy,
      rating_dat$rated_accuracy,
      references$target_accuracy
    ),
    c(acc_ses, rating_ses, rep(0, nrow(references))),
    bounds = c(0, 1),
    pad = 0.12
  )
  rating_dat <- rating_dat %>%
    mutate(
      rating_label_se = if ("se" %in% names(.)) replace_na(se, 0) else 0,
      rating_label_y = rated_accuracy - rating_label_se - 0.012
    )

  p <- ggplot(
    acc_dat,
    aes(x = x_plot, y = mean_accuracy, colour = decision, shape = decision)
  ) +
    geom_segment(
      data = references,
      aes(
        x = condition_order - 0.28,
        xend = condition_order + 0.28,
        y = target_accuracy,
        yend = target_accuracy
      ),
      inherit.aes = FALSE,
      colour = "black",
      linetype = "dashed",
      linewidth = 0.8
    )

  if ("se" %in% names(acc_dat)) {
    p <- p +
      geom_errorbar(
        aes(ymin = mean_accuracy - se, ymax = mean_accuracy + se),
        width = 0.06,
        linewidth = 0.55,
        show.legend = FALSE
      )
  }

  if ("se" %in% names(rating_dat)) {
    p <- p +
      geom_errorbar(
        data = rating_dat,
        aes(
          x = condition_order,
          ymin = rated_accuracy - se,
          ymax = rated_accuracy + se,
          colour = rating_measure
        ),
        inherit.aes = FALSE,
        width = 0.08,
        linewidth = 0.55,
        show.legend = FALSE
      )
  }

  p <- p +
    geom_line(aes(group = decision), linewidth = 0.85) +
    geom_point(size = 3) +
    geom_text(
      aes(
        label = sprintf("%.1f%%", mean_accuracy * 100),
        vjust = if_else(decision == "Decision 1", 1.7, -0.9)
      ),
      size = 3.2,
      fontface = "bold",
      show.legend = FALSE
    ) +
    geom_point(
      data = rating_dat,
      aes(
        x = condition_order,
        y = rated_accuracy,
        colour = rating_measure,
        shape = rating_measure
      ),
      inherit.aes = FALSE,
      size = 3.4
    ) +
    geom_text(
      data = rating_dat,
      aes(
        x = condition_order,
        y = rating_label_y,
        label = sprintf("%.1f%%", rated_accuracy * 100),
        colour = rating_measure
      ),
      inherit.aes = FALSE,
      vjust = 1,
      size = 3.2,
      fontface = "bold",
      show.legend = FALSE
    ) +
    condition_axis() +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    coord_cartesian(ylim = accuracy_limits, clip = "off") +
    labs(
      x = NULL,
      y = "Mean accuracy",
      title = str_wrap(title, width = 66),
      subtitle = str_wrap(subtitle, width = 88),
      caption = str_wrap(
        paste0(
          "Black dashes: Practice target or assigned aid accuracy; ",
          "squares/diamonds: post-block self-ratings"
        ),
        width = 105
      )
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 15, hjust = 1),
      legend.position = "bottom"
    )

  p + accuracy_scales()
}

make_change_plot <- function(change_dat, title, subtitle) {
  change_max <- if ("se" %in% names(change_dat)) {
    max(change_dat$changed_response_prop + replace_na(change_dat$se, 0), na.rm = TRUE)
  } else {
    max(change_dat$changed_response_prop, na.rm = TRUE)
  }
  if (!is.finite(change_max)) change_max <- 0.1
  change_ylim <- c(0, max(0.1, change_max * 1.25))

  p <- ggplot(
    change_dat,
    aes(x = condition_order, y = changed_response_prop, group = 1)
  )

  if ("se" %in% names(change_dat)) {
    p <- p +
      geom_errorbar(
        aes(
          ymin = pmax(0, changed_response_prop - se),
          ymax = changed_response_prop + se
        ),
        width = 0.08,
        linewidth = 0.55,
        colour = "#6A3D9A"
      )
  }

  p +
    geom_line(colour = "#6A3D9A", linewidth = 0.85) +
    geom_point(colour = "#6A3D9A", shape = 18, size = 3.2) +
    geom_text(
      aes(label = sprintf("%.1f%%", changed_response_prop * 100)),
      vjust = -0.9,
      size = 3.2,
      fontface = "bold",
      colour = "#6A3D9A"
    ) +
    condition_axis() +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    coord_cartesian(ylim = change_ylim, clip = "off") +
    labs(
      x = NULL,
      y = "Changed response",
      title = str_wrap(title, width = 66),
      subtitle = str_wrap(subtitle, width = 88)
    ) +
    theme_classic(base_size = 11) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))
}

make_rt_plot <- function(rt_dat, title, subtitle) {
  rt_max <- if ("se" %in% names(rt_dat)) {
    max(rt_dat$mean_rt + rt_dat$se, na.rm = TRUE)
  } else {
    max(rt_dat$mean_rt, na.rm = TRUE)
  }
  if (!is.finite(rt_max)) rt_max <- 1

  rt_dat <- rt_dat %>%
    mutate(
      label_se = if ("se" %in% names(.)) replace_na(se, 0) else 0,
      label_y = if_else(
        decision == "Decision 1",
        mean_rt + label_se + rt_max * 0.035,
        pmax(0, mean_rt - label_se - rt_max * 0.035)
      )
    )

  p <- ggplot(
    rt_dat,
    aes(x = x_plot, y = mean_rt, colour = decision, shape = decision)
  )

  if ("se" %in% names(rt_dat)) {
    p <- p +
      geom_errorbar(
        aes(ymin = mean_rt - se, ymax = mean_rt + se),
        width = 0.06,
        linewidth = 0.55,
        show.legend = FALSE
      )
  }

  p <- p +
    geom_line(aes(group = decision), linewidth = 0.85) +
    geom_point(size = 3) +
    geom_text(
      aes(
        y = label_y,
        label = sprintf("%.3f s", mean_rt),
        vjust = if_else(decision == "Decision 1", 0, 1)
      ),
      size = 3.2,
      fontface = "bold",
      show.legend = FALSE
    ) +
    condition_axis() +
    coord_cartesian(ylim = c(0, max(rt_dat$label_y) * 1.08), clip = "off") +
    labs(
      x = "Condition",
      y = "Mean correct RT (s)",
      title = str_wrap(title, width = 66),
      subtitle = str_wrap(subtitle, width = 88)
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 15, hjust = 1),
      legend.position = "bottom"
    )

  p + decision_scales()
}

make_change_dynamics_plot <- function(
  rt_dat,
  difference_dat,
  accuracy_difference_dat,
  title,
  subtitle
) {
  rt_ses <- if ("se" %in% names(rt_dat)) replace_na(rt_dat$se, 0) else {
    rep(0, nrow(rt_dat))
  }
  rt_max <- max(rt_dat$mean_rt + rt_ses, na.rm = TRUE)
  if (!is.finite(rt_max)) rt_max <- 1

  rt_dat <- rt_dat %>%
    mutate(
      change_status_order = as.integer(
        factor(change_status, levels = CHANGE_STATUS_LEVELS)
      ),
      x_plot = change_status_order +
        unname(DECISION_OFFSETS[as.character(decision)]),
      label_se = if ("se" %in% names(.)) replace_na(se, 0) else 0,
      label_y = if_else(
        decision == "Decision 1",
        mean_rt + label_se + rt_max * 0.035,
        pmax(0, mean_rt - label_se - rt_max * 0.035)
      ),
      label_vjust = if_else(decision == "Decision 1", 0, 1)
    )
  rt_lines <- rt_dat %>%
    group_by(condition, decision) %>%
    filter(sum(is.finite(mean_rt)) > 1) %>%
    ungroup()

  p_rt <- ggplot(
    rt_dat,
    aes(x = x_plot, y = mean_rt, colour = decision, shape = decision)
  )

  if ("se" %in% names(rt_dat)) {
    p_rt <- p_rt +
      geom_errorbar(
        aes(ymin = mean_rt - se, ymax = mean_rt + se),
        width = 0.06,
        linewidth = 0.55,
        show.legend = FALSE
      )
  }

  p_rt <- p_rt +
    geom_line(
      data = rt_lines,
      aes(group = decision),
      linewidth = 0.85
    ) +
    geom_point(size = 3) +
    geom_text(
      aes(
        y = label_y,
        label = sprintf("%.3f s", mean_rt),
        vjust = label_vjust
      ),
      size = 3,
      fontface = "bold",
      show.legend = FALSE
    ) +
    facet_wrap(~condition, nrow = 1) +
    scale_x_continuous(
      breaks = seq_along(CHANGE_STATUS_LEVELS),
      labels = CHANGE_STATUS_LEVELS,
      limits = c(0.65, length(CHANGE_STATUS_LEVELS) + 0.35)
    ) +
    coord_cartesian(
      ylim = c(0, max(rt_dat$label_y, na.rm = TRUE) * 1.08),
      clip = "off"
    ) +
    labs(
      x = "Change-of-mind status",
      y = "Mean correct RT (s)",
      title = str_wrap(title, width = 72),
      subtitle = str_wrap(subtitle, width = 100)
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    ) +
    decision_scales()

  difference_ses <- if ("se" %in% names(difference_dat)) {
    replace_na(difference_dat$se, 0)
  } else {
    rep(0, nrow(difference_dat))
  }
  difference_span <- diff(
    range(
      c(
        difference_dat$mean_rt_difference - difference_ses,
        difference_dat$mean_rt_difference + difference_ses,
        0
      ),
      na.rm = TRUE
    )
  )
  if (!is.finite(difference_span) || difference_span == 0) {
    difference_span <- 0.1
  }

  difference_dat <- difference_dat %>%
    mutate(
      change_status = factor(change_status, levels = CHANGE_STATUS_LEVELS),
      x_plot = condition_order +
        unname(CHANGE_STATUS_OFFSETS[as.character(change_status)]),
      label_se = if ("se" %in% names(.)) replace_na(se, 0) else 0,
      label_y = if_else(
        change_status == "No change of mind",
        mean_rt_difference + label_se + difference_span * 0.045,
        mean_rt_difference - label_se - difference_span * 0.045
      ),
      label_vjust = if_else(change_status == "No change of mind", 0, 1)
    )
  difference_lines <- difference_dat %>%
    group_by(change_status) %>%
    filter(sum(is.finite(mean_rt_difference)) > 1) %>%
    ungroup()

  difference_limits <- range(
    c(
      difference_dat$mean_rt_difference - difference_dat$label_se,
      difference_dat$mean_rt_difference + difference_dat$label_se,
      difference_dat$label_y,
      0
    ),
    na.rm = TRUE
  )
  difference_limits <- difference_limits +
    c(-1, 1) * max(diff(difference_limits), 0.1) * 0.08

  p_difference <- ggplot(
    difference_dat,
    aes(
      x = x_plot,
      y = mean_rt_difference,
      colour = change_status,
      shape = change_status
    )
  ) +
    geom_hline(yintercept = 0, colour = "grey45", linetype = "dashed")

  if ("se" %in% names(difference_dat)) {
    p_difference <- p_difference +
      geom_errorbar(
        aes(
          ymin = mean_rt_difference - se,
          ymax = mean_rt_difference + se
        ),
        width = 0.06,
        linewidth = 0.55,
        show.legend = FALSE
      )
  }

  p_difference <- p_difference +
    geom_line(
      data = difference_lines,
      aes(group = change_status),
      linewidth = 0.85
    ) +
    geom_point(size = 3) +
    geom_text(
      aes(
        y = label_y,
        label = sprintf("%+.3f s", mean_rt_difference),
        vjust = label_vjust
      ),
      size = 3,
      fontface = "bold",
      show.legend = FALSE
    ) +
    condition_axis() +
    coord_cartesian(ylim = difference_limits, clip = "off") +
    labs(
      x = "Condition",
      y = "Mean correct RT difference (s)",
      title = "Decision 2 - Decision 1 mean correct RT",
      caption = str_wrap(
        paste0(
          "For each participant, the Decision 1 correct-trial mean is ",
          "subtracted from the Decision 2 correct-trial mean; negative ",
          "values mean correct Decision 2 responses were faster."
        ),
        width = 105
      )
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 15, hjust = 1),
      legend.position = "bottom"
    ) +
    change_status_scales()

  accuracy_difference_ses <- if ("se" %in% names(accuracy_difference_dat)) {
    replace_na(accuracy_difference_dat$se, 0)
  } else {
    rep(0, nrow(accuracy_difference_dat))
  }
  accuracy_difference_span <- diff(
    range(
      c(
        accuracy_difference_dat$mean_accuracy_difference -
          accuracy_difference_ses,
        accuracy_difference_dat$mean_accuracy_difference +
          accuracy_difference_ses,
        0
      ),
      na.rm = TRUE
    )
  )
  if (!is.finite(accuracy_difference_span) || accuracy_difference_span == 0) {
    accuracy_difference_span <- 0.1
  }

  accuracy_difference_dat <- accuracy_difference_dat %>%
    mutate(
      x_plot = condition_order,
      label_se = if ("se" %in% names(.)) replace_na(se, 0) else 0,
      label_y = if_else(
        mean_accuracy_difference >= 0,
          mean_accuracy_difference + label_se + accuracy_difference_span * 0.045,
          mean_accuracy_difference - label_se - accuracy_difference_span * 0.045
      ),
      label_vjust = if_else(mean_accuracy_difference >= 0, 0, 1)
    )

  accuracy_difference_limits <- range(
    c(
      accuracy_difference_dat$mean_accuracy_difference -
        accuracy_difference_dat$label_se,
      accuracy_difference_dat$mean_accuracy_difference +
        accuracy_difference_dat$label_se,
      accuracy_difference_dat$label_y,
      0
    ),
    na.rm = TRUE
  )
  accuracy_difference_limits <- accuracy_difference_limits +
    c(-1, 1) * max(diff(accuracy_difference_limits), 0.1) * 0.08
  accuracy_difference_limits <- pmax(
    -1,
    pmin(1, accuracy_difference_limits)
  )

  p_accuracy_difference <- ggplot(
    accuracy_difference_dat,
    aes(x = x_plot, y = mean_accuracy_difference, group = 1)
  ) +
    geom_hline(yintercept = 0, colour = "grey45", linetype = "dashed")

  if ("se" %in% names(accuracy_difference_dat)) {
    p_accuracy_difference <- p_accuracy_difference +
      geom_errorbar(
        aes(
          ymin = mean_accuracy_difference - se,
          ymax = mean_accuracy_difference + se
        ),
        width = 0.06,
        linewidth = 0.55,
        colour = CHANGE_STATUS_COLOURS[["Change of mind"]],
        show.legend = FALSE
      )
  }

  p_accuracy_difference <- p_accuracy_difference +
    geom_line(
      colour = CHANGE_STATUS_COLOURS[["Change of mind"]],
      linewidth = 0.85
    ) +
    geom_point(
      colour = CHANGE_STATUS_COLOURS[["Change of mind"]],
      shape = CHANGE_STATUS_SHAPES[["Change of mind"]],
      size = 3
    ) +
    geom_text(
      aes(
        y = label_y,
        label = sprintf("%+.1f pp", mean_accuracy_difference * 100),
        vjust = label_vjust
      ),
      size = 3,
      fontface = "bold",
      colour = CHANGE_STATUS_COLOURS[["Change of mind"]],
      show.legend = FALSE
    ) +
    condition_axis() +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    coord_cartesian(ylim = accuracy_difference_limits, clip = "off") +
    labs(
      x = "Condition",
      y = "Mean accuracy change",
      title = "Decision 2 - Decision 1 accuracy",
      caption = str_wrap(
        paste0(
          "Overall condition-level accuracy differences; unchanged-response ",
          "trials contribute zero, so the net difference is attributable to ",
          "change-of-mind trials. Positive values indicate improved accuracy ",
          "at Decision 2."
        ),
        width = 105
      )
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 15, hjust = 1),
      legend.position = "bottom"
    )

  p_rt / p_difference / p_accuracy_difference +
    plot_layout(heights = c(1, 0.9, 0.9), guides = "collect") &
    theme(legend.position = "bottom")
}

make_rating_plot <- function(rating_dat, references, title, subtitle) {
  rating_lines <- rating_dat %>%
    group_by(rating_measure) %>%
    filter(n() > 1) %>%
    ungroup()

  p <- ggplot(
    rating_dat,
    aes(
      x = condition_order,
      y = rated_accuracy,
      colour = rating_measure,
      shape = rating_measure
    )
  ) +
    geom_segment(
      data = references,
      aes(
        x = condition_order - 0.12,
        xend = condition_order + 0.12,
        y = observed_reference,
        yend = observed_reference
      ),
      inherit.aes = FALSE,
      colour = "black",
      linewidth = 0.85
    ) +
    geom_segment(
      data = references %>% filter(!is.na(target_reference)),
      aes(
        x = condition_order - 0.24,
        xend = condition_order + 0.24,
        y = target_reference,
        yend = target_reference
      ),
      inherit.aes = FALSE,
      colour = "black",
      linetype = "dashed",
      linewidth = 0.8
    )

  if ("se" %in% names(rating_dat)) {
    p <- p +
      geom_errorbar(
        aes(ymin = rated_accuracy - se, ymax = rated_accuracy + se),
        width = 0.08,
        linewidth = 0.55,
        show.legend = FALSE
      )
  }

  p +
    geom_line(
      data = rating_lines,
      aes(group = rating_measure),
      linewidth = 0.8
    ) +
    geom_point(size = 3) +
    geom_text(
      aes(
        label = sprintf("%.1f%%", rated_accuracy * 100),
        vjust = if_else(rated_accuracy >= 0.94, 1.7, -0.9)
      ),
      size = 3.2,
      fontface = "bold",
      show.legend = FALSE
    ) +
    condition_axis(include_practice = FALSE) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    scale_colour_manual(values = RATING_COLOURS, name = NULL) +
    scale_shape_manual(values = RATING_SHAPES, name = NULL) +
    coord_cartesian(ylim = c(0, 1.08), clip = "off") +
    labs(
      x = NULL,
      y = "Self-rated accuracy",
      title = str_wrap(title, width = 66),
      subtitle = str_wrap(subtitle, width = 88),
      caption = str_wrap(
        paste0(
          "Black solid segments: observed final accuracy (Manual) or empirical aid ",
          "accuracy (aided conditions); black dashes: assigned aid accuracy"
        ),
        width = 105
      )
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 15, hjust = 1),
      legend.position = "bottom"
    )
}

make_individual_plots <- function(
  participant_summary,
  participant_change_rt_summary,
  participant_ratings,
  rating_references,
  participant_label
) {
  acc_dat <- to_accuracy_long(participant_summary)
  rt_dat <- to_rt_long(participant_summary)
  change_rt_dat <- to_change_rt_long(participant_change_rt_summary)
  accuracy_difference_dat <- make_participant_accuracy_difference(
    participant_summary
  )
  targets <- make_target_references(participant_summary)
  changes <- participant_summary %>%
    select(participant_id, condition, condition_order, changed_response_prop)
  common_subtitle <- PRACTICE_PLOT_LABEL

  p_acc <- make_accuracy_plot(
    acc_dat,
    participant_ratings,
    targets,
    paste0(participant_label, " accuracy and self-ratings by condition"),
    common_subtitle
  )
  p_change <- make_change_plot(
    changes,
    paste0(participant_label, " response changes by condition"),
    common_subtitle
  )
  p_rt <- make_rt_plot(
    rt_dat,
    paste0(participant_label, " decision correct RT by condition"),
    common_subtitle
  )
  p_rt_change <- make_change_dynamics_plot(
    change_rt_dat,
    participant_change_rt_summary,
    accuracy_difference_dat,
    paste0(
      participant_label,
      " decision correct RT by change-of-mind status and overall accuracy change"
    ),
    common_subtitle
  )
  p_rating <- make_rating_plot(
    participant_ratings,
    rating_references,
    paste0(participant_label, " self-rated accuracy by condition"),
    "Manual rates own accuracy; aided conditions rate aid accuracy"
  )

  combined <- (
    p_acc + labs(title = NULL, caption = NULL) + theme(legend.position = "bottom")
  ) / (
    p_change + labs(title = NULL)
  ) / (
    p_rt + labs(title = NULL) + theme(legend.position = "bottom")
  ) +
    plot_annotation(
      title = paste0(
        STUDY_TITLE,
        ": ",
        participant_label,
        " accuracy/self-ratings, response changes, and correct RT"
      )
    )

  list(
    accuracy = p_acc,
    change = p_change,
    rating = p_rating,
    rt = p_rt,
    rt_change = p_rt_change,
    combined = combined
  )
}

make_group_plots <- function(
  participant_summary,
  participant_change_rt_summary,
  participant_ratings,
  rating_references,
  plot_label
) {
  participant_acc <- to_accuracy_long(participant_summary)
  participant_rt <- to_rt_long(participant_summary)
  participant_change_rt_long <- to_change_rt_long(
    participant_change_rt_summary
  )
  participant_accuracy_difference <- make_participant_accuracy_difference(
    participant_summary
  )

  acc_summary <- summarise_repeated(
    participant_acc,
    "mean_accuracy",
    "condition",
    "decision"
  ) %>%
    mutate(
      condition = factor(condition, levels = CONDITION_LABELS),
      condition_order = as.integer(condition),
      x_plot = condition_order + unname(DECISION_OFFSETS[as.character(decision)]),
      mean_accuracy = mean
    )

  rt_summary <- summarise_repeated(
    participant_rt,
    "mean_rt",
    "condition",
    "decision"
  ) %>%
    mutate(
      condition = factor(condition, levels = CONDITION_LABELS),
      condition_order = as.integer(condition),
      x_plot = condition_order + unname(DECISION_OFFSETS[as.character(decision)]),
      mean_rt = mean
    )

  change_rt_summary <- summarise_repeated_complete(
    participant_change_rt_long,
    "mean_rt",
    "condition",
    c("change_status", "decision")
  ) %>%
    mutate(
      condition = factor(condition, levels = CONDITION_LABELS),
      condition_order = as.integer(condition),
      change_status = factor(change_status, levels = CHANGE_STATUS_LEVELS),
      decision = factor(decision, levels = DECISION_LEVELS),
      x_plot = condition_order +
        unname(DECISION_OFFSETS[as.character(decision)]),
      mean_rt = mean
    )

  change_rt_difference_summary <- summarise_repeated_complete(
    participant_change_rt_summary,
    "mean_rt_difference",
    "condition",
    "change_status"
  ) %>%
    mutate(
      condition = factor(condition, levels = CONDITION_LABELS),
      condition_order = as.integer(condition),
      change_status = factor(change_status, levels = CHANGE_STATUS_LEVELS),
      mean_rt_difference = mean
    )

  change_accuracy_difference_summary <- summarise_repeated_complete(
    participant_accuracy_difference,
    "mean_accuracy_difference",
    "condition",
    "metric"
  ) %>%
    mutate(
      condition = factor(condition, levels = CONDITION_LABELS),
      condition_order = as.integer(condition),
      mean_accuracy_difference = mean
    )

  change_rt_complete_n <- participant_change_rt_long %>%
    filter(is.finite(mean_rt)) %>%
    group_by(change_status, decision, participant_id) %>%
    summarise(n_conditions = n_distinct(condition), .groups = "drop") %>%
    filter(n_conditions == length(CONDITION_LABELS)) %>%
    count(change_status, decision, name = "n_complete") %>%
    complete(
      change_status = factor(
        CHANGE_STATUS_LEVELS,
        levels = CHANGE_STATUS_LEVELS
      ),
      decision = factor(DECISION_LEVELS, levels = DECISION_LEVELS),
      fill = list(n_complete = 0)
    ) %>%
    arrange(change_status, decision)

  change_difference_complete_n <- participant_change_rt_summary %>%
    filter(is.finite(mean_rt_difference)) %>%
    group_by(change_status, participant_id) %>%
    summarise(n_conditions = n_distinct(condition), .groups = "drop") %>%
    filter(n_conditions == length(CONDITION_LABELS)) %>%
    count(change_status, name = "n_complete") %>%
    complete(
      change_status = factor(
        CHANGE_STATUS_LEVELS,
        levels = CHANGE_STATUS_LEVELS
      ),
      fill = list(n_complete = 0)
    ) %>%
    arrange(change_status)

  change_rt_n_text <- change_rt_complete_n %>%
    transmute(
      label = paste0(
        as.character(change_status),
        " ",
        as.character(decision),
        " n = ",
        n_complete
      )
    ) %>%
    pull(label) %>%
    paste(collapse = ", ")

  change_difference_n_text <- change_difference_complete_n %>%
    transmute(
      label = paste0(as.character(change_status), " n = ", n_complete)
    ) %>%
    pull(label) %>%
    paste(collapse = ", ")

  rating_summary <- summarise_repeated(
    participant_ratings %>% mutate(rating_set = "Self-rating"),
    "rated_accuracy",
    "condition",
    "rating_set"
  ) %>%
    left_join(
      participant_ratings %>% distinct(condition, rating_measure),
      by = "condition"
    ) %>%
    mutate(
      condition = factor(condition, levels = CONDITION_LABELS),
      condition_order = as.integer(condition),
      rated_accuracy = mean
    )

  change_summary <- summarise_repeated(
    participant_summary %>% mutate(metric = "Response change"),
    "changed_response_prop",
    "condition",
    "metric"
  ) %>%
    mutate(
      condition = factor(condition, levels = CONDITION_LABELS),
      condition_order = as.integer(condition),
      changed_response_prop = mean
    )

  target_refs <- make_target_references(participant_summary)
  group_rating_refs <- rating_references %>%
    group_by(condition, condition_order) %>%
    summarise(
      observed_reference = mean(observed_reference),
      target_reference = if (all(is.na(target_reference))) {
        NA_real_
      } else {
        mean(target_reference, na.rm = TRUE)
      },
      .groups = "drop"
    )

  n_participants <- n_distinct(participant_summary$participant_id)
  common_subtitle <- paste0(
    "Morey-Cousineau within-participant SEs across 4 conditions; n = ",
    n_participants,
    "; ",
    PRACTICE_PLOT_LABEL
  )

  p_acc <- make_accuracy_plot(
    acc_summary,
    rating_summary,
    target_refs,
    paste0(plot_label, " group accuracy and self-ratings by condition"),
    paste0(
      "Morey-Cousineau within-participant SEs: decisions across 4 conditions, ",
      "self-ratings across 3 measured conditions; n = ",
      n_participants,
      "; ",
      PRACTICE_PLOT_LABEL
    )
  )

  p_change <- make_change_plot(
    change_summary,
    paste0(plot_label, " group response changes by condition"),
    common_subtitle
  )

  p_rt <- make_rt_plot(
    rt_summary,
    paste0(plot_label, " group decision correct RT by condition"),
    common_subtitle
  )

  rt_change_subtitle <- paste0(
    "Participant means with Morey-Cousineau within-participant SEs across 4 ",
    "conditions; complete participants for correct RT: ",
    change_rt_n_text,
    "; correct-RT difference: ",
    change_difference_n_text,
    "; accuracy difference n = ",
    n_participants,
    "; ",
    PRACTICE_PLOT_LABEL
  )
  p_rt_change <- make_change_dynamics_plot(
    change_rt_summary,
    change_rt_difference_summary,
    change_accuracy_difference_summary,
    paste0(
      plot_label,
      " group decision correct RT by change-of-mind status and overall accuracy change"
    ),
    rt_change_subtitle
  )

  p_rating <- make_rating_plot(
    rating_summary,
    group_rating_refs,
    paste0(plot_label, " group self-rated accuracy by condition"),
    paste0(
      "Morey-Cousineau within-participant SEs across 3 measured conditions; n = ",
      n_participants
    )
  )

  combined <- (
    p_acc + labs(title = NULL, caption = NULL) + theme(legend.position = "bottom")
  ) / (
    p_change + labs(title = NULL)
  ) / (
    p_rt + labs(title = NULL) + theme(legend.position = "bottom")
  ) +
    plot_annotation(
      title = paste0(
        STUDY_TITLE,
        ": ",
        plot_label,
        " group accuracy/self-ratings, response changes, and correct RT"
      )
    )

  list(
    accuracy = p_acc,
    change = p_change,
    rating = p_rating,
    rt = p_rt,
    rt_change = p_rt_change,
    combined = combined,
    accuracy_summary = acc_summary,
    change_summary = change_summary,
    rating_summary = rating_summary,
    rt_summary = rt_summary,
    change_rt_summary = change_rt_summary,
    change_rt_difference_summary = change_rt_difference_summary,
    change_accuracy_difference_summary = change_accuracy_difference_summary
  )
}

make_participant_aid_outcome_summaries <- function(trial_dat) {
  decision_trials <- trial_dat %>%
    pivot_longer(
      cols = matches("^decision[12]_(correct_num|rt_valid)$"),
      names_to = c("decision_number", ".value"),
      names_pattern = "^decision([12])_(correct_num|rt_valid)$"
    ) %>%
    mutate(
      decision = factor(
        paste("Decision", decision_number),
        levels = DECISION_LEVELS
      )
    )

  aided_accuracy <- decision_trials %>%
    filter(
      as.character(condition) %in% AID_OUTCOME_FACET_LEVELS,
      aid_correct_num %in% c(0, 1),
      is.finite(correct_num)
    ) %>%
    mutate(
      facet_condition = as.character(condition),
      aid_outcome = if_else(
        aid_correct_num == 1,
        "Aid correct",
        "Aid incorrect"
      )
    ) %>%
    group_by(participant_id, facet_condition, aid_outcome, decision) %>%
    summarise(
      mean_accuracy = mean(correct_num),
      n_trials = n(),
      .groups = "drop"
    )

  manual_accuracy <- decision_trials %>%
    filter(
      as.character(condition) == "Manual",
      is.finite(correct_num)
    ) %>%
    group_by(participant_id, decision) %>%
    summarise(
      mean_accuracy = mean(correct_num),
      n_trials = n(),
      .groups = "drop"
    ) %>%
    crossing(facet_condition = AID_OUTCOME_FACET_LEVELS) %>%
    mutate(aid_outcome = "Manual")

  aided_rt <- decision_trials %>%
    filter(
      as.character(condition) %in% AID_OUTCOME_FACET_LEVELS,
      aid_correct_num %in% c(0, 1),
      correct_num == 1,
      is.finite(rt_valid)
    ) %>%
    mutate(
      facet_condition = as.character(condition),
      aid_outcome = if_else(
        aid_correct_num == 1,
        "Aid correct",
        "Aid incorrect"
      )
    ) %>%
    group_by(participant_id, facet_condition, aid_outcome, decision) %>%
    summarise(
      mean_rt = mean(rt_valid),
      n_trials = n(),
      .groups = "drop"
    )

  manual_rt <- decision_trials %>%
    filter(
      as.character(condition) == "Manual",
      correct_num == 1,
      is.finite(rt_valid)
    ) %>%
    group_by(participant_id, decision) %>%
    summarise(
      mean_rt = mean(rt_valid),
      n_trials = n(),
      .groups = "drop"
    ) %>%
    crossing(facet_condition = AID_OUTCOME_FACET_LEVELS) %>%
    mutate(aid_outcome = "Manual")

  list(
    accuracy = bind_rows(aided_accuracy, manual_accuracy) %>%
      mutate(
        facet_condition = factor(
          facet_condition,
          levels = AID_OUTCOME_FACET_LEVELS
        ),
        aid_outcome = factor(aid_outcome, levels = AID_OUTCOME_LEVELS),
        decision = factor(decision, levels = DECISION_LEVELS)
      ),
    rt = bind_rows(aided_rt, manual_rt) %>%
      mutate(
        facet_condition = factor(
          facet_condition,
          levels = AID_OUTCOME_FACET_LEVELS
        ),
        aid_outcome = factor(aid_outcome, levels = AID_OUTCOME_LEVELS),
        decision = factor(decision, levels = DECISION_LEVELS)
      )
  )
}

format_aid_outcome_complete_n <- function(summary_dat) {
  complete_n <- summary_dat %>%
    distinct(facet_condition, decision, n_participants) %>%
    arrange(facet_condition, decision)

  if (n_distinct(complete_n$n_participants) == 1) {
    return(
      paste0(
        "n = ",
        first(complete_n$n_participants),
        " for each decision in each facet"
      )
    )
  }

  complete_n %>%
    transmute(
      label = paste0(
        as.character(facet_condition),
        " ",
        as.character(decision),
        " n = ",
        n_participants
      )
    ) %>%
    pull(label) %>%
    paste(collapse = "; ")
}

make_group_aid_outcome_plots <- function(trial_dat, plot_label) {
  participant <- make_participant_aid_outcome_summaries(trial_dat)

  acc_summary <- summarise_repeated_complete(
    participant$accuracy,
    "mean_accuracy",
    "aid_outcome",
    c("facet_condition", "decision"),
    expected_conditions = AID_OUTCOME_LEVELS
  ) %>%
    mutate(
      facet_condition = factor(
        facet_condition,
        levels = AID_OUTCOME_FACET_LEVELS
      ),
      aid_outcome = factor(aid_outcome, levels = AID_OUTCOME_LEVELS),
      decision = factor(decision, levels = DECISION_LEVELS),
      mean_accuracy = mean
    )

  rt_summary <- summarise_repeated_complete(
    participant$rt,
    "mean_rt",
    "aid_outcome",
    c("facet_condition", "decision"),
    expected_conditions = AID_OUTCOME_LEVELS
  ) %>%
    mutate(
      facet_condition = factor(
        facet_condition,
        levels = AID_OUTCOME_FACET_LEVELS
      ),
      aid_outcome = factor(aid_outcome, levels = AID_OUTCOME_LEVELS),
      decision = factor(decision, levels = DECISION_LEVELS),
      mean_rt = mean
    )

  acc_limits <- get_axis_limits(
    acc_summary$mean_accuracy,
    acc_summary$se,
    bounds = c(0, 1),
    pad = 0.18
  )
  rt_limits <- get_axis_limits(
    rt_summary$mean_rt,
    rt_summary$se,
    bounds = c(0, Inf),
    pad = 0.18
  )
  p_acc <- ggplot(
    acc_summary,
    aes(
      x = aid_outcome,
      y = mean_accuracy,
      colour = decision,
      shape = decision,
      group = decision
    )
  ) +
    geom_errorbar(
      aes(
        ymin = pmax(0, mean_accuracy - se),
        ymax = pmin(1, mean_accuracy + se)
      ),
      width = 0.08,
      linewidth = 0.55,
      show.legend = FALSE
    ) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 3.2) +
    facet_wrap(~facet_condition, nrow = 1, drop = FALSE) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    coord_cartesian(ylim = acc_limits) +
    labs(
      x = NULL,
      y = "Mean accuracy",
      title = paste0(
        plot_label,
        " group decision accuracy by aid correctness"
      ),
      subtitle = str_wrap(
        paste0(
          "Participant means with Morey-Cousineau SEs across three categories ",
          "within decision and facet; ",
          format_aid_outcome_complete_n(acc_summary)
        ),
        width = 75
      )
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 15, hjust = 1),
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    ) +
    decision_scales()

  p_rt <- ggplot(
    rt_summary,
    aes(
      x = aid_outcome,
      y = mean_rt,
      colour = decision,
      shape = decision,
      group = decision
    )
  ) +
    geom_errorbar(
      aes(
        ymin = pmax(0, mean_rt - se),
        ymax = mean_rt + se
      ),
      width = 0.08,
      linewidth = 0.55,
      show.legend = FALSE
    ) +
    geom_line(linewidth = 0.85) +
    geom_point(size = 3.2) +
    facet_wrap(~facet_condition, nrow = 1, drop = FALSE) +
    coord_cartesian(ylim = rt_limits) +
    labs(
      x = "Aid outcome",
      y = "Mean correct RT (s)",
      title = paste0(
        plot_label,
        " group correct RT by aid correctness"
      ),
      subtitle = str_wrap(
        paste0(
          "Correct-trial participant means with Morey-Cousineau SEs across ",
          "three categories within decision and facet; ",
          format_aid_outcome_complete_n(rt_summary)
        ),
        width = 75
      )
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 15, hjust = 1),
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    ) +
    decision_scales()

  combined <- (
    p_acc + labs(title = NULL)
  ) / (
    p_rt + labs(title = NULL)
  ) +
    plot_layout(heights = c(1, 1), guides = "collect") +
    plot_annotation(
      title = paste0(
        STUDY_TITLE,
        ": ",
        plot_label,
        " group accuracy and correct RT by aid correctness"
      )
    ) &
    theme(legend.position = "bottom")

  list(
    accuracy = p_acc,
    rt = p_rt,
    combined = combined,
    accuracy_summary = acc_summary,
    rt_summary = rt_summary,
    participant_accuracy = participant$accuracy,
    participant_rt = participant$rt
  )
}

save_plot_pair <- function(plot, output_dir, stem, width, height, dpi = 300) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  pdf_file <- file.path(output_dir, paste0(stem, ".pdf"))
  png_file <- file.path(output_dir, paste0(stem, ".png"))

  ggsave(
    filename = pdf_file,
    plot = plot,
    device = cairo_pdf,
    width = width,
    height = height,
    units = "in"
  )
  ggsave(
    filename = png_file,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi
  )

  c(pdf_file, png_file)
}

main_raw <- read_latest_combined(
  "^results_.*_b00_ALL\\.csv$",
  "complete main-trial"
)
practice_raw <- read_latest_combined(
  "^results_.*_b00_PRACTICE\\.csv$",
  "Practice"
)
slider_raw <- read_latest_combined(
  "^results_.*_b00_POSTBLOCK_SLIDERS_ALL\\.csv$",
  "complete slider"
)

trial_dat <- prepare_trials(main_raw, practice_raw)
calibration_dat <- prepare_calibration_trials(practice_raw)
coverage <- validate_trial_coverage(trial_dat)
participant_summary <- make_participant_summary(trial_dat)
participant_change_rt_summary <- make_participant_change_rt_summary(trial_dat)
participant_ratings <- prepare_ratings(slider_raw)
rating_references <- make_rating_references(participant_summary)

trial_ids <- sort(unique(participant_summary$participant_id))
rating_ids <- sort(unique(participant_ratings$participant_id))
calibration_ids <- sort(unique(calibration_dat$participant_id))
if (!identical(trial_ids, rating_ids) || !identical(trial_ids, calibration_ids)) {
  stop(
    "Trial, calibration, and slider participant IDs do not match.",
    call. = FALSE
  )
}

if (PLOT_MODE == "single" && length(trial_ids) != 1) {
  stop("single mode requires exactly one participant.", call. = FALSE)
}

message("Trial coverage after restricting Practice to its last 40 calibration trials:")
print(coverage, n = Inf)

written <- character(0)

if (PLOT_MODE %in% c("single", "cohort")) {
  for (pid in trial_ids) {
    pid_prefix <- sprintf("p%03d", pid)
    participant_dir <- if (PLOT_MODE == "single") {
      OUTPUT_DIR
    } else {
      file.path(OUTPUT_DIR, "individual", pid_prefix)
    }
    participant_label <- paste("Participant", sprintf("%03d", pid))
    summary_pid <- participant_summary %>% filter(participant_id == pid)
    ratings_pid <- participant_ratings %>% filter(participant_id == pid)
    rating_refs_pid <- rating_references %>% filter(participant_id == pid)
    calibration_pid <- calibration_dat %>% filter(participant_id == pid)
    calibration_plot <- make_calibration_plot(
      calibration_pid,
      participant_label
    )

    plots <- make_individual_plots(
      summary_pid,
      participant_change_rt_summary %>% filter(participant_id == pid),
      ratings_pid,
      rating_refs_pid,
      participant_label
    )

    written <- c(
      written,
      save_plot_pair(
        calibration_plot,
        participant_dir,
        paste0(pid_prefix, "_calibration_dynamics"),
        width = 10,
        height = 7
      ),
      save_plot_pair(
        plots$accuracy,
        participant_dir,
        paste0(pid_prefix, "_condition_accuracy_means"),
        width = 8.5,
        height = 5.4
      ),
      save_plot_pair(
        plots$change,
        participant_dir,
        paste0(pid_prefix, "_condition_response_change_means"),
        width = 8.5,
        height = 5.4
      ),
      save_plot_pair(
        plots$rating,
        participant_dir,
        paste0(pid_prefix, "_condition_self_rated_accuracy_means"),
        width = 8.5,
        height = 5.4
      ),
      save_plot_pair(
        plots$rt,
        participant_dir,
        paste0(pid_prefix, "_condition_rt_means"),
        width = 8.5,
        height = 5.4
      ),
      save_plot_pair(
        plots$rt_change,
        participant_dir,
        paste0(pid_prefix, "_condition_rt_by_response_change"),
        width = 12,
        height = 12
      ),
      save_plot_pair(
        plots$combined,
        participant_dir,
        paste0(pid_prefix, "_condition_accuracy_rt_means"),
        width = 8.5,
        height = 13.5
      )
    )

    message(participant_label, " summary:")
    print(summary_pid)
    print(ratings_pid)
  }
}

if (PLOT_MODE %in% c("cohort", "group")) {
  group_dir <- file.path(OUTPUT_DIR, "group")
  group_prefix <- paste0(OUTPUT_PREFIX, "_group")
  group_plots <- make_group_plots(
    participant_summary,
    participant_change_rt_summary,
    participant_ratings,
    rating_references,
    PLOT_LABEL
  )
  aid_outcome_plots <- make_group_aid_outcome_plots(trial_dat, PLOT_LABEL)

  written <- c(
    written,
    save_plot_pair(
      group_plots$accuracy,
      group_dir,
      paste0(group_prefix, "_condition_accuracy_means"),
      width = 8.5,
      height = 6
    ),
    save_plot_pair(
      group_plots$change,
      group_dir,
      paste0(group_prefix, "_condition_response_change_means"),
      width = 8.5,
      height = 5.4
    ),
    save_plot_pair(
      group_plots$rating,
      group_dir,
      paste0(group_prefix, "_condition_self_rated_accuracy_means"),
      width = 8.5,
      height = 6
    ),
    save_plot_pair(
      group_plots$rt,
      group_dir,
      paste0(group_prefix, "_condition_rt_means"),
      width = 8.5,
      height = 5.4
    ),
    save_plot_pair(
      group_plots$rt_change,
      group_dir,
      paste0(group_prefix, "_condition_rt_by_response_change"),
      width = 12,
      height = 12
    ),
    save_plot_pair(
      group_plots$combined,
      group_dir,
      paste0(group_prefix, "_condition_accuracy_rt_means"),
      width = 8.5,
      height = 13.5
    ),
    save_plot_pair(
      aid_outcome_plots$accuracy,
      group_dir,
      paste0(group_prefix, "_aid_outcome_accuracy_means"),
      width = 7.5,
      height = 4.35
    ),
    save_plot_pair(
      aid_outcome_plots$rt,
      group_dir,
      paste0(group_prefix, "_aid_outcome_correct_rt_means"),
      width = 7.5,
      height = 4.35
    ),
    save_plot_pair(
      aid_outcome_plots$combined,
      group_dir,
      paste0(group_prefix, "_aid_outcome_accuracy_correct_rt_means"),
      width = 7.5,
      height = 8.625
    )
  )

  message("Group decision accuracy summary:")
  print(group_plots$accuracy_summary)
  message("Group response-change summary:")
  print(group_plots$change_summary)
  message("Group rating summary:")
  print(group_plots$rating_summary)
  message("Group RT summary:")
  print(group_plots$rt_summary)
  message("Group RT-by-change summary:")
  print(group_plots$change_rt_summary)
  message("Group RT-difference-by-change summary:")
  print(group_plots$change_rt_difference_summary)
  message("Group overall accuracy-difference summary:")
  print(group_plots$change_accuracy_difference_summary)
  message("Group decision accuracy by aid outcome summary:")
  print(aid_outcome_plots$accuracy_summary)
  message("Group correct RT by aid outcome summary:")
  print(aid_outcome_plots$rt_summary)
}

cat("Wrote:\n")
cat(paste0(" - ", written, collapse = "\n"), "\n", sep = "")
