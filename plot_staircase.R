# Plot calibration staircase and post-calibration performance dynamics.
#
# Usage:
#   Rscript plot_staircase.R [input_dir_or_file] [output_dir] [output_prefix] [plot_label] [mode] [slider_file]
#
# Defaults are set for the Harry pilot data. Deadline labels are read from the
# CSV, so older 2/4 s pilot runs and current 1.5/3 s runs are both labelled as
# collected. Set mode to "cohort" for participant-specific and group plots from
# a collated, multi-participant data_virus_all.csv file, or "group" to regenerate
# only the cohort group plots. Set mode to "accuracy" to regenerate only the
# individual and group plots that contain accuracy panels.

rm(list = ls())

library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(zoo)

args <- commandArgs(trailingOnly = TRUE)

INPUT_PATH <- if (length(args) >= 1) args[[1]] else "output/pilot_harry"
OUTPUT_DIR <- if (length(args) >= 2) args[[2]] else "plots"
OUTPUT_PREFIX <- if (length(args) >= 3) args[[3]] else "harry_pilot"
PLOT_LABEL <- if (length(args) >= 4) args[[4]] else "Harry pilot"
PLOT_MODE <- if (length(args) >= 5) args[[5]] else "single"
SLIDER_INPUT_PATH <- if (length(args) >= 6) args[[6]] else NA_character_

if (!PLOT_MODE %in% c("single", "cohort", "group", "accuracy")) {
  stop("mode must be 'single', 'cohort', 'group', or 'accuracy'", call. = FALSE)
}

WINDOW <- 25
TARGET_ACC <- 0.80
BURN_IN_TRIALS <- 50
CALIB_SUMMARY_LAST_N <- 150
DELTA_SD <- 0.01
GROUP_DODGE_WIDTH <- 0.24
POST_CONDITION_CODES <- c("M_HP", "A_HP", "A_LP", "M_LP")
ALL_CONDITION_CODES <- c("CAL_LP", POST_CONDITION_CODES)
RELIABILITY_PATTERN_LEVELS <- c("HP95_LP65", "HP65_LP95")
RELIABILITY_PATTERN_COLORS <- c(
  "HP95_LP65" = "#0072B2",
  "HP65_LP95" = "#D55E00"
)
RELIABILITY_PATTERN_SHAPES <- c(
  "HP95_LP65" = 16,
  "HP65_LP95" = 17
)
ACCURACY_MEASURE_LEVELS <- c(
  "Observed accuracy",
  "Self-rated own accuracy",
  "Self-rated aid accuracy"
)
ACCURACY_MEASURE_SHAPES <- c(
  "Observed accuracy" = 16,
  "Self-rated own accuracy" = 15,
  "Self-rated aid accuracy" = 18
)
SUMMARY_CONDITION_CODES <- if (PLOT_MODE %in% c("cohort", "group", "accuracy")) {
  ALL_CONDITION_CODES
} else {
  POST_CONDITION_CODES
}

required_cols <- c(
  "participant_id", "run_timestamp", "block", "block_idx",
  "condition_deadline_code", "time_pressure_condition", "trial_deadline_s",
  "automation_reliability_pattern",
  "trial", "difficulty_mode", "delta_fixed_mean", "delta_fixed_sd",
  "delta_stair_realised", "delta_stair_mean", "vblack_prop", "aid_correct",
  "aid_accuracy_setting", "response", "correct", "rt_s"
)

numeric_cols <- c(
  "participant_id", "block_idx", "trial_deadline_s", "trial", "global_trial",
  "delta_fixed_mean", "delta_fixed_sd", "delta_stair_realised",
  "delta_stair_mean", "delta_step_down_used", "delta_step_up_used",
  "vblack_prop", "n_vblack", "n_vwhite", "auto_on", "aid_accuracy_setting",
  "aid_onset_ms", "aid_onset_ms_rel", "rt_s"
)

logical_cols <- c("keymap_flip", "correct", "aid_correct")

resolve_input_file <- function(path) {
  if (file.exists(path) && !dir.exists(path)) {
    return(path)
  }

  if (!dir.exists(path)) {
    stop("Input path does not exist: ", path, call. = FALSE)
  }

  files <- list.files(
    path,
    pattern = "^results_.*_b00_ALL\\.csv$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop("No results_*_b00_ALL.csv file found in ", path, call. = FALSE)
  }

  files[[which.max(file.info(files)$mtime)]]
}

parse_bool <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  x_chr <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    x_chr %in% c("true", "t", "1") ~ TRUE,
    x_chr %in% c("false", "f", "0") ~ FALSE,
    TRUE ~ NA
  )
}

read_task_csv <- function(path) {
  dat <- read_csv(
    path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )

  missing_cols <- setdiff(required_cols, names(dat))
  if (length(missing_cols) > 0) {
    stop(
      "Input file is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dat <- dat %>%
    filter(!(participant_id == "participant_id" & run_timestamp == "run_timestamp"))

  for (col in intersect(numeric_cols, names(dat))) {
    dat[[col]] <- suppressWarnings(as.numeric(dat[[col]]))
  }

  for (col in intersect(logical_cols, names(dat))) {
    dat[[col]] <- parse_bool(dat[[col]])
  }

  dat
}

resolve_slider_file <- function(input_file, explicit_path, required = FALSE) {
  if (!is.na(explicit_path) && nzchar(explicit_path)) {
    if (!file.exists(explicit_path) || dir.exists(explicit_path)) {
      stop("Slider input file does not exist: ", explicit_path, call. = FALSE)
    }
    return(explicit_path)
  }

  sibling_path <- file.path(dirname(input_file), "data_virus_sliders_all.csv")
  if (file.exists(sibling_path)) {
    return(sibling_path)
  }

  if (required) {
    stop(
      "A slider CSV is required. Expected sibling file: ",
      sibling_path,
      " or pass it as the sixth argument.",
      call. = FALSE
    )
  }

  NA_character_
}

read_slider_csv <- function(path) {
  slider_required_cols <- c(
    "participant_id",
    "automation_reliability_pattern",
    "condition_deadline_code",
    "question_key",
    "response_percent"
  )

  slider_dat <- read_csv(
    path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
  missing_cols <- setdiff(slider_required_cols, names(slider_dat))
  if (length(missing_cols) > 0) {
    stop(
      "Slider file is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  slider_dat %>%
    mutate(
      participant_id = suppressWarnings(as.numeric(participant_id)),
      response_percent = suppressWarnings(as.numeric(response_percent))
    )
}

validate_slider_coverage <- function(slider_dat, participant_ids) {
  expected <- tibble(
    condition_deadline_code = ALL_CONDITION_CODES,
    expected_question_key = c(
      "perc_self_correct",
      "perc_self_correct",
      "perc_auto_correct",
      "perc_auto_correct",
      "perc_self_correct"
    )
  )

  checked <- slider_dat %>%
    inner_join(expected, by = "condition_deadline_code")

  invalid <- checked %>%
    filter(
      question_key != expected_question_key |
        is.na(response_percent) |
        response_percent < 0 |
        response_percent > 100
    )

  coverage <- checked %>%
    count(participant_id, condition_deadline_code, name = "n")
  expected_rows <- length(participant_ids) * length(ALL_CONDITION_CODES)

  if (
    nrow(invalid) > 0 ||
      nrow(coverage) != expected_rows ||
      any(coverage$n != 1) ||
      !setequal(unique(coverage$participant_id), participant_ids)
  ) {
    stop(
      "Slider data must contain exactly one applicable 0-100 rating for every participant and block.",
      call. = FALSE
    )
  }
}

finite_values <- function(x) {
  x[is.finite(x)]
}

nice_ceiling <- function(x, step = 0.02, fallback = 0.10) {
  x <- finite_values(x)
  if (length(x) == 0) {
    return(fallback)
  }
  max(step, ceiling(max(x) / step) * step)
}

first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  x[[1]]
}

format_deadline <- function(x) {
  x <- sort(unique(finite_values(x)))
  if (length(x) == 0) {
    return("NA")
  }
  paste(format(x, trim = TRUE, scientific = FALSE), collapse = "/")
}

block_label <- function(dat_block) {
  pressure <- paste(sort(unique(na.omit(dat_block$time_pressure_condition))), collapse = "/")
  deadline <- format_deadline(dat_block$trial_deadline_s)
  code <- paste(sort(unique(na.omit(dat_block$condition_deadline_code))), collapse = "/")

  paste0(pressure, " deadline = ", deadline, " s (", code, ")")
}

condition_display_name <- function(code) {
  dplyr::recode(
    as.character(code),
    "CAL_LP" = "Calibration LP",
    "M_HP" = "Manual HP",
    "A_HP" = "Automation HP",
    "A_LP" = "Automation LP",
    "M_LP" = "Manual LP",
    .default = as.character(code)
  )
}

unique_or_missing <- function(x) {
  x <- unique(x[!is.na(x)])
  if (length(x) == 1) x[[1]] else NA_real_
}

get_axis_limits <- function(y, se = NULL, pad_prop = 0.08, bounds = NULL) {
  if (is.null(se)) {
    lo <- min(y, na.rm = TRUE)
    hi <- max(y, na.rm = TRUE)
  } else {
    se <- ifelse(is.na(se), 0, se)
    lo <- min(y - se, na.rm = TRUE)
    hi <- max(y + se, na.rm = TRUE)
  }

  if (!is.finite(lo) || !is.finite(hi)) {
    return(bounds)
  }

  if (identical(lo, hi)) {
    pad <- max(0.05 * abs(lo), 0.05)
  } else {
    pad <- (hi - lo) * pad_prop
  }

  limits <- c(lo - pad, hi + pad)

  if (!is.null(bounds)) {
    limits <- c(
      max(bounds[[1]], limits[[1]], na.rm = TRUE),
      min(bounds[[2]], limits[[2]], na.rm = TRUE)
    )
  }

  limits
}

accuracy_vars <- function(dat_block) {
  dat_block %>%
    arrange(trial) %>%
    mutate(
      correct_num = if_else(is.na(correct), 0, as.numeric(correct)),
      aid_correct_num = if_else(is.na(aid_correct), NA_real_, as.numeric(aid_correct)),
      acc_running = cumsum(correct_num) / row_number(),
      acc_slide = zoo::rollapply(
        correct_num,
        width = WINDOW,
        FUN = mean,
        align = "right",
        fill = NA,
        partial = TRUE
      )
    )
}

save_plot_pair <- function(plot, stem, width, height) {
  pdf_file <- file.path(OUTPUT_DIR, paste0(stem, ".pdf"))
  png_file <- file.path(OUTPUT_DIR, paste0(stem, ".png"))

  ggsave(
    filename = pdf_file,
    plot = plot,
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
    dpi = 200
  )

  c(pdf_file, png_file)
}

make_condition_metadata <- function(dat, split_by_pattern = FALSE) {
  group_cols <- c(
    if (split_by_pattern) "automation_reliability_pattern",
    "condition_deadline_code"
  )

  out <- dat %>%
    filter(condition_deadline_code %in% SUMMARY_CONDITION_CODES) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      block = first_non_missing(block),
      time_pressure_condition = first_non_missing(time_pressure_condition),
      trial_deadline_s = first_non_missing(trial_deadline_s),
      aid_accuracy_setting = unique_or_missing(aid_accuracy_setting),
      .groups = "drop"
    ) %>%
    mutate(
      condition_deadline_code = factor(
        condition_deadline_code,
        levels = SUMMARY_CONDITION_CODES
      ),
      condition_label = paste0(
        condition_display_name(condition_deadline_code),
        "\n",
        format(trial_deadline_s, trim = TRUE, scientific = FALSE),
        " s"
      )
    ) %>%
    arrange(condition_deadline_code)

  if (split_by_pattern) {
    out <- out %>%
      mutate(
        automation_reliability_pattern = factor(
          automation_reliability_pattern,
          levels = RELIABILITY_PATTERN_LEVELS
        )
      ) %>%
      arrange(automation_reliability_pattern, condition_deadline_code)
  }

  out
}

summarise_morey_one_group <- function(
  data,
  value_col,
  mean_name,
  se_name,
  n_conditions = length(SUMMARY_CONDITION_CODES)
) {
  n_subjects <- n_distinct(data$participant_id[!is.na(data$participant_id)])

  if (n_subjects < 2) {
    return(data %>%
      group_by(condition_deadline_code) %>%
      summarise(
        !!mean_name := mean(.data[[value_col]], na.rm = TRUE),
        !!se_name := NA_real_,
        n_participants = n_subjects,
        .groups = "drop"
      ))
  }

  morey_cf <- sqrt(n_conditions / (n_conditions - 1))
  grand_mean <- mean(data[[value_col]], na.rm = TRUE)

  data %>%
    group_by(participant_id) %>%
    mutate(.subj_mean = mean(.data[[value_col]], na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(.norm_value = .data[[value_col]] - .subj_mean + grand_mean) %>%
    group_by(condition_deadline_code) %>%
    summarise(
      !!mean_name := mean(.data[[value_col]], na.rm = TRUE),
      !!se_name := sd(.norm_value, na.rm = TRUE) /
        sqrt(sum(!is.na(.norm_value))) * morey_cf,
      n_participants = n_subjects,
      .groups = "drop"
    )
}

summarise_morey_condition_mean <- function(
  data,
  value_col,
  mean_name,
  se_name,
  n_conditions = length(SUMMARY_CONDITION_CODES),
  between_col = NULL
) {
  if (is.null(between_col)) {
    return(summarise_morey_one_group(
      data = data,
      value_col = value_col,
      mean_name = mean_name,
      se_name = se_name,
      n_conditions = n_conditions
    ))
  }

  between_values <- unique(as.character(data[[between_col]]))

  bind_rows(lapply(between_values, function(between_value) {
    group_dat <- data %>%
      filter(as.character(.data[[between_col]]) == between_value)

    group_summary <- summarise_morey_one_group(
      data = group_dat,
      value_col = value_col,
      mean_name = mean_name,
      se_name = se_name,
      n_conditions = n_conditions
    )

    group_summary[[between_col]] <- between_value
    group_summary
  })) %>%
    relocate(all_of(between_col), .before = condition_deadline_code)
}

make_pattern_labels <- function(data) {
  pattern_counts <- data %>%
    distinct(participant_id, automation_reliability_pattern) %>%
    count(automation_reliability_pattern, name = "n_participants")

  labels <- paste0(
    pattern_counts$automation_reliability_pattern,
    " (n = ",
    pattern_counts$n_participants,
    ")"
  )
  setNames(labels, as.character(pattern_counts$automation_reliability_pattern))
}

add_pattern_scales <- function(plot, pattern_labels) {
  pattern_breaks <- intersect(
    RELIABILITY_PATTERN_LEVELS,
    names(pattern_labels)
  )
  plot +
    scale_colour_manual(
      values = RELIABILITY_PATTERN_COLORS,
      breaks = pattern_breaks,
      labels = unname(pattern_labels[pattern_breaks]),
      name = "Assigned aid reliability"
    ) +
    scale_shape_manual(
      values = RELIABILITY_PATTERN_SHAPES,
      breaks = pattern_breaks,
      labels = unname(pattern_labels[pattern_breaks]),
      name = "Assigned aid reliability"
    )
}

add_pattern_colour_scale <- function(plot, pattern_labels) {
  pattern_breaks <- intersect(
    RELIABILITY_PATTERN_LEVELS,
    names(pattern_labels)
  )
  plot +
    scale_colour_manual(
      values = RELIABILITY_PATTERN_COLORS,
      breaks = pattern_breaks,
      labels = unname(pattern_labels[pattern_breaks]),
      name = "Assigned aid reliability"
    )
}

add_accuracy_scales <- function(plot, pattern_labels) {
  pattern_breaks <- intersect(
    RELIABILITY_PATTERN_LEVELS,
    names(pattern_labels)
  )
  plot +
    scale_colour_manual(
      values = RELIABILITY_PATTERN_COLORS,
      breaks = pattern_breaks,
      labels = unname(pattern_labels[pattern_breaks]),
      name = "Assigned aid reliability"
    ) +
    scale_shape_manual(
      values = ACCURACY_MEASURE_SHAPES,
      breaks = ACCURACY_MEASURE_LEVELS,
      name = "Accuracy measure"
    ) +
    guides(
      colour = guide_legend(order = 1, nrow = 1),
      shape = guide_legend(order = 2, nrow = 1)
    )
}

add_self_rating_scales <- function(plot, pattern_labels) {
  pattern_breaks <- intersect(
    RELIABILITY_PATTERN_LEVELS,
    names(pattern_labels)
  )
  self_rating_breaks <- ACCURACY_MEASURE_LEVELS[
    ACCURACY_MEASURE_LEVELS != "Observed accuracy"
  ]
  plot +
    scale_colour_manual(
      values = RELIABILITY_PATTERN_COLORS,
      breaks = pattern_breaks,
      labels = unname(pattern_labels[pattern_breaks]),
      name = "Assigned aid reliability"
    ) +
    scale_shape_manual(
      values = ACCURACY_MEASURE_SHAPES,
      breaks = self_rating_breaks,
      name = "Self-rating"
    ) +
    guides(
      colour = guide_legend(order = 1, nrow = 1),
      shape = guide_legend(order = 2, nrow = 1)
    )
}

make_rating_summary <- function(slider_dat, condition_meta, split_by_pattern) {
  rating_dat <- slider_dat %>%
    filter(condition_deadline_code %in% SUMMARY_CONDITION_CODES) %>%
    mutate(
      condition_deadline_code = factor(
        condition_deadline_code,
        levels = SUMMARY_CONDITION_CODES
      ),
      automation_reliability_pattern = factor(
        automation_reliability_pattern,
        levels = RELIABILITY_PATTERN_LEVELS
      ),
      accuracy_measure = recode(
        question_key,
        "perc_self_correct" = "Self-rated own accuracy",
        "perc_auto_correct" = "Self-rated aid accuracy",
        .default = NA_character_
      ),
      accuracy_measure = factor(
        accuracy_measure,
        levels = ACCURACY_MEASURE_LEVELS
      ),
      rated_accuracy = response_percent / 100
    ) %>%
    filter(!is.na(accuracy_measure), !is.na(rated_accuracy))

  participant_rating_cols <- c(
    "participant_id",
    if (split_by_pattern) "automation_reliability_pattern",
    "accuracy_measure",
    "condition_deadline_code"
  )
  subj_rating <- rating_dat %>%
    group_by(across(all_of(participant_rating_cols))) %>%
    summarise(
      rated_accuracy = mean(rated_accuracy),
      .groups = "drop"
    )

  rating_summaries <- lapply(
    c("Self-rated own accuracy", "Self-rated aid accuracy"),
    function(measure) {
      measure_dat <- subj_rating %>%
        filter(as.character(accuracy_measure) == measure)
      n_conditions <- n_distinct(measure_dat$condition_deadline_code)

      out <- summarise_morey_condition_mean(
        data = measure_dat,
        value_col = "rated_accuracy",
        mean_name = "mean_rated_accuracy",
        se_name = "se_rated_accuracy",
        n_conditions = n_conditions,
        between_col = if (split_by_pattern) {
          "automation_reliability_pattern"
        } else {
          NULL
        }
      )
      out$accuracy_measure <- measure
      out
    }
  )

  join_cols <- c(
    "condition_deadline_code",
    if (split_by_pattern) "automation_reliability_pattern"
  )
  rating_summary <- bind_rows(rating_summaries) %>%
    left_join(condition_meta, by = join_cols) %>%
    mutate(
      condition_label = factor(
        condition_label,
        levels = unique(condition_meta$condition_label)
      ),
      accuracy_measure = factor(
        accuracy_measure,
        levels = ACCURACY_MEASURE_LEVELS
      )
    )

  if (!split_by_pattern) {
    pattern <- unique(as.character(rating_dat$automation_reliability_pattern))
    if (length(pattern) != 1) {
      stop(
        "An individual accuracy plot must contain one reliability pattern.",
        call. = FALSE
      )
    }
    rating_summary$automation_reliability_pattern <- factor(
      pattern,
      levels = RELIABILITY_PATTERN_LEVELS
    )
  }

  list(summary = rating_summary, participant = subj_rating)
}

validate_condition_coverage <- function(dat, condition_codes) {
  coverage <- dat %>%
    filter(condition_deadline_code %in% condition_codes) %>%
    distinct(participant_id, condition_deadline_code) %>%
    count(participant_id, name = "n_conditions")

  incomplete <- coverage %>%
    filter(n_conditions != length(condition_codes))

  if (nrow(coverage) == 0 || nrow(incomplete) > 0) {
    bad_ids <- if (nrow(incomplete) == 0) {
      "none detected"
    } else {
      paste(incomplete$participant_id, collapse = ", ")
    }
    stop(
      "Every participant must have all conditions: ",
      paste(condition_codes, collapse = ", "),
      ". Incomplete participant IDs: ",
      bad_ids,
      call. = FALSE
    )
  }
}

restrict_calibration_summary_trials <- function(dat) {
  dat %>%
    filter(condition_deadline_code %in% SUMMARY_CONDITION_CODES) %>%
    group_by(participant_id, condition_deadline_code) %>%
    arrange(trial, .by_group = TRUE) %>%
    filter(
      as.character(condition_deadline_code) != "CAL_LP" |
        row_number() > pmax(n() - CALIB_SUMMARY_LAST_N, 0)
    ) %>%
    ungroup()
}

make_condition_summary_plots <- function(
  dat,
  slider_dat = NULL,
  split_by_pattern = FALSE
) {
  condition_meta <- make_condition_metadata(dat, split_by_pattern)
  missing_conditions <- setdiff(
    SUMMARY_CONDITION_CODES,
    as.character(condition_meta$condition_deadline_code)
  )

  if (length(missing_conditions) > 0) {
    stop(
      "Missing summary conditions: ",
      paste(missing_conditions, collapse = ", "),
      call. = FALSE
    )
  }

  summary_dat <- dat %>%
    restrict_calibration_summary_trials() %>%
    mutate(
      condition_deadline_code = factor(
        condition_deadline_code,
        levels = SUMMARY_CONDITION_CODES
      ),
      correct_num = if_else(is.na(correct), 0, as.numeric(correct))
    )

  if (split_by_pattern) {
    summary_dat <- summary_dat %>%
      mutate(
        automation_reliability_pattern = factor(
          automation_reliability_pattern,
          levels = RELIABILITY_PATTERN_LEVELS
        )
      )
  }

  participant_condition_cols <- c(
    "participant_id",
    if (split_by_pattern) "automation_reliability_pattern",
    "condition_deadline_code"
  )

  subj_acc <- summary_dat %>%
    group_by(across(all_of(participant_condition_cols))) %>%
    summarise(
      acc = mean(correct_num, na.rm = TRUE),
      .groups = "drop"
    )

  subj_rt <- summary_dat %>%
    filter(!is.na(rt_s)) %>%
    group_by(across(all_of(participant_condition_cols))) %>%
    summarise(
      mean_rt = mean(rt_s, na.rm = TRUE),
      .groups = "drop"
    )

  acc_summary <- summarise_morey_condition_mean(
    data = subj_acc,
    value_col = "acc",
    mean_name = "mean_acc",
    se_name = "se_acc",
    between_col = if (split_by_pattern) "automation_reliability_pattern" else NULL
  ) %>%
    left_join(
      condition_meta,
      by = c(
        "condition_deadline_code",
        if (split_by_pattern) "automation_reliability_pattern"
      )
    ) %>%
    mutate(
      condition_label = factor(
        condition_label,
        levels = unique(condition_meta$condition_label)
      )
    )

  rt_summary <- summarise_morey_condition_mean(
    data = subj_rt,
    value_col = "mean_rt",
    mean_name = "mean_rt",
    se_name = "se_rt",
    between_col = if (split_by_pattern) "automation_reliability_pattern" else NULL
  ) %>%
    left_join(
      condition_meta,
      by = c(
        "condition_deadline_code",
        if (split_by_pattern) "automation_reliability_pattern"
      )
    ) %>%
    mutate(
      condition_label = factor(
        condition_label,
        levels = unique(condition_meta$condition_label)
      )
    )

  rating <- if (is.null(slider_dat)) {
    NULL
  } else {
    make_rating_summary(
      slider_dat = slider_dat,
      condition_meta = condition_meta,
      split_by_pattern = split_by_pattern
    )
  }
  rating_summary <- if (is.null(rating)) NULL else rating$summary

  if (!split_by_pattern && !is.null(rating_summary)) {
    individual_pattern <- unique(
      as.character(summary_dat$automation_reliability_pattern)
    )
    if (length(individual_pattern) != 1) {
      stop(
        "An individual accuracy plot must contain one reliability pattern.",
        call. = FALSE
      )
    }
    acc_summary$automation_reliability_pattern <- factor(
      individual_pattern,
      levels = RELIABILITY_PATTERN_LEVELS
    )
  }

  acc_summary$accuracy_measure <- factor(
    "Observed accuracy",
    levels = ACCURACY_MEASURE_LEVELS
  )

  aid_reference <- condition_meta %>%
    filter(block == "AUTOMATION", !is.na(aid_accuracy_setting)) %>%
    mutate(x_pos = as.numeric(condition_deadline_code))

  if (!split_by_pattern && !is.null(rating_summary)) {
    aid_reference$automation_reliability_pattern <- factor(
      individual_pattern,
      levels = RELIABILITY_PATTERN_LEVELS
    )
  }

  acc_ylim <- get_axis_limits(
    c(
      acc_summary$mean_acc,
      if (!is.null(rating_summary)) rating_summary$mean_rated_accuracy,
      TARGET_ACC,
      aid_reference$aid_accuracy_setting
    ),
    c(
      acc_summary$se_acc,
      if (!is.null(rating_summary)) rating_summary$se_rated_accuracy,
      0,
      rep(0, nrow(aid_reference))
    ),
    pad_prop = 0.14,
    bounds = c(0, 1)
  )

  rt_ylim <- get_axis_limits(rt_summary$mean_rt, rt_summary$se_rt, pad_prop = 0.18)
  n_subjects <- n_distinct(summary_dat$participant_id)
  rt_subtitle <- if (split_by_pattern) {
    paste0(
      "Within-pattern Morey-Cousineau SEs across ",
      length(SUMMARY_CONDITION_CODES),
      " blocks; HP65_LP95 n = 1 (no SE); calibration = final ",
      CALIB_SUMMARY_LAST_N,
      " trials"
    )
  } else if (n_subjects >= 2) {
    paste0(
      "Error bars are Morey-Cousineau within-subject SEs across ",
      length(SUMMARY_CONDITION_CODES),
      " conditions; calibration uses the final ",
      CALIB_SUMMARY_LAST_N,
      " trials"
    )
  } else {
    paste0("Calibration mean uses the final ", CALIB_SUMMARY_LAST_N, " trials")
  }

  accuracy_subtitle <- rt_subtitle
  rating_subtitle <- NULL
  if (!is.null(rating_summary) && split_by_pattern) {
    pattern_counts <- summary_dat %>%
      distinct(participant_id, automation_reliability_pattern) %>%
      count(automation_reliability_pattern, name = "n_participants")
    singleton_text <- pattern_counts %>%
      filter(n_participants < 2) %>%
      transmute(note = paste0(automation_reliability_pattern, " n = 1 (no SE)")) %>%
      pull(note)
    accuracy_subtitle <- paste0(
      "Within-pattern Morey-Cousineau SEs: observed = 5 blocks, own rating = 3, aid rating = 2",
      "\n",
      if (length(singleton_text) > 0) {
        paste0(paste(singleton_text, collapse = ", "), "; ")
      } else {
        ""
      },
      " calibration observed mean = final ",
      CALIB_SUMMARY_LAST_N,
      " trials"
    )
    rating_subtitle <- paste0(
      "Within-pattern Morey-Cousineau SEs: own rating = 3 blocks; aid rating = 2 blocks",
      if (length(singleton_text) > 0) {
        paste0("; ", paste(singleton_text, collapse = ", "))
      } else {
        ""
      }
    )
  } else if (!is.null(rating_summary)) {
    accuracy_subtitle <- paste0(
      "Assigned aid reliability: ",
      individual_pattern,
      "; calibration observed mean = final ",
      CALIB_SUMMARY_LAST_N,
      " trials"
    )
  }

  p_rating <- NULL

  if (split_by_pattern) {
    group_dodge <- position_dodge(width = GROUP_DODGE_WIDTH)
    pattern_labels <- make_pattern_labels(summary_dat)
    acc_summary <- acc_summary %>%
      mutate(
        automation_reliability_pattern = factor(
          automation_reliability_pattern,
          levels = RELIABILITY_PATTERN_LEVELS
        )
      )
    observed_reference <- acc_summary %>%
      mutate(
        pattern_index = match(
          as.character(automation_reliability_pattern),
          RELIABILITY_PATTERN_LEVELS
        ),
        reference_x = as.numeric(condition_label) + (
          pattern_index - (length(RELIABILITY_PATTERN_LEVELS) + 1) / 2
        ) * (GROUP_DODGE_WIDTH / length(RELIABILITY_PATTERN_LEVELS))
      )
    rt_summary <- rt_summary %>%
      mutate(
        automation_reliability_pattern = factor(
          automation_reliability_pattern,
          levels = RELIABILITY_PATTERN_LEVELS
        )
      )

    if (!is.null(rating_summary)) {
      rating_summary <- rating_summary %>%
        mutate(
          automation_reliability_pattern = factor(
            automation_reliability_pattern,
            levels = RELIABILITY_PATTERN_LEVELS
          )
        )
    }

    if (!is.null(rating_summary)) {
      p_acc <- ggplot(
        acc_summary,
        aes(
          x = condition_label,
          y = mean_acc,
          group = automation_reliability_pattern,
          colour = automation_reliability_pattern
        )
      ) +
        geom_line(linewidth = 0.9, position = group_dodge) +
        geom_point(
          aes(shape = automation_reliability_pattern),
          size = 3.2,
          position = group_dodge
        ) +
        geom_errorbar(
          aes(
            ymin = mean_acc - se_acc,
            ymax = mean_acc + se_acc
          ),
          width = 0.12,
          na.rm = TRUE,
          position = group_dodge
        ) +
        geom_segment(
          data = aid_reference,
          aes(
            x = x_pos - 0.35,
            xend = x_pos + 0.35,
            y = aid_accuracy_setting,
            yend = aid_accuracy_setting,
            colour = automation_reliability_pattern
          ),
          inherit.aes = FALSE,
          linetype = "dashed",
          linewidth = 0.8,
          show.legend = FALSE
        ) +
        geom_hline(yintercept = TARGET_ACC, linetype = "dashed") +
        geom_label(
          aes(
            label = sprintf("%.1f%%", mean_acc * 100)
          ),
          vjust = -0.80,
          size = 3.2,
          fontface = "bold",
          fill = "white",
          linewidth = 0,
          label.padding = grid::unit(0.08, "lines"),
          show.legend = FALSE,
          position = group_dodge
        ) +
        scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
        labs(
          x = NULL,
          y = "Mean accuracy",
          title = paste0(PLOT_LABEL, " accuracy by block and reliability pattern"),
          subtitle = rt_subtitle,
          caption = "Short dashed segments at automation blocks show assigned aid accuracy"
        ) +
        coord_cartesian(ylim = acc_ylim, clip = "off") +
        theme_classic() +
        theme(
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(5.5, 12, 5.5, 5.5)
        )
      p_acc <- add_pattern_scales(p_acc, pattern_labels)

      p_rating <- ggplot(
        rating_summary,
        aes(
          x = condition_label,
          y = mean_rated_accuracy,
          group = automation_reliability_pattern,
          colour = automation_reliability_pattern
        )
      ) +
        geom_line(linewidth = 0.9, position = group_dodge) +
        geom_point(
          aes(shape = accuracy_measure),
          size = 3.2,
          position = group_dodge
        ) +
        geom_errorbar(
          aes(
            ymin = mean_rated_accuracy - se_rated_accuracy,
            ymax = mean_rated_accuracy + se_rated_accuracy
          ),
          width = 0.12,
          na.rm = TRUE,
          position = group_dodge
        ) +
        geom_segment(
          data = observed_reference,
          aes(
            x = reference_x - 0.05,
            xend = reference_x + 0.05,
            y = mean_acc,
            yend = mean_acc
          ),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.9
        ) +
        geom_segment(
          data = aid_reference,
          aes(
            x = x_pos - 0.35,
            xend = x_pos + 0.35,
            y = aid_accuracy_setting,
            yend = aid_accuracy_setting,
            colour = automation_reliability_pattern
          ),
          inherit.aes = FALSE,
          linetype = "dashed",
          linewidth = 0.8,
          show.legend = FALSE
        ) +
        geom_hline(yintercept = TARGET_ACC, linetype = "dashed") +
        scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
        labs(
          x = NULL,
          y = "Self-rated accuracy",
          title = paste0(
            PLOT_LABEL,
            " self-rated accuracy by block"
          ),
          subtitle = rating_subtitle,
          caption = paste0(
            "Own: calibration/manual; aid: automation. ",
            "Black segments: observed accuracy; colored dashes: assigned aid accuracy."
          )
        ) +
        coord_cartesian(ylim = acc_ylim, clip = "off") +
        theme_classic() +
        theme(
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(5.5, 12, 5.5, 5.5)
        )
      p_rating <- add_self_rating_scales(p_rating, pattern_labels)
    } else {
      p_rating <- NULL
      p_acc <- ggplot(
        acc_summary,
        aes(
          x = condition_label,
          y = mean_acc,
          group = automation_reliability_pattern,
          colour = automation_reliability_pattern
        )
      ) +
        geom_line(linewidth = 0.9, position = group_dodge) +
        geom_point(
          aes(shape = automation_reliability_pattern),
          size = 3.2,
          position = group_dodge
        ) +
        geom_errorbar(
          aes(
            ymin = mean_acc - se_acc,
            ymax = mean_acc + se_acc
          ),
          width = 0.12,
          na.rm = TRUE,
          position = group_dodge
        ) +
        geom_segment(
          data = aid_reference,
          aes(
            x = x_pos - 0.35,
            xend = x_pos + 0.35,
            y = aid_accuracy_setting,
            yend = aid_accuracy_setting,
            colour = automation_reliability_pattern
          ),
          inherit.aes = FALSE,
          linetype = "dashed",
          linewidth = 0.8,
          show.legend = FALSE
        ) +
        geom_hline(yintercept = TARGET_ACC, linetype = "dashed") +
        geom_label(
          aes(
            label = sprintf("%.1f%%", mean_acc * 100)
          ),
          vjust = -0.80,
          size = 3.2,
          fontface = "bold",
          fill = "white",
          linewidth = 0,
          label.padding = grid::unit(0.08, "lines"),
          show.legend = FALSE,
          position = group_dodge
        ) +
        scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
        labs(
          x = NULL,
          y = "Mean accuracy",
          title = paste0(PLOT_LABEL, " accuracy by block and reliability pattern"),
          subtitle = accuracy_subtitle,
          caption = "Colored dashed segments show assigned aid accuracy in automation blocks"
        ) +
        coord_cartesian(ylim = acc_ylim, clip = "off") +
        theme_classic() +
        theme(
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(5.5, 12, 5.5, 5.5)
        )
      p_acc <- add_pattern_scales(p_acc, pattern_labels)
    }

    p_rt <- ggplot(
      rt_summary,
      aes(
        x = condition_label,
        y = mean_rt,
        group = automation_reliability_pattern,
        colour = automation_reliability_pattern
      )
    ) +
      geom_line(linewidth = 0.9, position = group_dodge) +
      geom_point(
        aes(shape = automation_reliability_pattern),
        size = 3.2,
        position = group_dodge
      ) +
      geom_errorbar(
        aes(
          ymin = mean_rt - se_rt,
          ymax = mean_rt + se_rt
        ),
        width = 0.12,
        na.rm = TRUE,
        position = group_dodge
      ) +
      geom_label(
        aes(
          label = sprintf("%.3f s", mean_rt)
        ),
        vjust = -0.80,
        size = 3.2,
        fontface = "bold",
        fill = "white",
        linewidth = 0,
        label.padding = grid::unit(0.08, "lines"),
        show.legend = FALSE,
        position = group_dodge
      ) +
      labs(
        x = NULL,
        y = "Mean RT (s)",
        title = paste0(PLOT_LABEL, " mean RT by block and reliability pattern"),
        subtitle = rt_subtitle
      ) +
      coord_cartesian(ylim = rt_ylim, clip = "off") +
      theme_classic() +
      theme(
        legend.position = "bottom",
        plot.margin = margin(5.5, 12, 5.5, 5.5)
      )
    p_rt <- add_pattern_scales(p_rt, pattern_labels)
  } else {
    if (!is.null(rating_summary)) {
      pattern_labels <- make_pattern_labels(summary_dat)
      p_acc <- ggplot() +
        geom_line(
          data = acc_summary,
          aes(
            x = condition_label,
            y = mean_acc,
            group = automation_reliability_pattern,
            colour = automation_reliability_pattern
          ),
          linewidth = 0.9
        ) +
        geom_point(
          data = acc_summary,
          aes(
            x = condition_label,
            y = mean_acc,
            colour = automation_reliability_pattern,
            shape = accuracy_measure
          ),
          size = 3.2
        ) +
        geom_point(
          data = rating_summary,
          aes(
            x = condition_label,
            y = mean_rated_accuracy,
            colour = automation_reliability_pattern,
            shape = accuracy_measure
          ),
          size = 3.2
        ) +
        geom_segment(
          data = aid_reference,
          aes(
            x = x_pos - 0.35,
            xend = x_pos + 0.35,
            y = aid_accuracy_setting,
            yend = aid_accuracy_setting,
            colour = automation_reliability_pattern
          ),
          inherit.aes = FALSE,
          linetype = "dashed",
          linewidth = 0.7,
          show.legend = FALSE
        ) +
        geom_hline(yintercept = TARGET_ACC, linetype = "dashed") +
        geom_label(
          data = acc_summary,
          aes(
            x = condition_label,
            y = mean_acc,
            label = sprintf("%.1f%%", mean_acc * 100)
          ),
          vjust = -0.80,
          size = 3.4,
          fontface = "bold",
          fill = "white",
          linewidth = 0,
          label.padding = grid::unit(0.10, "lines"),
          show.legend = FALSE
        ) +
        scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
        labs(
          x = NULL,
          y = "Accuracy / self-rated accuracy",
          title = paste0(PLOT_LABEL, " accuracy by block"),
          subtitle = accuracy_subtitle,
          caption = "Short dashed segments at automation blocks show assigned aid accuracy"
        ) +
        coord_cartesian(ylim = acc_ylim, clip = "off") +
        theme_classic() +
        theme(
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(5.5, 12, 5.5, 5.5)
        )
      p_acc <- add_accuracy_scales(p_acc, pattern_labels)
    } else {
      p_acc <- ggplot(acc_summary, aes(x = condition_label, y = mean_acc, group = 1)) +
        geom_line(linewidth = 0.8, colour = "black") +
        geom_point(size = 3, colour = "black") +
        geom_errorbar(
          aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
          width = 0.12,
          na.rm = TRUE
        ) +
        geom_segment(
          data = aid_reference,
          aes(
            x = x_pos - 0.35,
            xend = x_pos + 0.35,
            y = aid_accuracy_setting,
            yend = aid_accuracy_setting
          ),
          inherit.aes = FALSE,
          linetype = "dashed",
          linewidth = 0.7,
          colour = "forestgreen"
        ) +
        geom_hline(yintercept = TARGET_ACC, linetype = "dashed") +
        geom_label(
          aes(label = sprintf("%.1f%%", mean_acc * 100)),
          vjust = -0.80,
          size = 3.4,
          fontface = "bold",
          fill = "white",
          linewidth = 0,
          label.padding = grid::unit(0.10, "lines")
        ) +
        scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
        labs(
          x = NULL,
          y = "Mean accuracy",
          title = paste0(PLOT_LABEL, " accuracy by block"),
          subtitle = accuracy_subtitle
        ) +
        coord_cartesian(ylim = acc_ylim, clip = "off") +
        theme_classic() +
        theme(plot.margin = margin(5.5, 12, 5.5, 5.5))
    }

    p_rt <- ggplot(rt_summary, aes(x = condition_label, y = mean_rt, group = 1)) +
      geom_line(linewidth = 0.8, colour = "black") +
      geom_point(size = 3, colour = "black") +
      geom_errorbar(
        aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
        width = 0.12,
        na.rm = TRUE
      ) +
      geom_label(
        aes(label = sprintf("%.3f s", mean_rt)),
        vjust = -0.80,
        size = 3.4,
        fontface = "bold",
        fill = "white",
        linewidth = 0,
        label.padding = grid::unit(0.10, "lines")
      ) +
      labs(
        x = NULL,
        y = "Mean RT (s)",
        title = paste0(PLOT_LABEL, " mean RT by block"),
        subtitle = rt_subtitle
      ) +
      coord_cartesian(ylim = rt_ylim, clip = "off") +
      theme_classic() +
      theme(plot.margin = margin(5.5, 12, 5.5, 5.5))
  }

  list(
    acc_plot = p_acc,
    rating_plot = p_rating,
    rt_plot = p_rt,
    acc_summary = acc_summary,
    rt_summary = rt_summary,
    rating_summary = rating_summary
  )
}

make_timeout_plot <- function(dat, split_by_pattern = FALSE) {
  timeout_meta <- dat %>%
    filter(condition_deadline_code %in% ALL_CONDITION_CODES) %>%
    group_by(condition_deadline_code) %>%
    summarise(
      trial_deadline_s = first_non_missing(trial_deadline_s),
      .groups = "drop"
    ) %>%
    mutate(
      condition_deadline_code = factor(
        condition_deadline_code,
        levels = ALL_CONDITION_CODES
      ),
      condition_label = paste0(
        condition_display_name(condition_deadline_code),
        "\n",
        format(trial_deadline_s, trim = TRUE, scientific = FALSE),
        " s"
      )
    ) %>%
    arrange(condition_deadline_code)

  missing_conditions <- setdiff(
    ALL_CONDITION_CODES,
    as.character(timeout_meta$condition_deadline_code)
  )
  if (length(missing_conditions) > 0) {
    stop(
      "Missing TIMEOUT summary conditions: ",
      paste(missing_conditions, collapse = ", "),
      call. = FALSE
    )
  }

  subj_timeout <- dat %>%
    filter(condition_deadline_code %in% ALL_CONDITION_CODES) %>%
    mutate(
      condition_deadline_code = factor(
        condition_deadline_code,
        levels = ALL_CONDITION_CODES
      )
    )

  if (split_by_pattern) {
    subj_timeout <- subj_timeout %>%
      mutate(
        automation_reliability_pattern = factor(
          automation_reliability_pattern,
          levels = RELIABILITY_PATTERN_LEVELS
        )
      )
  }

  timeout_group_cols <- c(
    "participant_id",
    if (split_by_pattern) "automation_reliability_pattern",
    "condition_deadline_code"
  )

  subj_timeout <- subj_timeout %>%
    group_by(across(all_of(timeout_group_cols))) %>%
    summarise(
      n_trials = n(),
      n_timeout = sum(response == "TIMEOUT", na.rm = TRUE),
      timeout_pct = 100 * n_timeout / n_trials,
      .groups = "drop"
    )

  timeout_summary <- summarise_morey_condition_mean(
    data = subj_timeout,
    value_col = "timeout_pct",
    mean_name = "mean_timeout_pct",
    se_name = "se_timeout_pct",
    n_conditions = length(ALL_CONDITION_CODES),
    between_col = if (split_by_pattern) "automation_reliability_pattern" else NULL
  ) %>%
    left_join(timeout_meta, by = "condition_deadline_code") %>%
    mutate(
      condition_label = factor(
        condition_label,
        levels = timeout_meta$condition_label
      )
    )

  n_subjects <- n_distinct(subj_timeout$participant_id)
  timeout_subtitle <- if (split_by_pattern) {
    "Within-pattern Morey-Cousineau SEs across five blocks; HP65_LP95 n = 1 (no SE)"
  } else if (n_subjects >= 2) {
    "Error bars are Morey-Cousineau within-subject SEs across five blocks"
  } else {
    "Percentage of all trials in each block with response == TIMEOUT"
  }
  timeout_ylim <- get_axis_limits(
    timeout_summary$mean_timeout_pct,
    timeout_summary$se_timeout_pct,
    pad_prop = 0.30,
    bounds = c(0, 100)
  )

  if (split_by_pattern) {
    group_dodge <- position_dodge(width = GROUP_DODGE_WIDTH)
    pattern_labels <- make_pattern_labels(subj_timeout)
    timeout_summary <- timeout_summary %>%
      mutate(
        automation_reliability_pattern = factor(
          automation_reliability_pattern,
          levels = RELIABILITY_PATTERN_LEVELS
        ),
        label_vjust = case_when(
          automation_reliability_pattern == RELIABILITY_PATTERN_LEVELS[[1]] ~ -0.85,
          mean_timeout_pct <= 0.30 ~ -2.20,
          TRUE ~ 1.65
        )
      )

    p_timeout <- ggplot(
      timeout_summary,
      aes(
        x = condition_label,
        y = mean_timeout_pct,
        group = automation_reliability_pattern,
        colour = automation_reliability_pattern
      )
    ) +
      geom_line(linewidth = 0.9, position = group_dodge) +
      geom_point(
        aes(shape = automation_reliability_pattern),
        size = 3.2,
        position = group_dodge
      ) +
      geom_errorbar(
        aes(
          ymin = mean_timeout_pct - se_timeout_pct,
          ymax = mean_timeout_pct + se_timeout_pct
        ),
        width = 0.12,
        na.rm = TRUE,
        position = group_dodge
      ) +
      geom_label(
        aes(
          label = sprintf("%.2f%%", mean_timeout_pct),
          vjust = label_vjust
        ),
        size = 3.2,
        fontface = "bold",
        fill = "white",
        linewidth = 0,
        label.padding = grid::unit(0.08, "lines"),
        show.legend = FALSE,
        position = group_dodge
      ) +
      scale_y_continuous(labels = function(x) paste0(format(x, trim = TRUE), "%")) +
      labs(
        x = NULL,
        y = "TIMEOUT responses (%)",
        title = paste0(PLOT_LABEL, " TIMEOUT responses by block and reliability pattern"),
        subtitle = timeout_subtitle
      ) +
      coord_cartesian(
        ylim = c(min(-0.10, timeout_ylim[[1]]), timeout_ylim[[2]]),
        clip = "off"
      ) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        plot.margin = margin(5.5, 12, 5.5, 5.5)
      )
    p_timeout <- add_pattern_scales(p_timeout, pattern_labels)
  } else {
    p_timeout <- ggplot(
      timeout_summary,
      aes(x = condition_label, y = mean_timeout_pct, group = 1)
    ) +
      geom_line(linewidth = 0.8, colour = "black") +
      geom_point(size = 3, colour = "black") +
      geom_errorbar(
        aes(
          ymin = mean_timeout_pct - se_timeout_pct,
          ymax = mean_timeout_pct + se_timeout_pct
        ),
        width = 0.12,
        na.rm = TRUE
      ) +
      geom_label(
        aes(label = sprintf("%.2f%%", mean_timeout_pct)),
        vjust = -0.80,
        size = 3.4,
        fontface = "bold",
        fill = "white",
        linewidth = 0,
        label.padding = grid::unit(0.10, "lines")
      ) +
      scale_y_continuous(labels = function(x) paste0(format(x, trim = TRUE), "%")) +
      labs(
        x = NULL,
        y = "TIMEOUT responses (%)",
        title = paste0(PLOT_LABEL, " TIMEOUT responses by block"),
        subtitle = timeout_subtitle
      ) +
      coord_cartesian(ylim = timeout_ylim, clip = "off") +
      theme_classic() +
      theme(plot.margin = margin(5.5, 12, 5.5, 5.5))
  }

  list(plot = p_timeout, summary = timeout_summary, participant = subj_timeout)
}

make_calibration_plot <- function(dat) {
  dat_calib <- dat %>%
    filter(block == "CALIBRATION", condition_deadline_code == "CAL_LP") %>%
    accuracy_vars()

  if (nrow(dat_calib) == 0) {
    stop("No CAL_LP calibration trials found.", call. = FALSE)
  }

  dat_calib_post <- dat_calib %>%
    filter(trial > BURN_IN_TRIALS)

  if (nrow(dat_calib_post) == 0) {
    stop("No post-burn-in calibration trials found.", call. = FALSE)
  }

  n_last <- min(CALIB_SUMMARY_LAST_N, nrow(dat_calib_post))
  dat_calib_last_n <- dat_calib_post %>%
    slice_tail(n = n_last)

  acc_mean_last_n <- mean(dat_calib_last_n$correct_num, na.rm = TRUE)
  delta_mean_calib <- mean(dat_calib_last_n$delta_stair_realised, na.rm = TRUE)
  delta_sd_calib <- sd(dat_calib_last_n$delta_stair_realised, na.rm = TRUE)

  if (!is.finite(delta_sd_calib)) {
    delta_sd_calib <- 0
  }

  last_n_start_trial <- min(dat_calib_last_n$trial, na.rm = TRUE)
  delta_ymax <- nice_ceiling(c(
    dat_calib$delta_stair_mean + DELTA_SD,
    dat_calib$delta_stair_realised,
    delta_mean_calib + delta_sd_calib
  ))

  p_delta <- ggplot(dat_calib, aes(x = trial)) +
    geom_ribbon(
      data = dat_calib %>% filter(trial <= BURN_IN_TRIALS),
      aes(ymin = 0, ymax = delta_ymax),
      fill = "red",
      alpha = 0.15
    ) +
    geom_ribbon(
      data = dat_calib %>% filter(trial >= last_n_start_trial),
      aes(
        ymin = delta_mean_calib - delta_sd_calib,
        ymax = delta_mean_calib + delta_sd_calib
      ),
      fill = "orange",
      alpha = 0.40
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
      size = 1,
      alpha = 1
    ) +
    annotate(
      "segment",
      x = last_n_start_trial,
      xend = max(dat_calib$trial, na.rm = TRUE),
      y = delta_mean_calib,
      yend = delta_mean_calib,
      linetype = "dashed",
      colour = "orange",
      alpha = 1
    ) +
    geom_line(
      aes(y = delta_stair_mean),
      colour = "purple",
      linewidth = 0.75
    ) +
    scale_y_continuous(
      breaks = seq(0, delta_ymax, by = 0.02),
      limits = c(0, delta_ymax)
    ) +
    labs(x = NULL, y = "Delta") +
    theme_classic() +
    ggtitle(
      paste0("Calibration staircase: ", block_label(dat_calib)),
      subtitle = paste0(
        "Staircase-adjusted difficulty (dot proportion difference from 0.50)\n",
        "Summary uses last ", nrow(dat_calib_last_n), " post-burn-in calibration trials"
      )
    )

  p_acc <- ggplot(dat_calib, aes(x = trial)) +
    geom_ribbon(
      data = dat_calib %>% filter(trial <= BURN_IN_TRIALS),
      aes(ymin = 0, ymax = 1),
      fill = "red",
      alpha = 0.15
    ) +
    geom_point(
      aes(y = correct_num),
      shape = 4,
      size = 1,
      stroke = 0.8,
      alpha = 0.5,
      colour = "black"
    ) +
    geom_line(
      aes(y = acc_slide),
      linewidth = 0.50,
      colour = "steelblue",
      alpha = 0.70
    ) +
    geom_line(
      aes(y = acc_running),
      linewidth = 0.75,
      colour = "orange",
      alpha = 1
    ) +
    geom_hline(yintercept = TARGET_ACC, linetype = 2, colour = "purple") +
    geom_hline(
      yintercept = acc_mean_last_n,
      linetype = 1,
      linewidth = 0.5,
      colour = "orange",
      alpha = 0.5
    ) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      hjust = 1.05,
      vjust = -5.0,
      size = 3.5,
      colour = "black",
      label = sprintf("Target acc = %.2f", TARGET_ACC)
    ) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      hjust = 1.05,
      vjust = -3.0,
      size = 3.5,
      colour = "black",
      label = sprintf("Last %d acc = %.2f", nrow(dat_calib_last_n), acc_mean_last_n)
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "Trial", y = "Accuracy") +
    theme_classic() +
    ggtitle("", subtitle = "Trial correctness, rolling accuracy, and running accuracy")

  list(
    plot = p_delta / p_acc + plot_layout(heights = c(1, 1)),
    delta_ymax = delta_ymax,
    n_last = nrow(dat_calib_last_n)
  )
}

make_post_block_plot <- function(dat, code, title, delta_ymax, n_calib_last) {
  dat_block <- dat %>%
    filter(condition_deadline_code == code) %>%
    accuracy_vars()

  if (nrow(dat_block) == 0) {
    stop("No trials found for condition_deadline_code == ", code, call. = FALSE)
  }

  acc_mean <- mean(dat_block$correct_num, na.rm = TRUE)
  delta_mean <- first_non_missing(dat_block$delta_fixed_mean)
  delta_sd <- first_non_missing(dat_block$delta_fixed_sd)

  if (!is.finite(delta_mean)) {
    delta_mean <- NA_real_
  }
  if (!is.finite(delta_sd)) {
    delta_sd <- 0
  }

  observed_delta <- abs(0.50 - dat_block$vblack_prop)
  block_delta_ymax <- nice_ceiling(c(delta_ymax, observed_delta, delta_mean + delta_sd))

  p_delta <- ggplot(dat_block, aes(x = trial)) +
    geom_ribbon(
      aes(
        ymin = pmax(0, delta_mean - delta_sd),
        ymax = delta_mean + delta_sd
      ),
      fill = "orange",
      alpha = 0.40
    ) +
    geom_point(
      aes(y = abs(0.50 - vblack_prop)),
      colour = "orange",
      size = 1,
      alpha = 1
    ) +
    geom_hline(
      yintercept = delta_mean,
      linetype = "dashed",
      colour = "orange",
      alpha = 1
    ) +
    scale_y_continuous(
      breaks = seq(0, block_delta_ymax, by = 0.02),
      limits = c(0, block_delta_ymax)
    ) +
    labs(x = NULL, y = "Delta") +
    theme_classic() +
    ggtitle(
      paste0(title, ": ", block_label(dat_block)),
      subtitle = paste0(
        "Difficulty sampled from calibration mean and SD\n",
        "Calibration summary used last ", n_calib_last, " post-burn-in trials"
      )
    )

  p_acc <- ggplot(dat_block, aes(x = trial))

  if (any(!is.na(dat_block$aid_correct_num))) {
    aid_acc_mean <- mean(dat_block$aid_correct_num, na.rm = TRUE)
    aid_setting <- first_non_missing(dat_block$aid_accuracy_setting)

    p_acc <- p_acc +
      geom_point(
        aes(y = aid_correct_num),
        shape = 1,
        size = 1.4,
        stroke = 0.8,
        alpha = 0.6,
        colour = "forestgreen"
      ) +
      annotate(
        "text",
        x = Inf,
        y = -Inf,
        hjust = 1.05,
        vjust = -7.0,
        size = 3.5,
        colour = "black",
        label = sprintf("Aid acc = %.2f (assigned %.2f)", aid_acc_mean, aid_setting)
      )
  }

  p_acc <- p_acc +
    geom_point(
      aes(y = correct_num),
      shape = 4,
      size = 1,
      stroke = 0.8,
      alpha = 0.5,
      colour = "black"
    ) +
    geom_line(
      aes(y = acc_slide),
      linewidth = 0.50,
      colour = "steelblue",
      alpha = 0.70
    ) +
    geom_line(
      aes(y = acc_running),
      linewidth = 0.75,
      colour = "orange",
      alpha = 1
    ) +
    geom_hline(yintercept = TARGET_ACC, linetype = 2, colour = "purple") +
    geom_hline(
      yintercept = acc_mean,
      linetype = 1,
      linewidth = 0.5,
      colour = "orange",
      alpha = 0.5
    ) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      hjust = 1.05,
      vjust = -5.0,
      size = 3.5,
      colour = "black",
      label = sprintf("Target acc = %.2f", TARGET_ACC)
    ) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      hjust = 1.05,
      vjust = -3.0,
      size = 3.5,
      colour = "black",
      label = sprintf("Observed acc = %.2f", acc_mean)
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "Trial", y = "Accuracy") +
    theme_classic() +
    ggtitle("", subtitle = "Trial correctness, rolling accuracy, and running accuracy")

  p_delta / p_acc + plot_layout(heights = c(1, 1))
}

if (PLOT_MODE %in% c("cohort", "group", "accuracy") && dir.exists(INPUT_PATH)) {
  stop(
    "Cohort/group/accuracy mode requires a collated data_virus_all.csv file.",
    call. = FALSE
  )
}

input_file <- resolve_input_file(INPUT_PATH)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

message("Reading: ", input_file)
dat <- read_task_csv(input_file)

slider_required <- PLOT_MODE %in% c("cohort", "group", "accuracy")
slider_explicit <- !is.na(SLIDER_INPUT_PATH) && nzchar(SLIDER_INPUT_PATH)
slider_file <- if (slider_required || slider_explicit) {
  resolve_slider_file(
    input_file = input_file,
    explicit_path = SLIDER_INPUT_PATH,
    required = slider_required
  )
} else {
  NA_character_
}
slider_dat <- if (is.na(slider_file)) NULL else read_slider_csv(slider_file)

if (!is.null(slider_dat)) {
  validate_slider_coverage(
    slider_dat,
    sort(unique(dat$participant_id[!is.na(dat$participant_id)]))
  )
  message("Reading slider ratings: ", slider_file)
}

message("Detected blocks:")
print(dat %>%
  distinct(participant_id, block_idx, block, condition_deadline_code,
           time_pressure_condition, trial_deadline_s) %>%
  arrange(participant_id, block_idx))

if (PLOT_MODE %in% c("cohort", "group", "accuracy")) {
  validate_condition_coverage(dat, ALL_CONDITION_CODES)

  base_output_dir <- OUTPUT_DIR
  cohort_prefix <- OUTPUT_PREFIX
  cohort_label <- PLOT_LABEL
  participant_ids <- sort(unique(dat$participant_id[!is.na(dat$participant_id)]))
  written <- character(0)

  if (PLOT_MODE %in% c("cohort", "accuracy")) for (pid in participant_ids) {
    pid_prefix <- sprintf("p%03d", as.integer(pid))
    OUTPUT_DIR <- file.path(base_output_dir, "individual", pid_prefix)
    OUTPUT_PREFIX <- pid_prefix
    PLOT_LABEL <- paste("Participant", sprintf("%03d", as.integer(pid)))
    dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

    dat_pid <- dat %>% filter(participant_id == pid)
    slider_pid <- slider_dat %>% filter(participant_id == pid)
    condition_summary <- make_condition_summary_plots(
      dat_pid,
      slider_dat = slider_pid
    )
    condition_summary_combined <- (
      condition_summary$acc_plot +
        labs(title = NULL) +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    ) / (
      condition_summary$rt_plot + labs(title = NULL)
    ) +
      plot_layout(guides = "collect") +
      plot_annotation(title = paste0(PLOT_LABEL, " accuracy and RT by block")) &
      theme(legend.position = "bottom", legend.box = "vertical")

    written <- c(
      written,
      save_plot_pair(
        condition_summary$acc_plot,
        paste0(OUTPUT_PREFIX, "_block_accuracy_means"),
        width = 8.5,
        height = 5
      ),
      save_plot_pair(
        condition_summary_combined,
        paste0(OUTPUT_PREFIX, "_block_accuracy_rt_means"),
        width = 8.5,
        height = 8
      )
    )

    if (PLOT_MODE == "cohort") {
      calib <- make_calibration_plot(dat_pid)
      calibration_plot <- calib$plot +
        plot_annotation(title = paste0(PLOT_LABEL, " calibration dynamics"))
      timeout_summary <- make_timeout_plot(dat_pid)
      written <- c(
        written,
        save_plot_pair(
          calibration_plot,
          paste0(OUTPUT_PREFIX, "_calibration_dynamics"),
          width = 9,
          height = 6.5
        ),
        save_plot_pair(
          condition_summary$rt_plot,
          paste0(OUTPUT_PREFIX, "_block_rt_means"),
          width = 8.5,
          height = 5
        ),
        save_plot_pair(
          timeout_summary$plot,
          paste0(OUTPUT_PREFIX, "_block_timeout_percent"),
          width = 8.5,
          height = 5
        )
      )
    }

    message(PLOT_LABEL, " accuracy summary:")
    print(condition_summary$acc_summary %>%
      select(condition_deadline_code, mean_acc))
    message(PLOT_LABEL, " self-rating summary:")
    print(condition_summary$rating_summary %>%
      select(accuracy_measure, condition_deadline_code, mean_rated_accuracy))
    message(PLOT_LABEL, " RT summary:")
    print(condition_summary$rt_summary %>%
      select(condition_deadline_code, mean_rt))
    if (PLOT_MODE == "cohort") {
      message(PLOT_LABEL, " TIMEOUT summary:")
      print(timeout_summary$participant)
    }
  }

  OUTPUT_DIR <- file.path(base_output_dir, "group")
  OUTPUT_PREFIX <- paste0(cohort_prefix, "_group")
  PLOT_LABEL <- paste0(cohort_label, " group")
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  group_condition_summary <- make_condition_summary_plots(
    dat,
    slider_dat = slider_dat,
    split_by_pattern = TRUE
  )
  group_condition_combined <- (
    group_condition_summary$acc_plot +
      labs(title = NULL, caption = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      guides(colour = "none", shape = "none")
  ) / (
    group_condition_summary$rating_plot +
      labs(title = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  ) / (
    group_condition_summary$rt_plot +
      labs(title = NULL) +
      guides(colour = "none", shape = "none")
  ) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = paste0(PLOT_LABEL, " accuracy, self-ratings, and RT by block")
    ) &
    theme(legend.position = "bottom", legend.box = "vertical")

  written <- c(
    written,
    save_plot_pair(
      group_condition_summary$acc_plot,
      paste0(OUTPUT_PREFIX, "_block_accuracy_means"),
      width = 8.5,
      height = 6
    ),
    save_plot_pair(
      group_condition_summary$rating_plot,
      paste0(OUTPUT_PREFIX, "_block_self_rated_accuracy_means"),
      width = 8.5,
      height = 6
    ),
    save_plot_pair(
      group_condition_combined,
      paste0(OUTPUT_PREFIX, "_block_accuracy_rt_means"),
      width = 8.5,
      height = 11
    )
  )

  if (PLOT_MODE %in% c("cohort", "group")) {
    group_timeout_summary <- make_timeout_plot(
      dat,
      split_by_pattern = TRUE
    )
    written <- c(
      written,
      save_plot_pair(
        group_condition_summary$rt_plot,
        paste0(OUTPUT_PREFIX, "_block_rt_means"),
        width = 8.5,
        height = 5
      ),
      save_plot_pair(
        group_timeout_summary$plot,
        paste0(OUTPUT_PREFIX, "_block_timeout_percent"),
        width = 8.5,
        height = 5
      )
    )
  }

  message("Group accuracy summary:")
  print(group_condition_summary$acc_summary %>%
    select(automation_reliability_pattern, condition_deadline_code,
           mean_acc, se_acc, n_participants))
  message("Group self-rating summary:")
  print(group_condition_summary$rating_summary %>%
    select(automation_reliability_pattern, accuracy_measure,
           condition_deadline_code, mean_rated_accuracy,
           se_rated_accuracy, n_participants))
  message("Group RT summary:")
  print(group_condition_summary$rt_summary %>%
    select(automation_reliability_pattern, condition_deadline_code,
           mean_rt, se_rt, n_participants))
  if (PLOT_MODE %in% c("cohort", "group")) {
    message("Group TIMEOUT summary:")
    print(group_timeout_summary$summary %>%
      select(automation_reliability_pattern, condition_deadline_code,
             mean_timeout_pct, se_timeout_pct, n_participants))
  }
} else {
  calib <- make_calibration_plot(dat)

  p_manual_hp <- make_post_block_plot(
    dat = dat,
    code = "M_HP",
    title = "Manual HP block",
    delta_ymax = calib$delta_ymax,
    n_calib_last = calib$n_last
  )

  p_manual_lp <- make_post_block_plot(
    dat = dat,
    code = "M_LP",
    title = "Manual LP block",
    delta_ymax = calib$delta_ymax,
    n_calib_last = calib$n_last
  )

  p_auto_hp <- make_post_block_plot(
    dat = dat,
    code = "A_HP",
    title = "Automation HP block",
    delta_ymax = calib$delta_ymax,
    n_calib_last = calib$n_last
  )

  p_auto_lp <- make_post_block_plot(
    dat = dat,
    code = "A_LP",
    title = "Automation LP block",
    delta_ymax = calib$delta_ymax,
    n_calib_last = calib$n_last
  )

  p_manual <- wrap_plots(
    wrap_elements(full = p_manual_hp),
    wrap_elements(full = p_manual_lp),
    ncol = 2
  ) +
    plot_annotation(title = "Manual HP/LP performance dynamics")

  p_auto <- wrap_plots(
    wrap_elements(full = p_auto_hp),
    wrap_elements(full = p_auto_lp),
    ncol = 2
  ) +
    plot_annotation(title = "Automation HP/LP performance dynamics")

  p_all <- wrap_plots(
    wrap_elements(full = calib$plot),
    wrap_elements(full = p_manual),
    wrap_elements(full = p_auto),
    ncol = 1
  ) +
    plot_annotation(title = "Calibration and post-calibration performance dynamics")

  condition_summary <- make_condition_summary_plots(dat, slider_dat = slider_dat)
  condition_summary_combined <- (
    condition_summary$acc_plot +
      labs(title = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  ) / (
    condition_summary$rt_plot + labs(title = NULL)
  ) +
    plot_annotation(title = paste0(PLOT_LABEL, " condition means"))

  written <- c(
    save_plot_pair(
      calib$plot,
      paste0(OUTPUT_PREFIX, "_calibration_staircase"),
      width = 9,
      height = 6
    ),
    save_plot_pair(
      p_manual,
      paste0(OUTPUT_PREFIX, "_manual_hp_lp_dynamics"),
      width = 16,
      height = 9
    ),
    save_plot_pair(
      p_auto,
      paste0(OUTPUT_PREFIX, "_automation_hp_lp_dynamics"),
      width = 16,
      height = 9
    ),
    save_plot_pair(
      p_all,
      paste0(OUTPUT_PREFIX, "_all_dynamics"),
      width = 16,
      height = 18
    ),
    save_plot_pair(
      condition_summary$acc_plot,
      paste0(OUTPUT_PREFIX, "_condition_accuracy_means"),
      width = 7,
      height = 4.5
    ),
    save_plot_pair(
      condition_summary$rt_plot,
      paste0(OUTPUT_PREFIX, "_condition_rt_means"),
      width = 7,
      height = 4.5
    ),
    save_plot_pair(
      condition_summary_combined,
      paste0(OUTPUT_PREFIX, "_condition_accuracy_rt_means"),
      width = 7,
      height = 4.25
    )
  )

  message("Condition accuracy summary:")
  print(condition_summary$acc_summary %>%
    select(condition_deadline_code, condition_label, mean_acc, se_acc, n_participants))
  message("Condition RT summary:")
  print(condition_summary$rt_summary %>%
    select(condition_deadline_code, condition_label, mean_rt, se_rt, n_participants))
}

message("Wrote plots:")
writeLines(written)
