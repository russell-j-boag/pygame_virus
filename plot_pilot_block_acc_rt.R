# Plot observed accuracy, self-rated accuracy, and RT by block/drop phase.
#
# Usage:
#   Rscript plot_pilot_block_acc_rt.R [input_dir_or_file] [output_dir] \
#     [output_prefix] [plot_label] [mode] [slider_file]
#
# Modes:
#   single  - plot one participant (slider_file optional)
#   cohort  - plot every participant plus CAL65/CAL90 group summaries
#   group   - plot only CAL65/CAL90 group summaries
#
# Individual output includes a calibration-dynamics plot showing staircase
# difficulty and rolling/cumulative accuracy across the 300 calibration trials.

rm(list = ls())

library("ggplot2")
library("patchwork")
library("dplyr")
library("readr")
library("tidyr")

fontconfig_cache <- file.path(tempdir(), "fontconfig-cache")
dir.create(fontconfig_cache, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(XDG_CACHE_HOME = fontconfig_cache)

args <- commandArgs(trailingOnly = TRUE)

INPUT_PATH <- if (length(args) >= 1) args[[1]] else "output/pilot_harry"
OUTPUT_DIR <- if (length(args) >= 2) args[[2]] else "plots"
OUTPUT_PREFIX <- if (length(args) >= 3) args[[3]] else "pilot_harry"
PLOT_LABEL <- if (length(args) >= 4) args[[4]] else "Pilot"
PLOT_MODE <- if (length(args) >= 5) args[[5]] else "single"
SLIDER_INPUT_PATH <- if (length(args) >= 6) args[[6]] else NA_character_

if (!PLOT_MODE %in% c("single", "cohort", "group")) {
  stop("mode must be 'single', 'cohort', or 'group'", call. = FALSE)
}

CALIB_SUMMARY_LAST_N <- 150
BURN_IN_TRIALS <- 50
ACCURACY_ROLLING_WINDOW <- 25
DELTA_SD <- 0.01
GROUP_DODGE_WIDTH <- 0.28
GROUP_RT_LABEL_OFFSET <- 0.04
OBSERVED_LEVELS <- c(
  "Calibration",
  "Manual pre",
  "Auto P1 95%",
  "Auto P2 70%",
  "Auto P3 95%",
  "Manual post"
)
RATING_LEVELS <- c(
  "Calibration",
  "Manual pre",
  "Automation overall",
  "Manual post"
)
TARGET_GROUP_LEVELS <- c("CAL65", "CAL90")
TARGET_GROUP_COLOURS <- c(
  "CAL65" = "#0072B2",
  "CAL90" = "#D55E00"
)
TARGET_GROUP_SHAPES <- c(
  "CAL65" = 16,
  "CAL90" = 17
)
RATING_MEASURE_LEVELS <- c(
  "Self-rated own accuracy",
  "Self-rated aid accuracy"
)
RATING_MEASURE_SHAPES <- c(
  "Self-rated own accuracy" = 15,
  "Self-rated aid accuracy" = 18
)

required_trial_cols <- c(
  "participant_id",
  "block",
  "block_idx",
  "calibration_target_group",
  "calibration_target_accuracy",
  "manual_segment",
  "reliability_phase_idx",
  "reliability_phase_label",
  "aid_reliability_level",
  "trial",
  "correct",
  "rt_s",
  "delta_stair_realised",
  "delta_stair_mean"
)

as_binary <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  case_when(
    is.na(x_chr) | x_chr == "na" ~ NA_real_,
    x_chr %in% c("true", "t", "1") ~ 1,
    x_chr %in% c("false", "f", "0") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

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
    recursive = TRUE,
    full.names = TRUE
  )

  if (!length(files)) {
    stop("No results_*_b00_ALL.csv file found in ", path, call. = FALSE)
  }

  files[[which.max(file.info(files)$mtime)]]
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

read_trial_csv <- function(path) {
  dat <- read_csv(path, show_col_types = FALSE)
  missing_cols <- setdiff(required_trial_cols, names(dat))
  if (length(missing_cols)) {
    stop(
      "Trial input is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  dat
}

read_slider_csv <- function(path) {
  dat <- read_csv(path, show_col_types = FALSE)

  if (!"question_key" %in% names(dat) && "slider_key" %in% names(dat)) {
    dat$question_key <- dat$slider_key
  }
  if (!"response_percent" %in% names(dat) && "response" %in% names(dat)) {
    dat$response_percent <- dat$response
  }

  required <- c(
    "participant_id",
    "block",
    "calibration_target_group",
    "calibration_target_accuracy",
    "manual_segment",
    "reliability_phase_label",
    "question_key",
    "response_percent"
  )
  missing_cols <- setdiff(required, names(dat))
  if (length(missing_cols)) {
    stop(
      "Slider input is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  dat
}

observed_phase <- function(block, manual_segment, reliability_phase_label) {
  case_when(
    block == "CALIBRATION" ~ "Calibration",
    block == "MANUAL" & manual_segment == "PRE_AUTOMATION" ~ "Manual pre",
    block == "AUTOMATION" & reliability_phase_label == "P1_95" ~ "Auto P1 95%",
    block == "AUTOMATION" & reliability_phase_label == "P2_70" ~ "Auto P2 70%",
    block == "AUTOMATION" & reliability_phase_label == "P3_95" ~ "Auto P3 95%",
    block == "MANUAL" & manual_segment == "POST_AUTOMATION" ~ "Manual post",
    TRUE ~ NA_character_
  )
}

prepare_trial_data <- function(dat) {
  dat <- dat %>%
    mutate(
      participant_id = suppressWarnings(as.integer(participant_id)),
      trial = suppressWarnings(as.integer(trial)),
      rt_s = suppressWarnings(as.numeric(rt_s)),
      delta_stair_realised = suppressWarnings(as.numeric(delta_stair_realised)),
      delta_stair_mean = suppressWarnings(as.numeric(delta_stair_mean)),
      calibration_target_accuracy = suppressWarnings(
        as.numeric(calibration_target_accuracy)
      ),
      aid_reliability_level = suppressWarnings(as.numeric(aid_reliability_level)),
      invalid_nonpositive_rt = !is.na(rt_s) & is.finite(rt_s) & rt_s <= 0
    )

  invalid_rows <- dat %>%
    filter(invalid_nonpositive_rt) %>%
    select(
      participant_id,
      block,
      manual_segment,
      reliability_phase_label,
      trial,
      correct,
      rt_s
    )

  if (nrow(invalid_rows)) {
    message(
      "Excluding ",
      nrow(invalid_rows),
      " nonpositive-RT trial(s) from both accuracy and RT summaries:"
    )
    print(invalid_rows)
  }

  dat %>%
    filter(!invalid_nonpositive_rt) %>%
    mutate(
      correct_num = replace_na(as_binary(correct), 0),
      rt_valid = if_else(!is.na(rt_s) & is.finite(rt_s) & rt_s > 0, rt_s, NA_real_),
      phase_label = observed_phase(block, manual_segment, reliability_phase_label),
      phase_label = factor(phase_label, levels = OBSERVED_LEVELS),
      phase_order = as.integer(phase_label),
      calibration_target_group = factor(
        calibration_target_group,
        levels = TARGET_GROUP_LEVELS
      )
    ) %>%
    filter(!is.na(participant_id), !is.na(phase_label))
}

restrict_calibration_trials <- function(dat) {
  dat %>%
    group_by(participant_id, phase_label) %>%
    arrange(trial, .by_group = TRUE) %>%
    filter(
      as.character(phase_label) != "Calibration" |
        row_number() > pmax(n() - CALIB_SUMMARY_LAST_N, 0)
    ) %>%
    ungroup()
}

validate_trial_coverage <- function(dat) {
  coverage <- dat %>%
    distinct(participant_id, phase_label) %>%
    count(participant_id, name = "n_phases")

  incomplete <- coverage %>% filter(n_phases != length(OBSERVED_LEVELS))
  if (nrow(coverage) == 0 || nrow(incomplete)) {
    stop(
      "Each participant must have all six observed block/drop phases. ",
      "Incomplete participants: ",
      paste(incomplete$participant_id, collapse = ", "),
      call. = FALSE
    )
  }
}

make_participant_observed_summary <- function(dat) {
  dat %>%
    group_by(
      participant_id,
      calibration_target_group,
      calibration_target_accuracy,
      phase_label,
      phase_order
    ) %>%
    summarise(
      mean_accuracy = mean(correct_num, na.rm = TRUE),
      mean_rt = mean(rt_valid, na.rm = TRUE),
      n_accuracy = n(),
      n_rt = sum(!is.na(rt_valid)),
      aid_target = if (all(is.na(aid_reliability_level))) {
        NA_real_
      } else {
        mean(aid_reliability_level, na.rm = TRUE)
      },
      .groups = "drop"
    ) %>%
    mutate(
      phase_label = factor(phase_label, levels = OBSERVED_LEVELS),
      accuracy_label = sprintf("%.1f%%", mean_accuracy * 100),
      rt_label = sprintf("%.3f s", mean_rt)
    )
}

prepare_slider_data <- function(dat) {
  out <- dat %>%
    mutate(
      participant_id = suppressWarnings(as.integer(participant_id)),
      response_percent = suppressWarnings(as.numeric(response_percent)),
      calibration_target_accuracy = suppressWarnings(
        as.numeric(calibration_target_accuracy)
      ),
      calibration_target_group = factor(
        calibration_target_group,
        levels = TARGET_GROUP_LEVELS
      ),
      rating_measure = recode(
        question_key,
        "perc_self_correct" = "Self-rated own accuracy",
        "perc_auto_correct" = "Self-rated aid accuracy",
        .default = NA_character_
      ),
      rating_label = case_when(
        question_key == "perc_self_correct" & block == "CALIBRATION" ~ "Calibration",
        question_key == "perc_self_correct" & block == "MANUAL" &
          manual_segment == "PRE_AUTOMATION" ~ "Manual pre",
        question_key == "perc_auto_correct" & block == "AUTOMATION" &
          reliability_phase_label == "DROP95_70_95" ~ "Automation overall",
        question_key == "perc_self_correct" & block == "MANUAL" &
          manual_segment == "POST_AUTOMATION" ~ "Manual post",
        TRUE ~ NA_character_
      ),
      rating_label = factor(rating_label, levels = RATING_LEVELS),
      rating_order = as.integer(rating_label),
      rating_measure = factor(rating_measure, levels = RATING_MEASURE_LEVELS),
      rated_accuracy = response_percent / 100
    ) %>%
    filter(
      !is.na(participant_id),
      !is.na(rating_label),
      !is.na(rating_measure),
      is.finite(rated_accuracy),
      rated_accuracy >= 0,
      rated_accuracy <= 1
    ) %>%
    group_by(
      participant_id,
      calibration_target_group,
      calibration_target_accuracy,
      rating_measure,
      rating_label,
      rating_order
    ) %>%
    summarise(rated_accuracy = mean(rated_accuracy), .groups = "drop") %>%
    mutate(
      rating_accuracy_label = sprintf("%.1f%%", rated_accuracy * 100),
      rating_label_vjust = if_else(rated_accuracy >= 0.94, 1.6, -1.0)
    )

  coverage <- out %>% count(participant_id, name = "n_ratings")
  incomplete <- coverage %>% filter(n_ratings != length(RATING_LEVELS))
  if (nrow(coverage) == 0 || nrow(incomplete)) {
    stop(
      "Each participant must have the four expected ratings. Incomplete participants: ",
      paste(incomplete$participant_id, collapse = ", "),
      call. = FALSE
    )
  }

  out
}

make_rating_references <- function(trial_dat, observed_dat) {
  observed_reference <- observed_dat %>%
    mutate(
      rating_label = case_when(
        as.character(phase_label) == "Calibration" ~ "Calibration",
        as.character(phase_label) == "Manual pre" ~ "Manual pre",
        grepl("^Auto", as.character(phase_label)) ~ "Automation overall",
        as.character(phase_label) == "Manual post" ~ "Manual post",
        TRUE ~ NA_character_
      ),
      rating_label = factor(rating_label, levels = RATING_LEVELS),
      rating_order = as.integer(rating_label)
    ) %>%
    filter(!is.na(rating_label)) %>%
    group_by(
      participant_id,
      calibration_target_group,
      calibration_target_accuracy,
      rating_label,
      rating_order
    ) %>%
    summarise(observed_accuracy = mean(correct_num), .groups = "drop")

  auto_target <- trial_dat %>%
    filter(block == "AUTOMATION", !is.na(aid_reliability_level)) %>%
    distinct(participant_id, reliability_phase_label, aid_reliability_level) %>%
    group_by(participant_id) %>%
    summarise(target_accuracy = mean(aid_reliability_level), .groups = "drop") %>%
    mutate(
      rating_label = factor("Automation overall", levels = RATING_LEVELS),
      rating_order = as.integer(rating_label)
    )

  own_target <- trial_dat %>%
    distinct(
      participant_id,
      calibration_target_group,
      calibration_target_accuracy
    ) %>%
    crossing(rating_label = c("Calibration", "Manual pre", "Manual post")) %>%
    mutate(
      target_accuracy = calibration_target_accuracy,
      rating_label = factor(rating_label, levels = RATING_LEVELS),
      rating_order = as.integer(rating_label)
    ) %>%
    select(participant_id, rating_label, rating_order, target_accuracy)

  target_reference <- bind_rows(own_target, auto_target)

  list(observed = observed_reference, target = target_reference)
}

summarise_repeated <- function(
  data,
  value_col,
  condition_col,
  use_morey = TRUE
) {
  value_sym <- rlang::sym(value_col)
  condition_sym <- rlang::sym(condition_col)

  clean <- data %>%
    filter(
      !is.na(calibration_target_group),
      !is.na(participant_id),
      !is.na(!!condition_sym),
      is.finite(!!value_sym)
    )

  clean %>%
    group_by(calibration_target_group) %>%
    group_modify(function(.x, .y) {
      n_conditions <- n_distinct(.x[[condition_col]])
      n_subjects <- n_distinct(.x$participant_id)

      if (use_morey && n_conditions > 1) {
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
            sd(.normalized_value) / sqrt(n_distinct(participant_id)) * correction
          } else {
            NA_real_
          },
          n_participants = n_distinct(participant_id),
          .groups = "drop"
        )
    }) %>%
    ungroup()
}

get_axis_limits <- function(values, ses = NULL, bounds = c(-Inf, Inf), pad = 0.12) {
  if (is.null(ses)) {
    ses <- rep(0, length(values))
  }
  lo <- suppressWarnings(min(values - ses, na.rm = TRUE))
  hi <- suppressWarnings(max(values + ses, na.rm = TRUE))
  if (!is.finite(lo) || !is.finite(hi)) {
    return(bounds)
  }
  span <- hi - lo
  extra <- if (span > 0) span * pad else max(abs(lo) * 0.05, 0.05)
  c(max(bounds[[1]], lo - extra), min(bounds[[2]], hi + extra))
}

make_group_labels <- function(trial_dat) {
  trial_dat %>%
    distinct(participant_id, calibration_target_group) %>%
    count(calibration_target_group, name = "n") %>%
    mutate(label = paste0(calibration_target_group, " (n = ", n, ")")) %>%
    { setNames(.$label, as.character(.$calibration_target_group)) }
}

target_colour_scale <- function(group_labels) {
  scale_colour_manual(
    values = TARGET_GROUP_COLOURS,
    breaks = intersect(TARGET_GROUP_LEVELS, names(group_labels)),
    labels = group_labels[intersect(TARGET_GROUP_LEVELS, names(group_labels))],
    name = "Calibration target"
  )
}

target_shape_scale <- function(group_labels) {
  scale_shape_manual(
    values = TARGET_GROUP_SHAPES,
    breaks = intersect(TARGET_GROUP_LEVELS, names(group_labels)),
    labels = group_labels[intersect(TARGET_GROUP_LEVELS, names(group_labels))],
    name = "Calibration target"
  )
}

make_accuracy_references <- function(observed_summary) {
  own <- observed_summary %>%
    filter(phase_order %in% c(1, 2, 6)) %>%
    transmute(
      calibration_target_group,
      phase_order,
      target_accuracy = calibration_target_accuracy
    )

  aid <- observed_summary %>%
    filter(phase_order %in% c(3, 4, 5)) %>%
    transmute(phase_order, target_accuracy = aid_target) %>%
    distinct()

  list(own = own, aid = aid)
}

make_calibration_dynamics_plot <- function(calibration_trials, plot_label) {
  calibration_trials <- calibration_trials %>%
    arrange(trial) %>%
    mutate(
      running_accuracy = cumsum(correct_num) / row_number(),
      rolling_accuracy = zoo::rollapply(
        correct_num,
        width = ACCURACY_ROLLING_WINDOW,
        FUN = mean,
        align = "right",
        fill = NA_real_,
        partial = TRUE
      )
    )

  if (!nrow(calibration_trials)) {
    stop(plot_label, " has no calibration trials.", call. = FALSE)
  }

  post_burnin <- calibration_trials %>% filter(trial > BURN_IN_TRIALS)
  if (!nrow(post_burnin)) {
    post_burnin <- calibration_trials
  }
  final_window <- post_burnin %>%
    slice_tail(n = min(CALIB_SUMMARY_LAST_N, nrow(post_burnin)))

  window_start <- min(final_window$trial)
  window_end <- max(final_window$trial)
  target_group <- as.character(first(calibration_trials$calibration_target_group))
  target_accuracy <- first(calibration_trials$calibration_target_accuracy)
  whole_accuracy <- mean(calibration_trials$correct_num)
  final_accuracy <- mean(final_window$correct_num)
  final_delta_mean <- mean(final_window$delta_stair_realised, na.rm = TRUE)
  final_delta_sd <- sd(final_window$delta_stair_realised, na.rm = TRUE)

  if (!is.finite(final_delta_sd)) {
    final_delta_sd <- 0
  }

  delta_max <- max(
    calibration_trials$delta_stair_realised,
    calibration_trials$delta_stair_mean + DELTA_SD,
    final_delta_mean + final_delta_sd,
    na.rm = TRUE
  )
  if (!is.finite(delta_max) || delta_max <= 0) {
    delta_max <- 0.10
  }
  delta_max <- max(0.02, ceiling(delta_max / 0.02) * 0.02)

  p_delta <- ggplot(calibration_trials, aes(x = trial)) +
    geom_ribbon(
      data = calibration_trials %>% filter(trial <= BURN_IN_TRIALS),
      aes(ymin = 0, ymax = delta_max),
      fill = "red",
      alpha = 0.15
    ) +
    geom_ribbon(
      data = calibration_trials %>% filter(trial >= window_start),
      aes(
        ymin = pmax(0, final_delta_mean - final_delta_sd),
        ymax = final_delta_mean + final_delta_sd
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
      alpha = 1,
      na.rm = TRUE
    ) +
    annotate(
      "segment",
      x = window_start,
      xend = window_end,
      y = final_delta_mean,
      yend = final_delta_mean,
      linetype = "dashed",
      colour = "orange",
      alpha = 1
    ) +
    geom_line(
      aes(y = delta_stair_mean),
      colour = "purple",
      linewidth = 0.75,
      na.rm = TRUE
    ) +
    scale_y_continuous(
      breaks = seq(0, delta_max, by = 0.02),
      limits = c(0, delta_max)
    ) +
    labs(x = NULL, y = "Delta") +
    theme_classic() +
    ggtitle(
      paste0(
        plot_label,
        " calibration staircase: ",
        target_group,
        " target ",
        sprintf("%.0f%%", target_accuracy * 100)
      ),
      subtitle = paste0(
        "Staircase-adjusted difficulty (dot proportion difference from 0.50)\n",
        "Summary uses last ", nrow(final_window),
        " post-burn-in calibration trials"
      )
    )

  p_accuracy <- ggplot(calibration_trials, aes(x = trial)) +
    geom_ribbon(
      data = calibration_trials %>% filter(trial <= BURN_IN_TRIALS),
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
      aes(y = rolling_accuracy),
      linewidth = 0.50,
      colour = "steelblue",
      alpha = 0.70
    ) +
    geom_line(
      aes(y = running_accuracy),
      linewidth = 0.75,
      colour = "orange",
      alpha = 1
    ) +
    geom_hline(
      yintercept = target_accuracy,
      linetype = 2,
      colour = "purple"
    ) +
    geom_hline(
      yintercept = final_accuracy,
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
      vjust = -7.0,
      size = 3.5,
      colour = "black",
      label = sprintf("Target acc = %.2f", target_accuracy)
    ) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      hjust = 1.05,
      vjust = -5.0,
      size = 3.5,
      colour = "black",
      label = sprintf("Whole-block acc = %.2f", whole_accuracy)
    ) +
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      hjust = 1.05,
      vjust = -3.0,
      size = 3.5,
      colour = "black",
      label = sprintf("Last %d acc = %.2f", nrow(final_window), final_accuracy)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1)
    ) +
    labs(x = "Trial", y = "Accuracy") +
    theme_classic() +
    ggtitle("", subtitle = "Trial correctness, rolling accuracy, and running accuracy")

  p_delta / p_accuracy + plot_layout(heights = c(1, 1))
}

make_individual_plots <- function(
  observed_summary,
  rating_summary = NULL,
  rating_references = NULL,
  plot_label
) {
  pid <- unique(observed_summary$participant_id)
  group_labels <- make_group_labels(observed_summary)
  refs <- make_accuracy_references(observed_summary)
  acc_ylim <- get_axis_limits(
    c(
      observed_summary$mean_accuracy,
      refs$own$target_accuracy,
      refs$aid$target_accuracy
    ),
    bounds = c(0, 1),
    pad = 0.18
  )
  rt_ylim <- get_axis_limits(observed_summary$mean_rt, bounds = c(0, Inf), pad = 0.20)

  p_acc <- ggplot(
    observed_summary,
    aes(
      x = phase_order,
      y = mean_accuracy,
      colour = calibration_target_group,
      group = 1
    )
  ) +
    geom_segment(
      data = refs$own,
      aes(
        x = phase_order - 0.32,
        xend = phase_order + 0.32,
        y = target_accuracy,
        yend = target_accuracy
      ),
      inherit.aes = FALSE,
      colour = "black",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_segment(
      data = refs$aid,
      aes(
        x = phase_order - 0.32,
        xend = phase_order + 0.32,
        y = target_accuracy,
        yend = target_accuracy
      ),
      inherit.aes = FALSE,
      colour = "black",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 3) +
    geom_text(
      aes(label = accuracy_label),
      vjust = -1.0,
      size = 3.4,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_x_continuous(breaks = seq_along(OBSERVED_LEVELS), labels = OBSERVED_LEVELS) +
    scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    target_colour_scale(group_labels) +
    coord_cartesian(ylim = acc_ylim) +
    labs(
      x = NULL,
      y = "Mean accuracy",
      title = paste0(plot_label, " observed accuracy by block/drop phase"),
      subtitle = paste0("Calibration mean uses the final ", CALIB_SUMMARY_LAST_N, " trials"),
      caption = "Black dashes: target accuracy"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "bottom"
    )

  p_rt <- ggplot(
    observed_summary,
    aes(
      x = phase_order,
      y = mean_rt,
      colour = calibration_target_group,
      group = 1
    )
  ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 3) +
    geom_text(
      aes(label = rt_label),
      vjust = -1.0,
      size = 3.4,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_x_continuous(breaks = seq_along(OBSERVED_LEVELS), labels = OBSERVED_LEVELS) +
    target_colour_scale(group_labels) +
    coord_cartesian(ylim = rt_ylim) +
    labs(
      x = "Block/drop phase",
      y = "Mean RT (s)",
      title = paste0(plot_label, " mean RT by block/drop phase"),
      subtitle = paste0("Calibration mean uses the final ", CALIB_SUMMARY_LAST_N, " trials")
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "bottom"
    )

  p_rating <- NULL
  combined <- (p_acc + theme(legend.position = "none")) /
    (p_rt + theme(legend.position = "none")) +
    plot_annotation(title = paste0(plot_label, " accuracy and RT by block/drop phase"))

  if (!is.null(rating_summary)) {
    rating_obs <- rating_references$observed %>% filter(participant_id == pid)
    rating_target <- rating_references$target %>% filter(participant_id == pid)
    rating_ylim <- get_axis_limits(
      c(
        rating_summary$rated_accuracy,
        rating_obs$observed_accuracy,
        rating_target$target_accuracy
      ),
      bounds = c(0, 1),
      pad = 0.18
    )

    p_rating <- ggplot(
      rating_summary,
      aes(
        x = rating_order,
        y = rated_accuracy,
        colour = calibration_target_group,
        shape = rating_measure
      )
    ) +
      geom_segment(
        data = rating_obs,
        aes(
          x = rating_order - 0.16,
          xend = rating_order + 0.16,
          y = observed_accuracy,
          yend = observed_accuracy
        ),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.9
      ) +
      geom_segment(
        data = rating_target,
        aes(
          x = rating_order - 0.30,
          xend = rating_order + 0.30,
          y = target_accuracy,
          yend = target_accuracy
        ),
        inherit.aes = FALSE,
        colour = "black",
        linetype = "dashed",
        linewidth = 0.8
      ) +
      geom_point(size = 3, na.rm = TRUE) +
      geom_text(
        aes(label = rating_accuracy_label, vjust = rating_label_vjust),
        size = 3.4,
        fontface = "bold",
        show.legend = FALSE,
        na.rm = TRUE
      ) +
      scale_x_continuous(breaks = seq_along(RATING_LEVELS), labels = RATING_LEVELS) +
      scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
      target_colour_scale(group_labels) +
      scale_shape_manual(values = RATING_MEASURE_SHAPES, name = "Self-rating") +
      coord_cartesian(ylim = rating_ylim) +
      labs(
        x = "Rating occasion",
        y = "Self-rated accuracy",
        title = paste0(plot_label, " self-rated accuracy by block"),
        subtitle = "Automation rating refers to the full 1,200-trial aided block",
        caption = paste0(
          "Black solid segments: observed accuracy; black dashes: calibration target ",
          "or mean assigned aid accuracy"
        )
      ) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "bottom",
        legend.box = "vertical"
      )

    combined <- (
      p_acc + labs(title = NULL, caption = NULL) + theme(legend.position = "none")
    ) / (
      p_rating + labs(title = NULL, caption = NULL) + theme(legend.position = "bottom")
    ) / (
      p_rt + labs(title = NULL) + theme(legend.position = "none")
    ) +
      plot_annotation(
        title = paste0(plot_label, " accuracy, self-ratings, and RT")
      )
  }

  list(accuracy = p_acc, rating = p_rating, rt = p_rt, combined = combined)
}

add_group_offsets <- function(data, position_col) {
  data %>%
    mutate(
      .group_index = match(as.character(calibration_target_group), TARGET_GROUP_LEVELS),
      .group_offset = (.group_index - 1.5) * (GROUP_DODGE_WIDTH / 2),
      .x_reference = .data[[position_col]] + .group_offset
    )
}

make_group_plots <- function(
  participant_observed,
  participant_ratings,
  rating_references,
  plot_label
) {
  group_labels <- make_group_labels(participant_observed)

  acc_summary <- summarise_repeated(
    participant_observed,
    "mean_accuracy",
    "phase_label",
    use_morey = TRUE
  ) %>%
    mutate(
      phase_label = factor(phase_label, levels = OBSERVED_LEVELS),
      phase_order = as.integer(phase_label),
      accuracy_label = sprintf("%.1f%%", mean * 100),
      accuracy_label_vjust = if_else(
        as.character(calibration_target_group) == "CAL65",
        1.6,
        -1.0
      )
    )

  rt_summary <- summarise_repeated(
    participant_observed,
    "mean_rt",
    "phase_label",
    use_morey = TRUE
  ) %>%
    mutate(
      phase_label = factor(phase_label, levels = OBSERVED_LEVELS),
      phase_order = as.integer(phase_label),
      rt_label = sprintf("%.3f s", mean)
    ) %>%
    group_by(phase_label) %>%
    mutate(
      rt_label_y = if_else(
        n() > 1 & mean == max(mean),
        mean + GROUP_RT_LABEL_OFFSET,
        mean - GROUP_RT_LABEL_OFFSET
      )
    ) %>%
    ungroup()

  own_ratings <- summarise_repeated(
    participant_ratings %>%
      filter(rating_measure == "Self-rated own accuracy"),
    "rated_accuracy",
    "rating_label",
    use_morey = TRUE
  ) %>%
    mutate(rating_measure = "Self-rated own accuracy")

  aid_ratings <- summarise_repeated(
    participant_ratings %>%
      filter(rating_measure == "Self-rated aid accuracy"),
    "rated_accuracy",
    "rating_label",
    use_morey = FALSE
  ) %>%
    mutate(rating_measure = "Self-rated aid accuracy")

  rating_summary <- bind_rows(own_ratings, aid_ratings) %>%
    mutate(
      rating_label = factor(rating_label, levels = RATING_LEVELS),
      rating_order = as.integer(rating_label),
      rating_measure = factor(rating_measure, levels = RATING_MEASURE_LEVELS),
      rating_accuracy_label = sprintf("%.1f%%", mean * 100),
      rating_label_vjust = case_when(
        as.character(calibration_target_group) == "CAL65" ~ 1.6,
        mean >= 0.94 ~ 1.6,
        TRUE ~ -1.0
      )
    )

  accuracy_refs <- make_accuracy_references(participant_observed)
  own_acc_refs <- accuracy_refs$own %>%
    group_by(calibration_target_group, phase_order) %>%
    summarise(target_accuracy = mean(target_accuracy), .groups = "drop") %>%
    add_group_offsets("phase_order")
  aid_acc_refs <- accuracy_refs$aid

  group_rating_observed <- rating_references$observed %>%
    group_by(calibration_target_group, rating_label, rating_order) %>%
    summarise(observed_accuracy = mean(observed_accuracy), .groups = "drop") %>%
    add_group_offsets("rating_order")
  group_rating_target <- rating_references$target %>%
    left_join(
      participant_observed %>%
        distinct(participant_id, calibration_target_group),
      by = "participant_id"
    ) %>%
    group_by(calibration_target_group, rating_label, rating_order) %>%
    summarise(target_accuracy = mean(target_accuracy), .groups = "drop") %>%
    add_group_offsets("rating_order")

  acc_ylim <- get_axis_limits(
    c(
      acc_summary$mean,
      own_acc_refs$target_accuracy,
      aid_acc_refs$target_accuracy
    ),
    c(acc_summary$se, rep(0, nrow(own_acc_refs) + nrow(aid_acc_refs))),
    bounds = c(0, 1),
    pad = 0.16
  )
  rt_ylim <- get_axis_limits(
    c(rt_summary$mean, rt_summary$rt_label_y),
    c(rt_summary$se, rep(0, nrow(rt_summary))),
    bounds = c(0, Inf),
    pad = 0.20
  )
  rating_ylim <- get_axis_limits(
    c(
      rating_summary$mean,
      group_rating_observed$observed_accuracy,
      group_rating_target$target_accuracy
    ),
    c(
      rating_summary$se,
      rep(0, nrow(group_rating_observed) + nrow(group_rating_target))
    ),
    bounds = c(0, 1),
    pad = 0.16
  )

  dodge <- position_dodge(width = GROUP_DODGE_WIDTH)

  p_acc <- ggplot(
    acc_summary,
    aes(
      x = phase_order,
      y = mean,
      colour = calibration_target_group,
      shape = calibration_target_group,
      group = calibration_target_group
    )
  ) +
    geom_segment(
      data = own_acc_refs,
      aes(
        x = .x_reference - 0.10,
        xend = .x_reference + 0.10,
        y = target_accuracy,
        yend = target_accuracy
      ),
      inherit.aes = FALSE,
      colour = "black",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_segment(
      data = aid_acc_refs,
      aes(
        x = phase_order - 0.30,
        xend = phase_order + 0.30,
        y = target_accuracy,
        yend = target_accuracy
      ),
      inherit.aes = FALSE,
      colour = "black",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_line(position = dodge, linewidth = 0.9) +
    geom_point(position = dodge, size = 3) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      position = dodge,
      width = 0.12,
      na.rm = TRUE
    ) +
    geom_text(
      aes(label = accuracy_label, vjust = accuracy_label_vjust),
      position = dodge,
      size = 3.3,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_x_continuous(breaks = seq_along(OBSERVED_LEVELS), labels = OBSERVED_LEVELS) +
    scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    target_colour_scale(group_labels) +
    target_shape_scale(group_labels) +
    coord_cartesian(ylim = acc_ylim) +
    labs(
      x = NULL,
      y = "Mean accuracy",
      title = paste0(plot_label, " group observed accuracy by block/drop phase"),
      subtitle = paste0(
        "Within-group Morey-Cousineau SEs across 6 phases; ",
        "CAL90 n = 1 (no SE); calibration = final ",
        CALIB_SUMMARY_LAST_N,
        " trials"
      ),
      caption = "Black dashes: target accuracy"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "bottom"
    )

  p_rt <- ggplot(
    rt_summary,
    aes(
      x = phase_order,
      y = mean,
      colour = calibration_target_group,
      shape = calibration_target_group,
      group = calibration_target_group
    )
  ) +
    geom_line(position = dodge, linewidth = 0.9) +
    geom_point(position = dodge, size = 3) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      position = dodge,
      width = 0.12,
      na.rm = TRUE
    ) +
    geom_text(
      aes(y = rt_label_y, label = rt_label),
      position = dodge,
      vjust = 0.5,
      size = 3.3,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_x_continuous(breaks = seq_along(OBSERVED_LEVELS), labels = OBSERVED_LEVELS) +
    target_colour_scale(group_labels) +
    target_shape_scale(group_labels) +
    coord_cartesian(ylim = rt_ylim) +
    labs(
      x = "Block/drop phase",
      y = "Mean RT (s)",
      title = paste0(plot_label, " group mean RT by block/drop phase"),
      subtitle = paste0(
        "Within-group Morey-Cousineau SEs across 6 phases; ",
        "CAL90 n = 1 (no SE); calibration = final ",
        CALIB_SUMMARY_LAST_N,
        " trials"
      )
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "bottom"
    )

  p_rating <- ggplot(
    rating_summary,
    aes(
      x = rating_order,
      y = mean,
      colour = calibration_target_group,
      shape = rating_measure
    )
  ) +
    geom_segment(
      data = group_rating_observed,
      aes(
        x = .x_reference - 0.08,
        xend = .x_reference + 0.08,
        y = observed_accuracy,
        yend = observed_accuracy
      ),
      inherit.aes = FALSE,
      colour = "black",
      linewidth = 0.9
    ) +
    geom_segment(
      data = group_rating_target,
      aes(
        x = .x_reference - 0.11,
        xend = .x_reference + 0.11,
        y = target_accuracy,
        yend = target_accuracy
      ),
      inherit.aes = FALSE,
      colour = "black",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_point(position = dodge, size = 3, na.rm = TRUE) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      position = dodge,
      width = 0.12,
      na.rm = TRUE
    ) +
    geom_text(
      aes(label = rating_accuracy_label, vjust = rating_label_vjust),
      position = dodge,
      size = 3.3,
      fontface = "bold",
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    scale_x_continuous(breaks = seq_along(RATING_LEVELS), labels = RATING_LEVELS) +
    scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    target_colour_scale(group_labels) +
    scale_shape_manual(values = RATING_MEASURE_SHAPES, name = "Self-rating") +
    coord_cartesian(ylim = rating_ylim) +
    labs(
      x = "Rating occasion",
      y = "Self-rated accuracy",
      title = paste0(plot_label, " group self-rated accuracy by block"),
      subtitle = paste0(
        "Own ratings: within-group Morey-Cousineau SEs across 3 occasions\n",
        "aid rating: between-participant SE; CAL90 n = 1 (no SE)"
      ),
      caption = paste0(
        "Automation rating refers to the full 1,200-trial aided block. ",
        "Black solid segments: observed accuracy; black dashes: target accuracy."
      )
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "bottom",
      legend.box = "vertical"
    )

  combined <- (
    p_acc + labs(title = NULL, caption = NULL) + theme(legend.position = "none")
  ) / (
    p_rating + labs(title = NULL, caption = NULL) + theme(legend.position = "bottom")
  ) / (
    p_rt + labs(title = NULL) + theme(legend.position = "none")
  ) +
    plot_annotation(
      title = paste0(
        "Dynamic Reliability Study: ",
        plot_label,
        " group accuracy, self-ratings, and RT"
      )
    )

  list(
    accuracy = p_acc,
    rating = p_rating,
    rt = p_rt,
    combined = combined,
    acc_summary = acc_summary,
    rating_summary = rating_summary,
    rt_summary = rt_summary
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

input_file <- resolve_input_file(INPUT_PATH)
message("Reading trials: ", input_file)
trial_dat_raw <- read_trial_csv(input_file)
trial_dat <- prepare_trial_data(trial_dat_raw)
validate_trial_coverage(trial_dat)
observed_dat <- restrict_calibration_trials(trial_dat)
participant_observed <- make_participant_observed_summary(observed_dat)

slider_required <- PLOT_MODE %in% c("cohort", "group")
slider_explicit <- !is.na(SLIDER_INPUT_PATH) && nzchar(SLIDER_INPUT_PATH)
slider_file <- if (slider_required || slider_explicit) {
  resolve_slider_file(input_file, SLIDER_INPUT_PATH, required = slider_required)
} else {
  NA_character_
}

participant_ratings <- NULL
rating_references <- NULL
if (!is.na(slider_file)) {
  message("Reading ratings: ", slider_file)
  participant_ratings <- prepare_slider_data(read_slider_csv(slider_file))
  trial_ids <- sort(unique(trial_dat$participant_id))
  rating_ids <- sort(unique(participant_ratings$participant_id))
  if (!identical(trial_ids, rating_ids)) {
    stop("Trial and slider participant IDs do not match.", call. = FALSE)
  }
  rating_references <- make_rating_references(trial_dat, observed_dat)
}

participant_ids <- sort(unique(trial_dat$participant_id))
if (PLOT_MODE == "single" && length(participant_ids) != 1) {
  stop("single mode requires exactly one participant.", call. = FALSE)
}

written <- character(0)

if (PLOT_MODE %in% c("single", "cohort")) {
  for (pid in participant_ids) {
    pid_prefix <- sprintf("p%03d", pid)
    participant_dir <- if (PLOT_MODE == "single") {
      OUTPUT_DIR
    } else {
      file.path(OUTPUT_DIR, "individual", pid_prefix)
    }
    participant_label <- paste("Participant", sprintf("%03d", pid))
    calibration_pid <- trial_dat %>%
      filter(participant_id == pid, block == "CALIBRATION")
    observed_pid <- participant_observed %>% filter(participant_id == pid)
    ratings_pid <- if (is.null(participant_ratings)) {
      NULL
    } else {
      participant_ratings %>% filter(participant_id == pid)
    }

    plots <- make_individual_plots(
      observed_pid,
      ratings_pid,
      rating_references,
      participant_label
    )
    calibration_plot <- make_calibration_dynamics_plot(
      calibration_pid,
      participant_label
    )

    written <- c(
      written,
      save_plot_pair(
        calibration_plot,
        participant_dir,
        paste0(pid_prefix, "_calibration_dynamics"),
        width = 9,
        height = 6
      ),
      save_plot_pair(
        plots$accuracy,
        participant_dir,
        paste0(pid_prefix, "_block_accuracy_means"),
        width = 8.5,
        height = 5
      ),
      save_plot_pair(
        plots$rt,
        participant_dir,
        paste0(pid_prefix, "_block_rt_means"),
        width = 8.5,
        height = 5
      ),
      save_plot_pair(
        plots$combined,
        participant_dir,
        paste0(pid_prefix, "_block_accuracy_rt_means"),
        width = 8.5,
        height = if (is.null(plots$rating)) 8 else 11
      )
    )

    if (!is.null(plots$rating)) {
      written <- c(
        written,
        save_plot_pair(
          plots$rating,
          participant_dir,
          paste0(pid_prefix, "_block_self_rated_accuracy_means"),
          width = 8.5,
          height = 5
        )
      )
    }

    message(participant_label, " observed summary:")
    print(observed_pid %>% select(phase_label, mean_accuracy, mean_rt, n_accuracy, n_rt))
    if (!is.null(ratings_pid)) {
      message(participant_label, " rating summary:")
      print(ratings_pid %>% select(rating_measure, rating_label, rated_accuracy))
    }
  }
}

if (PLOT_MODE %in% c("cohort", "group")) {
  group_dir <- file.path(OUTPUT_DIR, "group")
  group_prefix <- paste0(OUTPUT_PREFIX, "_group")
  group_plots <- make_group_plots(
    participant_observed,
    participant_ratings,
    rating_references,
    PLOT_LABEL
  )

  written <- c(
    written,
    save_plot_pair(
      group_plots$accuracy,
      group_dir,
      paste0(group_prefix, "_block_accuracy_means"),
      width = 8.5,
      height = 6
    ),
    save_plot_pair(
      group_plots$rating,
      group_dir,
      paste0(group_prefix, "_block_self_rated_accuracy_means"),
      width = 8.5,
      height = 6
    ),
    save_plot_pair(
      group_plots$rt,
      group_dir,
      paste0(group_prefix, "_block_rt_means"),
      width = 8.5,
      height = 5
    ),
    save_plot_pair(
      group_plots$combined,
      group_dir,
      paste0(group_prefix, "_block_accuracy_rt_means"),
      width = 8.5,
      height = 11
    )
  )

  message("Group observed accuracy summary:")
  print(group_plots$acc_summary)
  message("Group rating summary:")
  print(group_plots$rating_summary)
  message("Group RT summary:")
  print(group_plots$rt_summary)
}

cat("Wrote:\n")
cat(paste0(" - ", written, collapse = "\n"), "\n", sep = "")
