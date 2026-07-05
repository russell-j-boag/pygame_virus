# Plot calibration staircase and post-calibration performance dynamics.
#
# Usage:
#   Rscript plot_staircase.R [input_dir_or_file] [output_dir] [output_prefix]
#
# Defaults are set for the Harry pilot data. Deadline labels are read from the
# CSV, so older 2/4 s pilot runs and current 1.5/3 s runs are both labelled as
# collected.

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

WINDOW <- 25
TARGET_ACC <- 0.80
BURN_IN_TRIALS <- 50
CALIB_SUMMARY_LAST_N <- 150
DELTA_SD <- 0.01
POST_CONDITION_CODES <- c("M_HP", "A_HP", "A_LP", "M_LP")

required_cols <- c(
  "participant_id", "run_timestamp", "block", "block_idx",
  "condition_deadline_code", "time_pressure_condition", "trial_deadline_s",
  "trial", "difficulty_mode", "delta_fixed_mean", "delta_fixed_sd",
  "delta_stair_realised", "delta_stair_mean", "vblack_prop", "aid_correct",
  "aid_accuracy_setting", "correct", "rt_s"
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
    "M_HP" = "Manual HP",
    "A_HP" = "Automation HP",
    "A_LP" = "Automation LP",
    "M_LP" = "Manual LP",
    .default = as.character(code)
  )
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

make_condition_metadata <- function(dat) {
  dat %>%
    filter(condition_deadline_code %in% POST_CONDITION_CODES) %>%
    group_by(condition_deadline_code) %>%
    summarise(
      block = first_non_missing(block),
      time_pressure_condition = first_non_missing(time_pressure_condition),
      trial_deadline_s = first_non_missing(trial_deadline_s),
      aid_accuracy_setting = first_non_missing(aid_accuracy_setting),
      .groups = "drop"
    ) %>%
    mutate(
      condition_deadline_code = factor(
        condition_deadline_code,
        levels = POST_CONDITION_CODES
      ),
      condition_label = paste0(
        condition_display_name(condition_deadline_code),
        "\n",
        format(trial_deadline_s, trim = TRUE, scientific = FALSE),
        " s"
      )
    ) %>%
    arrange(condition_deadline_code)
}

summarise_morey_condition_mean <- function(data, value_col, mean_name, se_name) {
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

  morey_cf <- sqrt(length(POST_CONDITION_CODES) / (length(POST_CONDITION_CODES) - 1))
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

make_condition_summary_plots <- function(dat) {
  condition_meta <- make_condition_metadata(dat)
  missing_conditions <- setdiff(
    POST_CONDITION_CODES,
    as.character(condition_meta$condition_deadline_code)
  )

  if (length(missing_conditions) > 0) {
    stop(
      "Missing post-calibration conditions: ",
      paste(missing_conditions, collapse = ", "),
      call. = FALSE
    )
  }

  post_dat <- dat %>%
    filter(condition_deadline_code %in% POST_CONDITION_CODES) %>%
    mutate(
      condition_deadline_code = factor(
        condition_deadline_code,
        levels = POST_CONDITION_CODES
      ),
      correct_num = if_else(is.na(correct), 0, as.numeric(correct))
    )

  subj_acc <- post_dat %>%
    group_by(participant_id, condition_deadline_code) %>%
    summarise(
      acc = mean(correct_num, na.rm = TRUE),
      .groups = "drop"
    )

  subj_rt <- post_dat %>%
    filter(!is.na(rt_s)) %>%
    group_by(participant_id, condition_deadline_code) %>%
    summarise(
      mean_rt = mean(rt_s, na.rm = TRUE),
      .groups = "drop"
    )

  acc_summary <- summarise_morey_condition_mean(
    data = subj_acc,
    value_col = "acc",
    mean_name = "mean_acc",
    se_name = "se_acc"
  ) %>%
    left_join(condition_meta, by = "condition_deadline_code") %>%
    mutate(condition_label = factor(condition_label, levels = condition_meta$condition_label))

  rt_summary <- summarise_morey_condition_mean(
    data = subj_rt,
    value_col = "mean_rt",
    mean_name = "mean_rt",
    se_name = "se_rt"
  ) %>%
    left_join(condition_meta, by = "condition_deadline_code") %>%
    mutate(condition_label = factor(condition_label, levels = condition_meta$condition_label))

  aid_reference <- condition_meta %>%
    filter(block == "AUTOMATION", !is.na(aid_accuracy_setting)) %>%
    mutate(x_pos = as.numeric(condition_deadline_code))

  acc_ylim <- get_axis_limits(
    c(acc_summary$mean_acc, TARGET_ACC, aid_reference$aid_accuracy_setting),
    c(acc_summary$se_acc, 0, rep(0, nrow(aid_reference))),
    pad_prop = 0.14,
    bounds = c(0, 1)
  )

  rt_ylim <- get_axis_limits(rt_summary$mean_rt, rt_summary$se_rt, pad_prop = 0.18)

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
      title = "Harry pilot accuracy by manual/automation condition"
    ) +
    coord_cartesian(ylim = acc_ylim, clip = "off") +
    theme_classic() +
    theme(plot.margin = margin(5.5, 12, 5.5, 5.5))

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
      title = "Harry pilot mean RT by manual/automation condition"
    ) +
    coord_cartesian(ylim = rt_ylim, clip = "off") +
    theme_classic() +
    theme(plot.margin = margin(5.5, 12, 5.5, 5.5))

  list(
    acc_plot = p_acc,
    rt_plot = p_rt,
    acc_summary = acc_summary,
    rt_summary = rt_summary
  )
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

input_file <- resolve_input_file(INPUT_PATH)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

message("Reading: ", input_file)
dat <- read_task_csv(input_file)

message("Detected blocks:")
print(dat %>%
  distinct(block_idx, block, condition_deadline_code, time_pressure_condition, trial_deadline_s) %>%
  arrange(block_idx))

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

condition_summary <- make_condition_summary_plots(dat)

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
  plot_annotation(title = "Harry pilot condition means")

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

message("Wrote plots:")
writeLines(written)
