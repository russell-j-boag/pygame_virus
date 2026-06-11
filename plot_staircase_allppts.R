# Clear workspace
rm(list = ls())

library("dplyr")
library("readr")
library("tidyr")
library("ggplot2")
library("patchwork")
library("zoo")

dat <- read_csv("data/data_virus_all.csv", show_col_types = FALSE)

WINDOW <- 25
TARGET_ACC <- 0.85
BURN_IN_TRIALS <- 50
CALIB_SUMMARY_LAST_N <- 150

safe_max <- function(x, fallback = 0.25) {
  value <- suppressWarnings(max(x, na.rm = TRUE))
  if (is.finite(value)) value else fallback
}

condition_label <- function(aid_condition) {
  out <- dplyr::case_when(
    !is.na(aid_condition) & aid_condition == "simultaneous" ~ "Aid + stimulus",
    !is.na(aid_condition) & aid_condition == "aid_first" ~ "Aid first",
    !is.na(aid_condition) & aid_condition == "stimulus_first_change" ~ "Stimulus first, change allowed",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Aid + stimulus", "Aid first", "Stimulus first, change allowed"))
}

add_running_accuracy <- function(data) {
  data %>%
    arrange(trial) %>%
    mutate(
      correct_num = if_else(is.na(correct), 0, as.numeric(correct)),
      aid_correct_num = case_when(
        aid_correct %in% c(TRUE, 1, "1", "TRUE", "True", "true") ~ 1,
        aid_correct %in% c(FALSE, 0, "0", "FALSE", "False", "false") ~ 0,
        TRUE ~ NA_real_
      ),
      acc_running = cumsum(correct_num) / row_number(),
      acc_slide = rollapply(
        correct_num,
        width = WINDOW,
        FUN = mean,
        align = "right",
        fill = NA,
        partial = TRUE
      )
    )
}

make_calibration_plot <- function(dat_calib, pid) {
  dat_calib <- add_running_accuracy(dat_calib)
  dat_calib_post <- dat_calib %>% filter(trial > BURN_IN_TRIALS)
  dat_calib_lastN <- dat_calib_post %>%
    slice_tail(n = min(CALIB_SUMMARY_LAST_N, nrow(dat_calib_post)))

  delta_mean <- mean(dat_calib_lastN$delta_stair_realised, na.rm = TRUE)
  delta_sd <- sd(dat_calib_lastN$delta_stair_realised, na.rm = TRUE)
  lastN_start_trial <- min(dat_calib_lastN$trial, na.rm = TRUE)
  y_max <- ceiling(safe_max(dat_calib$delta_stair_realised) * 100) / 100

  p_delta <- ggplot(dat_calib, aes(x = trial)) +
    geom_ribbon(
      data = dat_calib %>% filter(trial >= lastN_start_trial),
      aes(ymin = delta_mean - delta_sd, ymax = delta_mean + delta_sd),
      fill = "orange",
      alpha = 0.35
    ) +
    geom_point(aes(y = delta_stair_realised), colour = "orange", size = 1) +
    geom_line(aes(y = delta_stair_mean), colour = "purple", linewidth = 0.75) +
    geom_hline(yintercept = delta_mean, linetype = "dashed", colour = "orange") +
    scale_y_continuous(limits = c(0, y_max)) +
    labs(x = NULL, y = "Delta", title = paste0("Calibration; Participant ", pid)) +
    theme_classic()

  p_acc <- ggplot(dat_calib, aes(x = trial)) +
    geom_point(aes(y = correct_num), shape = 4, size = 1, alpha = 0.5) +
    geom_line(aes(y = acc_running), linewidth = 0.75, colour = "orange") +
    geom_hline(yintercept = TARGET_ACC, linetype = 2, colour = "purple") +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "Trial", y = "Running accuracy") +
    theme_classic()

  p_delta / p_acc
}

make_automation_plot <- function(dat_auto, pid) {
  dat_auto <- add_running_accuracy(dat_auto)
  label <- unique(dat_auto$aid_condition_label)[1]
  aid_acc <- mean(dat_auto$aid_correct_num, na.rm = TRUE)
  observed_acc <- mean(dat_auto$correct_num, na.rm = TRUE)
  delta_mean <- dat_auto$delta_fixed_mean[1]
  delta_sd <- dat_auto$delta_fixed_sd[1]
  y_max <- ceiling(safe_max(abs(0.5 - dat_auto$vblack_prop)) * 100) / 100

  p_delta <- ggplot(dat_auto, aes(x = trial)) +
    geom_ribbon(
      aes(ymin = delta_mean - delta_sd, ymax = delta_mean + delta_sd),
      fill = "orange",
      alpha = 0.35
    ) +
    geom_point(aes(y = abs(0.5 - vblack_prop)), colour = "orange", size = 1) +
    geom_hline(yintercept = delta_mean, linetype = "dashed", colour = "orange") +
    scale_y_continuous(limits = c(0, y_max)) +
    labs(
      x = NULL,
      y = "Delta",
      title = paste0(label, " automation block; Participant ", pid)
    ) +
    theme_classic()

  p_acc <- ggplot(dat_auto, aes(x = trial)) +
    geom_point(aes(y = aid_correct_num), shape = 1, size = 1.4, alpha = 0.6, colour = "forestgreen") +
    geom_point(aes(y = correct_num), shape = 4, size = 1, alpha = 0.5) +
    geom_line(aes(y = acc_running), linewidth = 0.75, colour = "orange") +
    geom_hline(yintercept = TARGET_ACC, linetype = 2, colour = "purple") +
    geom_hline(yintercept = observed_acc, colour = "orange", alpha = 0.6) +
    annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -5,
             label = sprintf("Aid acc = %.2f", aid_acc), size = 3.5) +
    annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -3,
             label = sprintf("Observed acc = %.2f", observed_acc), size = 3.5) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "Trial", y = "Running accuracy") +
    theme_classic()

  p_delta / p_acc
}

if (!dir.exists("plots")) {
  dir.create("plots", recursive = TRUE)
}

participant_ids <- sort(unique(dat$participant_id))

for (pid in participant_ids) {
  dat_pid <- dat %>% filter(participant_id == pid)
  dat_calib <- dat_pid %>% filter(block == "CALIBRATION")

  if (!nrow(dat_calib)) {
    warning("Skipping participant ", pid, ": no calibration block found.")
    next
  }

  p_calib <- make_calibration_plot(dat_calib, pid)

  dat_auto <- dat_pid %>%
    mutate(aid_condition_label = condition_label(aid_condition)) %>%
    filter(block == "AUTOMATION", !is.na(aid_condition_label))

  auto_plots <- dat_auto %>%
    group_split(aid_condition_label, .keep = TRUE) %>%
    lapply(make_automation_plot, pid = pid)

  p_combo <- wrap_plots(c(list(p_calib), auto_plots), ncol = 1)

  ggsave(
    filename = paste0("plots/combined_aid_condition_p", pid, ".pdf"),
    plot = p_combo,
    device = cairo_pdf,
    width = 10,
    height = 14,
    units = "in"
  )
}
