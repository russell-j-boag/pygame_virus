# Clear workspace
rm(list = ls())

# Load libraries
library("dplyr")
library("readr")
library("stringr")
library("tidyverse")
library("zoo")
library("patchwork")

# Load master data
dat <- read_csv("data/data_virus_all.csv", show_col_types = FALSE)
head(dat)
str(dat)

# Calibration settings
WINDOW <- 25
TARGET_ACC <- 0.80
BURN_IN_TRIALS <- 50
CALIB_SUMMARY_LAST_N <- 150
DELTA_SD <- 0.01

# Make plots folder if needed
if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)

# Participant IDs
participant_ids <- sort(unique(dat$participant_id))

for (pid in participant_ids) {
  
  message("Working on participant: ", pid)
  
  dat_pid <- dat %>%
    filter(participant_id == pid)
  
  # -----------------------------
  # Calibration block
  # -----------------------------
  
  dat_calib <- dat_pid %>%
    filter(block == "CALIBRATION") %>%
    arrange(trial) %>%
    mutate(
      correct_num = if_else(is.na(correct), 0, as.numeric(correct)),
      acc_running = cumsum(correct_num) / row_number(),
      acc_slide   = rollapply(
        correct_num,
        width   = WINDOW,
        FUN     = mean,
        align   = "right",
        fill    = NA,
        partial = TRUE
      )
    )
  
  dat_calib_post <- dat_calib %>%
    filter(trial > BURN_IN_TRIALS)
  
  n_last <- min(CALIB_SUMMARY_LAST_N, nrow(dat_calib_post))
  
  dat_calib_lastN <- dat_calib_post %>%
    slice_tail(n = n_last)
  
  acc_mean_postburn <- mean(dat_calib_post$correct_num, na.rm = TRUE)
  acc_mean_lastN <- mean(dat_calib_lastN$correct_num, na.rm = TRUE)
  
  delta_mean_calib <- mean(dat_calib_lastN$delta_stair_realised, na.rm = TRUE)
  delta_sd_calib   <- sd(dat_calib_lastN$delta_stair_realised, na.rm = TRUE)
  
  lastN_start_trial <- min(dat_calib_lastN$trial, na.rm = TRUE)
  
  calib_ymax <- ceiling(max(dat_calib$delta_stair_mean + DELTA_SD, na.rm = TRUE) * 100) / 100
  
  p_delta <- ggplot(dat_calib, aes(x = trial)) +
    geom_ribbon(
      data = dat_calib %>% filter(trial <= BURN_IN_TRIALS),
      aes(ymin = 0, ymax = calib_ymax),
      fill  = "red",
      alpha = 0.15
    ) +
    geom_ribbon(
      data = dat_calib %>% filter(trial >= lastN_start_trial),
      aes(
        ymin = delta_mean_calib - delta_sd_calib,
        ymax = delta_mean_calib + delta_sd_calib
      ),
      fill  = "orange",
      alpha = 0.4
    ) +
    geom_ribbon(
      aes(
        ymin = delta_stair_mean - DELTA_SD,
        ymax = delta_stair_mean + DELTA_SD
      ),
      fill  = "purple",
      alpha = 0.2
    ) +
    geom_point(
      aes(y = delta_stair_realised),
      colour = "orange", size = 1, alpha = 1
    ) +
    annotate(
      "segment",
      x        = lastN_start_trial,
      xend     = max(dat_calib$trial, na.rm = TRUE),
      y        = delta_mean_calib,
      yend     = delta_mean_calib,
      linetype = "dashed",
      colour   = "orange",
      alpha    = 1
    ) +
    geom_line(
      aes(y = delta_stair_mean),
      colour = "purple", linewidth = 0.75
    ) +
    scale_y_continuous(
      breaks = seq(0, calib_ymax, by = 0.02),
      limits = c(0, calib_ymax)
    ) +
    labs(x = NULL, y = "Delta") +
    theme_classic() +
    ggtitle(
      paste0("Calibration block (Participant ", pid, ")"),
      subtitle = paste0(
        "Staircase-adjusted difficulty (dot prop. difference from 0.50)\n",
        "Summary uses last ", nrow(dat_calib_lastN), " post-burn-in calibration trials"
      )
    )
  
  p_acc <- ggplot(dat_calib, aes(x = trial)) +
    geom_ribbon(
      data = dat_calib %>% filter(trial <= BURN_IN_TRIALS),
      aes(ymin = 0, ymax = 1),
      fill  = "red",
      alpha = 0.15
    ) +
    geom_point(
      aes(y = correct_num),
      shape = 4,
      size  = 1,
      stroke = 0.8,
      alpha = 0.5,
      colour = "black"
    ) +
    geom_line(
      aes(y = acc_running),
      linewidth = 0.75, colour = "orange", alpha = 1
    ) +
    geom_hline(yintercept = TARGET_ACC, linetype = 2, colour = "purple") +
    geom_hline(
      yintercept = acc_mean_lastN,
      linetype   = 1,
      linewidth  = 0.5,
      colour     = "orange",
      alpha      = 0.5
    ) +
    annotate(
      "text",
      x      = Inf,
      y      = -Inf,
      hjust  = 1.05,
      vjust  = -5.0,
      size   = 3.5,
      colour = "black",
      label  = sprintf("Target acc = %.2f", TARGET_ACC)
    ) +
    annotate(
      "text",
      x      = Inf,
      y      = -Inf,
      hjust  = 1.05,
      vjust  = -3.0,
      size   = 3.5,
      colour = "black",
      label  = sprintf("Observed acc = %.2f", acc_mean_postburn)
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "Trial", y = "Running accuracy") +
    theme_classic() +
    ggtitle("", subtitle = "Observed accuracy")
  
  p_calib <- p_delta / p_acc +
    plot_layout(heights = c(1, 1))
  
  # -----------------------------
  # Manual block
  # -----------------------------
  
  dat_manual <- dat_pid %>%
    filter(block == "MANUAL") %>%
    arrange(trial) %>%
    mutate(
      correct_num = if_else(is.na(correct), 0, as.numeric(correct)),
      acc_running = cumsum(correct_num) / row_number(),
      acc_slide   = rollapply(
        correct_num,
        width   = WINDOW,
        FUN     = mean,
        align   = "right",
        fill    = NA,
        partial = TRUE
      )
    )
  
  acc_mean_manual <- mean(dat_manual$correct_num, na.rm = TRUE)
  delta_mean_manual <- dat_manual$delta_fixed_mean[1]
  delta_sd_manual   <- dat_manual$delta_fixed_sd[1]
  
  p_delta <- ggplot(dat_manual, aes(x = trial)) +
    geom_ribbon(
      aes(
        ymin = delta_mean_manual - delta_sd_manual,
        ymax = delta_mean_manual + delta_sd_manual
      ),
      fill  = "orange",
      alpha = 0.4
    ) +
    geom_point(
      aes(y = abs(0.50 - vblack_prop)),
      colour = "orange", size = 1, alpha = 1
    ) +
    geom_hline(
      yintercept = delta_mean_manual,
      linetype = "dashed",
      colour = "orange",
      alpha = 1
    ) +
    scale_y_continuous(
      breaks = seq(0, calib_ymax, by = 0.02),
      limits = c(0, calib_ymax)
    ) +
    labs(x = NULL, y = "Delta") +
    theme_classic() +
    ggtitle(
      paste0("Manual block (Participant ", pid, ")"),
      subtitle = paste0(
        "Difficulty sampled from calibration mean and sd\n",
        "Summary uses last ", nrow(dat_calib_lastN), " post-burn-in calibration trials"
      )
    )
  
  p_acc <- ggplot(dat_manual, aes(x = trial)) +
    geom_point(
      aes(y = correct_num),
      shape = 4,
      size  = 1,
      stroke = 0.8,
      alpha = 0.5,
      colour = "black"
    ) +
    geom_line(
      aes(y = acc_running),
      linewidth = 0.75, colour = "orange", alpha = 1
    ) +
    geom_hline(yintercept = TARGET_ACC, linetype = 2, colour = "purple") +
    geom_hline(
      yintercept = acc_mean_manual,
      linetype   = 1,
      linewidth  = 0.5,
      colour     = "orange",
      alpha      = 0.5
    ) +
    annotate(
      "text",
      x      = Inf,
      y      = -Inf,
      hjust  = 1.05,
      vjust  = -5.0,
      size   = 3.5,
      colour = "black",
      label  = sprintf("Target acc = %.2f", TARGET_ACC)
    ) +
    annotate(
      "text",
      x      = Inf,
      y      = -Inf,
      hjust  = 1.05,
      vjust  = -3.0,
      size   = 3.5,
      colour = "black",
      label  = sprintf("Observed acc = %.2f", acc_mean_manual)
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "Trial", y = "Running accuracy") +
    theme_classic() +
    ggtitle("", subtitle = "Observed accuracy")
  
  p_manual <- p_delta / p_acc +
    plot_layout(heights = c(1, 1))
  
  p_combo_manual <- p_calib | p_manual
  
  ggsave(
    filename = paste0("plots/combined_manual_p", pid, ".pdf"),
    plot     = p_combo_manual,
    device   = cairo_pdf,
    width    = 16,
    height   = 9,
    units    = "in"
  )
  
  # -----------------------------
  # Automation 1 block
  # -----------------------------
  
  dat_auto1 <- dat_pid %>%
    filter(block == "AUTOMATION1") %>%
    arrange(trial) %>%
    mutate(
      correct_num = if_else(is.na(correct), 0, as.numeric(correct)),
      acc_running = cumsum(correct_num) / row_number(),
      acc_slide   = rollapply(
        correct_num,
        width   = WINDOW,
        FUN     = mean,
        align   = "right",
        fill    = NA,
        partial = TRUE
      )
    )
  
  acc_mean_auto1 <- mean(dat_auto1$correct_num, na.rm = TRUE)
  delta_mean_auto1 <- dat_auto1$delta_fixed_mean[1]
  delta_sd_auto1   <- dat_auto1$delta_fixed_sd[1]
  aid_acc_mean1 <- mean(dat_auto1$aid_correct, na.rm = TRUE)
  
  p_delta <- ggplot(dat_auto1, aes(x = trial)) +
    geom_ribbon(
      aes(
        ymin = delta_mean_auto1 - delta_sd_auto1,
        ymax = delta_mean_auto1 + delta_sd_auto1
      ),
      fill  = "orange",
      alpha = 0.4
    ) +
    geom_point(
      aes(y = abs(0.50 - vblack_prop)),
      colour = "orange", size = 1, alpha = 1
    ) +
    geom_hline(
      yintercept = delta_mean_auto1,
      linetype = "dashed",
      colour = "orange",
      alpha = 1
    ) +
    scale_y_continuous(
      breaks = seq(0, calib_ymax, by = 0.02),
      limits = c(0, calib_ymax)
    ) +
    labs(x = NULL, y = "Delta") +
    theme_classic() +
    ggtitle(
      paste0("Automation block (high reliability; Participant ", pid, ")"),
      subtitle = paste0(
        "Difficulty sampled from calibration mean and sd\n",
        "Summary uses last ", nrow(dat_calib_lastN), " post-burn-in calibration trials"
      )
    )
  
  p_acc <- ggplot(dat_auto1, aes(x = trial)) +
    geom_point(
      aes(y = as.numeric(aid_correct)),
      shape  = 1,
      size   = 1.4,
      stroke = 0.8,
      alpha  = 0.6,
      colour = "forestgreen"
    ) +
    geom_point(
      aes(y = correct_num),
      shape  = 4,
      size   = 1,
      stroke = 0.8,
      alpha  = 0.5,
      colour = "black"
    ) +
    geom_line(
      aes(y = acc_running),
      linewidth = 0.75,
      colour    = "orange"
    ) +
    geom_hline(
      yintercept = TARGET_ACC,
      linetype   = 2,
      colour     = "purple"
    ) +
    geom_hline(
      yintercept = acc_mean_auto1,
      linetype   = 1,
      linewidth  = 0.5,
      colour     = "orange",
      alpha      = 0.5
    ) +
    annotate(
      "text",
      x      = Inf,
      y      = -Inf,
      hjust  = 1.05,
      vjust  = -5.0,
      size   = 3.5,
      colour = "black",
      label  = sprintf("Aid acc = %.2f", aid_acc_mean1)
    ) +
    annotate(
      "text",
      x      = Inf,
      y      = -Inf,
      hjust  = 1.05,
      vjust  = -3.0,
      size   = 3.5,
      colour = "black",
      label  = sprintf("Observed acc = %.2f", acc_mean_auto1)
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "Trial", y = "Running accuracy") +
    theme_classic() +
    ggtitle("", subtitle = "Observed accuracy")
  
  p_auto1 <- p_delta / p_acc +
    plot_layout(heights = c(1, 1))
  
  # -----------------------------
  # Automation 2 block
  # -----------------------------
  
  dat_auto2 <- dat_pid %>%
    filter(block == "AUTOMATION2") %>%
    arrange(trial) %>%
    mutate(
      correct_num = if_else(is.na(correct), 0, as.numeric(correct)),
      acc_running = cumsum(correct_num) / row_number(),
      acc_slide   = rollapply(
        correct_num,
        width   = WINDOW,
        FUN     = mean,
        align   = "right",
        fill    = NA,
        partial = TRUE
      )
    )
  
  acc_mean_auto2 <- mean(dat_auto2$correct_num, na.rm = TRUE)
  delta_mean_auto2 <- dat_auto2$delta_fixed_mean[1]
  delta_sd_auto2   <- dat_auto2$delta_fixed_sd[1]
  aid_acc_mean2 <- mean(dat_auto2$aid_correct, na.rm = TRUE)
  
  p_delta <- ggplot(dat_auto2, aes(x = trial)) +
    geom_ribbon(
      aes(
        ymin = delta_mean_auto2 - delta_sd_auto2,
        ymax = delta_mean_auto2 + delta_sd_auto2
      ),
      fill  = "orange",
      alpha = 0.4
    ) +
    geom_point(
      aes(y = abs(0.50 - vblack_prop)),
      colour = "orange", size = 1, alpha = 1
    ) +
    geom_hline(
      yintercept = delta_mean_auto2,
      linetype = "dashed",
      colour = "orange",
      alpha = 1
    ) +
    scale_y_continuous(
      breaks = seq(0, calib_ymax, by = 0.02),
      limits = c(0, calib_ymax)
    ) +
    labs(x = NULL, y = "Delta") +
    theme_classic() +
    ggtitle(
      paste0("Automation block (low reliability; Participant ", pid, ")"),
      subtitle = paste0(
        "Difficulty sampled from calibration mean and sd\n",
        "Summary uses last ", nrow(dat_calib_lastN), " post-burn-in calibration trials"
      )
    )
  
  p_acc <- ggplot(dat_auto2, aes(x = trial)) +
    geom_point(
      aes(y = as.numeric(aid_correct)),
      shape  = 1,
      size   = 1.4,
      stroke = 0.8,
      alpha  = 0.6,
      colour = "forestgreen"
    ) +
    geom_point(
      aes(y = correct_num),
      shape  = 4,
      size   = 1,
      stroke = 0.8,
      alpha  = 0.5,
      colour = "black"
    ) +
    geom_line(
      aes(y = acc_running),
      linewidth = 0.75,
      colour    = "orange"
    ) +
    geom_hline(
      yintercept = TARGET_ACC,
      linetype   = 2,
      colour     = "purple"
    ) +
    geom_hline(
      yintercept = acc_mean_auto2,
      linetype   = 1,
      linewidth  = 0.5,
      colour     = "orange",
      alpha      = 0.5
    ) +
    annotate(
      "text",
      x      = Inf,
      y      = -Inf,
      hjust  = 1.05,
      vjust  = -5.0,
      size   = 3.5,
      colour = "black",
      label  = sprintf("Aid acc = %.2f", aid_acc_mean2)
    ) +
    annotate(
      "text",
      x      = Inf,
      y      = -Inf,
      hjust  = 1.05,
      vjust  = -3.0,
      size   = 3.5,
      colour = "black",
      label  = sprintf("Observed acc = %.2f", acc_mean_auto2)
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "Trial", y = "Running accuracy") +
    theme_classic() +
    ggtitle("", subtitle = "Observed accuracy")
  
  p_auto2 <- p_delta / p_acc +
    plot_layout(heights = c(1, 1))
  
  p_combo_auto <- p_auto1 | p_auto2
  
  ggsave(
    filename = paste0("plots/combined_auto_p", pid, ".pdf"),
    plot     = p_combo_auto,
    device   = cairo_pdf,
    width    = 16,
    height   = 9,
    units    = "in"
  )
}
