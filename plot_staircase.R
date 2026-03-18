rm(list = ls())

library("dplyr")
library("readr")
library("stringr")
library("tidyverse")
library("zoo")
library("patchwork")

files <- list.files(
  "output",
  pattern = "^results_.*_b00_ALL\\.csv$",
  full.names = TRUE
)

latest_file <- files[which.max(file.info(files)$mtime)]
print(latest_file)

dat <- read_csv(latest_file)
head(dat)
str(dat)

WINDOW <- 25
burn_in_trials <- 50
target_acc <- 0.80


# Calibration block -------------------------------------------------------

dat_calib <- dat %>%
  filter(block == "CALIBRATION") %>%
  arrange(trial) %>%
  mutate(
    correct_num = as.numeric(correct),
    acc_running = cummean(correct),
    acc_slide   = rollapply(
      correct_num,
      width   = WINDOW,
      FUN     = mean,
      align   = "right",
      fill    = NA,
      partial = TRUE
    )
  )

# burn-in–excluded data for summaries
dat_calib_post <- dat_calib %>%
  filter(trial > burn_in_trials)

t(dat_calib_post %>%
       summarise(
         mean_delta_stair_mean  = mean(delta_stair_mean,  na.rm=TRUE),
         sd_delta_stair_mean    = sd(delta_stair_mean,    na.rm=TRUE),
         mean_delta_stair_realised = mean(delta_stair_realised, na.rm=TRUE),
         sd_delta_stair_realised   = sd(delta_stair_realised,   na.rm=TRUE),
         mean_realised    = mean(abs(0.5 - vblack_prop), na.rm=TRUE),
         sd_realised      = sd(abs(0.5 - vblack_prop),   na.rm=TRUE)
       ))


# global observed accuracy (post burn-in)
acc_global <- mean(dat_calib_post$correct_num, na.rm = TRUE)

# across-trial mean and sd of delta_stair_realised (post burn-in)
delta_mean_global <- mean(dat_calib_post$delta_stair_realised, na.rm = TRUE)
delta_sd_global   <- sd(dat_calib_post$delta_stair_realised, na.rm = TRUE)


SD_FIXED <- 0.01

p_delta <- ggplot(dat_calib, aes(x = trial)) +
  geom_ribbon(
    data = dat_calib %>% filter(trial <= burn_in_trials),
    aes(
      ymin = 0,
      ymax = ceiling(max(delta_stair_mean + SD_FIXED, na.rm = TRUE) * 100) / 100
    ),
    fill  = "red",
    alpha = 0.15
  ) +
  # ORANGE ribbon: starts after burn-in
  geom_ribbon(
    data = dat_calib %>% filter(trial > burn_in_trials),
    aes(
      ymin = delta_mean_global - delta_sd_global,
      ymax = delta_mean_global + delta_sd_global
    ),
    fill  = "orange",
    alpha = 0.4
  ) +
  # PURPLE ribbon: full trial range
  geom_ribbon(
    aes(
      ymin = delta_stair_mean - SD_FIXED,
      ymax = delta_stair_mean + SD_FIXED
    ),
    fill  = "purple",
    alpha = 0.2
  ) +
  geom_point(aes(y = delta_stair_realised),
             colour = "orange", size = 1, alpha = 1) +
  geom_hline(yintercept = delta_mean_global,
             linetype = "dashed",
             colour = "orange",
             alpha = 1) +
  # geom_vline(xintercept = burn_in_trials,
  #            linetype = 1,
  #            colour = "red",
  #            alpha = 0.5) +
  geom_line(aes(y = delta_stair_mean),
            colour = "purple", linewidth = 0.75) +
  scale_y_continuous(
    breaks = seq(
      0,
      ceiling(max(dat_calib$delta_stair_realised, na.rm = TRUE) * 100) / 100,
      by = 0.02
    ),
    limits = c(0, ceiling(max(dat_calib$delta_stair_mean + SD_FIXED, na.rm = TRUE) * 100) / 100)
  ) +
  labs(
    x = NULL,
    y = "Delta"
  ) +
  theme_classic() +
  ggtitle("Calibration block", subtitle = "Staircase-adjusted difficulty (dot prop. difference from 0.50)")


p_acc <- ggplot(dat_calib, aes(x = trial)) +
  geom_ribbon(
    data = dat_calib %>% filter(trial <= burn_in_trials),
    aes(
      ymin = 0,
      ymax = 1
    ),
    fill  = "red",
    alpha = 0.15
  ) +
  # trial-level correctness (X markers)
  geom_point(
    aes(y = correct_num),
    shape = 4,          # X
    size  = 1,
    stroke = 0.8,
    alpha = 0.5,
    colour = "black"
  ) +
  # geom_line(aes(y = acc_slide),
  #           size = 0.5, colour = "black", alpha = 0.5, linetype = 1) +
  geom_line(aes(y = acc_running),
            linewidth = 0.75, colour = "orange", alpha = 1, linetype = 1) +
  # geom_vline(xintercept = burn_in_trials, linetype = 1, colour = "red", alpha = 0.5) +
  geom_hline(yintercept = target_acc, linetype = 2, colour = "purple") +
  geom_hline(
    yintercept = acc_global,
    linetype   = 1,
    linewidth  = 0.5,
    colour     = "orange",
    alpha      = 0.5
  ) +
  # annotation: target accuracy
  annotate(
    "text",
    x      = Inf,
    y      = -Inf,
    hjust  = 1.05,
    vjust  = -5.0,
    size   = 3.5,
    colour = "black",
    label  = sprintf("Target acc = %.2f", target_acc)
  ) +
  # annotation: observed global accuracy
  annotate(
    "text",
    x      = Inf,
    y      = -Inf,
    hjust  = 1.05,
    vjust  = -3.0,
    size   = 3.5,
    colour = "black",
    label  = sprintf("Observed acc = %.2f", acc_global)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.5, 1)
  ) +
  labs(
    x = "Trial",
    y = "Running accuracy"
  ) +
  theme_classic() +
  ggtitle("", subtitle = "Observed accuracy")


p_calib <- p_delta / p_acc +
  plot_layout(heights = c(1, 1))

p_calib

# ggsave(
#   filename = "plots/calibration.pdf",
#   plot     = p_calib,
#   device   = cairo_pdf,
#   width    = 9,
#   height   = 6,
#   units    = "in"
# )
# 
# ggsave(
#   filename = "plots/calibration.png",
#   plot     = p_calib,
#   width    = 9,
#   height   = 6,
#   units    = "in"
# )


# Manual block ------------------------------------------------------------

dat_manual <- dat %>%
  filter(block == "MANUAL") %>%
  arrange(trial) %>%
  mutate(
    correct_num = as.numeric(correct),
    acc_running = cummean(correct),
    acc_slide   = rollapply(
      correct_num,
      width   = WINDOW,
      FUN     = mean,
      align   = "right",
      fill    = NA,
      partial = TRUE
    )
  )

acc_global <- mean(dat_manual$correct_num, na.rm = TRUE)

delta_mean_global <- dat_manual$delta_fixed_mean[1]
delta_sd_global   <- dat_manual$delta_fixed_sd[1]


p_delta <- ggplot(dat_manual, aes(x = trial)) +
  geom_ribbon(
    aes(
      ymin = delta_mean_global - delta_sd_global,
      ymax = delta_mean_global + delta_sd_global
    ),
    fill  = "orange",
    alpha = 0.4
  ) +
  geom_point(aes(y = abs(0.50 - vblack_prop)),
             colour = "orange", size = 1, alpha = 1) +
  geom_hline(yintercept = delta_mean_global,
             linetype = "dashed",
             colour = "orange",
             alpha = 1) +
  scale_y_continuous(
    breaks = seq(
      0,
      ceiling(max(dat_calib$delta_stair_realised, na.rm = TRUE) * 100) / 100,
      by = 0.02
    ),
    limits = c(0, ceiling(max(dat_calib$delta_stair_mean + SD_FIXED, na.rm = TRUE) * 100) / 100)
  ) +
  labs(
    x = NULL,
    y = "Delta"
  ) +
  theme_classic() +
  ggtitle("Manual block", subtitle = "Difficulty sampled from calibration-block mean and sd")


p_acc <- ggplot(dat_manual, aes(x = trial)) +
  # trial-level correctness (X markers)
  geom_point(
    aes(y = correct_num),
    shape = 4,          # X
    size  = 1,
    stroke = 0.8,
    alpha = 0.5,
    colour = "black"
  ) +
  # geom_line(aes(y = acc_slide),
  #           size = 0.5, colour = "black", alpha = 0.5, linetype = 1) +
  geom_line(aes(y = acc_running),
            linewidth = 0.75, colour = "orange", alpha = 1, linetype = 1) +
  geom_hline(yintercept = target_acc, linetype = 2, colour = "purple") +
  geom_hline(
    yintercept = acc_global,
    linetype   = 1,
    linewidth  = 0.5,
    colour     = "orange",
    alpha      = 0.5
  ) +
  # annotation: target accuracy
  annotate(
    "text",
    x      = Inf,
    y      = -Inf,
    hjust  = 1.05,
    vjust  = -5.0,
    size   = 3.5,
    colour = "black",
    label  = sprintf("Target acc = %.2f", target_acc)
  ) +
  # annotation: observed global accuracy
  annotate(
    "text",
    x      = Inf,
    y      = -Inf,
    hjust  = 1.05,
    vjust  = -3.0,
    size   = 3.5,
    colour = "black",
    label  = sprintf("Observed acc = %.2f", acc_global)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.5, 1)
  ) +
  labs(
    x = "Trial",
    y = "Running accuracy"
  ) +
  theme_classic() +
  ggtitle("", subtitle = "Observed accuracy")


p_manual <- p_delta / p_acc +
  plot_layout(heights = c(1, 1))

p_manual

# ggsave(
#   filename = "plots/manual.pdf",
#   plot     = p_manual,
#   device   = cairo_pdf,
#   width    = 9,
#   height   = 6,
#   units    = "in"
# )
# 
# ggsave(
#   filename = "plots/manual.png",
#   plot     = p_manual,
#   width    = 9,
#   height   = 6,
#   units    = "in"
# )

# Combine plots
p_combo <- p_calib | p_manual

p_combo

ggsave(
  filename = "plots/combined_manual.pdf",
  plot     = p_combo,
  device   = cairo_pdf,
  width    = 16,
  height   = 9,
  units    = "in"
)
# 
# ggsave(
#   filename = "plots/combined_manual.png",
#   plot     = p_combo,
#   width    = 15,
#   height   = 6,
#   units    = "in"
# )


# Automation block (high reliability) -------------------------------------

dat_auto1 <- dat %>%
  filter(block == "AUTOMATION1") %>%
  arrange(trial) %>%
  mutate(
    correct_num = as.numeric(correct),
    acc_running = cummean(correct),
    acc_slide   = rollapply(
      correct_num,
      width   = WINDOW,
      FUN     = mean,
      align   = "right",
      fill    = NA,
      partial = TRUE
    )
  )

acc_global <- mean(dat_auto1$correct_num, na.rm = TRUE)

delta_mean_global <- dat_auto1$delta_fixed_mean[1]
delta_sd_global   <- dat_auto1$delta_fixed_sd[1]

aid_acc_mean <- mean(dat_auto1$aid_correct, na.rm = TRUE)
aid_acc_setting <- unique(dat_auto1$aid_accuracy_setting)


p_delta <- ggplot(dat_auto1, aes(x = trial)) +
  geom_ribbon(
    aes(
      ymin = delta_mean_global - delta_sd_global,
      ymax = delta_mean_global + delta_sd_global
    ),
    fill  = "orange",
    alpha = 0.4
  ) +
  geom_point(aes(y = abs(0.50 - vblack_prop)),
             colour = "orange", size = 1, alpha = 1) +
  geom_hline(yintercept = delta_mean_global,
             linetype = "dashed",
             colour = "orange",
             alpha = 1) +
  scale_y_continuous(
    breaks = seq(
      0,
      ceiling(max(dat_calib$delta_stair_realised, na.rm = TRUE) * 100) / 100,
      by = 0.02
    ),
    limits = c(0, ceiling(max(dat_calib$delta_stair_mean + SD_FIXED, na.rm = TRUE) * 100) / 100)
  ) +
  labs(
    x = NULL,
    y = "Delta"
  ) +
  theme_classic() +
  ggtitle("Automation block (high reliability)", subtitle = "Difficulty sampled from calibration-block mean and sd")


p_acc <- ggplot(dat_auto1, aes(x = trial)) +
  # trial-level aid correctness (open circles)
  geom_point(
    aes(y = as.numeric(aid_correct)),
    shape  = 1,        # open circle
    size   = 1.4,
    stroke = 0.8,
    alpha  = 0.6,
    colour = "forestgreen"
  ) +
  # trial-level correctness (X markers)
  geom_point(
    aes(y = correct_num),
    shape  = 4,
    size   = 1,
    stroke = 0.8,
    alpha  = 0.5,
    colour = "black"
  ) +
  # running accuracy
  geom_line(
    aes(y = acc_running),
    linewidth = 0.75,
    colour    = "orange"
  ) +
  # target accuracy
  geom_hline(
    yintercept = target_acc,
    linetype   = 2,
    colour     = "purple"
  ) +
  # observed global accuracy
  geom_hline(
    yintercept = acc_global,
    linetype   = 1,
    linewidth  = 0.5,
    colour     = "orange",
    alpha      = 0.5
  ) +
  # annotation: observed aid accuracy
  annotate(
    "text",
    x      = Inf,
    y      = -Inf,
    hjust  = 1.05,
    vjust  = -5.0,
    size   = 3.5,
    colour = "black",
    label  = sprintf("Aid acc = %.2f", aid_acc_mean)
  ) +
  # annotation: observed global accuracy
  annotate(
    "text",
    x      = Inf,
    y      = -Inf,
    hjust  = 1.05,
    vjust  = -3.0,
    size   = 3.5,
    colour = "black",
    label  = sprintf("Observed acc = %.2f", acc_global)
  ) +

  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.5, 1)
  ) +

  labs(
    x = "Trial",
    y = "Running accuracy"
  ) +
  theme_classic() +
  ggtitle(
    "",
    subtitle = "Observed accuracy"
  )


p_auto1 <- p_delta / p_acc +
  plot_layout(heights = c(1, 1))

p_auto1

# ggsave(
#   filename = "plots/auto1.pdf",
#   plot     = p_auto1,
#   device   = cairo_pdf,
#   width    = 9,
#   height   = 6,
#   units    = "in"
# )
# 
# ggsave(
#   filename = "plots/auto1.png",
#   plot     = p_auto1,
#   width    = 9,
#   height   = 6,
#   units    = "in"
# )


# Automation block (low reliability) --------------------------------------

dat_auto2 <- dat %>%
  filter(block == "AUTOMATION2") %>%
  arrange(trial) %>%
  mutate(
    correct_num = as.numeric(correct),
    acc_running = cummean(correct),
    acc_slide   = rollapply(
      correct_num,
      width   = WINDOW,
      FUN     = mean,
      align   = "right",
      fill    = NA,
      partial = TRUE
    )
  )

acc_global <- mean(dat_auto2$correct_num, na.rm = TRUE)

delta_mean_global <- dat_auto2$delta_fixed_mean[1]
delta_sd_global   <- dat_auto2$delta_fixed_sd[1]

aid_acc_mean <- mean(dat_auto2$aid_correct, na.rm = TRUE)
aid_acc_setting <- unique(dat_auto2$aid_accuracy_setting)


p_delta <- ggplot(dat_auto2, aes(x = trial)) +
  geom_ribbon(
    aes(
      ymin = delta_mean_global - delta_sd_global,
      ymax = delta_mean_global + delta_sd_global
    ),
    fill  = "orange",
    alpha = 0.4
  ) +
  geom_point(aes(y = abs(0.50 - vblack_prop)),
             colour = "orange", size = 1, alpha = 1) +
  geom_hline(yintercept = delta_mean_global,
             linetype = "dashed",
             colour = "orange",
             alpha = 1) +
  scale_y_continuous(
    breaks = seq(
      0,
      ceiling(max(dat_calib$delta_stair_realised, na.rm = TRUE) * 100) / 100,
      by = 0.02
    ),
    limits = c(0, ceiling(max(dat_calib$delta_stair_mean + SD_FIXED, na.rm = TRUE) * 100) / 100)
  ) +
  labs(
    x = NULL,
    y = "Delta"
  ) +
  theme_classic() +
  ggtitle("Automation block (low reliability)", subtitle = "Difficulty sampled from calibration-block mean and sd")


p_acc <- ggplot(dat_auto2, aes(x = trial)) +
  # trial-level aid correctness (open circles)
  geom_point(
    aes(y = as.numeric(aid_correct)),
    shape  = 1,        # open circle
    size   = 1.4,
    stroke = 0.8,
    alpha  = 0.6,
    colour = "forestgreen"
  ) +
  # trial-level correctness (X markers)
  geom_point(
    aes(y = correct_num),
    shape  = 4,
    size   = 1,
    stroke = 0.8,
    alpha  = 0.5,
    colour = "black"
  ) +
  # running accuracy
  geom_line(
    aes(y = acc_running),
    linewidth = 0.75,
    colour    = "orange"
  ) +
  # target accuracy
  geom_hline(
    yintercept = target_acc,
    linetype   = 2,
    colour     = "purple"
  ) +
  # observed global accuracy
  geom_hline(
    yintercept = acc_global,
    linetype   = 1,
    linewidth  = 0.5,
    colour     = "orange",
    alpha      = 0.5
  ) +
  # annotation: observed aid accuracy
  annotate(
    "text",
    x      = Inf,
    y      = -Inf,
    hjust  = 1.05,
    vjust  = -5.0,
    size   = 3.5,
    colour = "black",
    label  = sprintf("Aid acc = %.2f", aid_acc_mean)
  ) +
  # annotation: observed global accuracy
  annotate(
    "text",
    x      = Inf,
    y      = -Inf,
    hjust  = 1.05,
    vjust  = -3.0,
    size   = 3.5,
    colour = "black",
    label  = sprintf("Observed acc = %.2f", acc_global)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.5, 1)
  ) +
  
  labs(
    x = "Trial",
    y = "Running accuracy"
  ) +
  theme_classic() +
  ggtitle(
    "",
    subtitle = "Observed accuracy"
  )


p_auto2 <- p_delta / p_acc +
  plot_layout(heights = c(1, 1))

p_auto2

# ggsave(
#   filename = "plots/auto2.pdf",
#   plot     = p_auto2,
#   device   = cairo_pdf,
#   width    = 9,
#   height   = 6,
#   units    = "in"
# )
# 
# ggsave(
#   filename = "plots/auto2.png",
#   plot     = p_auto2,
#   width    = 9,
#   height   = 6,
#   units    = "in"
# )

# Combine plots
p_combo <- p_auto1 | p_auto2

p_combo

ggsave(
  filename = "plots/combined_auto.pdf",
  plot     = p_combo,
  device   = cairo_pdf,
  width    = 16,
  height   = 9,
  units    = "in"
)
# 
# ggsave(
#   filename = "plots/combined_auto.png",
#   plot     = p_combo,
#   width    = 15,
#   height   = 6,
#   units    = "in"
# )