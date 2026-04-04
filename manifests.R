# Clear workspace
rm(list = ls())

# Load libraries
library("dplyr")
library("readr")
library("tidyr")
library("patchwork")
library("ggplot2")

# ------------------
# Settings
# ------------------
rt_mode  <- "mean"              # "mean" or "quantile"
# rt_mode <- "quantile"
rt_probs <- c(0.1, 0.5, 0.9)    # used only if rt_mode == "quantile"

# ------------------
# Helpers
# ------------------
make_quantile_labels <- function(probs) {
  format(probs, trim = TRUE, scientific = FALSE)
}

get_axis_limits <- function(y, se = NULL, pad_prop = 0.06) {
  if (is.null(se)) {
    lo <- min(y, na.rm = TRUE)
    hi <- max(y, na.rm = TRUE)
  } else {
    lo <- min(y - se, na.rm = TRUE)
    hi <- max(y + se, na.rm = TRUE)
  }
  
  if (!is.finite(lo) || !is.finite(hi)) {
    return(NULL)
  }
  
  if (identical(lo, hi)) {
    pad <- max(0.05 * abs(lo), 0.05)
  } else {
    pad <- (hi - lo) * pad_prop
  }
  
  c(lo - pad, hi + pad)
}

get_morey_cf <- function(data, subject_col, facet_col, x_col) {
  subj_sym  <- rlang::ensym(subject_col)
  facet_sym <- rlang::ensym(facet_col)
  x_sym     <- rlang::ensym(x_col)
  
  m_counts <- data %>%
    dplyr::filter(!is.na(!!subj_sym), !is.na(!!facet_sym), !is.na(!!x_sym)) %>%
    dplyr::mutate(
      rm_condition = interaction(!!facet_sym, !!x_sym, drop = TRUE, lex.order = TRUE)
    ) %>%
    dplyr::distinct(!!subj_sym, rm_condition) %>%
    dplyr::count(!!subj_sym, name = "m")
  
  if (nrow(m_counts) == 0) {
    return(1)
  }
  
  m <- max(m_counts$m, na.rm = TRUE)
  
  if (!is.finite(m) || m <= 1) {
    return(1)
  }
  
  sqrt(m / (m - 1))
}

# ------------------
# Load data
# ------------------
dat <- read_csv("data/data_virus_all.csv", show_col_types = FALSE)
slider_dat_raw <- read_csv("data/data_virus_sliders_all.csv", show_col_types = FALSE)

if (!dir.exists("plots")) dir.create("plots", recursive = TRUE)

dat <- dat %>%
  mutate(
    block = factor(
      block,
      levels = c("CALIBRATION", "MANUAL", "AUTOMATION1", "AUTOMATION2"),
      labels = c(
        "Calibration",
        "Manual",
        "Automation 95%",
        "Automation 65%"
      )
    ),
    aid_correct = case_when(
      aid_correct %in% c(TRUE, 1, "1", "TRUE", "True", "true") ~ "Aid correct",
      aid_correct %in% c(FALSE, 0, "0", "FALSE", "False", "false") ~ "Aid incorrect",
      TRUE ~ NA_character_
    ),
    facet_group = case_when(
      block %in% c("Calibration", "Manual") ~ "Unaided",
      block == "Automation 95%" ~ "Automation 95%",
      block == "Automation 65%" ~ "Automation 65%",
      TRUE ~ NA_character_
    ),
    x_group = case_when(
      block %in% c("Calibration", "Manual") ~ as.character(block),
      block %in% c("Automation 95%", "Automation 65%") ~ aid_correct,
      TRUE ~ NA_character_
    ),
    facet_group = factor(
      facet_group,
      levels = c("Unaided", "Automation 95%", "Automation 65%")
    ),
    x_group = factor(
      x_group,
      levels = c(
        "Calibration",
        "Manual",
        "Aid correct",
        "Aid incorrect"
      )
    )
  )

# ------------------
# Accuracy summary
# ------------------
subj_acc <- dat %>%
  filter(!is.na(facet_group), !is.na(x_group)) %>%
  group_by(participant_id, facet_group, x_group) %>%
  summarise(
    acc = mean(correct, na.rm = TRUE),
    .groups = "drop"
  )

morey_cf_acc <- get_morey_cf(
  data = subj_acc,
  subject_col = participant_id,
  facet_col = facet_group,
  x_col = x_group
)

grand_mean_acc <- mean(subj_acc$acc, na.rm = TRUE)

acc_summary <- subj_acc %>%
  group_by(participant_id) %>%
  mutate(
    acc_norm = acc - mean(acc, na.rm = TRUE) + grand_mean_acc
  ) %>%
  ungroup() %>%
  group_by(facet_group, x_group) %>%
  summarise(
    mean_acc = mean(acc, na.rm = TRUE),
    se_acc   = sd(acc_norm, na.rm = TRUE) / sqrt(sum(!is.na(acc_norm))) * morey_cf_acc,
    .groups = "drop"
  )

# ------------------
# RT helper
# ------------------
make_rt_summary <- function(data, correct_value, rt_mode = "mean", rt_probs = c(0.1, 0.5, 0.9)) {
  
  rt_dat <- data %>%
    filter(
      correct %in% correct_value,
      !is.na(rt_s),
      !is.na(facet_group),
      !is.na(x_group)
    )
  
  if (rt_mode == "mean") {
    
    subj_rt <- rt_dat %>%
      group_by(participant_id, facet_group, x_group) %>%
      summarise(
        stat = mean(rt_s, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(stat_label = "Mean")
    
  } else if (rt_mode == "quantile") {
    
    q_labels <- make_quantile_labels(rt_probs)
    
    subj_rt <- rt_dat %>%
      group_by(participant_id, facet_group, x_group) %>%
      summarise(
        stat = list(as.numeric(
          quantile(rt_s, probs = rt_probs, na.rm = TRUE, names = FALSE, type = 7)
        )),
        .groups = "drop"
      ) %>%
      mutate(
        stat_label = list(q_labels)
      ) %>%
      unnest(c(stat, stat_label))
    
    subj_rt$stat_label <- factor(subj_rt$stat_label, levels = q_labels)
    
  } else {
    stop("rt_mode must be 'mean' or 'quantile'")
  }
  
  morey_cf_rt <- get_morey_cf(
    data = subj_rt,
    subject_col = participant_id,
    facet_col = facet_group,
    x_col = x_group
  )
  
  grand_means <- subj_rt %>%
    group_by(stat_label) %>%
    summarise(
      grand_mean = mean(stat, na.rm = TRUE),
      .groups = "drop"
    )
  
  subj_rt_norm <- subj_rt %>%
    group_by(participant_id, stat_label) %>%
    mutate(
      subj_mean = mean(stat, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    left_join(grand_means, by = "stat_label") %>%
    mutate(
      stat_norm = stat - subj_mean + grand_mean
    )
  
  rt_summary <- subj_rt_norm %>%
    group_by(facet_group, x_group, stat_label) %>%
    summarise(
      mean_rt = mean(stat, na.rm = TRUE),
      se_rt   = sd(stat_norm, na.rm = TRUE) / sqrt(sum(!is.na(stat_norm))) * morey_cf_rt,
      .groups = "drop"
    )
  
  rt_summary
}

# ------------------
# Correct and error RT summaries
# ------------------
rt_correct_summary <- make_rt_summary(
  data = dat,
  correct_value = TRUE,
  rt_mode = rt_mode,
  rt_probs = rt_probs
)

rt_error_summary <- make_rt_summary(
  data = dat,
  correct_value = FALSE,
  rt_mode = rt_mode,
  rt_probs = rt_probs
)

# ------------------
# Axis limits
# ------------------
acc_ylim <- get_axis_limits(acc_summary$mean_acc, acc_summary$se_acc)

rt_all_vals <- c(rt_correct_summary$mean_rt, rt_error_summary$mean_rt)
rt_all_ses  <- c(rt_correct_summary$se_rt, rt_error_summary$se_rt)
rt_ylim <- get_axis_limits(rt_all_vals, rt_all_ses)

# ------------------
# Plot accuracy
# ------------------
acc_hlines <- tibble(
  facet_group = factor(
    c("Unaided", "Automation 95%", "Automation 65%"),
    levels = levels(acc_summary$facet_group)
  ),
  yint = c(0.80, 0.95, 0.65)
)

p_acc <- ggplot(acc_summary, aes(x = x_group, y = mean_acc, group = 1)) +
  geom_hline(
    data = acc_hlines,
    aes(yintercept = yint),
    linetype = "dashed"
  ) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
    width = 0.15
  ) +
  facet_wrap(~ facet_group, scales = "free_x", nrow = 1) +
  labs(
    x = NULL,
    y = "Mean accuracy",
    title = "Accuracy"
  ) +
  coord_cartesian(ylim = acc_ylim) +
  theme_classic()

# ------------------
# Plot RTs
# ------------------
if (rt_mode == "mean") {
  
  p_rt_correct <- ggplot(rt_correct_summary, aes(x = x_group, y = mean_rt, group = 1)) +
    geom_point(size = 3) +
    geom_line(linewidth = 0.8) +
    geom_errorbar(
      aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
      width = 0.15
    ) +
    facet_wrap(~ facet_group, scales = "free_x", nrow = 1) +
    labs(
      x = NULL,
      y = "Mean correct RT (s)",
      title = "Correct RT"
    ) +
    coord_cartesian(ylim = rt_ylim) +
    theme_classic()
  
  p_rt_error <- ggplot(rt_error_summary, aes(x = x_group, y = mean_rt, group = 1)) +
    geom_point(size = 3) +
    geom_line(linewidth = 0.8) +
    geom_errorbar(
      aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
      width = 0.15
    ) +
    facet_wrap(~ facet_group, scales = "free_x", nrow = 1) +
    labs(
      x = NULL,
      y = "Mean error RT (s)",
      title = "Error RT"
    ) +
    coord_cartesian(ylim = rt_ylim) +
    theme_classic()
  
} else {
  
  p_rt_correct <- ggplot(
    rt_correct_summary,
    aes(x = x_group, y = mean_rt, group = stat_label)
  ) +
    geom_point(size = 2.8) +
    geom_line(linewidth = 0.8, linetype = "dashed") +
    geom_errorbar(
      aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
      width = 0.15
    ) +
    facet_wrap(~ facet_group, scales = "free_x", nrow = 1) +
    labs(
      x = NULL,
      y = "Correct RT quantile (s)",
      title = "Correct RT quantiles"
    ) +
    coord_cartesian(ylim = rt_ylim) +
    theme_classic()
  
  p_rt_error <- ggplot(
    rt_error_summary,
    aes(x = x_group, y = mean_rt, group = stat_label)
  ) +
    geom_point(size = 2.8) +
    geom_line(linewidth = 0.8, linetype = "dashed") +
    geom_errorbar(
      aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
      width = 0.15
    ) +
    facet_wrap(~ facet_group, scales = "free_x", nrow = 1) +
    labs(
      x = NULL,
      y = "Error RT quantile (s)",
      title = "Error RT quantiles"
    ) +
    coord_cartesian(ylim = rt_ylim) +
    theme_classic()
}

# ------------------
# Combine plots
# ------------------
combined_plot <- p_acc / p_rt_correct / p_rt_error

# Show in R
combined_plot

# ------------------
# Save combined plot
# ------------------
dir.create("plots", showWarnings = FALSE)

ggsave(
  filename = "plots/combined_plot.pdf",
  plot = combined_plot,
  width = 11,
  height = 8
)

ggsave(
  filename = "plots/combined_plot.png",
  plot = combined_plot,
  width = 11,
  height = 8,
  dpi = 300
)


# ------------------
# Accuracy and mean RT by block (not grouped by stimulus)
# ------------------

dat_block <- dat %>%
  mutate(
    block_simple = factor(
      block,
      levels = c("Calibration", "Manual", "Automation 95%", "Automation 65%")
    )
  ) %>%
  filter(!is.na(block_simple))

subj_block_summary <- dat_block %>%
  group_by(participant_id, block_simple) %>%
  summarise(
    acc = mean(correct, na.rm = TRUE),
    mean_rt = mean(rt_s, na.rm = TRUE),
    .groups = "drop"
  )

morey_cf_block <- get_morey_cf(
  data = subj_block_summary %>%
    mutate(
      facet_group = "All trials",
      x_group = block_simple
    ),
  subject_col = participant_id,
  facet_col = facet_group,
  x_col = x_group
)

# Accuracy summary
grand_mean_acc_block <- mean(subj_block_summary$acc, na.rm = TRUE)

acc_block_summary <- subj_block_summary %>%
  group_by(participant_id) %>%
  mutate(
    acc_norm = acc - mean(acc, na.rm = TRUE) + grand_mean_acc_block
  ) %>%
  ungroup() %>%
  group_by(block_simple) %>%
  summarise(
    mean_acc = mean(acc, na.rm = TRUE),
    se_acc   = sd(acc_norm, na.rm = TRUE) / sqrt(sum(!is.na(acc_norm))) * morey_cf_block,
    .groups = "drop"
  )

# Mean RT summary
grand_mean_rt_block <- mean(subj_block_summary$mean_rt, na.rm = TRUE)

rt_block_summary <- subj_block_summary %>%
  group_by(participant_id) %>%
  mutate(
    rt_norm = mean_rt - mean(mean_rt, na.rm = TRUE) + grand_mean_rt_block
  ) %>%
  ungroup() %>%
  group_by(block_simple) %>%
  summarise(
    mean_rt = mean(mean_rt, na.rm = TRUE),
    se_rt   = sd(rt_norm, na.rm = TRUE) / sqrt(sum(!is.na(rt_norm))) * morey_cf_block,
    .groups = "drop"
  )

# Self-rated block accuracy summary
subj_slider_block_summary <- slider_dat_raw %>%
  mutate(
    block_simple = factor(
      block,
      levels = c("CALIBRATION", "MANUAL", "AUTOMATION1", "AUTOMATION2"),
      labels = c("Calibration", "Manual", "Automation 95%", "Automation 65%")
    ),
    rating_type = factor(
      question_key,
      levels = c("perc_self_correct", "perc_auto_correct"),
      labels = c("Self-rated own accuracy", "Self-rated aid accuracy")
    )
  ) %>%
  filter(
    !is.na(participant_id),
    !is.na(block_simple),
    !is.na(rating_type),
    !is.na(response_percent)
  ) %>%
  group_by(participant_id, rating_type, block_simple) %>%
  summarise(
    rated_acc = mean(response_percent, na.rm = TRUE) / 100,
    .groups = "drop"
  )

morey_cf_slider_block <- get_morey_cf(
  data = subj_slider_block_summary,
  subject_col = participant_id,
  facet_col = rating_type,
  x_col = block_simple
)

grand_mean_slider_block <- mean(subj_slider_block_summary$rated_acc, na.rm = TRUE)

slider_block_summary <- subj_slider_block_summary %>%
  group_by(participant_id) %>%
  mutate(
    rated_acc_norm = rated_acc - mean(rated_acc, na.rm = TRUE) + grand_mean_slider_block
  ) %>%
  ungroup() %>%
  group_by(rating_type, block_simple) %>%
  summarise(
    mean_rated_acc = mean(rated_acc, na.rm = TRUE),
    se_rated_acc = sd(rated_acc_norm, na.rm = TRUE) /
      sqrt(sum(!is.na(rated_acc_norm))) * morey_cf_slider_block,
    .groups = "drop"
  )

# Reference lines
acc_block_ylim <- get_axis_limits(
  c(
    acc_block_summary$mean_acc,
    slider_block_summary$mean_rated_acc,
    0.80,
    0.95,
    0.65
  ),
  c(
    acc_block_summary$se_acc,
    slider_block_summary$se_rated_acc,
    0,
    0,
    0
  )
)
rt_block_ylim  <- get_axis_limits(rt_block_summary$mean_rt, rt_block_summary$se_rt)

p_acc_block <- ggplot() +
  geom_point(
    data = acc_block_summary,
    aes(
      x = block_simple,
      y = mean_acc,
      colour = "Observed accuracy",
      shape = "Observed accuracy"
    ),
    size = 3
  ) +
  geom_line(
    data = acc_block_summary,
    aes(
      x = block_simple,
      y = mean_acc,
      group = 1,
      colour = "Observed accuracy",
      linetype = "Observed accuracy"
    ),
    linewidth = 0.8
  ) +
  geom_errorbar(
    data = acc_block_summary,
    aes(
      x = block_simple,
      ymin = mean_acc - se_acc,
      ymax = mean_acc + se_acc,
      colour = "Observed accuracy"
    ),
    width = 0.15
  ) +
  geom_point(
    data = slider_block_summary,
    aes(
      x = block_simple,
      y = mean_rated_acc,
      colour = rating_type,
      shape = rating_type
    ),
    size = 2.8,
    na.rm = TRUE
  ) +
  geom_line(
    data = slider_block_summary,
    aes(
      x = block_simple,
      y = mean_rated_acc,
      group = rating_type,
      colour = rating_type,
      linetype = rating_type
    ),
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_errorbar(
    data = slider_block_summary,
    aes(
      x = block_simple,
      ymin = mean_rated_acc - se_rated_acc,
      ymax = mean_rated_acc + se_rated_acc,
      colour = rating_type
    ),
    width = 0.12,
    na.rm = TRUE
  ) +
  annotate("segment", x = 0.5, xend = 4.5, y = 0.80, yend = 0.80, linetype = "dashed") +
  annotate("segment", x = 2.5, xend = 3.5, y = 0.95, yend = 0.95, linetype = "dashed") +
  annotate("segment", x = 3.5, xend = 4.5, y = 0.65, yend = 0.65, linetype = "dashed") +
  scale_colour_manual(
    values = c(
      "Observed accuracy" = "black",
      "Self-rated own accuracy" = "#0072B2",
      "Self-rated aid accuracy" = "#D55E00"
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(
      "Observed accuracy" = 16,
      "Self-rated own accuracy" = 17,
      "Self-rated aid accuracy" = 15
    ),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c(
      "Observed accuracy" = "solid",
      "Self-rated own accuracy" = "dotted",
      "Self-rated aid accuracy" = "dotted"
    ),
    name = NULL
  ) +
  scale_y_continuous(
    labels = function(x) paste0(round(x * 100), "%")
  ) +
  labs(
    x = "Block",
    y = "Accuracy (%)",
    title = "Accuracy by block"
  ) +
  coord_cartesian(ylim = acc_block_ylim) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )

p_rt_block <- ggplot(
  rt_block_summary,
  aes(x = block_simple, y = mean_rt, group = 1)
) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
    width = 0.15
  ) +
  labs(
    x = "Block",
    y = "Mean RT (s)",
    title = "Mean RT by block"
  ) +
  coord_cartesian(ylim = rt_block_ylim) +
  theme_classic()

block_plot <- p_acc_block / p_rt_block
block_plot

ggsave(
  filename = "plots/block_acc_plot.pdf",
  plot = p_acc_block,
  width = 6,
  height = 4.5
)

ggsave(
  filename = "plots/block_acc_plot.png",
  plot = p_acc_block,
  width = 6,
  height = 4.5,
  dpi = 300
)

# ggsave(
#   filename = "plots/block_plot.pdf",
#   plot = block_plot,
#   width = 6,
#   height = 6
# )
# 
# ggsave(
#   filename = "plots/block_plot.png",
#   plot = block_plot,
#   width = 6,
#   height = 6,
#   dpi = 300
# )

# ------------------
# Accuracy and mean RT by stimulus and block
# ------------------

dat_stim <- dat %>%
  mutate(
    block_simple = factor(
      block,
      levels = c("Calibration", "Manual", "Automation 95%", "Automation 65%")
    ),
    stimulus = factor(
      stimulus,
      levels = c("BLACK", "WHITE"),
      labels = c("BLACK", "WHITE")
    )
  ) %>%
  filter(!is.na(stimulus), !is.na(block_simple))

subj_stim_summary <- dat_stim %>%
  group_by(participant_id, stimulus, block_simple) %>%
  summarise(
    acc = mean(correct, na.rm = TRUE),
    mean_rt = mean(rt_s, na.rm = TRUE),
    .groups = "drop"
  )

morey_cf_stim <- get_morey_cf(
  data = subj_stim_summary %>%
    mutate(
      facet_group = stimulus,
      x_group = block_simple
    ),
  subject_col = participant_id,
  facet_col = facet_group,
  x_col = x_group
)

# Accuracy summary
grand_mean_acc_stim <- mean(subj_stim_summary$acc, na.rm = TRUE)

acc_stim_summary <- subj_stim_summary %>%
  group_by(participant_id, stimulus) %>%
  mutate(
    acc_norm = acc - mean(acc, na.rm = TRUE) + grand_mean_acc_stim
  ) %>%
  ungroup() %>%
  group_by(stimulus, block_simple) %>%
  summarise(
    mean_acc = mean(acc, na.rm = TRUE),
    se_acc   = sd(acc_norm, na.rm = TRUE) / sqrt(sum(!is.na(acc_norm))) * morey_cf_stim,
    .groups = "drop"
  )

# Mean RT summary
grand_mean_rt_stim <- mean(subj_stim_summary$mean_rt, na.rm = TRUE)

rt_stim_summary <- subj_stim_summary %>%
  group_by(participant_id, stimulus) %>%
  mutate(
    rt_norm = mean_rt - mean(mean_rt, na.rm = TRUE) + grand_mean_rt_stim
  ) %>%
  ungroup() %>%
  group_by(stimulus, block_simple) %>%
  summarise(
    mean_rt = mean(mean_rt, na.rm = TRUE),
    se_rt   = sd(rt_norm, na.rm = TRUE) / sqrt(sum(!is.na(rt_norm))) * morey_cf_stim,
    .groups = "drop"
  )

# Axis limits
acc_stim_ylim <- get_axis_limits(acc_stim_summary$mean_acc, acc_stim_summary$se_acc)
rt_stim_ylim  <- get_axis_limits(rt_stim_summary$mean_rt, rt_stim_summary$se_rt)

grey_bg <- rgb(128, 128, 128, maxColorValue = 255)
stim_dodge <- position_dodge(width = 0.25)

p_acc_stim <- ggplot(
  acc_stim_summary,
  aes(x = block_simple, y = mean_acc, group = stimulus, colour = stimulus)
) +
  geom_point(size = 3, position = stim_dodge) +
  geom_line(linewidth = 0.8, position = stim_dodge) +
  geom_errorbar(
    aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
    width = 0.15,
    position = stim_dodge
  ) +
  scale_colour_manual(
    values = c("BLACK" = "#111111", "WHITE" = "#ffffff")
  ) +
  labs(
    x = "Block",
    y = "Mean accuracy",
    title = "Accuracy by stimulus and block",
    colour = "Stimulus"
  ) +
  coord_cartesian(ylim = acc_stim_ylim) +
  theme_classic() +
  theme(
    panel.background  = element_rect(fill = grey_bg, colour = NA),
    plot.background   = element_rect(fill = grey_bg, colour = NA),
    legend.background = element_rect(fill = grey_bg, colour = NA),
    legend.key        = element_rect(fill = grey_bg, colour = NA),
    axis.text         = element_text(colour = "white"),
    axis.title        = element_text(colour = "white"),
    plot.title        = element_text(colour = "white"),
    legend.text       = element_text(colour = "white"),
    legend.title      = element_text(colour = "white"),
    axis.line         = element_line(colour = "white"),
    axis.ticks        = element_line(colour = "white")
  )

p_rt_stim <- ggplot(
  rt_stim_summary,
  aes(x = block_simple, y = mean_rt, group = stimulus, colour = stimulus)
) +
  geom_point(size = 3, position = stim_dodge) +
  geom_line(linewidth = 0.8, position = stim_dodge) +
  geom_errorbar(
    aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
    width = 0.15,
    position = stim_dodge
  ) +
  scale_colour_manual(
    values = c("BLACK" = "#111111", "WHITE" = "#ffffff")
  ) +
  labs(
    x = "Block",
    y = "Mean RT (s)",
    title = "Mean RT by stimulus and block",
    colour = "Stimulus"
  ) +
  coord_cartesian(ylim = rt_stim_ylim) +
  theme_classic() +
  theme(
    panel.background  = element_rect(fill = grey_bg, colour = NA),
    plot.background   = element_rect(fill = grey_bg, colour = NA),
    legend.background = element_rect(fill = grey_bg, colour = NA),
    legend.key        = element_rect(fill = grey_bg, colour = NA),
    axis.text         = element_text(colour = "white"),
    axis.title        = element_text(colour = "white"),
    plot.title        = element_text(colour = "white"),
    legend.text       = element_text(colour = "white"),
    legend.title      = element_text(colour = "white"),
    axis.line         = element_line(colour = "white"),
    axis.ticks        = element_line(colour = "white")
  )

stimulus_plot <- p_acc_stim / p_rt_stim
stimulus_plot

ggsave(
  filename = "plots/stimulus_plot.pdf",
  plot = stimulus_plot,
  width = 8,
  height = 6
)

ggsave(
  filename = "plots/stimulus_plot.png",
  plot = stimulus_plot,
  width = 8,
  height = 6,
  dpi = 300
)


# ------------------
# Individual differences: participant accuracy and mean RT by block
# ------------------

dat_id <- dat %>%
  mutate(
    block_simple = factor(
      block,
      levels = c("Calibration", "Manual", "Automation 95%", "Automation 65%")
    ),
    participant_id = factor(participant_id)
  ) %>%
  filter(!is.na(participant_id), !is.na(block_simple))

id_summary <- dat_id %>%
  group_by(participant_id, block_simple) %>%
  summarise(
    acc = mean(correct, na.rm = TRUE),
    mean_rt = mean(rt_s, na.rm = TRUE),
    .groups = "drop"
  )

# use Calibration accuracy only to define participant order
participant_order <- id_summary %>%
  filter(block_simple == "Calibration") %>%
  arrange(acc) %>%
  pull(participant_id)

id_summary <- id_summary %>%
  mutate(
    participant_id = factor(participant_id, levels = participant_order)
  )

# orange 0.80 line in all panels
acc_base_lines <- tibble(
  block_simple = factor(
    c("Calibration", "Manual", "Automation 95%", "Automation 65%"),
    levels = c("Calibration", "Manual", "Automation 95%", "Automation 65%")
    # c("Calibration", "Manual"),
    # levels = c("Calibration", "Manual")
  ),
  xint = 0.80
)

# purple automation-target lines in auto panels
acc_auto_lines <- tibble(
  block_simple = factor(
    c("Automation 95%", "Automation 65%"),
    levels = c("Calibration", "Manual", "Automation 95%", "Automation 65%")
  ),
  xint = c(0.95, 0.65)
)

# empirical mean accuracy line per block
acc_mean_lines <- id_summary %>%
  group_by(block_simple) %>%
  summarise(
    xint = mean(acc, na.rm = TRUE),
    .groups = "drop"
  )

# empirical mean RT line per block
rt_mean_lines <- id_summary %>%
  group_by(block_simple) %>%
  summarise(
    xint = mean(mean_rt, na.rm = TRUE),
    .groups = "drop"
  )

acc_id_xlim <- c(0.6, 1.0)
rt_id_xlim  <- get_axis_limits(c(id_summary$mean_rt, rt_mean_lines$xint))

p_acc_id <- ggplot(
  id_summary,
  aes(x = acc, y = participant_id)
) +
  geom_vline(
    data = acc_base_lines,
    aes(xintercept = xint),
    colour = "orange",
    linetype = "dashed"
  ) +
  geom_vline(
    data = acc_auto_lines,
    aes(xintercept = xint),
    colour = "purple",
    linetype = "dashed"
  ) +
  geom_vline(
    data = acc_mean_lines,
    aes(xintercept = xint),
    linetype = "solid"
  ) +
  geom_point(size = 2.2) +
  facet_wrap(~ block_simple, nrow = 1) +
  labs(
    x = "Mean accuracy",
    y = "Participant",
    title = "Individual accuracy by block"
  ) +
  coord_cartesian(xlim = acc_id_xlim) +
  theme_classic()

p_rt_id <- ggplot(
  id_summary,
  aes(x = mean_rt, y = participant_id)
) +
  geom_vline(
    data = rt_mean_lines,
    aes(xintercept = xint),
    linetype = "solid"
  ) +
  geom_point(size = 2.2) +
  facet_wrap(~ block_simple, nrow = 1) +
  labs(
    x = "Mean RT (s)",
    y = "Participant",
    title = "Individual mean RT by block"
  ) +
  coord_cartesian(xlim = rt_id_xlim) +
  theme_classic()

individual_differences_plot <- p_acc_id / p_rt_id
individual_differences_plot

ggsave(
  filename = "plots/individual_differences_plot.pdf",
  plot = individual_differences_plot,
  width = 9,
  height = 6
)

ggsave(
  filename = "plots/individual_differences_plot.png",
  plot = individual_differences_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------
# Calibration vs Manual participant accuracy difference
# ------------------

calib_manual_acc <- dat %>%
  mutate(
    block_simple = factor(
      block,
      levels = c("Calibration", "Manual")
    )
  ) %>%
  filter(!is.na(correct), !is.na(block_simple)) %>%
  group_by(participant_id, block_simple) %>%
  summarise(
    accuracy = mean(correct, na.rm = TRUE),
    n_trials = dplyr::n(),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = block_simple,
    values_from = c(accuracy, n_trials)
  ) %>%
  mutate(
    accuracy_diff = accuracy_Manual - accuracy_Calibration
  ) %>%
  arrange(participant_id)

calib_manual_test <- t.test(
  calib_manual_acc$accuracy_Manual,
  calib_manual_acc$accuracy_Calibration,
  paired = TRUE
)

calibration_vs_target_test <- t.test(
  calib_manual_acc$accuracy_Calibration,
  mu = 0.80
)

manual_vs_target_test <- t.test(
  calib_manual_acc$accuracy_Manual,
  mu = 0.80
)

calib_manual_long <- calib_manual_acc %>%
  select(participant_id, accuracy_Calibration, accuracy_Manual) %>%
  pivot_longer(
    cols = c(accuracy_Calibration, accuracy_Manual),
    names_to = "block_simple",
    values_to = "accuracy"
  ) %>%
  mutate(
    block_simple = factor(
      block_simple,
      levels = c("accuracy_Calibration", "accuracy_Manual"),
      labels = c("Calibration", "Manual")
    ),
    participant_id = factor(participant_id, levels = calib_manual_acc$participant_id)
  )

participant_palette <- setNames(
  scales::hue_pal()(length(levels(calib_manual_long$participant_id))),
  levels(calib_manual_long$participant_id)
)

calib_manual_ref_lines <- tibble(
  yint = c(0.95, 0.80, 0.65),
  label = c("Aid high", "Calib. target", "Aid low")
)

calib_manual_diff_limits <- get_axis_limits(
  calib_manual_acc$accuracy_diff,
  pad_prop = 0.12
)

p_calib_manual_acc <- ggplot(
  calib_manual_long,
  aes(x = block_simple, y = accuracy, group = participant_id)
) +
  geom_hline(
    data = calib_manual_ref_lines,
    aes(yintercept = yint),
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = calib_manual_ref_lines,
    aes(x = "Manual", y = yint, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = -0.25,
    size = 3.2,
    nudge_x = 0.48
  ) +
  geom_line(colour = "grey70", linewidth = 0.7) +
  geom_point(aes(colour = participant_id), size = 2.4, show.legend = FALSE) +
  scale_colour_manual(values = participant_palette) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    linewidth = 1.1,
    colour = "black"
  ) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "point",
    size = 3.2,
    colour = "black"
  ) +
  labs(
    x = NULL,
    y = "Participant accuracy",
    title = "Calibration vs Manual accuracy"
  ) +
  coord_cartesian(ylim = c(0.50, 1.00), clip = "off") +
  theme_classic() +
  theme(
    plot.margin = margin(5.5, 45, 5.5, 5.5)
  )

p_calib_manual_diff <- ggplot(
  calib_manual_acc,
  aes(
    x = reorder(factor(participant_id), accuracy_diff),
    y = accuracy_diff,
    fill = factor(participant_id)
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col() +
  scale_fill_manual(values = participant_palette, guide = "none") +
  labs(
    x = "Participant",
    y = "Manual - Calibration accuracy",
    title = "Participant accuracy difference"
  ) +
  coord_cartesian(ylim = calib_manual_diff_limits) +
  theme_classic()

calib_manual_plot <- p_calib_manual_acc / p_calib_manual_diff
calib_manual_plot

ggsave(
  filename = "plots/calibration_manual_accuracy_plot.pdf",
  plot = calib_manual_plot,
  width = 7,
  height = 7
)

ggsave(
  filename = "plots/calibration_manual_accuracy_plot.png",
  plot = calib_manual_plot,
  width = 7,
  height = 7,
  dpi = 300
)

write_csv(
  calib_manual_acc,
  "plots/calibration_manual_accuracy_by_participant.csv"
)

test_summary <- bind_rows(
  tibble(
    test = "paired_t_test_manual_vs_calibration_accuracy",
    n_participants = length(calib_manual_acc$participant_id),
    reference_value = NA_real_,
    mean_calibration = mean(calib_manual_acc$accuracy_Calibration, na.rm = TRUE),
    mean_manual = mean(calib_manual_acc$accuracy_Manual, na.rm = TRUE),
    mean_difference = mean(calib_manual_acc$accuracy_diff, na.rm = TRUE),
    t_statistic = unname(calib_manual_test$statistic),
    df = unname(calib_manual_test$parameter),
    p_value = calib_manual_test$p.value,
    conf_low = calib_manual_test$conf.int[1],
    conf_high = calib_manual_test$conf.int[2]
  ),
  tibble(
    test = "one_sample_t_test_calibration_vs_0.80",
    n_participants = length(calib_manual_acc$participant_id),
    reference_value = 0.80,
    mean_calibration = mean(calib_manual_acc$accuracy_Calibration, na.rm = TRUE),
    mean_manual = NA_real_,
    mean_difference = mean(calib_manual_acc$accuracy_Calibration, na.rm = TRUE) - 0.80,
    t_statistic = unname(calibration_vs_target_test$statistic),
    df = unname(calibration_vs_target_test$parameter),
    p_value = calibration_vs_target_test$p.value,
    conf_low = calibration_vs_target_test$conf.int[1],
    conf_high = calibration_vs_target_test$conf.int[2]
  ),
  tibble(
    test = "one_sample_t_test_manual_vs_0.80",
    n_participants = length(calib_manual_acc$participant_id),
    reference_value = 0.80,
    mean_calibration = NA_real_,
    mean_manual = mean(calib_manual_acc$accuracy_Manual, na.rm = TRUE),
    mean_difference = mean(calib_manual_acc$accuracy_Manual, na.rm = TRUE) - 0.80,
    t_statistic = unname(manual_vs_target_test$statistic),
    df = unname(manual_vs_target_test$parameter),
    p_value = manual_vs_target_test$p.value,
    conf_low = manual_vs_target_test$conf.int[1],
    conf_high = manual_vs_target_test$conf.int[2]
  )
)

write_csv(
  test_summary,
  "plots/calibration_manual_accuracy_test_summary.csv"
)
