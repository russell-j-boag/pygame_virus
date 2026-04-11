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
CALIB_SUMMARY_LAST_N <- 150
PLOT_DIR <- "plots"
BLOCK_RAW_LEVELS <- c("CALIBRATION", "MANUAL", "AUTOMATION1", "AUTOMATION2")
BLOCK_LEVELS <- c("Calibration", "Manual", "Automation 95%", "Automation 65%")
AUTO_BLOCK_LABELS <- c(
  "AUTOMATION1" = "Automation 95%",
  "AUTOMATION2" = "Automation 65%"
)
MOREY_SE_SUBTITLE <- "Error bars are Morey-Cousineau within-subject SEs"
rt_mode  <- "mean"              # "mean" or "quantile"
# rt_mode <- "quantile"
rt_probs <- c(0.1, 0.5, 0.9)    # used only if rt_mode == "quantile"

# ------------------
# Helpers
# ------------------
make_quantile_labels <- function(probs) {
  format(probs, trim = TRUE, scientific = FALSE)
}

factor_display_block <- function(x) {
  factor(x, levels = BLOCK_RAW_LEVELS, labels = BLOCK_LEVELS)
}

factor_block_simple <- function(x) {
  factor(x, levels = BLOCK_LEVELS)
}

factor_auto_block <- function(x) {
  factor(
    recode(as.character(x), !!!AUTO_BLOCK_LABELS),
    levels = unname(AUTO_BLOCK_LABELS)
  )
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

restrict_calibration_trials <- function(
  data,
  participant_col,
  block_col,
  trial_col,
  calibration_label = "Calibration",
  last_n = CALIB_SUMMARY_LAST_N
) {
  participant_sym <- rlang::ensym(participant_col)
  block_sym <- rlang::ensym(block_col)
  trial_sym <- rlang::ensym(trial_col)
  
  data %>%
    mutate(
      .trial_idx_num = suppressWarnings(as.integer(!!trial_sym))
    ) %>%
    group_by(!!participant_sym, !!block_sym) %>%
    arrange(.trial_idx_num, .by_group = TRUE) %>%
    filter(
      as.character(!!block_sym) != calibration_label |
        row_number() > pmax(dplyr::n() - last_n, 0)
    ) %>%
    ungroup() %>%
    select(-.trial_idx_num)
}

summarise_morey_mean <- function(
  data,
  subject_col,
  value_col,
  x_col,
  summary_cols,
  facet_col = NULL,
  norm_group_cols = NULL,
  mean_name = "mean_value",
  se_name = "se_value"
) {
  subject_sym <- rlang::sym(subject_col)
  value_sym <- rlang::sym(value_col)
  x_sym <- rlang::sym(x_col)
  summary_syms <- rlang::syms(summary_cols)
  norm_group_cols <- if (is.null(norm_group_cols)) character(0) else norm_group_cols
  norm_group_syms <- rlang::syms(norm_group_cols)
  
  if (is.null(facet_col)) {
    data <- data %>%
      mutate(.morey_facet = "All trials")
    facet_sym <- rlang::sym(".morey_facet")
  } else {
    facet_sym <- rlang::sym(facet_col)
  }
  
  morey_cf <- rlang::inject(
    get_morey_cf(
      data = data,
      subject_col = !!subject_sym,
      facet_col = !!facet_sym,
      x_col = !!x_sym
    )
  )
  
  if (length(norm_group_cols) == 0) {
    grand_mean <- mean(data[[value_col]], na.rm = TRUE)
    
    data_norm <- data %>%
      group_by(!!subject_sym) %>%
      mutate(
        .subj_mean = mean(!!value_sym, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        .norm_value = !!value_sym - .subj_mean + grand_mean
      )
  } else {
    grand_means <- data %>%
      group_by(!!!norm_group_syms) %>%
      summarise(
        .grand_mean = mean(!!value_sym, na.rm = TRUE),
        .groups = "drop"
      )
    
    data_norm <- data %>%
      group_by(!!subject_sym, !!!norm_group_syms) %>%
      mutate(
        .subj_mean = mean(!!value_sym, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      left_join(grand_means, by = norm_group_cols) %>%
      mutate(
        .norm_value = !!value_sym - .subj_mean + .grand_mean
      )
  }
  
  data_norm %>%
    group_by(!!!summary_syms) %>%
    summarise(
      !!mean_name := mean(!!value_sym, na.rm = TRUE),
      !!se_name := sd(.norm_value, na.rm = TRUE) /
        sqrt(sum(!is.na(.norm_value))) * morey_cf,
      .groups = "drop"
    )
}

save_plot_pair <- function(plot, filename_stem, width, height, dpi = 300) {
  ggsave(
    filename = file.path(PLOT_DIR, paste0(filename_stem, ".pdf")),
    plot = plot,
    width = width,
    height = height
  )
  
  ggsave(
    filename = file.path(PLOT_DIR, paste0(filename_stem, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
}

# ------------------
# Load data
# ------------------
trial_dat_raw <- read_csv("data/data_virus_all.csv", show_col_types = FALSE)
slider_dat_raw <- read_csv("data/data_virus_sliders_all.csv", show_col_types = FALSE)
postblock_dat_raw <- read_csv("data/data_virus_postblock_all.csv", show_col_types = FALSE)

if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

dat <- trial_dat_raw %>%
  mutate(
    block = factor_display_block(block),
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
  filter(!is.na(facet_group), !is.na(x_group), !is.na(correct)) %>%
  restrict_calibration_trials(
    participant_col = participant_id,
    block_col = block,
    trial_col = trial_idx
  ) %>%
  group_by(participant_id, facet_group, x_group) %>%
  summarise(
    acc = mean(correct, na.rm = TRUE),
    .groups = "drop"
  )

acc_summary <- summarise_morey_mean(
  data = subj_acc,
  subject_col = "participant_id",
  value_col = "acc",
  x_col = "x_group",
  summary_cols = c("facet_group", "x_group"),
  facet_col = "facet_group",
  mean_name = "mean_acc",
  se_name = "se_acc"
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
    ) %>%
    restrict_calibration_trials(
      participant_col = participant_id,
      block_col = block,
      trial_col = trial_idx
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
  
  summarise_morey_mean(
    data = subj_rt,
    subject_col = "participant_id",
    value_col = "stat",
    x_col = "x_group",
    summary_cols = c("facet_group", "x_group", "stat_label"),
    facet_col = "facet_group",
    norm_group_cols = "stat_label",
    mean_name = "mean_rt",
    se_name = "se_rt"
  )
}

make_rt_plot <- function(data, rt_mode, y_label, title, y_limits) {
  if (rt_mode == "mean") {
    base_plot <- ggplot(data, aes(x = x_group, y = mean_rt, group = 1)) +
      geom_point(size = 3) +
      geom_line(linewidth = 0.8)
  } else if (rt_mode == "quantile") {
    base_plot <- ggplot(data, aes(x = x_group, y = mean_rt, group = stat_label)) +
      geom_point(size = 2.8) +
      geom_line(linewidth = 0.8, linetype = "dashed")
  } else {
    stop("rt_mode must be 'mean' or 'quantile'")
  }
  
  base_plot +
    geom_errorbar(
      aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt),
      width = 0.15
    ) +
    facet_wrap(~ facet_group, scales = "free_x", nrow = 1) +
    labs(
      x = NULL,
      y = y_label,
      title = title,
      subtitle = MOREY_SE_SUBTITLE
    ) +
    coord_cartesian(ylim = y_limits) +
    theme_classic()
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
    title = "Accuracy",
    subtitle = MOREY_SE_SUBTITLE
  ) +
  coord_cartesian(ylim = acc_ylim) +
  theme_classic()

# ------------------
# Plot RTs
# ------------------
if (rt_mode == "mean") {
  p_rt_correct <- make_rt_plot(
    data = rt_correct_summary,
    rt_mode = rt_mode,
    y_label = "Mean correct RT (s)",
    title = "Correct RT",
    y_limits = rt_ylim
  )
  
  p_rt_error <- make_rt_plot(
    data = rt_error_summary,
    rt_mode = rt_mode,
    y_label = "Mean error RT (s)",
    title = "Error RT",
    y_limits = rt_ylim
  )
} else {
  p_rt_correct <- make_rt_plot(
    data = rt_correct_summary,
    rt_mode = rt_mode,
    y_label = "Correct RT quantile (s)",
    title = "Correct RT quantiles",
    y_limits = rt_ylim
  )
  
  p_rt_error <- make_rt_plot(
    data = rt_error_summary,
    rt_mode = rt_mode,
    y_label = "Error RT quantile (s)",
    title = "Error RT quantiles",
    y_limits = rt_ylim
  )
}

# ------------------
# Combine plots
# ------------------
block_acc_rt_plot <- p_acc / p_rt_correct / p_rt_error

# Show in R
block_acc_rt_plot

# ------------------
# Save combined plot
# ------------------
save_plot_pair(block_acc_rt_plot, "block_acc_rt_plot", width = 11, height = 8)


# ------------------
# Accuracy and mean RT by block (not grouped by stimulus)
# ------------------

dat_block <- dat %>%
  mutate(
    block_simple = factor_block_simple(block)
  ) %>%
  filter(!is.na(block_simple))

dat_block_acc <- dat_block %>%
  filter(!is.na(correct)) %>%
  restrict_calibration_trials(
    participant_col = participant_id,
    block_col = block_simple,
    trial_col = trial_idx
  )

subj_block_acc_summary <- dat_block_acc %>%
  group_by(participant_id, block_simple) %>%
  summarise(
    acc = mean(correct, na.rm = TRUE),
    .groups = "drop"
  )

subj_block_rt_summary <- dat_block %>%
  filter(!is.na(rt_s)) %>%
  restrict_calibration_trials(
    participant_col = participant_id,
    block_col = block_simple,
    trial_col = trial_idx
  ) %>%
  group_by(participant_id, block_simple) %>%
  summarise(
    mean_rt = mean(rt_s, na.rm = TRUE),
    .groups = "drop"
  )

acc_block_summary <- summarise_morey_mean(
  data = subj_block_acc_summary,
  subject_col = "participant_id",
  value_col = "acc",
  x_col = "block_simple",
  summary_cols = "block_simple",
  mean_name = "mean_acc",
  se_name = "se_acc"
)

rt_block_summary <- summarise_morey_mean(
  data = subj_block_rt_summary,
  subject_col = "participant_id",
  value_col = "mean_rt",
  x_col = "block_simple",
  summary_cols = "block_simple",
  mean_name = "mean_rt",
  se_name = "se_rt"
)

# Self-rated block accuracy summary
subj_slider_block_summary <- slider_dat_raw %>%
  mutate(
    block_simple = factor_display_block(block),
    rating_type = factor(
      slider_key,
      levels = c("perc_self_correct", "perc_auto_correct"),
      labels = c("Self-rated own accuracy", "Self-rated aid accuracy")
    )
  ) %>%
  filter(
    !is.na(participant_id),
    !is.na(block_simple),
    !is.na(rating_type),
    !is.na(response)
  ) %>%
  group_by(participant_id, rating_type, block_simple) %>%
  summarise(
    rated_acc = mean(response, na.rm = TRUE) / 100,
    .groups = "drop"
  )

slider_block_summary <- summarise_morey_mean(
  data = subj_slider_block_summary,
  subject_col = "participant_id",
  value_col = "rated_acc",
  x_col = "block_simple",
  summary_cols = c("rating_type", "block_simple"),
  facet_col = "rating_type",
  mean_name = "mean_rated_acc",
  se_name = "se_rated_acc"
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
    title = "Accuracy by block",
    subtitle = MOREY_SE_SUBTITLE
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
    title = "Mean RT by block",
    subtitle = MOREY_SE_SUBTITLE
  ) +
  coord_cartesian(ylim = rt_block_ylim) +
  theme_classic()

block_plot <- p_acc_block / p_rt_block
block_plot

save_plot_pair(p_acc_block, "block_acc_plot", width = 6, height = 4.5)

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
    block_simple = factor_block_simple(block),
    stimulus = factor(
      stimulus,
      levels = c("conflict", "nonconflict"),
      labels = c("Conflict", "Non-conflict")
    )
  ) %>%
  filter(!is.na(stimulus), !is.na(block_simple))

subj_stim_acc_summary <- dat_stim %>%
  filter(!is.na(correct)) %>%
  restrict_calibration_trials(
    participant_col = participant_id,
    block_col = block_simple,
    trial_col = trial_idx
  ) %>%
  group_by(participant_id, stimulus, block_simple) %>%
  summarise(
    acc = mean(correct, na.rm = TRUE),
    .groups = "drop"
  )

subj_stim_rt_summary <- dat_stim %>%
  filter(!is.na(rt_s)) %>%
  restrict_calibration_trials(
    participant_col = participant_id,
    block_col = block_simple,
    trial_col = trial_idx
  ) %>%
  group_by(participant_id, stimulus, block_simple) %>%
  summarise(
    mean_rt = mean(rt_s, na.rm = TRUE),
    .groups = "drop"
  )

acc_stim_summary <- summarise_morey_mean(
  data = subj_stim_acc_summary,
  subject_col = "participant_id",
  value_col = "acc",
  x_col = "block_simple",
  summary_cols = c("stimulus", "block_simple"),
  facet_col = "stimulus",
  norm_group_cols = "stimulus",
  mean_name = "mean_acc",
  se_name = "se_acc"
)

rt_stim_summary <- summarise_morey_mean(
  data = subj_stim_rt_summary,
  subject_col = "participant_id",
  value_col = "mean_rt",
  x_col = "block_simple",
  summary_cols = c("stimulus", "block_simple"),
  facet_col = "stimulus",
  norm_group_cols = "stimulus",
  mean_name = "mean_rt",
  se_name = "se_rt"
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
    values = c("Conflict" = "#111111", "Non-conflict" = "#ffffff")
  ) +
  labs(
    x = "Block",
    y = "Mean accuracy",
    title = "Accuracy by stimulus and block",
    subtitle = MOREY_SE_SUBTITLE,
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
    plot.subtitle     = element_text(colour = "white"),
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
    values = c("Conflict" = "#111111", "Non-conflict" = "#ffffff")
  ) +
  labs(
    x = "Block",
    y = "Mean RT (s)",
    title = "Mean RT by stimulus and block",
    subtitle = MOREY_SE_SUBTITLE,
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
    plot.subtitle     = element_text(colour = "white"),
    legend.text       = element_text(colour = "white"),
    legend.title      = element_text(colour = "white"),
    axis.line         = element_line(colour = "white"),
    axis.ticks        = element_line(colour = "white")
  )

stimulus_plot <- p_acc_stim / p_rt_stim
stimulus_plot

save_plot_pair(stimulus_plot, "stimulus_plot", width = 8, height = 6)


# ------------------
# Individual differences: participant accuracy and mean RT by block
# ------------------

dat_id <- dat %>%
  mutate(
    block_simple = factor_block_simple(block),
    participant_id = factor(participant_id)
  ) %>%
  filter(!is.na(participant_id), !is.na(block_simple))

id_acc_summary <- dat_id %>%
  filter(!is.na(correct)) %>%
  restrict_calibration_trials(
    participant_col = participant_id,
    block_col = block_simple,
    trial_col = trial_idx
  ) %>%
  group_by(participant_id, block_simple) %>%
  summarise(
    acc = mean(correct, na.rm = TRUE),
    .groups = "drop"
  )

id_rt_summary <- dat_id %>%
  filter(!is.na(rt_s)) %>%
  restrict_calibration_trials(
    participant_col = participant_id,
    block_col = block_simple,
    trial_col = trial_idx
  ) %>%
  group_by(participant_id, block_simple) %>%
  summarise(
    mean_rt = mean(rt_s, na.rm = TRUE),
    .groups = "drop"
  )

# use Calibration accuracy only to define participant order
participant_order <- id_acc_summary %>%
  filter(block_simple == "Calibration") %>%
  arrange(acc) %>%
  pull(participant_id)

id_acc_summary <- id_acc_summary %>%
  mutate(
    participant_id = factor(participant_id, levels = participant_order)
  )

id_rt_summary <- id_rt_summary %>%
  mutate(
    participant_id = factor(participant_id, levels = participant_order)
  )

# orange 0.80 line in all panels
acc_base_lines <- tibble(
  block_simple = factor_block_simple(BLOCK_LEVELS),
  xint = 0.80
)

# purple automation-target lines in auto panels
acc_auto_lines <- tibble(
  block_simple = factor_block_simple(unname(AUTO_BLOCK_LABELS)),
  xint = c(0.95, 0.65)
)

# empirical mean accuracy line per block
acc_mean_lines <- id_acc_summary %>%
  group_by(block_simple) %>%
  summarise(
    xint = mean(acc, na.rm = TRUE),
    .groups = "drop"
  )

# empirical mean RT line per block
rt_mean_lines <- id_rt_summary %>%
  group_by(block_simple) %>%
  summarise(
    xint = mean(mean_rt, na.rm = TRUE),
    .groups = "drop"
  )

acc_id_xlim <- c(0.6, 1.0)
rt_id_xlim  <- get_axis_limits(c(id_rt_summary$mean_rt, rt_mean_lines$xint))

p_acc_id <- ggplot(
  id_acc_summary,
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
  id_rt_summary,
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

save_plot_pair(
  individual_differences_plot,
  "individual_differences_plot",
  width = 9,
  height = 6
)

# ------------------
# Calibration vs Manual participant accuracy difference
# ------------------

calib_manual_acc <- dat %>%
  mutate(
    participant_id = as.character(participant_id),
    block_simple = factor(block, levels = c("Calibration", "Manual"))
  ) %>%
  filter(!is.na(correct), !is.na(block_simple)) %>%
  restrict_calibration_trials(
    participant_col = participant_id,
    block_col = block_simple,
    trial_col = trial_idx
  ) %>%
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

block_order_codes <- trial_dat_raw %>%
  mutate(
    participant_id = as.character(participant_id),
    block = as.character(block)
  ) %>%
  filter(block %in% c("MANUAL", "AUTOMATION1", "AUTOMATION2"), !is.na(block_idx)) %>%
  distinct(participant_id, block, block_idx) %>%
  group_by(participant_id) %>%
  arrange(block_idx, .by_group = TRUE) %>%
  mutate(
    block_code = recode(
      block,
      "MANUAL" = "M",
      "AUTOMATION1" = "H",
      "AUTOMATION2" = "L"
    )
  ) %>%
  summarise(
    block_order_code = paste(block_code, collapse = "/"),
    .groups = "drop"
  )

calib_manual_acc <- calib_manual_acc %>%
  left_join(block_order_codes, by = "participant_id") %>%
  mutate(
    block_order_code = coalesce(block_order_code, "NA")
  )

calib_manual_n <- nrow(calib_manual_acc)

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

calib_manual_diff_range <- range(calib_manual_acc$accuracy_diff, na.rm = TRUE)
calib_manual_label_offset <- pmax(0.02, 0.08 * diff(calib_manual_diff_range))

calib_manual_acc <- calib_manual_acc %>%
  mutate(
    block_order_label_y = if_else(
      accuracy_diff > 0,
      accuracy_diff + calib_manual_label_offset,
      calib_manual_label_offset
    ),
    block_order_label_y = pmax(block_order_label_y, 0)
  )

calib_manual_diff_limits <- get_axis_limits(
  c(
    calib_manual_acc$accuracy_diff,
    calib_manual_acc$block_order_label_y,
    0,
    min(calib_manual_acc$accuracy_diff, na.rm = TRUE) -
      pmax(0.03, 0.10 * diff(calib_manual_diff_range))
  ),
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
  geom_text(
    aes(
      x = reorder(factor(participant_id), accuracy_diff),
      y = block_order_label_y,
      label = block_order_code
    ),
    inherit.aes = FALSE,
    angle = 90,
    hjust = 0.5,
    vjust = 0,
    size = 2.8
  ) +
  annotate(
    "text",
    x = 1.15,
    y = Inf,
    label = paste0("N = ", calib_manual_n),
    hjust = 0,
    vjust = 1.2,
    size = 3.5
  ) +
  scale_fill_manual(values = participant_palette, guide = "none") +
  labs(
    x = "Participant",
    y = "Manual - Calibration accuracy",
    title = "Participant accuracy difference"
  ) +
  coord_cartesian(ylim = calib_manual_diff_limits, clip = "off") +
  theme_classic() +
  theme(
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)
  )

calib_manual_plot <- p_calib_manual_acc / p_calib_manual_diff
calib_manual_plot

save_plot_pair(calib_manual_plot, "calibration_manual_accuracy_plot", width = 7, height = 7)

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

# ------------------
# Aid accuracy discrepancy vs trust ratings (automation blocks only)
# ------------------

wrap_text <- function(x, width = 42) {
  vapply(
    x,
    function(txt) paste(strwrap(txt, width = width), collapse = "\n"),
    character(1)
  )
}

safe_cor_summary <- function(x, y) {
  keep <- complete.cases(x, y)
  x <- x[keep]
  y <- y[keep]
  
  if (length(x) < 3 || length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(tibble(
      n = length(x),
      correlation = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      p_value = NA_real_
    ))
  }
  
  cor_out <- suppressWarnings(cor.test(x, y, method = "pearson"))
  
  tibble(
    n = length(x),
    correlation = unname(cor_out$estimate),
    conf_low = unname(cor_out$conf.int[1]),
    conf_high = unname(cor_out$conf.int[2]),
    p_value = cor_out$p.value
  )
}

empirical_aid_acc <- trial_dat_raw %>%
  mutate(
    participant_id = as.character(participant_id),
    block = as.character(block),
    aid_correct_num = case_when(
      aid_correct %in% c(TRUE, 1, "1", "TRUE", "True", "true") ~ 1,
      aid_correct %in% c(FALSE, 0, "0", "FALSE", "False", "false") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(block %in% names(AUTO_BLOCK_LABELS), !is.na(aid_correct_num)) %>%
  group_by(participant_id, block) %>%
  summarise(
    empirical_aid_accuracy = mean(aid_correct_num, na.rm = TRUE),
    n_trials = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(
    block_label = factor_auto_block(block)
  )

self_rated_aid_acc <- slider_dat_raw %>%
  mutate(
    participant_id = as.character(participant_id),
    block = as.character(block),
    response = as.numeric(response)
  ) %>%
  filter(
    block %in% names(AUTO_BLOCK_LABELS),
    slider_key == "perc_auto_correct",
    !is.na(response)
  ) %>%
  group_by(participant_id, block) %>%
  summarise(
    self_rated_aid_accuracy = mean(response, na.rm = TRUE) / 100,
    .groups = "drop"
  ) %>%
  mutate(
    block_label = factor_auto_block(block)
  )

aid_accuracy_diff <- empirical_aid_acc %>%
  inner_join(
    self_rated_aid_acc,
    by = c("participant_id", "block", "block_label")
  ) %>%
  mutate(
    aid_accuracy_difference = empirical_aid_accuracy - self_rated_aid_accuracy
  ) %>%
  arrange(block_label, participant_id)

trust_ratings <- postblock_dat_raw %>%
  mutate(
    participant_id = as.character(participant_id),
    block = as.character(block),
    question_idx = as.integer(question_idx),
    response = as.numeric(response)
  ) %>%
  filter(block %in% names(AUTO_BLOCK_LABELS), !is.na(question_idx), !is.na(response)) %>%
  group_by(participant_id, block, question_idx, question) %>%
  summarise(
    trust_rating = mean(response, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    block_label = factor_auto_block(block),
    question_label = paste0("Q", question_idx, ". ", wrap_text(question))
  )

question_levels <- trust_ratings %>%
  distinct(question_idx, question_label) %>%
  arrange(question_idx) %>%
  pull(question_label)

aid_accuracy_trust_dat <- aid_accuracy_diff %>%
  select(
    participant_id,
    block,
    block_label,
    empirical_aid_accuracy,
    self_rated_aid_accuracy,
    aid_accuracy_difference
  ) %>%
  inner_join(
    trust_ratings %>%
      select(
        participant_id,
        block,
        block_label,
        question_idx,
        question,
        question_label,
        trust_rating
      ),
    by = c("participant_id", "block", "block_label")
  )

aid_accuracy_trust_cor <- aid_accuracy_trust_dat %>%
  group_by(block_label, question_idx, question, question_label) %>%
  group_modify(~ safe_cor_summary(
    x = .x$aid_accuracy_difference,
    y = .x$trust_rating
  )) %>%
  ungroup() %>%
  mutate(
    question_label = factor(question_label, levels = rev(question_levels))
  ) %>%
  arrange(block_label, question_idx)

p_aid_accuracy_trust_cor <- ggplot(
  aid_accuracy_trust_cor,
  aes(
    x = question_label,
    y = correlation,
    ymin = conf_low,
    ymax = conf_high
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(linewidth = 0.45, width = 0.15) +
  geom_point(size = 2.3) +
  coord_flip(ylim = c(-1, 1)) +
  facet_wrap(~ block_label, ncol = 1) +
  labs(
    x = NULL,
    y = "Pearson r",
    title = "Trust ratings vs aid-accuracy discrepancy",
    subtitle = "Discrepancy = empirical aid accuracy - self-rated aid accuracy"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(face = "bold")
  )

p_aid_accuracy_trust_cor

write_csv(
  aid_accuracy_diff,
  "plots/aid_accuracy_discrepancy_by_participant.csv"
)

write_csv(
  aid_accuracy_trust_cor,
  "plots/aid_accuracy_trust_correlations.csv"
)

save_plot_pair(
  p_aid_accuracy_trust_cor,
  "aid_accuracy_trust_correlations_plot",
  width = 10,
  height = 8
)

# ------------------
# Pooled aid accuracy discrepancy vs pooled trust
# ------------------

pooled_aid_accuracy_diff <- aid_accuracy_diff %>%
  group_by(participant_id) %>%
  summarise(
    pooled_empirical_aid_accuracy = mean(empirical_aid_accuracy, na.rm = TRUE),
    pooled_self_rated_aid_accuracy = mean(self_rated_aid_accuracy, na.rm = TRUE),
    pooled_aid_accuracy_difference = mean(aid_accuracy_difference, na.rm = TRUE),
    n_blocks = dplyr::n(),
    .groups = "drop"
  )

pooled_trust_ratings <- trust_ratings %>%
  group_by(participant_id) %>%
  summarise(
    pooled_trust_rating = mean(trust_rating, na.rm = TRUE),
    n_trust_ratings = dplyr::n(),
    .groups = "drop"
  )

pooled_aid_accuracy_trust_dat <- pooled_aid_accuracy_diff %>%
  inner_join(pooled_trust_ratings, by = "participant_id")

pooled_aid_accuracy_trust_cor <- safe_cor_summary(
  x = pooled_aid_accuracy_trust_dat$pooled_aid_accuracy_difference,
  y = pooled_aid_accuracy_trust_dat$pooled_trust_rating
) %>%
  mutate(
    analysis = "pooled_across_automation_blocks_and_trust_questions"
  )

pooled_cor_label <- pooled_aid_accuracy_trust_cor %>%
  transmute(
    label = paste0(
      "r = ", sprintf("%.2f", correlation),
      "\np = ", sprintf("%.3f", p_value),
      "\nn = ", n
    )
  ) %>%
  pull(label)

pooled_diff_xlim <- get_axis_limits(
  pooled_aid_accuracy_trust_dat$pooled_aid_accuracy_difference,
  pad_prop = 0.10
)

p_aid_accuracy_trust_pooled <- ggplot(
  pooled_aid_accuracy_trust_dat,
  aes(
    x = pooled_trust_rating,
    y = pooled_aid_accuracy_difference
  )
) +
  geom_vline(xintercept = 3, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2.6) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    linewidth = 0.8,
    colour = "black"
  ) +
  annotate(
    geom = "text",
    x = Inf,
    y = Inf,
    label = pooled_cor_label,
    hjust = 1.05,
    vjust = 1.3,
    size = 3.5
  ) +
  labs(
    x = "Pooled trust rating",
    y = "Pooled aid-accuracy discrepancy",
    title = "Pooled trust vs pooled aid-accuracy discrepancy",
    subtitle = paste(
      "Discrepancy is averaged across Automation 95% and Automation 65%;",
      "trust is averaged across all 6 trust items in both automation blocks"
    )
  ) +
  coord_cartesian(
    xlim = c(1, 5),
    ylim = pooled_diff_xlim,
    clip = "off"
  ) +
  theme_classic() +
  theme(
    plot.margin = margin(5.5, 20, 5.5, 5.5)
  )

p_aid_accuracy_trust_pooled

write_csv(
  pooled_aid_accuracy_trust_dat,
  "plots/aid_accuracy_trust_pooled_by_participant.csv"
)

write_csv(
  pooled_aid_accuracy_trust_cor,
  "plots/aid_accuracy_trust_pooled_correlation_summary.csv"
)

save_plot_pair(
  p_aid_accuracy_trust_pooled,
  "aid_accuracy_trust_pooled_plot",
  width = 8,
  height = 6
)
