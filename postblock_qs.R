# Clear workspace
rm(list = ls())

# Load libraries
library("dplyr")
library("readr")
library("tidyr")
library("patchwork")
library("ggplot2")
library("forcats")

BLOCK_RAW_LEVELS <- c("CALIBRATION", "MANUAL", "AUTOMATION", "AUTOMATION1", "AUTOMATION2")
BLOCK_LABELS <- c("Calibration", "Manual", "Automation", "Auto95", "Auto65")
BLOCK_DEADLINE_LEVELS <- c(
  "Calibration",
  "Manual 3s",
  "Manual 6s",
  "Automation 3s",
  "Automation 6s",
  "Auto95 3s",
  "Auto95 6s",
  "Auto65 3s",
  "Auto65 6s",
  "Manual",
  "Automation",
  "Auto95",
  "Auto65"
)
RELIABILITY_LEVELS <- c("high", "low", "none")

ensure_deadline_columns <- function(data) {
  if (!"trial_deadline_s" %in% names(data)) {
    data$trial_deadline_s <- NA_real_
  }
  if (!"automation_reliability_group" %in% names(data)) {
    data$automation_reliability_group <- NA_character_
  }
  if (!"aid_accuracy_setting" %in% names(data)) {
    data$aid_accuracy_setting <- NA_real_
  }
  data
}

derive_reliability_group <- function(data) {
  data %>%
    mutate(
      automation_reliability_group = case_when(
        !is.na(automation_reliability_group) & automation_reliability_group != "" ~
          as.character(automation_reliability_group),
        as.character(block) == "AUTOMATION1" ~ "high",
        as.character(block) == "AUTOMATION2" ~ "low",
        suppressWarnings(as.numeric(aid_accuracy_setting)) >= 0.90 ~ "high",
        suppressWarnings(as.numeric(aid_accuracy_setting)) < 0.90 &
          !is.na(suppressWarnings(as.numeric(aid_accuracy_setting))) ~ "low",
        TRUE ~ "none"
      )
    )
}

factor_reliability_group <- function(x) {
  factor(x, levels = RELIABILITY_LEVELS)
}

deadline_suffix <- function(block_label, deadline_s) {
  deadline_num <- suppressWarnings(as.numeric(deadline_s))
  suffix <- ifelse(
    is.na(deadline_num) | block_label == "Calibration",
    "",
    ifelse(
      deadline_num == floor(deadline_num),
      paste0(" ", as.integer(deadline_num), "s"),
      paste0(" ", deadline_num, "s")
    )
  )
  suffix
}

make_block_deadline <- function(block, deadline_s) {
  block_label <- factor(block, levels = BLOCK_RAW_LEVELS, labels = BLOCK_LABELS)
  block_deadline <- paste0(as.character(block_label), deadline_suffix(block_label, deadline_s))
  factor(block_deadline, levels = BLOCK_DEADLINE_LEVELS)
}

# ------------------
# Load data
# ------------------
dat <- read_csv("data/data_virus_postblock_all.csv", show_col_types = FALSE)
dat <- dat %>%
  ensure_deadline_columns() %>%
  derive_reliability_group()
str(dat)

# ------------------
# Questionnaire responses by block
# Across-participant mean with Morey-Cousineau within-subject SEs
# ------------------
dat_q <- dat %>%
  mutate(
    participant_id = factor(participant_id),
    automation_reliability_group = factor_reliability_group(automation_reliability_group),
    block_deadline = make_block_deadline(block, trial_deadline_s),
    question = factor(question, levels = unique(question))
  ) %>%
  filter(!is.na(participant_id), !is.na(block_deadline), !is.na(question), !is.na(response))

subj_q_summary <- dat_q %>%
  group_by(participant_id, automation_reliability_group, block_deadline, question) %>%
  summarise(
    response = mean(response, na.rm = TRUE),
    .groups = "drop"
  )

get_morey_cf <- function(data, subject_col, within_cols, dv_col) {
  
  subj_means <- data %>%
    group_by(.data[[subject_col]]) %>%
    summarise(
      subj_mean = mean(.data[[dv_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  grand_mean <- mean(data[[dv_col]], na.rm = TRUE)
  
  data_norm <- data %>%
    left_join(subj_means, by = setNames(subject_col, subject_col)) %>%
    mutate(
      dv_norm = .data[[dv_col]] - subj_mean + grand_mean
    )
  
  n_within <- prod(sapply(within_cols, function(v) dplyr::n_distinct(data[[v]])))
  morey_correction <- sqrt(n_within / (n_within - 1))
  
  out <- data_norm %>%
    group_by(across(all_of(within_cols))) %>%
    summarise(
      mean = mean(.data[[dv_col]], na.rm = TRUE),
      sd_norm = sd(dv_norm, na.rm = TRUE),
      n = dplyr::n_distinct(.data[[subject_col]]),
      se = (sd_norm / sqrt(n)) * morey_correction,
      .groups = "drop"
    )
  
  out
}

q_plot_dat <- get_morey_cf(
  data = subj_q_summary,
  subject_col = "participant_id",
  within_cols = c("automation_reliability_group", "block_deadline", "question"),
  dv_col = "response"
)

p_q <- ggplot(q_plot_dat, aes(x = block_deadline, y = mean, group = 1)) +
  geom_hline(yintercept = 3, linetype = "dashed") +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.12) +
  facet_grid(automation_reliability_group ~ question) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    x = "Block and deadline",
    y = "Mean response",
    title = "Questionnaire responses by block and deadline",
    subtitle = "Error bars are Morey-Cousineau within-subject SEs"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

p_q

ggsave(
  filename = "plots/postblock_qs_plot.pdf",
  plot = p_q,
  width = 11,
  height = 9
)

ggsave(
  filename = "plots/postblock_qs_plot.png",
  plot = p_q,
  width = 11,
  height = 9,
  dpi = 300
)

# ------------------
# Load slider data
# ------------------
dat_slider_raw <- read_csv("data/data_virus_sliders_all.csv", show_col_types = FALSE)
dat_slider_raw <- dat_slider_raw %>%
  ensure_deadline_columns() %>%
  derive_reliability_group()
str(dat_slider_raw)

# ------------------
# Prepare slider data
# ------------------
dat_slider <- dat_slider_raw %>%
  mutate(
    participant_id = factor(participant_id),
    automation_reliability_group = factor_reliability_group(automation_reliability_group),
    block_deadline = make_block_deadline(block, trial_deadline_s),
    question_key = factor(
      question_key,
      levels = c("perc_self_correct", "perc_auto_correct"),
      labels = c("Self rated accuracy", "Aid rated accuracy")
    )
  ) %>%
  filter(
    !is.na(participant_id),
    !is.na(block_deadline),
    !is.na(question_key),
    !is.na(response_percent)
  )

# If there is ever more than one row per participant/block/question_key,
# average within participant first
subj_slider_summary <- dat_slider %>%
  group_by(participant_id, automation_reliability_group, block_deadline, question_key) %>%
  summarise(
    response_percent = mean(response_percent, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------
# Morey-Cousineau summary
# ------------------
slider_plot_dat <- get_morey_cf(
  data = subj_slider_summary,
  subject_col = "participant_id",
  within_cols = c("automation_reliability_group", "block_deadline", "question_key"),
  dv_col = "response_percent"
)

# ------------------
# Plot
# ------------------
p_slider <- ggplot(
  slider_plot_dat,
  aes(x = block_deadline, y = mean, group = 1)
) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.12) +
  facet_grid(automation_reliability_group ~ question_key) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  labs(
    x = "Block and deadline",
    y = "Mean slider response (%)",
    title = "Slider responses by block and deadline",
    subtitle = "Error bars are Morey-Cousineau within-subject SEs"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

p_slider

ggsave(
  filename = "plots/postblock_sliders_plot.pdf",
  plot = p_slider,
  width = 7,
  height = 6
)

ggsave(
  filename = "plots/postblock_sliders_plot.png",
  plot = p_slider,
  width = 7,
  height = 6,
  dpi = 300
)
