# Clear workspace
rm(list = ls())

# Load libraries
library("dplyr")
library("readr")
library("tidyr")
library("patchwork")
library("ggplot2")
library("forcats")

# ------------------
# Load data
# ------------------
dat <- read_csv("data/data_virus_postblock_all.csv", show_col_types = FALSE)
str(dat)

# ------------------
# Questionnaire responses by block
# Across-participant mean with Morey-Cousineau within-subject SEs
# ------------------
dat_q <- dat %>%
  mutate(
    participant_id = factor(participant_id),
    block = factor(
      block,
      levels = c("CALIBRATION", "MANUAL", "AUTOMATION1", "AUTOMATION2"),
      labels = c("Calibration", "Manual", "Auto95", "Auto65")
    ),
    question = factor(question, levels = unique(question))
  ) %>%
  filter(!is.na(participant_id), !is.na(block), !is.na(question), !is.na(response))

subj_q_summary <- dat_q %>%
  group_by(participant_id, block, question) %>%
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
  within_cols = c("block", "question"),
  dv_col = "response"
)

p_q <- ggplot(q_plot_dat, aes(x = block, y = mean, group = 1)) +
  geom_hline(yintercept = 3, linetype = "dashed") +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.12) +
  facet_wrap(~ question, ncol = 2) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    x = "Block",
    y = "Mean response",
    title = "Questionnaire responses by block",
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
str(dat_slider_raw)

# ------------------
# Prepare slider data
# ------------------
dat_slider <- dat_slider_raw %>%
  mutate(
    participant_id = factor(participant_id),
    block = factor(
      block,
      levels = c("CALIBRATION", "MANUAL", "AUTOMATION1", "AUTOMATION2"),
      labels = c("Calibration", "Manual", "Auto95", "Auto65")
    ),
    question_key = factor(
      question_key,
      levels = c("perc_self_correct", "perc_auto_correct"),
      labels = c("Self rated accuracy", "Aid rated accuracy")
    )
  ) %>%
  filter(
    !is.na(participant_id),
    !is.na(block),
    !is.na(question_key),
    !is.na(response_percent)
  )

# If there is ever more than one row per participant/block/question_key,
# average within participant first
subj_slider_summary <- dat_slider %>%
  group_by(participant_id, block, question_key) %>%
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
  within_cols = c("block", "question_key"),
  dv_col = "response_percent"
)

# ------------------
# Plot
# ------------------
p_slider <- ggplot(
  slider_plot_dat,
  aes(x = block, y = mean, group = 1)
) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.12) +
  facet_wrap(~ question_key, ncol = 1) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  labs(
    x = "Block",
    y = "Mean slider response (%)",
    title = "Slider responses by block",
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
