# Plot participant mean accuracy from calibration through the HP and LP manual blocks.
#
# Usage:
#   Rscript plot_calibration_manual_accuracy.R [input_csv] [output_dir] [output_stem] [plot_title]

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)

INPUT_FILE <- if (length(args) >= 1) {
  args[[1]]
} else {
  "data/semester2_2026_data_pilot/data_virus_all.csv"
}
OUTPUT_DIR <- if (length(args) >= 2) {
  args[[2]]
} else {
  "plots/semester2_2026_data_pilot"
}
OUTPUT_STEM <- if (length(args) >= 3) {
  args[[3]]
} else {
  "calibration_manual_hp_lp_accuracy"
}
PLOT_TITLE <- if (length(args) >= 4) {
  args[[4]]
} else {
  "Calibration vs Manual HP/LP accuracy (Time-pressure task)"
}

CALIB_SUMMARY_LAST_N <- 150
CALIBRATION_TARGET_ACCURACY <- 0.77
CONDITION_CODES <- c("CAL_LP", "M_HP", "M_LP")
CONDITION_LABELS <- c(
  "CAL_LP" = "Calibration",
  "M_HP" = "Manual HP",
  "M_LP" = "Manual LP"
)
CONDITION_NAMES <- c(
  "CAL_LP" = "calibration",
  "M_HP" = "manual_hp",
  "M_LP" = "manual_lp"
)
REQUIRED_COLUMNS <- c(
  "participant_id",
  "condition_deadline_code",
  "trial",
  "correct"
)

if (!file.exists(INPUT_FILE)) {
  stop("Input CSV does not exist: ", INPUT_FILE, call. = FALSE)
}

trial_dat <- read_csv(INPUT_FILE, show_col_types = FALSE)
missing_columns <- setdiff(REQUIRED_COLUMNS, names(trial_dat))

if (length(missing_columns) > 0) {
  stop(
    "Input CSV is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

correct_text <- toupper(trimws(as.character(trial_dat$correct)))
invalid_correct <- setdiff(
  unique(correct_text[!is.na(correct_text)]),
  c("TRUE", "FALSE", "1", "0")
)

if (length(invalid_correct) > 0) {
  stop(
    "The correct column contains unsupported values: ",
    paste(invalid_correct, collapse = ", "),
    call. = FALSE
  )
}

accuracy_dat <- trial_dat %>%
  transmute(
    participant_id = as.character(participant_id),
    condition_deadline_code = as.character(condition_deadline_code),
    trial = suppressWarnings(as.integer(trial)),
    correct_num = if_else(
      toupper(trimws(as.character(correct))) %in% c("TRUE", "1"),
      1,
      0
    )
  ) %>%
  filter(condition_deadline_code %in% CONDITION_CODES)

if (nrow(accuracy_dat) == 0) {
  stop(
    "No CAL_LP, M_HP, or M_LP trials were found in: ",
    INPUT_FILE,
    call. = FALSE
  )
}

if (any(is.na(accuracy_dat$participant_id)) ||
    any(accuracy_dat$participant_id == "")) {
  stop("Requested-condition trials contain missing participant IDs.", call. = FALSE)
}

if (any(is.na(accuracy_dat$trial))) {
  stop("Requested-condition trials contain non-numeric trial values.", call. = FALSE)
}

participant_ids <- unique(accuracy_dat$participant_id)
condition_coverage <- expand_grid(
  participant_id = participant_ids,
  condition_deadline_code = CONDITION_CODES
)
missing_conditions <- condition_coverage %>%
  anti_join(
    accuracy_dat %>%
      distinct(participant_id, condition_deadline_code),
    by = c("participant_id", "condition_deadline_code")
  )

if (nrow(missing_conditions) > 0) {
  missing_text <- missing_conditions %>%
    transmute(missing = paste0(participant_id, ":", condition_deadline_code)) %>%
    pull(missing) %>%
    paste(collapse = ", ")

  stop(
    "Every participant must contain CAL_LP, M_HP, and M_LP trials. Missing: ",
    missing_text,
    call. = FALSE
  )
}

participant_order <- tibble(participant_id = participant_ids) %>%
  mutate(participant_num = suppressWarnings(as.numeric(participant_id))) %>%
  arrange(is.na(participant_num), participant_num, participant_id) %>%
  pull(participant_id)

participant_condition_means <- accuracy_dat %>%
  group_by(participant_id, condition_deadline_code) %>%
  arrange(trial, .by_group = TRUE) %>%
  filter(
    condition_deadline_code != "CAL_LP" |
      row_number() > pmax(n() - CALIB_SUMMARY_LAST_N, 0)
  ) %>%
  summarise(
    accuracy = mean(correct_num),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  mutate(
    participant_id = factor(participant_id, levels = participant_order),
    condition_deadline_code = factor(
      condition_deadline_code,
      levels = CONDITION_CODES
    ),
    condition_label = factor(
      CONDITION_LABELS[as.character(condition_deadline_code)],
      levels = unname(CONDITION_LABELS[CONDITION_CODES])
    )
  ) %>%
  arrange(participant_id, condition_deadline_code)

participant_means_wide <- participant_condition_means %>%
  mutate(
    participant_id = as.character(participant_id),
    condition_name = CONDITION_NAMES[as.character(condition_deadline_code)]
  ) %>%
  select(participant_id, condition_name, accuracy, n_trials) %>%
  pivot_wider(
    names_from = condition_name,
    values_from = c(accuracy, n_trials),
    names_glue = "{.value}_{condition_name}"
  ) %>%
  mutate(
    participant_id = factor(participant_id, levels = participant_order)
  ) %>%
  arrange(participant_id) %>%
  mutate(participant_id = as.character(participant_id)) %>%
  select(
    participant_id,
    accuracy_calibration,
    n_trials_calibration,
    accuracy_manual_hp,
    n_trials_manual_hp,
    accuracy_manual_lp,
    n_trials_manual_lp
  )

participant_palette <- setNames(
  scales::hue_pal()(length(participant_order)),
  participant_order
)

reference_lines <- tibble(
  yint = c(0.95, CALIBRATION_TARGET_ACCURACY, 0.65),
  label = c("Aid high", "Calib. target", "Aid low")
)

point_labels <- participant_condition_means %>%
  filter(accuracy < 0.65 | accuracy > 0.95)

accuracy_plot <- ggplot(
  participant_condition_means,
  aes(
    x = condition_label,
    y = accuracy,
    group = participant_id
  )
) +
  geom_hline(
    data = reference_lines,
    aes(yintercept = yint),
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = reference_lines,
    aes(x = "Manual LP", y = yint, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = -0.25,
    nudge_x = 0.48,
    size = 3.2
  ) +
  geom_line(colour = "grey70", linewidth = 0.7) +
  geom_point(
    aes(colour = participant_id),
    size = 2.4,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(point_labels, condition_deadline_code == "CAL_LP"),
    aes(label = participant_id),
    hjust = 1,
    nudge_x = -0.03,
    size = 3,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(point_labels, condition_deadline_code == "M_HP"),
    aes(label = participant_id),
    vjust = -0.75,
    size = 3,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(point_labels, condition_deadline_code == "M_LP"),
    aes(label = participant_id),
    hjust = 0,
    nudge_x = 0.03,
    size = 3,
    show.legend = FALSE
  ) +
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
    title = PLOT_TITLE,
    subtitle = paste0(
      "Calibration mean uses the final ",
      CALIB_SUMMARY_LAST_N,
      " trials; manual means use all trials"
    )
  ) +
  coord_cartesian(ylim = c(0.50, 1.00), clip = "off") +
  theme_classic() +
  theme(
    plot.margin = margin(5.5, 45, 5.5, 5.5)
  )

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

pdf_path <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, "_plot.pdf"))
png_path <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, "_plot.png"))
csv_path <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, "_by_participant.csv"))

ggsave(
  filename = pdf_path,
  plot = accuracy_plot,
  width = 10,
  height = 5.5
)
ggsave(
  filename = png_path,
  plot = accuracy_plot,
  width = 10,
  height = 5.5,
  dpi = 300
)
write_csv(participant_means_wide, csv_path)

message("Participant means:")
print(participant_means_wide)
message("Wrote outputs:")
writeLines(c(pdf_path, png_path, csv_path))
