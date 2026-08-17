# Plot participant mean accuracy across calibration and the two manual blocks.
#
# Usage:
#   Rscript plot_calibration_manual_accuracy.R [input_csv] [output_dir] \
#     [output_stem] [plot_title]

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tidyr)
})

fontconfig_cache <- file.path(tempdir(), "fontconfig-cache")
dir.create(fontconfig_cache, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(XDG_CACHE_HOME = fontconfig_cache)

args <- commandArgs(trailingOnly = TRUE)

INPUT_FILE <- if (length(args) >= 1) {
  args[[1]]
} else {
  "data/data_virus_all.csv"
}
OUTPUT_DIR <- if (length(args) >= 2) {
  args[[2]]
} else {
  "plots/semester2_2026_data"
}
OUTPUT_STEM <- if (length(args) >= 3) {
  args[[3]]
} else {
  "calibration_manual_pre_post_accuracy"
}
PLOT_TITLE <- if (length(args) >= 4) {
  args[[4]]
} else {
  "Calibration vs Manual pre/post accuracy (Dynamic reliability task)"
}

CALIB_SUMMARY_LAST_N <- 150
CONDITION_CODES <- c("CAL", "MAN_PRE", "MAN_POST")
CONDITION_LABELS <- c(
  "CAL" = "Calibration",
  "MAN_PRE" = "Manual pre",
  "MAN_POST" = "Manual post"
)
CONDITION_NAMES <- c(
  "CAL" = "calibration",
  "MAN_PRE" = "manual_pre",
  "MAN_POST" = "manual_post"
)
TARGET_GROUPS <- c("CAL65", "CAL90")
TARGET_ACCURACIES <- c("CAL65" = 0.65, "CAL90" = 0.90)
ANNOTATION_TARGET_BAND <- 0.10
PARTICIPANT_LABEL_SIZE <- 2
PARTICIPANT_LABEL_NUDGE_X <- 0.05
GROUP_MEAN_LABEL_SIZE <- 3
GROUP_MEAN_LABEL_NUDGE_Y <- 0.008
PLOT_WIDTH_IN <- 10
PLOT_HEIGHT_IN <- 7.875
TARGET_GROUP_COLOURS <- c(
  "CAL65" = "#0072B2",
  "CAL90" = "#D55E00"
)
TARGET_GROUP_SHAPES <- c(
  "CAL65" = 16,
  "CAL90" = 17
)
REQUIRED_COLUMNS <- c(
  "participant_id",
  "block",
  "calibration_target_group",
  "calibration_target_accuracy",
  "manual_segment",
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
    participant_id = trimws(as.character(participant_id)),
    block = toupper(trimws(as.character(block))),
    calibration_target_group = toupper(
      trimws(as.character(calibration_target_group))
    ),
    calibration_target_accuracy = suppressWarnings(
      as.numeric(calibration_target_accuracy)
    ),
    manual_segment = toupper(trimws(as.character(manual_segment))),
    trial = suppressWarnings(as.integer(trial)),
    correct_num = if_else(correct_text %in% c("TRUE", "1"), 1, 0),
    condition_code = case_when(
      block == "CALIBRATION" ~ "CAL",
      block == "MANUAL" & manual_segment == "PRE_AUTOMATION" ~ "MAN_PRE",
      block == "MANUAL" & manual_segment == "POST_AUTOMATION" ~ "MAN_POST",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(condition_code %in% CONDITION_CODES)

if (nrow(accuracy_dat) == 0) {
  stop(
    "No Calibration, Manual pre, or Manual post trials were found in: ",
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

if (any(is.na(accuracy_dat$calibration_target_group)) ||
    any(accuracy_dat$calibration_target_group == "NA") ||
    any(accuracy_dat$calibration_target_group == "")) {
  stop(
    "Requested-condition trials contain missing calibration target groups.",
    call. = FALSE
  )
}

unsupported_target_groups <- setdiff(
  unique(accuracy_dat$calibration_target_group),
  TARGET_GROUPS
)

if (length(unsupported_target_groups) > 0) {
  stop(
    "Requested-condition trials contain unsupported calibration target groups: ",
    paste(unsupported_target_groups, collapse = ", "),
    call. = FALSE
  )
}

if (any(is.na(accuracy_dat$calibration_target_accuracy))) {
  stop(
    "Requested-condition trials contain missing or non-numeric calibration targets.",
    call. = FALSE
  )
}

participant_metadata <- accuracy_dat %>%
  distinct(
    participant_id,
    calibration_target_group,
    calibration_target_accuracy
  )

inconsistent_metadata <- participant_metadata %>%
  count(participant_id, name = "n_metadata_rows") %>%
  filter(n_metadata_rows != 1)

if (nrow(inconsistent_metadata) > 0) {
  stop(
    "Calibration target metadata is inconsistent for participant(s): ",
    paste(inconsistent_metadata$participant_id, collapse = ", "),
    call. = FALSE
  )
}

target_mismatches <- participant_metadata %>%
  mutate(
    expected_target = unname(
      TARGET_ACCURACIES[calibration_target_group]
    )
  ) %>%
  filter(abs(calibration_target_accuracy - expected_target) > 1e-8)

if (nrow(target_mismatches) > 0) {
  stop(
    "Calibration target group/accuracy mismatch for participant(s): ",
    paste(target_mismatches$participant_id, collapse = ", "),
    call. = FALSE
  )
}

participant_ids <- unique(accuracy_dat$participant_id)
condition_coverage <- expand_grid(
  participant_id = participant_ids,
  condition_code = CONDITION_CODES
)
missing_conditions <- condition_coverage %>%
  anti_join(
    accuracy_dat %>% distinct(participant_id, condition_code),
    by = c("participant_id", "condition_code")
  )

if (nrow(missing_conditions) > 0) {
  missing_text <- missing_conditions %>%
    transmute(missing = paste0(participant_id, ":", condition_code)) %>%
    pull(missing) %>%
    paste(collapse = ", ")

  stop(
    "Every participant must contain Calibration, Manual pre, and Manual post ",
    "trials. Missing: ",
    missing_text,
    call. = FALSE
  )
}

participant_order <- tibble(participant_id = participant_ids) %>%
  mutate(participant_num = suppressWarnings(as.numeric(participant_id))) %>%
  arrange(is.na(participant_num), participant_num, participant_id) %>%
  pull(participant_id)

participant_condition_means <- accuracy_dat %>%
  group_by(
    participant_id,
    calibration_target_group,
    calibration_target_accuracy,
    condition_code
  ) %>%
  arrange(trial, .by_group = TRUE) %>%
  filter(
    condition_code != "CAL" |
      row_number() > pmax(n() - CALIB_SUMMARY_LAST_N, 0)
  ) %>%
  summarise(
    accuracy = mean(correct_num),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  mutate(
    participant_id = factor(participant_id, levels = participant_order),
    calibration_target_group = factor(
      calibration_target_group,
      levels = TARGET_GROUPS
    ),
    condition_code = factor(condition_code, levels = CONDITION_CODES),
    condition_label = factor(
      CONDITION_LABELS[as.character(condition_code)],
      levels = unname(CONDITION_LABELS[CONDITION_CODES])
    )
  ) %>%
  arrange(participant_id, condition_code)

participant_means_wide <- participant_condition_means %>%
  mutate(
    participant_id = as.character(participant_id),
    calibration_target_group = as.character(calibration_target_group),
    condition_name = CONDITION_NAMES[as.character(condition_code)]
  ) %>%
  select(
    participant_id,
    calibration_target_group,
    calibration_target_accuracy,
    condition_name,
    accuracy,
    n_trials
  ) %>%
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
    calibration_target_group,
    calibration_target_accuracy,
    accuracy_calibration,
    n_trials_calibration,
    accuracy_manual_pre,
    n_trials_manual_pre,
    accuracy_manual_post,
    n_trials_manual_post
  )

participant_palette <- setNames(
  scales::hue_pal()(length(participant_order)),
  participant_order
)

group_labels <- participant_condition_means %>%
  distinct(participant_id, calibration_target_group) %>%
  count(calibration_target_group, name = "n") %>%
  mutate(
    label = paste0(calibration_target_group, " (n = ", n, ")")
  ) %>%
  { setNames(.$label, as.character(.$calibration_target_group)) }
group_breaks <- intersect(TARGET_GROUPS, names(group_labels))

group_mean_labels <- participant_condition_means %>%
  group_by(calibration_target_group, condition_label) %>%
  summarise(mean_accuracy = mean(accuracy), .groups = "drop") %>%
  mutate(mean_label = sprintf("%.1f%%", mean_accuracy * 100))

reference_lines <- tibble(
  yint = unname(TARGET_ACCURACIES),
  label = c("CAL65 target", "CAL90 target")
)

participant_labels <- participant_condition_means %>%
  filter(condition_code %in% c("MAN_PRE", "MAN_POST")) %>%
  mutate(
    target_deviation = abs(accuracy - calibration_target_accuracy)
  ) %>%
  filter(target_deviation > ANNOTATION_TARGET_BAND + 1e-10) %>%
  group_by(condition_code, condition_label, accuracy) %>%
  summarise(
    participant_label = paste(
      sort(as.integer(as.character(participant_id))),
      collapse = ", "
    ),
    .groups = "drop"
  )

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
  geom_label(
    data = reference_lines,
    aes(x = "Manual post", y = yint, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    nudge_x = 0.48,
    size = 3.2,
    fill = "white",
    linewidth = 0,
    label.padding = grid::unit(0.08, "lines")
  ) +
  geom_line(colour = "grey70", linewidth = 0.7) +
  geom_point(
    aes(fill = participant_id),
    shape = 21,
    colour = "white",
    stroke = 0.35,
    size = 2.8,
    show.legend = FALSE
  ) +
  geom_text(
    data = participant_labels,
    aes(
      x = condition_label,
      y = accuracy,
      label = participant_label
    ),
    inherit.aes = FALSE,
    hjust = 0,
    nudge_x = PARTICIPANT_LABEL_NUDGE_X,
    size = PARTICIPANT_LABEL_SIZE,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = participant_palette) +
  stat_summary(
    aes(
      group = calibration_target_group,
      colour = calibration_target_group
    ),
    fun = mean,
    geom = "line",
    linewidth = 1.2
  ) +
  stat_summary(
    aes(
      group = calibration_target_group,
      colour = calibration_target_group,
      shape = calibration_target_group
    ),
    fun = mean,
    geom = "point",
    size = 3.4
  ) +
  geom_label(
    data = group_mean_labels,
    aes(
      x = condition_label,
      y = mean_accuracy,
      label = mean_label,
      colour = calibration_target_group
    ),
    inherit.aes = FALSE,
    nudge_y = GROUP_MEAN_LABEL_NUDGE_Y,
    vjust = 0,
    size = GROUP_MEAN_LABEL_SIZE,
    fontface = "bold",
    fill = "white",
    linewidth = 0,
    label.padding = grid::unit(0.08, "lines"),
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = TARGET_GROUP_COLOURS,
    breaks = group_breaks,
    labels = group_labels[group_breaks],
    name = "Calibration target"
  ) +
  scale_shape_manual(
    values = TARGET_GROUP_SHAPES,
    breaks = group_breaks,
    labels = group_labels[group_breaks],
    name = "Calibration target"
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
    plot.margin = margin(5.5, 45, 5.5, 5.5),
    legend.position = "bottom"
  )

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

pdf_path <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, "_plot.pdf"))
png_path <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, "_plot.png"))
csv_path <- file.path(
  OUTPUT_DIR,
  paste0(OUTPUT_STEM, "_by_participant.csv")
)

ggsave(
  filename = pdf_path,
  plot = accuracy_plot,
  device = cairo_pdf,
  width = PLOT_WIDTH_IN,
  height = PLOT_HEIGHT_IN,
  units = "in"
)
ggsave(
  filename = png_path,
  plot = accuracy_plot,
  width = PLOT_WIDTH_IN,
  height = PLOT_HEIGHT_IN,
  units = "in",
  dpi = 300
)
write_csv(participant_means_wide, csv_path)

message("Participant means:")
print(participant_means_wide)
message("Wrote outputs:")
writeLines(c(pdf_path, png_path, csv_path))
