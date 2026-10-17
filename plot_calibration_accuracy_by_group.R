# Slide-sized calibration accuracy distribution, grouped by reliability assignment.
# Usage: Rscript plot_calibration_accuracy_by_group.R [input_csv] [output_dir] [output_stem] [layout]
# Layout: wide (default, 16 x 9 in) or compact (8 x 7 in, for text beside the plot).

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1) args[[1]] else
  "data/semester2_2026_data/data_virus_all.csv"
output_dir <- if (length(args) >= 2) args[[2]] else
  "plots/semester2_2026_data/group"
layout <- if (length(args) >= 4) args[[4]] else "wide"
if (!layout %in% c("wide", "compact")) stop("Layout must be 'wide' or 'compact'.")
compact <- layout == "compact"
output_stem <- if (length(args) >= 3) args[[3]] else paste0(
  "semester2_2026_group_calibration_accuracy_distribution",
  if (compact) "_compact" else ""
)

patterns <- c("HP95_LP65", "HP65_LP95")
pattern_labels <- c("95% HP / 65% LP", "65% HP / 95% LP")
pattern_colours <- c("HP95_LP65" = "#0072B2", "HP65_LP95" = "#D55E00")
summary_trials <- 150L
target_accuracy <- 0.77

trials <- read_csv(input_file, show_col_types = FALSE)
required <- c(
  "participant_id", "condition_deadline_code", "trial", "correct",
  "trial_deadline_s", "automation_reliability_pattern"
)
if (length(setdiff(required, names(trials)))) {
  stop("Input is missing columns: ", paste(setdiff(required, names(trials)), collapse = ", "))
}
if (anyNA(trials$participant_id)) stop("Participant IDs must not be missing.")

calibration <- trials %>%
  filter(condition_deadline_code == "CAL_LP") %>%
  mutate(correct_text = toupper(trimws(as.character(correct))))
if (!nrow(calibration)) stop("No CAL_LP trials found.")
if (length(setdiff(unique(trials$participant_id), unique(calibration$participant_id)))) {
  stop("Every participant must have calibration data.")
}
if (anyNA(calibration$trial_deadline_s) || any(calibration$trial_deadline_s != 3)) {
  stop("CAL_LP must use the 3 s deadline.")
}
if (any(!calibration$automation_reliability_pattern %in% patterns)) {
  stop("Calibration contains an unknown reliability assignment.")
}
if (any(!is.na(calibration$correct_text) &
        !calibration$correct_text %in% c("TRUE", "FALSE", "1", "0"))) {
  stop("Unsupported correctness values.")
}

coverage <- calibration %>%
  group_by(participant_id) %>%
  summarise(
    complete = identical(as.numeric(sort(trial)), as.numeric(seq_len(300))),
    n_patterns = n_distinct(automation_reliability_pattern),
    .groups = "drop"
  )
if (any(!coverage$complete | coverage$n_patterns != 1)) {
  stop("Each participant must have trials 1-300 exactly once and one reliability assignment.")
}

participant_accuracy <- calibration %>%
  group_by(participant_id, automation_reliability_pattern) %>%
  arrange(trial, .by_group = TRUE) %>%
  slice_tail(n = summary_trials) %>%
  summarise(
    # Match the existing plots: missed/incorrect responses count as incorrect.
    accuracy = mean(correct_text %in% c("TRUE", "1")),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  mutate(group_x = match(automation_reliability_pattern, patterns))

if (any(participant_accuracy$accuracy < 0.60)) {
  stop("Calibration accuracy is below 60%; widen the slide's accuracy axis before exporting.")
}

# Separate equal accuracies horizontally without changing their measured values.
# Sorting by participant makes the layout deterministic across repeated runs.
participant_accuracy <- participant_accuracy %>%
  group_by(automation_reliability_pattern, accuracy) %>%
  arrange(participant_id, .by_group = TRUE) %>%
  mutate(point_x = group_x + (row_number() - (n() + 1) / 2) * 0.045) %>%
  ungroup()

group_means <- participant_accuracy %>%
  group_by(automation_reliability_pattern, group_x) %>%
  summarise(n = n(), accuracy = mean(accuracy), .groups = "drop") %>%
  arrange(group_x) %>%
  mutate(mean_x = group_x + 0.28, label = sprintf("%.1f%%", 100 * accuracy))
if (!identical(group_means$automation_reliability_pattern, patterns)) {
  stop("Both reliability-assignment groups are required for this plot.")
}

reference_lines <- tibble(
  accuracy = c(0.65, target_accuracy, 0.95),
  label = c("Low-reliability aid: 65%", "Calibration target: 77%", "High-reliability aid: 95%"),
  label_vjust = c(-0.7, 1.4, -0.7)
)
if (compact) {
  reference_lines$label <- c("65% aid", "77% target", "95% aid")
}

calibration_plot <- ggplot() +
  geom_hline(yintercept = c(0.65, 0.95), colour = "grey65", linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = target_accuracy, colour = "grey35", linetype = "dashed", linewidth = 0.8) +
  geom_text(
    data = reference_lines,
    aes(x = 2.78, y = accuracy, label = label, vjust = label_vjust),
    hjust = 1, size = if (compact) 4.5 else 5.5, colour = "grey30"
  ) +
  geom_point(
    data = participant_accuracy,
    aes(x = point_x, y = accuracy, colour = automation_reliability_pattern),
    size = if (compact) 1.5 else 2.1,
    stroke = if (compact) 0.25 else 0.5, show.legend = FALSE
  ) +
  geom_point(
    data = group_means, aes(x = mean_x, y = accuracy), shape = 18,
    size = if (compact) 3.5 else 4.5
  ) +
  geom_text(
    data = group_means,
    aes(x = mean_x + 0.065, y = accuracy, label = label),
    hjust = 0, size = if (compact) 4.8 else 6.5, fontface = "bold"
  ) +
  scale_colour_manual(values = pattern_colours) +
  scale_x_continuous(
    breaks = 1:2,
    labels = paste0(pattern_labels, "\n(n = ", group_means$n, ")"),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    breaks = seq(0.60, 1, 0.05),
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = 0)
  ) +
  coord_cartesian(xlim = c(0.48, 2.83), ylim = c(0.60, 1)) +
  labs(
    title = if (compact) "Calibration accuracy" else
      "Calibration accuracy was similar across groups",
    subtitle = if (compact) "Dots: participants; diamonds: group means" else
      "Each dot represents one participant; diamonds show group means",
    x = if (compact) "Assigned aid reliability" else
      "Assigned aid reliability in subsequent blocks",
    y = "Calibration accuracy",
    caption = paste0(nrow(participant_accuracy), " participants; final ", summary_trials,
                     if (compact) " LP calibration trials." else
                       " low-pressure calibration trials.")
  ) +
  theme_classic(base_size = 20) +
  theme(
    plot.title = element_text(size = 27, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, colour = "grey35", margin = margin(b = 24)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(size = 17, hjust = 0, colour = "grey35", margin = margin(t = 16)),
    axis.title.x = element_text(size = 18, margin = margin(t = 16)),
    axis.title.y = element_text(margin = margin(r = 14)),
    axis.text = element_text(colour = "grey15"),
    axis.text.x = element_text(size = 20, lineheight = 1.2, margin = margin(t = 12)),
    axis.ticks.x = element_blank(),
    plot.margin = margin(24, 30, 20, 24)
  )

if (compact) {
  calibration_plot <- calibration_plot + theme(
    plot.title = element_text(size = 21, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, margin = margin(b = 16)),
    plot.caption = element_text(size = 12, margin = margin(t = 12)),
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),
    axis.title.y = element_text(size = 15, margin = margin(r = 10)),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 14, margin = margin(t = 10)),
    plot.margin = margin(16, 18, 14, 16)
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
for (extension in c("pdf", "png")) {
  ggsave(
    file.path(output_dir, paste0(output_stem, ".", extension)),
    calibration_plot, width = if (compact) 8 else 16,
    height = if (compact) 7 else 9, units = "in", dpi = 240, bg = "white"
  )
}
print(group_means %>% select(automation_reliability_pattern, n, accuracy))
message("Wrote PNG and PDF to: ", file.path(output_dir, output_stem))
