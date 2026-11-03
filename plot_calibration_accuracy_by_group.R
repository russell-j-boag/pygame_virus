# Compact calibration distribution for the dynamic-reliability study.
# Usage: Rscript plot_calibration_accuracy_by_group.R [input_csv] [output_dir] [output_stem]

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1) args[[1]] else "data/data_virus_all.csv"
output_dir <- if (length(args) >= 2) args[[2]] else "plots/semester2_2026_data/group"
output_stem <- if (length(args) >= 3) args[[3]] else
  "semester2_2026_group_calibration_accuracy_distribution_compact"

groups <- c("CAL65", "CAL90")
targets <- c("CAL65" = 0.65, "CAL90" = 0.90)
colours <- c("CAL65" = "#0072B2", "CAL90" = "#D55E00")
summary_trials <- 150L

trials <- read_csv(input_file, show_col_types = FALSE)
required <- c("participant_id", "block", "trial", "correct", "trial_deadline_s",
              "calibration_target_group", "calibration_target_accuracy")
if (length(setdiff(required, names(trials)))) {
  stop("Missing columns: ", paste(setdiff(required, names(trials)), collapse = ", "))
}
if (anyNA(trials$participant_id)) stop("Missing participant IDs.")
calibration <- trials %>%
  filter(block == "CALIBRATION") %>%
  mutate(correct_text = toupper(trimws(as.character(correct))))
if (!nrow(calibration)) stop("No calibration trials found.")
if (length(setdiff(unique(trials$participant_id), unique(calibration$participant_id)))) {
  stop("Every participant must have calibration data.")
}
if (any(!calibration$calibration_target_group %in% groups)) stop("Unknown calibration group.")
if (anyNA(calibration$calibration_target_accuracy) ||
    any(abs(calibration$calibration_target_accuracy -
            targets[calibration$calibration_target_group]) > 1e-8)) {
  stop("Calibration group and target accuracy do not match.")
}
if (anyNA(calibration$trial_deadline_s) || any(calibration$trial_deadline_s != 5)) {
  stop("Calibration must use the 5 s response window.")
}
if (any(!is.na(calibration$correct_text) &
        !calibration$correct_text %in% c("TRUE", "FALSE", "1", "0"))) {
  stop("Unsupported correctness values.")
}
coverage <- calibration %>%
  group_by(participant_id) %>%
  summarise(
    complete = identical(as.numeric(sort(trial)), as.numeric(seq_len(300))),
    n_groups = n_distinct(calibration_target_group),
    .groups = "drop"
  )
if (any(!coverage$complete | coverage$n_groups != 1)) {
  stop("Each participant must have trials 1-300 exactly once and one calibration group.")
}

participant_accuracy <- calibration %>%
  group_by(participant_id, calibration_target_group) %>%
  arrange(trial, .by_group = TRUE) %>%
  slice_tail(n = summary_trials) %>%
  summarise(
    # Match existing plots: missed and incorrect responses count as incorrect.
    accuracy = mean(correct_text %in% c("TRUE", "1")),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  mutate(group_x = match(calibration_target_group, groups)) %>%
  group_by(calibration_target_group, accuracy) %>%
  arrange(participant_id, .by_group = TRUE) %>%
  mutate(point_x = group_x + (row_number() - (n() + 1) / 2) * 0.045) %>%
  ungroup()
if (any(participant_accuracy$accuracy < 0.50)) {
  stop("Calibration accuracy is below 50%; widen the accuracy axis before exporting.")
}

group_means <- participant_accuracy %>%
  group_by(calibration_target_group, group_x) %>%
  summarise(
    n = n(), sd_accuracy = sd(accuracy),
    min_accuracy = min(accuracy), max_accuracy = max(accuracy),
    accuracy = mean(accuracy), .groups = "drop"
  ) %>%
  arrange(group_x) %>%
  mutate(target = unname(targets[calibration_target_group]),
         mean_x = group_x + 0.28, label = sprintf("%.1f%%", 100 * accuracy),
         target_label = sprintf("%.0f%%\ntarget", 100 * target))
if (!identical(group_means$calibration_target_group, groups)) stop("Both calibration groups are required.")

aid_references <- tibble(accuracy = c(0.70, 0.95), label = c("70% aid", "95% aid"))

calibration_plot <- ggplot() +
  geom_hline(data = aid_references, aes(yintercept = accuracy),
             colour = "grey60", linetype = "dotted", linewidth = 0.7) +
  geom_text(data = aid_references, aes(x = 2.78, y = accuracy, label = label),
            hjust = 1, vjust = -0.7, size = 4.5, colour = "grey30") +
  geom_segment(data = group_means,
               aes(x = group_x - 0.28, xend = group_x + 0.24,
                   y = target, yend = target, colour = calibration_target_group),
               linetype = "dashed", linewidth = 0.7, show.legend = FALSE) +
  geom_text(data = group_means,
            aes(x = group_x - 0.31, y = target, label = target_label,
                colour = calibration_target_group),
            hjust = 1, lineheight = 0.95, size = 4.0, show.legend = FALSE) +
  geom_point(data = participant_accuracy,
             aes(x = point_x, y = accuracy, colour = calibration_target_group),
             size = 1.4, stroke = 0.2, show.legend = FALSE) +
  geom_point(data = group_means, aes(x = mean_x, y = accuracy), shape = 18, size = 3.5) +
  geom_text(data = group_means, aes(x = mean_x + 0.065, y = accuracy, label = label),
            hjust = 0, size = 4.8, fontface = "bold") +
  scale_colour_manual(values = colours) +
  scale_x_continuous(breaks = 1:2,
                     labels = paste0(c("65% target", "90% target"), "\n(n = ", group_means$n, ")"),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = seq(0.50, 1, 0.05),
                     labels = scales::label_percent(accuracy = 1), expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(0.40, 2.83), ylim = c(0.50, 1)) +
  labs(title = "Calibration accuracy",
       subtitle = "Dots: participants; diamonds: group means",
       x = "Calibration group", y = "Calibration accuracy",
       caption = paste0(nrow(participant_accuracy), " participants; final ", summary_trials,
                        " calibration trials.")) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(size = 21, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, colour = "grey35", margin = margin(b = 16)),
    plot.title.position = "plot", plot.caption.position = "plot",
    plot.caption = element_text(size = 12, hjust = 0, colour = "grey35", margin = margin(t = 12)),
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),
    axis.title.y = element_text(size = 15, margin = margin(r = 10)),
    axis.text = element_text(colour = "grey15"), axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 14, lineheight = 1.2, margin = margin(t = 10)),
    axis.ticks.x = element_blank(), plot.margin = margin(16, 18, 14, 16)
  )

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
for (extension in c("pdf", "png")) {
  ggsave(file.path(output_dir, paste0(output_stem, ".", extension)), calibration_plot,
         width = 8, height = 7, units = "in", dpi = 240, bg = "white")
}
print(group_means %>% select(calibration_target_group, n, accuracy, sd_accuracy, min_accuracy, max_accuracy))
message("Wrote PNG and PDF to: ", file.path(output_dir, output_stem))
