# Compact, participant-level final-decision calibration accuracy for aid onset.
# Usage: Rscript plot_calibration_accuracy_distribution.R [input_dir] [output_dir] [output_stem] [excluded_ids]
# excluded_ids is a comma-separated list of participant numbers, or 'none' (default).
# The latest matching timestamped Practice/ALL run is retained, including replacement p059.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(purrr)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1) args[[1]] else "output/semester2_2026_data"
output_dir <- if (length(args) >= 2) args[[2]] else "plots/semester2_2026_data/group"
output_stem <- if (length(args) >= 3) args[[3]] else
  "semester2_2026_group_calibration_accuracy_distribution_compact"
exclusion_arg <- if (length(args) >= 4) args[[4]] else "none"
excluded_ids <- if (exclusion_arg == "none") integer(0) else
  as.integer(strsplit(exclusion_arg, ",", fixed = TRUE)[[1]])
if (anyNA(excluded_ids)) stop("excluded_ids must be participant numbers or 'none'.")

latest_files <- function(suffix) {
  paths <- list.files(input_dir, pattern = paste0("^results_p[0-9]+_[0-9]{8}_[0-9]{6}_b00_", suffix, "[.]csv$"),
                     full.names = TRUE)
  if (!length(paths)) stop("No ", suffix, " exports found in ", input_dir)
  tibble(path = paths, participant_id = as.integer(str_match(basename(paths), "^results_p([0-9]+)_")[, 2]),
         run_timestamp = str_match(basename(paths), "^results_p[0-9]+_([0-9]{8}_[0-9]{6})_")[, 2]) %>%
    arrange(participant_id, desc(run_timestamp), desc(path)) %>%
    distinct(participant_id, .keep_all = TRUE)
}

practice_files <- latest_files("PRACTICE")
main_files <- latest_files("ALL")
if (!setequal(practice_files$participant_id, main_files$participant_id)) {
  stop("Practice and complete main-block exports must cover the same participants.")
}
matched_runs <- inner_join(practice_files, main_files, by = "participant_id", suffix = c("_practice", "_main"))
if (any(sub("_PRACTICE[.]csv$", "_ALL.csv", basename(matched_runs$path_practice)) !=
        basename(matched_runs$path_main))) {
  stop("The latest Practice and ALL exports come from different runs; resolve before plotting.")
}
if (length(setdiff(excluded_ids, practice_files$participant_id))) {
  stop("An excluded participant is absent from the source exports.")
}
selected_files <- practice_files %>% filter(!participant_id %in% excluded_ids)
if (!nrow(selected_files)) stop("No participants remain after exclusions.")

participant_accuracy <- map2_dfr(selected_files$path, selected_files$participant_id, function(path, pid) {
  dat <- read_csv(path, show_col_types = FALSE)
  required <- c("participant_id", "condition_code", "trial", "decision2_correct", "staircase_target_accuracy")
  if (length(setdiff(required, names(dat)))) stop("Missing required fields in ", basename(path))
  recorded_ids <- as.integer(sub("^[pP]", "", as.character(dat$participant_id)))
  if (anyNA(recorded_ids) || any(recorded_ids != pid)) stop("Participant ID mismatch in ", basename(path))
  if (anyNA(dat$condition_code) || any(dat$condition_code != "PRACTICE") ||
      !identical(as.numeric(sort(dat$trial)), as.numeric(seq_len(60)))) {
    stop("Expected exactly Practice trials 1-60 in ", basename(path))
  }
  if (anyNA(dat$staircase_target_accuracy) || any(abs(dat$staircase_target_accuracy - 0.75) > 1e-8)) {
    stop("Expected the 75% calibration target in ", basename(path))
  }
  window <- dat %>% arrange(trial) %>% slice_tail(n = 40)
  correctness <- toupper(trimws(as.character(window$decision2_correct)))
  if (anyNA(correctness) || any(!correctness %in% c("TRUE", "FALSE", "1", "0"))) {
    stop("Final 40 trials require nonmissing binary decision2_correct in ", basename(path))
  }
  tibble(participant_id = pid, n_trials = nrow(window),
         accuracy = mean(correctness %in% c("TRUE", "1")))
}) %>%
  group_by(accuracy) %>%
  arrange(participant_id, .by_group = TRUE) %>%
  mutate(point_x = 1 + (row_number() - (n() + 1) / 2) * 0.065) %>%
  ungroup()

if (any(participant_accuracy$accuracy < 0.50)) stop("Accuracy below 50%; widen the plotting axis.")
summary <- participant_accuracy %>% summarise(
  n = n(), sd_accuracy = sd(accuracy), min_accuracy = min(accuracy),
  max_accuracy = max(accuracy), accuracy = mean(accuracy)
) %>% mutate(label = sprintf("%.1f%%", 100 * accuracy))
references <- tibble(accuracy = c(0.75, 0.85), label = c("75% target", "85% aid"),
                     vjust = c(1.4, -0.7))
caption <- paste0(summary$n, " participants; final 40 practice trials (Decision 2).")
if (length(excluded_ids)) {
  caption <- paste0(caption, "\nExcluded participant IDs: ", paste(excluded_ids, collapse = ", "), ".")
}

calibration_plot <- ggplot() +
  geom_hline(yintercept = 0.85, colour = "grey60", linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = 0.75, colour = "grey35", linetype = "dashed", linewidth = 0.8) +
  geom_text(data = references, aes(x = 1.83, y = accuracy, label = label, vjust = vjust),
            hjust = 1, size = 4.5, colour = "grey30") +
  geom_point(data = participant_accuracy, aes(x = point_x, y = accuracy),
             colour = "#0072B2", size = 2.1, stroke = 0.25) +
  geom_point(data = summary, aes(x = 1.36, y = accuracy), shape = 18, size = 3.5) +
  geom_text(data = summary, aes(x = 1.42, y = accuracy, label = label),
            hjust = 0, size = 4.8, fontface = "bold") +
  scale_x_continuous(breaks = 1, labels = paste0("Calibration\n(n = ", summary$n, ")"),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = seq(0.50, 1, 0.05), labels = scales::label_percent(accuracy = 1),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(0.40, 1.87), ylim = c(0.50, 1)) +
  labs(title = "Calibration accuracy", subtitle = "Dots: participants; diamond: group mean",
       x = NULL, y = "Final-decision accuracy", caption = caption) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(size = 21, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, colour = "grey35", margin = margin(b = 16)),
    plot.title.position = "plot", plot.caption.position = "plot",
    plot.caption = element_text(size = 12, hjust = 0, colour = "grey35", margin = margin(t = 12)),
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
print(summary)
message("Excluded IDs: ", if (length(excluded_ids)) paste(excluded_ids, collapse = ", ") else "none")
message("Wrote PNG and PDF to: ", file.path(output_dir, output_stem))
