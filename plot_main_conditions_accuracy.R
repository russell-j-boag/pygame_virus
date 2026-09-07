# Plot participant mean accuracy across the three main task conditions.
#
# Usage:
#   Rscript plot_main_conditions_accuracy.R [input_dir] [output_dir] \
#     [output_stem] [plot_title]

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

font_cache_dir <- file.path(tempdir(), "fontconfig-cache")
dir.create(font_cache_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(XDG_CACHE_HOME = font_cache_dir)

args <- commandArgs(trailingOnly = TRUE)

INPUT_DIR <- if (length(args) >= 1) {
  args[[1]]
} else {
  "output/semester2_2026_data"
}
OUTPUT_DIR <- if (length(args) >= 2) {
  args[[2]]
} else {
  "plots/semester2_2026_data/group"
}
OUTPUT_STEM <- if (length(args) >= 3) {
  args[[3]]
} else {
  "semester2_2026_group_main_conditions_accuracy"
}
PLOT_TITLE <- if (length(args) >= 4) {
  args[[4]]
} else {
  "Manual, Aid first, and Stimulus first mean accuracy (Aid Onset Study)"
}

MAIN_CONDITION_TRIAL_N <- 260
CONDITION_CODES <- c("MANUAL", "AIDFIRST", "STIMFIRST")
CONDITION_LABELS <- c(
  "MANUAL" = "Manual",
  "AIDFIRST" = "Aid first",
  "STIMFIRST" = "Stimulus first"
)
CONDITION_NAMES <- c(
  "MANUAL" = "manual",
  "AIDFIRST" = "aid_first",
  "STIMFIRST" = "stimulus_first"
)
DECISION_CODES <- c("decision1", "decision2")
DECISION_LABELS <- c(
  "decision1" = "Decision 1",
  "decision2" = "Decision 2"
)
REQUIRED_COLUMNS <- c(
  "participant_id",
  "condition_code",
  "trial",
  "decision1_correct",
  "decision2_correct"
)

if (!dir.exists(INPUT_DIR)) {
  stop("Input directory does not exist: ", INPUT_DIR, call. = FALSE)
}

latest_participant_files <- function(pattern, label) {
  files <- list.files(INPUT_DIR, pattern = pattern, full.names = TRUE)

  if (!length(files)) {
    stop("No ", label, " files found in ", INPUT_DIR, call. = FALSE)
  }

  file_tbl <- tibble(
    path = files,
    file_name = basename(files),
    participant_id = str_extract(basename(files), "(?<=results_)p[0-9]+"),
    mtime = file.info(files)$mtime
  )

  if (any(is.na(file_tbl$participant_id))) {
    stop(
      "Could not extract participant IDs from all ",
      label,
      " filenames.",
      call. = FALSE
    )
  }

  file_tbl %>%
    group_by(participant_id) %>%
    slice_max(order_by = mtime, n = 1, with_ties = FALSE) %>%
    ungroup()
}

validate_required_columns <- function(dat, label) {
  missing_columns <- setdiff(REQUIRED_COLUMNS, names(dat))

  if (length(missing_columns)) {
    stop(
      label,
      " input is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

read_selected_files <- function(selected_files, label) {
  message("Selected ", nrow(selected_files), " ", label, " file(s):")
  print(selected_files %>% select(participant_id, path, mtime))

  map2_dfr(
    selected_files$path,
    selected_files$participant_id,
    function(path, participant_key) {
      dat <- read_csv(path, show_col_types = FALSE)
      validate_required_columns(dat, label)

      recorded_ids <- unique(as.character(dat$participant_id))
      recorded_ids <- recorded_ids[!is.na(recorded_ids) & recorded_ids != ""]
      recorded_nums <- suppressWarnings(
        as.integer(str_remove(tolower(recorded_ids), "^p"))
      )
      expected_num <- parse_number(participant_key)

      if (length(recorded_ids) != 1 ||
          length(recorded_nums) != 1 ||
          is.na(recorded_nums) ||
          recorded_nums != expected_num) {
        stop(
          "Participant ID in ",
          basename(path),
          " does not match filename ID ",
          participant_key,
          ".",
          call. = FALSE
        )
      }

      dat
    }
  )
}

as_binary_strict <- function(x, column_name) {
  x_text <- tolower(trimws(as.character(x)))
  missing_value <- is.na(x_text) | x_text == "" | x_text == "na"
  true_value <- x_text %in% c("true", "t", "1")
  false_value <- x_text %in% c("false", "f", "0")
  invalid_value <- !missing_value & !true_value & !false_value

  if (any(invalid_value)) {
    stop(
      "The ",
      column_name,
      " column contains unsupported values: ",
      paste(sort(unique(x_text[invalid_value])), collapse = ", "),
      call. = FALSE
    )
  }

  case_when(
    missing_value ~ NA_real_,
    true_value ~ 1,
    false_value ~ 0
  )
}

main_files <- latest_participant_files(
  "^results_.*_b00_ALL[.]csv$",
  "complete trial"
)

main_dat <- read_selected_files(main_files, "complete trial") %>%
  filter(condition_code %in% CONDITION_CODES) %>%
  mutate(trial_num = suppressWarnings(as.integer(trial)))

if (any(is.na(main_dat$trial_num))) {
  stop("Main-condition trials contain non-numeric trial values.", call. = FALSE)
}

main_counts <- main_dat %>%
  count(participant_id, condition_code, name = "n_main")
expected_main_counts <- expand_grid(
  participant_id = sort(unique(main_dat$participant_id)),
  condition_code = CONDITION_CODES
) %>%
  left_join(main_counts, by = c("participant_id", "condition_code")) %>%
  mutate(n_main = replace_na(n_main, 0L))
invalid_main_counts <- expected_main_counts %>%
  filter(n_main != MAIN_CONDITION_TRIAL_N)

if (nrow(invalid_main_counts)) {
  stop(
    "Main-condition trial counts must equal ",
    MAIN_CONDITION_TRIAL_N,
    ". Invalid participant-condition counts: ",
    paste0(
      invalid_main_counts$participant_id,
      ":",
      CONDITION_LABELS[invalid_main_counts$condition_code],
      " (n = ",
      invalid_main_counts$n_main,
      ")",
      collapse = ", "
    ),
    call. = FALSE
  )
}

accuracy_dat <- main_dat %>%
  transmute(
    participant_id = as.character(participant_id),
    condition_code = as.character(condition_code),
    trial = trial_num,
    decision1 = as_binary_strict(decision1_correct, "decision1_correct"),
    decision2 = as_binary_strict(decision2_correct, "decision2_correct")
  ) %>%
  pivot_longer(
    cols = all_of(DECISION_CODES),
    names_to = "decision_code",
    values_to = "correct_num"
  )

condition_coverage <- expand_grid(
  participant_id = sort(unique(accuracy_dat$participant_id)),
  condition_code = CONDITION_CODES,
  decision_code = DECISION_CODES
)
missing_cells <- condition_coverage %>%
  anti_join(
    accuracy_dat %>% distinct(participant_id, condition_code, decision_code),
    by = c("participant_id", "condition_code", "decision_code")
  )

if (nrow(missing_cells)) {
  missing_text <- missing_cells %>%
    transmute(
      missing = paste0(participant_id, ":", decision_code, ":", condition_code)
    ) %>%
    pull(missing) %>%
    paste(collapse = ", ")

  stop(
    "Every participant must contain both decisions in every main condition. Missing: ",
    missing_text,
    call. = FALSE
  )
}

participant_order <- tibble(
  participant_id = unique(accuracy_dat$participant_id)
) %>%
  mutate(participant_num = parse_number(participant_id)) %>%
  arrange(is.na(participant_num), participant_num, participant_id) %>%
  pull(participant_id)

participant_condition_means <- accuracy_dat %>%
  group_by(participant_id, condition_code, decision_code) %>%
  summarise(
    mean_accuracy = if (all(is.na(correct_num))) {
      NA_real_
    } else {
      mean(correct_num, na.rm = TRUE)
    },
    n_trials = n(),
    n_valid = sum(!is.na(correct_num)),
    .groups = "drop"
  )

missing_accuracy <- participant_condition_means %>%
  filter(!is.finite(mean_accuracy))
if (nrow(missing_accuracy)) {
  stop(
    "No valid accuracy values for: ",
    paste0(
      missing_accuracy$participant_id,
      ":",
      missing_accuracy$decision_code,
      ":",
      missing_accuracy$condition_code,
      collapse = ", "
    ),
    call. = FALSE
  )
}

partial_accuracy <- participant_condition_means %>%
  filter(n_valid < n_trials)
if (nrow(partial_accuracy)) {
  warning(
    "Missing correctness values were excluded for: ",
    paste0(
      partial_accuracy$participant_id,
      ":",
      partial_accuracy$decision_code,
      ":",
      partial_accuracy$condition_code,
      " (",
      partial_accuracy$n_valid,
      "/",
      partial_accuracy$n_trials,
      ")",
      collapse = ", "
    ),
    call. = FALSE
  )
}

participant_condition_means <- participant_condition_means %>%
  mutate(
    participant_id = factor(participant_id, levels = participant_order),
    condition_code = factor(condition_code, levels = CONDITION_CODES),
    condition_label = factor(
      CONDITION_LABELS[as.character(condition_code)],
      levels = unname(CONDITION_LABELS[CONDITION_CODES])
    ),
    decision_code = factor(decision_code, levels = DECISION_CODES),
    decision_label = factor(
      DECISION_LABELS[as.character(decision_code)],
      levels = unname(DECISION_LABELS[DECISION_CODES])
    )
  ) %>%
  arrange(participant_id, decision_code, condition_code)

participant_output_columns <- "participant_id"
for (decision in DECISION_CODES) {
  for (condition in CONDITION_CODES) {
    participant_output_columns <- c(
      participant_output_columns,
      paste0(
        decision,
        "_",
        c("mean_accuracy", "n_trials", "n_valid"),
        "_",
        CONDITION_NAMES[[condition]]
      )
    )
  }
}

participant_means_wide <- participant_condition_means %>%
  mutate(
    participant_id = as.character(participant_id),
    decision_name = as.character(decision_code),
    condition_name = CONDITION_NAMES[as.character(condition_code)]
  ) %>%
  select(
    participant_id,
    decision_name,
    condition_name,
    mean_accuracy,
    n_trials,
    n_valid
  ) %>%
  pivot_wider(
    names_from = c(decision_name, condition_name),
    values_from = c(mean_accuracy, n_trials, n_valid),
    names_glue = "{decision_name}_{.value}_{condition_name}"
  ) %>%
  mutate(
    participant_id = factor(participant_id, levels = participant_order)
  ) %>%
  arrange(participant_id) %>%
  mutate(participant_id = as.character(participant_id)) %>%
  select(all_of(participant_output_columns))

participant_palette <- setNames(
  scales::hue_pal()(length(participant_order)),
  participant_order
)

accuracy_extrema <- participant_condition_means %>%
  group_by(decision_code, decision_label, condition_code, condition_label) %>%
  summarise(
    min_accuracy = min(mean_accuracy),
    max_accuracy = max(mean_accuracy),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(min_accuracy, max_accuracy),
    names_to = "extremum",
    values_to = "mean_accuracy"
  ) %>%
  inner_join(
    participant_condition_means %>%
      select(
        participant_id,
        decision_code,
        decision_label,
        condition_code,
        condition_label,
        mean_accuracy
      ),
    by = c(
      "decision_code",
      "decision_label",
      "condition_code",
      "condition_label",
      "mean_accuracy"
    )
  ) %>%
  group_by(
    decision_code,
    decision_label,
    condition_code,
    condition_label,
    extremum,
    mean_accuracy
  ) %>%
  summarise(
    participant_ids = paste(as.character(participant_id), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      if_else(extremum == "min_accuracy", "Min: ", "Max: "),
      scales::percent(mean_accuracy, accuracy = 0.1),
      " (ID ",
      participant_ids,
      ")"
    ),
    label_y = if_else(
      extremum == "min_accuracy",
      mean_accuracy - 0.025,
      mean_accuracy + 0.025
    )
  )

group_means <- participant_condition_means %>%
  group_by(decision_code, decision_label, condition_code, condition_label) %>%
  summarise(mean_accuracy = mean(mean_accuracy), .groups = "drop") %>%
  mutate(
    label = paste0(
      "Group mean: ",
      scales::percent(mean_accuracy, accuracy = 0.1)
    ),
    label_y = mean_accuracy + 0.025
  )

make_accuracy_panel <- function(decision) {
  plot_dat <- participant_condition_means %>%
    filter(decision_code == decision)
  plot_group_means <- group_means %>%
    filter(decision_code == decision)
  plot_extrema <- accuracy_extrema %>%
    filter(decision_code == decision)

  ggplot(
    plot_dat,
    aes(
      x = condition_label,
      y = mean_accuracy,
      group = participant_id
    )
  ) +
    geom_hline(
      yintercept = seq(0.50, 1.00, by = 0.05),
      colour = "grey90",
      linewidth = 0.3
    ) +
    geom_line(colour = "grey70", linewidth = 0.7) +
    geom_point(
      aes(colour = participant_id),
      size = 2.4,
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
    geom_label(
      data = plot_group_means,
      aes(x = condition_label, y = label_y, label = label),
      inherit.aes = FALSE,
      size = 3,
      fontface = "bold",
      fill = "white",
      linewidth = 0.2
    ) +
    geom_label(
      data = filter(plot_extrema, extremum == "max_accuracy"),
      aes(x = condition_label, y = label_y, label = label),
      inherit.aes = FALSE,
      size = 3,
      fontface = "bold",
      fill = "white",
      linewidth = 0.2
    ) +
    geom_label(
      data = filter(plot_extrema, extremum == "min_accuracy"),
      aes(x = condition_label, y = label_y, label = label),
      inherit.aes = FALSE,
      size = 3,
      fontface = "bold",
      fill = "white",
      linewidth = 0.2
    ) +
    scale_y_continuous(
      breaks = seq(0.50, 1.00, by = 0.05),
      labels = scales::label_percent(accuracy = 1)
    ) +
    coord_cartesian(ylim = c(0.47, 1.03), clip = "off") +
    labs(
      x = NULL,
      y = "Mean accuracy",
      title = DECISION_LABELS[[decision]]
    ) +
    theme_classic(base_size = 11) +
    theme(
      plot.margin = margin(5.5, 80, 5.5, 5.5),
      plot.title = element_text(face = "bold")
    )
}

decision1_plot <- make_accuracy_panel("decision1")
decision2_plot <- make_accuracy_panel("decision2")

accuracy_plot <- (decision1_plot / decision2_plot) +
  plot_annotation(
    title = PLOT_TITLE,
    subtitle = paste0(
      "Means use all ",
      MAIN_CONDITION_TRIAL_N,
      " trials in each main condition."
    )
  )

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

pdf_path <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, "_plot.pdf"))
png_path <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, "_plot.png"))
csv_path <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, "_by_participant.csv"))

ggsave(
  filename = pdf_path,
  plot = accuracy_plot,
  width = 10,
  height = 9.5
)
ggsave(
  filename = png_path,
  plot = accuracy_plot,
  width = 10,
  height = 9.5,
  dpi = 300
)
write_csv(participant_means_wide, csv_path)

message("Participant means:")
print(participant_means_wide)
message("Wrote outputs:")
writeLines(c(pdf_path, png_path, csv_path))
