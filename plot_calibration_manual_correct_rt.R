# Plot participant mean correct RT across selected task conditions.
#
# Usage:
#   Rscript plot_calibration_manual_correct_rt.R [input_dir] [output_dir] \
#     [output_stem] [plot_title] [plot_variant]
#
# Plot variants:
#   calibration_manual - final 40 Practice trials and all Manual trials
#   main_conditions    - Manual, Aid first, and Stimulus first

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
PLOT_VARIANT <- if (length(args) >= 5) {
  args[[5]]
} else {
  "calibration_manual"
}

if (!PLOT_VARIANT %in% c("calibration_manual", "main_conditions")) {
  stop(
    "plot_variant must be 'calibration_manual' or 'main_conditions'.",
    call. = FALSE
  )
}

PRACTICE_KEEP_N <- 40
MAIN_CONDITION_TRIAL_N <- 260

if (PLOT_VARIANT == "calibration_manual") {
  default_output_stem <- "semester2_2026_group_calibration_manual_correct_rt"
  default_plot_title <- "Calibration vs Manual mean correct RT (Aid Onset Study)"
  plot_subtitle <- paste0(
    "Calibration means use correct responses from the final ",
    PRACTICE_KEEP_N,
    " Practice trials; Manual means use correct responses from all ",
    MAIN_CONDITION_TRIAL_N,
    " manual-block trials."
  )
  CONDITION_CODES <- c("PRACTICE", "MANUAL")
  CONDITION_LABELS <- c(
    "PRACTICE" = "Calibration",
    "MANUAL" = "Manual"
  )
  CONDITION_NAMES <- c(
    "PRACTICE" = "calibration",
    "MANUAL" = "manual"
  )
} else {
  default_output_stem <- "semester2_2026_group_main_conditions_correct_rt"
  default_plot_title <- paste0(
    "Manual, Aid first, and Stimulus first mean correct RT ",
    "(Aid Onset Study)"
  )
  plot_subtitle <- paste0(
    "Means use correct responses from all ",
    MAIN_CONDITION_TRIAL_N,
    " trials in each main condition."
  )
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
}

OUTPUT_STEM <- if (length(args) >= 3) {
  args[[3]]
} else {
  default_output_stem
}
PLOT_TITLE <- if (length(args) >= 4) {
  args[[4]]
} else {
  default_plot_title
}

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
  "decision1_rt_s",
  "decision2_correct",
  "decision2_rt_s"
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

as_rt_numeric_strict <- function(x, column_name) {
  x_text <- trimws(as.character(x))
  missing_value <- is.na(x_text) | x_text == "" | tolower(x_text) == "na"
  parsed <- suppressWarnings(as.numeric(x_text))
  invalid_value <- !missing_value & is.na(parsed)

  if (any(invalid_value)) {
    stop(
      "The ",
      column_name,
      " column contains unsupported values: ",
      paste(sort(unique(x_text[invalid_value])), collapse = ", "),
      call. = FALSE
    )
  }

  parsed[missing_value] <- NA_real_
  parsed
}

format_quality_cells <- function(dat, count_column) {
  dat %>%
    filter(.data[[count_column]] > 0) %>%
    transmute(
      label = paste0(
        participant_id,
        ":",
        decision_label,
        ":",
        condition_label,
        " (n = ",
        .data[[count_column]],
        ")"
      )
    ) %>%
    pull(label) %>%
    paste(collapse = ", ")
}

main_files <- latest_participant_files(
  "^results_.*_b00_ALL[.]csv$",
  "complete trial"
)
main_condition_codes <- setdiff(CONDITION_CODES, "PRACTICE")

main_dat <- read_selected_files(main_files, "complete trial") %>%
  filter(condition_code %in% main_condition_codes) %>%
  mutate(trial_num = suppressWarnings(as.integer(trial)))

if (any(is.na(main_dat$trial_num))) {
  stop("Main-condition trials contain non-numeric trial values.", call. = FALSE)
}

main_counts <- main_dat %>%
  count(participant_id, condition_code, name = "n_main")
expected_main_counts <- expand_grid(
  participant_id = sort(unique(main_dat$participant_id)),
  condition_code = main_condition_codes
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

practice_dat <- NULL
if ("PRACTICE" %in% CONDITION_CODES) {
  practice_files <- latest_participant_files(
    "^results_.*_b00_PRACTICE[.]csv$",
    "Practice"
  )

  file_coverage <- full_join(
    practice_files %>% transmute(participant_id, has_practice = TRUE),
    main_files %>% transmute(participant_id, has_main = TRUE),
    by = "participant_id"
  ) %>%
    mutate(
      has_practice = replace_na(has_practice, FALSE),
      has_main = replace_na(has_main, FALSE)
    )

  incomplete_files <- file_coverage %>% filter(!has_practice | !has_main)
  if (nrow(incomplete_files)) {
    incomplete_text <- incomplete_files %>%
      transmute(
        missing = paste0(
          participant_id,
          ":",
          case_when(
            !has_practice & !has_main ~ "PRACTICE+ALL",
            !has_practice ~ "PRACTICE",
            TRUE ~ "ALL"
          )
        )
      ) %>%
      pull(missing) %>%
      paste(collapse = ", ")

    stop(
      "Every participant must have both Practice and complete trial files. Missing: ",
      incomplete_text,
      call. = FALSE
    )
  }

  practice_dat <- read_selected_files(practice_files, "Practice") %>%
    filter(condition_code == "PRACTICE") %>%
    mutate(trial_num = suppressWarnings(as.integer(trial)))

  if (any(is.na(practice_dat$trial_num))) {
    stop("Practice trials contain non-numeric trial values.", call. = FALSE)
  }

  practice_counts <- practice_dat %>%
    count(participant_id, name = "n_practice")
  incomplete_practice <- practice_counts %>%
    filter(n_practice < PRACTICE_KEEP_N)
  if (nrow(incomplete_practice)) {
    stop(
      "Practice files have fewer than ",
      PRACTICE_KEEP_N,
      " trials for participant(s): ",
      paste(incomplete_practice$participant_id, collapse = ", "),
      call. = FALSE
    )
  }

  practice_dat <- practice_dat %>%
    group_by(participant_id) %>%
    arrange(trial_num, .by_group = TRUE) %>%
    slice_tail(n = PRACTICE_KEEP_N) %>%
    ungroup()
}

rt_dat <- bind_rows(practice_dat, main_dat) %>%
  transmute(
    participant_id = as.character(participant_id),
    condition_code = as.character(condition_code),
    trial = trial_num,
    decision1_correct_num = as_binary_strict(
      decision1_correct,
      "decision1_correct"
    ),
    decision1_rt_s = as_rt_numeric_strict(decision1_rt_s, "decision1_rt_s"),
    decision2_correct_num = as_binary_strict(
      decision2_correct,
      "decision2_correct"
    ),
    decision2_rt_s = as_rt_numeric_strict(decision2_rt_s, "decision2_rt_s")
  ) %>%
  pivot_longer(
    cols = matches("^decision[12]_(correct_num|rt_s)$"),
    names_to = c("decision_code", ".value"),
    names_pattern = "(decision[12])_(correct_num|rt_s)"
  ) %>%
  mutate(
    valid_correct_rt = correct_num == 1 & is.finite(rt_s) & rt_s > 0
  )

if (!nrow(rt_dat)) {
  stop("No requested condition trials were found in: ", INPUT_DIR, call. = FALSE)
}

condition_coverage <- expand_grid(
  participant_id = sort(unique(rt_dat$participant_id)),
  condition_code = CONDITION_CODES,
  decision_code = DECISION_CODES
)
missing_cells <- condition_coverage %>%
  anti_join(
    rt_dat %>% distinct(participant_id, condition_code, decision_code),
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
    "Every participant must contain both decisions in every requested condition. Missing: ",
    missing_text,
    call. = FALSE
  )
}

participant_order <- tibble(
  participant_id = unique(rt_dat$participant_id)
) %>%
  mutate(participant_num = parse_number(participant_id)) %>%
  arrange(is.na(participant_num), participant_num, participant_id) %>%
  pull(participant_id)

participant_condition_means <- rt_dat %>%
  group_by(participant_id, condition_code, decision_code) %>%
  summarise(
    mean_correct_rt = if (any(valid_correct_rt)) {
      mean(rt_s[valid_correct_rt])
    } else {
      NA_real_
    },
    n_trials = n(),
    n_correct = sum(correct_num == 1, na.rm = TRUE),
    n_correct_rt = sum(valid_correct_rt, na.rm = TRUE),
    n_missing_correctness = sum(is.na(correct_num)),
    n_invalid_correct_rt = sum(
      correct_num == 1 & (!is.finite(rt_s) | rt_s <= 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    decision_label = DECISION_LABELS[decision_code],
    condition_label = CONDITION_LABELS[condition_code]
  )

missing_rt <- participant_condition_means %>% filter(!is.finite(mean_correct_rt))
if (nrow(missing_rt)) {
  stop(
    "No valid correct RT values for: ",
    paste(
      paste0(
        missing_rt$participant_id,
        ":",
        missing_rt$decision_label,
        ":",
        missing_rt$condition_label
      ),
      collapse = ", "
    ),
    call. = FALSE
  )
}

missing_correctness <- participant_condition_means %>%
  filter(n_missing_correctness > 0)
if (nrow(missing_correctness)) {
  warning(
    "Missing correctness values were excluded for: ",
    format_quality_cells(missing_correctness, "n_missing_correctness"),
    call. = FALSE
  )
}

invalid_correct_rt <- participant_condition_means %>%
  filter(n_invalid_correct_rt > 0)
if (nrow(invalid_correct_rt)) {
  warning(
    "Correct responses with missing, non-finite, or non-positive RTs were excluded for: ",
    format_quality_cells(invalid_correct_rt, "n_invalid_correct_rt"),
    call. = FALSE
  )
}

participant_condition_means <- participant_condition_means %>%
  mutate(
    participant_id = factor(participant_id, levels = participant_order),
    condition_code = factor(condition_code, levels = CONDITION_CODES),
    condition_label = factor(
      condition_label,
      levels = unname(CONDITION_LABELS[CONDITION_CODES])
    ),
    decision_code = factor(decision_code, levels = DECISION_CODES),
    decision_label = factor(
      decision_label,
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
        c("mean_correct_rt", "n_trials", "n_correct", "n_correct_rt"),
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
    mean_correct_rt,
    n_trials,
    n_correct,
    n_correct_rt
  ) %>%
  pivot_wider(
    names_from = c(decision_name, condition_name),
    values_from = c(mean_correct_rt, n_trials, n_correct, n_correct_rt),
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

panel_ranges <- participant_condition_means %>%
  group_by(decision_code) %>%
  summarise(panel_max = max(mean_correct_rt), .groups = "drop")

condition_extrema <- participant_condition_means %>%
  group_by(decision_code, decision_label, condition_code, condition_label) %>%
  summarise(
    min_correct_rt = min(mean_correct_rt),
    max_correct_rt = max(mean_correct_rt),
    .groups = "drop"
  ) %>%
  left_join(panel_ranges, by = "decision_code") %>%
  mutate(
    max_label = sprintf("Max: %.3f s", max_correct_rt),
    min_label = sprintf("Min: %.3f s", min_correct_rt),
    max_label_y = max_correct_rt + panel_max * 0.05,
    min_label_y = min_correct_rt - pmin(
      panel_max * 0.025,
      min_correct_rt * 0.35
    )
  )

group_means <- participant_condition_means %>%
  group_by(decision_code, decision_label, condition_code, condition_label) %>%
  summarise(mean_correct_rt = mean(mean_correct_rt), .groups = "drop") %>%
  left_join(panel_ranges, by = "decision_code") %>%
  mutate(
    label = sprintf("Group mean: %.3f s", mean_correct_rt),
    label_y = mean_correct_rt + panel_max * 0.035
  )

point_labels <- participant_condition_means %>%
  group_by(decision_code, condition_code) %>%
  mutate(
    q1 = quantile(mean_correct_rt, 0.25),
    q3 = quantile(mean_correct_rt, 0.75),
    lower_fence = q1 - 1.5 * (q3 - q1),
    upper_fence = q3 + 1.5 * (q3 - q1)
  ) %>%
  filter(mean_correct_rt < lower_fence | mean_correct_rt > upper_fence) %>%
  ungroup() %>%
  left_join(panel_ranges, by = "decision_code") %>%
  group_by(decision_code, condition_code) %>%
  arrange(mean_correct_rt, participant_id, .by_group = TRUE) %>%
  group_modify(function(.x, .y) {
    label_y <- .x$mean_correct_rt
    min_gap <- unique(.x$panel_max) * 0.035
    if (length(label_y) > 1) {
      for (i in 2:length(label_y)) {
        label_y[[i]] <- max(label_y[[i]], label_y[[i - 1]] + min_gap)
      }
    }
    .x %>% mutate(label_y = label_y)
  }) %>%
  ungroup()

make_rt_panel <- function(decision) {
  plot_dat <- participant_condition_means %>%
    filter(decision_code == decision)
  plot_group_means <- group_means %>%
    filter(decision_code == decision)
  plot_point_labels <- point_labels %>%
    filter(decision_code == decision)
  plot_extrema <- condition_extrema %>%
    filter(decision_code == decision)

  upper_limit <- max(
    plot_dat$mean_correct_rt,
    plot_group_means$label_y,
    plot_point_labels$label_y,
    plot_extrema$max_label_y,
    na.rm = TRUE
  ) * 1.08

  ggplot(
    plot_dat,
    aes(
      x = condition_label,
      y = mean_correct_rt,
      group = participant_id
    )
  ) +
    geom_line(colour = "grey70", linewidth = 0.7) +
    geom_point(
      aes(colour = participant_id),
      size = 2.4,
      show.legend = FALSE
    ) +
    geom_text(
      data = filter(
        plot_point_labels,
        as.character(condition_code) == CONDITION_CODES[[1]]
      ),
      aes(y = label_y, label = participant_id),
      hjust = 1,
      nudge_x = -0.03,
      size = 3,
      show.legend = FALSE
    ) +
    geom_text(
      data = filter(
        plot_point_labels,
        as.character(condition_code) != CONDITION_CODES[[1]]
      ),
      aes(y = label_y, label = participant_id),
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
      data = plot_extrema,
      aes(x = condition_label, y = max_label_y, label = max_label),
      inherit.aes = FALSE,
      size = 3,
      fontface = "bold",
      fill = "white",
      linewidth = 0.2
    ) +
    geom_label(
      data = plot_extrema,
      aes(x = condition_label, y = min_label_y, label = min_label),
      inherit.aes = FALSE,
      size = 3,
      fontface = "bold",
      fill = "white",
      linewidth = 0.2
    ) +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 0.1, suffix = " s")
    ) +
    coord_cartesian(ylim = c(0, upper_limit), clip = "off") +
    labs(
      x = NULL,
      y = "Mean correct RT (s)",
      title = DECISION_LABELS[[decision]]
    ) +
    theme_classic(base_size = 11) +
    theme(
      plot.margin = margin(5.5, 80, 5.5, 5.5),
      plot.title = element_text(face = "bold")
    )
}

decision1_plot <- make_rt_panel("decision1")
decision2_plot <- make_rt_panel("decision2")

rt_plot <- (decision1_plot / decision2_plot) +
  plot_annotation(
    title = PLOT_TITLE,
    subtitle = plot_subtitle
  )

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

pdf_path <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, "_plot.pdf"))
png_path <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, "_plot.png"))
csv_path <- file.path(OUTPUT_DIR, paste0(OUTPUT_STEM, "_by_participant.csv"))

ggsave(
  filename = pdf_path,
  plot = rt_plot,
  width = 10,
  height = 9.5
)
ggsave(
  filename = png_path,
  plot = rt_plot,
  width = 10,
  height = 9.5,
  dpi = 300
)
write_csv(participant_means_wide, csv_path)

message("Participant means:")
print(participant_means_wide)
message("Wrote outputs:")
writeLines(c(pdf_path, png_path, csv_path))
