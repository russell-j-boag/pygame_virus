# Plot participant mean final-decision accuracy from Practice calibration to Manual.
#
# Usage:
#   Rscript plot_calibration_manual_accuracy.R [input_dir] [output_dir] \
#     [output_stem] [plot_title]

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
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
  "semester2_2026_group_calibration_manual_accuracy"
}
PLOT_TITLE <- if (length(args) >= 4) {
  args[[4]]
} else {
  "Calibration vs Manual accuracy (Aid Onset Study)"
}

PRACTICE_KEEP_N <- 40
MANUAL_TRIAL_N <- 260
PRACTICE_TARGET <- 0.75
GLOBAL_AID_ACCURACY <- 0.85
CONDITION_CODES <- c("PRACTICE", "MANUAL")
CONDITION_LABELS <- c(
  "PRACTICE" = "Calibration",
  "MANUAL" = "Manual"
)
CONDITION_NAMES <- c(
  "PRACTICE" = "calibration",
  "MANUAL" = "manual"
)
REQUIRED_COLUMNS <- c(
  "participant_id",
  "condition_code",
  "trial",
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

as_binary_strict <- function(x) {
  x_text <- tolower(trimws(as.character(x)))
  missing_value <- is.na(x_text) | x_text == "" | x_text == "na"
  true_value <- x_text %in% c("true", "t", "1")
  false_value <- x_text %in% c("false", "f", "0")
  invalid_value <- !missing_value & !true_value & !false_value

  if (any(invalid_value)) {
    stop(
      "The decision2_correct column contains unsupported values: ",
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

practice_files <- latest_participant_files(
  "^results_.*_b00_PRACTICE[.]csv$",
  "Practice"
)
main_files <- latest_participant_files(
  "^results_.*_b00_ALL[.]csv$",
  "complete trial"
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
  filter(condition_code == "PRACTICE")
main_dat <- read_selected_files(main_files, "complete trial") %>%
  filter(condition_code == "MANUAL")

practice_counts <- practice_dat %>% count(participant_id, name = "n_practice")
incomplete_practice <- practice_counts %>% filter(n_practice < PRACTICE_KEEP_N)
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
  arrange(suppressWarnings(as.integer(trial)), .by_group = TRUE) %>%
  slice_tail(n = PRACTICE_KEEP_N) %>%
  ungroup()

accuracy_dat <- bind_rows(practice_dat, main_dat) %>%
  transmute(
    participant_id = as.character(participant_id),
    condition_code = as.character(condition_code),
    trial = suppressWarnings(as.integer(trial)),
    correct_num = as_binary_strict(decision2_correct)
  )

if (!nrow(accuracy_dat)) {
  stop("No Practice or Manual trials were found in: ", INPUT_DIR, call. = FALSE)
}

if (any(is.na(accuracy_dat$trial))) {
  stop("Practice or Manual trials contain non-numeric trial values.", call. = FALSE)
}

condition_coverage <- expand_grid(
  participant_id = sort(unique(accuracy_dat$participant_id)),
  condition_code = CONDITION_CODES
)
missing_conditions <- condition_coverage %>%
  anti_join(
    accuracy_dat %>% distinct(participant_id, condition_code),
    by = c("participant_id", "condition_code")
  )

if (nrow(missing_conditions)) {
  missing_text <- missing_conditions %>%
    transmute(missing = paste0(participant_id, ":", condition_code)) %>%
    pull(missing) %>%
    paste(collapse = ", ")

  stop(
    "Every participant must contain Practice and Manual trials. Missing: ",
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
  group_by(participant_id, condition_code) %>%
  summarise(
    accuracy = if (all(is.na(correct_num))) NA_real_ else mean(correct_num, na.rm = TRUE),
    n_trials = n(),
    n_valid = sum(!is.na(correct_num)),
    .groups = "drop"
  )

missing_accuracy <- participant_condition_means %>% filter(!is.finite(accuracy))
if (nrow(missing_accuracy)) {
  missing_text <- missing_accuracy %>%
    transmute(missing = paste0(participant_id, ":", condition_code)) %>%
    pull(missing) %>%
    paste(collapse = ", ")
  stop("No valid accuracy values for: ", missing_text, call. = FALSE)
}

partial_accuracy <- participant_condition_means %>% filter(n_valid < n_trials)
if (nrow(partial_accuracy)) {
  warning(
    "Missing decision2_correct values were excluded for: ",
    paste(
      paste0(
        partial_accuracy$participant_id,
        ":",
        partial_accuracy$condition_code,
        " (",
        partial_accuracy$n_valid,
        "/",
        partial_accuracy$n_trials,
        ")"
      ),
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
    )
  ) %>%
  arrange(participant_id, condition_code)

participant_means_wide <- participant_condition_means %>%
  mutate(
    participant_id = as.character(participant_id),
    condition_name = CONDITION_NAMES[as.character(condition_code)]
  ) %>%
  select(participant_id, condition_name, accuracy, n_trials, n_valid) %>%
  pivot_wider(
    names_from = condition_name,
    values_from = c(accuracy, n_trials, n_valid),
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
    n_valid_calibration,
    accuracy_manual,
    n_trials_manual,
    n_valid_manual
  )

participant_palette <- setNames(
  scales::hue_pal()(length(participant_order)),
  participant_order
)

reference_lines <- tibble(
  yint = c(PRACTICE_TARGET, GLOBAL_AID_ACCURACY),
  label = c("Calibration target (75%)", "Aid accuracy (85%)")
)

group_means <- participant_condition_means %>%
  group_by(condition_code, condition_label) %>%
  summarise(accuracy = mean(accuracy), .groups = "drop") %>%
  mutate(
    label = paste0(
      "Group mean: ",
      scales::percent(accuracy, accuracy = 0.1)
    )
  )

aid_exceedance <- participant_condition_means %>%
  group_by(condition_code, condition_label) %>%
  summarise(
    n_above_aid = sum(accuracy > GLOBAL_AID_ACCURACY),
    n_participants = n(),
    .groups = "drop"
  )

aid_exceedance_total <- participant_condition_means %>%
  group_by(participant_id) %>%
  summarise(
    above_aid_in_either_condition = any(accuracy > GLOBAL_AID_ACCURACY),
    .groups = "drop"
  ) %>%
  summarise(
    n_above_aid = sum(above_aid_in_either_condition),
    n_participants = n()
  )

aid_exceedance_annotation <- aid_exceedance %>%
  arrange(condition_code) %>%
  summarise(
    label = paste(
      c(
        " ",
        paste0(
          condition_label,
          ": ",
          n_above_aid,
          "/",
          n_participants
        ),
        paste0(
          "Cal OR Man: ",
          aid_exceedance_total[["n_above_aid"]],
          "/",
          aid_exceedance_total[["n_participants"]]
        )
      ),
      collapse = "\n"
    )
  ) %>%
  mutate(
    header_label = paste("N accuracy > aid", " ", " ", " ", sep = "\n")
  )

accuracy_extrema <- participant_condition_means %>%
  group_by(condition_code, condition_label) %>%
  summarise(
    min_accuracy = min(accuracy),
    max_accuracy = max(accuracy),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(min_accuracy, max_accuracy),
    names_to = "extremum",
    values_to = "accuracy"
  ) %>%
  mutate(
    label = paste0(
      if_else(extremum == "min_accuracy", "Min: ", "Max: "),
      scales::percent(accuracy, accuracy = 0.1)
    )
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
    yintercept = seq(0.50, 1.00, by = 0.05),
    colour = "grey90",
    linewidth = 0.3
  ) +
  geom_hline(
    data = reference_lines,
    aes(yintercept = yint),
    linetype = "dashed",
    colour = "grey35",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = reference_lines,
    aes(x = "Manual", y = yint, label = label),
    inherit.aes = FALSE,
    hjust = -0.08,
    vjust = -0.25,
    size = 3.84
  ) +
  geom_line(colour = "grey70", linewidth = 0.7) +
  geom_point(
    aes(colour = participant_id),
    size = 2.4,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(accuracy_extrema, condition_code == "PRACTICE"),
    aes(x = condition_label, y = accuracy, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    nudge_x = -0.03,
    size = 3.6,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(accuracy_extrema, condition_code == "MANUAL"),
    aes(x = condition_label, y = accuracy, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    nudge_x = -0.03,
    size = 3.6,
    fontface = "bold",
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
    data = group_means,
    aes(x = condition_label, y = accuracy, label = label),
    inherit.aes = FALSE,
    nudge_y = 0.025,
    size = 3.6,
    fontface = "bold",
    fill = "white",
    linewidth = 0.2
  ) +
  geom_label(
    data = aid_exceedance_annotation,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.6,
    fontface = "plain",
    fill = "white",
    linewidth = 0
  ) +
  geom_text(
    data = aid_exceedance_annotation,
    aes(x = Inf, y = 1.023, label = header_label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.15,
    size = 3.6,
    fontface = "bold"
  ) +
  scale_y_continuous(
    breaks = seq(0.50, 1.00, by = 0.05),
    labels = scales::label_percent(accuracy = 1)
  ) +
  labs(
    x = NULL,
    y = "Participant accuracy",
    title = PLOT_TITLE,
    subtitle = paste0(
      "Calibration mean uses the final ",
      PRACTICE_KEEP_N,
      " Practice trials; Manual mean uses all ",
      MANUAL_TRIAL_N,
      " manual-block trials."
    )
  ) +
  coord_cartesian(ylim = c(0.50, 1.00), clip = "off") +
  theme_classic(base_size = 11) +
  theme(
    axis.title.x = element_text(size = 13.2),
    axis.text.x = element_text(size = 10.56),
    plot.title = element_text(size = 15.84),
    plot.subtitle = element_text(size = 13.2),
    plot.margin = margin(5.5, 130, 5.5, 5.5)
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
