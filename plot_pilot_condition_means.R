# Clear workspace
rm(list = ls())

font_cache_dir <- file.path(tempdir(), "fontconfig-cache")
dir.create(font_cache_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(XDG_CACHE_HOME = font_cache_dir)

library("dplyr")
library("ggplot2")
library("patchwork")
library("readr")
library("tidyr")

PILOT_DIRS <- c(
  scott = "output/pilot_scott",
  harry = "output/pilot_harry"
)

CONDITION_LEVELS <- c("PRACTICE", "MANUAL", "AIDFIRST", "STIMFIRST")
CONDITION_LABELS <- c("Practice", "Manual", "Aid first", "Stimulus first")

DECISION_COLOURS <- c(
  "Decision 1" = "#1f77b4",
  "Decision 2" = "#d62728"
)

BAR_DODGE <- position_dodge(width = 0.72)

read_single_matching_csv <- function(path, pattern) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)

  if (length(files) != 1) {
    stop(
      "Expected exactly one file matching '", pattern, "' in ", path,
      "; found ", length(files), "."
    )
  }

  read_csv(files, show_col_types = FALSE)
}

read_pilot_trials <- function(pilot_name, pilot_dir) {
  practice <- read_single_matching_csv(
    pilot_dir,
    "^results_.*_b00_PRACTICE\\.csv$"
  )
  main_blocks <- read_single_matching_csv(
    pilot_dir,
    "^results_.*_b00_ALL\\.csv$"
  )

  bind_rows(practice, main_blocks) %>%
    mutate(
      pilot = pilot_name,
      condition_code = factor(
        condition_code,
        levels = CONDITION_LEVELS,
        labels = CONDITION_LABELS
      )
    )
}

as_numeric_logical <- function(x) {
  case_when(
    is.na(x) ~ NA_real_,
    x %in% c(TRUE, 1, "1", "TRUE", "True", "true") ~ 1,
    x %in% c(FALSE, 0, "0", "FALSE", "False", "false") ~ 0,
    TRUE ~ NA_real_
  )
}

summarise_pilot <- function(trials) {
  trials %>%
    mutate(
      decision1_correct_num = as_numeric_logical(decision1_correct),
      decision2_correct_num = as_numeric_logical(decision2_correct),
      changed_response_num = as_numeric_logical(changed_response)
    ) %>%
    group_by(pilot, condition_code) %>%
    summarise(
      n = n(),
      decision1_accuracy = mean(decision1_correct_num, na.rm = TRUE),
      decision2_accuracy = mean(decision2_correct_num, na.rm = TRUE),
      decision1_rt_s = mean(decision1_rt_s, na.rm = TRUE),
      decision2_rt_s = mean(decision2_rt_s, na.rm = TRUE),
      changed_response_prop = mean(changed_response_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(
      pilot,
      condition_code = factor(CONDITION_LABELS, levels = CONDITION_LABELS)
    ) %>%
    arrange(pilot, condition_code)
}

make_accuracy_plot <- function(summary_dat) {
  acc_dat <- summary_dat %>%
    select(
      pilot,
      condition_code,
      `Decision 1` = decision1_accuracy,
      `Decision 2` = decision2_accuracy
    ) %>%
    pivot_longer(
      cols = c(`Decision 1`, `Decision 2`),
      names_to = "decision",
      values_to = "mean_accuracy"
    ) %>%
    mutate(
      label = if_else(
        is.na(mean_accuracy),
        NA_character_,
        scales::percent(mean_accuracy, accuracy = 0.1)
      ),
      label_y = if_else(
        is.na(mean_accuracy),
        NA_real_,
        pmin(mean_accuracy + 0.03, 1.06)
      )
    )

  change_dat <- summary_dat %>%
    mutate(
      label = if_else(
        is.na(changed_response_prop),
        "Changed: NA",
        sprintf("Changed: %.1f%%", 100 * changed_response_prop)
      ),
      y = 0.06
    )

  ggplot(acc_dat, aes(x = condition_code, y = mean_accuracy, fill = decision)) +
    geom_col(
      position = BAR_DODGE,
      width = 0.62,
      colour = "grey25",
      linewidth = 0.2
    ) +
    geom_text(
      aes(y = label_y, label = label, group = decision),
      position = BAR_DODGE,
      vjust = 0,
      size = 3.1,
      colour = "grey15",
      na.rm = TRUE
    ) +
    geom_label(
      data = change_dat,
      aes(x = condition_code, y = y, label = label),
      inherit.aes = FALSE,
      size = 3.2,
      colour = "grey20",
      fill = "white",
      alpha = 0.9,
      linewidth = 0.15
    ) +
    scale_fill_manual(values = DECISION_COLOURS) +
    scale_y_continuous(
      limits = c(0, 1.1),
      breaks = seq(0, 1, by = 0.25),
      labels = scales::label_percent(accuracy = 1)
    ) +
    labs(
      x = NULL,
      y = "Mean accuracy",
      fill = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "top",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

make_rt_plot <- function(summary_dat) {
  rt_dat <- summary_dat %>%
    select(
      pilot,
      condition_code,
      `Decision 1` = decision1_rt_s,
      `Decision 2` = decision2_rt_s
    ) %>%
    pivot_longer(
      cols = c(`Decision 1`, `Decision 2`),
      names_to = "decision",
      values_to = "mean_rt_s"
    ) %>%
    mutate(
      label = if_else(
        is.na(mean_rt_s),
        NA_character_,
        sprintf("%.2f s", mean_rt_s)
      )
    )

  ggplot(rt_dat, aes(x = condition_code, y = mean_rt_s, fill = decision)) +
    geom_col(
      position = BAR_DODGE,
      width = 0.62,
      colour = "grey25",
      linewidth = 0.2
    ) +
    geom_text(
      aes(label = label, group = decision),
      position = BAR_DODGE,
      vjust = -0.45,
      size = 3.1,
      colour = "grey15",
      na.rm = TRUE
    ) +
    scale_fill_manual(values = DECISION_COLOURS) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
    labs(
      x = NULL,
      y = "Mean RT (s)",
      fill = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(legend.position = "none")
}

make_pilot_plot <- function(summary_dat, pilot_name) {
  pilot_summary <- summary_dat %>%
    filter(pilot == pilot_name)

  title <- paste0("Pilot ", tools::toTitleCase(pilot_name), " condition means")

  make_accuracy_plot(pilot_summary) /
    make_rt_plot(pilot_summary) +
    plot_annotation(title = title)
}

if (!dir.exists("plots")) {
  dir.create("plots", recursive = TRUE)
}

pilot_trials <- bind_rows(
  Map(read_pilot_trials, names(PILOT_DIRS), PILOT_DIRS)
)

pilot_summary <- summarise_pilot(pilot_trials)
print(pilot_summary)

for (pilot_name in names(PILOT_DIRS)) {
  pilot_plot <- make_pilot_plot(pilot_summary, pilot_name)
  out_file <- file.path(
    "plots",
    paste0("pilot_", pilot_name, "_condition_decision_means.pdf")
  )

  ggsave(
    filename = out_file,
    plot = pilot_plot,
    device = cairo_pdf,
    width = 10,
    height = 7,
    units = "in"
  )
}
