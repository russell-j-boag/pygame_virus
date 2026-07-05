# Plot pilot participant mean accuracy and mean RT by block.
#
# Usage:
#   Rscript plot_pilot_block_acc_rt.R [input_dir_or_file] [output_dir] [output_prefix]
#
# Defaults are set for output/pilot_harry.

rm(list = ls())

library("dplyr")
library("ggplot2")
library("patchwork")
library("readr")

fontconfig_cache <- file.path(tempdir(), "fontconfig-cache")
dir.create(fontconfig_cache, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(XDG_CACHE_HOME = fontconfig_cache)

args <- commandArgs(trailingOnly = TRUE)

INPUT_PATH <- if (length(args) >= 1) args[[1]] else "output/pilot_harry"
OUTPUT_DIR <- if (length(args) >= 2) args[[2]] else "plots"
OUTPUT_PREFIX <- if (length(args) >= 3) args[[3]] else "pilot_harry"

CALIB_SUMMARY_LAST_N <- 150

required_cols <- c(
  "participant_id", "block", "block_idx", "manual_segment",
  "reliability_phase_idx", "reliability_phase_label",
  "aid_reliability_level", "trial", "correct", "rt_s"
)

resolve_input_file <- function(path) {
  if (file.exists(path) && !dir.exists(path)) {
    return(path)
  }

  if (!dir.exists(path)) {
    stop("Input path does not exist: ", path, call. = FALSE)
  }

  files <- list.files(
    path,
    pattern = "^results_.*_b00_ALL\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (!length(files)) {
    stop("No results_*_b00_ALL.csv file found in ", path, call. = FALSE)
  }

  files[[which.max(file.info(files)$mtime)]]
}

as_binary <- function(x) {
  x_chr <- as.character(x)
  case_when(
    is.na(x_chr) ~ NA_real_,
    x_chr %in% c("TRUE", "True", "true", "T", "t", "1") ~ 1,
    x_chr %in% c("FALSE", "False", "false", "F", "f", "0") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

get_axis_limits <- function(x, pad_prop = 0.08, lower = NULL, upper = NULL) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(NULL)
  }

  lo <- min(x)
  hi <- max(x)

  if (identical(lo, hi)) {
    pad <- max(0.05 * abs(lo), 0.05)
  } else {
    pad <- (hi - lo) * pad_prop
  }

  limits <- c(lo - pad, hi + pad)
  if (!is.null(lower)) limits[[1]] <- max(lower, limits[[1]])
  if (!is.null(upper)) limits[[2]] <- min(upper, limits[[2]])
  limits
}

format_phase_label <- function(reliability_phase_label, aid_reliability_level) {
  phase <- gsub("_", " ", as.character(reliability_phase_label))
  reliability_pct <- suppressWarnings(round(as.numeric(aid_reliability_level) * 100))

  ifelse(
    !is.na(phase) & phase != "NA",
    paste("Auto", phase),
    ifelse(!is.na(reliability_pct), paste0("Auto ", reliability_pct, "%"), "Automation")
  )
}

prepare_block_data <- function(dat) {
  dat %>%
    mutate(
      correct_num = as_binary(correct),
      rt_s = suppressWarnings(as.numeric(rt_s)),
      trial = suppressWarnings(as.integer(trial)),
      reliability_phase_idx = suppressWarnings(as.integer(reliability_phase_idx)),
      aid_reliability_level = suppressWarnings(as.numeric(aid_reliability_level)),
      block_label = case_when(
        block == "CALIBRATION" ~ "Calibration",
        block == "MANUAL" & manual_segment == "PRE_AUTOMATION" ~ "Manual pre",
        block == "MANUAL" & manual_segment == "POST_AUTOMATION" ~ "Manual post",
        block == "AUTOMATION" ~ format_phase_label(reliability_phase_label, aid_reliability_level),
        TRUE ~ NA_character_
      ),
      block_order = case_when(
        block == "CALIBRATION" ~ 1L,
        block == "MANUAL" & manual_segment == "PRE_AUTOMATION" ~ 2L,
        block == "AUTOMATION" & !is.na(reliability_phase_idx) ~ 2L + reliability_phase_idx,
        block == "MANUAL" & manual_segment == "POST_AUTOMATION" ~ 6L,
        TRUE ~ NA_integer_
      )
    ) %>%
    filter(!is.na(block_order), !is.na(block_label)) %>%
    arrange(block_order, trial)
}

summarise_blocks <- function(dat_block) {
  dat_block %>%
    group_by(participant_id, block_order, block_label) %>%
    arrange(trial, .by_group = TRUE) %>%
    mutate(raw_n_trials = n()) %>%
    filter(
      block_label != "Calibration" |
        row_number() > pmax(n() - CALIB_SUMMARY_LAST_N, 0)
    ) %>%
    summarise(
      mean_accuracy = mean(correct_num, na.rm = TRUE),
      mean_rt_s = mean(rt_s, na.rm = TRUE),
      n_trials_summarised = n(),
      raw_n_trials = first(raw_n_trials),
      n_accuracy = sum(!is.na(correct_num)),
      n_rt = sum(!is.na(rt_s)),
      target_aid_accuracy = first(na.omit(aid_reliability_level)),
      .groups = "drop"
    ) %>%
    arrange(block_order) %>%
    mutate(
      target_aid_accuracy = ifelse(
        is.na(target_aid_accuracy),
        NA_real_,
        as.numeric(target_aid_accuracy)
      ),
      block_label = factor(block_label, levels = block_label),
      accuracy_label = sprintf("%.2f", mean_accuracy),
      rt_label = sprintf("%.2f s", mean_rt_s)
    )
}

make_block_plot <- function(summary_dat, input_file) {
  participant_label <- paste(sort(unique(summary_dat$participant_id)), collapse = ", ")
  target_acc <- suppressWarnings(
    as.numeric(first(na.omit(read_csv(input_file, show_col_types = FALSE)$calibration_target_accuracy)))
  )
  if (!is.finite(target_acc)) {
    target_acc <- NA_real_
  }

  auto_reference <- summary_dat %>%
    filter(!is.na(target_aid_accuracy)) %>%
    mutate(
      xmin = block_order - 0.35,
      xmax = block_order + 0.35
    )

  p_acc <- ggplot(summary_dat, aes(x = block_order, y = mean_accuracy, group = 1)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    geom_text(aes(label = accuracy_label), vjust = -1.1, size = 3.4) +
    geom_segment(
      data = auto_reference,
      aes(x = xmin, xend = xmax, y = target_aid_accuracy, yend = target_aid_accuracy),
      inherit.aes = FALSE,
      linetype = "dashed",
      colour = "forestgreen",
      linewidth = 0.7
    ) +
    scale_x_continuous(
      breaks = summary_dat$block_order,
      labels = levels(summary_dat$block_label)
    ) +
    scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      x = NULL,
      y = "Mean accuracy",
      title = paste0("Pilot participant ", participant_label, ": mean accuracy by block"),
      subtitle = paste0(
        "Calibration uses last ", CALIB_SUMMARY_LAST_N,
        " trials; purple dashed line shows calibration target\n",
        "Green dashed segments show target aid accuracy"
      )
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))

  if (is.finite(target_acc)) {
    p_acc <- p_acc +
      geom_hline(yintercept = target_acc, linetype = "dashed", colour = "purple")
  }

  rt_ylim <- get_axis_limits(
    c(summary_dat$mean_rt_s, summary_dat$mean_rt_s + 0.08),
    lower = 0
  )

  p_rt <- ggplot(summary_dat, aes(x = block_order, y = mean_rt_s, group = 1)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    geom_text(aes(label = rt_label), vjust = -1.1, size = 3.4) +
    scale_x_continuous(
      breaks = summary_dat$block_order,
      labels = levels(summary_dat$block_label)
    ) +
    coord_cartesian(ylim = rt_ylim) +
    labs(
      x = "Block",
      y = "Mean RT (s)",
      title = "Mean RT by block"
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))

  p_acc / p_rt
}

save_plot_pair <- function(plot, stem, width = 9, height = 7.5, dpi = 300) {
  pdf_file <- file.path(OUTPUT_DIR, paste0(stem, ".pdf"))
  png_file <- file.path(OUTPUT_DIR, paste0(stem, ".png"))

  ggsave(
    filename = pdf_file,
    plot = plot,
    device = cairo_pdf,
    width = width,
    height = height,
    units = "in"
  )

  ggsave(
    filename = png_file,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi
  )

  c(pdf_file, png_file)
}

input_file <- resolve_input_file(INPUT_PATH)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

message("Reading: ", input_file)
dat <- read_csv(input_file, show_col_types = FALSE)

missing_cols <- setdiff(required_cols, names(dat))
if (length(missing_cols)) {
  stop("Input file is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

dat_block <- prepare_block_data(dat)

message("Raw block counts:")
print(dat_block %>%
  count(block_order, block_label, name = "raw_n_trials") %>%
  arrange(block_order))

block_summary <- summarise_blocks(dat_block)

message("Plot summary:")
print(block_summary)

block_plot <- make_block_plot(block_summary, input_file)
written <- save_plot_pair(block_plot, paste0(OUTPUT_PREFIX, "_block_acc_rt"))

cat("Wrote:\n")
cat(paste0(" - ", written, collapse = "\n"), "\n", sep = "")
