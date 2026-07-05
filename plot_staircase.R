# Clear workspace
rm(list = ls())

library("dplyr")
library("readr")
library("tidyr")
library("ggplot2")
library("patchwork")
library("zoo")

fontconfig_cache <- file.path(tempdir(), "fontconfig-cache")
dir.create(fontconfig_cache, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(XDG_CACHE_HOME = fontconfig_cache)

WINDOW <- 25
BURN_IN_TRIALS <- 50
CALIB_SUMMARY_LAST_N <- 150
DEFAULT_INPUT_ROOT <- "output/pilot_harry"

slugify <- function(x) {
  out <- gsub("[^A-Za-z0-9]+", "_", x)
  out <- gsub("^_+|_+$", "", out)
  out <- tolower(out)
  if (nzchar(out)) out else "results"
}

first_non_missing <- function(x, fallback = NA) {
  x <- x[!is.na(x)]
  if (length(x)) x[[1]] else fallback
}

safe_max <- function(x, fallback = 0.25) {
  value <- suppressWarnings(max(x, na.rm = TRUE))
  if (is.finite(value) && value > 0) value else fallback
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

find_results_file <- function(input_root) {
  search_roots <- input_root
  if (!identical(normalizePath(input_root, mustWork = FALSE),
                 normalizePath("output", mustWork = FALSE))) {
    search_roots <- c(search_roots, "output")
  }

  for (root in unique(search_roots)) {
    if (!dir.exists(root)) {
      next
    }
    files <- list.files(
      root,
      pattern = "^results_.*_b00_ALL\\.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
    if (length(files)) {
      return(files[which.max(file.info(files)$mtime)])
    }
  }

  stop("No complete results files found under ", paste(unique(search_roots), collapse = " or "), ".")
}

add_running_accuracy <- function(data, order_col = "trial") {
  data %>%
    arrange(.data[[order_col]]) %>%
    mutate(
      correct_num = as_binary(correct),
      aid_correct_num = as_binary(aid_correct),
      acc_running = cumsum(replace_na(correct_num, 0)) / row_number(),
      acc_slide = rollapply(
        correct_num,
        width = WINDOW,
        FUN = function(x) mean(x, na.rm = TRUE),
        align = "right",
        fill = NA,
        partial = TRUE
      )
    )
}

phase_label <- function(block, manual_segment, reliability_phase_label, aid_reliability_level) {
  reliability_pct <- suppressWarnings(round(as.numeric(aid_reliability_level) * 100))
  case_when(
    block == "MANUAL" & manual_segment == "PRE_AUTOMATION" ~ "Manual pre",
    block == "MANUAL" & manual_segment == "POST_AUTOMATION" ~ "Manual post",
    block == "AUTOMATION" & !is.na(reliability_phase_label) &
      !is.na(reliability_pct) ~ paste0("Auto ", reliability_phase_label, " (", reliability_pct, "%)"),
    block == "AUTOMATION" & !is.na(reliability_phase_label) ~ paste0("Auto ", reliability_phase_label),
    TRUE ~ block
  )
}

phase_fill <- function(block, manual_segment, reliability_phase_label) {
  case_when(
    block == "MANUAL" & manual_segment == "PRE_AUTOMATION" ~ "manual_pre",
    block == "MANUAL" & manual_segment == "POST_AUTOMATION" ~ "manual_post",
    block == "AUTOMATION" & reliability_phase_label == "P2_70" ~ "auto_drop",
    block == "AUTOMATION" ~ "auto_high",
    TRUE ~ "other"
  )
}

make_calibration_plot <- function(dat_calib) {
  dat_calib <- add_running_accuracy(dat_calib)
  dat_calib_post <- dat_calib %>% filter(trial > BURN_IN_TRIALS)
  if (!nrow(dat_calib_post)) {
    dat_calib_post <- dat_calib
  }
  dat_calib_lastN <- dat_calib_post %>%
    slice_tail(n = min(CALIB_SUMMARY_LAST_N, nrow(dat_calib_post)))

  delta_mean <- mean(dat_calib_lastN$delta_stair_realised, na.rm = TRUE)
  delta_sd <- sd(dat_calib_lastN$delta_stair_realised, na.rm = TRUE)
  if (!is.finite(delta_sd)) {
    delta_sd <- 0
  }
  lastN_start_trial <- min(dat_calib_lastN$trial, na.rm = TRUE)
  y_max <- ceiling(safe_max(dat_calib$delta_stair_realised) * 100) / 100
  target_acc <- as.numeric(first_non_missing(dat_calib$calibration_target_accuracy))
  target_group <- first_non_missing(dat_calib$calibration_target_group, "unknown")
  pid <- first_non_missing(dat_calib$participant_id, "unknown")
  acc_mean_all <- mean(dat_calib$correct_num, na.rm = TRUE)
  acc_mean_lastN <- mean(dat_calib_lastN$correct_num, na.rm = TRUE)

  p_delta <- ggplot(dat_calib, aes(x = trial)) +
    geom_ribbon(
      data = dat_calib %>% filter(trial >= lastN_start_trial),
      aes(ymin = pmax(0, delta_mean - delta_sd), ymax = delta_mean + delta_sd),
      fill = "orange",
      alpha = 0.35
    ) +
    geom_point(aes(y = delta_stair_realised), colour = "orange", size = 1) +
    geom_line(aes(y = delta_stair_mean), colour = "purple", linewidth = 0.75) +
    geom_hline(yintercept = delta_mean, linetype = "dashed", colour = "orange") +
    coord_cartesian(ylim = c(0, y_max)) +
    labs(
      x = NULL,
      y = "Delta",
      title = paste0("Calibration dynamics; participant ", pid),
      subtitle = paste0(
        "Target group: ", target_group,
        "; last ", nrow(dat_calib_lastN),
        " post-burn-in trials delta mean = ", sprintf("%.3f", delta_mean)
      )
    ) +
    theme_classic()

  p_acc <- ggplot(dat_calib, aes(x = trial)) +
    geom_point(aes(y = correct_num), shape = 4, size = 1, alpha = 0.45, na.rm = TRUE) +
    geom_line(aes(y = acc_slide), linewidth = 0.65, colour = "steelblue") +
    geom_line(aes(y = acc_running), linewidth = 0.75, colour = "orange") +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "Trial", y = "Accuracy") +
    theme_classic()

  if (is.finite(target_acc)) {
    p_acc <- p_acc +
      geom_hline(yintercept = target_acc, linetype = 2, colour = "purple")
  }

  if (is.finite(acc_mean_lastN)) {
    p_acc <- p_acc +
      geom_hline(
        yintercept = acc_mean_lastN,
        linetype = 1,
        linewidth = 0.5,
        colour = "orange",
        alpha = 0.5
      )
  }

  if (is.finite(target_acc)) {
    p_acc <- p_acc +
      annotate(
        "text",
        x = Inf,
        y = -Inf,
        hjust = 1.05,
        vjust = -7.0,
        size = 3.5,
        colour = "black",
        label = sprintf("Target acc = %.2f", target_acc)
      )
  }

  if (is.finite(acc_mean_all)) {
    p_acc <- p_acc +
      annotate(
        "text",
        x = Inf,
        y = -Inf,
        hjust = 1.05,
        vjust = -5.0,
        size = 3.5,
        colour = "black",
        label = sprintf("Whole-block acc = %.2f", acc_mean_all)
      )
  }

  if (is.finite(acc_mean_lastN)) {
    p_acc <- p_acc +
      annotate(
        "text",
        x = Inf,
        y = -Inf,
        hjust = 1.05,
        vjust = -3.0,
        size = 3.5,
        colour = "black",
        label = sprintf("Last %d acc = %.2f", nrow(dat_calib_lastN), acc_mean_lastN)
      )
  }

  p_delta / p_acc
}

make_dynamic_plot <- function(dat_dynamic) {
  dat_dynamic <- dat_dynamic %>%
    filter(block %in% c("MANUAL", "AUTOMATION")) %>%
    mutate(
      phase_label = phase_label(
        block,
        manual_segment,
        reliability_phase_label,
        aid_reliability_level
      ),
      phase_fill = phase_fill(block, manual_segment, reliability_phase_label),
      stimulus_delta = abs(0.5 - vblack_prop)
    ) %>%
    add_running_accuracy(order_col = "global_trial") %>%
    mutate(dynamic_trial = row_number() - 1)

  if (!nrow(dat_dynamic)) {
    stop("No MANUAL or AUTOMATION rows found for manual-auto-manual dynamics.")
  }

  phase_bounds <- dat_dynamic %>%
    group_by(
      block,
      block_idx,
      manual_segment,
      reliability_phase_idx,
      reliability_phase_label,
      aid_reliability_level,
      phase_label,
      phase_fill
    ) %>%
    summarise(
      xmin = min(dynamic_trial, na.rm = TRUE),
      xmax = max(dynamic_trial, na.rm = TRUE) + 1,
      xmid = (xmin + xmax) / 2,
      n_trials = n(),
      participant_acc = mean(correct_num, na.rm = TRUE),
      aid_acc = mean(aid_correct_num, na.rm = TRUE),
      target_aid_accuracy = first(na.omit(aid_reliability_level)),
      .groups = "drop"
    ) %>%
    arrange(xmin) %>%
    mutate(
      target_aid_accuracy = ifelse(
        is.na(target_aid_accuracy),
        NA_real_,
        as.numeric(target_aid_accuracy)
      ),
      observed_label = sprintf("Observed acc\n= %.2f", participant_acc),
      aid_label = if_else(
        block == "AUTOMATION" & is.finite(aid_acc) & is.finite(target_aid_accuracy),
        sprintf("Aid acc = %.2f\n(target %.2f)", aid_acc, target_aid_accuracy),
        NA_character_
      )
    )

  phase_boundaries <- phase_bounds$xmin[-1]
  target_acc <- as.numeric(first_non_missing(dat_dynamic$calibration_target_accuracy))
  target_group <- first_non_missing(dat_dynamic$calibration_target_group, "unknown")
  pid <- first_non_missing(dat_dynamic$participant_id, "unknown")
  y_max <- ceiling(safe_max(dat_dynamic$stimulus_delta) * 100) / 100
  x_max <- max(phase_bounds$xmax, na.rm = TRUE)
  x_breaks <- seq(0, ceiling(x_max / 200) * 200, by = 200)
  x_limits <- range(x_breaks)

  p_delta <- ggplot(dat_dynamic, aes(x = dynamic_trial)) +
    geom_rect(
      data = phase_bounds,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = phase_fill),
      inherit.aes = FALSE,
      alpha = 0.08
    ) +
    geom_ribbon(
      data = dat_dynamic %>% filter(!is.na(delta_fixed_mean), !is.na(delta_fixed_sd)),
      aes(ymin = pmax(0, delta_fixed_mean - delta_fixed_sd), ymax = delta_fixed_mean + delta_fixed_sd),
      fill = "grey65",
      alpha = 0.25
    ) +
    geom_point(aes(y = stimulus_delta, colour = block), size = 0.8, alpha = 0.55) +
    geom_line(aes(y = delta_fixed_mean), colour = "purple", linewidth = 0.65, na.rm = TRUE) +
    geom_vline(xintercept = phase_boundaries, colour = "grey45", linetype = "dotted") +
    geom_text(
      data = phase_bounds,
      aes(x = xmid, y = y_max, label = phase_label),
      inherit.aes = FALSE,
      size = 3,
      vjust = 1.3
    ) +
    scale_fill_manual(
      values = c(
        manual_pre = "grey40",
        auto_high = "forestgreen",
        auto_drop = "firebrick",
        manual_post = "grey40",
        other = "grey80"
      ),
      guide = "none"
    ) +
    scale_colour_manual(values = c(MANUAL = "grey35", AUTOMATION = "orange"), guide = "none") +
    scale_x_continuous(breaks = x_breaks, limits = x_limits, expand = expansion(mult = 0)) +
    coord_cartesian(ylim = c(0, y_max)) +
    labs(
      x = NULL,
      y = "Delta",
      title = paste0("Manual -> automation reliability drop -> manual; participant ", pid),
      subtitle = paste0("Calibration target: ", target_group)
    ) +
    theme_classic()

  auto_bounds <- phase_bounds %>% filter(block == "AUTOMATION")

  p_acc <- ggplot(dat_dynamic, aes(x = dynamic_trial)) +
    geom_rect(
      data = phase_bounds,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = phase_fill),
      inherit.aes = FALSE,
      alpha = 0.08
    ) +
    geom_point(aes(y = correct_num), shape = 4, size = 0.9, alpha = 0.35, na.rm = TRUE) +
    geom_point(
      data = dat_dynamic %>% filter(!is.na(aid_correct_num)),
      aes(y = aid_correct_num),
      shape = 1,
      size = 1.15,
      alpha = 0.5,
      colour = "forestgreen",
      na.rm = TRUE
    ) +
    geom_line(aes(y = acc_slide), linewidth = 0.65, colour = "steelblue") +
    geom_line(aes(y = acc_running), linewidth = 0.7, colour = "orange") +
    geom_segment(
      data = phase_bounds,
      aes(x = xmin, xend = xmax, y = participant_acc, yend = participant_acc),
      inherit.aes = FALSE,
      colour = "black",
      linewidth = 0.75
    ) +
    geom_segment(
      data = auto_bounds,
      aes(
        x = xmin,
        xend = xmax,
        y = target_aid_accuracy,
        yend = target_aid_accuracy
      ),
      inherit.aes = FALSE,
      colour = "forestgreen",
      linewidth = 0.75,
      linetype = "dashed"
    ) +
    geom_vline(xintercept = phase_boundaries, colour = "grey45", linetype = "dotted") +
    geom_text(
      data = phase_bounds,
      aes(x = xmid, y = 0.12, label = observed_label),
      inherit.aes = FALSE,
      size = 3.1,
      lineheight = 0.9,
      colour = "black"
    ) +
    geom_text(
      data = phase_bounds %>% filter(!is.na(aid_label)),
      aes(x = xmid, y = 0.27, label = aid_label),
      inherit.aes = FALSE,
      size = 3.1,
      lineheight = 0.9,
      colour = "black"
    ) +
    scale_fill_manual(
      values = c(
        manual_pre = "grey40",
        auto_high = "forestgreen",
        auto_drop = "firebrick",
        manual_post = "grey40",
        other = "grey80"
      ),
      guide = "none"
    ) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits, expand = expansion(mult = 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "Trial number", y = "Accuracy / aid correctness") +
    theme_classic()

  if (is.finite(target_acc)) {
    p_acc <- p_acc +
      geom_hline(yintercept = target_acc, linetype = 2, colour = "purple")
  }

  p_delta / p_acc
}

args <- commandArgs(trailingOnly = TRUE)
input_root <- if (length(args)) args[[1]] else DEFAULT_INPUT_ROOT
latest_file <- find_results_file(input_root)
print(latest_file)

dat <- read_csv(latest_file, show_col_types = FALSE)
print(
  dat %>%
    count(block, block_idx, condition_code, manual_segment, reliability_phase_label, aid_reliability_level)
)

dat_calib <- dat %>% filter(block == "CALIBRATION")
if (!nrow(dat_calib)) {
  stop("No CALIBRATION block found in results file.")
}

p_calib <- make_calibration_plot(dat_calib)
p_dynamic <- make_dynamic_plot(dat)
p_combo <- p_calib / p_dynamic

if (!dir.exists("plots")) {
  dir.create("plots", recursive = TRUE)
}

parent_label <- basename(dirname(latest_file))
if (!nzchar(parent_label) || parent_label == "output") {
  parent_label <- "latest"
}
output_slug <- slugify(parent_label)
pid <- as.integer(first_non_missing(dat$participant_id, 0))
pid_slug <- if (is.finite(pid) && pid > 0) sprintf("p%03d", pid) else "p_unknown"

calibration_file <- file.path("plots", paste0(output_slug, "_calibration_", pid_slug, ".pdf"))
dynamic_file <- file.path("plots", paste0(output_slug, "_manual_auto_manual_", pid_slug, ".pdf"))
combined_file <- file.path("plots", paste0(output_slug, "_staircase_", pid_slug, ".pdf"))

save_plot_pair <- function(filename, plot, width, height) {
  png_file <- sub("\\.pdf$", ".png", filename)

  ggsave(
    filename = filename,
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
    dpi = 300
  )

  c(filename, png_file)
}

written_files <- c(
  save_plot_pair(calibration_file, p_calib, width = 10, height = 7),
  save_plot_pair(dynamic_file, p_dynamic, width = 12, height = 8),
  save_plot_pair(combined_file, p_combo, width = 12, height = 15)
)

cat("Wrote:\n")
cat(paste0(" - ", written_files, "\n"), sep = "")
