# Clear workspace
rm(list = ls())

library("dplyr")
library("readr")
library("tidyr")
library("ggplot2")
library("patchwork")
library("zoo")

dat <- read_csv("data/data_virus_all.csv", show_col_types = FALSE)

if (!"decision2_correct" %in% names(dat) && "correct" %in% names(dat)) {
  dat <- dat %>%
    mutate(decision2_correct = correct)
}

WINDOW <- 25
GLOBAL_AID_ACCURACY <- 0.85

safe_max <- function(x, fallback = 0.25) {
  value <- suppressWarnings(max(x, na.rm = TRUE))
  if (is.finite(value)) value else fallback
}

condition_label <- function(aid_condition) {
  out <- dplyr::case_when(
    !is.na(aid_condition) & aid_condition == "manual" ~ "Manual",
    !is.na(aid_condition) & aid_condition == "aid_first" ~ "Aid first",
    !is.na(aid_condition) & aid_condition == "stimulus_first" ~ "Stimulus first",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Manual", "Aid first", "Stimulus first"))
}

add_running_accuracy <- function(data) {
  data %>%
    arrange(trial) %>%
    mutate(
      correct_num = if_else(is.na(decision2_correct), 0, as.numeric(decision2_correct)),
      aid_correct_num = case_when(
        aid_correct %in% c(TRUE, 1, "1", "TRUE", "True", "true") ~ 1,
        aid_correct %in% c(FALSE, 0, "0", "FALSE", "False", "false") ~ 0,
        TRUE ~ NA_real_
      ),
      acc_running = cumsum(correct_num) / row_number(),
      acc_slide = rollapply(
        correct_num,
        width = WINDOW,
        FUN = mean,
        align = "right",
        fill = NA,
        partial = TRUE
      )
    )
}

make_automation_plot <- function(dat_auto, pid) {
  dat_auto <- add_running_accuracy(dat_auto)
  label <- unique(dat_auto$aid_condition_label)[1]
  aid_acc <- mean(dat_auto$aid_correct_num, na.rm = TRUE)
  observed_acc <- mean(dat_auto$correct_num, na.rm = TRUE)
  delta_mean <- dat_auto$delta_fixed_mean[1]
  delta_sd <- dat_auto$delta_fixed_sd[1]
  y_max <- ceiling(safe_max(abs(0.5 - dat_auto$vblack_prop)) * 100) / 100

  p_delta <- ggplot(dat_auto, aes(x = trial)) +
    geom_ribbon(
      aes(ymin = delta_mean - delta_sd, ymax = delta_mean + delta_sd),
      fill = "orange",
      alpha = 0.35
    ) +
    geom_point(aes(y = abs(0.5 - vblack_prop)), colour = "orange", size = 1) +
    geom_hline(yintercept = delta_mean, linetype = "dashed", colour = "orange") +
    scale_y_continuous(limits = c(0, y_max)) +
    labs(
      x = NULL,
      y = "Delta",
      title = paste0(label, " block; Participant ", pid)
    ) +
    theme_classic()

  p_acc <- ggplot(dat_auto, aes(x = trial)) +
    geom_point(aes(y = correct_num), shape = 4, size = 1, alpha = 0.5) +
    geom_line(aes(y = acc_running), linewidth = 0.75, colour = "orange") +
    geom_hline(yintercept = observed_acc, colour = "orange", alpha = 0.6) +
    annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -3,
             label = sprintf("Observed acc = %.2f", observed_acc), size = 3.5) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "Trial", y = "Running accuracy") +
    theme_classic()

  if (is.finite(aid_acc)) {
    p_acc <- p_acc +
      geom_point(aes(y = aid_correct_num), shape = 1, size = 1.4, alpha = 0.6, colour = "forestgreen") +
      geom_hline(yintercept = GLOBAL_AID_ACCURACY, linetype = 2, colour = "forestgreen") +
      annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -5,
               label = sprintf("Aid acc = %.2f", aid_acc), size = 3.5)
  }

  p_delta / p_acc
}

if (!dir.exists("plots")) {
  dir.create("plots", recursive = TRUE)
}

participant_ids <- sort(unique(dat$participant_id))

for (pid in participant_ids) {
  dat_pid <- dat %>% filter(participant_id == pid)

  dat_auto <- dat_pid %>%
    mutate(aid_condition_label = condition_label(aid_condition)) %>%
    filter(!is.na(aid_condition_label))

  if (!nrow(dat_auto)) {
    warning("Skipping participant ", pid, ": no scheduled main-condition blocks found.")
    next
  }

  auto_plots <- dat_auto %>%
    group_split(aid_condition_label, .keep = TRUE) %>%
    lapply(make_automation_plot, pid = pid)

  p_combo <- wrap_plots(auto_plots, ncol = 1)

  ggsave(
    filename = paste0("plots/combined_aid_condition_p", pid, ".pdf"),
    plot = p_combo,
    device = cairo_pdf,
    width = 10,
    height = 12,
    units = "in"
  )
}
