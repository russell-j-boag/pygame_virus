# Integration validation after running both scripts with their default paths.
source("analyse_time_pressure.R")
a <- "analysis_outputs/semester2_2026_behavioural"
p <- "plots/semester2_2026_behavioural"
r <- function(f) read_csv(file.path(a, paste0(f, ".csv")), show_col_types = FALSE)
d <- r("participant_performance"); rel <- r("participant_reliance"); tr <- r("participant_trust")
c <- r("planned_contrasts"); s <- r("participant_sensitivity_contrasts"); m <- r("input_manifest")
raw <- bind_rows(lapply(m$path[m$kind == "trials"], read_csv, show_col_types = FALSE))
dat <- prepare_trials(raw)
n_subjects <- n_distinct(dat$subject)
stopifnot(nrow(d) == n_subjects * 4, sum(d$n_trials) == nrow(dat),
          sum(d$n_correct) == sum(dat$accuracy),
          sum(d$n_timeouts) == sum(dat$timeout), sum(d$n_correct_rt) == sum(dat$valid_correct_rt),
          sum(d$n_invalid_correct_rt) == sum(dat$accuracy == 1 & !dat$valid_correct_rt),
          nrow(rel) == n_subjects * 4, sum(rel$n_answered) == sum(dat$mode == "Automation" & !dat$timeout),
          nrow(tr) == n_subjects * 2, nrow(m) == n_subjects * 2,
          nrow(c) == 37, nrow(s) == 37, all(c$lower <= c$estimate & c$upper >= c$estimate),
          all(s$lower <= s$estimate & s$upper >= s$estimate),
          all(c$p_holm >= c$p_raw, na.rm = TRUE), all(s$p_holm >= s$p_raw),
          identical(unname(tools::md5sum(m$path)), m$md5))
for (nm in c("accuracy", "mean_correct_rt", "timeout_rate")) {
  plotted <- read_csv(file.path(p, "data", paste0(nm, "_plotted_means.csv")), show_col_types = FALSE) %>% arrange(pattern, pressure, mode)
  expected <- r("descriptive_performance") %>% filter(outcome == nm) %>% arrange(pattern, pressure, mode)
  stopifnot(isTRUE(all.equal(plotted$mean, expected$mean * if (nm == "mean_correct_rt") 1 else 100)))
}
h <- r("source_checksums"); stopifnot(identical(unname(tools::md5sum(h$path)), h$md5))
h <- read_csv(file.path(p, "data/input_checksums.csv"), show_col_types = FALSE)
stopifnot(identical(unname(tools::md5sum(h$path)), h$md5))
h <- read_csv(file.path(p, "data/artifact_manifest.csv"), show_col_types = FALSE)
stopifnot(nrow(h) == 26, identical(unname(tools::md5sum(file.path(p, h$file))), h$md5))
# Independently check the new benefit scale against a paired participant t interval.
rt_plot <- read_csv(file.path(p, "data/rt_benefit_estimates.csv"), show_col_types = FALSE)
stopifnot(nrow(rt_plot) == 8, all(rt_plot$benefit_lower <= rt_plot$rt_reduction_percent),
          all(rt_plot$rt_reduction_percent <= rt_plot$benefit_upper))
paired_rt <- d %>% filter(pattern == "HP95_LP65", pressure == "HP") %>%
  select(participant_id, mode, mean_log_correct_rt) %>% pivot_wider(names_from = mode, values_from = mean_log_correct_rt)
tt <- t.test(paired_rt$Manual - paired_rt$Automation)
one <- rt_plot %>% filter(method == "Participant sensitivity", pattern == "HP95_LP65", pressure == "HP")
stopifnot(nrow(one) == 1, abs(one$rt_reduction_percent - 100*(1-exp(-tt$estimate))) < 1e-10,
          max(abs(c(one$benefit_lower, one$benefit_upper) - 100*(1-exp(-tt$conf.int)))) < 1e-10)
# New headline panels must preserve allocation-specific baselines and source contrasts.
plot_data <- function(stem) read_csv(file.path(p, "data", paste0(stem, ".csv")), show_col_types = FALSE)
manual <- plot_data("manual_pressure_means")
comp <- plot_data("compensation_means")
for (cells in list(manual, comp)) {
  expected <- r("descriptive_performance") %>%
    mutate(across(c(mean, lower, upper), ~.x * ifelse(outcome == "accuracy", 100, 1)))
  checked <- left_join(cells, expected, by = c("pattern", "mode", "pressure", "outcome"), suffix = c("", "_expected"))
  stopifnot(nrow(checked) == nrow(cells), !anyNA(checked$mean_expected),
    max(abs(checked$mean - checked$mean_expected)) < 1e-10,
    max(abs(checked$lower - checked$lower_expected)) < 1e-10,
    max(abs(checked$upper - checked$upper_expected)) < 1e-10)
}
stopifnot(nrow(manual) == 8, all(manual$mode == "Manual"), nrow(comp) == 4,
  all((comp$mode == "Manual" & comp$pressure == "LP") | (comp$mode == "Automation" & comp$pressure == "HP")))
sources <- bind_rows(c %>% mutate(method = "Random-intercept model"), s %>% mutate(method = "Participant sensitivity"))
for (stem in c("accuracy_gain_estimates", "rt_benefit_estimates", "remaining_gap_estimates",
               "manual_pressure_contrasts", "reliability_effect_estimates")) {
  plotted <- plot_data(stem)
  matched <- left_join(plotted, sources %>% select(outcome, contrast, method, estimate, lower, upper),
    by = c("outcome", "contrast", "method"), suffix = c("", "_expected"))
  stopifnot(nrow(matched) == nrow(plotted), !anyNA(matched$estimate_expected),
    max(abs(matched$estimate - matched$estimate_expected)) < 1e-10,
    max(abs(matched$lower - matched$lower_expected)) < 1e-10,
    max(abs(matched$upper - matched$upper_expected)) < 1e-10)
}
for (stem in c("accuracy_gain_estimates", "rt_benefit_estimates")) {
  plotted <- plot_data(stem)
  meta <- d %>% filter(mode == "Automation") %>% distinct(pattern, pressure, reliability)
  checked <- left_join(plotted, meta, by = c("pattern", "pressure"), suffix = c("", "_actual"))
  stopifnot(nrow(checked) == 8, all(checked$reliability == paste(checked$reliability_actual, "aid")))
}
# Paired manual pressure contrasts and compensation are checked directly from participant scores.
for (g in unique(d$pattern)) {
  part <- filter(d, pattern == g)
  paired_manual <- part %>% filter(mode == "Manual") %>% select(participant_id, pressure, accuracy, mean_log_correct_rt) %>%
    pivot_wider(names_from = pressure, values_from = c(accuracy, mean_log_correct_rt))
  checks <- plot_data("manual_pressure_contrasts") %>% filter(pattern == g, method == "Participant sensitivity")
  acc <- filter(checks, outcome == "accuracy"); rt <- filter(checks, outcome == "correct_rt")
  ta <- t.test(paired_manual$accuracy_HP - paired_manual$accuracy_LP)
  trt <- t.test(paired_manual$mean_log_correct_rt_LP - paired_manual$mean_log_correct_rt_HP)
  stopifnot(abs(acc$display_estimate - 100*ta$estimate) < 1e-10,
    max(abs(c(acc$display_lower, acc$display_upper) - 100*ta$conf.int)) < 1e-10,
    abs(rt$display_estimate - 100*(1-exp(-trt$estimate))) < 1e-10,
    max(abs(c(rt$display_lower, rt$display_upper) - 100*(1-exp(-trt$conf.int)))) < 1e-10)
  paired_comp <- part %>% filter((mode == "Manual" & pressure == "LP") | (mode == "Automation" & pressure == "HP")) %>%
    select(participant_id, mode, accuracy) %>% pivot_wider(names_from = mode, values_from = accuracy)
  tc <- t.test(paired_comp$Automation - paired_comp$Manual)
  cp <- plot_data("remaining_gap_estimates") %>% filter(pattern == g, method == "Participant sensitivity")
  stopifnot(abs(cp$estimate - 100*tc$estimate) < 1e-10,
    max(abs(c(cp$lower, cp$upper) - 100*tc$conf.int)) < 1e-10)
}
rel_effects <- plot_data("reliability_effect_estimates")
stopifnot(nrow(rel_effects) == 12, all(rel_effects$pressure == ifelse(grepl(" HP ", rel_effects$contrast), "HP", "LP")))
for (nm in c("agreement", "trust")) {
  plotted <- plot_data(paste0(nm, "_plotted_means"))
  expected <- r(if (nm == "agreement") "descriptive_reliance" else "descriptive_trust") %>%
    mutate(across(c(mean, lower, upper), ~.x * if (nm == "agreement") 100 else 1),
           reliability = paste(reliability, "aid"))
  keys <- c("pattern", "pressure", "reliability", if (nm == "agreement") "advice")
  checked <- left_join(plotted, expected, by = keys, suffix = c("", "_expected"))
  stopifnot(nrow(checked) == nrow(expected), !anyNA(checked$mean_expected),
    max(abs(checked$mean - checked$mean_expected)) < 1e-10,
    max(abs(checked$lower - checked$lower_expected)) < 1e-10,
    max(abs(checked$upper - checked$upper_expected)) < 1e-10)
}
# Finding-led titles must be supported by both analyses after within-outcome Holm correction.
headline_gains <- plot_data("accuracy_gain_estimates") %>% filter(reliability == "95% aid")
headline_gap <- plot_data("remaining_gap_estimates") %>% filter(reliability == "95% aid")
headline_cost <- plot_data("manual_pressure_contrasts")
stopifnot(all(headline_gains$estimate > 0 & headline_gains$p_holm < .05),
  all(headline_gap$estimate > 0 & headline_gap$p_holm < .05),
  all(headline_cost$estimate < 0 & headline_cost$p_holm < .05),
  all(rel_effects$estimate > 0 & rel_effects$p_holm < .05))
receipt <- c(sprintf("%d-participant integration validation passed.", n_subjects),
  sprintf("%d source files unchanged; %d experimental trials; %d timeouts retained as incorrect.", nrow(m), nrow(dat), sum(dat$timeout)),
  sprintf("%d correct RTs; %d invalid correct RTs excluded from RT only; %d answered aided trials.",
          sum(d$n_correct_rt), sum(d$n_invalid_correct_rt), sum(rel$n_answered)),
  sprintf("%d six-item trust means; 37 model and 37 participant sensitivity contrasts.", nrow(tr)),
  "Confidence-interval ordering, Holm adjustment, plotted means, independent RT-benefit CI check and input/artifact hashes passed.",
  "Redesigned headline panels preserve allocation-specific baselines, source contrasts, reliability mapping, and independently reproduced paired pressure/compensation intervals.")
writeLines(receipt, file.path(a, "validation.txt"))
cat(paste(receipt, collapse = "\n"), "\n")
