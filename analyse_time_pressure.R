# Reproducible main-study behavioural analysis; no task or existing data edits.
# Usage: Rscript analyse_time_pressure.R [input_dir] [output_dir]
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(ggplot2)
})

binary <- function(x, label) {
  z <- tolower(trimws(as.character(x)))
  if (any(!is.na(z) & !z %in% c("true", "false", "1", "0")))
    stop("Invalid binary values in ", label)
  ifelse(is.na(z), NA_integer_, as.integer(z %in% c("true", "1")))
}
check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
patterns <- c("HP95_LP65", "HP65_LP95")

strict_model_control <- function(binary_outcome) {
  constructor <- if (binary_outcome) glmerControl else lmerControl
  settings <- if (binary_outcome)
    list(optimizer = "bobyqa", calc.derivs = TRUE,
         optCtrl = list(maxfun = 300000, rhoend = 1e-9))
    else list(optimizer = "nloptwrap", calc.derivs = TRUE,
              optCtrl = list(maxeval = 300000, xtol_abs = 1e-8, ftol_abs = 1e-8))
  # lme4 2.x otherwise skips derivative checks for these large trial datasets.
  for (key in c("check.conv.nobsmax", "check.conv.nparmax"))
    if (key %in% names(formals(constructor))) settings[[key]] <- Inf
  do.call(constructor, settings)
}

prepare_trials <- function(raw) {
  needed <- c("participant_id", "run_timestamp", "condition_deadline_code", "block_idx",
              "trial", "response", "correct", "rt_s", "stimulus", "aid_label",
              "aid_correct", "automation_reliability_pattern", "aid_accuracy_setting",
              "time_pressure_condition", "trial_deadline_s")
  check(all(needed %in% names(raw)), "Missing required trial columns")
  check(!anyDuplicated(raw[c("participant_id", "run_timestamp", "block_idx", "trial")]),
        "Duplicate trial keys")
  check(all(raw$response %in% c("BLACK", "WHITE", "TIMEOUT")), "Unexpected response")
  raw_correct <- binary(raw$correct, "correct")
  timeout <- raw$response == "TIMEOUT"
  check(all(!is.na(raw_correct[!timeout])), "Answered trial has missing correctness")
  check(all(is.na(raw_correct[timeout]) | raw_correct[timeout] == 0), "Timeout marked correct")
  check(all(raw_correct[!timeout] == as.integer(raw$response[!timeout] == raw$stimulus[!timeout])),
        "Correctness differs from response/stimulus comparison")
  aided <- raw$condition_deadline_code %in% c("A_HP", "A_LP")
  aid_correct <- binary(raw$aid_correct, "aid_correct")
  check(all(raw$aid_label[aided] %in% c("BLACK", "WHITE")), "Missing or invalid advice")
  check(all(aid_correct[aided] == as.integer(raw$aid_label[aided] == raw$stimulus[aided])),
        "Advice correctness differs from advice/stimulus comparison")
  raw %>% mutate(
    subject = factor(participant_id), pattern = factor(automation_reliability_pattern, patterns),
    pressure = factor(time_pressure_condition, c("LP", "HP")),
    mode = factor(ifelse(aided, "Automation", "Manual"), c("Manual", "Automation")),
    reliability = factor(ifelse(aided, ifelse(aid_accuracy_setting == .95, "95%", "65%"), NA),
                         c("65%", "95%")),
    advice = factor(ifelse(aid_correct == 1, "Correct", "Incorrect"), c("Incorrect", "Correct")),
    timeout = timeout, accuracy = ifelse(timeout, 0L, raw_correct),
    valid_correct_rt = accuracy == 1 & is.finite(rt_s) & rt_s > 0,
    agreement = ifelse(aided & !timeout, as.integer(response == aid_label), NA_integer_)
  ) %>% filter(condition_deadline_code != "CAL_LP")
}

performance_contrasts <- function(grid) {
  cell <- function(m, p, g) as.numeric(grid$mode == m & grid$pressure == p & grid$pattern == g)
  gain <- function(p, g) cell("Automation", p, g) - cell("Manual", p, g)
  methods <- list()
  for (g in patterns) {
    for (p in c("HP", "LP")) methods[[paste(g, p, "Automation - Manual")]] <- gain(p, g)
    methods[[paste(g, "HP gain - LP gain")]] <- gain("HP", g) - gain("LP", g)
    methods[[paste(g, "Manual HP - Manual LP")]] <- cell("Manual", "HP", g) - cell("Manual", "LP", g)
    methods[[paste(g, "Aided HP - Manual LP")]] <- cell("Automation", "HP", g) - cell("Manual", "LP", g)
  }
  methods[["Difference in HP-minus-LP gains: HP95_LP65 - HP65_LP95"]] <-
    gain("HP", patterns[1]) - gain("LP", patterns[1]) - gain("HP", patterns[2]) + gain("LP", patterns[2])
  methods
}

aided_contrasts <- function(grid, stratified = FALSE) {
  methods <- list()
  for (a in if (stratified) c("Incorrect", "Correct") else "Trust") {
    cell <- function(p, r) as.numeric(grid$pressure == p & grid$reliability == r &
                                      if (stratified) grid$advice == a else TRUE)
    for (p in c("HP", "LP")) methods[[paste(a, p, "95% - 65%")]] <- cell(p, "95%") - cell(p, "65%")
    for (r in c("65%", "95%")) methods[[paste(a, r, "HP - LP")]] <- cell("HP", r) - cell("LP", r)
    methods[[paste(a, "Reliability effect HP - LP")]] <-
      cell("HP", "95%") - cell("HP", "65%") - cell("LP", "95%") + cell("LP", "65%")
  }
  methods
}

describe <- function(data, groups, value) {
  data %>% group_by(across(all_of(groups))) %>% summarise(
    n = sum(is.finite(.data[[value]])), mean = mean(.data[[value]], na.rm = TRUE),
    se = sd(.data[[value]], na.rm = TRUE) / sqrt(n), .groups = "drop"
  ) %>% mutate(lower = mean - qt(.975, n - 1) * se, upper = mean + qt(.975, n - 1) * se)
}

fit_outcome <- function(name, formula, data, binary_outcome, outdir) {
  message("Fitting ", name, " (", nrow(data), " observations)")
  warnings <- character()
  model <- withCallingHandlers(
    if (binary_outcome) glmer(formula, data, family = binomial(), nAGQ = 1,
                              control = strict_model_control(TRUE))
    else lmerTest::lmer(formula, data, REML = TRUE,
                       control = strict_model_control(FALSE)),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  saveRDS(model, file.path(outdir, "models", paste0(name, ".rds")))
  messages <- model@optinfo$conv$lme4$messages
  converged <- is.null(messages) && all(model@optinfo$conv$opt == 0) && all(is.finite(fixef(model)))
  singular <- isSingular(model, tol = 1e-4)
  hessian <- model@optinfo$derivs$Hessian
  gradient <- model@optinfo$derivs$gradient
  scaled_gradient <- if (!is.null(hessian) && !is.null(gradient))
    tryCatch(max(abs(solve(chol(hessian), gradient))), error = function(e) NA_real_) else NA_real_
  diagnostic <- tibble(outcome = name, n_observations = nrow(data), n_participants = n_distinct(data$subject),
                       converged = converged, singular = singular, inference_available = converged && !singular,
                       max_scaled_gradient = scaled_gradient, warnings = paste(unique(warnings), collapse = " | "),
                       convergence_message = paste(messages, collapse = " | "))
  if (binary_outcome) {
    # Conditional binomial checks within participant x design cells. These detect
    # lack of fit not visible in observation-level Bernoulli Pearson residuals.
    f <- fitted(model)
    y <- model.response(model.frame(model))
    id <- interaction(data$subject, round(f, 12), drop = TRUE)
    counts <- tibble(cell = id, y = y, p = f) %>% group_by(cell) %>%
      summarise(n = n(), k = sum(y), p = mean(p), .groups = "drop")
    variance <- pmax(counts$n * counts$p * (1 - counts$p), 1e-10)
    discrepancy <- sum((counts$k - counts$n * counts$p)^2 / variance)
    set.seed(20261005)
    simulated <- replicate(2000, sum((rbinom(nrow(counts), counts$n, counts$p) -
                                      counts$n * counts$p)^2 / variance))
    diagnostic$conditional_cell_dispersion <- discrepancy / nrow(counts)
    diagnostic$conditional_cell_check_p <- (1 + sum(simulated >= discrepancy)) / 2001
    diagnostic$boundary_observed_cells <- sum(counts$k == 0 | counts$k == counts$n)
    diagnostic$extreme_fitted_probability <- any(f < 1e-6 | f > 1 - 1e-6)
    write_csv(counts, file.path(outdir, paste0(name, "_conditional_cell_checks.csv")))
  }
  writeLines(capture.output(summary(model)), file.path(outdir, paste0(name, "_model_summary.txt")))
  cell_vars <- if (name %in% c("accuracy", "correct_rt")) c("pressure", "pattern", "mode")
    else c("pressure", "reliability", if (name == "reliance") "advice")
  residual_data <- tibble(fitted = fitted(model), residual = residuals(model, type = "pearson"),
                          cell = do.call(interaction, c(lapply(data[cell_vars], factor), list(drop = TRUE))))
  residual_summary <- residual_data %>% group_by(cell) %>% summarise(n = n(), residual_sd = sd(residual),
                                                                     residual_mean = mean(residual), .groups = "drop")
  write_csv(residual_summary, file.path(outdir, paste0(name, "_residual_summary.csv")))
  diagnostic$residual_sd_max_min_ratio <- max(residual_summary$residual_sd) / min(residual_summary$residual_sd)
  if (!binary_outcome) {
    p <- ggplot(residual_data, aes(sample = residual)) + stat_qq(alpha = .15, size = .5) +
      stat_qq_line() + facet_wrap(~cell, scales = "free") + theme_minimal() + labs(title = paste(name, "residual Q-Q plots"))
    ggsave(file.path(outdir, paste0(name, "_residual_qq.png")), p, width = 10, height = 6, dpi = 150)
  }
  list(model = model, diagnostics = diagnostic)
}

extract_results <- function(fit, name, specification, contrast_function, binary_outcome, outdir) {
  emm <- emmeans(fit$model, specification, lmer.df = "satterthwaite", lmerTest.limit = Inf)
  # Probability differences must be contrasted AFTER response-scale regridding.
  contrast_emm <- if (binary_outcome) regrid(emm, transform = "response") else emm
  methods <- contrast_function(as.data.frame(contrast_emm))
  check(all(vapply(methods, function(w) abs(sum(w)) < 1e-12 && any(w != 0), logical(1))),
        "Invalid planned contrast weights")
  weight_table <- bind_rows(lapply(names(methods), function(nm) {
    as.data.frame(contrast_emm) %>% mutate(contrast = nm, weight = methods[[nm]])
  }))
  write_csv(weight_table, file.path(outdir, paste0(name, "_contrast_weights.csv")))
  tab <- as.data.frame(summary(contrast(contrast_emm, methods), infer = c(TRUE, TRUE), adjust = "none"))
  lower_col <- intersect(c("lower.CL", "asymp.LCL"), names(tab))
  upper_col <- intersect(c("upper.CL", "asymp.UCL"), names(tab))
  scale <- if (binary_outcome) 100 else 1
  result <- tibble(outcome = name, contrast = tab$contrast, estimate = tab$estimate * scale,
                   lower = tab[[lower_col]] * scale, upper = tab[[upper_col]] * scale,
                   se = tab$SE * scale, p_raw = tab$p.value, p_holm = p.adjust(tab$p.value, "holm"),
                   units = if (binary_outcome) "percentage points" else if (name == "correct_rt") "log seconds contrast" else "trust points",
                   inference_available = fit$diagnostics$inference_available)
  if (name == "correct_rt") {
    result <- result %>% mutate(ratio = exp(estimate), ratio_lower = exp(lower), ratio_upper = exp(upper))
  }
  if (!fit$diagnostics$inference_available) result$p_raw <- result$p_holm <- NA_real_
  means <- as.data.frame(summary(emm, type = "response", infer = c(TRUE, FALSE)))
  write_csv(means, file.path(outdir, paste0(name, "_model_means.csv")))
  write_csv(result, file.path(outdir, paste0(name, "_contrasts.csv")))
  result
}

participant_sensitivity <- function(data, value, grid, methods, name, scale = 1) {
  # The two allocation patterns are independent groups. Within each group all
  # relevant conditions are paired. Test participant contrast scores, preserving
  # arbitrary within-person dependence and allowing unequal group variances.
  keys <- intersect(c("mode", "pressure", "pattern", "reliability", "advice"), names(grid))
  results <- lapply(names(methods), function(nm) {
    weights <- grid[, keys, drop = FALSE]
    weights$weight <- methods[[nm]]
    cells <- inner_join(data, weights, by = keys)
    active <- cells %>% group_by(pattern) %>% summarise(active = any(weight != 0), .groups = "drop")
    scores <- cells %>% filter(pattern %in% active$pattern[active$active]) %>%
      group_by(pattern, participant_id) %>% summarise(score = sum(.data[[value]] * weight), .groups = "drop")
    components <- scores %>% group_by(pattern) %>% summarise(n = n(), estimate = mean(score),
                                                             variance = var(score) / n, .groups = "drop")
    check(all(components$n > 1) && all(is.finite(components$variance)), "Insufficient participant contrasts")
    estimate_raw <- sum(components$estimate)
    se_raw <- sqrt(sum(components$variance))
    df <- sum(components$variance)^2 / sum(components$variance^2 / (components$n - 1))
    tibble(outcome = name, contrast = nm, estimate = estimate_raw * scale,
           lower = (estimate_raw - qt(.975, df) * se_raw) * scale, upper = (estimate_raw + qt(.975, df) * se_raw) * scale,
           se = se_raw * scale, df = df, p_raw = 2 * pt(-abs(estimate_raw / se_raw), df),
           units = if (scale == 100) "percentage points" else if (name == "correct_rt") "participant mean log RT contrast" else "trust points")
  }) %>% bind_rows() %>% mutate(p_holm = p.adjust(p_raw, "holm"))
  if (name == "correct_rt") results <- results %>% mutate(ratio = exp(estimate), ratio_lower = exp(lower), ratio_upper = exp(upper))
  results
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  check(length(args) <= 2, "Usage: Rscript analyse_time_pressure.R [input_dir] [output_dir]")
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
  root <- dirname(normalizePath(script))
  input <- if (length(args) >= 1) args[1] else file.path(root, "output/semester2_2026_data")
  out <- if (length(args) >= 2) args[2] else file.path(root, "analysis_outputs/semester2_2026_behavioural")
  check(dir.exists(input), "Input directory does not exist")
  dir.create(file.path(out, "models"), recursive = TRUE, showWarnings = FALSE)
  # Remove only a prior success receipt, so failed reruns cannot look complete.
  unlink(file.path(out, "COMPLETE.txt"))
  python <- Sys.getenv("TIME_PRESSURE_PYTHON", "/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python")
  status <- system2(python, c(shQuote(file.path(root, "python/validate_time_pressure_allocation.py")),
                             shQuote(input), shQuote(file.path(out, "allocation_validation.csv"))),
                    env = "PYTHONDONTWRITEBYTECODE=1")
  check(status == 0, "Task allocation validation failed")
  files <- sort(list.files(input, "^results_.*_b00_ALL\\.csv$", full.names = TRUE))
  selected <- lapply(files, function(path) {
    trials <- read_csv(path, show_col_types = FALSE)
    qpath <- sub("_ALL.csv$", "_POSTBLOCK_ALL.csv", path)
    check(file.exists(qpath), paste("Missing questionnaire for", basename(path)))
    questions <- read_csv(qpath, show_col_types = FALSE)
    check(n_distinct(trials$participant_id) == 1 && n_distinct(trials$run_timestamp) == 1, "Mixed trial run")
    check(all(questions$participant_id == trials$participant_id[1]) &&
            all(questions$run_timestamp == trials$run_timestamp[1]), "Questionnaire run mismatch")
    selected_paths <- normalizePath(c(path, qpath))
    list(trials = trials, questions = questions,
         manifest = tibble(participant_id = trials$participant_id[1], run_timestamp = trials$run_timestamp[1],
                           kind = c("trials", "trust"), path = selected_paths,
                           md5 = unname(tools::md5sum(selected_paths)), n_rows = c(nrow(trials), nrow(questions))))
  })
  manifest <- bind_rows(lapply(selected, `[[`, "manifest"))
  write_csv(manifest, file.path(out, "input_manifest.csv"))
  source_paths <- c(script, file.path(root, "analyse_time_pressure_exploratory.R"), file.path(root, "python/validate_time_pressure_allocation.py"), file.path(root, "python/virus_task.py"))
  write_csv(tibble(path = normalizePath(source_paths), md5 = unname(tools::md5sum(source_paths))),
            file.path(out, "source_checksums.csv"))
  raw <- bind_rows(lapply(selected, `[[`, "trials"))
  dat <- prepare_trials(raw)
  check(n_distinct(dat$pattern) == 2, "Both assignment groups are required")
  write_csv(dat %>% count(pattern, subject) %>% count(pattern, name = "n_participants"), file.path(out, "cohort_counts.csv"))
  participant <- dat %>% group_by(participant_id, pattern, mode, pressure, reliability) %>% summarise(
    n_trials = n(), n_correct = sum(accuracy), n_invalid_correct_rt = sum(accuracy == 1 & !valid_correct_rt),
    accuracy = mean(accuracy),
    n_timeouts = sum(timeout), timeout_rate = mean(timeout),
    n_correct_rt = sum(valid_correct_rt), mean_correct_rt = if (sum(valid_correct_rt)) mean(rt_s[valid_correct_rt]) else NA_real_,
    mean_log_correct_rt = if (sum(valid_correct_rt)) mean(log(rt_s[valid_correct_rt])) else NA_real_,
    overall_agreement = if (all(is.na(agreement))) NA_real_ else mean(agreement, na.rm = TRUE),
    realised_aid_accuracy = if (all(is.na(advice))) NA_real_ else mean(advice == "Correct", na.rm = TRUE), .groups = "drop")
  check(sum(participant$n_invalid_correct_rt) == sum(dat$accuracy == 1 & !dat$valid_correct_rt),
        "Participant invalid-RT audit does not match trial data")
  write_csv(participant, file.path(out, "participant_performance.csv"))
  group <- bind_rows(lapply(c("accuracy", "mean_correct_rt", "timeout_rate"), function(value) {
    describe(participant, c("pattern", "mode", "pressure"), value) %>% mutate(outcome = value)
  }))
  write_csv(group, file.path(out, "descriptive_performance.csv"))
  exclusions <- dat %>% filter(timeout | accuracy == 1 & !valid_correct_rt) %>%
    transmute(participant_id, run_timestamp, block_idx, condition_deadline_code, trial, response, rt_s,
              reason = ifelse(timeout, "Timeout: incorrect accuracy; no RT/agreement", "Correct trial: invalid RT only"))
  write_csv(exclusions, file.path(out, "trial_exclusions_and_timeouts.csv"))
  rt_audit <- dat %>% filter(valid_correct_rt) %>% group_by(pattern, pressure, mode) %>% summarise(
    n = n(), min_rt = min(rt_s), q01 = quantile(rt_s, .01), median_rt = median(rt_s), q99 = quantile(rt_s, .99),
    max_rt = max(rt_s), n_under_100ms = sum(rt_s < .1), n_over_nominal_deadline = sum(rt_s > trial_deadline_s), .groups = "drop")
  write_csv(rt_audit, file.path(out, "correct_rt_distribution_audit.csv"))
  reliance_dat <- dat %>% filter(mode == "Automation", !timeout)
  reliance <- reliance_dat %>% group_by(participant_id, pattern, pressure, reliability, advice) %>%
    summarise(n_answered = n(), n_agree = sum(agreement), agreement = mean(agreement), .groups = "drop")
  write_csv(reliance, file.path(out, "participant_reliance.csv"))
  write_csv(describe(reliance, c("pattern", "pressure", "reliability", "advice"), "agreement"),
            file.path(out, "descriptive_reliance.csv"))
  questions <- bind_rows(lapply(selected, `[[`, "questions"))
  check(all(questions$response %in% 1:5), "Trust responses must be complete and scored 1-5")
  check(all(questions$scale_min == 1 & questions$scale_max == 5), "Unexpected trust scale")
  check(all(questions$condition_deadline_code %in% c("A_HP", "A_LP")), "Unexpected trust block")
  check(!anyDuplicated(questions[c("participant_id", "block_idx", "question_idx")]), "Duplicate trust item")
  coverage <- questions %>% group_by(participant_id, block_idx) %>%
    summarise(valid = n() == 6 && setequal(question_idx, 1:6), .groups = "drop")
  check(all(coverage$valid) && nrow(coverage) == 2 * n_distinct(dat$subject), "Incomplete six-item trust data")
  metadata <- dat %>% filter(mode == "Automation") %>% distinct(participant_id, block_idx, condition_deadline_code,
    time_pressure_condition, trial_deadline_s, automation_reliability_pattern, automation_reliability_group,
    subject, pressure, reliability, pattern)
  keys <- c("participant_id", "block_idx", "condition_deadline_code", "time_pressure_condition", "trial_deadline_s",
            "automation_reliability_pattern", "automation_reliability_group")
  check(nrow(anti_join(questions, metadata, by = keys)) == 0, "Trust metadata does not match trial blocks")
  trust <- questions %>% inner_join(metadata, by = keys) %>%
    group_by(participant_id, subject, pattern, pressure, reliability) %>% summarise(trust = mean(response), .groups = "drop")
  write_csv(trust, file.path(out, "participant_trust.csv"))
  write_csv(describe(trust, c("pattern", "pressure", "reliability"), "trust"), file.path(out, "descriptive_trust.csv"))
  check(all((dat %>% count(pattern, pressure, mode))$n > 0), "Missing performance cells")
  fits <- list(
    accuracy = fit_outcome("accuracy", accuracy ~ mode * pressure * pattern + (1 | subject), dat, TRUE, out),
    correct_rt = fit_outcome("correct_rt", log(rt_s) ~ mode * pressure * pattern + (1 | subject),
                            filter(dat, valid_correct_rt), FALSE, out),
    reliance = fit_outcome("reliance", agreement ~ pressure * reliability * advice + (1 | subject), reliance_dat, TRUE, out),
    trust = fit_outcome("trust", trust ~ pressure * reliability + (1 | subject), trust, FALSE, out))
  diagnostics <- bind_rows(lapply(fits, `[[`, "diagnostics"))
  write_csv(diagnostics, file.path(out, "model_diagnostics.csv"))
  results <- bind_rows(
    extract_results(fits$accuracy, "accuracy", ~mode * pressure * pattern, performance_contrasts, TRUE, out),
    extract_results(fits$correct_rt, "correct_rt", ~mode * pressure * pattern, performance_contrasts, FALSE, out),
    extract_results(fits$reliance, "reliance", ~pressure * reliability * advice,
                    function(g) aided_contrasts(g, TRUE), TRUE, out),
    extract_results(fits$trust, "trust", ~pressure * reliability, aided_contrasts, FALSE, out))
  write_csv(results, file.path(out, "planned_contrasts.csv"))
  perf_grid <- as.data.frame(emmeans(fits$accuracy$model, ~mode * pressure * pattern))
  reliance_grid <- as.data.frame(emmeans(fits$reliance$model, ~pressure * reliability * advice))
  trust_grid <- as.data.frame(emmeans(fits$trust$model, ~pressure * reliability))
  sensitivities <- bind_rows(
    participant_sensitivity(participant, "accuracy", perf_grid, performance_contrasts(perf_grid), "accuracy", 100),
    participant_sensitivity(participant, "mean_log_correct_rt", perf_grid, performance_contrasts(perf_grid), "correct_rt"),
    participant_sensitivity(reliance, "agreement", reliance_grid, aided_contrasts(reliance_grid, TRUE), "reliance", 100),
    participant_sensitivity(trust, "trust", trust_grid, aided_contrasts(trust_grid), "trust"))
  write_csv(sensitivities, file.path(out, "participant_sensitivity_contrasts.csv"))
  comparison <- results %>% select(outcome, contrast, model_estimate = estimate, model_p_holm = p_holm) %>%
    left_join(sensitivities %>% select(outcome, contrast, participant_estimate = estimate, participant_p_holm = p_holm),
              by = c("outcome", "contrast")) %>%
    mutate(same_direction = sign(model_estimate) == sign(participant_estimate),
           both_p_below_05 = model_p_holm < .05 & participant_p_holm < .05)
  write_csv(comparison, file.path(out, "model_participant_comparison.csv"))
  writeLines(capture.output(sessionInfo()), file.path(out, "session_info.txt"))
  p_text <- function(p) ifelse(is.na(p), "withheld", ifelse(p < .001, "< .001", sprintf("= %.3f", p)))
  result_line <- function(outcome_name, contrast_name) {
    m <- filter(results, outcome == outcome_name, contrast == contrast_name)
    s <- filter(sensitivities, outcome == outcome_name, contrast == contrast_name)
    stopifnot(nrow(m) == 1, nrow(s) == 1)
    if (outcome_name == "correct_rt") sprintf(
      "%s: model RT ratio %.3f [%.3f, %.3f], p %s; participant RT ratio %.3f [%.3f, %.3f], p %s.",
      contrast_name, m$ratio, m$ratio_lower, m$ratio_upper, p_text(m$p_holm),
      s$ratio, s$ratio_lower, s$ratio_upper, p_text(s$p_holm))
    else sprintf("%s: model %+.2f [%.2f, %.2f]; participant %+.2f [%.2f, %.2f] %s, p %s.",
                 contrast_name, m$estimate, m$lower, m$upper, s$estimate, s$lower, s$upper,
                 if (outcome_name == "trust") "trust points" else "percentage points", p_text(s$p_holm))
  }
  key_findings <- c("RESULTS AT A GLANCE", "Intervals below are pointwise 95% CIs; p-values are Holm-adjusted.",
    "Performance: trial-level mixed-model estimates are accompanied by participant-level sensitivity estimates.",
    vapply(c(paste(patterns[1], c("HP Automation - Manual", "LP Automation - Manual", "HP gain - LP gain", "Aided HP - Manual LP")),
             paste(patterns[2], c("HP Automation - Manual", "LP Automation - Manual", "HP gain - LP gain", "Aided HP - Manual LP")),
             "Difference in HP-minus-LP gains: HP95_LP65 - HP65_LP95"),
           function(x) result_line("accuracy", x), character(1)), "", "SPEED",
    vapply(c(paste(patterns[1], c("HP Automation - Manual", "LP Automation - Manual")),
             paste(patterns[2], c("HP Automation - Manual", "LP Automation - Manual"))),
           function(x) result_line("correct_rt", x), character(1)), "", "RELIANCE AND TRUST",
    vapply(c("Incorrect HP 95% - 65%", "Incorrect LP 95% - 65%"),
           function(x) result_line("reliance", x), character(1)),
    vapply(c("Trust HP 95% - 65%", "Trust LP 95% - 65%", "Trust 65% HP - LP", "Trust 95% HP - LP"),
           function(x) result_line("trust", x), character(1)), "",
    "Because accuracy/reliance models show extra participant-cell variation, interpret small model-only effects cautiously.",
    "See model_participant_comparison.csv for every agreement/disagreement between analyses.")
  report <- c("TIME-PRESSURE BEHAVIOURAL ANALYSIS", "",
    sprintf("Cohort: %d participants; %d post-calibration trials. Pilots excluded.", n_distinct(dat$subject), nrow(dat)),
    sprintf("Timeouts counted incorrect: %d. Other invalid correct RTs excluded from RT only: %d.",
            sum(dat$timeout), sum(dat$accuracy == 1 & !dat$valid_correct_rt)),
    "", key_findings, "", "METHODS AND INTERPRETATION",
    "Participant random intercepts only; no trial random effects or random slopes.",
    "Accuracy/reliance model means are conditional on a zero participant random effect, not population averages.",
    "Descriptive means weight participants equally. Pointwise 95% CIs; two-sided p-values Holm-adjusted within outcome.",
    "Accuracy and reliance contrasts are probability differences in percentage points.",
    "RT inference uses log RT: exponentiated simple contrasts are geometric-mean ratios; interaction contrasts are ratios of ratios.",
    "Descriptive RT plots show arithmetic means of valid correct responses. Deadline and correctness selection restrict interpretation.",
    "Reliance is agreement on answered trials, stratified by advice correctness; it does not identify advice-caused changes.",
    "Reliability comparisons at fixed pressure compare assignment groups, not the same participant at both reliabilities.",
    "Trust is the mean of all six positively worded 1-5 items. Perceived-aid-accuracy sliders are analysed separately in exploratory/H3.",
    "Aided HP - Manual LP estimates the remaining accuracy gap; a nonsignificant gap is not equivalence/noninferiority.",
    "", "MODEL DIAGNOSTICS", capture.output(print(as.data.frame(diagnostics), row.names = FALSE)),
    "Conditional cell checks simulate independent binomial counts at fitted participant-cell probabilities, without refitting.",
    "They are lack-of-fit diagnostics, not formal dispersion tests. Small p-values indicate unmodelled participant-by-condition variation or trial dependence.",
    "DHARMa is not installed; no DHARMa residual diagnostics were performed. Random-intercept-only intervals may be optimistic if cell variation remains.",
    "Correct-RT Q-Q plots and cell residual SDs must be inspected for non-normal tails and unequal variances; no additional RT trimming is applied.",
    sprintf("RT audit: %d valid correct responses below 100 ms; %d beyond the nominal deadline. These are reported, not automatically excluded.",
            sum(rt_audit$n_under_100ms), sum(rt_audit$n_over_nominal_deadline)),
    "Participant sensitivity tests use paired contrast scores within each assignment group and Welch-Satterthwaite tests across independent groups.",
    "These tests weight participants equally, allow arbitrary within-person covariance, and do not assume independent trials.",
    "Their accuracy/RT estimands differ from conditional/trial-weighted mixed-model estimates; discrepancies must be reported, not selected by significance.",
    "Nonconverged or singular models have inferential p-values withheld. Estimates/CIs from these fits require caution.",
    "", "PLANNED CONTRASTS", capture.output(print(as.data.frame(results), row.names = FALSE, digits = 5)),
    "", "PARTICIPANT SENSITIVITY CONTRASTS", capture.output(print(as.data.frame(sensitivities), row.names = FALSE, digits = 5)),
    "", "DESCRIPTIVE PARTICIPANT MEANS", capture.output(print(as.data.frame(group), row.names = FALSE, digits = 4)))
  writeLines(report, file.path(out, "results_report.txt"))
  escaped <- gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", report, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  writeLines(c('<!doctype html><meta charset="utf-8"><title>Time-pressure behavioural analysis</title>',
               '<style>body{font:15px/1.55 system-ui;margin:40px;color:#1c2d3e}pre{white-space:pre-wrap;overflow-wrap:anywhere;font-size:13px}</style>',
               '<p><a href="exploratory/exploratory_report.html">Six additional exploratory analyses</a></p>',
               "<pre>", escaped, "</pre>"), file.path(out, "results_report.html"))
  source(file.path(root, "analyse_time_pressure_exploratory.R"), local = TRUE)
  exploratory_main(out, root)
  writeLines(c(paste("Completed", format(Sys.time(), tz = "UTC")),
               paste("All models numerically usable:", all(diagnostics$inference_available))), file.path(out, "COMPLETE.txt"))
  message("Analysis saved to ", out)
  invisible(list(results = results, diagnostics = diagnostics))
}

if (sys.nframe() == 0) main()
