# Six exploratory extensions. Reuses the validated main cohort and model helpers.
# Standalone: Rscript analyse_time_pressure_exploratory.R [base_analysis_dir]

make_sequence <- function(dat) {
  # Lag BEFORE removing timeouts; never bridge missing trials or block boundaries.
  dat %>% filter(mode == "Automation") %>% arrange(participant_id, run_timestamp, block_idx, trial) %>%
    group_by(participant_id, run_timestamp, block_idx) %>% mutate(
      previous_trial = lag(trial), previous_timeout = lag(timeout),
      previous_aid_error = factor(ifelse(lag(advice) == "Incorrect", "Error", "Correct"), c("Correct", "Error")),
      previous_response_error = as.integer(lag(accuracy) == 0),
      progress = (trial - 200.5) / 399) %>% ungroup() %>%
    filter(!timeout, !is.na(previous_trial), previous_trial == trial - 1, !previous_timeout)
}

benchmark_components <- function(dat) {
  dat %>% filter(mode == "Automation") %>% group_by(participant_id, pattern, subject, pressure, reliability) %>%
    summarise(n_trials = n(), human_accuracy = mean(accuracy), aid_accuracy = mean(advice == "Correct"),
      rescued = mean(advice == "Incorrect" & accuracy == 1),
      spoiled = mean(advice == "Correct" & !timeout & accuracy == 0),
      missed_correct_advice = mean(advice == "Correct" & timeout), .groups = "drop") %>%
    mutate(net_gain = human_accuracy - aid_accuracy)
}

completion_components <- function(dat) {
  block <- dat %>% group_by(participant_id, subject, pattern, pressure, mode) %>% summarise(
    completion = mean(!timeout), answered_accuracy = mean(accuracy[!timeout]), total_accuracy = mean(accuracy), .groups = "drop")
  wide <- block %>% pivot_wider(names_from = mode, values_from = c(completion, answered_accuracy, total_accuracy))
  wide %>% mutate(
    completion_component = (completion_Automation - completion_Manual) * (answered_accuracy_Automation + answered_accuracy_Manual) / 2,
    choice_component = (answered_accuracy_Automation - answered_accuracy_Manual) * (completion_Automation + completion_Manual) / 2,
    total_gain = total_accuracy_Automation - total_accuracy_Manual,
    reliability = factor(ifelse((pattern == "HP95_LP65" & pressure == "HP") |
      (pattern == "HP65_LP95" & pressure == "LP"), "95%", "65%"), c("65%", "95%")))
}

cell_contrasts <- function(grid, zero_tests = FALSE) {
  methods <- aided_contrasts(grid)
  names(methods) <- sub("^Trust ", "", names(methods))
  if (zero_tests) for (p in c("HP", "LP")) for (r in c("65%", "95%"))
    methods[[paste(p, r, "versus zero")]] <- as.numeric(grid$pressure == p & grid$reliability == r)
  methods
}

selectivity_contrasts <- function(grid) {
  # Lift each pressure x reliability contrast to Correct minus Incorrect advice.
  coarse <- unique(grid[c("pressure", "reliability")])
  methods <- cell_contrasts(coarse)
  index <- match(paste(grid$pressure, grid$reliability), paste(coarse$pressure, coarse$reliability))
  lapply(methods, function(w) w[index] * ifelse(grid$advice == "Correct", 1, -1))
}

trend_contrasts <- function(grid) {
  methods <- list()
  for (a in c("Incorrect", "Correct")) {
    for (p in c("HP", "LP")) methods[[paste(a, p, "slope")]] <- as.numeric(grid$advice == a & grid$pressure == p)
    methods[[paste(a, "HP - LP slope")]] <- as.numeric(grid$advice == a) * ifelse(grid$pressure == "HP", 1, -1)
  }
  methods
}

lag_contrasts <- function(grid) {
  # Standardise current advice correctness to its assigned probability within reliability.
  cell <- function(p, r) as.numeric(grid$pressure == p & grid$reliability == r) *
    ifelse(grid$previous_aid_error == "Error", 1, -1) *
    ifelse(grid$advice == "Correct", ifelse(grid$reliability == "95%", .95, .65),
           ifelse(grid$reliability == "95%", .05, .35))
  out <- list()
  for (p in c("HP", "LP")) for (r in c("65%", "95%")) out[[paste(p, r, "After error - after correct advice")]] <- cell(p, r)
  for (r in c("65%", "95%")) out[[paste(r, "HP - LP adjustment")]] <- cell("HP", r) - cell("LP", r)
  out[["Difference in HP-minus-LP adjustment: 95% - 65%"]] <- cell("HP", "95%") - cell("LP", "95%") - cell("HP", "65%") + cell("LP", "65%")
  out
}

fit_exploratory <- function(name, formula, data, binary_outcome, out) {
  message("Exploratory fit: ", name, " (", nrow(data), " rows)")
  warns <- character()
  model <- withCallingHandlers(if (binary_outcome)
    lme4::glmer(formula, data = data, family = binomial(),
      control = strict_model_control(TRUE))
    else lmerTest::lmer(formula, data = data,
      control = strict_model_control(FALSE)),
    warning = function(w) {warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning")})
  messages <- model@optinfo$conv$lme4$messages
  converged <- is.null(messages) && all(model@optinfo$conv$opt == 0) && all(is.finite(fixef(model)))
  singular <- isSingular(model, tol = 1e-4)
  rank_deficient <- !is.null(attr(getME(model, "X"), "col.dropped"))
  gradient <- tryCatch(max(abs(solve(chol(model@optinfo$derivs$Hessian), model@optinfo$derivs$gradient))), error = function(e) NA_real_)
  diag <- tibble(model = name, n_observations = nrow(data), n_participants = n_distinct(data$subject),
    converged = converged, singular = singular, rank_deficient = rank_deficient,
    inference_available = converged && !singular && !rank_deficient,
    max_scaled_gradient = gradient, warnings = paste(unique(warns), collapse = " | "),
    convergence_message = paste(messages, collapse = " | "))
  if (binary_outcome) {
    checks <- data %>% mutate(.p = fitted(model), .y = model.response(model.frame(model))) %>%
      group_by(across(all_of(intersect(c("subject", "mode", "pattern", "pressure", "reliability", "advice"), all.vars(formula))))) %>%
      summarise(n = n(), observed = sum(.y), expected = sum(.p), variance = sum(.p * (1-.p)), .groups = "drop")
    diag$conditional_cell_dispersion <- mean((checks$observed - checks$expected)^2 / pmax(checks$variance, 1e-10))
    diag$extreme_fitted_probability <- any(fitted(model) < 1e-6 | fitted(model) > 1 - 1e-6)
    write_csv(checks, file.path(out, paste0(name, "_cell_checks.csv")))
  } else {
    qq <- ggplot(tibble(residual = residuals(model)), aes(sample = residual)) + stat_qq() + stat_qq_line() +
      theme_minimal() + labs(title = paste(name, "residual Q-Q plot"))
    ggsave(file.path(out, paste0(name, "_residual_qq.png")), qq, width = 7, height = 5, dpi = 120)
  }
  saveRDS(model, file.path(out, "models", paste0(name, ".rds")))
  writeLines(capture.output(summary(model)), file.path(out, paste0(name, "_model_summary.txt")))
  list(model = model, diagnostics = diag)
}

exploratory_table <- function(emm, methods, hypothesis, outcome, analysis, units, scale = 1, usable = TRUE, out = NULL) {
  tab <- as.data.frame(summary(contrast(emm, methods), infer = c(TRUE, TRUE), adjust = "none"))
  lo <- intersect(c("lower.CL", "asymp.LCL"), names(tab)); hi <- intersect(c("upper.CL", "asymp.UCL"), names(tab))
  ans <- tibble(hypothesis = hypothesis, outcome = outcome, analysis = analysis, contrast = tab$contrast,
    estimate = tab$estimate * scale, lower = tab[[lo]] * scale, upper = tab[[hi]] * scale,
    se = tab$SE * scale, df = tab$df, p_raw = if (usable) tab$p.value else NA_real_,
    units = units, inference_available = usable)
  if (!is.null(out)) {
    key <- paste(hypothesis, outcome, analysis, sep = "_")
    write_csv(as.data.frame(emm), file.path(out, paste0(key, "_estimates.csv")))
    write_csv(bind_rows(lapply(names(methods), function(nm) as.data.frame(emm) %>% mutate(contrast = nm, weight = methods[[nm]]))),
              file.path(out, paste0(key, "_weights.csv")))
  }
  ans
}

cell_analysis <- function(fit, participant, value, hypothesis, outcome, out, scale = 100, zero_tests = FALSE) {
  emm <- emmeans(fit$model, ~pressure * reliability, lmer.df = "satterthwaite")
  grid <- as.data.frame(emm); methods <- cell_contrasts(grid, zero_tests)
  model <- exploratory_table(emm, methods, hypothesis, outcome, "mixed_model", if (scale == 100) "percentage points" else "rating percentage points",
    scale, fit$diagnostics$inference_available, out)
  sens <- participant_sensitivity(participant, value, grid, methods, outcome, scale) %>%
    transmute(hypothesis = hypothesis, outcome, analysis = "participant_sensitivity", contrast, estimate, lower, upper, se, df, p_raw,
      units = model$units[1], inference_available = TRUE)
  bind_rows(model, sens)
}

association_analysis <- function(data, block_scores, score_name, unit_scale, hypothesis, out) {
  # Decompose ratings into person mean and block deviation; two blocks per participant.
  score_table <- block_scores %>% group_by(participant_id) %>%
    mutate(score_between = mean(.data[[score_name]]) / unit_scale,
           score_within = (.data[[score_name]] - mean(.data[[score_name]])) / unit_scale) %>% ungroup()
  score_table$score_between <- score_table$score_between - mean(score_table$score_between)
  data <- data %>% left_join(score_table %>% select(participant_id, pressure, score_within, score_between),
                             by = c("participant_id", "pressure")) %>% mutate(block_position = block_idx - 3.5)
  check(!anyNA(data$score_within), paste("Missing", score_name, "scores"))
  formula <- agreement ~ pressure * reliability * advice + (score_within + score_between) * pressure * advice + block_position + (1 | subject)
  fit <- fit_exploratory(paste0(hypothesis, "_", score_name, "_agreement"), formula, data, TRUE, out)
  # Sensitivity: equal-weight participant x block x advice proportions; subject-clustered covariance.
  cells <- data %>% group_by(participant_id, subject, pressure, reliability, advice, score_within, score_between, block_position) %>%
    summarise(agreement = mean(agreement), n_answered = n(), .groups = "drop")
  lm_fit <- lm(lme4::nobars(formula), data = cells)
  vc <- sandwich::vcovCL(lm_fit, cluster = cells$participant_id, type = "HC1")
  saveRDS(lm_fit, file.path(out, "models", paste0(hypothesis, "_", score_name, "_participant_lm.rds")))
  write_csv(cells, file.path(out, paste0(hypothesis, "_", score_name, "_participant_cells.csv")))
  tables <- list()
  for (component in c("score_within", "score_between")) {
    em <- emtrends(fit$model, ~pressure * advice, var = component)
    methods <- trend_contrasts(as.data.frame(em))
    tables[[paste(component, "model")]] <- exploratory_table(em, methods, hypothesis, paste(score_name, component, sep = "_"),
      "mixed_model", paste("log odds per", unit_scale, if (score_name == "trust") "trust point" else "rating points"),
      usable = fit$diagnostics$inference_available, out = out)
    es <- emtrends(lm_fit, ~pressure * advice, var = component, vcov. = vc, df = n_distinct(cells$participant_id) - 1)
    tables[[paste(component, "sensitivity")]] <- exploratory_table(es, trend_contrasts(as.data.frame(es)), hypothesis,
      paste(score_name, component, sep = "_"), "participant_cluster_sensitivity",
      paste("agreement percentage points per", unit_scale, if (score_name == "trust") "trust point" else "rating points"),
      scale = 100, out = out)
  }
  # Predictions only within the observed score range in each cell; no extrapolated endpoints.
  predictions <- cells %>% group_by(pressure, reliability, advice) %>%
    summarise(score_min = quantile(score_within, .10), score_max = quantile(score_within, .90), .groups = "drop")
  predictions <- bind_rows(lapply(seq_len(nrow(predictions)), function(i) {
    row <- predictions[i, ]; nd <- row[rep(1, 40), ]; nd$score_within <- seq(row$score_min, row$score_max, length.out = 40)
    nd$score_between <- 0; nd$block_position <- 0; nd$subject <- data$subject[1]
    nd$probability <- predict(fit$model, newdata = nd, type = "response", re.form = NA)
    frame <- model.frame(fit$model)
    for (v in intersect(names(nd), names(frame))) if (is.factor(frame[[v]])) nd[[v]] <- factor(nd[[v]], levels = levels(frame[[v]]))
    design <- model.matrix(delete.response(terms(nobars(formula))), nd,
                           contrasts.arg = attr(getME(fit$model, "X"), "contrasts"))
    design <- design[, names(fixef(fit$model)), drop = FALSE]
    eta <- as.numeric(design %*% fixef(fit$model))
    se <- sqrt(pmax(0, rowSums((design %*% as.matrix(vcov(fit$model))) * design)))
    nd$lower <- plogis(eta - 1.96 * se); nd$upper <- plogis(eta + 1.96 * se)
    nd$score_deviation <- nd$score_within * unit_scale; nd
  }))
  write_csv(predictions, file.path(out, paste0(hypothesis, "_", score_name, "_predictions.csv")))
  list(results = bind_rows(tables), diagnostics = fit$diagnostics)
}

exploratory_main <- function(base, root) {
  check(requireNamespace("sandwich", quietly = TRUE), "Install sandwich for participant-cluster sensitivity checks")
  out <- file.path(base, "exploratory")
  dir.create(file.path(out, "models"), recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(out, c("COMPLETE.txt", "validation.txt", "visual_qa.txt")))
  manifest <- read_csv(file.path(base, "input_manifest.csv"), show_col_types = FALSE)
  check(identical(unname(tools::md5sum(manifest$path)), manifest$md5), "Base analysis input hashes changed")
  raw <- bind_rows(lapply(manifest$path[manifest$kind == "trials"], read_csv, show_col_types = FALSE))
  dat <- prepare_trials(raw)
  aided <- dat %>% filter(mode == "Automation")
  answered <- aided %>% filter(!timeout)
  perf <- read_csv(file.path(base, "participant_performance.csv"), show_col_types = FALSE)
  reliance <- read_csv(file.path(base, "participant_reliance.csv"), show_col_types = FALSE)
  trust <- read_csv(file.path(base, "participant_trust.csv"), show_col_types = FALSE)
  block_meta <- aided %>% distinct(participant_id, run_timestamp, block_idx, condition_deadline_code, pressure, reliability, pattern, subject,
    time_pressure_condition, trial_deadline_s, automation_reliability_pattern, automation_reliability_group)
  slider_paths <- sub("_ALL.csv$", "_POSTBLOCK_SLIDERS_ALL.csv", manifest$path[manifest$kind == "trials"])
  check(all(file.exists(slider_paths)), "Missing same-run sliders")
  sliders <- bind_rows(lapply(slider_paths, read_csv, show_col_types = FALSE)) %>% filter(question_key == "perc_auto_correct")
  keys <- c("participant_id", "run_timestamp", "block_idx", "condition_deadline_code", "time_pressure_condition", "trial_deadline_s",
            "automation_reliability_pattern", "automation_reliability_group")
  check(!anyDuplicated(sliders[c("participant_id", "run_timestamp", "block_idx")]), "Duplicate aid-reliability ratings")
  check(all(is.finite(sliders$response_percent) & sliders$response_percent >= 0 & sliders$response_percent <= 100), "Invalid aid-reliability rating")
  check(nrow(anti_join(sliders, block_meta, by = keys)) == 0 && nrow(sliders) == nrow(block_meta), "Slider/run coverage mismatch")
  slider_manifest <- tibble(kind = "perceived_reliability", path = normalizePath(slider_paths), md5 = unname(tools::md5sum(slider_paths)))
  write_csv(bind_rows(manifest %>% select(kind, path, md5), slider_manifest), file.path(out, "input_manifest.csv"))
  sources <- file.path(root, c("analyse_time_pressure.R", "analyse_time_pressure_exploratory.R"))
  write_csv(tibble(path = sources, md5 = unname(tools::md5sum(sources))), file.path(out, "source_checksums.csv"))
  hypotheses <- tibble(hypothesis = paste0("H", 1:6), question = c("Selective agreement", "Value beyond always following the aid",
    "Awareness of aid reliability", "Trust-behaviour association", "Next-trial adjustment after aid errors", "Completion versus choice accuracy"),
    status = "Exploratory: specified after inspecting the main results")
  write_csv(hypotheses, file.path(out, "hypothesis_registry.csv"))
  results <- list(); diagnostics <- list()

  # H1: probability-scale interaction of pressure, reliability and advice correctness.
  rel_model <- readRDS(file.path(base, "models/reliance.rds"))
  rel_diag <- read_csv(file.path(base, "model_diagnostics.csv"), show_col_types = FALSE) %>% filter(outcome == "reliance")
  em <- regrid(emmeans(rel_model, ~pressure * reliability * advice), transform = "response")
  methods <- selectivity_contrasts(as.data.frame(em))
  results$H1_model <- exploratory_table(em, methods, "H1", "selectivity", "mixed_model", "percentage points", 100, rel_diag$inference_available, out)
  sel <- reliance %>% select(participant_id, pattern, pressure, reliability, advice, agreement) %>%
    pivot_wider(names_from = advice, values_from = agreement) %>% mutate(selectivity = Correct - Incorrect)
  write_csv(sel, file.path(out, "H1_participant_selectivity.csv"))
  grid <- unique(as.data.frame(em)[c("pressure", "reliability")])
  results$H1_sensitivity <- participant_sensitivity(sel, "selectivity", grid, cell_contrasts(grid), "selectivity", 100) %>%
    mutate(hypothesis = "H1", analysis = "participant_sensitivity", inference_available = TRUE)

  # H2: exact accounting identity, with timeouts on correct advice as a separate loss.
  benchmark <- benchmark_components(dat)
  check(max(abs(benchmark$net_gain - benchmark$rescued + benchmark$spoiled + benchmark$missed_correct_advice)) < 1e-12,
        "Aid benchmark decomposition failed")
  write_csv(benchmark, file.path(out, "H2_participant_benchmark.csv"))
  fit <- fit_exploratory("H2_net_gain", net_gain ~ pressure * reliability + (1 | subject), benchmark, FALSE, out)
  diagnostics$H2 <- fit$diagnostics
  results$H2 <- cell_analysis(fit, benchmark, "net_gain", "H2", "net_gain", out, zero_tests = TRUE)

  # H3: bias, absolute calibration error, perceived discrimination, and behaviour.
  ratings <- inner_join(sliders, block_meta, by = keys) %>%
    left_join(benchmark %>% select(participant_id, pressure, aid_accuracy), by = c("participant_id", "pressure")) %>%
    mutate(perceived = response_percent, signed_error = perceived - 100 * aid_accuracy, absolute_error = abs(signed_error))
  write_csv(ratings, file.path(out, "H3_participant_calibration.csv"))
  for (value in c("signed_error", "absolute_error", "perceived")) {
    fit <- fit_exploratory(paste0("H3_", value), as.formula(paste(value, "~ pressure * reliability + (1 | subject)")), ratings, FALSE, out)
    diagnostics[[paste0("H3_", value)]] <- fit$diagnostics
    results[[paste0("H3_", value)]] <- cell_analysis(fit, ratings, value, "H3", value, out, scale = 1, zero_tests = value == "signed_error")
  }
  association <- association_analysis(answered, ratings, "perceived", 10, "H3", out)
  results$H3_association <- association$results; diagnostics$H3_association <- association$diagnostics

  # H4: trust ratings are measured AFTER each block; these are associations.
  association <- association_analysis(answered, trust, "trust", 1, "H4", out)
  results$H4 <- association$results; diagnostics$H4 <- association$diagnostics

  # H5: one-trial lag, previous response outcome and within-block time controlled.
  seqdat <- make_sequence(dat)
  write_csv(seqdat %>% count(pattern, pressure, reliability, previous_aid_error, previous_response_error, advice), file.path(out, "H5_sequence_coverage.csv"))
  write_csv(seqdat %>% group_by(participant_id, pattern, pressure, reliability, previous_aid_error) %>%
    summarise(n = n(), agreement = mean(agreement), .groups = "drop"), file.path(out, "H5_participant_sequence.csv"))
  formula <- agreement ~ pressure * reliability * advice + previous_aid_error * pressure * reliability + previous_response_error + progress + (1 | subject)
  fit <- fit_exploratory("H5_next_trial", formula, seqdat, TRUE, out)
  diagnostics$H5 <- fit$diagnostics
  em <- regrid(emmeans(fit$model, ~previous_aid_error * pressure * reliability * advice,
    at = list(previous_response_error = mean(seqdat$previous_response_error), progress = 0)), transform = "response")
  results$H5_model <- exploratory_table(em, lag_contrasts(as.data.frame(em)), "H5", "next_trial_agreement", "mixed_model",
    "percentage points", 100, fit$diagnostics$inference_available, out)
  seqdat <- seqdat %>% group_by(participant_id, block_idx) %>% mutate(block_weight = 1 / n()) %>% ungroup()
  lm_fit <- lm(nobars(formula), data = seqdat, weights = block_weight)
  vc <- sandwich::vcovCL(lm_fit, cluster = seqdat$participant_id, type = "HC1")
  saveRDS(lm_fit, file.path(out, "models/H5_participant_cluster_lm.rds"))
  es <- emmeans(lm_fit, ~previous_aid_error * pressure * reliability * advice, vcov. = vc,
    df = n_distinct(seqdat$participant_id) - 1, at = list(previous_response_error = mean(seqdat$previous_response_error), progress = 0))
  results$H5_sensitivity <- exploratory_table(es, lag_contrasts(as.data.frame(es)), "H5", "next_trial_agreement",
    "participant_cluster_sensitivity", "percentage points", 100, out = out)

  # H6: binary models and an exact symmetric decomposition of each paired accuracy gain.
  for (value in c("timeout", "answered_accuracy")) {
    x <- if (value == "timeout") dat %>% mutate(y = as.integer(timeout)) else dat %>% filter(!timeout) %>% mutate(y = accuracy)
    fit <- fit_exploratory(paste0("H6_", value), y ~ mode * pressure * pattern + (1 | subject), x, TRUE, out)
    diagnostics[[paste0("H6_", value)]] <- fit$diagnostics
    em <- regrid(emmeans(fit$model, ~mode * pressure * pattern), transform = "response")
    methods <- performance_contrasts(as.data.frame(em))
    results[[paste0("H6_", value, "_model")]] <- exploratory_table(em, methods, "H6", value, "mixed_model", "percentage points", 100,
      fit$diagnostics$inference_available, out)
    cells <- x %>% group_by(participant_id, pattern, mode, pressure) %>% summarise(y = mean(y), .groups = "drop")
    results[[paste0("H6_", value, "_sensitivity")]] <- participant_sensitivity(cells, "y", as.data.frame(em), methods, value, 100) %>%
      mutate(hypothesis = "H6", analysis = "participant_sensitivity", inference_available = TRUE)
  }
  completion <- completion_components(dat)
  check(max(abs(completion$total_gain - completion$completion_component - completion$choice_component)) < 1e-12,
        "Completion/choice decomposition failed")
  write_csv(completion, file.path(out, "H6_participant_decomposition.csv"))

  all_results <- bind_rows(results) %>% select(hypothesis, outcome, analysis, contrast, estimate, lower, upper, se, df, p_raw, units, inference_available) %>%
    mutate(analysis_family = ifelse(analysis == "mixed_model", "mixed_model", "sensitivity")) %>%
    group_by(hypothesis, analysis_family) %>% mutate(p_holm = p.adjust(p_raw, "holm")) %>% ungroup() %>%
    # An additional correction across all six exploratory questions is provided.
    group_by(analysis_family) %>% mutate(p_holm_all_six = p.adjust(p_raw, "holm")) %>% ungroup()
  write_csv(all_results, file.path(out, "exploratory_contrasts.csv"))
  write_csv(bind_rows(diagnostics), file.path(out, "model_diagnostics.csv"))
  write_csv(all_results %>% filter(analysis == "mixed_model") %>% select(hypothesis, outcome, contrast, model_estimate = estimate, model_p = p_holm) %>%
    left_join(all_results %>% filter(analysis != "mixed_model") %>% select(hypothesis, outcome, contrast, sensitivity_estimate = estimate, sensitivity_p = p_holm),
      by = c("hypothesis", "outcome", "contrast")) %>% mutate(same_direction = sign(model_estimate) == sign(sensitivity_estimate),
        both_p_below_05 = model_p < .05 & sensitivity_p < .05), file.path(out, "model_sensitivity_comparison.csv"))
  notes <- c("SIX EXPLORATORY TIME-PRESSURE ANALYSES", "",
    "These questions were added after the main results were examined. They are exploratory, not preregistered confirmations.",
    "Two-sided tests; pointwise 95% CIs. Holm correction within question and analysis method; an all-six Holm column is also supplied.",
    "H1: selective agreement = P(agree | correct advice, answered) - P(agree | incorrect advice, answered). Not a direct measure of verification.",
    "H2: human accuracy minus realised aid accuracy on all trials. Rescue - spoiling - timeout on correct advice equals net gain exactly.",
    "The always-follow benchmark assumes a timely response. No pre-advice decision was recorded, so rescue/spoiling are outcome categories, not observed changes of mind.",
    "H3: post-block perceived aid accuracy minus realised accuracy; analyse signed and absolute errors separately. Perceived-discrimination tests compare 95% vs 65% ratings at each pressure.",
    "H3/H4 rating-behaviour models separate participant mean rating from each block's deviation, control pressure/reliability/advice correctness and block position.",
    "Their slopes are stratified by pressure and advice correctness and averaged equally over reliability. Model slopes are log odds; sensitivity slopes are probability differences.",
    "Within-person slopes use two blocks per participant. Post-block ratings support associations, not causal mediation or prediction of future behaviour.",
    "H5: next-trial agreement following an aid error vs correct advice; restrict to consecutive answered trials within the same block.",
    "Previous participant error and linear trial position are controlled. Timeouts are removed only AFTER lags are constructed.",
    "H5 probability differences standardise current advice correctness to assigned reliability; previous response error is fixed at its observed mean, progress at block midpoint.",
    "An error may have been inferable from binary feedback but need not have been noticed. Lag associations do not establish trust updating.",
    "H6: timeout and answered-trial accuracy models distinguish completion from choice accuracy. Conditioning on completion changes the estimand.",
    "The symmetric decomposition averages the two possible product decompositions: delta(response rate)*average(answered accuracy) + delta(answered accuracy)*average(response rate).",
    "This is an accounting identity, not causal mediation. Its uncertainty is summarised over paired participant scores.",
    "Cell-mean sensitivity tests preserve pairing within each allocation group and use Welch comparisons across groups.",
    sprintf("H3/H4 sensitivity: equal-weight participant-block-advice proportions with participant-cluster HC1 covariance and %d df for this cohort.", n_distinct(dat$subject) - 1),
    "H5 sensitivity: linear probability model with equal total weight per participant block and participant-cluster HC1 covariance.",
    "Full lme4 derivative checks are enabled regardless of observation/parameter counts, with bobyqa rhoend=1e-9.",
    "Numeric convergence is distinct from model adequacy. Conditional cell dispersion is descriptive; no DHARMa tests were run.",
    "Singular, rank-deficient or nonconverged mixed models retain estimates but have p-values withheld; sensitivity results are labelled separately.")
  report <- c(notes, "", "MODEL DIAGNOSTICS", capture.output(print(as.data.frame(bind_rows(diagnostics)), row.names = FALSE)),
    unlist(lapply(paste0("H", 1:6), function(h) c("", paste(h, hypotheses$question[hypotheses$hypothesis == h]),
      capture.output(print(as.data.frame(filter(all_results, hypothesis == h) %>% select(outcome, analysis, contrast, estimate, lower, upper, p_holm, p_holm_all_six, units)), row.names = FALSE, digits = 4))))))
  writeLines(report, file.path(out, "exploratory_report.txt"))
  escaped <- gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", report, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  writeLines(c('<!doctype html><meta charset="utf-8"><title>Exploratory time-pressure analyses</title>',
    '<style>body{font:15px/1.5 system-ui;margin:40px;color:#1c2d3e}pre{white-space:pre-wrap;overflow-wrap:anywhere;font-size:13px}</style>',
    '<h1>Six exploratory analyses</h1><pre>', escaped, '</pre>'), file.path(out, "exploratory_report.html"))
  writeLines(capture.output(sessionInfo()), file.path(out, "session_info.txt"))
  writeLines(paste("Completed", format(Sys.time(), tz = "UTC")), file.path(out, "COMPLETE.txt"))
  message("Exploratory results saved to ", out)
  invisible(all_results)
}

if (sys.nframe() == 0) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
  root <- dirname(normalizePath(script)); args <- commandArgs(trailingOnly = TRUE)
  check_args <- length(args) <= 1; if (!check_args) stop("Usage: Rscript analyse_time_pressure_exploratory.R [base_analysis_dir]")
  source(file.path(root, "analyse_time_pressure.R"))
  exploratory_main(if (length(args)) args[1] else file.path(root, "analysis_outputs/semester2_2026_behavioural"), root)
}
