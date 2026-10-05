# Retrospectively specified behavioural analyses of the 95 -> 70 -> 95 sequence.
# Usage: Rscript analyse_dynamic_reliability.R [raw_input_dir] [output_dir]
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lme4)
  library(lmerTest); library(emmeans); library(ggplot2)
})
GROUPS <- c("CAL65", "CAL90")
STAGES <- c("Manual pre", "P1", "P2", "P3", "Manual post")
ADVICE <- c("Correct", "Incorrect")
check <- function(x, message) if (!isTRUE(x)) stop(message, call. = FALSE)
binary <- function(x, label) {
  z <- tolower(trimws(as.character(x)))
  check(all(is.na(z) | z %in% c("true", "false", "0", "1")), paste("Invalid binary", label))
  ifelse(is.na(z), NA_integer_, as.integer(z %in% c("true", "1")))
}
read_table <- function(path) read_csv(path, show_col_types = FALSE)
hash_table <- function(paths) tibble(path = normalizePath(paths), md5 = unname(tools::md5sum(paths)))

prepare_trials <- function(raw) {
  required <- c("participant_id", "run_timestamp", "block", "block_idx", "trial", "global_trial",
    "calibration_target_group", "calibration_target_accuracy", "main_block_order", "manual_segment",
    "reliability_phase_idx", "trial_in_reliability_phase", "reliability_phase_label", "aid_reliability_level",
    "automation_reliability_group", "condition_code", "key_black", "key_white", "trial_deadline_s",
    "response", "stimulus", "aid_label", "aid_correct", "correct", "rt_s")
  check(all(required %in% names(raw)), "Missing required trial columns")
  check(!anyDuplicated(raw[c("participant_id", "run_timestamp", "block_idx", "trial")]), "Duplicate trial keys")
  check(all(is.finite(raw$participant_id) & raw$participant_id >= 1 & raw$participant_id %% 1 == 0), "Invalid participant IDs")
  check(all(raw$response %in% c("BLACK", "WHITE", "TIMEOUT")), "Invalid response")
  check(all(raw$stimulus %in% c("BLACK", "WHITE")), "Invalid stimulus")
  correct <- binary(raw$correct, "correctness"); aid <- binary(raw$aid_correct, "advice")
  timeout <- raw$response == "TIMEOUT"; aided <- raw$block == "AUTOMATION"
  check(all(!is.na(correct[!timeout])) && all(correct[!timeout] == as.integer(raw$response[!timeout] == raw$stimulus[!timeout])),
        "Answered correctness disagrees with stimulus/response")
  check(all(is.na(correct[timeout]) | correct[timeout] == 0), "Timeout marked correct")
  check(all(raw$aid_label[aided] %in% c("BLACK", "WHITE")) &&
          all(aid[aided] == as.integer(raw$aid_label[aided] == raw$stimulus[aided])), "Invalid advice correctness")
  check(all(is.na(aid[!aided])) && all(is.na(raw$aid_label[!aided])), "Unexpected manual/calibration advice")
  for (pid in unique(raw$participant_id)) {
    d <- raw[raw$participant_id == pid, ]
    check(n_distinct(d$run_timestamp) == 1 && !anyNA(d$run_timestamp), "Ambiguous duplicate participant runs")
    check(nrow(d) == 1900 && setequal(d$global_trial, 1:1900), "Incomplete global trial sequence")
    expected_group <- if (pid %% 2 == 1) "CAL65" else "CAL90"
    flip <- ((pid - 1) %/% 2) %% 2 == 1
    check(all(d$calibration_target_group == expected_group) &&
      all(d$calibration_target_accuracy == if (expected_group == "CAL65") .65 else .9) &&
      all(d$main_block_order == "SPLIT_MANUAL") && all(d$trial_deadline_s == 5) &&
      all(d$key_black == if (flip) "J" else "D") && all(d$key_white == if (flip) "D" else "J"),
      "Allocation, key mapping, target, or deadline mismatch")
    for (b in 1:4) {
      x <- d[d$block_idx == b, ]; n <- c(300L, 200L, 1200L, 200L)[b]
      check(nrow(x) == n && setequal(x$trial, seq_len(n)) &&
        all(x$global_trial == x$trial + c(0, 300, 500, 1700)[b]) &&
        all(x$block == c("CALIBRATION", "MANUAL", "AUTOMATION", "MANUAL")[b]) &&
        all(x$condition_code == c("CAL", "MAN", "REL_DROP", "MAN")[b]), "Block sequence/count mismatch")
      if (b %in% c(2, 4)) check(all(x$manual_segment == if (b == 2) "PRE_AUTOMATION" else "POST_AUTOMATION"), "Manual segment mismatch")
      else check(all(is.na(x$manual_segment)), "Unexpected manual segment")
      if (b == 3) {
        phase <- (x$trial - 1L) %/% 400L + 1L
        check(all(x$reliability_phase_idx == phase) &&
          all(x$trial_in_reliability_phase == (x$trial - 1L) %% 400L + 1L) &&
          all(x$reliability_phase_label == c("P1_95", "P2_70", "P3_95")[phase]) &&
          all(x$aid_reliability_level == c(.95, .7, .95)[phase]) &&
          all(x$automation_reliability_group == ifelse(phase == 2, "low", "high")), "Aided phase metadata mismatch")
      } else check(all(is.na(x$reliability_phase_idx)) && all(is.na(x$reliability_phase_label)) &&
        all(is.na(x$trial_in_reliability_phase)) && all(is.na(x$aid_reliability_level)), "Unexpected non-aided phase metadata")
    }
  }
  raw %>% mutate(subject = factor(participant_id), group = factor(calibration_target_group, GROUPS),
    stage = factor(case_when(block_idx == 2 ~ "Manual pre", block_idx == 4 ~ "Manual post",
      aided ~ paste0("P", reliability_phase_idx), TRUE ~ NA_character_), STAGES),
    phase = factor(ifelse(aided, paste0("P", reliability_phase_idx), NA), c("P1", "P2", "P3")),
    advice = factor(ifelse(aided, ifelse(aid == 1, "Correct", "Incorrect"), NA), ADVICE),
    accuracy = ifelse(timeout, 0L, correct), timeout = timeout,
    valid_correct_rt = accuracy == 1 & is.finite(rt_s) & rt_s > 0,
    log_rt = ifelse(valid_correct_rt, log(pmax(rt_s, .Machine$double.xmin)), NA_real_),
    agreement = ifelse(aided & !timeout, as.integer(response == aid_label), NA_integer_),
    progress = (trial_in_reliability_phase - 1) / 399,
    bin = ceiling(trial_in_reliability_phase / 100))
}

load_cohort <- function(input) {
  files <- sort(list.files(input, "^results_.*_b00_ALL\\.csv$", full.names = TRUE))
  check(length(files) > 0, "No complete trial exports found")
  selected <- lapply(files, function(path) {
    paths <- c(path, sub("_ALL.csv$", "_POSTBLOCK_ALL.csv", path), sub("_ALL.csv$", "_POSTBLOCK_SLIDERS_ALL.csv", path))
    check(all(file.exists(paths)), paste("Missing same-run ratings for", basename(path)))
    x <- lapply(paths, read_table); d <- x[[1]]; q <- x[[2]]; s <- x[[3]]
    check(n_distinct(d$participant_id) == 1 && n_distinct(d$run_timestamp) == 1, "Mixed trial export")
    pid <- d$participant_id[1]; stamp <- d$run_timestamp[1]
    check(basename(path) == sprintf("results_p%03d_%s_b00_ALL.csv", pid, stamp), "Filename/run identity mismatch")
    # Legacy trust exports contain no timestamp column: same-run identity comes from the exact filename.
    for (r in list(q, s)) {
      check(all(r$participant_id == pid) && all(r$calibration_target_group == d$calibration_target_group[1]) &&
        all(r$calibration_target_accuracy == d$calibration_target_accuracy[1]) &&
        all(r$main_block_order == "SPLIT_MANUAL") && all(r$trial_deadline_s == 5), "Questionnaire identity/design mismatch")
      if ("run_timestamp" %in% names(r)) check(all(r$run_timestamp == stamp), "Questionnaire run mismatch")
    }
    check(nrow(q) == 6 && setequal(q$question_idx, 1:6) && all(q$block == "AUTOMATION") &&
      all(q$block_idx == 3) && all(q$condition_code == "REL_DROP") &&
      all(q$reliability_phase_label == "DROP95_70_95") && all(q$postblock_scope == "full_automation_block") &&
      all(q$scale_min == 1 & q$scale_max == 5) && all(q$response %in% 1:5), "Invalid six-item trust questionnaire")
    expected_key <- c("perc_self_correct", "perc_self_correct", "perc_auto_correct", "perc_self_correct")
    s <- arrange(s, block_idx)
    check(nrow(s) == 4 && identical(as.integer(s$block_idx), 1:4) &&
      all(s$question_key == expected_key) && all(is.finite(s$response_percent) & s$response_percent >= 0 & s$response_percent <= 100) &&
      all(s$block == c("CALIBRATION", "MANUAL", "AUTOMATION", "MANUAL")) &&
      all(s$postblock_scope == c("completed_block", "completed_block", "full_automation_block", "completed_block")) &&
      s$reliability_phase_label[3] == "DROP95_70_95" &&
      s$manual_segment[2] == "PRE_AUTOMATION" && s$manual_segment[4] == "POST_AUTOMATION", "Invalid slider scopes or values")
    list(trials = d, ratings = tibble(participant_id = pid, group = factor(d$calibration_target_group[1], GROUPS),
      trust = mean(q$response), perceived_aid_accuracy = s$response_percent[3],
      self_calibration = s$response_percent[1], self_pre = s$response_percent[2], self_post = s$response_percent[4]),
      manifest = hash_table(paths) %>% mutate(participant_id = pid, run_timestamp = stamp,
        kind = c("trials", "trust", "sliders"), n_rows = vapply(x, nrow, integer(1))))
  })
  raw <- bind_rows(lapply(selected, `[[`, "trials"))
  list(trials = prepare_trials(raw), ratings = bind_rows(lapply(selected, `[[`, "ratings")),
       manifest = bind_rows(lapply(selected, `[[`, "manifest")))
}

describe <- function(data, keys, value) {
  data %>% group_by(across(all_of(keys))) %>% summarise(n = sum(is.finite(.data[[value]])),
    mean = if (n > 0) mean(.data[[value]], na.rm = TRUE) else NA_real_,
    se = if (n > 1) sd(.data[[value]], na.rm = TRUE) / sqrt(n) else NA_real_, .groups = "drop") %>%
    mutate(lower = mean - qt(.975, pmax(n - 1, 1)) * se, upper = mean + qt(.975, pmax(n - 1, 1)) * se)
}

# Contrast registries are constructed from design grids, never observed effects.
performance_weights <- function(grid, baseline = "Manual pre", manual_only = FALSE) {
  cell <- function(g, s) as.numeric(grid$group == g & grid$stage == s)
  pairs <- if (manual_only) list(c("Manual post", "Manual pre")) else
    c(lapply(c("P1", "P2", "P3"), function(p) c(p, baseline)),
      if (baseline == "Manual pre") list(c("P2", "P1"), c("P3", "P2"), c("P3", "P1")))
  out <- list()
  for (p in pairs) {
    for (g in GROUPS) out[[paste(g, paste(p, collapse = " - "), sep = ": ")]] <- cell(g, p[1]) - cell(g, p[2])
    out[[paste("CAL90 - CAL65", paste(p, collapse = " - "), sep = ": ")]] <-
      cell("CAL90", p[1]) - cell("CAL90", p[2]) - cell("CAL65", p[1]) + cell("CAL65", p[2])
  }
  out
}
agreement_weights <- function(grid) {
  out <- list()
  for (a in ADVICE) {
    cell <- function(g, p) as.numeric(grid$group == g & grid$phase == p & grid$advice == a)
    for (p in list(c("P2", "P1"), c("P3", "P2"), c("P3", "P1"))) {
      for (g in GROUPS) out[[paste(a, g, paste(p, collapse = " - "), sep = ": ")]] <- cell(g, p[1]) - cell(g, p[2])
      out[[paste(a, "CAL90 - CAL65", paste(p, collapse = " - "), sep = ": ")]] <-
        cell("CAL90", p[1]) - cell("CAL90", p[2]) - cell("CAL65", p[1]) + cell("CAL65", p[2])
    }
  }
  out
}
trend_weights <- function(grid, endpoints = TRUE) {
  out <- list()
  for (a in if ("advice" %in% names(grid)) ADVICE else "All") for (p in c("P1", "P2", "P3")) {
    cell <- function(g) as.numeric(grid$group == g & grid$phase == p &
      if (a == "All") TRUE else grid$advice == a) * if (endpoints) ifelse(grid$progress == 1, 1, -1) else 1
    for (g in GROUPS) out[[paste(a, p, g, "end - start", sep = ": ")]] <- cell(g)
    out[[paste(a, p, "CAL90 - CAL65", "end - start", sep = ": ")]] <- cell("CAL90") - cell("CAL65")
  }
  out
}
strict_control <- function(binary_outcome) {
  fn <- if (binary_outcome) glmerControl else lmerControl
  x <- if (binary_outcome) list(optimizer = "bobyqa", calc.derivs = TRUE, optCtrl = list(maxfun = 300000, rhoend = 1e-9))
    else list(optimizer = "nloptwrap", calc.derivs = TRUE, optCtrl = list(maxeval = 300000, xtol_abs = 1e-8, ftol_abs = 1e-8))
  for (key in intersect(c("check.conv.nobsmax", "check.conv.nparmax"), names(formals(fn)))) x[[key]] <- Inf
  do.call(fn, x)
}
fit_model <- function(name, formula, data, is_binary, out) {
  message("Fitting ", name, " (", nrow(data), " observations)")
  unlink(c(file.path(out, "models", paste0(name, ".rds")),
    file.path(out, paste0(name, c("_summary.txt", "_cell_diagnostics.csv", "_residual_qq.png")))))
  warnings <- character()
  model <- tryCatch(withCallingHandlers(
    if (is_binary) glmer(formula, data, family = binomial(), control = strict_control(TRUE))
    else lmerTest::lmer(formula, data, REML = TRUE, control = strict_control(FALSE)),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") }), error = identity)
  if (inherits(model, "error")) return(list(model = NULL, diagnostics = tibble(model = name, n = nrow(data),
    converged = FALSE, singular = NA, rank_deficient = NA, inference_available = FALSE,
    warnings = conditionMessage(model))))
  saveRDS(model, file.path(out, "models", paste0(name, ".rds")))
  messages <- model@optinfo$conv$lme4$messages
  converged <- is.null(messages) && all(model@optinfo$conv$opt == 0) && all(is.finite(fixef(model)))
  singular <- isSingular(model, tol = 1e-4)
  deficient <- !is.null(attr(getME(model, "X"), "col.dropped"))
  gradient <- tryCatch(max(abs(solve(chol(model@optinfo$derivs$Hessian), model@optinfo$derivs$gradient))), error = function(e) NA_real_)
  diag <- tibble(model = name, n = nrow(data), converged, singular, rank_deficient = deficient,
    inference_available = converged && !singular && !deficient, max_scaled_gradient = gradient,
    warnings = paste(unique(c(warnings, messages)), collapse = " | "))
  keys <- intersect(c("subject", "group", "stage", "phase", "advice"), all.vars(formula))
  r <- data %>% mutate(.p = fitted(model), .y = model.response(model.frame(model)), .res = residuals(model, type = "pearson"))
  cells <- r %>% group_by(across(all_of(keys))) %>% summarise(n = n(), observed = sum(.y),
    expected = sum(.p), variance = sum(.p * (1 - .p)), residual_sd = sd(.res), .groups = "drop")
  if (is_binary) {
    # Conditional diagnostic only; simulations fix fitted parameters and do not refit.
    ids <- as.integer(interaction(r[keys], drop = TRUE)); k <- max(ids)
    sum_cell <- function(x) as.numeric(rowsum(x, ids, reorder = TRUE))
    expected <- sum_cell(r$.p); variance <- pmax(sum_cell(r$.p * (1-r$.p)), 1e-10)
    discrepancy <- sum((sum_cell(r$.y) - expected)^2 / variance)
    set.seed(20261005)
    simulations <- replicate(1000, sum((sum_cell(rbinom(nrow(r), 1, r$.p)) - expected)^2 / variance))
    diag$conditional_cell_dispersion <- discrepancy / k
    diag$conditional_cell_check_p <- (1 + sum(simulations >= discrepancy)) / 1001
    diag$extreme_fitted_probability <- any(r$.p < 1e-6 | r$.p > 1 - 1e-6)
  } else {
    diag$residual_sd_max_min_ratio <- max(cells$residual_sd) / min(cells$residual_sd)
    q <- ggplot(r, aes(sample = .res)) + stat_qq(alpha = .15, size = .3) + stat_qq_line() + theme_minimal() + labs(title = paste(name, "residual Q-Q plot"))
    ggsave(file.path(out, paste0(name, "_residual_qq.png")), q, width = 8, height = 5, dpi = 150)
  }
  write_csv(cells, file.path(out, paste0(name, "_cell_diagnostics.csv")))
  writeLines(capture.output(summary(model)), file.path(out, paste0(name, "_summary.txt")))
  list(model = model, diagnostics = diag)
}

model_results <- function(fit, spec, grid, weights, outcome, family, out, at = NULL) {
  weight_table <- bind_rows(lapply(names(weights), function(nm) mutate(grid, contrast = nm, weight = weights[[nm]])))
  write_csv(weight_table, file.path(out, paste0(outcome, "_", family, "_weights.csv")))
  unlink(file.path(out, paste0(outcome, "_", family, "_model_means.csv")))
  if (is.null(fit$model)) return(tibble(outcome, family, method = "Mixed model", contrast = names(weights),
    estimate = NA_real_, lower = NA_real_, upper = NA_real_, se = NA_real_, p_raw = NA_real_, inference_available = FALSE))
  binary_outcome <- outcome != "correct_rt"
  emm <- emmeans(fit$model, spec, at = at, lmer.df = "satterthwaite", lmerTest.limit = Inf)
  actual <- as.data.frame(emm)
  keys <- names(grid)
  ix <- match(do.call(paste, actual[keys]), do.call(paste, grid[keys]))
  check(!anyNA(ix) && length(ix) == nrow(grid), "EMM design grid mismatch")
  weights <- lapply(weights, function(w) w[ix])
  emm_response <- if (binary_outcome) regrid(emm, transform = "response") else emm
  write_csv(as.data.frame(summary(emm_response, infer = c(TRUE, FALSE))), file.path(out, paste0(outcome, "_", family, "_model_means.csv")))
  tab <- as.data.frame(summary(contrast(emm_response, weights), infer = c(TRUE, TRUE), adjust = "none"))
  lo <- intersect(c("lower.CL", "asymp.LCL"), names(tab)); hi <- intersect(c("upper.CL", "asymp.UCL"), names(tab))
  scale <- if (binary_outcome) 100 else 1
  ok <- fit$diagnostics$inference_available
  tibble(outcome, family, method = "Mixed model", contrast = tab$contrast, estimate = tab$estimate * scale,
    lower = if (ok) tab[[lo]] * scale else NA_real_, upper = if (ok) tab[[hi]] * scale else NA_real_,
    se = tab$SE * scale, df = tab$df, p_raw = if (ok) tab$p.value else NA_real_, inference_available = ok)
}
participant_results <- function(data, value, grid, weights, outcome, family) {
  keys <- names(grid); scale <- if (outcome == "correct_rt") 1 else 100
  bind_rows(lapply(names(weights), function(nm) {
    active <- grid %>% mutate(weight = weights[[nm]]) %>% filter(weight != 0)
    cells <- inner_join(data, active, by = keys)
    expected <- count(active, group, name = "expected")
    scores <- cells %>% group_by(group, participant_id) %>% summarise(n = n(),
      score = if (all(is.finite(.data[[value]]))) sum(.data[[value]] * weight) else NA_real_, .groups = "drop") %>%
      left_join(expected, by = "group") %>% filter(n == expected, is.finite(score))
    comp <- scores %>% group_by(group) %>% summarise(n = n(), estimate = mean(score), v = var(score)/n, .groups = "drop")
    ok <- nrow(comp) == n_distinct(active$group) && all(comp$n > 1) && all(is.finite(comp$v))
    estimate_raw <- if (ok) sum(comp$estimate) else NA_real_; se_raw <- if (ok) sqrt(sum(comp$v)) else NA_real_
    degrees <- if (ok && se_raw > 0) sum(comp$v)^2 / sum(comp$v^2/(comp$n-1)) else Inf
    p <- if (!ok) NA_real_ else if (se_raw == 0) ifelse(estimate_raw == 0, 1, 0) else 2*pt(-abs(estimate_raw/se_raw), degrees)
    tibble(outcome, family, method = "Participant sensitivity", contrast = nm, estimate = estimate_raw * scale,
      lower = (estimate_raw-qt(.975, degrees)*se_raw)*scale, upper = (estimate_raw+qt(.975, degrees)*se_raw)*scale,
      se = se_raw*scale, df = degrees, p_raw = p, inference_available = ok, n_participants = nrow(scores))
  }))
}
adjust_results <- function(x) x %>% group_by(outcome, family, method) %>%
  mutate(p_holm = p.adjust(p_raw, "holm", n = n()),
    units = ifelse(outcome == "correct_rt", "log RT contrast", "percentage points"),
    ratio = ifelse(outcome == "correct_rt", exp(estimate), NA_real_),
    ratio_lower = ifelse(outcome == "correct_rt", exp(lower), NA_real_),
    ratio_upper = ifelse(outcome == "correct_rt", exp(upper), NA_real_)) %>% ungroup()

subjective_models <- function(ratings, out) {
  results <- list()
  for (outcome in c("trust", "perceived_aid_error")) for (association in c(FALSE, TRUE)) {
    name <- paste0(outcome, if (association) "_association" else "_group")
    formula <- as.formula(paste(outcome, "~ group", if (association) "+ agreement_10pp" else ""))
    fit <- lm(formula, data = ratings); v <- sandwich::vcovHC(fit, type = "HC3")
    term <- if (association) "agreement_10pp" else "groupCAL90"
    b <- coef(fit)[[term]]; se <- sqrt(v[term, term]); df <- df.residual(fit)
    saveRDS(fit, file.path(out, "models", paste0(name, ".rds")))
    results[[name]] <- tibble(outcome, family = "subjective", method = "Participant HC3",
      contrast = if (association) "P2 incorrect-advice agreement: per 10 pp, adjusted for group" else "CAL90 - CAL65",
      estimate = b, lower = b-qt(.975, df)*se, upper = b+qt(.975, df)*se, se, df,
      p_raw = 2*pt(-abs(b/se), df), inference_available = is.finite(b) && is.finite(se),
      units = if (outcome == "trust") "trust points (1-5)" else "rating percentage points")
  }
  bind_rows(results) %>% group_by(outcome) %>% mutate(p_holm = p.adjust(p_raw, "holm")) %>% ungroup()
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  check(length(args) <= 2, "Usage: Rscript analyse_dynamic_reliability.R [input_dir] [output_dir]")
  script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])); root <- dirname(script)
  input <- if (length(args)) args[1] else file.path(root, "output/semester2_2026_data")
  out <- if (length(args) >= 2) args[2] else file.path(root, "analysis_outputs/semester2_2026_behavioural")
  dir.create(file.path(out, "models"), recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(out, "COMPLETE.txt"))
  cohort <- load_cohort(input); all <- cohort$trials; d <- filter(all, !is.na(stage))
  check(setequal(unique(d$group), GROUPS), "Both calibration groups required")
  write <- function(x, name) write_csv(x, file.path(out, paste0(name, ".csv")))
  write(cohort$manifest, "input_manifest"); write(hash_table(c(script, file.path(root, "python/virus_task.py"))), "source_checksums")
  write(d %>% distinct(participant_id, group) %>% count(group, name = "n_participants"), "cohort_counts")
  write(all %>% filter(block == "CALIBRATION", trial > 150) %>% group_by(participant_id, group) %>%
    summarise(n = n(), accuracy = mean(accuracy), target = first(calibration_target_accuracy), .groups = "drop"), "calibration_check")
  write(d %>% filter(timeout | !is.finite(rt_s) | rt_s <= 0) %>%
    select(participant_id, run_timestamp, stage, trial, response, accuracy, rt_s, valid_correct_rt) %>%
    mutate(reason = ifelse(response == "TIMEOUT", "Timeout: incorrect accuracy, excluded RT/agreement", "Invalid RT: retained accuracy/agreement")), "trial_audit")
  perf <- d %>% group_by(participant_id, group, stage) %>% summarise(n = n(), accuracy = mean(accuracy),
    n_timeouts = sum(timeout), n_correct_rt = sum(valid_correct_rt),
    mean_correct_rt = if (sum(valid_correct_rt)) mean(rt_s[valid_correct_rt]) else NA_real_,
    mean_log_rt = if (sum(valid_correct_rt)) mean(log_rt[valid_correct_rt]) else NA_real_, .groups = "drop")
  aid <- filter(d, !is.na(phase)); answered <- filter(aid, !timeout)
  agreement <- answered %>% group_by(participant_id, group, phase, advice) %>%
    summarise(n = n(), agreement = mean(agreement), .groups = "drop")
  check(all(answered$agreement == ifelse(answered$advice == "Correct", answered$accuracy, 1-answered$accuracy)), "Agreement identity failed")
  write(perf, "participant_performance"); write(agreement, "participant_agreement")
  write(bind_rows(lapply(c("accuracy", "mean_correct_rt"), function(v) describe(perf, c("group", "stage"), v) %>% mutate(outcome = v))), "descriptive_performance")
  write(describe(agreement, c("group", "phase", "advice"), "agreement"), "descriptive_agreement")
  bins <- bind_rows(aid %>% group_by(participant_id, group, phase, bin) %>% summarise(value = mean(accuracy), n_trials = n(), .groups = "drop") %>% mutate(outcome = "Accuracy"),
    answered %>% group_by(participant_id, group, phase, bin, advice) %>% summarise(value = mean(agreement), n_trials = n(), .groups = "drop") %>%
      mutate(outcome = paste(advice, "advice agreement")) %>% select(-advice))
  write(bins, "participant_trajectories"); write(describe(bins, c("group", "phase", "bin", "outcome"), "value"), "descriptive_trajectories")
  ratings <- cohort$ratings %>% left_join(aid %>% group_by(participant_id) %>%
      summarise(realised_aid_accuracy = 100*mean(advice == "Correct"), .groups = "drop"), by = "participant_id") %>%
    left_join(agreement %>% filter(phase == "P2", advice == "Incorrect") %>% select(participant_id, agreement), by = "participant_id") %>%
    mutate(perceived_aid_error = perceived_aid_accuracy-realised_aid_accuracy,
           agreement_10pp = (agreement-mean(agreement))/.1)
  check(!anyNA(ratings), "Incomplete participant ratings/behaviour joins")
  write(ratings, "participant_ratings")

  grid <- expand.grid(group = GROUPS, stage = STAGES, stringsAsFactors = FALSE)
  agrid <- expand.grid(group = GROUPS, phase = c("P1", "P2", "P3"), advice = ADVICE, stringsAsFactors = FALSE)
  registry <- bind_rows(tibble(hypothesis = c("H1", "H2", "H3", "E1", "E2", "E3"),
    question = c("Group moderation of agreement change at the drop", "Phase benefits relative to manual pre", "Recovery and residual P3-P1 differences",
      "Within-phase linear adaptation", "Correct RT and manual pre/post change", "Final subjective evaluation and P2 behaviour"),
    status = "Retrospectively specified"))
  write(registry, "hypothesis_registry")
  fits <- list(accuracy = fit_model("accuracy", accuracy ~ group*stage + (1|subject), d, TRUE, out),
    correct_rt = fit_model("correct_rt", log_rt ~ group*stage + (1|subject), filter(d, valid_correct_rt), FALSE, out),
    agreement = fit_model("agreement", agreement ~ group*phase*advice + (1|subject), answered, TRUE, out))
  results <- list()
  for (v in c("accuracy", "correct_rt")) {
    for (fam in c(if (v == "accuracy") "core" else "speed", "manual_change", if (v == "accuracy") "post_baseline_sensitivity")) {
      w <- performance_weights(grid, baseline = if (fam == "post_baseline_sensitivity") "Manual post" else "Manual pre", manual_only = fam == "manual_change")
      results[[paste(v, fam)]] <- bind_rows(model_results(fits[[v]], ~group*stage, grid, w, v, fam, out),
        participant_results(perf, if (v == "accuracy") "accuracy" else "mean_log_rt", grid, w, v, fam))
    }
  }
  aw <- agreement_weights(agrid)
  results$agreement <- bind_rows(model_results(fits$agreement, ~group*phase*advice, agrid, aw, "agreement", "core", out),
    participant_results(agreement, "agreement", agrid, aw, "agreement", "core"))
  fits$accuracy_trend <- fit_model("accuracy_trend", accuracy ~ group*phase*progress + (1|subject), aid, TRUE, out)
  fits$agreement_trend <- fit_model("agreement_trend", agreement ~ group*phase*advice*progress + (1|subject), answered, TRUE, out)
  for (v in c("accuracy", "agreement")) {
    tg <- if (v == "accuracy") unique(agrid[c("group", "phase")]) else agrid
    endpoints <- merge(tg, data.frame(progress = c(0, 1)))
    tw <- trend_weights(endpoints)
    dat <- if (v == "accuracy") aid else answered
    slopes <- dat %>% group_by(across(all_of(c("participant_id", names(tg))))) %>% summarise(
      slope = if (n_distinct(progress) >= 2) unname(coef(lm(.data[[v]] ~ progress))[2]) else NA_real_, .groups = "drop")
    write(slopes, paste0("participant_", v, "_slopes"))
    spec <- if (v == "accuracy") ~group*phase*progress else ~group*phase*advice*progress
    results[[paste0(v, "_trend")]] <- bind_rows(
      model_results(fits[[paste0(v, "_trend")]], spec, endpoints, tw, v, "adaptation", out, at = list(progress = c(0, 1))),
      participant_results(slopes, "slope", tg, trend_weights(tg, FALSE), v, "adaptation"))
    unlink(file.path(out, paste0(v, "_trend_curves.csv")))
    if (!is.null(fits[[paste0(v, "_trend")]]$model)) {
      curves <- as.data.frame(summary(emmeans(fits[[paste0(v, "_trend")]]$model, spec,
        at = list(progress = seq(0, 1, length.out = 41))), type = "response", infer = c(TRUE, FALSE)))
      write(curves, paste0(v, "_trend_curves"))
    }
  }
  contrasts <- adjust_results(bind_rows(results)) %>% mutate(hypothesis = case_when(
    family == "adaptation" ~ "E1", family %in% c("speed", "manual_change") ~ "E2",
    family == "post_baseline_sensitivity" | grepl("Manual pre$", contrast) ~ "H2",
    grepl("P2 - P1$", contrast) ~ "H1", TRUE ~ "H3"))
  write(contrasts, "planned_contrasts")
  comparison <- contrasts %>% select(outcome, family, contrast, method, estimate, p_holm) %>%
    pivot_wider(names_from = method, values_from = c(estimate, p_holm)) %>% mutate(
      same_direction = sign(`estimate_Mixed model`) == sign(`estimate_Participant sensitivity`),
      both_p_below_05 = `p_holm_Mixed model` < .05 & `p_holm_Participant sensitivity` < .05,
      model_only_p_below_05 = `p_holm_Mixed model` < .05 & `p_holm_Participant sensitivity` >= .05)
  write(comparison, "model_participant_comparison")
  key_names <- c("Incorrect: CAL65: P2 - P1", "Incorrect: CAL90: P2 - P1", "Incorrect: CAL90 - CAL65: P2 - P1",
    "CAL65: P2 - Manual pre", "CAL90: P2 - Manual pre", "CAL90 - CAL65: P2 - Manual pre",
    "Incorrect: CAL65: P3 - P2", "Incorrect: CAL90: P3 - P2", "Incorrect: CAL65: P3 - P1", "Incorrect: CAL90: P3 - P1")
  key_results <- contrasts %>% filter(family == "core", contrast %in% key_names)
  write(key_results, "key_results")
  subjective <- subjective_models(ratings, out); write(subjective, "subjective_contrasts")
  diagnostics <- bind_rows(lapply(fits, `[[`, "diagnostics")); write(diagnostics, "model_diagnostics")
  write(tibble(diagnostic = c("DHARMa", "Conditional fixed-parameter cell simulation"),
    available = c(requireNamespace("DHARMa", quietly = TRUE), TRUE),
    used = c(FALSE, TRUE), note = c("Not required or run; availability reported explicitly", "Diagnostic only, not a calibrated model adequacy test")), "diagnostic_availability")
  writeLines(capture.output(sessionInfo()), file.path(out, "session_info.txt"))
  report <- c("DYNAMIC RELIABILITY: BEHAVIOURAL ANALYSIS", "",
    sprintf("%d participants (%d CAL65, %d CAL90); %d experimental trials.", n_distinct(d$subject),
      n_distinct(d$subject[d$group == "CAL65"]), n_distinct(d$subject[d$group == "CAL90"]), nrow(d)),
    sprintf("%d experimental timeouts counted as incorrect; %d answered trials with nonpositive RT retained for accuracy/agreement.", sum(d$timeout), sum(!d$timeout & is.finite(d$rt_s) & d$rt_s <= 0)),
    sprintf("RT audit: %d valid correct RTs below 100 ms; %d beyond 5 s. No automatic trimming.", sum(d$valid_correct_rt & d$rt_s < .1, na.rm=TRUE), sum(d$valid_correct_rt & d$rt_s > 5, na.rm=TRUE)),
    "", "Central comparisons (model and participant estimates must be read together):",
    vapply(seq_len(nrow(key_results)), function(i) { x <- key_results[i, ]; sprintf(
      "%s | %s | %s: %+.2f pp [%.2f, %.2f], Holm p = %.4g",
      x$hypothesis, x$method, x$contrast, x$estimate, x$lower, x$upper, x$p_holm) }, character(1)),
    "", "Diagnostic qualification:",
    sprintf("%d of %d binomial models have conditional cell dispersion above 1 and diagnostic simulation p < .05.",
      sum(diagnostics$conditional_cell_dispersion > 1 & diagnostics$conditional_cell_check_p < .05, na.rm = TRUE),
      sum(!is.na(diagnostics$conditional_cell_dispersion))),
    "Extra participant-cell variation limits random-intercept-model uncertainty. Emphasise participant sensitivity alongside each model result, not whichever test is significant.",
    sprintf("DHARMa available: %s; not run. Correct-RT residual spread differs across participant/stage cells; inspect residual plots and participant sensitivity.", requireNamespace("DHARMa", quietly = TRUE)),
    "", "Interpretation and methods:",
    "All hypotheses were specified after data collection. Calibration/manual performance screens do not exclude participants.",
    "Fixed phase order confounds reliability with time, practice and fatigue; group differences also reflect calibrated visual difficulty.",
    "Agreement is not advice-caused switching. P3-P1 nonsignificance is not equivalence or proof of full recovery.",
    "Participant random intercepts only. Mixed-model probability means condition on a zero participant random effect.",
    "Participant sensitivities have equal person weights; their estimands differ from conditional mixed-model estimates.",
    "Two-sided Holm tests within outcome/family/method; CIs are pointwise. Model and participant discrepancies are retained.",
    "Core accuracy and agreement each have 18 registered contrasts per method; speed has 18; manual change has 3 per outcome; post-baseline sensitivity has 9.",
    "Adaptation contrasts compare linear-model endpoints (0 to 1 progress); participant sensitivity uses individual linear probability slopes.",
    "Correct RT inference uses log RT; plot arithmetic means are descriptive. Correct RT conditions on response correctness.",
    "Trust has six items; perceived aid error uses realised whole-block aid accuracy. No phase-specific ratings or automation self-accuracy rating exist.",
    "Legacy trust CSVs lack timestamps: exact same-run filenames and participant/design fields provide provenance.",
    "Conditional cell diagnostics fix model parameters and do not refit. They are not formal goodness-of-fit tests.",
    "Failed/singular/rank-deficient fits do not support inferential claims. Small model-only findings require particular caution.",
    "", "Model diagnostics:", capture.output(print(as.data.frame(diagnostics), row.names = FALSE)),
    "", "Registered contrasts (pp for binary outcomes; log units/ratios for RT):",
    capture.output(print(as.data.frame(contrasts %>% select(outcome, family, method, contrast, estimate, lower, upper, p_holm)), row.names = FALSE)),
    "", "Subjective evaluations (HC3 standard errors):", capture.output(print(as.data.frame(subjective), row.names = FALSE)))
  writeLines(report, file.path(out, "results_report.txt"))
  escaped <- gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", report, fixed=TRUE), fixed=TRUE), fixed=TRUE)
  writeLines(c('<!doctype html><meta charset="utf-8"><title>Dynamic reliability behavioural analysis</title>',
    '<style>body{font:15px system-ui;margin:40px;color:#1C2D3E}pre{white-space:pre-wrap;font:13px monospace}</style>',
    '<h1>Dynamic reliability behavioural analysis</h1><pre>', escaped, '</pre>'), file.path(out, "results_report.html"))
  # Receipt covers every artifact in this run directory; plotting checks these as well as raw inputs.
  artifacts <- list.files(out, full.names=TRUE, recursive=TRUE)
  artifacts <- artifacts[!basename(artifacts) %in% c("COMPLETE.txt", "analysis_artifacts.csv")]
  write(hash_table(artifacts), "analysis_artifacts")
  writeLines(c(format(Sys.time(), tz = "UTC"), "Completed computation; inspect diagnostics before interpreting models."), file.path(out, "COMPLETE.txt"))
  message("Completed: ", out)
}
if (sys.nframe() == 0L) main()
