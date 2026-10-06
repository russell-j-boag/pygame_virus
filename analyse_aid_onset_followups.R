# Exploratory presentation H5-H7; the original analysis H5 remains unchanged.
# Rscript analyse_aid_onset_followups.R [output_dir]
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lme4); library(emmeans)
})
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1]])
ROOT <- dirname(normalizePath(script))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 1L)
OUT <- if (length(args)) args[[1]] else file.path(ROOT, "analysis_outputs/semester2_2026_presentation_followups")
OLD <- file.path(ROOT, "analysis_outputs/semester2_2026_hypotheses")
dir.create(file.path(OUT, "models"), recursive = TRUE, showWarnings = FALSE)
consumed <- character()
read <- function(path) {
  consumed <<- unique(c(consumed, normalizePath(path, mustWork = TRUE)))
  read_csv(path, show_col_types = FALSE)
}
recorded <- read(file.path(OLD, "analysis_input_checksums.csv"))
stopifnot(all(file.exists(recorded$path)), identical(unname(tools::md5sum(recorded$path)), recorded$md5))
consumed <- unique(c(consumed, recorded$path))
raw <- read(file.path(ROOT, "data/data_virus_all.csv"))
primary <- read(file.path(OLD, "primary_hypothesis_tests.csv"))
sensitivity <- read(file.path(OLD, "participant_level_sensitivity_tests.csv"))
interaction <- read(file.path(OLD, "timing_advice_interaction.csv"))
old_diagnostics <- read(file.path(OLD, "model_diagnostics.csv"))
interaction_diagnostics <- read(file.path(OLD, "timing_advice_interaction_diagnostics.csv"))
stopifnot(all(old_diagnostics$converged), !any(old_diagnostics$singular),
  all(interaction_diagnostics$converged), !any(interaction_diagnostics$singular))
binary <- function(x) {
  x <- tolower(as.character(x))
  stopifnot(all(is.na(x) | x %in% c("true", "false", "0", "1")))
  ifelse(is.na(x), NA_integer_, as.integer(x %in% c("true", "1")))
}
levels_condition <- c("Manual", "Aid first", "Stimulus first")
trials <- raw %>% transmute(subject_no = participant_id, subject = factor(participant_id),
  condition = factor(recode(aid_condition, manual = "Manual", aid_first = "Aid first", stimulus_first = "Stimulus first"), levels_condition),
  d1 = binary(decision1_correct), d2 = binary(decision2_correct),
  advice_correct = binary(aid_correct), agreement1 = binary(decision1_matches_aid),
  agreement2 = binary(decision2_matches_aid), switched = binary(changed_response)) %>%
  mutate(beneficial = as.integer(d1 == 0 & d2 == 1), harmful = as.integer(d1 == 1 & d2 == 0))
stopifnot(nrow(trials) == 46800L, setequal(trials$subject_no, 1:60),
  identical(unique(raw$run_timestamp[raw$participant_id == 59]), "20260924_110831"),
  all((trials %>% count(subject_no, condition))$n == 260),
  all(trials$switched == trials$beneficial+trials$harmful),
  all(trials$d2-trials$d1 == trials$beneficial-trials$harmful))
aided <- trials %>% filter(condition != "Manual")
stopifnot(!anyNA(aided[c("advice_correct", "agreement1", "agreement2")]),
  all(aided$agreement1 == as.integer(aided$d1 == aided$advice_correct)),
  all(aided$agreement2 == as.integer(aided$d2 == aided$advice_correct)))

# H5 uses all trials for BOTH endpoints. They are fitted separately so the
# mutually exclusive outcomes are never treated as independent duplicate trials.
revision <- bind_rows(
  trials %>% mutate(panel = "Beneficial revisions", response = beneficial),
  trials %>% mutate(panel = "Harmful revisions", response = harmful)) %>%
  group_by(subject_no, condition, panel) %>% summarise(successes = sum(response),
    opportunities = n(), value = mean(response)*100, .groups = "drop") %>%
  mutate(hypothesis = "H5", level = as.character(condition),
    variable = paste0(if_else(panel == "Beneficial revisions", "beneficial_", "harmful_"),
      recode(level, Manual = "manual", `Aid first` = "aid_first", `Stimulus first` = "stimulus_first")),
    denominator = "All trials in condition")

# H6 is an exact final-error re-expression of the original H3/H4 timing tests.
errors <- aided %>% mutate(panel = if_else(advice_correct == 1,
  "Disagree with correct advice", "Agree with incorrect advice"), response = 1-d2) %>%
  group_by(subject_no, condition, panel) %>% summarise(successes = sum(response),
    opportunities = n(), value = mean(response)*100, .groups = "drop") %>%
  mutate(hypothesis = "H6", level = as.character(condition),
    variable = paste0(if_else(panel == "Disagree with correct advice", "reject_correct_", "accept_incorrect_"),
      recode(level, `Aid first` = "aid_first", `Stimulus first` = "stimulus_first")),
    denominator = if_else(panel == "Disagree with correct advice", "All correct-advice trials", "All incorrect-advice trials"))

# H7 uses only Stimulus-first: Decision 1 is an independent pre-advice response.
# Correct-advice uptake corrects an error; incorrect-advice uptake introduces one.
uptake_trials <- aided %>% filter(condition == "Stimulus first", agreement1 == 0) %>%
  mutate(level = factor(if_else(advice_correct == 1, "Correct advice", "Incorrect advice"),
    c("Correct advice", "Incorrect advice")), response = agreement2)
uptake <- uptake_trials %>% group_by(subject_no, level) %>% summarise(successes = sum(response),
  opportunities = n(), value = mean(response)*100, .groups = "drop") %>%
  mutate(hypothesis = "H7", panel = "Stimulus-first", level = as.character(level),
    variable = if_else(level == "Correct advice", "uptake_correct", "uptake_incorrect"),
    denominator = "Initial response disagrees with advice in Stimulus-first")
participant_cells <- bind_rows(revision, errors, uptake) %>%
  select(hypothesis, panel, subject_no, variable, level, successes, opportunities, value, denominator)
stopifnot(nrow(participant_cells) == 720L, all(participant_cells$opportunities > 0),
  !anyDuplicated(participant_cells[c("hypothesis", "subject_no", "variable")]))
write_csv(participant_cells, file.path(OUT, "participant_cells.csv"))

models <- list(); diagnostics <- list(); contrasts <- list()
fit <- function(d, h, panel, first, second, stem) {
  message("Fitting ", h, ": ", panel, " (", nrow(d), " trials)")
  model <- glmer(response ~ level + (1 | subject), data = d, family = binomial,
    nAGQ = 1, control = glmerControl(optimizer = "bobyqa", calc.derivs = TRUE,
      optCtrl = list(maxfun = 200000)))
  msg <- model@optinfo$conv$lme4$messages
  diag <- tibble(hypothesis = h, panel, n_trials = nrow(d), n_participants = n_distinct(d$subject),
    converged = is.null(msg) && isTRUE(model@optinfo$conv$opt == 0),
    singular = isSingular(model, tol = 1e-4), optimizer_code = model@optinfo$conv$opt,
    convergence_message = paste(msg, collapse = "; "),
    max_absolute_gradient = max(abs(model@optinfo$derivs$gradient)),
    pearson_dispersion_ratio = sum(residuals(model, type = "pearson")^2)/df.residual(model),
    pearson_overdispersion_p = pchisq(sum(residuals(model, type = "pearson")^2), df.residual(model), lower.tail = FALSE))
  diagnostics[[stem]] <<- diag
  write_csv(bind_rows(diagnostics), file.path(OUT, "model_diagnostics.csv"))
  stopifnot(diag$converged, !diag$singular)
  saveRDS(model, file.path(OUT, "models", paste0(stem, ".rds")))
  models[[stem]] <<- model
  emm <- emmeans(model, ~ level)
  methods <- lapply(seq_along(first), function(i) as.numeric(levels(d$level) == first[i]) - as.numeric(levels(d$level) == second[i]))
  names(methods) <- paste(first, second, sep = " / ")
  est <- as.data.frame(summary(contrast(emm, method = methods, adjust = "none"), infer = c(TRUE, TRUE)))
  cell <- participant_cells %>% filter(hypothesis == h, .data$panel == .env$panel)
  checks <- lapply(seq_along(first), function(i) {
    x <- cell %>% filter(level == first[i]) %>% arrange(subject_no)
    y <- cell %>% filter(level == second[i]) %>% arrange(subject_no)
    stopifnot(identical(x$subject_no, y$subject_no), nrow(x) == 60)
    difference <- x$value-y$value
    tt <- t.test(difference)
    tibble(paired_difference_pp = mean(difference), paired_conf_low_pp = tt$conf.int[1],
      paired_conf_high_pp = tt$conf.int[2], participant_p_raw = tt$p.value)
  })
  bind_cols(tibble(hypothesis = h, panel, contrast = names(methods), first_level = first,
    second_level = second, n_participants = 60L, analysis = "Trial-level logistic mixed model",
    estimate_scale = "Odds ratio", estimate = exp(est$estimate), conf_low = exp(est$asymp.LCL),
    conf_high = exp(est$asymp.UCL), statistic = est$z.ratio, p_value_raw = est$p.value,
    source_hypothesis = h, source_contrast = names(methods),
    source_file = paste0("models/", stem, ".rds"),
    analysis_origin = "Exploratory; specified after inspecting results"), bind_rows(checks))
}
for (endpoint in c("beneficial", "harmful")) {
  panel <- if (endpoint == "beneficial") "Beneficial revisions" else "Harmful revisions"
  d <- trials %>% mutate(level = condition, response = .data[[endpoint]])
  contrasts[[endpoint]] <- fit(d, "H5", panel,
    first = c("Stimulus first", "Stimulus first", "Aid first"),
    second = c("Aid first", "Manual", "Manual"), stem = paste0("h5_", endpoint))
}
h5 <- bind_rows(contrasts) %>% mutate(p_value_adjusted = p.adjust(p_value_raw, "holm"),
  participant_p_adjusted = p.adjust(participant_p_raw, "holm"),
  p_adjustment = "Holm across six H5 contrasts (both revision outcomes)")
h7 <- fit(uptake_trials, "H7", "Stimulus-first", "Correct advice", "Incorrect advice", "h7_selective_uptake") %>%
  mutate(p_value_adjusted = p_value_raw, participant_p_adjusted = participant_p_raw,
    p_adjustment = "none; single exploratory H7 contrast")
h6 <- bind_rows(lapply(c("H3", "H4"), function(source_h) {
  m <- primary %>% filter(hypothesis == source_h, contrast == "Stimulus first / Aid first")
  t <- sensitivity %>% filter(hypothesis == source_h, contrast == "Stimulus first - Aid first")
  stopifnot(nrow(m) == 1, nrow(t) == 1)
  tibble(hypothesis = "H6", panel = if (source_h == "H3") "Disagree with correct advice" else "Agree with incorrect advice",
    contrast = m$contrast, first_level = "Stimulus first", second_level = "Aid first", n_participants = 60L,
    analysis = "Saved logistic mixed-model contrast; accuracy complemented to error",
    estimate_scale = "Odds ratio", estimate = 1/m$estimate, conf_low = 1/m$conf_high, conf_high = 1/m$conf_low,
    statistic = -m$statistic, p_value_raw = m$p_value_raw, p_value_adjusted = m$p_value_adjusted,
    p_adjustment = paste0("Original ", source_h, " Holm family of three contrasts retained"),
    paired_difference_pp = -100*t$estimate, paired_conf_low_pp = -100*t$conf_high,
    paired_conf_high_pp = -100*t$conf_low, participant_p_raw = t$p_value_raw,
    participant_p_adjusted = t$p_value_adjusted,
    source_hypothesis = source_h, source_contrast = m$contrast, source_file = "primary_hypothesis_tests.csv",
    analysis_origin = "Exploratory presentation of existing H3/H4 timing evidence")
}))
tests <- bind_rows(h5, h6, h7) %>% mutate(
  participant_disagreement = (p_value_adjusted < .05) != (participant_p_adjusted < .05))
write_csv(tests, file.path(OUT, "contrasts.csv"))
original_interaction <- interaction %>% filter(grepl(":", term, fixed = TRUE))
stopifnot(nrow(original_interaction) == 1)
interaction <- tibble(hypothesis = "H6", source_term = original_interaction$term,
  interpretation = "Error-odds timing ratio under incorrect advice divided by the ratio under correct advice",
  estimate_log_odds = -original_interaction$estimate_log_odds, odds_ratio = 1/original_interaction$odds_ratio,
  conf_low = 1/original_interaction$conf_high, conf_high = 1/original_interaction$conf_low,
  p_value = original_interaction$p_value, p_adjustment = original_interaction$p_adjustment,
  source_file = "timing_advice_interaction.csv")
write_csv(interaction, file.path(OUT, "h6_interaction.csv"))
write_csv(tibble(diagnostic = c("Optimizer convergence", "Singularity", "Pearson dispersion", "DHARMa simulation residuals"),
  status = c("Checked", "Checked", "Checked; approximate Bernoulli diagnostic", "Not run"),
  detail = c("All three new models", "Participant random intercept only", "Not a simulation-based residual check",
    "Same diagnostic scope as the original analysis; no DHARMa diagnostics added")), file.path(OUT, "diagnostic_availability.csv"))
consumed <- unique(c(consumed, normalizePath(script)))
write_csv(tibble(path = consumed, md5 = unname(tools::md5sum(consumed))), file.path(OUT, "input_checksums.csv"))
writeLines(capture.output(sessionInfo()), file.path(OUT, "session_info.txt"))
writeLines(c("EXPLORATORY PRESENTATION FOLLOW-UPS H5-H7", "",
  "H5 = revision quality; H6 = advice-use errors; H7 = selective uptake in Stimulus-first.",
  "Numbering is for the revised presentation. Original H5 overall accuracy is retained in the original analysis tables.",
  "All three follow-ups were selected after inspecting results and remain exploratory.",
  "H5: two separate participant-random-intercept logistic models, one per binary revision endpoint, on all 46,800 trials.",
  "Three two-sided timing/Manual contrasts per endpoint; Holm across all six H5 contrasts, also for paired sensitivities.",
  "H5 rates use ALL trials per condition. They are not rates conditional on the initial decision, nor the composition of switches.",
  "H6: final disagreement with correct advice / agreement with incorrect advice are exactly 1 minus final accuracy.",
  "H6 transforms the original H3/H4 timing odds ratios and CIs; p-values retain the original three-contrast Holm families.",
  "H6 interaction is the complemented saved accuracy interaction; unadjusted exploratory p-value and pointwise Wald CI.",
  "H6 is a behavioural re-expression of existing evidence, not an independent replication or newly fitted test.",
  "H7: only Stimulus-first trials where Decision 1 disagrees with advice. Outcome: final agreement with advice.",
  "Correct-advice versus incorrect-advice uptake uses a participant-random-intercept logistic model, one two-sided contrast.",
  "H7 paired sensitivity compares each participant's two opportunity-normalized rates; all 60 have both denominators.",
  "Correct advice here implies an initially wrong response; incorrect advice implies an initially correct response.",
  "H7 therefore describes selectivity among observed opportunities, not an isolated causal effect of advice correctness.",
  "Aid-first Decision 1 already follows advice: cross-timing revision rates start from different information states.",
  "New model convergence, singularity and approximate Pearson dispersion checks are recorded; simulation residual diagnostics not run.",
  "Reproduce: Rscript analyse_aid_onset_followups.R; Rscript plot_aid_onset_slides.R"
), file.path(OUT, "README.txt"))
cat("Completed three exploratory models; nine plotted contrasts and one saved interaction.\n")
print(tests %>% select(hypothesis, panel, contrast, estimate, p_value_adjusted, participant_p_adjusted))
