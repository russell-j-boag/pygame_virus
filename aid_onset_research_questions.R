# Sourced by test_aid_onset_hypotheses.R after the established H1-H5 analyses.
# Keep new exploratory tests separate from the original multiplicity families.

if (any(trial_data$changed !=
        as.integer(trial_data$decision1_accuracy != trial_data$accuracy))) {
  stop("Changed responses and accuracy transitions disagree in the binary task.")
}

switch_cells <- bind_rows(
  trial_data %>% mutate(advice_subset = "Overall"),
  trial_data %>% filter(condition != "Manual") %>%
    mutate(advice_subset = if_else(aid_correct_num == 1, "Correct advice", "Incorrect advice"))
) %>%
  group_by(subject_no, condition, advice_subset) %>%
  summarise(
    n_trials = n(),
    n_initially_correct = sum(decision1_accuracy),
    n_initially_incorrect = sum(1 - decision1_accuracy),
    n_beneficial = sum(decision1_accuracy == 0 & accuracy == 1),
    n_harmful = sum(decision1_accuracy == 1 & accuracy == 0),
    decision1_accuracy = mean(decision1_accuracy),
    decision2_accuracy = mean(accuracy),
    switch_proportion = mean(changed),
    .groups = "drop"
  ) %>%
  mutate(
    beneficial_proportion = n_beneficial / n_trials,
    harmful_proportion = n_harmful / n_trials,
    net_accuracy_gain = decision2_accuracy - decision1_accuracy,
    conditional_beneficial = if_else(n_initially_incorrect > 0,
                                    n_beneficial / n_initially_incorrect, NA_real_),
    conditional_harmful = if_else(n_initially_correct > 0,
                                  n_harmful / n_initially_correct, NA_real_)
  )
stopifnot(nrow(switch_cells) == EXPECTED_PARTICIPANTS * 7L,
          max(abs(switch_cells$switch_proportion - switch_cells$beneficial_proportion -
                  switch_cells$harmful_proportion)) < 1e-12,
          max(abs(switch_cells$net_accuracy_gain - switch_cells$beneficial_proportion +
                  switch_cells$harmful_proportion)) < 1e-12)
switch_summary <- switch_cells %>%
  group_by(condition, advice_subset) %>%
  summarise(
    n_participants = n(), n_trials = sum(n_trials),
    n_initially_correct = sum(n_initially_correct),
    n_initially_incorrect = sum(n_initially_incorrect),
    n_beneficial = sum(n_beneficial), n_harmful = sum(n_harmful),
    across(c(decision1_accuracy, decision2_accuracy, switch_proportion,
             beneficial_proportion, harmful_proportion, net_accuracy_gain), mean),
    n_conditional_beneficial = sum(is.finite(conditional_beneficial)),
    n_conditional_harmful = sum(is.finite(conditional_harmful)),
    conditional_beneficial = mean(conditional_beneficial, na.rm = TRUE),
    conditional_harmful = mean(conditional_harmful, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(switch_cells, file.path(OUTPUT_DIR, "switch_decomposition_participants.csv"))
write_csv(switch_summary, file.path(OUTPUT_DIR, "switch_decomposition_summary.csv"))

# Correct advice is the reference, so the interaction is the SF/AF odds ratio
# under incorrect advice divided by the SF/AF odds ratio under correct advice.
interaction_data <- trial_data %>% filter(condition != "Manual") %>%
  mutate(timing = factor(as.character(condition), c("Aid first", "Stimulus first")),
         advice = factor(if_else(aid_correct_num == 1, "Correct", "Incorrect"),
                         c("Correct", "Incorrect")))
timing_model <- glmer(accuracy ~ timing * advice + (1 | subject),
                     data = interaction_data, family = binomial,
                     control = glmerControl(optimizer = "bobyqa", calc.derivs = TRUE,
                                            optCtrl = list(maxfun = 200000)))
timing_messages <- timing_model@optinfo$conv$lme4$messages
timing_pearson <- sum(residuals(timing_model, type = "pearson")^2)
timing_diagnostics <- tibble(
  analysis = "Exploratory timing by advice-correctness interaction",
  n_trials = nrow(interaction_data), n_participants = n_distinct(interaction_data$subject),
  converged = is.null(timing_messages) && isTRUE(timing_model@optinfo$conv$opt == 0),
  singular = isSingular(timing_model, tol = 1e-4),
  optimizer_code = timing_model@optinfo$conv$opt,
  convergence_message = paste(timing_messages, collapse = "; "),
  max_absolute_gradient = max(abs(timing_model@optinfo$derivs$gradient)),
  pearson_dispersion_ratio = timing_pearson / df.residual(timing_model),
  pearson_overdispersion_p = pchisq(timing_pearson, df.residual(timing_model), lower.tail = FALSE)
)
write_csv(timing_diagnostics, file.path(OUTPUT_DIR, "timing_advice_interaction_diagnostics.csv"))
if (!timing_diagnostics$converged || timing_diagnostics$singular) {
  stop("Exploratory interaction failed diagnostics; inspect its diagnostic CSV before interpretation.")
}
timing_coefficients <- as.data.frame(coef(summary(timing_model)))
timing_results <- tibble(
  term = rownames(timing_coefficients),
  estimate_log_odds = timing_coefficients[["Estimate"]],
  standard_error = timing_coefficients[["Std. Error"]],
  odds_ratio = exp(estimate_log_odds),
  conf_low = exp(estimate_log_odds - qnorm(.975) * standard_error),
  conf_high = exp(estimate_log_odds + qnorm(.975) * standard_error),
  p_value = timing_coefficients[["Pr(>|z|)"]],
  p_adjustment = "none; exploratory", confidence_interval = "95% pointwise Wald"
)
write_csv(timing_results, file.path(OUTPUT_DIR, "timing_advice_interaction.csv"))

# Join by the hypothesis AND the oriented contrast, never by row position.
research_contrasts <- primary_tests %>% filter(hypothesis %in% c("H2", "H3", "H4", "H5")) %>%
  mutate(paired_contrast = sub(" / ", " - ", contrast, fixed = TRUE)) %>%
  select(hypothesis, outcome, contrast, paired_contrast, n_participants,
         odds_ratio = estimate, odds_ratio_conf_low = conf_low,
         odds_ratio_conf_high = conf_high, model_p_raw = p_value_raw,
         model_p_holm = p_value_adjusted, contrast_result) %>%
  left_join(sensitivity_tests %>%
              transmute(hypothesis, paired_contrast = contrast,
                        mean_first, mean_second, difference_pp = estimate * 100,
                        paired_conf_low_pp = conf_low * 100,
                        paired_conf_high_pp = conf_high * 100,
                        paired_p_raw = p_value_raw, paired_p_holm = p_value_adjusted),
            by = c("hypothesis", "paired_contrast")) %>%
  mutate(confidence_intervals = "95% pointwise; p-values Holm within each hypothesis",
         difference_estimator = "Equal-weight participant means; paired t interval")
stopifnot(nrow(research_contrasts) == 12L, !anyNA(research_contrasts))
write_csv(research_contrasts, file.path(OUTPUT_DIR, "research_question_contrasts.csv"))

diagnostic_availability <- tibble(
  diagnostic = c("Optimizer convergence", "Singularity", "Raw gradient", "Pearson dispersion",
                 "DHARMa simulation-based residual diagnostics"),
  status = c("Checked", "Checked", "Recorded; interpreted with optimizer convergence",
             "Checked; approximate for Bernoulli mixed models", "Not run"),
  detail = c("H2-H5, timing interaction, and harmful-switch trust model",
             "Participant random-intercept models", "Raw gradient is not the scaled convergence criterion",
             "No substitution for simulation-based residual checks",
             if (requireNamespace("DHARMa", quietly = TRUE)) "Outside this established diagnostic workflow"
             else "DHARMa is not installed")
)
write_csv(diagnostic_availability, file.path(OUTPUT_DIR, "diagnostic_availability.csv"))

# Self-contained HTML, generated from tables rather than hand-copied results.
html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
html_table <- function(data, caption) {
  header <- paste0("<th scope='col'>", html_escape(names(data)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(data)), function(i) {
    values <- vapply(data, function(x) as.character(x[[i]]), character(1))
    paste0("<tr>", paste0("<td>", html_escape(values), "</td>", collapse = ""), "</tr>")
  }, character(1))
  paste0("<div class='table-wrap'><table><caption>", html_escape(caption),
         "</caption><thead><tr>", header, "</tr></thead><tbody>",
         paste(rows, collapse = "\n"), "</tbody></table></div>")
}
num <- function(x, digits = 2) formatC(x, digits = digits, format = "f")
p_text <- function(x) vapply(x, function(p) if (p < .001) "< .001" else num(p, 3), character(1))
interval <- function(est, lo, hi) paste0(num(est), " [", num(lo), ", ", num(hi), "]")
contrast_display <- function(data) data %>% transmute(
  Family = hypothesis, Contrast = paired_contrast,
  `First mean (%)` = num(mean_first * 100), `Second mean (%)` = num(mean_second * 100),
  `Difference pp [95% CI]` = interval(difference_pp, paired_conf_low_pp, paired_conf_high_pp),
  `OR [95% CI]` = interval(odds_ratio, odds_ratio_conf_low, odds_ratio_conf_high),
  `Model p (Holm)` = p_text(model_p_holm), `Paired p (Holm)` = p_text(paired_p_holm)
)
direct <- research_contrasts %>% filter(paired_contrast == "Stimulus first - Aid first") %>%
  arrange(match(hypothesis, c("H5", "H4", "H2", "H3")))
lead_sentence <- function(family, description) {
  row <- direct %>% filter(hypothesis == family)
  finding <- if (row$model_p_holm >= .05) {
    "No statistically detectable timing difference"
  } else if (row$odds_ratio > 1) "Higher in Stimulus-first" else "Lower in Stimulus-first"
  paste0("<li><strong>", description, ": ", finding, ".</strong> ",
         num(row$mean_first * 100), "% versus ", num(row$mean_second * 100),
         "% (Stimulus-first versus Aid-first); difference ",
         interval(row$difference_pp, row$paired_conf_low_pp, row$paired_conf_high_pp),
         " percentage points; model Holm p ", p_text(row$model_p_holm), ".</li>")
}
trust_row <- exploratory_test
h1_row_report <- h1_test
interaction_row_report <- timing_results %>% filter(grepl(":", term, fixed = TRUE))
switch_display <- switch_summary %>% transmute(
  Condition = as.character(condition), Advice = advice_subset,
  `D1 accuracy (%)` = num(decision1_accuracy * 100),
  `Beneficial (%)` = num(beneficial_proportion * 100),
  `Harmful (%)` = num(harmful_proportion * 100),
  `Switches (%)` = num(switch_proportion * 100),
  `Net gain (pp)` = num(net_accuracy_gain * 100),
  `D2 accuracy (%)` = num(decision2_accuracy * 100)
)
conditional_display <- switch_summary %>% transmute(
  Condition = as.character(condition), Advice = advice_subset,
  `Initially incorrect trials` = n_initially_incorrect,
  `Initially correct trials` = n_initially_correct,
  `Beneficial / initially incorrect (%)` = num(conditional_beneficial * 100),
  `N contributing (beneficial)` = n_conditional_beneficial,
  `Harmful / initially correct (%)` = num(conditional_harmful * 100),
  `N contributing (harmful)` = n_conditional_harmful
)
switch_explanation <- paste0(
  "<p>Each percentage is calculated within participant, condition and the displayed advice subset, ",
  "then averaged equally across participants. Beneficial and harmful percentages in the first table ",
  "use all trials in that cell as the denominator. Total switches = beneficial + harmful; ",
  "Decision 2 accuracy = Decision 1 accuracy + beneficial − harmful. Rounding can affect displayed sums.</p>",
  "<p>The conditional-rate table in the switch-breakdown report conditions beneficial switches on initially incorrect decisions and ",
  "harmful switches on initially correct decisions. Counts are pooled trial counts; percentages remain ",
  "participant means. A zero denominator is omitted from that conditional mean and the contributing ",
  "participant count is shown.</p>",
  "<p><strong>Timing limitation:</strong> Aid-first participants have seen advice before Decision 1. ",
  "Stimulus-first participants receive advice between decisions. The initial responses therefore ",
  "represent different information states; conditioning on initial accuracy selects different trial ",
  "subsets. Neither conditional harmful switching nor Decision 1→2 gain provides a comparable total ",
  "advice-effect estimate across timing conditions. H4 final accuracy is the direct susceptibility ",
  "contrast. More switching alone does not establish better decisions.</p>"
)
styles <- "body{font:16px/1.55 system-ui,-apple-system,sans-serif;color:#172538;background:#f4f6f9;margin:0}main{max-width:1250px;margin:36px auto;padding:36px;background:white;border-radius:12px}h1{font-size:30px;line-height:1.2}h2{font-size:23px;margin-top:36px}h3{font-size:19px;margin-top:28px}.meta,.note{color:#526174}.lead{background:#edf4fa;padding:16px 24px;border-left:4px solid #27739c}.table-wrap{overflow-x:auto;margin:20px 0}table{border-collapse:collapse;width:100%;font-size:14px}caption{text-align:left;font-weight:650;margin-bottom:10px}th,td{text-align:left;padding:10px;border-bottom:1px solid #dce3eb;vertical-align:top}th{background:#eaf0f5}tbody tr:nth-child(even){background:#f7f9fb}a{color:#14668b}li{margin-bottom:10px}code{overflow-wrap:anywhere}@media(max-width:700px){main{margin:0;padding:18px}h1{font-size:25px}}@media print{body{background:white}main{margin:0;padding:0;max-width:none}h2,h3{break-after:avoid}tr{break-inside:avoid}thead{display:table-header-group}.table-wrap{overflow:visible}table{font-size:10px}th,td{padding:5px}}"
document <- function(title, body) paste0(
  "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width,initial-scale=1'><title>", title,
  "</title><style>", styles, "</style></head><body><main><h1>", title, "</h1>",
  "<p class='meta'>Semester 2, 2026 · 60 participants · 46,800 experimental trials · Generated ",
  Sys.Date(), "</p>", body, "</main></body></html>"
)
cohort_note <- paste0(
  "<p><strong>Corrected cohort:</strong> All 60 participants are included. The earlier p59 run ",
  "<code>20260903_120309</code> was excluded for chance performance and replaced by ",
  "<code>20260924_110831</code>. The replacement contributes all three conditions and its matched ",
  "questionnaire and slider responses. These results supersede the previous 59-participant outputs.</p>"
)
switch_body <- paste0(cohort_note, switch_explanation,
                     html_table(switch_display, "Accuracy and revision decomposition"),
                     html_table(conditional_display, "Conditional revision rates and denominators"),
                     "<p><a href='research_questions_report.html'>Research questions and H1–H5 results</a></p>")
writeLines(document("Decision-switch proportion breakdowns", switch_body),
           file.path(OUTPUT_DIR, "switch_proportion_breakdowns.html"))

family_descriptions <- c(
  H2 = "Decision revisions: Stimulus-first > Aid-first, Stimulus-first > Manual, and Aid-first > Manual.",
  H3 = "Final accuracy with correct advice: Stimulus-first > Aid-first, and each aided condition > overall Manual.",
  H4 = "Final accuracy with incorrect advice: Manual > each aided condition, and Stimulus-first > Aid-first.",
  H5 = "Overall final accuracy: Stimulus-first > Aid-first, and each aided condition > Manual."
)
family_sections <- paste(vapply(names(family_descriptions), function(h) paste0(
  "<h3>", h, "</h3><p><strong>Predicted directions:</strong> ", html_escape(family_descriptions[[h]]), "</p>",
  html_table(contrast_display(research_contrasts %>% filter(hypothesis == h)),
             paste(h, "planned contrast family"))), character(1)), collapse = "\n")
diag_display <- bind_rows(
  model_diagnostics %>% transmute(Model = hypothesis, Converged = converged,
                                  Singular = singular, `Raw max gradient` = num(max_absolute_gradient, 6),
                                  `Pearson dispersion` = num(pearson_dispersion_ratio, 3)),
  timing_diagnostics %>% transmute(Model = "Timing × advice", Converged = converged,
                                   Singular = singular, `Raw max gradient` = num(max_absolute_gradient, 6),
                                   `Pearson dispersion` = num(pearson_dispersion_ratio, 3)),
  harmful_switch_model_diagnostics %>% transmute(Model = "Harmful switches × trust", Converged = converged,
                                                Singular = singular, `Raw max gradient` = num(max_absolute_gradient, 6),
                                                `Pearson dispersion` = "Not computed")
)
correlation_display <- trust_performance_correlations %>% transmute(
  Condition = condition, Outcome = performance_definition, Method = method,
  Correlation = num(correlation, 3), `Unadjusted p` = p_text(p_value)
)
body <- paste0(
  cohort_note,
  "<h2>Research questions</h2><p>The aim is to examine how advice timing affects accuracy, changes ",
  "of mind and trust. The prediction is that advice after an independent judgment produces more ",
  "revisions and better final accuracy than advice before the stimulus. Reduced susceptibility to ",
  "incorrect advice predicts higher final accuracy on incorrect-advice trials.</p>",
  "<div class='lead'><ul>", lead_sentence("H5", "Better final decisions overall"),
  lead_sentence("H4", "Resistance to incorrect advice"), lead_sentence("H2", "More revisions"),
  lead_sentence("H3", "Accuracy with correct advice"), "</ul></div>",
  "<p>A nonsignificant timing contrast does not demonstrate equivalence. Conclusions about the ",
  "overall prediction must distinguish increased revision frequency from improved final decisions.</p>",
  html_table(contrast_display(direct), "Direct timing contrasts: Stimulus-first minus Aid-first"),
  "<h2>Analysis and reporting conventions</h2><p>H1–H5 retain their established hierarchy. H2–H5 ",
  "use trial-level logistic mixed models with a participant random intercept only. All tests are ",
  "two-sided. Three contrasts per H2–H5 family receive Holm adjustment; H1 is a single paired test. ",
  "The paired participant tests are sensitivity analyses with the same within-family correction. ",
  "New secondary analyses are labelled exploratory, not retrospectively described as preregistered.</p>",
  "<p>Displayed means and percentage-point differences weight participants equally. Their 95% ",
  "confidence intervals are paired t intervals; model odds ratios use Wald intervals. All intervals ",
  "are pointwise, not multiplicity-adjusted. Model odds ratios are conditional on the participant ",
  "random intercept and are distinct from participant-mean percentage-point differences. The ",
  "model probability CSV contains inverse-logit fixed-effect predictions at random intercept zero, ",
  "not probabilities integrated over participant heterogeneity.</p>",
  "<p>Manual has no advice-correctness classification. In H3 and H4, all Manual trials are the ",
  "reference, while aided trials are restricted by advice correctness. These are not matched ",
  "Manual advice-correctness cells. Primary overall accuracy uses the observed advice mix; correct ",
  "and incorrect advice receive their observed trial frequencies, not equal 50:50 weight.</p>",
  "<h2>Complete H1–H5 results</h2><h3>H1: Perceived reliability</h3><p>The mean aided reliability ",
  "rating was ", num(h1_row_report$mean_first), "% versus ", num(h1_row_report$mean_second),
  "% for Manual self-reliability. Paired difference ",
  interval(h1_row_report$estimate, h1_row_report$conf_low, h1_row_report$conf_high),
  " rating percentage points; p ", p_text(h1_row_report$p_value_raw),
  ". The aided score averages the two aided conditions equally. This comparison concerns ",
  "perceived aid reliability versus perceived self-reliability; it is distinct from six-item trust ",
  "and does not directly compare advice timing.</p>", family_sections,
  "<h2>Trust: exploratory timing comparison</h2><p>Mean six-item trust (1–5) was ",
  num(trust_row$mean_first), " in Stimulus-first and ", num(trust_row$mean_second),
  " in Aid-first. Paired difference ", interval(trust_row$estimate, trust_row$conf_low, trust_row$conf_high),
  " scale points; unadjusted p ", p_text(trust_row$p_value_raw),
  ". Trust, perceived reliability and behavioural advice acceptance are distinct measures.</p>",
  "<details><summary>Exploratory trust–behaviour associations</summary><p>These are across-participant ",
  "associations using post-block trust. They cannot establish that trust caused reliance or mediated ",
  "the timing effect. P-values are unadjusted and should not be treated as confirmatory evidence. ",
  "Pearson confidence intervals, Spearman checks, paired participant bootstrap comparisons and the ",
  "harmful-switch trust model are available in the linked CSVs.</p>",
  html_table(correlation_display, "Trust associations"), "</details>",
  "<h2>Why revisions and final accuracy can differ</h2>", switch_explanation,
  html_table(switch_display, "Decision transitions and net accuracy gain"),
  "<p><a href='switch_proportion_breakdowns.html'>Conditional revision rates and denominators</a></p>",
  "<h2>Secondary timing × advice-correctness interaction</h2><p>An aided-only logistic mixed ",
  "model includes timing, advice correctness and their interaction, with a participant random ",
  "intercept. The interaction is the Stimulus-first/Aid-first accuracy odds ratio under incorrect ",
  "advice divided by that under correct advice: ",
  interval(interaction_row_report$odds_ratio, interaction_row_report$conf_low, interaction_row_report$conf_high),
  "; unadjusted exploratory p ", p_text(interaction_row_report$p_value),
  ". This is an interaction on the log-odds scale, not a percentage-point difference-in-differences. ",
  "It supplements the separate H3 and H4 timing contrasts without changing their tests.</p>",
  "<h2>Validation and diagnostics</h2><p>The inputs contain 60 participants, 260 trials in each of ",
  "three conditions per participant, 720 questionnaire rows and 180 slider rows. Source filenames ",
  "and checksums are recorded in the run manifest. Binary transitions satisfy both switch and ",
  "accuracy identities before aggregation.</p>",
  html_table(diag_display, "Model checks"),
  "<p>Raw gradients are recorded alongside optimizer convergence and are not the scaled convergence ",
  "criterion. Pearson dispersion checks are approximate for Bernoulli mixed models. Simulation-based ",
  "DHARMa residual diagnostics were not run; see diagnostic_availability.csv. Passing these checks ",
  "does not establish model adequacy. Participant-paired sensitivity results help assess dependence ",
  "on the participant-random-intercept specification.</p>",
  "<h2>Reproduce and inspect</h2><pre><code>Rscript collate_data.R output/semester2_2026_data data\n",
  "Rscript create_averaged_within_participants.R\nRscript test_aid_onset_hypotheses.R</code></pre>",
  "<p>Run from the repository root. Existing H1–H5 outputs and this report are regenerated together.</p><ul>",
  paste0("<li><a href='", c("research_question_contrasts.csv", "primary_hypothesis_tests.csv",
                           "participant_level_sensitivity_tests.csv", "switch_decomposition_participants.csv",
                           "switch_decomposition_summary.csv", "timing_advice_interaction.csv",
                           "trust_performance_correlations.csv", "trust_performance_correlation_difference.csv",
                           "harmful_switch_model_results.csv", "source_run_manifest.csv",
                           "analysis_exclusions.csv", "diagnostic_availability.csv"), "'>",
         c("Research-question contrast table", "Primary H1–H5 tests and exploratory trust",
           "Paired sensitivity tests", "Participant revision decomposition", "Group revision decomposition",
           "Timing interaction coefficients", "Trust correlations", "Trust correlation comparisons",
           "Harmful-switch trust model", "Source-run manifest", "Run-specific exclusion record",
           "Diagnostic availability"), "</a></li>", collapse = ""), "</ul>"
)
writeLines(document("Advice timing: behavioural research questions", body),
           file.path(OUTPUT_DIR, "research_questions_report.html"))

input_paths <- c(PARTICIPANT_CSV, TRIAL_CSV, manifest_file,
                 "collate_data.R", "create_averaged_within_participants.R",
                 "test_aid_onset_hypotheses.R", "aid_onset_research_questions.R")
write_csv(tibble(path = normalizePath(input_paths),
                 md5 = unname(tools::md5sum(input_paths))),
          file.path(OUTPUT_DIR, "analysis_input_checksums.csv"))
writeLines(capture.output(sessionInfo()), file.path(OUTPUT_DIR, "session_info.txt"))
