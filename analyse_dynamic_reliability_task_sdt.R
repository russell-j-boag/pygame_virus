# Complementary stimulus-based SDT: final response versus actual stimulus.
# Rscript analyse_dynamic_reliability_task_sdt.R [trials_csv] [qc_dir] [analysis_dir] [plot_dir]
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2); library(patchwork)
})
task_check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
task_sha <- function(path) digest::digest(file = path, algo = "sha256")
task_hashes <- function(paths) tibble(path = normalizePath(paths, mustWork = TRUE),
  sha256 = vapply(paths, task_sha, character(1), USE.NAMES = FALSE))
task_stages <- c("PRE", "P1", "P2", "P3", "POST")
task_groups <- c("CAL65", "CAL90", "CAL90 - CAL65")
task_colours <- c(CAL65 = "#0072B2", CAL90 = "#D55E00", "CAL90 - CAL65" = "#334858")
task_stage_labels <- c(PRE = "Manual\npre", P1 = "Phase 1\n95% aid", P2 = "Phase 2\n70% aid",
  P3 = "Phase 3\n95% aid", POST = "Manual\npost")

task_read_inputs <- function(input, qc) {
  manifest <- jsonlite::read_json(file.path(qc, "manifest.json"), simplifyVector = TRUE)
  task_check(identical(manifest$generator, "participant_qc") &&
    identical(task_sha(input), manifest$input_sha256), "QC is missing or belongs to different trial data")
  consumed <- c(input, file.path(qc, "manifest.json"), manifest$config_path, manifest$sources$path)
  task_check(task_sha(manifest$config_path) == manifest$config_sha256 &&
    all(vapply(manifest$sources$path, task_sha, character(1)) == manifest$sources$sha256),
    "QC source/configuration changed; regenerate the QC report first")
  for (name in c("exclusion_summary.json", "participant_summary.csv")) {
    m <- filter(manifest$outputs, path == name)
    task_check(nrow(m) == 1 && task_sha(file.path(qc, name)) == m$sha256,
      paste("QC artifact is stale:", name))
    consumed <- c(consumed, file.path(qc, name))
  }
  summary <- jsonlite::read_json(file.path(qc, "exclusion_summary.json"), simplifyVector = TRUE)
  people <- read_csv(file.path(qc, "participant_summary.csv"), show_col_types = FALSE)
  rules <- c("calibration_target", "near_chance_or_worse", "anticipatory")
  task_check(setequal(summary$exclusion_categories, rules) &&
    identical(summary$diagnostic_categories, "manual_target") &&
    all(people$n_not_assessable == 0) && length(summary$incomplete_assessment_ids) == 0,
    "Expected three exclusion rules, diagnostic-only manual target, and fully assessable QC")
  selected <- Reduce(`|`, lapply(people[rules], function(x) x == "flag"))
  task_check(!anyNA(selected) && !anyNA(people$exclude_by_rules) &&
    all(selected == people$exclude_by_rules) &&
    setequal(people$participant_id[selected], unlist(summary$participant_ids)) &&
    sum(selected) == summary$unique_flagged_n, "QC union and participant decisions disagree")
  raw <- read_csv(input, show_col_types = FALSE)
  required <- c("participant_id", "run_timestamp", "calibration_target_group", "calibration_target_accuracy",
    "key_black", "key_white", "main_block_order", "manual_segment", "reliability_phase_label",
    "automation_reliability_group", "block", "global_trial", "stimulus", "response", "aid_label", "aid_correct", "correct", "rt_s")
  task_check(all(required %in% names(raw)), "Missing trial columns")
  task_check(!anyDuplicated(raw[c("participant_id", "global_trial")]) &&
    !anyNA(raw[c("participant_id", "run_timestamp", "stimulus", "response", "block", "global_trial")]) &&
    all(raw$stimulus %in% c("BLACK", "WHITE")) &&
    all(raw$response %in% c("BLACK", "WHITE", "TIMEOUT")) &&
    all(raw$block %in% c("CALIBRATION", "MANUAL", "AUTOMATION")), "Invalid or duplicate trial identities/labels")
  metadata <- raw %>% distinct(participant_id, run_timestamp, calibration_target_group,
    calibration_target_accuracy, key_black, key_white, main_block_order)
  task_check(!anyDuplicated(metadata$participant_id) && !anyNA(metadata) &&
    setequal(metadata$participant_id, people$participant_id) &&
    nrow(people) == summary$participant_n && !anyDuplicated(people$participant_id),
    "Ambiguous run/metadata or QC cohort mismatch")
  matched <- people[match(metadata$participant_id, people$participant_id), ]
  task_check(all(metadata$run_timestamp == matched$run_timestamp) &&
    all(metadata$calibration_target_group == matched$calibration_target_group) &&
    all(metadata$calibration_target_group %in% task_groups[1:2]) &&
    all(metadata$calibration_target_accuracy == ifelse(metadata$calibration_target_group == "CAL65", .65, .9)) &&
    all(metadata$key_black %in% c("D", "J") & metadata$key_white %in% c("D", "J") & metadata$key_black != metadata$key_white) &&
    all(metadata$main_block_order == "SPLIT_MANUAL"), "Unexpected design metadata")
  trials <- raw %>% filter(block != "CALIBRATION") %>% mutate(
    group = calibration_target_group,
    stage = case_when(block == "MANUAL" & manual_segment == "PRE_AUTOMATION" ~ "PRE",
      block == "MANUAL" & manual_segment == "POST_AUTOMATION" ~ "POST",
      block == "AUTOMATION" & reliability_phase_label == "P1_95" ~ "P1",
      block == "AUTOMATION" & reliability_phase_label == "P2_70" ~ "P2",
      block == "AUTOMATION" & reliability_phase_label == "P3_95" ~ "P3"),
    answered = response %in% c("BLACK", "WHITE"), response_correct = answered & response == stimulus)
  task_check(!anyNA(trials$stage) && nrow(trials) == nrow(metadata)*1600 &&
    !anyNA(trials$correct[trials$answered]) &&
    all(trials$correct[trials$answered] == trials$response_correct[trials$answered]) &&
    all(is.na(trials$correct[!trials$answered]) | !trials$correct[!trials$answered]),
    "Incomplete task or response correctness mismatch")
  bounds <- list(PRE = 301:500, P1 = 501:900, P2 = 901:1300, P3 = 1301:1700, POST = 1701:1900)
  for (d in split(trials, interaction(trials$participant_id, trials$stage, drop = TRUE)))
    task_check(identical(sort(as.integer(d$global_trial)), bounds[[d$stage[1]]]), "Missing, extra or misplaced stage trials")
  aided <- filter(trials, block == "AUTOMATION")
  task_check(all(aided$aid_label %in% c("BLACK", "WHITE")) && !anyNA(aided$aid_correct) &&
    all(aided$aid_correct == (aided$aid_label == aided$stimulus)) &&
    all(aided$automation_reliability_group == ifelse(aided$stage == "P2", "low", "high")),
    "Invalid aid labels/correctness/reliability metadata")
  membership <- metadata %>% left_join(select(people, participant_id, exclude_by_rules, exclusion_reasons,
      manual_target), by = "participant_id") %>% mutate(in_primary = !exclude_by_rules, in_all_participants = TRUE)
  list(trials = trials, membership = membership, inputs = unique(consumed))
}

task_confusion <- function(d, agent) {
  # Preserve empty denominators so a missing signal class fails, rather than becoming a zero rate.
  d %>% group_by(participant_id, group, stage) %>% summarise(
    hits = sum(eligible & stimulus == "BLACK" & decision == "BLACK"),
    misses = sum(eligible & stimulus == "BLACK" & decision == "WHITE"),
    false_alarms = sum(eligible & stimulus == "WHITE" & decision == "BLACK"),
    correct_rejections = sum(eligible & stimulus == "WHITE" & decision == "WHITE"),
    .groups = "drop") %>% mutate(agent = agent, n_signal = hits + misses,
      n_noise = false_alarms + correct_rejections, n = n_signal + n_noise)
}
task_counts <- function(trials) {
  aided <- filter(trials, stage %in% c("P1", "P2", "P3"))
  bind_rows(task_confusion(mutate(trials, decision = response, eligible = answered), "participant"),
    task_confusion(mutate(aided, decision = aid_label, eligible = answered), "aid_matched"),
    task_confusion(mutate(aided, decision = aid_label, eligible = TRUE), "aid_all"))
}
task_scores <- function(counts, correction = c("loglinear", "extreme_only")) {
  correction <- match.arg(correction)
  cells <- c("hits", "misses", "false_alarms", "correct_rejections")
  task_check(all(c(cells, "n_signal", "n_noise", "n") %in% names(counts)) && nrow(counts) > 0 &&
    !anyNA(counts) && !anyDuplicated(counts[c("participant_id", "group", "stage", "agent")]), "Missing/duplicate SDT cells")
  task_check(all(vapply(counts[cells], function(x) all(is.finite(x) & x >= 0 & x == floor(x)), logical(1))) &&
    all(counts$n_signal == counts$hits + counts$misses & counts$n_noise == counts$false_alarms + counts$correct_rejections) &&
    all(counts$n_signal > 0 & counts$n_noise > 0 & counts$n == counts$n_signal + counts$n_noise),
    "Each SDT cell requires valid integer counts and both nonempty stimulus classes")
  rate <- function(k, n) if (correction == "loglinear") (k + .5)/(n + 1) else
    ifelse(k == 0, .5/n, ifelse(k == n, 1-.5/n, k/n))
  counts %>% mutate(hit_rate = rate(hits, n_signal), false_alarm_rate = rate(false_alarms, n_noise),
    dprime = qnorm(hit_rate)-qnorm(false_alarm_rate),
    criterion_c = -(qnorm(hit_rate)+qnorm(false_alarm_rate))/2, correction = correction)
}
task_performance <- function(trials) trials %>% group_by(participant_id, group, stage) %>% summarise(
  n_trials = n(), n_answered = sum(answered), n_timeouts = sum(!answered), n_correct = sum(response_correct),
  accuracy = mean(response_correct), timeout_rate = mean(!answered),
  answered_accuracy = sum(response_correct)/sum(answered),
  n_nonpositive_rt = sum(answered & is.finite(rt_s) & rt_s <= 0),
  n_fast = sum(answered & is.finite(rt_s) & rt_s < .2),
  n_answered_missing_rt = sum(answered & !is.finite(rt_s)), .groups = "drop")

# Families declared before computing results. H1/H3 are complementary outcomes,
# not a relabelling of the original reliance hypotheses as performance hypotheses.
task_specs <- function() tibble(
  family = c(rep("manual_benefit", 3), rep("reliability", 3), "manual_drift", rep("aid_benchmark", 3)),
  hypothesis = c(rep("Complementary benefit/cost", 3), "H1 performance complement", rep("H3 performance complement", 2),
    "Manual context", rep("Aid benchmark", 3)),
  comparison = c("P1 - PRE", "P2 - PRE", "P3 - PRE", "P2 - P1", "P3 - P2", "P3 - P1", "POST - PRE",
    "P1: participant - aid", "P2: participant - aid", "P3: participant - aid"),
  left = c("P1", "P2", "P3", "P2", "P3", "P3", "POST", "P1", "P2", "P3"),
  right = c("PRE", "PRE", "PRE", "P1", "P2", "P1", "PRE", "P1", "P2", "P3"),
  right_agent = c(rep("participant", 7), rep("aid_matched", 3)))
task_inference <- function(a, b = NULL) {
  n1 <- length(a); n2 <- length(b)
  estimate <- mean(a) - if (is.null(b)) 0 else mean(b)
  va <- if (n1 > 1) var(a)/n1 else NA_real_
  vb <- if (is.null(b)) 0 else if (n2 > 1) var(b)/n2 else NA_real_
  se <- sqrt(va+vb)
  df <- if (is.null(b)) n1-1 else (va+vb)^2/(va^2/(n1-1)+vb^2/(n2-1))
  ok <- is.finite(se) && se > 0 && is.finite(df) && df > 0 && is.finite(estimate)
  tibble(estimate, se, df, lower = if (ok) estimate-qt(.975, df)*se else NA_real_,
    upper = if (ok) estimate+qt(.975, df)*se else NA_real_, p_raw = if (ok) 2*pt(-abs(estimate/se), df) else NA_real_,
    inference_available = ok, n_participants = n1+n2,
    method = if (is.null(b)) "Paired participant change" else "Welch comparison of participant changes")
}
task_contrasts <- function(scores) {
  changes <- list(); specs <- task_specs()
  for (i in seq_len(nrow(specs))) for (outcome in c("dprime", "criterion_c")) {
    s <- specs[i, ]
    a <- scores %>% filter(agent == "participant", stage == s$left) %>%
      select(sample, correction, participant_id, group, left_value = all_of(outcome))
    b <- scores %>% filter(agent == s$right_agent, stage == s$right) %>%
      select(sample, correction, participant_id, group, right_value = all_of(outcome))
    d <- left_join(a, b, by = c("sample", "correction", "participant_id", "group"))
    task_check(nrow(d) == nrow(a) && !anyNA(d), "Unpaired or missing comparison values")
    changes[[length(changes)+1L]] <- d %>% mutate(value = left_value-right_value, outcome = outcome,
      family = s$family, hypothesis = s$hypothesis, comparison = s$comparison)
  }
  changes <- bind_rows(changes)
  results <- changes %>% group_by(sample, correction, outcome, family, hypothesis, comparison) %>%
    group_modify(function(d, keys) bind_rows(lapply(task_groups, function(g) {
      a <- d$value[d$group == if (g == "CAL65") "CAL65" else "CAL90"]
      b <- if (g == "CAL90 - CAL65") d$value[d$group == "CAL65"] else NULL
      task_inference(a, b) %>% mutate(group = g)
    }))) %>% ungroup() %>% group_by(sample, correction, outcome, family) %>%
    mutate(family_n = n(), p_holm = p.adjust(p_raw, "holm", n = n())) %>% ungroup() %>%
    group_by(sample, correction, family) %>% mutate(p_holm_both_outcomes = p.adjust(p_raw, "holm", n = n())) %>% ungroup() %>%
    group_by(sample, correction) %>% mutate(p_holm_all_contrasts = p.adjust(p_raw, "holm", n = n())) %>% ungroup()
  list(changes = changes, results = results)
}
task_sensitivity <- function(results) {
  results %>% select(sample, correction, outcome, family, comparison, group, estimate, p_holm) %>%
    mutate(variant = paste(sample, correction, sep = "_")) %>% select(-sample, -correction) %>%
    pivot_wider(names_from = variant, values_from = c(estimate, p_holm)) %>% mutate(
      sample_sensitive = sign(estimate_primary_loglinear) != sign(estimate_all_participants_loglinear) |
        ((p_holm_primary_loglinear < .05) != (p_holm_all_participants_loglinear < .05)),
      correction_sensitive_primary = sign(estimate_primary_loglinear) != sign(estimate_primary_extreme_only) |
        ((p_holm_primary_loglinear < .05) != (p_holm_primary_extreme_only < .05)),
      correction_sensitive_all = sign(estimate_all_participants_loglinear) != sign(estimate_all_participants_extreme_only) |
        ((p_holm_all_participants_loglinear < .05) != (p_holm_all_participants_extreme_only < .05)))
}
task_summaries <- function(d, levels) {
  # Cousineau-Morey normalization within sample x group x agent x correction x outcome.
  keys <- intersect(c("sample", "group", "agent", "correction", "outcome"), names(d)); k <- length(levels)
  task_check(k > 1 && all(is.finite(d$value)) && !anyDuplicated(d[c(keys, "participant_id", "stage")]), "Invalid repeated-measures cells")
  completeness <- d %>% group_by(across(all_of(c(keys, "participant_id")))) %>%
    summarise(ok = n() == k && setequal(stage, levels), .groups = "drop")
  task_check(all(completeness$ok), "Morey intervals require all planned stages for every participant")
  normalized <- d %>% group_by(across(all_of(c(keys, "participant_id")))) %>% mutate(person_mean = mean(value)) %>%
    group_by(across(all_of(keys))) %>% mutate(normalized = value-person_mean+mean(value)) %>% ungroup()
  normalized %>% group_by(across(all_of(c(keys, "stage")))) %>% summarise(n = n(),
    se = sd(value)/sqrt(n()), se_morey = sd(normalized)/sqrt(n())*sqrt(k/(k-1)), mean = mean(value), .groups = "drop") %>%
    mutate(lower = mean-qt(.975, n-1)*se, upper = mean+qt(.975, n-1)*se,
      lower_morey = mean-qt(.975, n-1)*se_morey, upper_morey = mean+qt(.975, n-1)*se_morey,
      morey_k = k, normalization_stages = paste(levels, collapse = ";"),
      interval = "Ordinary pointwise 95% t CI; _morey columns give within-participant condition-pattern CIs")
}

task_theme <- function() theme_minimal(base_size = 14, base_family = "Helvetica") + theme(
  text = element_text(colour = "#1C2D3E"), panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "#E4E9ED", linewidth = .35),
  axis.text = element_text(colour = "#1C2D3E", size = 11), plot.title = element_text(face = "bold", size = 16),
  plot.subtitle = element_text(size = 11), legend.position = "bottom", legend.title = element_blank(),
  plot.margin = margin(10, 10, 10, 10))
task_annotation <- function(title, subtitle, caption) plot_annotation(title = title, subtitle = subtitle,
  caption = caption, theme = theme(text = element_text(family = "Helvetica", colour = "#1C2D3E"),
    plot.title = element_text(size = 22, face = "bold"), plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0, lineheight = 1.12), plot.margin = margin(12, 16, 12, 16)))
task_line <- function(d, stages, title, ylab) {
  d <- mutate(d, x = match(stage, stages)+ifelse(group == "CAL65", -.055, .055))
  ggplot(d, aes(x, mean, colour = group, group = group)) + geom_line(linewidth = .7) +
    geom_errorbar(aes(ymin = lower_morey, ymax = upper_morey), width = .08, linewidth = .8) +
    geom_point(size = 3) + scale_colour_manual(values = task_colours) +
    scale_x_continuous(breaks = seq_along(stages), labels = task_stage_labels[stages], expand = expansion(add = .3)) +
    scale_y_continuous(expand = expansion(mult = c(.12, .15))) + task_theme() + labs(title = title, x = NULL, y = ylab)
}
task_forest <- function(d, title, ylab) {
  d <- mutate(d, label = paste(gsub("\\bP([123])\\b", "Phase \\1",
      gsub("PRE", "manual pre", gsub("POST", "manual post", comparison)), perl = TRUE), group, sep = "\n"),
    label = factor(label, rev(unique(label))), symbol = paste0(
      ifelse(!inference_available, "NA", ifelse(p_holm < .05, "*", "ns")),
      ifelse(sample_sensitive, " [S]", ""), ifelse(correction_sensitive, " [C]", "")))
  span <- diff(range(c(d$lower, d$upper, 0), na.rm = TRUE)); if (span == 0) span <- 1
  d$annotation_x <- max(c(d$upper, 0), na.rm = TRUE)+.13*span
  p <- ggplot(d, aes(estimate, label, colour = group)) + geom_vline(xintercept = 0, linetype = "dashed", colour = "#8C969F") +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = .16, linewidth = .8) +
    geom_point(size = 2.8) + geom_text(aes(x = annotation_x, label = symbol), size = 3.5, show.legend = FALSE) +
    scale_colour_manual(values = task_colours, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(.06, .12))) + task_theme() +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E4E9ED", linewidth = .35),
      axis.text.y = element_text(size = 9.5), plot.title.position = "plot") + labs(title = title, x = ylab, y = NULL)
  list(plot = p, data = d)
}

task_figures <- function(sample_name, summaries, performance_summary, results, sensitivity, folder, return_bundle = FALSE) {
  dir.create(file.path(folder, "data"), recursive = TRUE, showWarnings = FALSE)
  s <- filter(summaries, sample == sample_name, correction == "loglinear")
  perf <- filter(performance_summary, sample == sample_name)
  r <- filter(results, sample == sample_name, correction == "loglinear") %>% left_join(
    select(sensitivity, outcome, family, comparison, group, sample_sensitive,
      correction_sensitive_primary, correction_sensitive_all), by = c("outcome", "family", "comparison", "group")) %>%
    mutate(correction_sensitive = if (sample_name == "primary") correction_sensitive_primary else correction_sensitive_all)
  sizes <- s %>% filter(agent == "participant", stage == "PRE", outcome == "dprime")
  sample_label <- paste0(if (sample_name == "primary") "QC-retained primary sample" else "All-participant sensitivity", "; ",
    paste(paste0(sizes$group, " n = ", sizes$n), collapse = ", "))
  mean_note <- "Means: Cousineau-Morey within-participant 95% CIs within group (five stages); equal participant weights."
  test_note <- "Changes: paired / Welch tests; pointwise 95% CIs. * Holm p < .05; ns p >= .05; CAL90 - CAL65 compares changes."
  limits_note <- "Exploratory; fixed order includes practice/fatigue. Nonsignificance does not establish equivalence."
  figures <- list(); catalog <- list()
  save <- function(stem, p, title, data, annotations = tibble(note = "Descriptive figure; no tests")) {
    for (ext in c("png", "pdf")) ggsave(file.path(folder, paste0(stem, ".", ext)), p,
      width = 12, height = 6.75, dpi = 300, bg = "white", device = if (ext == "pdf") cairo_pdf else "png")
    write_csv(data, file.path(folder, "data", paste0(stem, "_data.csv")))
    write_csv(annotations, file.path(folder, "data", paste0(stem, "_annotations.csv")))
    figures[[stem]] <<- p
    catalog[[stem]] <<- tibble(sample = sample_name, filename = paste0(sample_name, "/", stem), title = title)
  }
  d <- filter(s, agent == "participant")
  p <- task_line(filter(d, outcome == "dprime"), task_stages, "Final-response discrimination", "Task d-prime") +
    task_line(filter(d, outcome == "criterion_c"), task_stages, "Colour-response criterion", "Criterion c (positive = WHITE bias)") +
    plot_layout(guides = "collect") + task_annotation("Task SDT across the full experimental sequence", sample_label,
      paste(mean_note, "\nAnswered trials, all RTs; loglinear correction. Aided d-prime reflects the participant + aid system.",
        "\nCriterion concerns BLACK/WHITE responses, not advice reliance.", limits_note))
  save("01_task_sdt_sequence", p, "Task SDT across the full experimental sequence", d)
  specs <- list(
    list("02_manual_benefit_cost", "Performance with aid relative to manual pre", "manual_benefit", NULL,
      "Nine tests per outcome in the manual-benefit family; positive d-prime differences indicate better discrimination."),
    list("03_H1_task_sdt_drop", "H1 complement: final performance after the reliability drop", "reliability", "P2 - P1",
      "Nine H1/H3 tests per outcome in the shared reliability family; negative d-prime differences indicate a drop."),
    list("04_H3_task_sdt_recovery", "H3 complement: recovery and residual performance differences", "reliability", c("P3 - P2", "P3 - P1"),
      "Nine H1/H3 tests per outcome in the shared reliability family; P3 - P1 tests a residual difference."),
    list("05_aid_only_benchmark", "Final responses compared with the aid alone", "aid_benchmark", NULL,
      "Nine tests per outcome in the aid-benchmark family; both decisions scored on the same answered trials."),
    list("06_manual_pre_post", "Manual performance before and after the aided block", "manual_drift", NULL,
      "Three tests per outcome in the manual-drift family; this is a context check, not an unexposed control."))
  for (spec in specs) {
    a <- filter(r, family == spec[[3]])
    if (!is.null(spec[[4]])) a <- filter(a, comparison %in% spec[[4]])
    a <- arrange(a, match(comparison, task_specs()$comparison), match(group, task_groups))
    left <- task_forest(filter(a, outcome == "dprime"), "Task discrimination", "Difference in d-prime")
    right <- task_forest(filter(a, outcome == "criterion_c"), "Colour-response criterion", "Difference in criterion c")
    p <- left$plot + right$plot + task_annotation(spec[[2]], sample_label,
      paste(test_note, "\n", spec[[5]], "\n[S] sample-sensitive; [C] correction-sensitive direction/test decision; see gallery.\n", limits_note))
    save(spec[[1]], p, spec[[2]], a, bind_rows(left$data, right$data))
  }
  p <- task_line(filter(perf, outcome == "accuracy"), task_stages, "Accuracy on all trials", "Accuracy (%)") +
    task_line(filter(perf, outcome == "timeout_rate"), task_stages, "Timeouts on all trials", "Timeouts (%)") +
    plot_layout(guides = "collect") + task_annotation("Accuracy and missing-response context", sample_label,
      paste(mean_note, "\nTimeouts count as incorrect for accuracy and are excluded from SDT. No RT-based trial trimming.",
        "\nIntervals are on the unbounded percentage scale; small negative timeout limits are possible.\n", limits_note))
  save("07_accuracy_timeouts", p, "Accuracy and missing-response context", perf)
  cairo_pdf(file.path(folder, "task_sdt_figures.pdf"), width = 12, height = 6.75, onefile = TRUE)
  tryCatch(invisible(lapply(figures, print)), finally = dev.off())
  if (return_bundle) list(catalog = bind_rows(catalog), figures = figures) else bind_rows(catalog)
}

task_notes <- function() c(
  "Complementary, retrospective exploratory stimulus-based SDT; existing advice SDT and behavioural analyses are retained.",
  "BLACK is the signal: H=P(response BLACK|stimulus BLACK), F=P(response BLACK|stimulus WHITE). Semantic responses, not D/J keys.",
  "d-prime=z(H)-z(F); c=-(z(H)+z(F))/2. Positive c favours WHITE. Aided d-prime measures effective participant-plus-aid discrimination, not pure visual sensitivity.",
  "Primary: participants retained by the three QC rules; manual target deviation remains diagnostic only. All participants provide a separate sensitivity analysis.",
  "QC was chosen retrospectively and includes performance after aid exposure. Selection can change group comparisons; inspect the all-participant sensitivity alongside the primary results.",
  "Manual pre/post are 200 trials; each aided phase is 400. Adaptive calibration is not a performance baseline. Equal participant weights.",
  "SDT includes all answered trials regardless of RT; timeouts are excluded, never coded as WHITE. All-trial accuracy counts timeouts as incorrect. No trial-level RT trimming.",
  "Loglinear correction (k+0.5)/(n+1) in every rate; extreme-only sensitivity substitutes 0.5/n or 1-0.5/n only for zero/one rates. Both stimulus classes must be present.",
  "Aid-matched SDT uses the same answered trials as participant SDT for paired benchmarks. Aid-all SDT additionally describes every aided trial, including participant timeouts.",
  "Manual-benefit family: P1-PRE, P2-PRE, P3-PRE. Reliability family: P2-P1 (H1 complement), P3-P2 and P3-P1 (H3 complements).",
  "Aid-benchmark family: participant-minus-aid in each phase. Manual-drift family: POST-PRE. Each contrast is tested within CAL65, within CAL90, and as CAL90-minus-CAL65 changes.",
  "Two-sided paired participant-change tests within group; Welch tests between groups. Pointwise 95% t intervals. No pseudoreplication of trials.",
  "Holm families are separate by sample, correction and outcome: 9 manual-benefit, 9 reliability, 9 aid-benchmark and 3 manual-drift tests. d-prime is the main performance index; c is secondary.",
  "Also export Holm adjustment across both outcomes within each family (18 or 6 tests), and across all 60 contrasts. Sensitivities are not additional independent replications.",
  "A group's adjusted p-value can change between samples even if its members, estimate and raw p-value are unchanged: Holm adjustment also depends on the other tests in the shared family.",
  "Trajectory figures use Cousineau-Morey within-participant 95% CIs, normalized within group over five participant stages; aid summaries normalize across three phases. Ordinary t CIs are also exported.",
  "Changing correct/incorrect-advice agreement to response correctness while retaining advice correctness as the signal only reparameterizes advice SDT: new d-prime=2*old(-c), new(-c)=old d-prime/2.",
  "These task SDT indices use actual stimulus classes. They reuse the same responses as accuracy and advice SDT, so are complementary descriptions, not independent evidence.",
  "Fixed sequence confounds reliability/aid exposure with time, practice and fatigue; pre/post manual blocks are not randomized controls. CAL65/CAL90 have different calibrated visual difficulty.",
  "Nonsignificant residual differences do not establish recovery to equivalence. Conventional equal-variance SDT is assumed. Criterion is a colour-response tendency, not advice reliance.",
  "References: Stanislaw & Todorov (1999), https://doi.org/10.3758/BF03207704; Hautus (1995), https://doi.org/10.3758/BF03203619; Morey (2008), https://doi.org/10.20982/tqmp.04.2.p061.")
task_escape <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE), fixed = TRUE)
task_html_table <- function(d) {
  d <- as.data.frame(d)
  for (k in names(d)) if (is.numeric(d[[k]])) d[[k]] <- formatC(d[[k]], digits = 4, format = "g")
  paste0("<table><thead><tr>", paste0("<th>", task_escape(names(d)), "</th>", collapse = ""), "</tr></thead><tbody>",
    paste(apply(d, 1, function(row) paste0("<tr>", paste0("<td>", task_escape(row), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
}
task_write_reports <- function(out, plots, catalog, membership, summaries, results, sensitivity) {
  primary <- results %>% filter(sample == "primary", correction == "loglinear") %>%
    select(family, outcome, comparison, group, estimate, lower, upper, p_holm, p_holm_both_outcomes, p_holm_all_contrasts)
  cohort <- membership %>% group_by(calibration_target_group) %>% summarise(all_participants = n(),
    primary = sum(in_primary), excluded = sum(!in_primary), .groups = "drop")
  overview <- summaries %>% filter(sample == "primary", correction == "loglinear", agent == "participant") %>%
    select(group, stage, outcome, n, mean) %>% pivot_wider(names_from = outcome, values_from = mean) %>%
    arrange(group, match(stage, task_stages))
  changed <- filter(sensitivity, sample_sensitive | correction_sensitive_primary | correction_sensitive_all)
  notes <- task_notes()
  writeLines(c(notes, "", "Cohort:", capture.output(print(as.data.frame(cohort), row.names = FALSE)),
    "", "Primary participant SDT:", capture.output(print(as.data.frame(overview), row.names = FALSE)),
    "", "Primary loglinear tests:", capture.output(print(as.data.frame(primary), row.names = FALSE)),
    "", "Sample/correction-sensitive conclusions (direction or Holm threshold):",
    capture.output(print(as.data.frame(changed), row.names = FALSE))), file.path(out, "report.txt"))
  style <- '<style>body{font:16px system-ui;color:#1c2d3e;max-width:1250px;margin:32px auto;padding:0 18px;line-height:1.5}img{width:100%}table{border-collapse:collapse;font-size:13px}th,td{padding:6px 9px;border-bottom:1px solid #dce3e8;text-align:left}th{background:#edf2f5}details{margin:18px 0;overflow:auto}a{color:#0072b2}li{margin:8px 0}</style>'
  report <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Stimulus-based SDT</title>', style,
    '</head><body><h1>Stimulus-based SDT: performance with and without aid</h1>',
    '<p>Retrospective exploratory complement to the existing advice-based SDT measures.</p>',
    '<h2>Cohort</h2>', task_html_table(cohort),
    paste0('<p>Excluded from this analysis only: ', paste(membership$participant_id[!membership$in_primary], collapse = ", "),
      '. Canonical data and earlier analyses remain unchanged. Manual target flags do not exclude.</p>'),
    '<h2>Primary task SDT</h2>', task_html_table(overview),
    '<details><summary>Definitions, inference and limitations</summary><ul>', paste0('<li>', task_escape(notes), '</li>'), '</ul></details>',
    '<h2>Primary contrasts</h2><p>Pointwise intervals; per-outcome family Holm p-values plus broader adjustments. These are estimates of sequence differences, not isolated causal effects of aid.</p>',
    '<details open><summary>All 60 primary tests</summary>', task_html_table(primary), '</details>',
    '<details><summary>Changes in direction or Holm decision across samples/corrections</summary>', task_html_table(changed), '</details>')
  writeLines(c(report, '<p>Full exports: <a href="participant_sdt.csv">Participant SDT</a> | <a href="contrasts.csv">All tests</a> | <a href="sensitivity_comparison.csv">All sensitivities</a> | <a href="cohort_membership.csv">Cohort</a></p></body></html>'), file.path(out, "index.html"))
  cards <- unlist(lapply(c("primary", "all_participants"), function(sample) {
    cat <- filter(catalog, .data$sample == .env$sample)
    c(paste0('<h2>', if (sample == "primary") 'Primary figures' else 'All-participant sensitivity figures',
      '</h2><p><a href="', sample, '/task_sdt_figures.pdf">Seven-figure PDF</a></p>'),
      vapply(seq_len(nrow(cat)), function(i) paste0('<h3>', task_escape(cat$title[i]), '</h3><img loading="lazy" src="',
        cat$filename[i], '.png" alt="', task_escape(cat$title[i]), '"><p><a href="', cat$filename[i], '.pdf">PDF</a> | <a href="',
        dirname(cat$filename[i]), '/data/', basename(cat$filename[i]), '_data.csv">Plotted data</a> | <a href="',
        dirname(cat$filename[i]), '/data/', basename(cat$filename[i]), '_annotations.csv">Tests</a></p>'), character(1)))
  }))
  # A self-contained numerical report precedes both galleries; no absolute file URLs.
  writeLines(c(report, cards, '</body></html>'), file.path(plots, "index.html"))
}

task_publish <- function(stage, destination) {
  if (dir.exists(destination)) {
    marker <- file.path(destination, "GENERATOR.txt")
    task_check(file.exists(marker) && identical(readLines(marker, warn = FALSE), "dynamic_reliability_task_sdt"),
      paste("Refusing to replace an unowned directory:", destination))
    backup <- paste0(destination, ".previous_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", Sys.getpid())
    task_check(file.rename(destination, backup), "Could not archive the preceding task-SDT report")
  }
  task_check(file.rename(stage, destination), "Could not publish task-SDT output")
}
run_task_sdt <- function(input, qc, out, plots, root) {
  task_check(normalizePath(out, mustWork = FALSE) != normalizePath(plots, mustWork = FALSE), "Analysis and plot directories must differ")
  for (path in c(out, plots)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(path)) task_check(file.exists(file.path(path, "GENERATOR.txt")) &&
      identical(readLines(file.path(path, "GENERATOR.txt"), warn = FALSE), "dynamic_reliability_task_sdt"),
      paste("Output directory is not owned by this pipeline:", path))
  }
  data <- task_read_inputs(input, qc)
  provenance <- task_hashes(c(data$inputs, file.path(root, "analyse_dynamic_reliability_task_sdt.R")))
  counts <- task_counts(data$trials)
  scores <- bind_rows(task_scores(counts), task_scores(counts, "extreme_only"))
  performance <- task_performance(data$trials)
  retained <- data$membership$participant_id[data$membership$in_primary]
  expand_samples <- function(d) bind_rows(mutate(filter(d, participant_id %in% retained), sample = "primary"),
    mutate(d, sample = "all_participants"))
  scores <- expand_samples(scores); performance <- expand_samples(performance)
  task_check(all(table(data$membership$calibration_target_group[data$membership$in_primary]) > 1) &&
    setequal(data$membership$calibration_target_group[data$membership$in_primary], task_groups[1:2]),
    "Both groups need at least two retained participants")
  tests <- task_contrasts(scores); sensitivity <- task_sensitivity(tests$results)
  long <- scores %>% pivot_longer(c(dprime, criterion_c), names_to = "outcome", values_to = "value")
  summaries <- bind_rows(task_summaries(filter(long, agent == "participant"), task_stages),
    task_summaries(filter(long, agent != "participant"), c("P1", "P2", "P3")))
  performance_summary <- performance %>% select(sample, participant_id, group, stage, accuracy, timeout_rate) %>%
    pivot_longer(c(accuracy, timeout_rate), names_to = "outcome", values_to = "value") %>% mutate(value = 100*value) %>%
    task_summaries(task_stages)
  stage_out <- tempfile(".task_sdt_build_", dirname(out)); dir.create(stage_out)
  stage_plots <- tempfile(".task_sdt_figures_", dirname(plots)); dir.create(stage_plots)
  on.exit(unlink(c(stage_out, stage_plots), recursive = TRUE), add = TRUE)
  exports <- list(cohort_membership = data$membership, participant_counts = counts, participant_sdt = scores,
    participant_performance = performance, descriptive_sdt = summaries, descriptive_performance = performance_summary,
    participant_changes = tests$changes, contrasts = tests$results, sensitivity_comparison = sensitivity,
    contrast_registry = task_specs(), input_checksums = provenance)
  for (name in names(exports)) write_csv(exports[[name]], file.path(stage_out, paste0(name, ".csv")))
  Sys.setenv(XDG_CACHE_HOME = file.path(tempdir(), "dynamic-task-sdt-fontconfig"))
  dir.create(Sys.getenv("XDG_CACHE_HOME"), recursive = TRUE, showWarnings = FALSE)
  samples <- c("primary", "all_participants")
  bundles <- setNames(lapply(samples, function(sample)
    task_figures(sample, summaries, performance_summary, tests$results, sensitivity,
      file.path(stage_plots, sample), return_bundle = TRUE)), samples)
  catalog <- bind_rows(lapply(bundles, `[[`, "catalog"))
  write_csv(catalog, file.path(stage_plots, "figure_catalog.csv"))
  task_write_reports(stage_out, stage_plots, catalog, data$membership, summaries, tests$results, sensitivity)
  writeLines(capture.output(sessionInfo()), file.path(stage_out, "session_info.txt"))
  writeLines("dynamic_reliability_task_sdt", file.path(stage_out, "GENERATOR.txt"))
  writeLines("dynamic_reliability_task_sdt", file.path(stage_plots, "GENERATOR.txt"))
  task_check(identical(vapply(provenance$path, task_sha, character(1), USE.NAMES = FALSE), provenance$sha256),
    "Inputs or source changed during generation")
  artifacts <- bind_rows(lapply(c("analysis", "plots"), function(kind) {
    folder <- if (kind == "analysis") stage_out else stage_plots
    paths <- list.files(folder, recursive = TRUE, full.names = TRUE)
    tibble(kind, path = substring(paths, nchar(folder)+2), bytes = file.info(paths)$size,
      sha256 = vapply(paths, task_sha, character(1), USE.NAMES = FALSE))
  }))
  write_csv(artifacts, file.path(stage_out, "artifact_manifest.csv"))
  receipt <- paste("Completed", format(Sys.time(), tz = "UTC"), "- exploratory stimulus-based SDT; primary and sensitivity outputs.")
  writeLines(receipt, file.path(stage_out, "COMPLETE.txt")); writeLines(receipt, file.path(stage_plots, "COMPLETE.txt"))
  task_publish(stage_out, out); task_publish(stage_plots, plots)
  message("Saved stimulus-based SDT to ", out, " and gallery to ", file.path(plots, "index.html"))
  invisible(list(scores = scores, performance = performance, summaries = summaries,
    tests = tests$results, sensitivity = sensitivity, membership = data$membership,
    catalog = catalog, figures = lapply(bundles, `[[`, "figures"), analysis_dir = out, plot_dir = plots))
}
main_task_sdt <- function(args = commandArgs(trailingOnly = TRUE)) {
  task_check(length(args) <= 4, "Usage: Rscript analyse_dynamic_reliability_task_sdt.R [trials_csv] [qc_dir] [analysis_dir] [plot_dir]")
  root <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
  if (!length(args)) {
    source(file.path(root, "plot_dynamic_reliability_hypotheses.R"), local = TRUE)
    return(plot_dynamic_reliability_hypotheses(file.path(root, "analysis_outputs/semester2_2026_behavioural"),
      file.path(root, "plots/semester2_2026_behavioural"), root))
  }
  defaults <- file.path(root, c("data/data_virus_all.csv", "analysis_outputs/semester2_2026_qc",
    "analysis_outputs/semester2_2026_behavioural/task_sdt", "plots/semester2_2026_behavioural/presentation/task_sdt"))
  defaults[seq_along(args)] <- args
  run_task_sdt(defaults[1], defaults[2], defaults[3], defaults[4], root)
}
if (sys.nframe() == 0L) main_task_sdt()
