# Matched aided-phase views. Dependencies: advice/task SDT component functions.
dynamic_joint_sdt <- function(advice, task, out) {
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  a <- bind_rows(lapply(names(advice), function(sample) mutate(advice[[sample]]$scores, sample = sample))) %>%
    transmute(sample, participant_id, group, phase, correction, n = n_answered_Correct+n_answered_Incorrect,
      bias = automation_bias, dprime, domain = "Advice")
  t <- task$scores %>% filter(agent == "participant", stage %in% c("P1", "P2", "P3")) %>%
    transmute(sample, participant_id, group, phase = stage, correction, n, bias = -criterion_c, dprime, domain = "Task")
  keys <- c("sample", "participant_id", "group", "phase", "correction")
  matched <- inner_join(a, t, by = keys, suffix = c("_advice", "_task"))
  stopifnot(!anyDuplicated(a[keys]), !anyDuplicated(t[keys]), nrow(matched) == nrow(a),
    nrow(matched) == nrow(t), all(matched$n_advice == matched$n_task))
  scores <- bind_rows(a, t) %>% pivot_longer(c(bias, dprime), names_to = "measure", values_to = "value")
  pieces <- list()
  for (pair in list(c("P2", "P1"), c("P3", "P2"), c("P3", "P1"))) {
    wide <- scores %>% select(-n) %>% pivot_wider(names_from = phase, values_from = value) %>%
      mutate(change = .data[[pair[1]]]- .data[[pair[2]]])
    pieces[[length(pieces)+1L]] <- wide %>% group_by(sample, domain, measure, correction) %>%
      group_modify(function(d, keys) bind_rows(lapply(task_groups, function(g) {
        x <- d$change[d$group == if (g == "CAL65") "CAL65" else "CAL90"]
        y <- if (g == "CAL90 - CAL65") d$change[d$group == "CAL65"] else NULL
        task_inference(x, y) %>% mutate(group = g, comparison = paste(pair, collapse = " - "))
      }))) %>% ungroup()
  }
  tests <- bind_rows(pieces) %>% group_by(sample, domain, measure, correction) %>%
    mutate(p_holm = p.adjust(p_raw, "holm", n = 9), family_n = 9L) %>% ungroup() %>%
    group_by(sample, correction) %>% mutate(p_holm_all_36 = p.adjust(p_raw, "holm", n = 36)) %>% ungroup()
  # Promotion and a common display must retain the component estimands and tests.
  advice_tests <- bind_rows(lapply(names(advice), function(sample) mutate(advice[[sample]]$contrasts, sample = sample))) %>%
    mutate(domain = "Advice", measure = ifelse(outcome == "automation_bias", "bias", "dprime"))
  task_tests <- task$tests %>% filter(family == "reliability") %>% mutate(domain = "Task",
    measure = ifelse(outcome == "criterion_c", "bias", "dprime"),
    old_lower = lower, lower = ifelse(measure == "bias", -upper, lower),
    upper = ifelse(measure == "bias", -old_lower, upper), estimate = ifelse(measure == "bias", -estimate, estimate))
  original <- bind_rows(advice_tests, task_tests)
  preserved <- inner_join(tests, original, by = c("sample", "domain", "measure", "correction", "comparison", "group"), suffix = c("", "_original"))
  stopifnot(nrow(preserved) == 144)
  for (column in c("estimate", "lower", "upper", "p_raw", "p_holm"))
    stopifnot(max(abs(preserved[[column]]-preserved[[paste0(column, "_original")]])) < 1e-10)
  means <- filter(scores, correction == "loglinear") %>% group_by(sample, domain, measure, group, phase) %>%
    summarise(n = n(), se = sd(value)/sqrt(n()), mean = mean(value), .groups = "drop") %>%
    mutate(lower = mean-qt(.975, n-1)*se, upper = mean+qt(.975, n-1)*se,
      interval = "Ordinary participant mean pointwise 95% t CI")
  sensitivity <- tests %>% select(sample, correction, domain, measure, comparison, group, estimate, p_holm) %>%
    mutate(variant = paste(sample, correction, sep = "_")) %>% select(-sample, -correction) %>%
    pivot_wider(names_from = variant, values_from = c(estimate, p_holm)) %>% mutate(
      sample_sensitive = sign(estimate_primary_loglinear) != sign(estimate_all_participants_loglinear) |
        ((p_holm_primary_loglinear < .05) != (p_holm_all_participants_loglinear < .05)),
      correction_sensitive_primary = sign(estimate_primary_loglinear) != sign(estimate_primary_extreme_only) |
        ((p_holm_primary_loglinear < .05) != (p_holm_primary_extreme_only < .05)),
      correction_sensitive_all = sign(estimate_all_participants_loglinear) != sign(estimate_all_participants_extreme_only) |
        ((p_holm_all_participants_loglinear < .05) != (p_holm_all_participants_extreme_only < .05)))
  notes <- c(
    "Matched advice/task SDT, with the QC-retained primary sample and all-participant sensitivity.",
    "Advice -c is acceptance tendency; advice d-prime discriminates agreement with correct versus incorrect advice.",
    "Task -c is BLACK response preference; task d-prime discriminates actual BLACK versus WHITE stimuli from final responses.",
    "For aligned bias panels only, task c is sign-reversed to -c; standalone task figures retain c (positive = WHITE). Tests and intervals transform exactly.",
    "Both domains use exactly the same answered aided trials. Timeouts are excluded; all finite/nonfinite RTs on answered trials remain eligible. See all-trial accuracy and timeout context.",
    "Phase contrasts: P2-P1 at the drop, P3-P2 recovery, P3-P1 residual difference. Each is tested within CAL65, within CAL90 and as CAL90-minus-CAL65 changes.",
    "Two-sided paired change / Welch tests with equal participant weights. Holm across nine tests per domain/measure/correction/sample. The original component tests are preserved exactly.",
    "An additional Holm adjustment across all 36 matched tests per sample/correction is exported. No test subtracts or compares an advice index with a task index.",
    "Loglinear rates (k+0.5)/(n+1) are primary; extreme-only correction is a sensitivity. Means and contrasts have ordinary pointwise 95% t CIs.",
    "The indices summarize the same responses and are not independent evidence. Task d-prime during aid reflects participant-plus-aid discrimination, not pure visual sensitivity.",
    "Fixed phase order confounds reliability with time/practice/fatigue; groups differ in calibrated visual difficulty. Nonsignificance does not establish equivalence or complete recovery.",
    "QC rules were chosen retrospectively and include post-exposure performance. Manual target deviation is diagnostic only. Sample-sensitive directions/tests are flagged [S]; correction-sensitive ones [C].",
    "Holm p-values can change when another test in the shared family changes, even if one group's members, estimate and raw p-value are unchanged.")
  for (name in c("scores", "means", "tests", "sensitivity")) {
    filename <- c(scores = "participant_scores", means = "descriptive_sdt", tests = "contrasts", sensitivity = "sensitivity_comparison")[[name]]
    write_csv(get(name), file.path(out, paste0(filename, ".csv")))
  }
  writeLines(notes, file.path(out, "methods.txt"))
  list(scores = scores, means = means, tests = tests, sensitivity = sensitivity, notes = notes)
}

dynamic_joint_figures <- function(joint, sample_name) {
  means <- filter(joint$means, sample == sample_name)
  tests <- filter(joint$tests, sample == sample_name, correction == "loglinear") %>% left_join(joint$sensitivity,
    by = c("domain", "measure", "comparison", "group")) %>% mutate(
      correction_sensitive = if (sample_name == "primary") correction_sensitive_primary else correction_sensitive_all,
      symbol = paste0(ifelse(inference_available, ifelse(p_holm < .05, "*", "ns"), "NA"),
        ifelse(sample_sensitive, " [S]", ""), ifelse(correction_sensitive, " [C]", "")))
  n <- means %>% filter(domain == "Advice", measure == "bias", phase == "P1")
  sample_label <- paste0(if (sample_name == "primary") "QC-retained primary sample" else "All-participant sensitivity",
    "; ", paste(paste0(n$group, " n = ", n$n), collapse = ", "))
  labels <- c(Advice.bias = "Advice acceptance (-c)", Task.bias = "BLACK response preference (-c)",
    Advice.dprime = "Advice discrimination (d-prime)", Task.dprime = "Task discrimination (d-prime)")
  joint_theme <- function() task_theme() + theme(plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 11), axis.text = element_text(size = 10), plot.title.position = "plot")
  limits <- setNames(lapply(c("bias", "dprime"), function(measure) {
    d <- filter(means, .data$measure == .env$measure); r <- range(c(0, d$lower, d$upper))
    r+c(-1, 1)*.1*diff(r)
  }), c("bias", "dprime"))
  panel <- function(domain, measure) {
    d <- filter(means, .data$domain == .env$domain, .data$measure == .env$measure) %>%
      mutate(x = match(phase, c("P1", "P2", "P3"))+ifelse(group == "CAL65", -.05, .05))
    ggplot(d, aes(x, mean, colour = group, group = group)) + geom_hline(yintercept = 0, colour = "#A8B1B8", linetype = "dotted") +
      geom_line(linewidth = .65) + geom_errorbar(aes(ymin = lower, ymax = upper), width = .07, linewidth = .7) +
      geom_point(size = 2.8) + scale_colour_manual(values = task_colours) +
      scale_x_continuous(breaks = 1:3, labels = c("Phase 1: 95%", "Phase 2: 70%", "Phase 3: 95%"), expand = expansion(add = .2)) +
      coord_cartesian(ylim = limits[[measure]]) + labs(x = NULL, y = if (measure == "bias") "Bias (-c)" else "Discrimination (d-prime)",
        title = labels[[paste(domain, measure, sep = ".")]]) + joint_theme()
  }
  overview <- ((panel("Advice", "bias") | panel("Task", "bias")) /
    (panel("Advice", "dprime") | panel("Task", "dprime"))) + plot_layout(guides = "collect") +
    task_annotation("Aid following and task performance together", sample_label,
      paste("Same answered aided trials; loglinear correction; equal participant means + pointwise 95% t CIs.",
        "\nAdvice indices concern following correct/incorrect advice; task indices concern final BLACK/WHITE decisions.",
        "\nAxes align within rows. The indices have different reference categories; this is not a test between domains.")) &
    theme(legend.position = "bottom")
  figures <- list(overview = list(plot = overview, data = means,
    annotations = tibble(note = "Descriptive matched means; no cross-domain test"),
    participants = filter(joint$scores, sample == sample_name, correction == "loglinear")))
  for (kind in c("drop", "recovery")) {
    comparisons <- if (kind == "drop") "P2 - P1" else c("P3 - P2", "P3 - P1")
    selected <- filter(tests, comparison %in% comparisons) %>%
      arrange(match(comparison, comparisons), match(group, task_groups))
    limits <- setNames(lapply(c("bias", "dprime"), function(measure) {
      d <- filter(selected, .data$measure == .env$measure); r <- range(c(0, d$lower, d$upper))
      r+c(-.08, .32)*diff(r)
    }), c("bias", "dprime"))
    forest <- function(domain, measure) {
      d <- filter(selected, .data$domain == .env$domain, .data$measure == .env$measure) %>% mutate(
        label = paste(gsub("P", "Phase ", comparison), group, sep = "\n"), label = factor(label, rev(unique(label))))
      annotation_x <- limits[[measure]][2]-.08*diff(limits[[measure]])
      ggplot(d, aes(estimate, label, colour = group)) + geom_vline(xintercept = 0, linetype = "dashed", colour = "#9BA5AD") +
        geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = .13, linewidth = .65) +
        geom_point(size = 2.5) + geom_text(aes(x = annotation_x, label = symbol), size = 3, show.legend = FALSE) +
        scale_colour_manual(values = task_colours, guide = "none") + coord_cartesian(xlim = limits[[measure]]) +
        labs(x = "Difference (SDT z units)", y = NULL, title = labels[[paste(domain, measure, sep = ".")]]) +
        joint_theme() + theme(axis.text.y = element_text(size = if (kind == "drop") 9 else 8),
          panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E4E9ED", linewidth = .3))
    }
    p <- ((forest("Advice", "bias") | forest("Task", "bias")) /
      (forest("Advice", "dprime") | forest("Task", "dprime"))) + task_annotation(
        if (kind == "drop") "Matched advice and task responses to the drop" else "Matched advice and task recovery", sample_label,
        paste("Paired / Welch changes; pointwise 95% CIs; * Holm p < .05; ns p >= .05. Nine H1/H3 tests per domain and measure.",
          "\n[S] sample-sensitive; [C] correction-sensitive direction/test decision. No inference compares advice and task indices.",
          "\nRetrospective analyses; fixed order includes time/practice/fatigue. Nonsignificance does not establish equivalence."))
    figures[[kind]] <- list(plot = p, data = selected, annotations = selected,
      participants = filter(joint$scores, sample == sample_name, correction == "loglinear"))
  }
  figures
}
