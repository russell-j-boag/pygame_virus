# Exploratory alternate H1/H3/H4 outcomes from validated, saved participant summaries.
# Rscript analyse_dynamic_reliability_sdt.R [analysis_dir] [plot_dir]
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2); library(patchwork)
})
sdt_check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
sdt_hashes <- function(paths) tibble(path = normalizePath(paths, mustWork = TRUE),
  md5 = unname(tools::md5sum(paths)))
sdt_verify <- function(path) {
  m <- read_csv(path, show_col_types = FALSE)
  sdt_check(all(c("path", "md5") %in% names(m)) && nrow(m) > 0 &&
    all(file.exists(m$path)) && identical(unname(tools::md5sum(m$path)), m$md5),
    paste("Stale or missing inputs/artifacts:", path))
}

sdt_counts <- function(agreement) {
  keys <- c("participant_id", "group", "phase", "advice")
  sdt_check(all(c(keys, "n", "agreement") %in% names(agreement)), "Missing agreement columns")
  sdt_check(!anyNA(agreement[c(keys, "n", "agreement")]) && !anyDuplicated(agreement[keys]),
    "Missing or duplicate agreement cells")
  sdt_check(all(agreement$group %in% c("CAL65", "CAL90")) &&
    all(agreement$phase %in% c("P1", "P2", "P3")) &&
    all(agreement$advice %in% c("Correct", "Incorrect")), "Unknown group, phase or advice")
  k <- agreement$n * agreement$agreement
  sdt_check(all(is.finite(k)) && all(agreement$n > 0 & agreement$n == floor(agreement$n)) &&
    all(agreement$agreement >= 0 & agreement$agreement <= 1) &&
    all(abs(k - round(k)) < 1e-8), "Invalid denominator or noninteger agreement count")
  agreement %>% transmute(across(all_of(keys)), n_answered = n, n_agree = round(k))
}

sdt_scores <- function(counts, correction = c("loglinear", "extreme_only")) {
  correction <- match.arg(correction)
  keys <- c("participant_id", "group", "phase")
  sdt_check(all(c(keys, "advice", "n_answered", "n_agree") %in% names(counts)) &&
    nrow(counts) > 0 && !anyNA(counts) && !anyDuplicated(counts[c(keys, "advice")]),
    "Missing or duplicate SDT counts")
  sdt_check(all(counts$advice %in% c("Correct", "Incorrect")) &&
    all(is.finite(counts$n_answered) & counts$n_answered > 0 & counts$n_answered == floor(counts$n_answered)) &&
    all(is.finite(counts$n_agree) & counts$n_agree >= 0 & counts$n_agree <= counts$n_answered &
          counts$n_agree == floor(counts$n_agree)), "Invalid SDT counts")
  d <- counts %>% select(all_of(keys), advice, n_answered, n_agree) %>%
    pivot_wider(names_from = advice, values_from = c(n_answered, n_agree))
  sdt_check(all(c("n_answered_Correct", "n_answered_Incorrect") %in% names(d)) && !anyNA(d),
    "Both nonempty advice cells are required; missing cells are not zero rates")
  rate <- function(k, n) if (correction == "loglinear") (k + .5)/(n + 1) else
    ifelse(k == 0, .5/n, ifelse(k == n, 1 - .5/n, k/n))
  d %>% mutate(hit_rate = rate(n_agree_Correct, n_answered_Correct),
    false_alarm_rate = rate(n_agree_Incorrect, n_answered_Incorrect),
    automation_bias = (qnorm(hit_rate) + qnorm(false_alarm_rate))/2,
    criterion_c = -automation_bias, dprime = qnorm(hit_rate) - qnorm(false_alarm_rate),
    correction = correction)
}

# One-sample inference on paired changes, or Welch inference on independent groups' changes.
sdt_difference <- function(a, b = NULL) {
  n1 <- length(a); n2 <- length(b)
  estimate <- mean(a) - if (is.null(b)) 0 else mean(b)
  va <- if (n1 > 1) var(a)/n1 else NA_real_
  vb <- if (is.null(b)) 0 else if (n2 > 1) var(b)/n2 else NA_real_
  se <- sqrt(va + vb)
  df <- if (is.null(b)) n1 - 1 else (va + vb)^2/(va^2/(n1-1) + vb^2/(n2-1))
  ok <- is.finite(estimate) && is.finite(se) && se > 0 && is.finite(df) && df > 0
  tibble(estimate, se, df, lower = if (ok) estimate-qt(.975, df)*se else NA_real_,
    upper = if (ok) estimate+qt(.975, df)*se else NA_real_,
    p_raw = if (ok) 2*pt(-abs(estimate/se), df) else NA_real_,
    inference_available = ok, n_participants = n1+n2)
}

sdt_contrasts <- function(scores) {
  out <- list(); k <- 0L
  for (correction_name in c("loglinear", "extreme_only")) {
    for (outcome_name in c("automation_bias", "dprime")) {
      wide <- scores %>% filter(correction == correction_name) %>%
        select(participant_id, group, phase, all_of(outcome_name)) %>%
        pivot_wider(names_from = phase, values_from = all_of(outcome_name))
      for (pair in list(c("P2", "P1"), c("P3", "P2"), c("P3", "P1"))) {
        changes <- wide[[pair[1]]] - wide[[pair[2]]]
        for (group_name in c("CAL65", "CAL90", "CAL90 - CAL65")) {
          a <- changes[wide$group == if (group_name == "CAL65") "CAL65" else "CAL90"]
          b <- if (group_name == "CAL90 - CAL65") changes[wide$group == "CAL65"] else NULL
          k <- k + 1L
          out[[k]] <- sdt_difference(a, b) %>% mutate(outcome = outcome_name,
            hypothesis = if (pair[1] == "P2") "H1" else "H3", correction = correction_name,
            group = group_name, comparison = paste(pair, collapse = " - "),
            contrast = paste0(group_name, ": ", comparison), units = "SDT z units",
            method = if (is.null(b)) "Paired participant change" else "Welch comparison of participant changes",
            adjustment_family = paste(outcome_name, correction_name, "nine H1/H3 contrasts"))
        }
      }
    }
  }
  bind_rows(out) %>% group_by(outcome, correction) %>%
    mutate(p_holm = p.adjust(p_raw, "holm", n = 9)) %>% ungroup() %>%
    group_by(correction) %>% mutate(p_holm_both_outcomes = p.adjust(p_raw, "holm", n = 18)) %>% ungroup()
}

sdt_subjective <- function(scores, ratings) {
  results <- list(); models <- list(); predictions <- list(); participants <- list(); diagnostics <- list()
  for (correction_name in c("loglinear", "extreme_only")) {
    d <- scores %>% filter(correction == correction_name, phase == "P2") %>%
      select(participant_id, group, automation_bias, correction) %>%
      left_join(select(ratings, participant_id, group, trust, perceived_aid_error), by = c("participant_id", "group")) %>%
      mutate(group = factor(group, c("CAL65", "CAL90")),
        centred_P2_bias = automation_bias-mean(automation_bias))
    sdt_check(!anyNA(d), "Incomplete H4 join")
    participants[[correction_name]] <- d
    for (outcome_name in c("trust", "perceived_aid_error")) {
      key <- paste(outcome_name, correction_name, sep = "_")
      fit <- lm(reformulate(c("group", "centred_P2_bias"), response = outcome_name), data = d)
      sdt_check(fit$rank == 3 && df.residual(fit) > 0, "Rank deficient H4 model")
      v <- sandwich::vcovHC(fit, type = "HC3")
      term <- "centred_P2_bias"; b <- coef(fit)[[term]]; se <- sqrt(v[term, term]); df <- df.residual(fit)
      ok <- is.finite(b) && is.finite(se) && se > 0
      results[[key]] <- tibble(hypothesis = "H4", outcome = outcome_name, correction = correction_name,
        contrast = "P2 -c: per one SDT unit, adjusted for calibration group", estimate = b, se, df,
        lower = if (ok) b-qt(.975, df)*se else NA_real_, upper = if (ok) b+qt(.975, df)*se else NA_real_,
        p_raw = if (ok) 2*pt(-abs(b/se), df) else NA_real_, inference_available = ok,
        n_participants = nrow(d), units = if (outcome_name == "trust") "Trust points per SDT unit" else
          "Rating percentage points per SDT unit", method = "Participant regression; HC3",
        adjustment_family = paste("H4", correction_name, "two association tests"))
      grid <- bind_rows(lapply(levels(d$group), function(g) tibble(group = g,
        automation_bias = seq(min(d$automation_bias[d$group == g]), max(d$automation_bias[d$group == g]), length.out = 80)))) %>%
        mutate(group = factor(group, levels(d$group)), centred_P2_bias = automation_bias-mean(d$automation_bias))
      x <- model.matrix(~ group + centred_P2_bias, grid)
      pred <- as.vector(x %*% coef(fit)); pred_se <- sqrt(rowSums((x %*% v)*x))
      predictions[[key]] <- grid %>% mutate(outcome = outcome_name, correction = correction_name,
        prediction = pred, lower = pred-qt(.975, df)*pred_se, upper = pred+qt(.975, df)*pred_se)
      diagnostics[[key]] <- tibble(outcome = outcome_name, correction = correction_name,
        n_participants = nobs(fit), rank = fit$rank, residual_df = df, max_leverage = max(hatvalues(fit)),
        max_cooks_distance = max(cooks.distance(fit)), inference_available = ok)
      models[[key]] <- list(fit = fit, vcov_hc3 = v, bias_centre = mean(d$automation_bias))
    }
  }
  list(results = bind_rows(results) %>% group_by(correction) %>%
    mutate(p_holm = p.adjust(p_raw, "holm", n = 2)) %>% ungroup(), models = models,
    predictions = bind_rows(predictions), participants = bind_rows(participants), diagnostics = bind_rows(diagnostics))
}

sdt_theme <- function() theme_minimal(base_size = 15, base_family = "Helvetica") + theme(
  text = element_text(colour = "#1C2D3E"), panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "#E4E9ED", linewidth = .35),
  axis.text = element_text(colour = "#1C2D3E", size = 12),
  plot.title = element_text(face = "bold", size = 16), plot.subtitle = element_text(size = 11),
  legend.position = "bottom", legend.title = element_blank(),
  plot.margin = margin(10, 10, 10, 10))
sdt_colours <- c(CAL65 = "#0072B2", CAL90 = "#D55E00", "CAL90 - CAL65" = "#334858")
# Display names only: phase codes and contrast lookup keys are unchanged.
sdt_phase_text <- function(x) gsub("\\bP([123])\\b", "Phase \\1", x, perl = TRUE)
sdt_symbol <- function(p) ifelse(is.na(p), "unavailable", ifelse(p < .05, "*", "ns"))
sdt_correction_flags <- function(primary, all_results) {
  other <- all_results %>% filter(correction == "extreme_only") %>%
    select(outcome, contrast, estimate_extreme_only = estimate, p_holm_extreme_only = p_holm)
  primary %>% left_join(other, by = c("outcome", "contrast")) %>%
    mutate(correction_sensitive = (sign(estimate) != sign(estimate_extreme_only)) |
      ((p_holm < .05) != (p_holm_extreme_only < .05)),
      symbol = paste0(sdt_symbol(p_holm), ifelse(!is.na(correction_sensitive) & correction_sensitive, "\u2020", "")))
}
sdt_correction_note <- function(a) {
  changed <- filter(a, correction_sensitive)
  if (!nrow(changed)) return("")
  paste0("\n\u2020 Correction-sensitive: ", paste(sprintf("%s, Holm p %.3f (loglinear) / %.3f (extreme-only)",
    changed$contrast, changed$p_holm, changed$p_holm_extreme_only), collapse = "; "), ".")
}
sdt_annotation <- function(title, subtitle, caption) plot_annotation(title = sdt_phase_text(title), subtitle = sdt_phase_text(subtitle),
  caption = sdt_phase_text(caption), theme = theme(text = element_text(family = "Helvetica", colour = "#1C2D3E"),
    plot.title = element_text(size = 23, face = "bold"), plot.subtitle = element_text(size = 13),
    plot.caption = element_text(size = 10.5, hjust = 0, lineheight = 1.12),
    plot.margin = margin(14, 16, 12, 16)))

sdt_phase_figure <- function(hypothesis, outcome_name, summaries, contrasts) {
  phases <- if (hypothesis == "H1") c("P1", "P2") else c("P1", "P2", "P3")
  bias <- outcome_name == "automation_bias"
  d <- filter(summaries, outcome == outcome_name, phase %in% phases) %>%
    mutate(x = match(phase, phases) + ifelse(group == "CAL65", -.06, .06))
  a <- filter(contrasts, .data$hypothesis == .env$hypothesis, outcome == outcome_name, correction == "loglinear") %>%
    sdt_correction_flags(contrasts) %>% mutate(label = paste0(group, "\n", sdt_phase_text(comparison)),
      label = factor(label, rev(unique(label))))
  left <- ggplot(d, aes(x, mean, colour = group, group = group)) +
    geom_line(linewidth = .7) + geom_errorbar(aes(ymin = lower, ymax = upper), width = .06, linewidth = .8) +
    geom_point(size = 3.3) + scale_colour_manual(values = sdt_colours) +
    scale_x_continuous(breaks = seq_along(phases), labels = c(P1 = "Phase 1\n95% aid", P2 = "Phase 2\n70% aid", P3 = "Phase 3\n95% aid")[phases],
      expand = expansion(add = .25)) + scale_y_continuous(expand = expansion(mult = c(.12, .14))) + sdt_theme() +
    labs(title = "Phase means", subtitle = "Equal participant weights; pointwise 95% CIs", x = NULL,
      y = if (bias) "Reliance bias (-c)" else "Discrimination (d-prime)")
  span <- diff(range(c(a$lower, a$upper, 0), na.rm = TRUE)); if (span == 0) span <- 1
  label_x <- max(c(a$upper, 0), na.rm = TRUE) + span*.22
  a$annotation_x <- label_x
  right <- ggplot(a, aes(estimate, label, colour = group)) +
    geom_vline(xintercept = 0, colour = "#8C969F", linetype = "dashed") +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = .14, linewidth = .8, na.rm = TRUE) +
    geom_point(size = 3, na.rm = TRUE) +
    geom_text(aes(x = annotation_x, label = symbol), size = 4, show.legend = FALSE) +
    scale_colour_manual(values = sdt_colours, guide = "none") + sdt_theme() +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E4E9ED", linewidth = .35),
      axis.text.y = element_text(size = 10.5), legend.position = "none") +
    labs(title = "Within-group and between-group changes", subtitle = "CAL90 - CAL65 compares paired changes", x = "Change (SDT z units)", y = NULL)
  title <- paste0(hypothesis, ": ", if (hypothesis == "H1") "Reliability drop and " else "Recovery of ",
    if (bias) "advice reliance" else "advice discrimination")
  subtitle <- if (hypothesis == "H1") {
    if (bias) "Does CAL90 reduce acceptance tendency more than CAL65 when reliability drops?" else
      "Does discrimination between correct and incorrect advice change differently between groups?"
  } else if (bias) "Does acceptance tendency increase from P2 to P3, and differ between P3 and P1?" else
    "Does discrimination change from P2 to P3, and differ between P3 and P1?"
  caption <- paste("Retrospective SDT analysis; answered trials only, with loglinear rate correction. Higher values indicate",
    if (bias) "greater acceptance tendency." else "greater discrimination.",
    "\nChanges: paired / Welch participant tests; * Holm p < .05; ns p >= .05 (nine H1/H3 tests per outcome). CIs are pointwise.",
    sdt_correction_note(a),
    "\nFixed phase order also includes time and experience effects; nonsignificance does not establish equivalence.")
  list(plot = (left + right + plot_layout(widths = c(1, 1.25))) + sdt_annotation(title, subtitle, caption),
    data = d, annotations = a, title = title)
}

sdt_h4_figure <- function(h4, original_subjective) {
  a <- filter(h4$results, correction == "loglinear") %>% sdt_correction_flags(h4$results) %>%
    mutate(panel_type = "Association")
  group_tests <- original_subjective %>% filter(contrast == "CAL90 - CAL65") %>%
    mutate(hypothesis = "H4", panel_type = "Group comparison", correction = "Not applicable",
      adjustment_family = "Original two-test family within rating outcome", correction_sensitive = FALSE,
      symbol = sdt_symbol(p_holm))
  sdt_check(nrow(group_tests) == 2 && setequal(group_tests$outcome, c("trust", "perceived_aid_error")),
    "Missing original H4 group comparisons")
  d <- filter(h4$participants, correction == "loglinear") %>%
    pivot_longer(c(trust, perceived_aid_error), names_to = "outcome", values_to = "rating") %>%
    mutate(panel_type = "Association")
  # Compute the SE before summarise replaces the individual rating column.
  means <- d %>% group_by(group, outcome) %>% summarise(n = n(), se = sd(rating)/sqrt(n),
    rating = mean(rating), .groups = "drop") %>%
    mutate(lower = rating-qt(.975, n-1)*se, upper = rating+qt(.975, n-1)*se, panel_type = "Group means")
  pred <- filter(h4$predictions, correction == "loglinear")
  group_panels <- lapply(c("trust", "perceived_aid_error"), function(outcome_name) {
    s <- filter(means, outcome == outcome_name) %>% mutate(x = match(group, c("CAL65", "CAL90")))
    ann <- filter(group_tests, outcome == outcome_name)
    span <- diff(range(c(s$lower, s$upper)))
    y <- max(c(s$upper, if (outcome_name == "perceived_aid_error") 0))+span*.18
    p <- ggplot(s, aes(x, rating, colour = group)) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = .08, linewidth = .8) + geom_point(size = 3) +
      annotate("segment", x = 1, xend = 2, y = y, yend = y, colour = "#58636C", linewidth = .55) +
      annotate("segment", x = c(1, 2), xend = c(1, 2), y = y-span*.035, yend = y, colour = "#58636C", linewidth = .55) +
      annotate("text", x = 1.5, y = y+span*.04, label = ann$symbol, size = 4, vjust = 0) +
      scale_x_continuous(breaks = 1:2, labels = c("CAL65", "CAL90"), limits = c(.6, 2.4)) +
      scale_y_continuous(expand = expansion(mult = c(.07, .13))) +
      scale_colour_manual(values = sdt_colours, guide = "none") + sdt_theme() +
      theme(plot.title = element_text(size = 15), axis.title = element_text(size = 12),
        axis.text = element_text(size = 11), legend.position = "none") +
      labs(title = if (outcome_name == "trust") "Final trust by group" else "Perceived aid-accuracy error by group",
        x = NULL, y = if (outcome_name == "trust") "Trust (1-5)" else "Error (pp)")
    if (outcome_name == "perceived_aid_error") p <- p + geom_hline(yintercept = 0, colour = "#87929D", linetype = "dashed")
    p
  })
  panels <- lapply(c("trust", "perceived_aid_error"), function(outcome_name) {
    ann <- filter(a, outcome == outcome_name)
    ggplot(filter(d, outcome == outcome_name), aes(automation_bias, rating, colour = group)) +
      geom_ribbon(data = filter(pred, outcome == outcome_name),
        aes(x = automation_bias, ymin = lower, ymax = upper, fill = group), inherit.aes = FALSE, alpha = .12) +
      geom_point(size = 2.2, alpha = .7) +
      geom_line(data = filter(pred, outcome == outcome_name), aes(y = prediction), linewidth = .8) +
      scale_colour_manual(values = sdt_colours) + scale_fill_manual(values = sdt_colours, guide = "none") +
      sdt_theme() + theme(plot.title = element_text(size = 15), plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 12), axis.text = element_text(size = 11)) +
      labs(title = if (outcome_name == "trust") "Trust and Phase 2 reliance" else "Aid-accuracy error and Phase 2 reliance",
        subtitle = sprintf("Slope %.2f [%.2f, %.2f]; Holm p = %s (%s)", ann$estimate, ann$lower, ann$upper,
          format.pval(ann$p_holm, digits = 2, eps = .001), ann$symbol),
        x = "Phase 2 reliance bias (-c)", y = if (outcome_name == "trust") "Trust (1-5)" else "Error (pp)")
  })
  title <- "H4: Final evaluations and Phase 2 reliance"
  caption <- paste("Top: group means + ordinary 95% t CIs; brackets reuse original HC3 Holm tests (* p < .05; ns p >= .05).",
    "\nBottom: group-adjusted HC3 slopes and pointwise 95% bands; Holm across two associations. Error = perceived minus realised accuracy.",
    sdt_correction_note(a),
    "\nExploratory associations with whole-block ratings; higher -c does not itself imply excessive reliance or causation.")
  list(plot = wrap_plots(c(group_panels, panels), ncol = 2, guides = "collect", heights = c(.85, 1.15)) +
      sdt_annotation(title, "Final ratings by calibration group, and their associations with Phase 2 acceptance tendency.", caption) &
      theme(legend.position = "bottom"), data = bind_rows(means, d), annotations = bind_rows(group_tests, a),
    predictions = pred, title = title)
}

run_dynamic_reliability_sdt <- function(base, plot_base, root, analysis_out = NULL) {
  sdt_check(file.exists(file.path(base, "COMPLETE.txt")), "Base analysis is incomplete")
  for (name in c("input_manifest", "source_checksums", "analysis_artifacts"))
    sdt_verify(file.path(base, paste0(name, ".csv")))
  read <- function(name) read_csv(file.path(base, paste0(name, ".csv")), show_col_types = FALSE)
  counts <- sdt_counts(read("participant_agreement")); ratings <- read("participant_ratings")
  original_subjective <- read("subjective_contrasts")
  cohort <- read("cohort_counts")
  person <- counts %>% group_by(participant_id) %>% summarise(n = n(), n_groups = n_distinct(group), .groups = "drop")
  sdt_check(all(person$n == 6 & person$n_groups == 1), "Every participant requires six phase/advice cells")
  actual <- counts %>% distinct(participant_id, group) %>% count(group)
  sdt_check(setequal(actual$group, c("CAL65", "CAL90")) && nrow(cohort) == 2 &&
    all(actual$n == cohort$n_participants[match(actual$group, cohort$group)]), "Cohort mismatch")
  sdt_check(!anyDuplicated(ratings$participant_id) && setequal(ratings$participant_id, person$participant_id), "Rating cohort mismatch")
  scores <- bind_rows(sdt_scores(counts), sdt_scores(counts, "extreme_only"))
  contrasts <- sdt_contrasts(scores); h4 <- sdt_subjective(scores, ratings)
  panel <- filter(scores, correction == "loglinear")
  summaries <- panel %>% pivot_longer(c(automation_bias, dprime), names_to = "outcome", values_to = "value") %>%
    group_by(group, phase, outcome) %>% summarise(n = n(), mean = mean(value), se = sd(value)/sqrt(n), .groups = "drop") %>%
    mutate(lower = mean-qt(.975, n-1)*se, upper = mean+qt(.975, n-1)*se,
      interval = "Ordinary participant mean pointwise 95% t CI")
  coverage <- panel %>% group_by(group, phase) %>% summarise(n = n(),
    correct_min = min(n_answered_Correct), incorrect_min = min(n_answered_Incorrect),
    incorrect_median = median(n_answered_Incorrect), incorrect_max = max(n_answered_Incorrect),
    extreme_correct = sum(n_agree_Correct == 0 | n_agree_Correct == n_answered_Correct),
    extreme_incorrect = sum(n_agree_Incorrect == 0 | n_agree_Incorrect == n_answered_Incorrect), .groups = "drop")
  robustness <- bind_rows(contrasts, h4$results) %>%
    select(hypothesis, outcome, contrast, correction, estimate, p_holm) %>%
    pivot_wider(names_from = correction, values_from = c(estimate, p_holm)) %>%
    mutate(same_direction = sign(estimate_loglinear) == sign(estimate_extreme_only),
      same_holm_decision = (p_holm_loglinear < .05) == (p_holm_extreme_only < .05))
  out <- if (is.null(analysis_out)) file.path(base, "sdt_alternates") else analysis_out
  plots <- file.path(plot_base, "presentation", "alternates")
  dir.create(file.path(out, "models"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(plots, "data"), recursive = TRUE, showWarnings = FALSE)
  unlink(c(file.path(out, "COMPLETE.txt"), file.path(plots, "COMPLETE.txt")))
  Sys.setenv(XDG_CACHE_HOME = file.path(tempdir(), "dynamic-sdt-fontconfig"))
  dir.create(Sys.getenv("XDG_CACHE_HOME"), recursive = TRUE, showWarnings = FALSE)
  write <- function(x, name) write_csv(x, file.path(out, paste0(name, ".csv")))
  write(counts, "participant_counts"); write(scores, "participant_sdt"); write(summaries, "descriptive_sdt")
  write(contrasts, "contrasts"); write(h4$results, "subjective_associations"); write(h4$participants, "subjective_participants")
  write(h4$predictions, "subjective_predictions"); write(h4$diagnostics, "subjective_diagnostics")
  write(coverage, "count_diagnostics"); write(robustness, "correction_sensitivity")
  for (name in names(h4$models)) saveRDS(h4$models[[name]], file.path(out, "models", paste0(name, ".rds")))
  specs <- tibble(hypothesis = c("H1", "H1", "H3", "H3", "H4"),
    outcome = c("automation_bias", "dprime", "automation_bias", "dprime", "automation_bias"),
    filename = c("01_main_H1_alternate_sdt_reliance", "01_main_H1_alternate_sdt_discrimination",
      "03_main_H3_alternate_sdt_reliance", "03_main_H3_alternate_sdt_discrimination",
      "04_main_H4_alternate_sdt_reliance_associations"))
  figures <- list(); catalog <- list()
  for (i in seq_len(nrow(specs))) {
    s <- specs[i, ]; f <- if (s$hypothesis == "H4") sdt_h4_figure(h4, original_subjective) else
      sdt_phase_figure(s$hypothesis, s$outcome, summaries, contrasts)
    for (ext in c("png", "pdf")) ggsave(file.path(plots, paste0(s$filename, ".", ext)), f$plot,
      width = 12, height = 6.75, dpi = 300, bg = "white", device = if (ext == "pdf") cairo_pdf else "png")
    for (name in c("data", "annotations")) write_csv(f[[name]], file.path(plots, "data", paste0(s$filename, "_", name, ".csv")))
    participants <- if (s$hypothesis == "H4") filter(h4$participants, correction == "loglinear") else
      panel %>% filter(s$hypothesis != "H1" | phase %in% c("P1", "P2")) %>%
        select(participant_id, group, phase, value = all_of(s$outcome))
    write_csv(participants, file.path(plots, "data", paste0(s$filename, "_participants.csv")))
    if (s$hypothesis == "H4") write_csv(f$predictions, file.path(plots, "data", paste0(s$filename, "_predictions.csv")))
    figures[[s$filename]] <- f$plot
    catalog[[i]] <- s %>% mutate(version = "Alternate", title = f$title,
      analysis_origin = "Retrospective exploratory outcome sensitivity")
  }
  catalog <- bind_rows(catalog); write_csv(catalog, file.path(plots, "figure_catalog.csv"))
  cairo_pdf(file.path(plots, "H1_H3_H4_alternate_sdt.pdf"), width = 12, height = 6.75, onefile = TRUE)
  tryCatch(invisible(lapply(figures, print)), finally = dev.off())
  notes <- c("Alternate H1/H3/H4 SDT sensitivity analyses, selected after inspection of original results.",
    "H = agreement with correct advice; F = agreement with incorrect advice. Bias = -c = (z(H)+z(F))/2; d-prime = z(H)-z(F).",
    "Answered trials regardless of RT. Timeouts are excluded, not treated as disagreements. Cohort matches the supplied behavioural analysis; see its membership and provenance.",
    "Loglinear correction: (agreements + 0.5)/(answered + 1), applied to both rates in every participant/phase.",
    "Extreme-only sensitivity: replace zero/one by 0.5/n or 1-0.5/n. Sparse-cell uncertainty remains under either correction.",
    "H1: P2-P1 within each group and CAL90-CAL65 difference in paired changes; a negative bias interaction matches the prediction.",
    "H3: P3-P2 and P3-P1 within groups and between-group differences in changes, for both -c and d-prime.",
    "Participant paired-change and Welch tests; equal weights, two-sided p-values, pointwise 95% confidence intervals.",
    "Holm: nine H1/H3 contrasts per outcome/correction; also export adjustment across both outcomes (18 tests).",
    "H4: each rating outcome ~ calibration group + grand-mean-centred P2 -c. Slopes per one SDT unit; HC3 covariance and residual-df t inference.",
    "H4 Holm family: two rating-association tests per correction. Top-row group plots reuse original saved HC3 tests and their outcome-wise Holm families; no new group tests.",
    "Perceived aid error retains perceived minus realised whole-block aid accuracy. Ratings are measured after the full aided block.",
    "Plots use loglinear correction. Means show ordinary pointwise participant t CIs; H4 lines/bands use the group-adjusted HC3 models.",
    "Both SDT indices derive from the same responses as accuracy/agreement; they are not independent evidence streams.",
    "Higher -c means greater acceptance tendency, not necessarily excessive reliance. Conventional equal-variance SDT is assumed.",
    "Behavioural advice discrimination also reflects participants' visual discrimination; CAL65 and CAL90 have different task difficulty.",
    "Neither index establishes deliberate verification or advice-caused switching. Phase changes also include time/practice/fatigue.",
    "Nonsignificant P3-P1 contrasts do not establish equivalence. Rating associations are not causal.",
    "Original models, tests, figures and their adjustment families are retained. These alternatives do not replace their conclusions.",
    "Reference: Hautus (1995), https://doi.org/10.3758/BF03203619. Coding and correction sensitivity match analyse_time_pressure_sdt.R.")
  table <- bind_rows(contrasts, h4$results) %>% filter(correction == "loglinear") %>%
    select(hypothesis, outcome, contrast, estimate, lower, upper, p_raw, p_holm)
  key_h1 <- filter(contrasts, correction == "loglinear", hypothesis == "H1", group == "CAL90 - CAL65")
  key_bias <- filter(key_h1, outcome == "automation_bias")
  interpretation <- c("Interpretation of the selected SDT outcomes:",
    if (key_bias$p_holm < .05 && key_bias$estimate < 0) "H1 reliance: supported; CAL90 reduced -c more than CAL65." else
      if (key_bias$p_holm < .05 && key_bias$estimate > 0) "H1 reliance: not supported; the group difference is opposite to the prediction (CAL90 reduced -c less than CAL65)." else
        "H1 reliance: not supported by the group difference in changes.",
    "H1 discrimination concerns a separate estimand; inspect its group contrast and the correction comparison below.",
    "H3 recovery is assessed separately by outcome, group and comparison. Nonsignificant differences do not establish a return to baseline.",
    "H4 positive slopes indicate higher final ratings with higher P2 -c, adjusting for group. Positive aid-error slopes mean more positive perceived-minus-realised errors, not necessarily better rating accuracy.")
  changed <- filter(robustness, !same_direction | !same_holm_decision)
  correction_note <- if (nrow(changed)) sprintf("Correction-sensitive result: %s %s, %s: estimate %.3f / %.3f; Holm p %.4f / %.4f (loglinear / extreme-only).",
    changed$hypothesis, changed$outcome, changed$contrast, changed$estimate_loglinear, changed$estimate_extreme_only,
    changed$p_holm_loglinear, changed$p_holm_extreme_only) else "All contrast directions and Holm decisions agree between corrections."
  report <- c(notes, "", interpretation, correction_note, "", "Loglinear results:", capture.output(print(as.data.frame(table), row.names = FALSE)),
    "", "Correction sensitivity:", capture.output(print(as.data.frame(robustness), row.names = FALSE)),
    "", "Count diagnostics:", capture.output(print(as.data.frame(coverage), row.names = FALSE)))
  writeLines(report, file.path(out, "report.txt"))
  escape <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  writeLines(c('<!doctype html><meta charset="utf-8"><title>SDT alternatives</title>',
    '<style>body{font:15px system-ui;margin:35px}pre{white-space:pre-wrap}</style><h1>Alternate H1/H3/H4 analyses</h1><pre>',
    escape(report), '</pre>'), file.path(out, "report.html"))
  cards <- vapply(seq_len(nrow(catalog)), function(i) paste0('<h2>', catalog$title[i], '</h2><img style="width:100%" src="',
    catalog$filename[i], '.png"><p><a href="', catalog$filename[i], '.pdf">PDF</a> | <a href="', catalog$filename[i],
    '.png">PNG</a> | <a href="data/', catalog$filename[i], '_data.csv">Plotted data</a> | <a href="data/',
    catalog$filename[i], '_annotations.csv">Tests</a></p>'), character(1))
  writeLines(c('<!doctype html><html><head><meta charset="utf-8"><title>Alternate H1/H3/H4: SDT</title></head>',
    '<body style="font:18px Helvetica,Arial;max-width:1200px;margin:32px auto"><h1>Alternate H1/H3/H4: SDT outcomes</h1>',
    '<p><a href="../index.html">Original hypothesis figures</a> | <a href="H1_H3_H4_alternate_sdt.pdf">All five alternates (PDF)</a></p>',
    '<p>Exploratory participant sensitivities. Higher -c indicates greater acceptance tendency; higher d-prime indicates greater behavioural discrimination.</p>',
    '<p>Holm: nine H1/H3 contrasts per outcome; two H4 associations. H4 top-row group plots retain the original saved HC3 Holm tests. Pointwise 95% intervals; answered trials only; loglinear rate correction.</p>',
    '<p>Fixed phase order includes time and experience effects. Nonsignificance does not establish equivalence; higher -c does not itself indicate overreliance.</p>',
    paste0('<p><strong>', escape(sdt_phase_text(correction_note)), '</strong></p>'),
    cards, '</body></html>'), file.path(plots, "index.html"))
  inputs <- c(file.path(base, paste0(c("participant_agreement", "participant_ratings", "subjective_contrasts", "cohort_counts", "input_manifest",
    "source_checksums", "analysis_artifacts"), ".csv")), file.path(base, "COMPLETE.txt"), file.path(root, "analyse_dynamic_reliability_sdt.R"))
  provenance <- sdt_hashes(inputs); write(provenance, "input_checksums")
  write_csv(provenance, file.path(plots, "data", "input_checksums.csv"))
  writeLines(capture.output(sessionInfo()), file.path(out, "session_info.txt"))
  artifacts <- list.files(plots, pattern = "\\.(png|pdf)$", full.names = TRUE)
  write_csv(tibble(file = basename(artifacts), bytes = file.info(artifacts)$size, md5 = unname(tools::md5sum(artifacts))),
    file.path(plots, "data", "artifact_manifest.csv"))
  all_artifacts <- c(list.files(out, recursive = TRUE, full.names = TRUE), list.files(plots, recursive = TRUE, full.names = TRUE))
  all_artifacts <- all_artifacts[!basename(all_artifacts) %in% c("COMPLETE.txt", "analysis_artifacts.csv")]
  write(sdt_hashes(all_artifacts), "analysis_artifacts")
  receipt <- paste("Completed", format(Sys.time(), tz = "UTC"), "- exploratory SDT alternatives; inspect diagnostics and correction sensitivity.")
  writeLines(receipt, file.path(out, "COMPLETE.txt")); writeLines(receipt, file.path(plots, "COMPLETE.txt"))
  message("Saved alternate H1/H3/H4 SDT analyses to ", out, " and five figure pairs to ", plots)
  invisible(list(scores = scores, contrasts = contrasts, subjective = h4,
    summaries = summaries, catalog = catalog, figures = figures, analysis_dir = out, plot_dir = plots))
}

main_sdt <- function(args = commandArgs(trailingOnly = TRUE)) {
  sdt_check(length(args) <= 2, "Usage: Rscript analyse_dynamic_reliability_sdt.R [analysis_dir] [plot_dir]")
  root <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
  base <- if (length(args)) args[1] else file.path(root, "analysis_outputs/semester2_2026_behavioural")
  plots <- if (length(args) >= 2) args[2] else file.path(root, "plots/semester2_2026_behavioural")
  source(file.path(root, "plot_dynamic_reliability_hypotheses.R"), local = TRUE)
  plot_dynamic_reliability_hypotheses(base, plots, root)
}
if (sys.nframe() == 0L) main_sdt()
