# Alternate H5/H9 outcomes from the validated, saved answered-trial counts.
# Rscript analyse_time_pressure_sdt.R [analysis_dir] [plot_dir]
sdt_scores <- function(counts, correction = c("loglinear", "extreme_only")) {
  correction <- match.arg(correction)
  keys <- c("participant_id", "pattern", "pressure", "reliability")
  stopifnot(!anyDuplicated(counts[c(keys, "advice")]),
    all(counts$n_answered > 0), all(counts$n_agree >= 0),
    all(counts$n_agree <= counts$n_answered),
    all(counts$n_answered == floor(counts$n_answered)),
    all(counts$n_agree == floor(counts$n_agree)))
  d <- counts %>% select(all_of(keys), advice, n_answered, n_agree) %>%
    pivot_wider(names_from = advice, values_from = c(n_answered, n_agree))
  stopifnot(!anyNA(d), all(c("n_answered_Correct", "n_answered_Incorrect") %in% names(d)))
  rate <- function(k, n) if (correction == "loglinear") (k + .5)/(n + 1) else
    ifelse(k == 0, .5/n, ifelse(k == n, 1 - .5/n, k/n))
  d %>% mutate(hit_rate = rate(n_agree_Correct, n_answered_Correct),
    false_alarm_rate = rate(n_agree_Incorrect, n_answered_Incorrect),
    automation_bias = (qnorm(hit_rate) + qnorm(false_alarm_rate))/2,
    dprime = qnorm(hit_rate) - qnorm(false_alarm_rate), correction = correction)
}

run_time_pressure_sdt <- function(base, plot_base, root) {
  cache <- file.path(tempdir(), "sdt-fontconfig-cache")
  dir.create(cache, showWarnings = FALSE)
  Sys.setenv(XDG_CACHE_HOME = cache)
  # Reuse the existing allocation-aware participant contrast calculation.
  source(file.path(root, "analyse_time_pressure.R"), local = TRUE)
  out <- file.path(base, "sdt_alternates")
  plots <- file.path(plot_base, "presentation", "alternates")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(plots, "data"), recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(out, "COMPLETE.txt"))
  stopifnot(file.exists(file.path(base, "COMPLETE.txt")))
  manifest <- read_csv(file.path(base, "input_manifest.csv"), show_col_types = FALSE)
  stopifnot(all(file.exists(manifest$path)),
    identical(unname(tools::md5sum(manifest$path)), manifest$md5))
  counts <- read_csv(file.path(base, "participant_reliance.csv"), show_col_types = FALSE)
  cohort <- read_csv(file.path(base, "cohort_counts.csv"), show_col_types = FALSE)
  scores <- bind_rows(sdt_scores(counts), sdt_scores(counts, "extreme_only"))
  panel <- filter(scores, correction == "loglinear")
  stopifnot(all(panel$pattern %in% patterns), all(panel$pressure %in% c("HP", "LP")),
    all(panel$reliability == ifelse((panel$pattern == "HP95_LP65" & panel$pressure == "HP") |
      (panel$pattern == "HP65_LP95" & panel$pressure == "LP"), "95%", "65%")))
  completeness <- panel %>% group_by(participant_id, pattern) %>% summarise(n = n(),
    complete = setequal(pressure, c("HP", "LP")), .groups = "drop")
  stopifnot(all(completeness$n == 2), all(completeness$complete),
    !anyDuplicated(completeness$participant_id),
    nrow(completeness) == sum(cohort$n_participants))
  actual <- completeness %>% count(pattern)
  stopifnot(all(actual$n == cohort$n_participants[match(actual$pattern, cohort$pattern)]))
  grid <- expand_grid(pressure = c("HP", "LP"), reliability = c("65%", "95%"))
  methods <- aided_contrasts(grid)
  names(methods) <- sub("^Trust ", "", names(methods))
  results <- bind_rows(lapply(c("loglinear", "extreme_only"), function(correction_name) {
    bind_rows(lapply(c("automation_bias", "dprime"), function(outcome_name) {
      participant_sensitivity(filter(scores, correction == correction_name), outcome_name,
        grid, methods, outcome_name) %>% mutate(correction = correction_name,
          hypothesis = ifelse(outcome_name == "automation_bias", "H5 alternate", "H9 alternate"),
          units = "SDT z units", analysis = "Participant contrast; Welch-Satterthwaite",
          adjustment_family = paste(hypothesis, correction_name, "five contrasts"))
    }))
  })) %>% group_by(correction) %>% mutate(p_holm_both_outcomes = p.adjust(p_raw, "holm")) %>% ungroup()
  raw_long <- panel %>% pivot_longer(c(automation_bias, dprime), names_to = "outcome", values_to = "value")
  summaries <- raw_long %>% group_by(pattern, pressure, reliability, outcome) %>%
    summarise(n = n(), se = sd(value)/sqrt(n), value = mean(value), .groups = "drop") %>%
    mutate(lower = value - qt(.975, n-1)*se, upper = value + qt(.975, n-1)*se,
      interval = "Ordinary participant mean pointwise 95% t CI")
  coverage <- panel %>% group_by(pattern, pressure, reliability) %>% summarise(
    n = n(), incorrect_min = min(n_answered_Incorrect),
    incorrect_median = median(n_answered_Incorrect), incorrect_max = max(n_answered_Incorrect),
    extreme_correct = sum(n_agree_Correct == 0 | n_agree_Correct == n_answered_Correct),
    extreme_incorrect = sum(n_agree_Incorrect == 0 | n_agree_Incorrect == n_answered_Incorrect), .groups = "drop")
  robustness <- results %>% select(outcome, contrast, correction, estimate, p_holm) %>%
    pivot_wider(names_from = correction, values_from = c(estimate, p_holm)) %>%
    mutate(same_direction = sign(estimate_loglinear) == sign(estimate_extreme_only),
      same_holm_decision = (p_holm_loglinear < .05) == (p_holm_extreme_only < .05))
  write_csv(scores, file.path(out, "participant_sdt.csv"))
  write_csv(summaries, file.path(out, "descriptive_sdt.csv"))
  write_csv(results, file.path(out, "contrasts.csv"))
  write_csv(coverage, file.path(out, "count_diagnostics.csv"))
  write_csv(robustness, file.path(out, "correction_sensitivity.csv"))
  weights <- bind_rows(lapply(names(methods), function(nm) mutate(grid, contrast = nm, weight = methods[[nm]])))
  write_csv(weights, file.path(out, "contrast_weights.csv"))
  notes <- c(
    "Alternate H5/H9 outcomes, specified after inspection of the existing results; exploratory sensitivity analyses.",
    "H5 alternate: acceptance tendency, automation_bias = -c = (z(H)+z(F))/2.",
    "H9 alternate: discrimination, dprime = z(H)-z(F), complementary to the original correct-minus-incorrect agreement.",
    "H = agreement given correct advice; F = agreement given incorrect advice. Answered trials only, regardless of RT.",
    "Primary correction: (agreements+0.5)/(answered+1), applied to both rates in every block before computing participant scores.",
    "Correction sensitivity: replace only zero/one rates by 0.5/n or 1-0.5/n. This does not eliminate sparse-cell uncertainty.",
    "Participant scores receive equal weight. Plots show raw participant means and ordinary pointwise 95% t intervals.",
    "Five contrasts per outcome: reliability within each pressure, pressure within each reliability, and their interaction.",
    "Reliability at fixed pressure and pressure at fixed reliability compare independent allocation groups.",
    "The interaction is a difference in participant sums between groups: HP95+LP65 versus HP65+LP95. Pairing is retained.",
    "Welch-Satterthwaite inference permits unequal group variances and arbitrary within-participant covariance.",
    "Holm adjustment is separate across the five contrasts per outcome/correction; an additional ten-contrast adjustment is exported.",
    "No original model p-values are reused. Original hypotheses, outputs and adjustment families are unchanged.",
    "Interpret dprime and -c under conventional equal-variance SDT; these indices do not establish verification or advice-caused changes.",
    "Neither timeout behaviour nor causal effects of advice on an initial unaided decision are identified by these conditional scores.",
    "References: Hautus (1995), https://doi.org/10.3758/BF03203619; coding matches auto_reliability_virus_atc/R/paper_original_hypotheses.R.")
  writeLines(c(notes, "", capture.output(print(filter(results, correction == "loglinear"), n = Inf, width = Inf))),
    file.path(out, "report.txt"))
  colours <- c("65%" = "#C56924", "95%" = "#1976A3")
  catalog <- list(); figures <- list()
  for (outcome_name in c("automation_bias", "dprime")) {
    is_bias <- outcome_name == "automation_bias"
    hypothesis <- if (is_bias) "H5" else "H9"
    stem <- if (is_bias) "05_main_H5_alternate_sdt_reliance" else "09_main_H9_alternate_sdt_discrimination"
    d <- filter(summaries, outcome == outcome_name) %>% mutate(
      x = ifelse(pressure == "HP", 1, 2) + ifelse(reliability == "65%", -.09, .09))
    a <- filter(results, correction == "loglinear", outcome == outcome_name)
    span <- max(d$upper) - min(d$lower)
    a <- a %>% mutate(x1 = c(.91, 1.91, .91, 1.09, 1),
      x2 = c(1.09, 2.09, 1.91, 2.09, 2),
      y = max(d$upper) + span*c(.17, .17, .40, .63, .86),
      tip = y - span*.035, text_y = y + span*.02,
      colour = c("#58636C", "#58636C", colours, "#58636C"),
      symbol = ifelse(p_holm < .05, "*", "ns"),
      label = paste0(c("", "", "65% HP vs LP: ", "95% HP vs LP: ", "Reliability x pressure: "), symbol))
    title <- if (is_bias) "H5: Reliance as SDT acceptance bias" else "H9: Selectivity as SDT discrimination"
    p <- ggplot(d, aes(x, value, colour = reliability)) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = .06, linewidth = 1.05) +
      geom_point(size = 4.2) + scale_colour_manual(values = colours, labels = c("65% aid", "95% aid")) +
      scale_x_continuous(breaks = c(1,2), labels = c("High pressure (1 s)", "Low pressure (3 s)"), limits = c(.6,2.4)) +
      scale_y_continuous(breaks = pretty(range(d$lower, d$upper), n = 5)) +
      coord_cartesian(ylim = c(min(d$lower)-span*.12, max(a$y)+span*.16)) +
      labs(title = title,
        subtitle = if (is_bias) "Higher -c indicates greater acceptance of advice, combining correct and incorrect advice trials." else
          "Higher d-prime indicates greater separation between agreement with correct and incorrect advice.",
        x = NULL, y = if (is_bias) "Reliance bias (-c)" else "Discrimination (d-prime)", colour = NULL,
        caption = paste("Exploratory. Observed participant means + pointwise 95% t CIs; answered trials only.",
          "Participant contrasts: * Holm p < .05; ns p >= .05 (five tests per outcome). Loglinear rate correction.", sep = "\n")) +
      theme_minimal(base_size = 18, base_family = "Helvetica") + theme(
        text = element_text(colour = "#202A31"), axis.text = element_text(size = 17),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E2E6E8", linewidth = .45),
        plot.title = element_text(size = 24, face = "bold", margin = margin(b = 8)),
        plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
        plot.caption = element_text(size = 12, hjust = 0, margin = margin(t = 8)),
        plot.title.position = "plot", plot.caption.position = "plot", legend.position = "bottom",
        plot.margin = margin(12, 16, 12, 12)) +
      geom_segment(data = a, aes(x = x1, xend = x2, y = y, yend = y), inherit.aes = FALSE, colour = a$colour) +
      geom_segment(data = a, aes(x = x1, xend = x1, y = tip, yend = y), inherit.aes = FALSE, colour = a$colour) +
      geom_segment(data = a, aes(x = x2, xend = x2, y = tip, yend = y), inherit.aes = FALSE, colour = a$colour) +
      geom_text(data = a, aes(x = (x1+x2)/2, y = text_y, label = label), inherit.aes = FALSE,
        colour = a$colour, vjust = 0, size = 4.8)
    for (ext in c("png", "pdf")) ggsave(file.path(plots, paste0(stem, ".", ext)), p,
      width = 12, height = 6.75, dpi = 300, bg = "white", device = if (ext == "pdf") cairo_pdf else "png")
    write_csv(d, file.path(plots, "data", paste0(stem, "_data.csv")))
    write_csv(a, file.path(plots, "data", paste0(stem, "_annotations.csv")))
    write_csv(filter(raw_long, outcome == outcome_name), file.path(plots, "data", paste0(stem, "_participants.csv")))
    catalog[[hypothesis]] <- tibble(hypothesis, version = "Alternate", outcome = outcome_name,
      filename = stem, title, analysis_origin = "Exploratory outcome choice after inspection of results")
    figures[[stem]] <- p
  }
  catalog <- bind_rows(catalog)
  write_csv(catalog, file.path(plots, "figure_catalog.csv"))
  cairo_pdf(file.path(plots, "H5_H9_alternate_sdt.pdf"), width = 12, height = 6.75, onefile = TRUE)
  tryCatch(invisible(lapply(figures, print)), finally = dev.off())
  cards <- vapply(seq_len(nrow(catalog)), function(i) paste0('<h2>', catalog$title[i], '</h2><img style="width:100%" src="',
    catalog$filename[i], '.png"><p><a href="', catalog$filename[i], '.png">PNG</a> | <a href="', catalog$filename[i],
    '.pdf">PDF</a> | <a href="data/', catalog$filename[i], '_data.csv">Plotted data</a> | <a href="data/',
    catalog$filename[i], '_annotations.csv">Tests</a></p>'), character(1))
  writeLines(c('<!doctype html><html><head><meta charset="utf-8"><title>Alternate H5 and H9</title></head>',
    '<body style="font:18px Helvetica,Arial;max-width:1200px;margin:32px auto"><h1>Alternate H5 and H9: SDT outcomes</h1>',
    '<p><a href="../index.html">Original hypothesis figures</a> | <a href="H5_H9_alternate_sdt.pdf">Both alternates (PDF)</a></p>',
    '<p>Exploratory alternate outcomes. Original plots and tests are retained. Means are equally weighted across participants.</p>',
    '<p>Short brackets compare reliability within pressure; coloured brackets compare pressure within reliability; the top bracket tests their interaction. Simple comparisons are between allocation groups; the interaction retains participant pairing.</p>',
    '<p>Holm adjustment covers five participant contrasts per outcome. Intervals are pointwise. Timeouts are excluded; positive reliance bias does not itself imply overreliance.</p>',
    cards, '</body></html>'), file.path(plots, "index.html"))
  inputs <- c(file.path(base, c("participant_reliance.csv", "cohort_counts.csv", "input_manifest.csv", "COMPLETE.txt")),
    file.path(root, c("analyse_time_pressure_sdt.R", "analyse_time_pressure.R")))
  provenance <- tibble(path = normalizePath(inputs), md5 = unname(tools::md5sum(inputs)))
  write_csv(provenance, file.path(out, "input_checksums.csv"))
  write_csv(provenance, file.path(plots, "data", "input_checksums.csv"))
  artifacts <- list.files(plots, pattern = "\\.(png|pdf)$", full.names = TRUE)
  write_csv(tibble(file = basename(artifacts), bytes = file.info(artifacts)$size,
    md5 = unname(tools::md5sum(artifacts))), file.path(plots, "data", "artifact_manifest.csv"))
  writeLines(capture.output(sessionInfo()), file.path(out, "session_info.txt"))
  writeLines(paste("Completed", format(Sys.time(), tz = "UTC")), file.path(out, "COMPLETE.txt"))
  message("Saved alternate H5 (-c) and H9 (d-prime) analyses and PNG/PDF plots to ", plots)
  invisible(list(scores = scores, contrasts = results, catalog = catalog))
}

if (sys.nframe() == 0) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
  root <- dirname(normalizePath(script)); args <- commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) <= 2)
  run_time_pressure_sdt(if (length(args)) args[1] else file.path(root, "analysis_outputs/semester2_2026_behavioural"),
    if (length(args) >= 2) args[2] else file.path(root, "plots/semester2_2026_behavioural"), root)
}
