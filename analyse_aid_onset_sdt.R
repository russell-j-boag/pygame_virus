# Exploratory H6 alternates; original analyses and presentation are read-only inputs.
# Rscript analyse_aid_onset_sdt.R [analysis_dir] [plot_dir]
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2)
})
sdt_check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
sdt_hashes <- function(paths) tibble(path = normalizePath(unique(paths), mustWork = TRUE),
  md5 = unname(tools::md5sum(unique(paths))))
sdt_verify <- function(path) {
  m <- read_csv(path, show_col_types = FALSE)
  sdt_check(nrow(m) > 0 && all(c("path", "md5") %in% names(m)) &&
    all(file.exists(m$path)) && identical(unname(tools::md5sum(m$path)), m$md5),
    paste("Stale or missing inputs:", path))
  m$path
}
sdt_binary <- function(x) {
  v <- tolower(as.character(x))
  sdt_check(!anyNA(v) && all(v %in% c("true", "false", "0", "1")),
    "Expected complete binary data")
  as.integer(v %in% c("true", "1"))
}

sdt_scores <- function(counts, correction = c("loglinear", "extreme_only")) {
  correction <- match.arg(correction)
  keys <- c("participant_id", "condition")
  fields <- c(keys, "advice", "n_answered", "n_agree")
  sdt_check(all(fields %in% names(counts)) && nrow(counts) > 0,
    "Missing SDT count columns")
  sdt_check(!anyNA(counts[fields]) && !anyDuplicated(counts[c(keys, "advice")]) &&
    all(counts$advice %in% c("Correct", "Incorrect")), "Missing or duplicate advice cells")
  sdt_check(all(is.finite(counts$n_answered) & counts$n_answered > 0 &
    counts$n_answered == floor(counts$n_answered)) &&
    all(is.finite(counts$n_agree) & counts$n_agree >= 0 &
      counts$n_agree <= counts$n_answered & counts$n_agree == floor(counts$n_agree)),
    "Invalid agreement counts or empty denominator")
  d <- counts %>% select(all_of(fields)) %>%
    pivot_wider(names_from = advice, values_from = c(n_answered, n_agree))
  sdt_check(all(c("n_answered_Correct", "n_answered_Incorrect") %in% names(d)) && !anyNA(d),
    "Both nonempty advice cells are required; absent cells are not zero rates")
  rate <- function(k, n) if (correction == "loglinear") (k + .5)/(n + 1) else
    ifelse(k == 0, .5/n, ifelse(k == n, 1 - .5/n, k/n))
  d %>% mutate(hit_rate = rate(n_agree_Correct, n_answered_Correct),
    false_alarm_rate = rate(n_agree_Incorrect, n_answered_Incorrect),
    automation_bias = (qnorm(hit_rate) + qnorm(false_alarm_rate))/2,
    criterion_c = -automation_bias, dprime = qnorm(hit_rate) - qnorm(false_alarm_rate),
    correction = correction)
}

sdt_contrasts <- function(scores) {
  bind_rows(lapply(c("loglinear", "extreme_only"), function(method) {
    bind_rows(lapply(c("automation_bias", "dprime"), function(outcome) {
      d <- scores %>% filter(correction == method) %>%
        select(participant_id, condition, all_of(outcome)) %>%
        pivot_wider(names_from = condition, values_from = all_of(outcome))
      sdt_check(all(c("Aid first", "Stimulus first") %in% names(d)) && !anyNA(d),
        "Incomplete paired SDT scores")
      change <- d[["Stimulus first"]] - d[["Aid first"]]
      sdt_check(length(change) > 1 && is.finite(sd(change)) && sd(change) > 0,
        "Paired inference requires nonconstant participant differences")
      fit <- t.test(change)
      tibble(hypothesis = "H6", outcome = outcome, correction = method,
        contrast = "Stimulus first - Aid first", n_participants = nrow(d),
        mean_aid_first = mean(d[["Aid first"]]), mean_stimulus_first = mean(d[["Stimulus first"]]),
        estimate = unname(fit$estimate), se = sd(change)/sqrt(nrow(d)),
        lower = fit$conf.int[1], upper = fit$conf.int[2],
        statistic = unname(fit$statistic), df = unname(fit$parameter), p_raw = fit$p.value,
        units = "SDT z units", method = "Two-sided paired participant t test",
        interval = "Pointwise 95% CI for paired mean difference")
    }))
  })) %>% group_by(correction) %>% mutate(p_holm = p.adjust(p_raw, "holm"),
    adjustment_family = "Two H6 timing contrasts: acceptance bias and discrimination") %>% ungroup()
}

# Preserve raw means; normalize across timings separately for each SDT outcome.
sdt_morey <- function(scores) {
  d <- scores %>% filter(correction == "loglinear") %>%
    select(participant_id, condition, automation_bias, dprime) %>%
    pivot_longer(c(automation_bias, dprime), names_to = "outcome", values_to = "value")
  complete <- d %>% group_by(participant_id, outcome) %>% summarise(
    ok = n() == 2 && setequal(condition, c("Aid first", "Stimulus first")), .groups = "drop")
  sdt_check(all(complete$ok) && all(is.finite(d$value)), "Incomplete normalization pairs")
  d <- d %>% group_by(outcome) %>% mutate(grand_mean = mean(value)) %>%
    group_by(outcome, participant_id) %>% mutate(normalized = value - mean(value) + grand_mean) %>%
    ungroup() %>% mutate(normalization_k = 2L, morey_factor = sqrt(2))
  s <- d %>% group_by(outcome, condition) %>% summarise(n = n(), estimate = mean(value),
    se = sd(normalized)/sqrt(n()) * sqrt(2), .groups = "drop") %>%
    mutate(lower = estimate - qt(.975, n-1)*se, upper = estimate + qt(.975, n-1)*se,
      normalization_k = 2L, morey_factor = sqrt(2),
      interval = "Cousineau-Morey within-participant pointwise 95% CI")
  list(participants = d, summary = s)
}

run_aid_onset_sdt <- function(root, out, plots) {
  base <- file.path(root, "analysis_outputs/semester2_2026_presentation_followups")
  manifest_path <- file.path(base, "input_checksums.csv")
  consumed <- c(manifest_path, sdt_verify(manifest_path))
  raw_path <- file.path(root, "data/data_virus_all.csv")
  collation_path <- file.path(root, "data/collation_manifest.csv")
  cells_path <- file.path(base, "participant_cells.csv")
  raw <- read_csv(raw_path, show_col_types = FALSE)
  manifest <- read_csv(collation_path, show_col_types = FALSE)
  runs <- manifest %>% filter(export_type == "trials")
  sdt_check(nrow(runs) == 60 && !anyDuplicated(runs$participant_id) &&
    setequal(runs$participant_id, 1:60) && all(file.exists(runs$source_file)) &&
    identical(unname(tools::md5sum(runs$source_file)), runs$source_md5), "Invalid source run manifest")
  selected <- raw %>% distinct(participant_id, run_timestamp)
  provenance <- left_join(selected, select(runs, participant_id, run_timestamp),
    by = "participant_id", suffix = c("_data", "_manifest"))
  sdt_check(nrow(raw) == 46800L && nrow(selected) == 60 && !anyNA(provenance) &&
    all(provenance$run_timestamp_data == provenance$run_timestamp_manifest) &&
    identical(unique(raw$run_timestamp[raw$participant_id == 59]), "20260924_110831") &&
    setequal(raw$aid_condition, c("manual", "aid_first", "stimulus_first")) &&
    all((raw %>% count(participant_id, aid_condition))$n == 260L), "Unexpected cohort or run selection")
  aided <- raw %>% filter(aid_condition != "manual") %>% transmute(participant_id,
    condition = recode(aid_condition, aid_first = "Aid first", stimulus_first = "Stimulus first"),
    advice_correct = sdt_binary(aid_correct), agreement = sdt_binary(decision2_matches_aid),
    accuracy = sdt_binary(decision2_correct), response = decision2_response)
  sdt_check(!anyNA(aided$response) && all(aided$response %in% c("BLACK", "WHITE")) &&
    all(aided$agreement == as.integer(aided$accuracy == aided$advice_correct)),
    "Missing final response or invalid agreement-accuracy identity")
  counts <- aided %>% mutate(advice = if_else(advice_correct == 1, "Correct", "Incorrect")) %>%
    group_by(participant_id, condition, advice) %>% summarise(n_answered = n(),
      n_agree = sum(agreement), .groups = "drop")
  # Match the established H6 denominators and error counts without borrowing its tests.
  h6 <- read_csv(cells_path, show_col_types = FALSE) %>% filter(hypothesis == "H6") %>%
    transmute(participant_id = subject_no, condition = level,
      advice = if_else(panel == "Disagree with correct advice", "Correct", "Incorrect"),
      opportunities, successes)
  checked <- left_join(counts, h6, by = c("participant_id", "condition", "advice"))
  sdt_check(nrow(counts) == 240 && nrow(checked) == 240 && !anyNA(checked) &&
    all(checked$n_answered == checked$opportunities) && all(checked$successes ==
      ifelse(checked$advice == "Correct", checked$n_answered-checked$n_agree, checked$n_agree)),
    "SDT counts disagree with saved H6 cells")
  scores <- bind_rows(sdt_scores(counts), sdt_scores(counts, "extreme_only"))
  contrasts <- sdt_contrasts(scores)
  cm <- sdt_morey(scores)
  coverage <- counts %>% group_by(condition, advice) %>% summarise(n_participants = n(),
    trials_min = min(n_answered), trials_median = median(n_answered), trials_max = max(n_answered),
    zero_agreement_cells = sum(n_agree == 0), perfect_agreement_cells = sum(n_agree == n_answered),
    .groups = "drop")
  robustness <- contrasts %>% select(outcome, contrast, correction, estimate, p_holm) %>%
    pivot_wider(names_from = correction, values_from = c(estimate, p_holm)) %>%
    mutate(same_direction = sign(estimate_loglinear) == sign(estimate_extreme_only),
      same_holm_decision = (p_holm_loglinear < .05) == (p_holm_extreme_only < .05))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(plots, "data"), recursive = TRUE, showWarnings = FALSE)
  unlink(c(file.path(out, "COMPLETE.txt"), file.path(plots, "COMPLETE.txt")))
  tables <- list(participant_counts = counts, participant_sdt = scores, contrasts = contrasts,
    descriptive_sdt = cm$summary, count_diagnostics = coverage, correction_sensitivity = robustness)
  for (name in names(tables)) write_csv(tables[[name]], file.path(out, paste0(name, ".csv")))
  cache <- file.path(tempdir(), "aid-onset-sdt-fontcache")
  dir.create(cache, showWarnings = FALSE); Sys.setenv(XDG_CACHE_HOME = cache)
  colours <- c("Aid first" = "#1976A3", "Stimulus first" = "#C56924")
  catalog <- list(); figures <- list()
  for (outcome_name in c("automation_bias", "dprime")) {
    bias <- outcome_name == "automation_bias"
    stem <- paste0("06_main_H6_alternate_sdt_", if (bias) "reliance" else "discrimination")
    s <- cm$summary %>% filter(outcome == outcome_name) %>%
      mutate(x = match(condition, names(colours))) %>% arrange(x)
    a <- contrasts %>% filter(correction == "loglinear", outcome == outcome_name)
    span <- max(diff(range(s$lower, s$upper)), .1)
    a <- a %>% mutate(x1 = 1, x2 = 2, y = max(s$upper)+span*.28,
      tip = y-span*.045, text_y = y+span*.025, symbol = if_else(p_holm < .05, "*", "ns"))
    s <- s %>% mutate(label = sprintf("%.2f", estimate), label_y = lower-span*.12)
    limits <- c(min(s$label_y)-span*.18, max(a$text_y)+span*.24)
    p_label <- if (a$p_holm < .001) "< .001" else paste0("= ", formatC(a$p_holm, digits = 3, format = "f"))
    title <- if (bias) "H6: Reliance as SDT acceptance bias" else "H6: Selectivity as SDT discrimination"
    p <- ggplot(s, aes(x, estimate, colour = condition)) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = .11, linewidth = 1.1) +
      geom_point(size = 4.8) +
      geom_text(aes(y = label_y, label = label), vjust = 1, fontface = "bold", size = 5.1) +
      geom_segment(data = a, aes(x = x1, xend = x2, y = y, yend = y),
        inherit.aes = FALSE, colour = "#58636C", linewidth = .65) +
      geom_segment(data = a, aes(x = x1, xend = x1, y = tip, yend = y),
        inherit.aes = FALSE, colour = "#58636C", linewidth = .65) +
      geom_segment(data = a, aes(x = x2, xend = x2, y = tip, yend = y),
        inherit.aes = FALSE, colour = "#58636C", linewidth = .65) +
      geom_text(data = a, aes(x = (x1+x2)/2, y = text_y, label = symbol),
        inherit.aes = FALSE, colour = "#58636C", vjust = 0, size = 5.5) +
      scale_colour_manual(values = colours) +
      scale_x_continuous(breaks = c(1, 2), labels = c("Aid-first", "Stimulus-first"),
        expand = expansion(add = .45)) +
      scale_y_continuous(breaks = pretty(range(s$lower, s$upper), n = 5), expand = expansion(mult = 0)) +
      coord_cartesian(ylim = limits) + labs(x = NULL,
        y = if (bias) "Reliance bias (-c)" else "Discrimination (d-prime)", title = title,
        subtitle = sprintf("Stimulus-first - Aid-first: %.3f [95%% CI %.3f, %.3f]; Holm p %s.",
          a$estimate, a$lower, a$upper, p_label),
        caption = paste("Exploratory. N = 60. Means + Morey 95% CIs across the two timings; final decisions.",
          "Paired tests; Holm across both SDT outcomes. * p < .05; ns p >= .05. Loglinear correction.",
          if (bias) "Higher -c indicates greater acceptance tendency; it does not itself establish overreliance." else
            "Higher d-prime indicates greater separation of correct- and incorrect-advice agreement.",
          "Final agreement does not by itself establish active advice uptake.", sep = "\n")) +
      theme_minimal(base_size = 18, base_family = "Helvetica") + theme(
        text = element_text(colour = "#202A31"), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "#E2E6E8", linewidth = .45),
        axis.text = element_text(size = 17, colour = "#303B44"), axis.title = element_text(size = 18),
        axis.title.y = element_text(margin = margin(r = 9)),
        plot.title = element_text(face = "bold", size = 24, margin = margin(b = 6)),
        plot.subtitle = element_text(size = 14, margin = margin(b = 12)),
        plot.caption = element_text(size = 12, hjust = 0, lineheight = 1.12, margin = margin(t = 10)),
        plot.title.position = "plot", plot.caption.position = "plot", legend.position = "none",
        plot.margin = margin(12, 16, 12, 12), plot.background = element_rect(fill = "white", colour = NA))
    for (ext in c("png", "pdf")) ggsave(file.path(plots, paste0(stem, ".", ext)), p,
      width = 12, height = 6.75, units = "in", dpi = 300, bg = "white",
      device = if (ext == "pdf") cairo_pdf else "png")
    write_csv(s, file.path(plots, "data", paste0(stem, "_data.csv")))
    write_csv(a, file.path(plots, "data", paste0(stem, "_annotations.csv")))
    write_csv(filter(cm$participants, outcome == outcome_name),
      file.path(plots, "data", paste0(stem, "_participants.csv")))
    catalog[[outcome_name]] <- tibble(hypothesis = "H6", version = "Alternate", outcome = outcome_name,
      filename = stem, title = title, analysis_origin = "Exploratory sensitivity specified after inspecting results",
      width_in = 12, height_in = 6.75, png_width = 3600L, png_height = 2025L,
      normalization_k = 2L, y_min = limits[1], y_max = limits[2])
    figures[[stem]] <- p
  }
  catalog <- bind_rows(catalog)
  write_csv(catalog, file.path(plots, "figure_catalog.csv"))
  cairo_pdf(file.path(plots, "H6_alternate_sdt.pdf"), width = 12, height = 6.75, onefile = TRUE)
  tryCatch(invisible(lapply(figures, print)), finally = dev.off())
  notes <- c("EXPLORATORY H6 ALTERNATES: SDT ACCEPTANCE BIAS AND DISCRIMINATION", "",
    "Current 60-participant cohort; p59 uses replacement run 20260924_110831.",
    "H = final agreement given correct advice; F = final agreement given incorrect advice.",
    "-c = (z(H)+z(F))/2; d-prime = z(H)-z(F). Compute per participant and aided condition.",
    "All answered aided trials, without initial-disagreement or RT filtering. No Manual SDT score.",
    "The current cohort has no missing final responses. Fail on missing/invalid responses or empty advice cells.",
    "Main correction: (agreements+0.5)/(answered+1) for every rate.",
    "Sensitivity: only zero/one rates replaced by 0.5/n or 1-0.5/n; sparse-cell uncertainty remains.",
    "Equal-weight participant means. Two-sided paired Stimulus-first minus Aid-first t tests.",
    "Holm covers the two outcomes separately within each correction. Original H6 p-values are not reused.",
    "Contrast CIs are pointwise paired 95% t intervals; plotted CIs use Cousineau-Morey across two timings.",
    "Plot intervals describe within-participant patterns; their overlap is not a significance test.",
    "Both outcomes retain their exploratory origin and conventional equal-variance SDT interpretation.",
    "Positive -c does not establish inappropriate overreliance; final agreement does not identify active uptake.",
    "d-prime can reflect perceptual task competence and does not establish deliberate advice verification.",
    "These decompose existing H6 information; they are not independent replication or replacements for H7.",
    "Hautus (1995): https://doi.org/10.3758/BF03203619", "Morey (2008): https://doi.org/10.20982/tqmp.04.2.p061")
  writeLines(c(notes, "", capture.output(print(contrasts, n = Inf, width = Inf))), file.path(out, "report.txt"))
  cards <- vapply(seq_len(nrow(catalog)), function(i) paste0('<h2>', catalog$title[i],
    '</h2><img style="width:100%" src="', catalog$filename[i], '.png"><p><a href="',
    catalog$filename[i], '.png">PNG</a> | <a href="', catalog$filename[i],
    '.pdf">PDF</a> | <a href="data/', catalog$filename[i], '_data.csv">Plotted data</a> | <a href="data/',
    catalog$filename[i], '_annotations.csv">Tests</a></p>'), character(1))
  writeLines(c('<!doctype html><html><head><meta charset="utf-8"><title>H6 alternate SDT figures</title></head>',
    '<body style="font:18px Helvetica,Arial;max-width:1200px;margin:32px auto"><h1>H6 alternate SDT figures</h1>',
    '<p><a href="../index.html">Original hypothesis figures</a> | <a href="H6_alternate_sdt.pdf">Both alternates (PDF)</a></p>',
    '<p>Exploratory final-decision sensitivities. Original figures and analyses are retained. Each participant has equal weight.</p>',
    '<p>Means use within-participant Morey intervals. Brackets use fresh paired tests with Holm adjustment across the two SDT outcomes.</p>',
    '<p>Final agreement does not establish active uptake, and positive acceptance bias does not establish inappropriate overreliance.</p>',
    cards, '</body></html>'), file.path(plots, "index.html"))
  consumed <- unique(c(consumed, raw_path, collation_path, cells_path, runs$source_file,
    file.path(root, "analyse_aid_onset_sdt.R")))
  provenance <- sdt_hashes(consumed)
  write_csv(provenance, file.path(out, "input_checksums.csv"))
  write_csv(provenance, file.path(plots, "data/input_checksums.csv"))
  writeLines(capture.output(sessionInfo()), file.path(out, "session_info.txt"))
  artifacts <- c(list.files(out, full.names = TRUE), list.files(plots, recursive = TRUE, full.names = TRUE))
  artifacts <- artifacts[!basename(artifacts) %in% c("COMPLETE.txt", "artifact_manifest.csv")]
  write_csv(sdt_hashes(artifacts), file.path(out, "artifact_manifest.csv"))
  receipt <- paste("Completed", format(Sys.time(), tz = "UTC"), "UTC")
  writeLines(receipt, file.path(out, "COMPLETE.txt")); writeLines(receipt, file.path(plots, "COMPLETE.txt"))
  message("Saved H6 alternate -c and d-prime figures to ", plots)
  invisible(list(scores = scores, contrasts = contrasts, catalog = catalog))
}

if (sys.nframe() == 0L) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1]])
  root <- dirname(normalizePath(script)); args <- commandArgs(trailingOnly = TRUE)
  sdt_check(length(args) <= 2, "Usage: Rscript analyse_aid_onset_sdt.R [analysis_dir] [plot_dir]")
  run_aid_onset_sdt(root,
    if (length(args)) args[1] else file.path(root, "analysis_outputs/semester2_2026_presentation_followups/sdt_alternates"),
    if (length(args) >= 2) args[2] else file.path(root, "plots/semester2_2026_key_findings/presentation/alternates"))
}
