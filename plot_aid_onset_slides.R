# One presentation figure per revised H1-H8, using saved inference.
# First run Rscript analyse_aid_onset_followups.R for exploratory H5-H7.
# Usage: Rscript plot_aid_onset_slides.R [output_dir]
# Style: auto_reliability_virus_atc/R/paper_hypotheses_plots.R and
# pygame_virus_time_pressure/plot_time_pressure_slides.R.
# No models are refitted and no new hypothesis tests are introduced.
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tidyr)
})
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
ROOT <- dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 1L)
OUT <- if (length(args)) args[[1]] else file.path(ROOT, "plots/semester2_2026_key_findings/presentation")
RESULTS <- file.path(ROOT, "analysis_outputs/semester2_2026_hypotheses")
FOLLOWUPS <- file.path(ROOT, "analysis_outputs/semester2_2026_presentation_followups")
cache <- file.path(tempdir(), "fontconfig-cache")
dir.create(cache, showWarnings = FALSE)
Sys.setenv(XDG_CACHE_HOME = cache)

consumed <- character()
read_input <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  consumed <<- unique(c(consumed, path))
  read_csv(path, show_col_types = FALSE)
}
recorded <- read_input(file.path(RESULTS, "analysis_input_checksums.csv"))
# Check all recorded data AND analysis-code inputs before borrowing its tests.
for (i in seq_len(nrow(recorded))) {
  path <- file.path(ROOT, if (grepl("/data/", recorded$path[i])) "data" else "", basename(recorded$path[i]))
  stopifnot(file.exists(path), unname(tools::md5sum(path)) == recorded$md5[i])
  consumed <- unique(c(consumed, normalizePath(path)))
}
participants <- read_input(file.path(ROOT, "data/semester2_2026_averaged_within_participants.csv"))
raw <- read_input(file.path(ROOT, "data/data_virus_all.csv"))
primary <- read_input(file.path(RESULTS, "primary_hypothesis_tests.csv"))
sensitivity <- read_input(file.path(RESULTS, "participant_level_sensitivity_tests.csv"))
diagnostics <- read_input(file.path(RESULTS, "model_diagnostics.csv"))
followup_manifest <- read_input(file.path(FOLLOWUPS, "input_checksums.csv"))
stopifnot(all(file.exists(followup_manifest$path)),
  identical(unname(tools::md5sum(followup_manifest$path)), followup_manifest$md5))
consumed <- unique(c(consumed, followup_manifest$path))
followup_cells <- read_input(file.path(FOLLOWUPS, "participant_cells.csv"))
followup_tests <- read_input(file.path(FOLLOWUPS, "contrasts.csv"))
followup_interaction <- read_input(file.path(FOLLOWUPS, "h6_interaction.csv"))
followup_diagnostics <- read_input(file.path(FOLLOWUPS, "model_diagnostics.csv"))
stopifnot(nrow(followup_diagnostics) == 3, all(followup_diagnostics$converged),
  !any(followup_diagnostics$singular), nrow(followup_cells) == 720, nrow(followup_tests) == 9)
stopifnot(nrow(participants) == 60L, !anyDuplicated(participants$subject_no),
  setequal(participants$subject_no, 1:60), nrow(raw) == 46800L,
  setequal(raw$participant_id, 1:60),
  identical(unique(raw$run_timestamp[raw$participant_id == 59]), "20260924_110831"),
  nrow(primary) == 14L, !anyDuplicated(primary[c("hypothesis", "contrast")]),
  nrow(sensitivity) == 12L, !anyDuplicated(sensitivity[c("hypothesis", "contrast")]),
  setequal(diagnostics$hypothesis, paste0("H", 2:5)),
  all(diagnostics$converged), !any(diagnostics$singular))
binary <- function(x) {
  x <- tolower(as.character(x))
  stopifnot(all(is.na(x) | x %in% c("true", "false", "1", "0")))
  ifelse(is.na(x), NA_integer_, as.integer(x %in% c("true", "1")))
}
trials <- raw %>% transmute(subject_no = participant_id, condition = aid_condition,
  accuracy = binary(decision2_correct), switched = binary(changed_response),
  advice_correct = binary(aid_correct))
stopifnot(!anyNA(trials$accuracy), !anyNA(trials$switched),
  all((trials %>% count(subject_no, condition))$n == 260L))

# Independently reconstruct every plotted accuracy/revision cell from trials.
overall <- trials %>% group_by(subject_no, condition) %>%
  summarise(accuracy = mean(accuracy), switched = mean(switched), .groups = "drop")
advice <- trials %>% filter(condition != "manual") %>%
  group_by(subject_no, condition, advice_correct) %>%
  summarise(value = mean(accuracy), .groups = "drop") %>%
  mutate(variable = paste0("decision2_accuracy_proportion_", condition,
    if_else(advice_correct == 1, "_aid_correct", "_aid_incorrect")))
reconstructed <- bind_rows(
  overall %>% transmute(subject_no, variable = paste0("switch_proportion_", condition), value = switched),
  overall %>% transmute(subject_no, variable = paste0("decision2_accuracy_proportion_", condition,
    if_else(condition == "manual", "", "_overall")), value = accuracy),
  advice %>% select(subject_no, variable, value))
provided <- participants %>% pivot_longer(-subject_no, names_to = "variable", values_to = "value")
checked <- left_join(reconstructed, provided, by = c("subject_no", "variable"), suffix = c("_raw", "_saved"))
stopifnot(nrow(checked) == 600L, !anyNA(checked),
  max(abs(checked$value_raw - checked$value_saved)) < 1e-12,
  max(abs(participants$aid_reliability_rating_mean_pct -
    (participants$aid_first_reliability_rating_pct + participants$stimulus_first_reliability_rating_pct)/2)) < 1e-12)

# Cousineau normalization followed by Morey's sqrt(k/(k-1)) SE correction.
# Morey (2008), https://doi.org/10.20982/tqmp.04.2.p061
# The observed means are unchanged; these are pointwise pattern intervals,
# not model intervals or confidence intervals for a difference between means.
morey <- function(variables, multiplier, normalization_set, input = provided) {
  k <- length(variables)
  d <- input %>% filter(variable %in% variables) %>% mutate(value = value * multiplier)
  complete <- d %>% group_by(subject_no) %>%
    summarise(ok = n() == k && setequal(variable, variables), .groups = "drop")
  stopifnot(k > 1, nrow(complete) == 60L, all(complete$ok),
    all(is.finite(d$value)), !anyDuplicated(d[c("subject_no", "variable")]))
  grand_mean <- mean(d$value)
  d <- d %>% group_by(subject_no) %>% mutate(normalized = value - mean(value) + grand_mean) %>%
    ungroup() %>% mutate(normalization_set, normalization_levels = paste(variables, collapse = ";"))
  s <- d %>% group_by(variable) %>% summarise(n = n(), estimate = mean(value),
    se = sd(normalized)/sqrt(n()) * sqrt(k/(k-1)), .groups = "drop") %>%
    mutate(k = k, morey_factor = sqrt(k/(k-1)), margin = qt(.975, n-1)*se,
      lower = estimate-margin, upper = estimate+margin,
      interval = "Cousineau-Morey within-participant 95% CI", normalization_set,
      normalization_levels = paste(variables, collapse = ";"))
  list(summary = s, participants = d)
}
manual <- "decision2_accuracy_proportion_manual"
correct <- paste0("decision2_accuracy_proportion_", c("aid_first", "stimulus_first"), "_aid_correct")
incorrect <- paste0("decision2_accuracy_proportion_", c("aid_first", "stimulus_first"), "_aid_incorrect")
# Shared five-cell normalization preserves identical Manual means AND CIs in H3/H4.
advice_summary <- morey(c(manual, correct, incorrect), 100, "advice_accuracy_five_cells")
sets <- list(
  H1 = morey(c("manual_reliability_rating_pct", "aid_reliability_rating_mean_pct"), 1, "perceived_reliability_pair"),
  H2 = morey(paste0("switch_proportion_", c("manual", "aid_first", "stimulus_first")), 100, "revision_three_conditions"),
  H3 = advice_summary,
  H4 = advice_summary,
  H8 = morey(paste0("trust_mean_", c("aid_first", "stimulus_first"), "_1to5"), 1, "trust_pair"))

slide_theme <- function() theme_minimal(base_size = 18, base_family = "Helvetica") + theme(
  text = element_text(colour = "#202A31"),
  panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(colour = "#E2E6E8", linewidth = .45),
  axis.text = element_text(size = 17, colour = "#303B44"),
  axis.title = element_text(size = 18), axis.title.y = element_text(margin = margin(r = 9)),
  plot.title = element_text(face = "bold", size = 24, lineheight = 1.02, margin = margin(b = 6)),
  plot.subtitle = element_text(size = 14, lineheight = 1.12, margin = margin(b = 12)),
  plot.caption = element_text(size = 12.5, hjust = 0, lineheight = 1.12, margin = margin(t = 10)),
  plot.title.position = "plot", plot.caption.position = "plot",
  legend.position = "none", plot.margin = margin(12, 16, 12, 12),
  plot.background = element_rect(fill = "white", colour = NA))
colours <- c("Manual" = "#727B88", "Aid first" = "#1976A3", "Stimulus first" = "#C56924",
  "Self" = "#727B88", "Aid" = "#1976A3", "Correct advice" = "#087F72", "Incorrect advice" = "#BB503B")
config <- list(
  H1 = list(stem = "01_h1_perceived_reliability", variables = c("manual_reliability_rating_pct", "aid_reliability_rating_mean_pct"),
    levels = c("Self", "Aid"), labels = c("Self\n(Manual)", "Aid\n(mean of both timings)"),
    title = "H1: The aid was rated as more reliable than the self",
    subtitle = sprintf("Perceived aid reliability exceeded perceived self-reliability by %.1f percentage points.",
      primary$estimate[primary$hypothesis == "H1"]),
    question = "Was the aid perceived as more reliable than the self?",
    ylab = "Perceived reliability (%)", limits = c(55, 84), breaks = seq(55, 80, 5),
    note = "Aid ratings are averaged within each participant before comparison with Manual self-ratings."),
  H2 = list(stem = "02_h2_decision_revisions", variables = paste0("switch_proportion_", c("manual", "aid_first", "stimulus_first")),
    title = "H2: Revisions were most frequent in stimulus-first",
    subtitle = "Aid-first produced fewer revisions than Manual, contrary to the prediction.",
    question = "Does stimulus-first produce the most decision revisions?",
    ylab = "Trials with a decision revision (%)", limits = c(0, 23), breaks = seq(0, 20, 5),
    note = "Advice precedes Decision 1 in Aid-first; revision rates start from different information states."),
  H3 = list(stem = "03_h3_correct_advice_accuracy", variables = c(manual, correct),
    title = "H3: Correct advice improved accuracy at both timings",
    subtitle = "Both aided conditions exceeded Manual; no timing difference was detected.",
    question = "Does stimulus-first give the greatest benefit from correct advice?",
    ylab = "Final-decision accuracy (%)", limits = c(55, 100), breaks = seq(55, 100, 5),
    note = "Aided points use correct-advice trials. Manual uses all trials; the same baseline appears in H4."),
  H4 = list(stem = "04_h4_incorrect_advice_accuracy", variables = c(manual, incorrect),
    title = "H4: Incorrect advice was most costly in stimulus-first",
    subtitle = "Both aided conditions fell below Manual; the timing difference opposes the prediction.",
    question = "Does stimulus-first better resist incorrect advice?",
    ylab = "Final-decision accuracy (%)", limits = c(55, 100), breaks = seq(55, 100, 5),
    note = "Aided points use incorrect-advice trials. Manual uses all trials; the same baseline appears in H3."),
  H8 = list(stem = "08_h8_trust", variables = paste0("trust_mean_", c("aid_first", "stimulus_first"), "_1to5"),
    levels = c("Aid first", "Stimulus first"), labels = c("Aid-first", "Stimulus-first"),
    title = "H8: Trust was lower in stimulus-first",
    subtitle = sprintf("The six-item mean trust rating was %.2f points lower in Stimulus-first.",
      -primary$estimate[primary$hypothesis == "Exploratory"]),
    question = "Does trust differ with advice timing?",
    ylab = "Trust (1-5)", limits = c(2.5, 3.65), breaks = seq(2.5, 3.5, .25),
    note = "Six-item mean trust; promoted from the exploratory comparison. No Manual trust score is defined."))

# Resolve bracket endpoints from named contrasts, never by result row order.
annotations_for <- function(h, spec) {
  source_h <- if (h == "H8") "Exploratory" else h
  a <- primary %>% filter(hypothesis == source_h) %>%
    mutate(source_hypothesis = hypothesis, hypothesis = h)
  is_model <- h %in% paste0("H", 2:4)
  if (is_model) {
    stopifnot(nrow(a) == 3L, all(a$analysis == "Trial-level logistic mixed model"),
      max(abs(a$p_value_adjusted - p.adjust(a$p_value_raw, "holm"))) < 1e-12)
    parts <- strsplit(a$contrast, " / ", fixed = TRUE)
    a$first_level <- vapply(parts, `[`, character(1), 1)
    a$second_level <- vapply(parts, `[`, character(1), 2)
    other <- sensitivity %>% filter(hypothesis == h)
    index <- match(gsub(" / ", " - ", a$contrast, fixed = TRUE), other$contrast)
    stopifnot(!anyNA(index))
    a$participant_p_adjusted <- other$p_value_adjusted[index]
    a$participant_disagreement <- (a$p_value_adjusted < .05) != (a$participant_p_adjusted < .05)
  } else {
    stopifnot(nrow(a) == 1L, a$analysis == "Paired t-test")
    a$first_level <- spec$levels[match(a$first_variable, spec$variables)]
    a$second_level <- spec$levels[match(a$second_variable, spec$variables)]
    a$participant_p_adjusted <- a$p_value_adjusted
    a$participant_disagreement <- FALSE
  }
  first_x <- match(a$first_level, spec$levels)
  second_x <- match(a$second_level, spec$levels)
  a <- a %>% mutate(x1 = pmin(first_x, second_x), x2 = pmax(first_x, second_x),
    symbol = paste0(if_else(p_value_adjusted < .05, "*", "ns"), if_else(participant_disagreement, "\u2020", "")))
  stopifnot(!anyNA(a$x1), !anyNA(a$x2), all(a$x1 < a$x2), all(a$n_participants == 60))
  a
}

dir.create(file.path(OUT, "data"), recursive = TRUE, showWarnings = FALSE)
figures <- list(); catalog <- list(); all_annotations <- list(); all_summaries <- list()
for (h in names(config)) {
  spec <- config[[h]]
  if (is.null(spec$levels)) {
    spec$levels <- c("Manual", "Aid first", "Stimulus first")
    spec$labels <- c("Manual", "Aid-first", "Stimulus-first")
  }
  s <- sets[[h]]$summary %>% filter(variable %in% spec$variables) %>%
    mutate(x = match(variable, spec$variables), condition = spec$levels[x]) %>% arrange(x)
  a <- annotations_for(h, spec)
  span <- diff(spec$limits)
  # Put the two adjacent comparisons on the same lane, with a small gap at x=2.
  # H3/H4 share axes and bracket heights for direct comparison across slides.
  top <- if (h %in% c("H3", "H4")) max(advice_summary$summary$upper) else max(s$upper)
  a <- a %>% mutate(lane = if_else(x2-x1 == 2, 2, 1),
    y = top + span*(.08 + .075*(lane-1)), tip = y-span*.018, text_y = y+span*.008,
    bracket_x1 = x1 + if_else(x1 == 2 & length(spec$levels) == 3, .025, 0),
    bracket_x2 = x2 - if_else(x2 == 2 & length(spec$levels) == 3, .025, 0))
  s <- s %>% mutate(label = if (h == "H8") sprintf("%.2f", estimate) else sprintf("%.1f%%", estimate),
    label_y = lower - span*.035)
  stopifnot(all(s$lower > min(spec$limits)), all(s$upper < max(spec$limits)),
    all(s$label_y > min(spec$limits)), max(a$text_y) + span*.05 < max(spec$limits))
  interval_note <- if (h %in% c("H3", "H4"))
    "N = 60. Observed means + Morey 95% CIs; shared normalization across five accuracy cells."
    else "N = 60. Observed means + Cousineau-Morey within-participant 95% CIs."
  test_note <- if (h %in% paste0("H", 2:4))
    paste0("Brackets: logistic GLMM, Holm within ", h, "; * p < .05; ns p >= .05.")
    else "Bracket: two-sided paired t test; * p < .05 (unadjusted single comparison)."
  if (any(a$participant_disagreement)) test_note <- paste0(test_note, " \u2020 paired test differs.")
  p <- ggplot(s, aes(x, estimate, colour = condition)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .11, linewidth = 1.1) +
    geom_point(size = 4.8) +
    geom_text(aes(y = label_y, label = label), vjust = 1, fontface = "bold", size = 5.1) +
    geom_segment(data = a, aes(x = bracket_x1, xend = bracket_x2, y = y, yend = y),
      inherit.aes = FALSE, colour = "#58636C", linewidth = .65) +
    geom_segment(data = a, aes(x = bracket_x1, xend = bracket_x1, y = tip, yend = y),
      inherit.aes = FALSE, colour = "#58636C", linewidth = .65) +
    geom_segment(data = a, aes(x = bracket_x2, xend = bracket_x2, y = tip, yend = y),
      inherit.aes = FALSE, colour = "#58636C", linewidth = .65) +
    geom_text(data = a, aes(x = (x1+x2)/2, y = text_y, label = symbol),
      inherit.aes = FALSE, colour = "#58636C", vjust = 0, size = 5.5) +
    scale_colour_manual(values = colours) +
    scale_x_continuous(breaks = seq_along(spec$levels), labels = spec$labels, expand = expansion(add = .45)) +
    scale_y_continuous(breaks = spec$breaks, expand = expansion(mult = 0)) +
    coord_cartesian(ylim = spec$limits) +
    labs(x = NULL, y = spec$ylab, title = spec$title, subtitle = spec$subtitle,
      caption = paste(interval_note, test_note, spec$note, sep = "\n")) + slide_theme()
  figures[[spec$stem]] <- p
  for (ext in c("png", "pdf")) ggsave(file.path(OUT, paste0(spec$stem, ".", ext)), p,
    width = 12, height = 6.75, units = "in", dpi = 300, bg = "white",
    device = if (ext == "pdf") cairo_pdf else "png")
  write_csv(s, file.path(OUT, "data", paste0(spec$stem, "_data.csv")))
  # All normalization cells are exported, including the other advice subset in H3/H4.
  write_csv(sets[[h]]$participants %>% mutate(displayed = variable %in% spec$variables),
    file.path(OUT, "data", paste0(spec$stem, "_participants.csv")))
  write_csv(a, file.path(OUT, "data", paste0(spec$stem, "_annotations.csv")))
  all_summaries[[h]] <- s %>% mutate(hypothesis = h, filename = spec$stem)
  all_annotations[[h]] <- a %>% mutate(filename = spec$stem)
  catalog[[h]] <- tibble(order = length(catalog)+1L, hypothesis = h,
    role = if (h == "H8") "Main presentation; exploratory origin" else "Main hypothesis",
    analysis_origin = if (h == "H8") "Exploratory timing comparison promoted to primary presentation H8" else "Original hypothesis",
    source_hypothesis = if (h == "H8") "Exploratory" else h,
    filename = spec$stem, question = spec$question, key_message = spec$title,
    width_in = 12, height_in = 6.75, png_width = 3600L, png_height = 2025L,
    normalization_set = s$normalization_set[1], normalization_k = s$k[1],
    y_min = spec$limits[1], y_max = spec$limits[2], notes = spec$note)
}

# H5-H7 are now main presentation figures, retaining their exploratory origin.
followup_config <- list(
  H5 = list(stem = "05_h5_revision_quality", panels = c("Beneficial revisions", "Harmful revisions"),
    facet_labels = c("Beneficial revisions\nWrong to correct", "Harmful revisions\nCorrect to wrong"),
    levels = c("Manual", "Aid first", "Stimulus first"), labels = c("Manual", "Aid-first", "Stimulus-first"),
    title = "H5: Stimulus-first increased both types of revision",
    subtitle = "Relative to Aid-first, more initial errors were corrected and more correct decisions were overturned.",
    question = "Does timing affect beneficial and harmful revision rates?",
    ylab = "Revisions (% of all trials)", limits = c(0, 18), breaks = seq(0, 15, 5),
    interval_note = "N = 60. Means + Morey 95% CIs; normalized across three conditions within each revision type.",
    test_note = "GLMMs; Holm across six contrasts. * p < .05; ns p >= .05; \u2020 paired test differs.",
    note = "Exploratory. Advice precedes Decision 1 in Aid-first; starting information differs across timings."),
  H6 = list(stem = "06_h6_advice_use_errors", panels = c("Disagree with correct advice", "Agree with incorrect advice"),
    facet_labels = c("Correct advice\nFinal disagreement", "Incorrect advice\nFinal agreement"),
    levels = c("Aid first", "Stimulus first"), labels = c("Aid-first", "Stimulus-first"),
    title = "H6: Stimulus-first increased incorrect-advice agreement",
    subtitle = sprintf("Timing x advice correctness interaction: p %s (unadjusted exploratory test).",
      if (followup_interaction$p_value < .001) "< .001" else paste0("= ", formatC(followup_interaction$p_value, digits = 3, format = "f"))),
    question = "Does timing affect rejection of correct advice and acceptance of incorrect advice?",
    ylab = "Final advice-use errors (%)", limits = c(0, 50), breaks = seq(0, 50, 10),
    interval_note = "N = 60. Means + Morey 95% CIs; two timings normalized separately within each advice type.",
    test_note = "Brackets: saved H3/H4 GLMM contrasts, original Holm families. * p < .05; ns p >= .05.",
    note = "Exploratory presentation of H3/H4 errors. Final agreement does not by itself establish active advice uptake."),
  H7 = list(stem = "07_h7_selective_uptake_stimulus_first", panels = "Stimulus-first", facet_labels = "Stimulus-first",
    levels = c("Correct advice", "Incorrect advice"), labels = c("Correct advice", "Incorrect advice"),
    title = "H7: Participants favoured correct advice when revising",
    subtitle = "Stimulus-first: switching toward the aid when the independent initial response disagreed with it.",
    question = "When initial judgments disagree with advice, is correct advice taken up more often?",
    ylab = "Uptake (% of initial disagreements)", limits = c(0, 75), breaks = seq(0, 70, 10),
    interval_note = "N = 60. Means + Morey 95% CIs across the two advice types within participants.",
    test_note = "Bracket: logistic GLMM, single exploratory contrast. * p < .05; ns p >= .05.",
    note = "Correct-advice opportunities start with a wrong judgment; incorrect-advice opportunities start with a correct one."))
for (h in names(followup_config)) {
  spec <- followup_config[[h]]
  d <- followup_cells %>% filter(hypothesis == h)
  summaries <- list(); normalized <- list()
  for (panel_name in spec$panels) {
    z <- d %>% filter(panel == panel_name) %>% arrange(match(level, spec$levels))
    variables <- unique(z$variable)
    result <- morey(variables, 1, paste(h, panel_name, sep = ": "), input = z)
    summaries[[panel_name]] <- result$summary %>% mutate(panel = panel_name,
      x = match(variable, variables), condition = spec$levels[x])
    normalized[[panel_name]] <- result$participants %>% mutate(displayed = TRUE)
  }
  s <- bind_rows(summaries) %>% mutate(panel = factor(panel, spec$panels))
  a <- followup_tests %>% filter(hypothesis == h) %>% mutate(panel = factor(panel, spec$panels),
    x1 = pmin(match(first_level, spec$levels), match(second_level, spec$levels)),
    x2 = pmax(match(first_level, spec$levels), match(second_level, spec$levels)),
    symbol = paste0(if_else(p_value_adjusted < .05, "*", "ns"), if_else(participant_disagreement, "\u2020", "")))
  span <- diff(spec$limits)
  top <- max(s$upper)
  a <- a %>% mutate(lane = if_else(x2-x1 == 2, 2, 1),
    y = top+span*(.08+.075*(lane-1)), tip = y-span*.018, text_y = y+span*.008,
    bracket_x1 = x1+if_else(x1 == 2 & length(spec$levels) == 3, .025, 0),
    bracket_x2 = x2-if_else(x2 == 2 & length(spec$levels) == 3, .025, 0))
  s <- s %>% mutate(label = sprintf("%.1f%%", estimate), label_y = lower-span*.035)
  stopifnot(!anyNA(a$x1), !anyNA(a$x2), all(a$x1 < a$x2),
    all(s$label_y > spec$limits[1]), max(a$text_y)+span*.05 < spec$limits[2])
  p <- ggplot(s, aes(x, estimate, colour = condition)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .11, linewidth = 1.1) +
    geom_point(size = 4.8) +
    geom_text(aes(y = label_y, label = label), vjust = 1, fontface = "bold", size = 5.1) +
    geom_segment(data = a, aes(x = bracket_x1, xend = bracket_x2, y = y, yend = y),
      inherit.aes = FALSE, colour = "#58636C", linewidth = .65) +
    geom_segment(data = a, aes(x = bracket_x1, xend = bracket_x1, y = tip, yend = y),
      inherit.aes = FALSE, colour = "#58636C", linewidth = .65) +
    geom_segment(data = a, aes(x = bracket_x2, xend = bracket_x2, y = tip, yend = y),
      inherit.aes = FALSE, colour = "#58636C", linewidth = .65) +
    geom_text(data = a, aes(x = (x1+x2)/2, y = text_y, label = symbol),
      inherit.aes = FALSE, colour = "#58636C", vjust = 0, size = 5.5) +
    scale_colour_manual(values = colours) +
    scale_x_continuous(breaks = seq_along(spec$levels), labels = spec$labels, expand = expansion(add = .4)) +
    scale_y_continuous(breaks = spec$breaks, expand = expansion(mult = 0)) +
    coord_cartesian(ylim = spec$limits) +
    labs(x = NULL, y = spec$ylab, title = spec$title, subtitle = spec$subtitle,
      caption = paste(spec$interval_note, spec$test_note, spec$note, sep = "\n")) + slide_theme() +
    theme(strip.text = element_text(size = 18, face = "bold", lineheight = 1.05, margin = margin(b = 8)),
      panel.spacing.x = grid::unit(1, "lines"))
  if (length(spec$panels) > 1) p <- p + facet_wrap(~panel, labeller = as_labeller(setNames(spec$facet_labels, spec$panels)))
  figures[[spec$stem]] <- p
  for (ext in c("png", "pdf")) ggsave(file.path(OUT, paste0(spec$stem, ".", ext)), p,
    width = 12, height = 6.75, units = "in", dpi = 300, bg = "white",
    device = if (ext == "pdf") cairo_pdf else "png")
  write_csv(s, file.path(OUT, "data", paste0(spec$stem, "_data.csv")))
  write_csv(bind_rows(normalized), file.path(OUT, "data", paste0(spec$stem, "_participants.csv")))
  write_csv(a, file.path(OUT, "data", paste0(spec$stem, "_annotations.csv")))
  if (h == "H6") write_csv(followup_interaction, file.path(OUT, "data", paste0(spec$stem, "_interaction.csv")))
  all_summaries[[h]] <- s %>% mutate(hypothesis = h, filename = spec$stem)
  all_annotations[[h]] <- a %>% mutate(filename = spec$stem)
  catalog[[h]] <- tibble(order = 0L, hypothesis = h, role = "Main presentation; exploratory origin",
    analysis_origin = if (h == "H6") "Exploratory presentation of existing H3/H4 evidence" else "Exploratory; specified after inspecting results",
    source_hypothesis = if (h == "H6") "Original H3/H4 plus exploratory timing interaction" else paste("Exploratory", h),
    filename = spec$stem, question = spec$question, key_message = spec$title,
    width_in = 12, height_in = 6.75, png_width = 3600L, png_height = 2025L,
    normalization_set = paste(unique(s$normalization_set), collapse = ";"), normalization_k = unique(s$k),
    y_min = spec$limits[1], y_max = spec$limits[2], notes = spec$note)
}
presentation_order <- paste0("H", 1:8)
catalog <- bind_rows(catalog)
catalog <- catalog %>% mutate(order = match(hypothesis, presentation_order)) %>% arrange(order)
stopifnot(nrow(catalog) == 8, identical(catalog$hypothesis, presentation_order))
figures <- figures[catalog$filename]
write_csv(catalog, file.path(OUT, "figure_catalog.csv"))
write_csv(bind_rows(all_summaries), file.path(OUT, "data/plotted_means_and_intervals.csv"))
write_csv(bind_rows(all_annotations), file.path(OUT, "data/significance_annotations.csv"))
for (bundle in c("primary_hypotheses", "all_hypotheses")) {
  selected <- figures
  cairo_pdf(file.path(OUT, paste0(bundle, ".pdf")), width = 12, height = 6.75, onefile = TRUE)
  tryCatch(invisible(lapply(selected, print)), finally = dev.off())
}
consumed <- unique(c(consumed, file.path(ROOT, "plot_aid_onset_slides.R")))
write_csv(tibble(path = consumed, md5 = unname(tools::md5sum(consumed))), file.path(OUT, "data/input_checksums.csv"))
writeLines(capture.output(sessionInfo()), file.path(OUT, "data/session_info.txt"))
escape <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE)
cards <- vapply(seq_len(nrow(catalog)), function(i) paste0(
  '<article><h2>', escape(catalog$key_message[i]), '</h2><a href="', catalog$filename[i], '.png"><img src="',
  catalog$filename[i], '.png" alt="', escape(catalog$key_message[i]), '"></a><p><a href="',
  catalog$filename[i], '.png">PNG</a> | <a href="', catalog$filename[i], '.pdf">PDF</a> | <a href="data/',
  catalog$filename[i], '_data.csv">Plotted data</a> | <a href="data/', catalog$filename[i],
  '_annotations.csv">Bracket tests</a></p></article>'), character(1))
writeLines(c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Aid-onset hypothesis figures</title>',
  '<style>body{font:18px Helvetica,Arial,sans-serif;color:#202A31;max-width:1200px;margin:32px auto;padding:0 24px}h1{font-size:32px}h2{font-size:24px}article{margin:40px 0 56px}img{width:100%;border:1px solid #e2e6e8}p{line-height:1.5}a{color:#1976a3}</style></head><body>',
  '<h1>Aid-onset: hypothesis figures for presentations</h1>',
  '<p>Eight main presentation figures (H1-H8). H5 is revision quality, H6 is advice-use errors, H7 is selective uptake in Stimulus-first, and H8 is trust. H5-H8 retain their exploratory origin; promotion to the primary presentation does not change when the questions were specified or their existing tests. All use 60 participants, including replacement p59. Each figure is 16:9: 3600 x 2025 PNG at 300 dpi, plus vector PDF. Insert the PNG at full slide width without cropping.</p>',
  '<p><a href="primary_hypotheses.pdf">H1-H8 primary presentation PDF</a> | <a href="all_hypotheses.pdf">All eight figures PDF</a> | <a href="figure_catalog.csv">Figure index and analysis origins</a></p>',
  '<p>Means weight participants equally. <a href="https://doi.org/10.20982/tqmp.04.2.p061">Cousineau-Morey intervals</a> remove between-participant variation for the complete repeated-measures set; they are pointwise, not model intervals or intervals for differences. CI overlap is not a formal test. H3/H4 share five-cell normalization and identical Manual means/intervals; Manual is not assigned an advice-correctness category. Their accuracy axes also match.</p>',
  '<p>H2-H4 brackets retain their original three-contrast Holm families. H5 uses two separate participant-random-intercept logistic GLMMs for beneficial and harmful revisions, with Holm correction across all six contrasts. H6 complements the original H3/H4 accuracy timing contrasts into error odds, retaining their original Holm families; its saved interaction is exploratory and unadjusted. H7 uses one exploratory participant-random-intercept GLMM contrast among initial-disagreement trials in Stimulus-first. H1 and trust retain their single paired t tests. * p &lt; .05; ns p &gt;= .05; a dagger flags a model-versus-participant significance disagreement. On H5, both harmful-revision contrasts involving Manual have this flag. The plotting script does not refit models. Nonsignificance does not establish equivalence.</p>',
  '<p>H5 rates use all trials, with three-condition Morey normalization separately for each revision type. H6 uses two-timing normalization separately for each advice type; it presents existing H3/H4 evidence, not new independent evidence. H7 uses initial-disagreement opportunities and two-advice-type normalization; correct-advice opportunities begin with an incorrect judgment, whereas incorrect-advice opportunities begin with a correct judgment. This is a conditional behavioural comparison, not an isolated causal effect of advice correctness. Aid-first Decision 1 already follows advice exposure, so cross-timing revision rates start from different information states. Original H5 overall accuracy is retired from this presentation and retained in the original analysis results and archived figures.</p>',
  cards, '</body></html>'), file.path(OUT, "index.html"))
writeLines(c("AID-ONSET PRESENTATION FIGURES", "",
  "Reproduce analyses: Rscript analyse_aid_onset_followups.R",
  "Reproduce plots: Rscript plot_aid_onset_slides.R [output_dir]", "",
  "H1-H8 each have one 12 x 6.75 inch (16:9) vector PDF and 3600 x 2025 PNG at 300 dpi.",
  "H5 = revision quality; H6 = advice-use errors; H7 = selective uptake in Stimulus-first.",
  "H5-H7 are main presentation figures with an explicitly exploratory origin, selected after inspecting results.",
  "The old H5 overall-accuracy figure is retired from the presentation; original analysis tables retain their original IDs.",
  "H8 = six-item mean trust, promoted from the original exploratory timing comparison.",
  "H8 retains the original two-sided paired t test, unadjusted single-comparison p-value and two-condition Morey CIs.",
  "primary_hypotheses.pdf and all_hypotheses.pdf both contain all eight main presentation figures (H1-H8).",
  "Primary presentation numbering does not recast exploratory origins as originally specified hypotheses.",
  "index.html is the visual gallery; figure_catalog.csv maps hypothesis, message and filename.", "",
  "COHORT: 60 participants. Replacement p59 20260924_110831 included; only the old run is excluded.",
  "All 600 source accuracy/revision participant cells were checked against the trial data.",
  "Input data and analysis scripts must match the saved analysis checksums before plotting.",
  "Morey intervals: center = observed mean; SE = SD(normalized scores)/sqrt(n) * sqrt(k/(k-1)).",
  "Normalize by subtracting the participant mean over the specified complete set and adding the grand mean.",
  "Use t(.975, n-1) for pointwise 95% limits. No multiplicity correction is applied to intervals.",
  "H1 k=2: Manual self-rating versus each person's mean rating of the two aided conditions.",
  "H2 k=3: overall revision proportions across Manual, Aid-first and Stimulus-first.",
  "H3/H4 k=5: Manual plus both timings for correct and incorrect advice, with Manual included only once.",
  "H5 k=3 within each revision endpoint, across the three conditions. BOTH rates use all trials.",
  "H6 k=2 within each advice type, across the two timings. Its CIs use this paired normalization, not the H3/H4 five-cell set.",
  "H7 k=2 across correct/incorrect advice uptake, conditional on initial disagreement in Stimulus-first. H8 trust k=2.",
  "These intervals describe within-person patterns, not between-person uncertainty or contrast CIs.",
  "H2-H4 brackets retain the original GLMM three-contrast Holm families.",
  "H5 brackets use two separate logistic GLMMs and Holm adjustment across six contrasts (both endpoints).",
  "H6 brackets invert original H3/H4 accuracy odds ratios into error odds; original Holm families are retained.",
  "H6 is a re-expression of existing evidence. Its interaction annotation uses the saved unadjusted exploratory test.",
  "H7 uses one exploratory GLMM contrast: correct-advice versus incorrect-advice uptake among initial disagreements.",
  "H1 and H8 trust brackets use the saved two-sided paired t tests, each a single comparison.",
  "Exact p-values, oriented contrasts, odds ratios, CIs and sensitivity p-values are exported in data/.",
  "A dagger flags any disagreement in significance with the matching participant sensitivity test.",
  "H5 harmful-revision contrasts of each timing with Manual are significant in the GLMM but not the paired sensitivity.",
  "Nonsignificance is not equivalence. The original analysis diagnostic limitations still apply.",
  "H3/H4 share y-axis ranges, Manual means and Manual intervals. No individual observations are plotted.",
  "Mean/interval plots have explicitly labelled nonzero y-axis minima where useful.",
  "Advice precedes Decision 1 in Aid-first; H2 revisions do not share an independent pre-advice baseline.",
  "H7 correct-advice opportunities begin with a wrong judgment; incorrect-advice opportunities begin with a correct judgment.",
  "H7 describes selectivity among observed opportunities, not an isolated causal effect of advice correctness.",
  "Older key-findings and individual/group diagnostic figures remain available in their original folders.",
  "Method: Morey (2008), https://doi.org/10.20982/tqmp.04.2.p061"
), file.path(OUT, "README.txt"))
# Retire only exact previous presentation names, after successful replacement.
# Preserve the artifacts rather than leaving a second H5 or trust slide in the active set.
retired_stems <- c("05_h5_overall_accuracy", "06_exploratory_trust", "08_exploratory_trust")
retired_files <- c(unlist(lapply(retired_stems, function(stem) paste0(stem, c(".pdf", ".png")))),
  unlist(lapply(retired_stems, function(stem) file.path("data", paste0(stem, c("_data.csv", "_participants.csv", "_annotations.csv"))))))
for (relative in retired_files) {
  old <- file.path(OUT, relative)
  if (file.exists(old)) {
    target <- file.path(OUT, "archive/retired", relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    stopifnot(!file.exists(target), file.rename(old, target))
  }
}
cat("Validated 60 participants, 600 trial-derived cells and saved analysis provenance.\n")
cat("Wrote H1-H8 (eight PDF/PNG pairs) to", OUT, "\n")
