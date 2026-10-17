# Independent counts, statistics, selection, plotted data, dimensions and provenance.
# Rscript tests/validate_dynamic_reliability_task_sdt.R [analysis_dir] [plot_dir]
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
root <- dirname(dirname(script)); args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 2)
out <- if (length(args)) args[1] else file.path(root, "analysis_outputs/semester2_2026_behavioural/hypothesis_components/task_sdt")
plots <- if (length(args) > 1) args[2] else file.path(root, "analysis_outputs/semester2_2026_behavioural/hypothesis_components/task_plots")
source(file.path(root, "analyse_dynamic_reliability_task_sdt.R"))
read <- function(name, folder = out) read_csv(file.path(folder, paste0(name, ".csv")), show_col_types = FALSE)
near <- function(x, y, tol = 1e-9) stopifnot(length(x) == length(y), length(x) > 0,
  all(is.finite(x)), all(is.finite(y)), max(abs(x-y)) < tol)
fails <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))

# Unit cases target semantic labels, signs, class reversal, missing classes and RT eligibility.
fixture <- tibble(participant_id = 1, group = "CAL65", stage = "P1", agent = "participant",
  hits = 80, misses = 20, false_alarms = 20, correct_rejections = 80, n_signal = 100, n_noise = 100, n = 200)
x <- task_scores(fixture); near(x$criterion_c, 0); near(x$dprime, 2*qnorm(80.5/101))
black <- task_scores(mutate(fixture, hits = 100, misses = 0, false_alarms = 100, correct_rejections = 0))
near(black$dprime, 0); stopifnot(black$criterion_c < 0)
asymmetric <- mutate(fixture, hits = 90, misses = 10, false_alarms = 30, correct_rejections = 70)
flipped <- mutate(asymmetric, hits = 70, misses = 30, false_alarms = 10, correct_rejections = 90)
near(task_scores(asymmetric)$dprime, task_scores(flipped)$dprime)
near(task_scores(asymmetric)$criterion_c, -task_scores(flipped)$criterion_c)
for (correction in c("loglinear", "extreme_only")) {
  s <- task_scores(mutate(fixture, hits = 100, misses = 0, false_alarms = 0, correct_rejections = 100), correction)
  stopifnot(is.finite(s$dprime), s$dprime > 0); near(s$criterion_c, 0)
}
fails(task_scores(mutate(fixture, n_signal = 0, hits = 0, misses = 0, n = 100)))
fails(task_scores(mutate(fixture, hits = -1)))
fails(task_scores(mutate(fixture, hits = 80.5, misses = 19.5)))
fails(task_scores(bind_rows(fixture, fixture)))
stopifnot(!task_inference(c(1, 1, 1))$inference_available)
trial_fixture <- tibble(participant_id = 1, group = "CAL65", stage = "P1",
  stimulus = c("BLACK", "WHITE", "BLACK", "WHITE", "BLACK"),
  response = c("BLACK", "WHITE", "WHITE", "BLACK", "TIMEOUT"),
  aid_label = c("BLACK", "WHITE", "BLACK", "WHITE", "BLACK"),
  answered = c(TRUE, TRUE, TRUE, TRUE, FALSE), rt_s = c(-.01, .1, .2, NA, NA), key_black = "J", key_white = "D")
tiny <- task_counts(trial_fixture)
stopifnot(tiny$n[tiny$agent == "participant"] == 4, tiny$n[tiny$agent == "aid_matched"] == 4,
  tiny$n[tiny$agent == "aid_all"] == 5, tiny$hits[tiny$agent == "participant"] == 1,
  tiny$false_alarms[tiny$agent == "participant"] == 1)
swapped_keys <- task_counts(mutate(trial_fixture, key_black = "D", key_white = "J"))
stopifnot(identical(tiny, swapped_keys))
tiny_performance <- task_performance(mutate(trial_fixture, response_correct = answered & response == stimulus))
stopifnot(tiny_performance$n_fast == 2, tiny_performance$n_nonpositive_rt == 1,
  tiny_performance$n_timeouts == 1, tiny_performance$n_answered_missing_rt == 1)
near(tiny_performance$accuracy, .4)

# Inputs and QC decisions must be current and all participants accounted for.
inputs <- read("input_checksums")
stopifnot(all(file.exists(inputs$path)),
  identical(vapply(inputs$path, task_sha, character(1), USE.NAMES = FALSE), inputs$sha256))
raw <- read_csv(inputs$path[1], show_col_types = FALSE)
qc_path <- inputs$path[basename(inputs$path) == "participant_summary.csv"]
qc <- read_csv(qc_path, show_col_types = FALSE)
membership <- read("cohort_membership")
rules <- c("calibration_target", "near_chance_or_worse", "anticipatory")
excluded <- qc$participant_id[rowSums(as.data.frame(lapply(qc[rules], function(x) x == "flag"))) > 0]
stopifnot(setequal(membership$participant_id, unique(raw$participant_id)),
  identical(sort(membership$participant_id[!membership$in_primary]), sort(excluded)),
  all(membership$in_all_participants), !anyDuplicated(membership$participant_id))
manual_only <- qc$participant_id[qc$manual_target == "flag" & !qc$exclude_by_rules]
stopifnot(all(manual_only %in% membership$participant_id[membership$in_primary]))

# Reconstruct every confusion matrix using table(), independently of production aggregation.
raw <- filter(raw, block != "CALIBRATION")
raw$stage <- ifelse(raw$block == "MANUAL", ifelse(raw$manual_segment == "PRE_AUTOMATION", "PRE", "POST"),
  paste0("P", raw$reliability_phase_idx))
counts <- read("participant_counts"); scores <- read("participant_sdt"); performance <- read("participant_performance")
N <- nrow(membership); retained_n <- sum(membership$in_primary)
stopifnot(nrow(counts) == N*11, nrow(scores) == (N+retained_n)*22,
  nrow(performance) == (N+retained_n)*5, !anyDuplicated(counts[c("participant_id", "stage", "agent")]))
for (i in seq_len(nrow(counts))) {
  z <- counts[i, ]; d <- raw[raw$participant_id == z$participant_id & raw$stage == z$stage, ]
  if (z$agent != "aid_all") d <- d[d$response != "TIMEOUT", ]
  decision <- if (z$agent == "participant") d$response else d$aid_label
  tab <- table(factor(d$stimulus, c("BLACK", "WHITE")), factor(decision, c("BLACK", "WHITE")))
  near(c(z$hits, z$misses, z$false_alarms, z$correct_rejections), c(tab[1, 1], tab[1, 2], tab[2, 1], tab[2, 2]))
  stopifnot(z$n == nrow(d), z$group == unique(d$calibration_target_group))
}
for (sample in c("primary", "all_participants")) {
  ids <- membership$participant_id[if (sample == "primary") membership$in_primary else TRUE]
  stopifnot(setequal(scores$participant_id[scores$sample == sample], ids),
    setequal(performance$participant_id[performance$sample == sample], ids))
}
for (i in seq_len(nrow(scores))) {
  z <- scores[i, ]; j <- which(counts$participant_id == z$participant_id & counts$stage == z$stage & counts$agent == z$agent)
  stopifnot(length(j) == 1)
  k <- counts[j, ]; near(unlist(z[c("hits", "misses", "false_alarms", "correct_rejections")]),
    unlist(k[c("hits", "misses", "false_alarms", "correct_rejections")]))
  rates <- if (z$correction == "loglinear") c((k$hits+.5)/(k$n_signal+1), (k$false_alarms+.5)/(k$n_noise+1)) else
    c(max(.5/k$n_signal, min(1-.5/k$n_signal, k$hits/k$n_signal)),
      max(.5/k$n_noise, min(1-.5/k$n_noise, k$false_alarms/k$n_noise)))
  near(c(z$hit_rate, z$false_alarm_rate), rates)
  near(z$dprime, diff(rev(qnorm(rates)))); near(z$criterion_c, -mean(qnorm(rates)))
}
for (i in seq_len(nrow(performance))) {
  z <- performance[i, ]; d <- raw[raw$participant_id == z$participant_id & raw$stage == z$stage, ]
  answered <- d$response != "TIMEOUT"
  expected <- c(nrow(d), sum(answered), sum(!answered), sum(d$response == d$stimulus),
    mean(d$response == d$stimulus), mean(!answered), mean(d$response[answered] == d$stimulus[answered]),
    sum(answered & is.finite(d$rt_s) & d$rt_s <= 0), sum(answered & is.finite(d$rt_s) & d$rt_s < .2),
    sum(answered & !is.finite(d$rt_s)))
  near(unlist(z[c("n_trials", "n_answered", "n_timeouts", "n_correct", "accuracy", "timeout_rate",
    "answered_accuracy", "n_nonpositive_rt", "n_fast", "n_answered_missing_rt")]), expected)
}

# Direct t.test() verifies all paired estimates, Welch interactions, CIs and tests.
results <- read("contrasts"); changes <- read("participant_changes")
stopifnot(nrow(results) == 240, all(results$inference_available), nrow(changes) == (N+retained_n)*40)
for (i in seq_len(nrow(results))) {
  z <- results[i, ]; d <- filter(scores, sample == z$sample, correction == z$correction)
  benchmark <- z$family == "aid_benchmark"
  pair <- if (benchmark) rep(substr(z$comparison, 1, 2), 2) else strsplit(z$comparison, " - ", fixed = TRUE)[[1]]
  a <- filter(d, agent == "participant", stage == pair[1]) %>% arrange(participant_id)
  b <- filter(d, agent == if (benchmark) "aid_matched" else "participant", stage == pair[2]) %>% arrange(participant_id)
  stopifnot(identical(a$participant_id, b$participant_id), identical(a$group, b$group))
  value <- a[[z$outcome]]-b[[z$outcome]]
  saved <- filter(changes, sample == z$sample, correction == z$correction, outcome == z$outcome,
    family == z$family, comparison == z$comparison) %>% arrange(participant_id)
  near(saved$value, value); near(saved$left_value, a[[z$outcome]]); near(saved$right_value, b[[z$outcome]])
  if (z$group == "CAL90 - CAL65") {
    tt <- t.test(value[a$group == "CAL90"], value[a$group == "CAL65"])
    estimate <- mean(value[a$group == "CAL90"])-mean(value[a$group == "CAL65"])
  } else {
    tt <- t.test(value[a$group == z$group]); estimate <- mean(value[a$group == z$group])
  }
  near(z$estimate, estimate); near(z$se, tt$stderr); near(z$df, unname(tt$parameter))
  near(z$p_raw, tt$p.value); near(c(z$lower, z$upper), tt$conf.int[1:2])
}
for (d in split(results, interaction(results$sample, results$correction, results$outcome, results$family))) {
  stopifnot(nrow(d) == if (d$family[1] == "manual_drift") 3 else 9, all(d$family_n == nrow(d)))
  near(d$p_holm, p.adjust(d$p_raw, "holm"))
}
for (d in split(results, interaction(results$sample, results$correction, results$family)))
  near(d$p_holm_both_outcomes, p.adjust(d$p_raw, "holm"))
for (d in split(results, interaction(results$sample, results$correction)))
  near(d$p_holm_all_contrasts, p.adjust(d$p_raw, "holm"))
sens <- read("sensitivity_comparison"); stopifnot(nrow(sens) == 60)
for (i in seq_len(nrow(sens))) {
  z <- sens[i, ]; d <- filter(results, outcome == z$outcome, family == z$family, comparison == z$comparison, group == z$group)
  for (j in seq_len(nrow(d))) {
    variant <- paste(d$sample[j], d$correction[j], sep = "_")
    near(z[[paste0("estimate_", variant)]], d$estimate[j]); near(z[[paste0("p_holm_", variant)]], d$p_holm[j])
  }
  changed <- function(a, b) sign(z[[paste0("estimate_", a)]]) != sign(z[[paste0("estimate_", b)]]) ||
    ((z[[paste0("p_holm_", a)]] < .05) != (z[[paste0("p_holm_", b)]] < .05))
  stopifnot(z$sample_sensitive == changed("primary_loglinear", "all_participants_loglinear"),
    z$correction_sensitive_primary == changed("primary_loglinear", "primary_extreme_only"),
    z$correction_sensitive_all == changed("all_participants_loglinear", "all_participants_extreme_only"))
}

# Independently construct participant x stage matrices for Morey and ordinary CIs.
summaries <- read("descriptive_sdt"); perf_summary <- read("descriptive_performance")
for (kind in c("sdt", "performance")) {
  summary <- if (kind == "sdt") summaries else perf_summary
  for (i in seq_len(nrow(summary))) {
    z <- summary[i, ]
    if (kind == "sdt") {
      d <- filter(scores, sample == z$sample, group == z$group, agent == z$agent, correction == z$correction)
    } else d <- filter(performance, sample == z$sample, group == z$group)
    d$value <- d[[z$outcome]]*if (kind == "performance") 100 else 1
    wide <- d %>% select(participant_id, stage, value) %>% pivot_wider(names_from = stage, values_from = value)
    mat <- as.matrix(select(wide, -participant_id)); normalized <- mat-rowMeans(mat)+mean(mat)
    vals <- mat[, z$stage]; norm <- normalized[, z$stage]; n <- nrow(mat); k <- ncol(mat)
    se <- sd(vals)/sqrt(n); sem <- sd(norm)/sqrt(n)*sqrt(k/(k-1))
    near(z$mean, mean(vals)); near(z$se, se); near(z$se_morey, sem)
    near(c(z$lower, z$upper), mean(vals)+c(-1, 1)*qt(.975, n-1)*se)
    near(c(z$lower_morey, z$upper_morey), mean(vals)+c(-1, 1)*qt(.975, n-1)*sem)
    stopifnot(z$n == n, z$morey_k == k)
  }
}

# Plots must carry the exact saved data/tests and valid slide dimensions.
catalog <- read("figure_catalog", plots); stopifnot(nrow(catalog) == 14)
for (i in seq_len(nrow(catalog))) {
  z <- catalog[i, ]; folder <- file.path(plots, dirname(z$filename), "data"); stem <- basename(z$filename)
  d <- read(paste0(stem, "_data"), folder); a <- read(paste0(stem, "_annotations"), folder)
  if ("p_holm" %in% names(a)) {
    expected <- filter(results, sample == z$sample, correction == "loglinear")
    keys <- c("outcome", "family", "comparison", "group")
    joined <- left_join(a, expected, by = c("sample", "correction", keys), suffix = c("", "_expected"))
    stopifnot(nrow(joined) == nrow(a), !anyNA(joined$estimate_expected))
    for (column in c("estimate", "lower", "upper", "p_raw", "p_holm")) near(joined[[column]], joined[[paste0(column, "_expected")]])
    flags <- left_join(a, select(sens, outcome, family, comparison, group,
      expected_sample_sensitive = sample_sensitive,
      expected_primary = correction_sensitive_primary, expected_all = correction_sensitive_all),
      by = keys)
    stopifnot(all(a$sample_sensitive == flags$expected_sample_sensitive),
      all(a$correction_sensitive == if (z$sample == "primary") flags$expected_primary else flags$expected_all),
      all(a$symbol == paste0(ifelse(a$p_holm < .05, "*", "ns"),
        ifelse(a$sample_sensitive, " [S]", ""), ifelse(a$correction_sensitive, " [C]", ""))), nrow(a) == nrow(d))
  } else {
    expected <- if (grepl("accuracy_timeouts", stem)) filter(perf_summary, sample == z$sample) else
      filter(summaries, sample == z$sample, correction == "loglinear", agent == "participant")
    stopifnot(isTRUE(all.equal(d, expected, tolerance = 1e-9)))
  }
  png <- file(file.path(plots, paste0(z$filename, ".png")), "rb"); seek(png, 16)
  dimensions <- readBin(png, integer(), n = 2, size = 4, endian = "big"); close(png)
  stopifnot(identical(dimensions, c(3600L, 2025L)))
  info <- system2("pdfinfo", shQuote(file.path(plots, paste0(z$filename, ".pdf"))), stdout = TRUE)
  stopifnot(any(grepl("^Pages: +1$", info)), any(grepl("^Page size: +864 x 486 pts", info)))
  bbox <- system2("pdftotext", c("-bbox", shQuote(file.path(plots, paste0(z$filename, ".pdf"))), "-"), stdout = TRUE)
  words <- bbox[grepl("<word xMin=", bbox)]
  coord <- function(name) as.numeric(sub(paste0('.*', name, '="([^"]+)".*'), '\\1', words))
  stopifnot(length(words) > 0, min(coord("xMin")) >= 0, min(coord("yMin")) >= 0,
    max(coord("xMax")) <= 864, max(coord("yMax")) <= 486)
}
for (sample in c("primary", "all_participants")) {
  info <- system2("pdfinfo", shQuote(file.path(plots, sample, "task_sdt_figures.pdf")), stdout = TRUE)
  stopifnot(any(grepl("^Pages: +7$", info)), any(grepl("^Page size: +864 x 486 pts", info)))
}
for (folder in c(out, plots)) {
  html <- paste(readLines(file.path(folder, "index.html"), warn = FALSE), collapse = "\n")
  links <- regmatches(html, gregexpr('(href|src)="[^"]+"', html))[[1]]
  links <- sub('^(href|src)="([^"]+)"$', '\\2', links)
  stopifnot(all(file.exists(file.path(folder, links[!grepl("^https?://|^#", links)]))),
    file.exists(file.path(folder, "COMPLETE.txt")))
}
artifacts <- read("artifact_manifest")
paths <- file.path(ifelse(artifacts$kind == "analysis", out, plots), artifacts$path)
stopifnot(all(file.exists(paths)), identical(vapply(paths, task_sha, character(1), USE.NAMES = FALSE), artifacts$sha256),
  all(file.info(paths)$size == artifacts$bytes))
message(sprintf("PASS: semantic/RT edge cases; %d raw confusion matrices; %d participant scores; all 240 paired/Welch tests and Holm families; QC union; both sensitivities; Morey intervals; 14 figure pairs, two bundles, HTML links and provenance.", nrow(counts), nrow(scores)))
