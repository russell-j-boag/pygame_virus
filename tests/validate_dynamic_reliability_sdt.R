# Independent raw-count, statistical and artifact checks for alternate H1/H3/H4.
# Rscript tests/validate_dynamic_reliability_sdt.R [analysis_dir] [plot_dir]
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
root <- dirname(dirname(script)); args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 2)
base <- if (length(args)) args[1] else file.path(root, "analysis_outputs/semester2_2026_behavioural")
plot_base <- if (length(args) >= 2) args[2] else file.path(root, "plots/semester2_2026_behavioural")
out <- file.path(base, "sdt_alternates"); plots <- file.path(plot_base, "presentation", "alternates")
source(file.path(root, "analyse_dynamic_reliability_sdt.R"))
read <- function(name, folder = out) read_csv(file.path(folder, paste0(name, ".csv")), show_col_types = FALSE)
near <- function(x, y, tol = 1e-9) stopifnot(length(x) == length(y), length(x) > 0,
  all(is.finite(x)), all(is.finite(y)), max(abs(x-y)) < tol)
fails <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))

# Known geometry with balanced denominators; rates are corrected before transformation.
fixture <- tibble(participant_id = 1, group = "CAL65", phase = "P1", advice = c("Correct", "Incorrect"),
  n_answered = 100, n_agree = c(80, 20))
x <- sdt_scores(fixture); near(x$automation_bias, 0); near(x$dprime, 2*qnorm(80.5/101))
accept <- sdt_scores(mutate(fixture, n_agree = 100)); reject <- sdt_scores(mutate(fixture, n_agree = 0))
near(accept$dprime, 0); stopifnot(accept$automation_bias > 0); near(reject$automation_bias, -accept$automation_bias)
flip <- sdt_scores(mutate(fixture, n_agree = rev(n_agree))); near(flip$dprime, -x$dprime)
near(x$criterion_c, -x$automation_bias)
unequal <- mutate(fixture, n_answered = c(380, 20), n_agree = c(380, 0))
for (correction in c("loglinear", "extreme_only")) {
  u <- sdt_scores(unequal, correction)
  stopifnot(is.finite(u$automation_bias), is.finite(u$dprime), u$dprime > 0)
}
fails(sdt_scores(mutate(fixture, n_answered = 0)))
fails(sdt_scores(fixture[1, ])); fails(sdt_scores(bind_rows(fixture, fixture)))
fails(sdt_scores(mutate(fixture, n_agree = n_answered+1)))
fails(sdt_counts(tibble(participant_id = 1, group = "CAL65", phase = "P1", advice = "Correct", n = 10, agreement = .333)))
stopifnot(!sdt_difference(c(1, 1, 1))$inference_available)

# Independent trial-level reconstruction verifies eligibility and integer counts.
for (name in c("input_manifest", "source_checksums", "analysis_artifacts")) sdt_verify(file.path(base, paste0(name, ".csv")))
manifest <- read("input_manifest", base)
raw <- bind_rows(lapply(manifest$path[manifest$kind == "trials"], read_csv, show_col_types = FALSE))
expected_counts <- raw %>% filter(block == "AUTOMATION", response != "TIMEOUT") %>%
  transmute(participant_id, group = calibration_target_group, phase = paste0("P", reliability_phase_idx),
    advice = ifelse(aid_label == stimulus, "Correct", "Incorrect"), agrees = as.integer(response == aid_label)) %>%
  group_by(participant_id, group, phase, advice) %>% summarise(n_answered = n(), n_agree = sum(agrees), .groups = "drop") %>%
  arrange(participant_id, phase, advice)
counts <- read("participant_counts") %>% arrange(participant_id, phase, advice)
stopifnot(identical(counts[c("participant_id", "group", "phase", "advice")],
                    expected_counts[c("participant_id", "group", "phase", "advice")]))
near(counts$n_answered, expected_counts$n_answered); near(counts$n_agree, expected_counts$n_agree)
scores <- read("participant_sdt"); n_people <- n_distinct(raw$participant_id)
stopifnot(nrow(counts) == n_people*6, nrow(scores) == n_people*6)
for (i in seq_len(nrow(scores))) {
  s <- scores[i, ]; z <- filter(counts, participant_id == s$participant_id, phase == s$phase)
  p <- if (s$correction == "loglinear") (z$n_agree+.5)/(z$n_answered+1) else
    pmax(.5/z$n_answered, pmin(1-.5/z$n_answered, z$n_agree/z$n_answered))
  zscores <- setNames(qnorm(p), z$advice)
  near(s$hit_rate, p[z$advice == "Correct"]); near(s$false_alarm_rate, p[z$advice == "Incorrect"])
  near(s$automation_bias, mean(zscores)); near(s$criterion_c, -mean(zscores))
  near(s$dprime, unname(zscores["Correct"]-zscores["Incorrect"]))
}

# Direct t.test calls independently verify all phase contrasts and directions.
contrasts <- read("contrasts"); stopifnot(nrow(contrasts) == 36, all(contrasts$inference_available))
for (i in seq_len(nrow(contrasts))) {
  z <- contrasts[i, ]; pair <- strsplit(z$comparison, " - ", fixed = TRUE)[[1]]
  d <- filter(scores, correction == z$correction) %>% select(participant_id, group, phase, all_of(z$outcome)) %>%
    pivot_wider(names_from = phase, values_from = all_of(z$outcome))
  change <- d[[pair[1]]] - d[[pair[2]]]
  if (z$group == "CAL90 - CAL65") {
    expected <- t.test(change[d$group == "CAL90"], change[d$group == "CAL65"])
    estimate <- mean(change[d$group == "CAL90"])-mean(change[d$group == "CAL65"])
  } else {
    expected <- t.test(change[d$group == z$group]); estimate <- mean(change[d$group == z$group])
  }
  near(z$estimate, estimate); near(z$p_raw, expected$p.value); near(z$df, unname(expected$parameter))
  near(z$lower, expected$conf.int[1]); near(z$upper, expected$conf.int[2])
  stopifnot(z$hypothesis == if (pair[1] == "P2") "H1" else "H3")
}
for (d in split(contrasts, interaction(contrasts$outcome, contrasts$correction))) {
  stopifnot(nrow(d) == 9); near(d$p_holm, p.adjust(d$p_raw, "holm"))
}
for (d in split(contrasts, contrasts$correction)) near(d$p_holm_both_outcomes, p.adjust(d$p_raw, "holm"))

# Independent matrix OLS + HC3 calculation, without sandwich::vcovHC.
ratings <- read("participant_ratings", base); associations <- read("subjective_associations")
predictions <- read("subjective_predictions")
stopifnot(nrow(associations) == 4, all(associations$inference_available))
for (i in seq_len(nrow(associations))) {
  z <- associations[i, ]; d <- filter(scores, phase == "P2", correction == z$correction) %>% arrange(participant_id)
  r <- ratings[match(d$participant_id, ratings$participant_id), ]
  near(r$perceived_aid_error, r$perceived_aid_accuracy-r$realised_aid_accuracy)
  x <- cbind(1, as.numeric(d$group == "CAL90"), d$automation_bias-mean(d$automation_bias))
  bread <- solve(crossprod(x)); beta <- as.vector(bread %*% crossprod(x, r[[z$outcome]]))
  residual <- r[[z$outcome]] - as.vector(x %*% beta)
  leverage <- rowSums((x %*% bread)*x)
  v <- bread %*% crossprod(x, x*as.vector((residual/(1-leverage))^2)) %*% bread
  se <- sqrt(v[3, 3]); df <- nrow(x)-3
  near(z$estimate, beta[3]); near(z$se, se); near(z$df, df)
  near(z$lower, beta[3]-qt(.975, df)*se); near(z$upper, beta[3]+qt(.975, df)*se)
  near(z$p_raw, 2*pt(-abs(beta[3]/se), df))
  saved <- readRDS(file.path(out, "models", paste0(z$outcome, "_", z$correction, ".rds")))
  near(unname(coef(saved$fit)), beta); near(as.vector(saved$vcov_hc3), as.vector(v))
  p <- filter(predictions, outcome == z$outcome, correction == z$correction)
  xp <- cbind(1, as.numeric(p$group == "CAL90"), p$automation_bias-mean(d$automation_bias))
  mean <- as.vector(xp %*% beta); se_pred <- sqrt(rowSums((xp %*% v)*xp))
  near(p$prediction, mean); near(p$lower, mean-qt(.975, df)*se_pred); near(p$upper, mean+qt(.975, df)*se_pred)
}
for (d in split(associations, associations$correction)) near(d$p_holm, p.adjust(d$p_raw, "holm"))
robustness <- read("correction_sensitivity"); stopifnot(nrow(robustness) == 20)
stopifnot(all(robustness$same_direction == (sign(robustness$estimate_loglinear) == sign(robustness$estimate_extreme_only))),
  all(robustness$same_holm_decision == ((robustness$p_holm_loglinear < .05) == (robustness$p_holm_extreme_only < .05))))

# Plot summaries, exact annotations, shape/size, gallery and complete provenance.
catalog <- read("figure_catalog", plots)
stopifnot(nrow(catalog) == 5, identical(catalog$hypothesis, c("H1", "H1", "H3", "H3", "H4")),
  all(catalog$version == "Alternate"), all(grepl("_alternate_sdt_", catalog$filename)))
for (i in seq_len(nrow(catalog))) {
  s <- catalog[i, ]; d <- read(paste0(s$filename, "_data"), file.path(plots, "data"))
  a <- read(paste0(s$filename, "_annotations"), file.path(plots, "data"))
  if (s$hypothesis == "H4") {
    groups <- filter(a, panel_type == "Group comparison")
    original <- read("subjective_contrasts", base) %>% filter(contrast == "CAL90 - CAL65")
    stopifnot(nrow(a) == 4, nrow(groups) == 2, identical(groups$outcome, original$outcome),
      all(groups$adjustment_family == "Original two-test family within rating outcome"))
    for (column in c("estimate", "lower", "upper", "p_raw", "p_holm")) near(groups[[column]], original[[column]])
    stopifnot(all(groups$symbol == ifelse(original$p_holm < .05, "*", "ns")))
    means <- filter(d, panel_type == "Group means")
    stopifnot(nrow(means) == 4, sum(d$panel_type == "Association") == 2*nrow(ratings))
    for (j in seq_len(nrow(means))) {
      z <- means[j, ]; vals <- ratings[[z$outcome]][ratings$group == z$group]
      tt <- t.test(vals)
      near(z$rating, mean(vals)); near(z$lower, tt$conf.int[1]); near(z$upper, tt$conf.int[2])
      near(z$se, sd(vals)/sqrt(length(vals))); stopifnot(z$n == length(vals))
    }
    a <- filter(a, panel_type == "Association")
  }
  expected <- if (s$hypothesis == "H4") filter(associations, correction == "loglinear") else
    filter(contrasts, correction == "loglinear", hypothesis == s$hypothesis, outcome == s$outcome)
  stopifnot(identical(a$contrast, expected$contrast), identical(a$outcome, expected$outcome))
  near(a$estimate, expected$estimate); near(a$lower, expected$lower); near(a$upper, expected$upper)
  near(a$p_holm, expected$p_holm)
  all_tests <- if (s$hypothesis == "H4") associations else contrasts
  alt <- filter(all_tests, correction == "extreme_only")
  ix <- match(paste(a$outcome, a$contrast), paste(alt$outcome, alt$contrast))
  changed <- sign(a$estimate) != sign(alt$estimate[ix]) | ((a$p_holm < .05) != (alt$p_holm[ix] < .05))
  stopifnot(all(a$correction_sensitive == changed), all(a$symbol == paste0(ifelse(expected$p_holm < .05, "*", "ns"),
    ifelse(changed, "\u2020", ""))))
  if (s$hypothesis != "H4") for (j in seq_len(nrow(d))) {
    z <- d[j, ]; vals <- filter(scores, correction == "loglinear", group == z$group, phase == z$phase)[[s$outcome]]
    expected_mean <- t.test(vals)
    near(z$mean, mean(vals)); near(z$lower, expected_mean$conf.int[1]); near(z$upper, expected_mean$conf.int[2])
    stopifnot(z$n == length(vals))
  }
  png <- file(file.path(plots, paste0(s$filename, ".png")), "rb"); seek(png, 16)
  dimensions <- readBin(png, integer(), n = 2, size = 4, endian = "big"); close(png)
  stopifnot(identical(dimensions, c(3600L, 2025L)))
  info <- system2("pdfinfo", shQuote(file.path(plots, paste0(s$filename, ".pdf"))), stdout = TRUE)
  stopifnot(any(grepl("^Pages: +1$", info)), any(grepl("^Page size: +864 x 486 pts", info)))
}
pdf <- file.path(plots, "H1_H3_H4_alternate_sdt.pdf")
info <- system2("pdfinfo", shQuote(pdf), stdout = TRUE)
stopifnot(any(grepl("^Pages: +5$", info)), any(grepl("^Page size: +864 x 486 pts", info)))
html <- readLines(file.path(plots, "index.html"))
stopifnot(all(vapply(catalog$filename, function(x) any(grepl(paste0(x, ".png"), html, fixed = TRUE)), logical(1))))
for (path in c(file.path(out, "input_checksums.csv"), file.path(out, "analysis_artifacts.csv"),
  file.path(plots, "data", "input_checksums.csv"))) sdt_verify(path)
artifacts <- read("artifact_manifest", file.path(plots, "data"))
stopifnot(nrow(artifacts) == 11, identical(unname(tools::md5sum(file.path(plots, artifacts$file))), artifacts$md5),
  file.exists(file.path(out, "COMPLETE.txt")), file.exists(file.path(plots, "COMPLETE.txt")))
message("PASS: raw counts/eligibility; SDT signs/corrections; all 36 paired/Welch contrasts; four independent HC3 models; Holm families; H4 group means and original saved tests; plotted means/annotations; five 16:9 figure pairs; combined PDF and provenance.")
