# Independent count, SDT, paired-inference, interval and artifact validation.
# Rscript tests/validate_aid_onset_sdt.R [analysis_dir] [plot_dir]
source("analyse_aid_onset_sdt.R")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 2)
out <- if (length(args)) args[1] else "analysis_outputs/semester2_2026_presentation_followups/sdt_alternates"
plots <- if (length(args) >= 2) args[2] else "plots/semester2_2026_key_findings/presentation/alternates"
read <- function(name) read.csv(file.path(out, paste0(name, ".csv")), stringsAsFactors = FALSE)
near <- function(a, b) stopifnot(length(a) == length(b), length(a) > 0,
  all(is.finite(a)), all(is.finite(b)), max(abs(a-b)) < 1e-9)
fails <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))

# Known geometry, reversed labels, extreme rates and invalid/missing cells.
fixture <- data.frame(participant_id = 1, condition = "Aid first", advice = c("Correct", "Incorrect"),
  n_answered = 100, n_agree = c(80, 20))
f <- sdt_scores(fixture)
near(f$automation_bias, 0); near(f$dprime, 2*qnorm(80.5/101))
near(sdt_scores(transform(fixture, n_agree = c(20, 80)))$dprime, -f$dprime)
accept <- sdt_scores(transform(fixture, n_agree = 100))
reject <- sdt_scores(transform(fixture, n_agree = 0))
near(accept$dprime, 0); near(reject$dprime, 0)
stopifnot(accept$automation_bias > 0, reject$automation_bias < 0)
near(reject$automation_bias, -accept$automation_bias)
unequal <- transform(fixture, n_answered = c(200, 40), n_agree = c(0, 40))
for (method in c("loglinear", "extreme_only")) {
  score <- sdt_scores(unequal, method)
  rates <- if (method == "loglinear") c(.5/201, 40.5/41) else c(.5/200, 1-.5/40)
  near(score$hit_rate, rates[1]); near(score$false_alarm_rate, rates[2])
}
fails(sdt_scores(transform(fixture, n_answered = 0)))
fails(sdt_scores(transform(fixture, n_agree = 101)))
fails(sdt_scores(transform(fixture, n_agree = 1.5)))
fails(sdt_scores(transform(fixture, n_agree = NA_real_)))
fails(sdt_scores(fixture[1, ]))
fails(sdt_scores(rbind(fixture, fixture[1, ])))

# Reconstruct all participant counts directly, without production count helpers.
raw <- read.csv("data/data_virus_all.csv")
a <- raw[raw$aid_condition != "manual", ]
stopifnot(nrow(raw) == 46800L, nrow(a) == 31200L, setequal(a$participant_id, 1:60),
  identical(unique(a$run_timestamp[a$participant_id == 59]), "20260924_110831"),
  !anyNA(a[c("decision2_response", "decision2_matches_aid", "aid_correct", "decision2_correct")]),
  all(a$decision2_matches_aid == (a$decision2_correct == a$aid_correct)))
counts <- read("participant_counts")
scores <- read("participant_sdt")
contrasts <- read("contrasts")
stopifnot(nrow(counts) == 240, nrow(scores) == 240, nrow(contrasts) == 4,
  !anyDuplicated(counts[c("participant_id", "condition", "advice")]),
  !anyDuplicated(scores[c("participant_id", "condition", "correction")]))
for (i in seq_len(nrow(counts))) {
  c <- counts[i, ]
  condition <- if (c$condition == "Aid first") "aid_first" else "stimulus_first"
  d <- a[a$participant_id == c$participant_id & a$aid_condition == condition &
    a$aid_correct == (c$advice == "Correct"), ]
  near(c$n_answered, nrow(d)); near(c$n_agree, sum(d$decision2_matches_aid))
}
for (i in seq_len(nrow(scores))) {
  s <- scores[i, ]
  c <- counts[counts$participant_id == s$participant_id & counts$condition == s$condition, ]
  c <- c[match(c("Correct", "Incorrect"), c$advice), ]
  rate <- c$n_agree/c$n_answered
  if (s$correction == "loglinear") rate <- (c$n_agree+.5)/(c$n_answered+1) else {
    rate[c$n_agree == 0] <- .5/c$n_answered[c$n_agree == 0]
    rate[c$n_agree == c$n_answered] <- 1-.5/c$n_answered[c$n_agree == c$n_answered]
  }
  z <- qnorm(rate)
  near(s$hit_rate, rate[1]); near(s$false_alarm_rate, rate[2])
  near(s$automation_bias, mean(z)); near(s$criterion_c, -mean(z)); near(s$dprime, z[1]-z[2])
}

# Calculate t statistics and confidence limits directly from paired differences.
for (i in seq_len(nrow(contrasts))) {
  t <- contrasts[i, ]; d <- scores[scores$correction == t$correction, ]
  af <- d[d$condition == "Aid first", ]; sf <- d[d$condition == "Stimulus first", ]
  af <- af[order(af$participant_id), ]; sf <- sf[order(sf$participant_id), ]
  stopifnot(identical(af$participant_id, sf$participant_id), nrow(af) == 60)
  change <- sf[[t$outcome]]-af[[t$outcome]]
  se <- sd(change)/sqrt(length(change)); df <- length(change)-1
  near(t$mean_aid_first, mean(af[[t$outcome]])); near(t$mean_stimulus_first, mean(sf[[t$outcome]]))
  near(t$estimate, mean(change)); near(t$se, se); near(t$statistic, mean(change)/se); near(t$df, df)
  near(c(t$lower, t$upper), mean(change)+c(-1,1)*qt(.975,df)*se)
  near(t$p_raw, 2*pt(-abs(mean(change)/se),df))
}
for (method in unique(contrasts$correction)) {
  t <- contrasts[contrasts$correction == method, ]
  near(t$p_holm, p.adjust(t$p_raw, "holm"))
}
robust <- read("correction_sensitivity")
for (i in seq_len(nrow(robust))) {
  r <- robust[i, ]; t <- contrasts[contrasts$outcome == r$outcome, ]
  stopifnot(r$same_direction == (length(unique(sign(t$estimate))) == 1),
    r$same_holm_decision == (length(unique(t$p_holm < .05)) == 1))
}

# k=2 Morey SE equals SD(paired difference)/sqrt(2*N), independent of normalization code.
catalog <- read.csv(file.path(plots, "figure_catalog.csv"))
stopifnot(nrow(catalog) == 2, all(catalog$hypothesis == "H6"), all(catalog$version == "Alternate"),
  identical(catalog$filename, c("06_main_H6_alternate_sdt_reliance", "06_main_H6_alternate_sdt_discrimination")),
  all(catalog$normalization_k == 2), all(catalog$width_in/catalog$height_in == 16/9))
for (i in seq_len(nrow(catalog))) {
  row <- catalog[i, ]; stem <- row$filename
  d <- scores[scores$correction == "loglinear", ]
  af <- d[d$condition == "Aid first", ]; sf <- d[d$condition == "Stimulus first", ]
  af <- af[order(af$participant_id), ]; sf <- sf[order(sf$participant_id), ]
  change <- sf[[row$outcome]]-af[[row$outcome]]
  expected_se <- sd(change)/sqrt(120)
  s <- read.csv(file.path(plots, "data", paste0(stem, "_data.csv")))
  for (j in seq_len(nrow(s))) {
    values <- d[d$condition == s$condition[j], row$outcome]
    near(s$estimate[j], mean(values)); near(s$se[j], expected_se)
    near(c(s$lower[j], s$upper[j]), mean(values)+c(-1,1)*qt(.975,59)*expected_se)
  }
  annotations <- read.csv(file.path(plots, "data", paste0(stem, "_annotations.csv")))
  t <- contrasts[contrasts$correction == "loglinear" & contrasts$outcome == row$outcome, ]
  near(annotations$p_holm, t$p_holm)
  stopifnot(annotations$symbol == ifelse(t$p_holm < .05, "*", "ns"),
    min(s$label_y) > row$y_min, max(annotations$text_y) < row$y_max)
  points <- read.csv(file.path(plots, "data", paste0(stem, "_participants.csv")))
  stopifnot(nrow(points) == 120)
  for (id in 1:60) {
    v <- points[points$participant_id == id, ]
    near(v$normalized, v$value-mean(v$value)+mean(points$value))
  }
  dimensions <- dim(png::readPNG(file.path(plots, paste0(stem, ".png"))))
  stopifnot(identical(dimensions[1:2], c(2025L, 3600L)))
}

for (path in c(file.path(out, "input_checksums.csv"), file.path(plots, "data/input_checksums.csv"),
  file.path(out, "artifact_manifest.csv"))) {
  m <- read.csv(path)
  stopifnot(all(file.exists(m$path)), identical(unname(tools::md5sum(m$path)), m$md5))
}
stopifnot(file.exists(file.path(out, "COMPLETE.txt")), file.exists(file.path(plots, "COMPLETE.txt")))
pdfinfo <- Sys.which("pdfinfo")
stopifnot(nzchar(pdfinfo))
for (stem in c(catalog$filename, "H6_alternate_sdt")) {
  info <- system2(pdfinfo, shQuote(file.path(plots, paste0(stem, ".pdf"))), stdout = TRUE)
  pages <- as.integer(sub("^Pages:\\s+", "", grep("^Pages:", info, value = TRUE)))
  stopifnot(pages == if (stem == "H6_alternate_sdt") 2L else 1L,
    grepl("864 x 486 pts", grep("^Page size:", info, value = TRUE), fixed = TRUE))
}
message("Passed: source counts, run provenance, SDT signs/corrections, paired inference, Holm, Morey intervals, annotations, PNG/PDF dimensions and checksums.")
