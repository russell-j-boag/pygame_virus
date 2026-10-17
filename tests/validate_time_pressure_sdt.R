# Independent checks of SDT coding, crossed-design contrasts and plot exports.
# Run after Rscript analyse_time_pressure_sdt.R, from the repository root.
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})
source("analyse_time_pressure_sdt.R")
args <- commandArgs(trailingOnly=TRUE);stopifnot(length(args)<=2)
base <- if(length(args)>=1)args[1] else "analysis_outputs/semester2_2026_behavioural"
out <- file.path(base, "sdt_alternates")
plots <- if(length(args)>=2)args[2] else file.path(base,"hypothesis_components/presentation/alternates")
read <- function(name) read_csv(file.path(out, paste0(name, ".csv")), show_col_types = FALSE)
near <- function(x, y) stopifnot(length(x) == length(y), length(x) > 0,
  all(is.finite(x)), all(is.finite(y)), max(abs(x-y)) < 1e-9)
# Known SDT geometry, sign, endpoints, and invalid denominator checks.
fixture <- tibble(participant_id = 1, pattern = "HP95_LP65", pressure = "HP", reliability = "95%",
  advice = c("Correct", "Incorrect"), n_answered = 100, n_agree = c(80,20))
x <- sdt_scores(fixture)
near(x$automation_bias, 0); near(x$dprime, 2*qnorm(80.5/101))
accept <- sdt_scores(mutate(fixture, n_agree = 100))
near(accept$dprime, 0); stopifnot(accept$automation_bias > 0)
reject <- sdt_scores(mutate(fixture, n_agree = 0))
near(reject$automation_bias, -accept$automation_bias)
stopifnot(inherits(try(sdt_scores(mutate(fixture, n_answered = 0)), silent = TRUE), "try-error"))
scores <- read("participant_sdt")
counts <- read_csv(file.path(base, "participant_reliance.csv"), show_col_types = FALSE)
for (i in seq_len(nrow(scores))) {
  s <- scores[i,]
  c <- filter(counts, participant_id == s$participant_id, pressure == s$pressure)
  stopifnot(nrow(c) == 2, all(c$pattern == s$pattern), all(c$reliability == s$reliability))
  p <- if (s$correction == "loglinear") (c$n_agree+.5)/(c$n_answered+1) else {
    rates <- c$n_agree/c$n_answered
    pmax(.5/c$n_answered, pmin(1-.5/c$n_answered, rates))
  }
  z <- setNames(qnorm(p), c$advice)
  near(s$automation_bias, mean(z)); near(s$dprime, unname(z["Correct"]-z["Incorrect"]))
}
tests <- read("contrasts")
stopifnot(nrow(tests) == 20)
for (i in seq_len(nrow(tests))) {
  t <- tests[i,]
  d <- filter(scores, correction == t$correction)
  d$v <- d[[t$outcome]]
  nm <- t$contrast
  # Direct Welch t.test calls, independently of the production weight engine.
  if (nm == "Reliability effect HP - LP") {
    sums <- d %>% group_by(participant_id, pattern) %>% summarise(v = sum(v), .groups = "drop")
    a <- filter(sums, pattern == "HP95_LP65")$v
    b <- filter(sums, pattern == "HP65_LP95")$v
  } else if (grepl("95% - 65%", nm, fixed = TRUE)) {
    p <- substr(nm, 1, 2)
    a <- filter(d, pressure == p, reliability == "95%")$v
    b <- filter(d, pressure == p, reliability == "65%")$v
  } else {
    r <- substr(nm, 1, 3)
    a <- filter(d, reliability == r, pressure == "HP")$v
    b <- filter(d, reliability == r, pressure == "LP")$v
  }
  expected <- t.test(a, b)
  near(t$estimate, mean(a)-mean(b)); near(t$p_raw, expected$p.value)
  near(t$lower, expected$conf.int[1]); near(t$upper, expected$conf.int[2])
  near(t$df, unname(expected$parameter))
}
for (d in split(tests, interaction(tests$outcome, tests$correction))) near(d$p_holm, p.adjust(d$p_raw, "holm"))
for (d in split(tests, tests$correction)) near(d$p_holm_both_outcomes, p.adjust(d$p_raw, "holm"))
catalog <- read_csv(file.path(plots, "figure_catalog.csv"), show_col_types = FALSE)
stopifnot(nrow(catalog) == 2, identical(catalog$hypothesis, c("H5","H9")), all(catalog$version == "Alternate"))
for (i in seq_len(nrow(catalog))) {
  stem <- catalog$filename[i]; outcome <- catalog$outcome[i]
  d <- read_csv(file.path(plots, "data", paste0(stem, "_data.csv")), show_col_types = FALSE)
  for (j in seq_len(nrow(d))) {
    s <- filter(scores, correction == "loglinear", pressure == d$pressure[j], reliability == d$reliability[j])
    expected <- t.test(s[[outcome]])
    near(d$value[j], unname(expected$estimate)); near(d$lower[j], expected$conf.int[1]); near(d$upper[j], expected$conf.int[2])
    stopifnot(nrow(s) == d$n[j])
  }
  a <- read_csv(file.path(plots, "data", paste0(stem, "_annotations.csv")), show_col_types = FALSE)
  expected <- filter(tests, correction == "loglinear", .data$outcome == .env$outcome)
  stopifnot(identical(a$contrast, expected$contrast), all(a$symbol == ifelse(expected$p_holm < .05, "*", "ns")))
  near(a$p_holm, expected$p_holm)
}
for (path in c(file.path(out,"input_checksums.csv"), file.path(plots,"data/input_checksums.csv"))) {
  m <- read_csv(path, show_col_types = FALSE)
  stopifnot(identical(unname(tools::md5sum(m$path)),m$md5))
}
m <- read_csv(file.path(plots,"data/artifact_manifest.csv"), show_col_types = FALSE)
stopifnot(nrow(m) == 5, identical(unname(tools::md5sum(file.path(plots,m$file))),m$md5))
message("Passed: SDT sign/correction, saved counts, independent Welch contrasts including paired interaction, Holm families, plotted CIs, annotations and provenance.")
