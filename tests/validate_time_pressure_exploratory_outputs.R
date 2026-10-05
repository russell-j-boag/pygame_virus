# Run after the default extended analysis and plotting commands.
source("analyse_time_pressure.R")
source("analyse_time_pressure_exploratory.R")
base <- "analysis_outputs/semester2_2026_behavioural"
out <- file.path(base, "exploratory"); plots <- "plots/semester2_2026_behavioural/exploratory"
r <- function(name) read_csv(file.path(out, paste0(name, ".csv")), show_col_types = FALSE)
m <- r("input_manifest")
stopifnot(identical(unname(tools::md5sum(m$path)), m$md5))
raw <- bind_rows(lapply(m$path[m$kind == "trials"], read_csv, show_col_types = FALSE))
dat <- prepare_trials(raw); n <- n_distinct(dat$subject)
stopifnot(nrow(m) == 3*n)
b <- r("H2_participant_benchmark"); c <- r("H3_participant_calibration"); d <- r("H6_participant_decomposition")
s <- r("H1_participant_selectivity")
stopifnot(nrow(b) == 2*n, nrow(c) == 2*n, nrow(d) == 2*n, nrow(s) == 2*n,
  max(abs(b$net_gain - b$rescued + b$spoiled + b$missed_correct_advice)) < 1e-12,
  max(abs(d$total_gain - d$completion_component - d$choice_component)) < 1e-12,
  max(abs(c$signed_error - (c$perceived - 100*c$aid_accuracy))) < 1e-12,
  max(abs(c$absolute_error - abs(c$signed_error))) < 1e-12)
expected_b <- benchmark_components(dat) %>% select(participant_id, pressure, expected_gain = net_gain)
checked_b <- inner_join(b, expected_b, by = c("participant_id", "pressure"))
stopifnot(nrow(checked_b) == nrow(b), max(abs(checked_b$net_gain - checked_b$expected_gain)) < 1e-12)
seqdat <- make_sequence(dat)
coverage <- r("H5_sequence_coverage")
stopifnot(sum(coverage$n) == nrow(seqdat), !any(seqdat$timeout), !any(seqdat$previous_timeout),
          all(seqdat$previous_trial == seqdat$trial - 1))
for (prefix in c("H3_perceived", "H4_trust")) {
  cells <- r(paste0(prefix, "_participant_cells"))
  scores <- cells %>% distinct(participant_id, pressure, score_within, score_between)
  stopifnot(nrow(cells) == 4*n, max(abs((scores %>% group_by(participant_id) %>% summarise(v = sum(score_within)))$v)) < 1e-12)
  pred <- r(paste0(prefix, "_predictions"))
  stopifnot(all(pred$probability >= 0 & pred$probability <= 1),
            all(pred$lower <= pred$probability & pred$upper >= pred$probability))
}
x <- r("exploratory_contrasts")
stopifnot(nrow(x) == 172, setequal(x$hypothesis, paste0("H", 1:6)), !anyDuplicated(x[c("hypothesis", "outcome", "analysis", "contrast")]),
          all(is.finite(x$estimate)), all(x$lower <= x$estimate & x$upper >= x$estimate),
          all(x$p_holm >= x$p_raw, na.rm = TRUE), all(x$p_holm_all_six >= x$p_holm, na.rm = TRUE))
check_holm <- x %>% group_by(hypothesis, analysis_family) %>% mutate(expected = p.adjust(p_raw, "holm")) %>% ungroup()
stopifnot(isTRUE(all.equal(check_holm$p_holm, check_holm$expected)))
# Independent paired t-test checks of the benchmark and its CI scale.
one <- filter(b, pressure == "HP", reliability == "95%")
tt <- t.test(one$net_gain)
observed <- filter(x, hypothesis == "H2", analysis == "participant_sensitivity", contrast == "HP 95% versus zero")
stopifnot(abs(observed$estimate - tt$estimate * 100) < 1e-10,
          abs(observed$lower - tt$conf.int[1] * 100) < 1e-10)
for (name in c("source_checksums")) {h <- r(name); stopifnot(identical(unname(tools::md5sum(h$path)), h$md5))}
h <- read_csv(file.path(plots, "data/input_checksums.csv"), show_col_types = FALSE)
stopifnot(identical(unname(tools::md5sum(h$path)), h$md5))
h <- read_csv(file.path(plots, "data/artifact_manifest.csv"), show_col_types = FALSE)
stopifnot(nrow(h) == 20, identical(unname(tools::md5sum(file.path(plots, h$file))), h$md5))
for (h in c("H2", "H6")) {
  bars <- read_csv(file.path(plots, "data", paste0(h, "_components.csv")), show_col_types = FALSE) %>%
    group_by(pattern, pressure) %>% summarise(total = sum(estimate), .groups = "drop")
  net <- read_csv(file.path(plots, "data", paste0(h, "_net.csv")), show_col_types = FALSE)
  joined <- inner_join(bars, net, by = c("pattern", "pressure"))
  stopifnot(nrow(joined) == 4, max(abs(joined$total - joined$estimate)) < 1e-10)
}
receipt <- c(sprintf("Six-question integration validation passed for %d participants and %d source files.", n, nrow(m)),
  sprintf("%d model and sensitivity contrasts; %d next-trial observations.", nrow(x), nrow(seqdat)),
  "Passed: same-run sliders, exact accounting identities, lag boundaries, within/between rating centering, independent CI check, Holm families, plotted decomposition sums and all provenance hashes.",
  "All 20 exploratory figure artifacts match their manifest.")
writeLines(receipt, file.path(out, "validation.txt")); cat(paste(receipt, collapse = "\n"), "\n")
