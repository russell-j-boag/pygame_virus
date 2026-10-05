# Bespoke research-question figures, using the validated 60-participant cohort.
# Usage: Rscript plot_key_findings.R [output_dir]
# Default output: plots/semester2_2026_key_findings (PDF + 300-dpi PNG).
font_cache <- file.path(tempdir(), "fontconfig-cache")
dir.create(font_cache, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(XDG_CACHE_HOME = font_cache)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(tidyr)
})
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
ROOT <- dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 1L)
OUTPUT_DIR <- if (length(args)) args[[1]] else file.path(ROOT, "plots/semester2_2026_key_findings")
RESULTS <- file.path(ROOT, "analysis_outputs/semester2_2026_hypotheses")
DATA <- file.path(ROOT, "data")
COLOURS <- c("Manual" = "#727B88", "Aid-first" = "#1976A3", "Stimulus-first" = "#C56924")
REVISION_COLOURS <- c("Beneficial" = "#087F72", "Harmful" = "#BB503B")
CONDITIONS <- names(COLOURS)
ADVICE <- c("Correct advice", "Incorrect advice")
TEXT <- "#1C2D3E"

input_paths <- c(file.path(DATA, "data_virus_all.csv"),
                 file.path(DATA, "semester2_2026_averaged_within_participants.csv"),
                 file.path(RESULTS, "research_question_contrasts.csv"),
                 file.path(RESULTS, "primary_hypothesis_tests.csv"),
                 file.path(DATA, "collation_manifest.csv"))
stopifnot(all(file.exists(input_paths)))
recorded <- read_csv(file.path(RESULTS, "analysis_input_checksums.csv"), show_col_types = FALSE)
for (path in input_paths[c(1, 2, 5)]) {
  expected <- recorded$md5[basename(recorded$path) == basename(path)]
  stopifnot(length(expected) == 1L, unname(tools::md5sum(path)) == expected)
}
raw <- read_csv(input_paths[[1]], show_col_types = FALSE)
participants <- read_csv(input_paths[[2]], show_col_types = FALSE)
reported_contrasts <- read_csv(input_paths[[3]], show_col_types = FALSE)
reported_tests <- read_csv(input_paths[[4]], show_col_types = FALSE)
stopifnot(nrow(raw) == 46800L, n_distinct(raw$participant_id) == 60L,
          setequal(raw$participant_id, 1:60), nrow(participants) == 60L,
          setequal(participants$subject_no, 1:60),
          identical(unique(raw$run_timestamp[raw$participant_id == 59]), "20260924_110831"))
binary <- function(x) {
  z <- tolower(as.character(x))
  stopifnot(all(is.na(z) | z %in% c("true", "false", "1", "0")))
  ifelse(is.na(z), NA_integer_, as.integer(z %in% c("true", "1")))
}
trials <- raw %>% transmute(
  subject_no = participant_id,
  condition = factor(recode(aid_condition, manual = "Manual", aid_first = "Aid-first",
                            stimulus_first = "Stimulus-first"), CONDITIONS),
  advice_correct = binary(aid_correct), d1 = binary(decision1_correct),
  d2 = binary(decision2_correct), switched = binary(changed_response)
)
stopifnot(!anyNA(trials$d1), !anyNA(trials$d2), !anyNA(trials$switched),
          all(trials$switched == as.integer(trials$d1 != trials$d2)),
          all((trials %>% count(subject_no, condition))$n == 260L))
# Count binary transitions before replacing d1/d2 with their cell means.
cells <- bind_rows(
  trials %>% mutate(advice_subset = "Overall"),
  trials %>% filter(condition != "Manual") %>%
    mutate(advice_subset = if_else(advice_correct == 1, ADVICE[[1]], ADVICE[[2]]))
) %>% group_by(subject_no, condition, advice_subset) %>% summarise(
  n_trials = n(), beneficial = mean(d1 == 0 & d2 == 1),
  harmful = mean(d1 == 1 & d2 == 0), d1 = mean(d1), d2 = mean(d2),
  switched = mean(switched), .groups = "drop"
)
stopifnot(nrow(cells) == 420L, all(cells$n_trials > 0),
          max(abs(cells$switched - cells$beneficial - cells$harmful)) < 1e-12,
          max(abs(cells$d2 - cells$d1 - cells$beneficial + cells$harmful)) < 1e-12)

trust <- participants %>% select(subject_no, starts_with("trust_mean_")) %>%
  pivot_longer(-subject_no, names_to = "variable", values_to = "value") %>%
  mutate(condition = factor(if_else(variable == "trust_mean_aid_first_1to5",
                                   "Aid-first", "Stimulus-first"), CONDITIONS),
         advice_subset = "Overall", outcome = "Trust") %>%
  select(subject_no, condition, advice_subset, outcome, value)
accuracy <- cells %>% transmute(subject_no, condition, advice_subset,
                                outcome = "Final accuracy", value = d2 * 100)
plot_data <- bind_rows(accuracy, trust)

paired_contrast <- function(data, outcome_label, hypothesis) {
  wide <- data %>% filter(condition != "Manual") %>%
    select(subject_no, condition, value) %>%
    pivot_wider(names_from = condition, values_from = value)
  stopifnot(nrow(wide) == 60L, !anyNA(wide))
  difference <- wide[["Stimulus-first"]] - wide[["Aid-first"]]
  tt <- t.test(difference)
  tibble(outcome = outcome_label, hypothesis, n = length(difference),
         mean_aid_first = mean(wide[["Aid-first"]]),
         mean_stimulus_first = mean(wide[["Stimulus-first"]]),
         estimate = mean(difference), conf_low = tt$conf.int[[1]], conf_high = tt$conf.int[[2]])
}
effects <- bind_rows(
  paired_contrast(filter(accuracy, advice_subset == "Overall"), "Overall accuracy", "H5"),
  paired_contrast(filter(accuracy, advice_subset == ADVICE[[1]]), ADVICE[[1]], "H3"),
  paired_contrast(filter(accuracy, advice_subset == ADVICE[[2]]), ADVICE[[2]], "H4"),
  paired_contrast(cells %>% filter(advice_subset == "Overall") %>%
                    mutate(value = switched * 100), "Revision frequency", "H2"),
  paired_contrast(trust, "Trust", "Exploratory")
)
for (h in c("H2", "H3", "H4", "H5")) {
  expected <- reported_contrasts %>% filter(hypothesis == h,
                                           paired_contrast == "Stimulus first - Aid first")
  observed <- effects %>% filter(hypothesis == h)
  stopifnot(nrow(expected) == 1L,
            abs(observed$estimate - expected$difference_pp) < 1e-10,
            abs(observed$conf_low - expected$paired_conf_low_pp) < 1e-10,
            abs(observed$conf_high - expected$paired_conf_high_pp) < 1e-10)
}
expected_trust <- reported_tests %>% filter(hypothesis == "Exploratory", outcome == "Six-item mean trust rating")
stopifnot(nrow(expected_trust) == 1L,
          abs(effects$estimate[effects$hypothesis == "Exploratory"] - expected_trust$estimate) < 1e-10,
          abs(effects$conf_low[effects$hypothesis == "Exploratory"] - expected_trust$conf_low) < 1e-10,
          abs(effects$conf_high[effects$hypothesis == "Exploratory"] - expected_trust$conf_high) < 1e-10)
signed <- function(x) sprintf("%+.2f", x)
effect_label <- function(hypothesis, units = "pp") {
  r <- effects %>% filter(.data$hypothesis == .env$hypothesis)
  paste0("SF - AF: ", signed(r$estimate), " ", units, "  |  95% CI [",
         signed(r$conf_low), ", ", signed(r$conf_high), "]")
}
facet_label <- function(hypothesis) {
  r <- effects %>% filter(.data$hypothesis == .env$hypothesis)
  paste0(r$outcome, "\nSF - AF: ", signed(r$estimate), " pp\n95% CI [",
         signed(r$conf_low), ", ", signed(r$conf_high), "]")
}

theme_findings <- function() theme_minimal(base_size = 12.5, base_family = "Helvetica") + theme(
  text = element_text(colour = TEXT),
  plot.title = element_text(size = 16, face = "bold", margin = margin(b = 7)),
  plot.subtitle = element_text(size = 11.5, lineheight = 1.15, margin = margin(b = 12)),
  plot.title.position = "plot", plot.caption.position = "plot",
  plot.caption = element_text(size = 10, colour = "#566372", hjust = 0, lineheight = 1.2,
                              margin = margin(t = 13)),
  panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(colour = "#E4E9ED", linewidth = .35),
  axis.title.x = element_blank(), axis.title.y = element_text(size = 12, margin = margin(r = 9)),
  axis.text = element_text(size = 11.5, colour = TEXT),
  strip.text = element_text(size = 11.5, face = "bold", lineheight = 1.2, margin = margin(b = 12)),
  panel.spacing = grid::unit(1.4, "lines"),
  legend.position = "bottom", legend.title = element_blank(),
  legend.text = element_text(size = 10.5),
  plot.margin = margin(15, 18, 12, 12),
  plot.background = element_rect(fill = "white", colour = NA)
)
mean_ci <- function(data, groups) {
  data %>% group_by(across(all_of(groups))) %>% summarise(
    n = n(), estimate = mean(value), sem = sd(value) / sqrt(n()), .groups = "drop"
  ) %>% mutate(low = estimate - qt(.975, n - 1) * sem,
               high = estimate + qt(.975, n - 1) * sem)
}
prepare_points <- function(data, aided_only = FALSE) {
  levels <- if (aided_only) CONDITIONS[-1] else CONDITIONS
  data %>% mutate(x = match(as.character(condition), levels),
                  # Fixed subject offsets preserve pairing and are reproducible.
                  x_individual = x + (((subject_no * 37L) %% 61L) / 60 - .5) * .18)
}
paired_plot <- function(data, aided_only = FALSE, limits, breaks, ytitle, label_digits = 1) {
  d <- prepare_points(data, aided_only)
  means <- mean_ci(d, c("condition", "advice_subset", "x")) %>%
    mutate(label = paste0(formatC(estimate, format = "f", digits = label_digits),
                          if (ytitle == "Trust (1-5)") "" else "%"),
           label_y = high + if (ytitle == "Trust (1-5)") .23 else 4)
  ggplot(d, aes(x_individual, value)) +
    geom_line(aes(group = interaction(subject_no, advice_subset)), colour = "#87929D",
              alpha = .19, linewidth = .3) +
    geom_point(aes(colour = condition), size = 1.25, alpha = .3) +
    geom_errorbar(data = means, aes(x = x, ymin = low, ymax = high, colour = condition),
                  inherit.aes = FALSE, width = .12, linewidth = .9) +
    geom_point(data = means, aes(x, estimate, fill = condition), inherit.aes = FALSE,
               shape = 21, colour = "white", stroke = 1, size = 4.7) +
    geom_label(data = means, aes(x, label_y, label = label, colour = condition),
               inherit.aes = FALSE, fill = "white", linewidth = 0, size = 4,
               fontface = "bold", label.padding = grid::unit(.1, "lines")) +
    scale_colour_manual(values = COLOURS, guide = "none") +
    scale_fill_manual(values = COLOURS, guide = "none") +
    scale_x_continuous(breaks = seq_len(if (aided_only) 2 else 3),
                       labels = if (aided_only) CONDITIONS[-1] else CONDITIONS,
                       expand = expansion(add = .4)) +
    scale_y_continuous(breaks = breaks, expand = expansion(mult = 0), labels = if (ytitle == "Trust (1-5)") scales::label_number(accuracy = 1)
                       else function(x) paste0(x, "%")) +
    coord_cartesian(ylim = limits) +
    labs(y = ytitle) + theme_findings()
}

panel_a <- paired_plot(filter(accuracy, advice_subset == "Overall"),
                       limits = c(50, 105), breaks = seq(50, 100, 10), ytitle = "Final accuracy") +
  labs(title = "A  Overall final accuracy", subtitle = effect_label("H5"))
panel_b <- paired_plot(filter(accuracy, advice_subset != "Overall"), aided_only = TRUE,
                       limits = c(0, 108), breaks = seq(0, 100, 20), ytitle = "Final accuracy") +
  facet_wrap(~advice_subset, labeller = as_labeller(setNames(c(facet_label("H3"), facet_label("H4")), ADVICE))) +
  labs(title = "B  Accuracy by advice correctness", subtitle = "Correct advice yields similar final accuracy; incorrect advice separates the conditions.")

revision_means <- cells %>% filter(advice_subset == "Overall") %>% group_by(condition) %>%
  summarise(beneficial = mean(beneficial) * 100, harmful = mean(harmful) * 100,
            total = mean(switched) * 100, .groups = "drop") %>% mutate(x = as.numeric(condition))
revision_segments <- bind_rows(
  revision_means %>% transmute(condition, x, type = "Beneficial", ymin = 0, ymax = beneficial, value = beneficial),
  revision_means %>% transmute(condition, x, type = "Harmful", ymin = beneficial,
                               ymax = beneficial + harmful, value = harmful)
) %>% mutate(type = factor(type, names(REVISION_COLOURS)), y = (ymin + ymax) / 2)
panel_c <- ggplot() +
  geom_rect(data = revision_segments, aes(xmin = x - .26, xmax = x + .26,
                                         ymin = ymin, ymax = ymax, fill = type)) +
  geom_text(data = revision_segments, aes(x, y, label = sprintf("%.1f", value)),
            colour = "white", size = 4, fontface = "bold") +
  geom_text(data = revision_means, aes(x, total + 1, label = sprintf("%.1f%%", total)),
            colour = TEXT, size = 4.4, fontface = "bold") +
  scale_fill_manual(values = REVISION_COLOURS,
                     labels = c("Beneficial: incorrect to correct", "Harmful: correct to incorrect")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_continuous(breaks = 1:3, labels = CONDITIONS, expand = expansion(add = .45)) +
  scale_y_continuous(breaks = seq(0, 20, 5), labels = function(x) paste0(x, "%"), expand = expansion(mult = 0)) +
  coord_cartesian(ylim = c(0, 20)) + theme_findings() +
  labs(title = "C  Beneficial and harmful revisions", subtitle = effect_label("H2"),
       y = "Revisions (% of all trials in condition)")
panel_d <- paired_plot(trust, aided_only = TRUE, limits = c(.85, 5.15), breaks = 1:5,
                       ytitle = "Trust (1-5)", label_digits = 2) +
  labs(title = "D  Trust was lower after an initial judgment", subtitle = effect_label("Exploratory", "points"))

COMMON_NOTE <- paste0(
  "60 participants, including replacement p59. SF = Stimulus-first; AF = Aid-first. All 95% intervals are pointwise.\n",
  "Faint dots/lines show paired participants; large dots and whiskers show means and their t intervals. Timing contrasts use paired t intervals.")
SUMMARY_NOTE <- paste0(COMMON_NOTE,
  "\nRevision segments use all trials in each condition; trust is the six-item mean (exploratory). Advice precedes Decision 1 in Aid-first.")
annotation_theme <- theme(plot.title = element_text(size = 23, face = "bold", colour = TEXT),
                          plot.subtitle = element_text(size = 13, colour = "#566372", margin = margin(b = 12)),
                          plot.caption = element_text(size = 10.5, hjust = 0, colour = "#566372", lineheight = 1.2),
                          plot.margin = margin(22, 22, 17, 22),
                          plot.background = element_rect(fill = "white", colour = NA))
summary_plot <- ((panel_a | panel_b) / (panel_c | panel_d)) +
  plot_annotation(title = "More revisions did not produce better final decisions",
                  subtitle = "Advice timing affected revision frequency, incorrect-advice accuracy and trust.",
                  caption = SUMMARY_NOTE, theme = annotation_theme)
slide_1 <- (panel_a | panel_b) + plot_layout(widths = c(1, 1.25)) +
  plot_annotation(title = "No overall accuracy benefit; poorer decisions with incorrect advice",
                  subtitle = "Both aided conditions outperformed Manual, but judging independently first did not improve final accuracy.",
                  caption = COMMON_NOTE, theme = annotation_theme)
slide_2 <- (panel_c | panel_d) +
  plot_annotation(title = "More revisions and lower trust did not protect against incorrect advice",
                  subtitle = "Revisions were more frequent in Stimulus-first, while self-reported trust was lower.",
                  caption = SUMMARY_NOTE, theme = annotation_theme)

trajectory_data <- cells %>% filter(advice_subset != "Overall") %>%
  pivot_longer(c(d1, d2), names_to = "decision", values_to = "value") %>%
  mutate(value = value * 100, decision = factor(decision, c("d1", "d2"), c("Decision 1", "Decision 2")))
trajectory_means <- mean_ci(trajectory_data, c("condition", "advice_subset", "decision")) %>%
  mutate(x = as.numeric(decision),
         label_y = if_else(condition == "Aid-first", high + 2, low - 2),
         label_y = if_else(condition == "Aid-first" & advice_subset == "Incorrect advice" &
                             decision == "Decision 1", low - 2, label_y))
trajectory_gains <- cells %>% filter(advice_subset != "Overall") %>%
  group_by(condition, advice_subset) %>% summarise(gain_pp = mean(d2 - d1) * 100, .groups = "drop")
trajectory <- ggplot(trajectory_means, aes(x, estimate, colour = condition, group = condition)) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin = low, ymax = high), width = .05, linewidth = .7) +
  geom_point(aes(shape = condition), size = 3.2) +
  geom_label(aes(y = label_y, label = sprintf("%.1f%%", estimate)),
             fill = "white", linewidth = 0, size = 3.8, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~advice_subset) + scale_colour_manual(values = COLOURS[-1]) +
  scale_shape_manual(values = c("Aid-first" = 16, "Stimulus-first" = 17)) +
  scale_x_continuous(breaks = 1:2, labels = c("Decision 1", "Decision 2"), expand = expansion(add = .17)) +
  scale_y_continuous(breaks = seq(50, 100, 10), labels = function(x) paste0(x, "%"), expand = expansion(mult = 0)) +
  coord_cartesian(ylim = c(50, 100)) + theme_findings() +
  labs(title = "In Stimulus-first, accuracy gains depended on advice correctness",
       subtitle = "Stimulus-first accuracy rose after correct advice and fell after incorrect advice.",
       y = "Decision accuracy", caption = paste0(
         "60 participants. Points show participant-mean accuracy; whiskers are pointwise 95% t intervals.\n",
         "Advice was already seen before Decision 1 in Aid-first; it arrived between decisions in Stimulus-first.\n",
         "The starting points represent different information states. These gains are descriptive, not comparable total advice effects."))

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_DIR, "data"), showWarnings = FALSE)
save_figure <- function(plot, name, width, height) {
  ggsave(file.path(OUTPUT_DIR, paste0(name, ".pdf")), plot, width = width, height = height,
         device = cairo_pdf, bg = "white")
  ggsave(file.path(OUTPUT_DIR, paste0(name, ".png")), plot, width = width, height = height,
         dpi = 300, bg = "white")
}
save_figure(summary_plot, "key_findings_four_panel", 16, 12)
save_figure(slide_1, "slide_1_final_accuracy", 16, 9)
save_figure(slide_2, "slide_2_revisions_and_trust", 16, 9)
save_figure(panel_a + labs(caption = COMMON_NOTE), "panel_a_overall_accuracy", 9, 6.5)
save_figure(panel_b + labs(caption = COMMON_NOTE), "panel_b_accuracy_by_advice", 11, 6.5)
save_figure(panel_c + labs(caption = paste0(
  "60 participants. Each segment is the participant-mean percentage of all trials within its condition.\n",
  "Totals = beneficial + harmful revisions. SF - AF is the total revision difference with a paired 95% t interval.")),
  "panel_c_revision_breakdown", 9, 6.5)
save_figure(panel_d + labs(caption = paste0(COMMON_NOTE, "\nTrust is the six-item mean; this comparison is exploratory.")),
            "panel_d_trust", 9, 6.5)
save_figure(trajectory, "decision1_to_decision2_by_advice", 13.33, 7.5)

write_csv(cells, file.path(OUTPUT_DIR, "data/participant_accuracy_and_revisions.csv"))
write_csv(trust, file.path(OUTPUT_DIR, "data/participant_trust.csv"))
write_csv(effects, file.path(OUTPUT_DIR, "data/paired_timing_contrasts.csv"))
write_csv(mean_ci(plot_data, c("outcome", "condition", "advice_subset")),
          file.path(OUTPUT_DIR, "data/plotted_means_and_intervals.csv"))
write_csv(revision_means, file.path(OUTPUT_DIR, "data/revision_segments.csv"))
write_csv(trajectory_means, file.path(OUTPUT_DIR, "data/decision_trajectory_means.csv"))
write_csv(trajectory_gains, file.path(OUTPUT_DIR, "data/decision_trajectory_gains.csv"))
all_input_paths <- c(input_paths, file.path(ROOT, "plot_key_findings.R"))
write_csv(tibble(path = normalizePath(all_input_paths), md5 = unname(tools::md5sum(all_input_paths))),
          file.path(OUTPUT_DIR, "data/input_checksums.csv"))
writeLines(capture.output(sessionInfo()), file.path(OUTPUT_DIR, "data/session_info.txt"))
writeLines(c(
  "ADVICE TIMING: KEY FINDINGS FIGURE SET", "",
  "Eight figures, each provided as vector PDF and 300-dpi PNG:",
  "key_findings_four_panel: combined A-D figure (16 x 12 inches).",
  "slide_1_final_accuracy: A+B, 16:9 layout.",
  "slide_2_revisions_and_trust: C+D, 16:9 layout.",
  "panel_a_overall_accuracy, panel_b_accuracy_by_advice, panel_c_revision_breakdown, panel_d_trust: separate panels.",
  "decision1_to_decision2_by_advice: explanatory trajectories, approximately 16:9.", "",
  "COHORT: 60 participants. Excluded only old p59 run 20260903_120309; included replacement 20260924_110831.",
  "All means weight participants equally. Accuracy/revision effects are percentage points; trust effects are scale points.",
  "Mean whiskers are pointwise 95% t intervals across participants (not normalized within-subject intervals).",
  "Annotated SF-AF differences use paired 95% t intervals and match the existing sensitivity tables.",
  "Do not use overlap of mean whiskers to infer the paired timing test. Intervals are not multiplicity-adjusted.",
  "Primary inference remains the H1-H5 mixed models and Holm tests in the analysis report; no new inferential tests were added.",
  "Revision segments share an all-trials denominator, including non-revised trials. Segment numbers are percentages.",
  "No artificial advice-correctness cells are assigned to Manual. Trust has no Manual comparator.",
  "Trajectory gains start from different information states because Aid-first Decision 1 already follows advice exposure.",
  "Point plots use labelled, nonzero accuracy-axis minima where appropriate; no observations are clipped.", "",
  "REPRODUCE from the repository root: Rscript plot_key_findings.R",
  "Data behind the figures and source checksums are in data/. Existing plots are not overwritten."
), file.path(OUTPUT_DIR, "README.txt"))
cat("Validated 60 participants and paired effects against the analysis tables.\n")
cat("Wrote 8 PDF/PNG figure pairs to", OUTPUT_DIR, "\n")
