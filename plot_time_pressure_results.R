# Figures from the validated behavioural analysis, PDF and 300-dpi PNG.
# Usage: Rscript plot_time_pressure_results.R [analysis_dir] [plot_dir]
font_cache <- file.path(tempdir(), "fontconfig-cache")
dir.create(font_cache, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(XDG_CACHE_HOME = font_cache)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
})
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 2)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
root <- dirname(normalizePath(script))
input <- if (length(args) >= 1) args[1] else file.path(root, "analysis_outputs/semester2_2026_behavioural")
out <- if (length(args) >= 2) args[2] else file.path(root, "plots/semester2_2026_behavioural")
stopifnot(file.exists(file.path(input, "COMPLETE.txt")))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out, "data"), showWarnings = FALSE)
manifest <- read_csv(file.path(input, "input_manifest.csv"), show_col_types = FALSE)
stopifnot(all(file.exists(manifest$path)), identical(unname(tools::md5sum(manifest$path)), manifest$md5))
patterns <- c("HP95_LP65", "HP65_LP95")
pattern_names <- c("HP95_LP65" = "95% HP / 65% LP", "HP65_LP95" = "65% HP / 95% LP")
colours <- c("Manual" = "#727B88", "65% aid" = "#C56924", "95% aid" = "#1976A3")
read <- function(name) read_csv(file.path(input, paste0(name, ".csv")), show_col_types = FALSE)
participant <- read("participant_performance")
means <- read("descriptive_performance")
reliance <- read("participant_reliance")
reliance_means <- read("descriptive_reliance")
trust <- read("participant_trust")
trust_means <- read("descriptive_trust")
counts <- read("cohort_counts")
N <- sum(counts$n_participants)
labels <- setNames(paste0(pattern_names[patterns], "  (n = ", counts$n_participants[match(patterns, counts$pattern)], ")"), patterns)
theme_findings <- function() theme_minimal(base_size = 12.5, base_family = "Helvetica") + theme(
  text = element_text(colour = "#1C2D3E"),
  plot.title = element_text(size = 17, face = "bold", margin = margin(b = 7)),
  plot.subtitle = element_text(size = 11.5, margin = margin(b = 12), lineheight = 1.15),
  plot.title.position = "plot", plot.caption.position = "plot",
  plot.caption = element_text(size = 9, colour = "#566372", hjust = 0, lineheight = 1.15, margin = margin(t = 10)),
  panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(colour = "#E4E9ED", linewidth = .35),
  axis.title.x = element_blank(), axis.text = element_text(colour = "#1C2D3E", size = 11),
  strip.text = element_text(size = 12, face = "bold"), panel.spacing = grid::unit(1.5, "lines"),
  legend.position = "bottom", legend.title = element_blank(),
  plot.margin = margin(12, 16, 10, 12), plot.background = element_rect(fill = "white", colour = NA))
position_data <- function(d) d %>% mutate(
  pattern = factor(pattern, patterns),
  x = ifelse(pressure == "HP", 1, 4) + ifelse(mode == "Automation", 1, 0),
  colour = ifelse(mode == "Manual", "Manual", ifelse(
    (pattern == "HP95_LP65" & pressure == "HP") | (pattern == "HP65_LP95" & pressure == "LP"), "95% aid", "65% aid")))
offset <- function(ids) (((match(ids, sort(unique(ids))) * 37L) %% 71L) / 70 - .5) * .18
caption <- paste0(N, " participants. Faint points/lines: paired participants. Large points: equal-weight participant means; whiskers: pointwise 95% t intervals.")
plotted <- list()

performance_plot <- function(value, title, ytitle, percent = FALSE) {
  mult <- if (percent) 100 else 1
  d <- position_data(participant) %>% mutate(value = .data[[value]] * mult, point_x = x + offset(participant_id))
  s <- position_data(filter(means, outcome == value)) %>% mutate(mean = mean * mult, lower = lower * mult, upper = upper * mult)
  span <- diff(range(c(d$value, s$lower, s$upper), na.rm = TRUE))
  s <- s %>% mutate(label_y = upper + span * .07,
                    label = if (percent) sprintf("%.1f%%", mean) else sprintf("%.3f s", mean))
  plotted[[value]] <<- s
  # Keep negative endpoints of approximate timeout intervals visible rather than silently clipping them.
  lo <- if (value == "accuracy") max(0, floor(min(d$value) / 10) * 10 - 2) else min(0, min(s$lower))
  hi <- max(c(d$value, s$label_y), na.rm = TRUE) + span * .07
  ggplot(d, aes(point_x, value)) +
    geom_line(aes(group = interaction(participant_id, pressure)), colour = "#87929D", alpha = .19, linewidth = .3) +
    geom_point(aes(colour = colour), size = 1.2, alpha = .35) +
    geom_errorbar(data = s, aes(x = x, ymin = lower, ymax = upper, colour = colour), inherit.aes = FALSE, width = .12, linewidth = .85) +
    geom_point(data = s, aes(x, mean, fill = colour), inherit.aes = FALSE, shape = 21, colour = "white", size = 4.5, stroke = 1) +
    geom_label(data = s, aes(x, label_y, label = label, colour = colour), inherit.aes = FALSE, linewidth = 0,
               fill = "white", size = 3.6, fontface = "bold", label.padding = grid::unit(.1, "lines")) +
    facet_wrap(~pattern, labeller = as_labeller(labels)) +
    scale_x_continuous(breaks = c(1, 2, 4, 5), labels = c("Manual\nHP", "Aided\nHP", "Manual\nLP", "Aided\nLP"), expand = expansion(add = .35)) +
    scale_colour_manual(values = colours, guide = "none") + scale_fill_manual(values = colours, name = NULL) +
    scale_y_continuous(labels = if (percent) function(x) paste0(x, "%") else scales::label_number(accuracy = .1)) +
    coord_cartesian(ylim = c(lo, hi)) + theme_findings() + labs(title = title,
      subtitle = if (value == "mean_correct_rt") "Arithmetic mean RT on correct trials; the inferential model uses log RT."
      else if (value == "accuracy") "All experimental trials, including timeouts as incorrect. HP = 1 s; LP = 3 s."
      else "Missed deadlines as a percentage of all trials. HP = 1 s; LP = 3 s.", y = ytitle, caption = caption)
}

# A single visual language: colour denotes reliability, shape denotes method.
method_levels <- c("Random-intercept model", "Participant sensitivity")
method_shapes <- c("Random-intercept model" = 5, "Participant sensitivity" = 16)
aid_colours <- colours[c("65% aid", "95% aid")]
pressure_labels <- c(HP = "High pressure (1 s)", LP = "Low pressure (3 s)")
neutral <- "#334858"
all_sensitivity <- read("participant_sensitivity_contrasts")
all_model <- read("planned_contrasts")
all_effects <- bind_rows(all_model %>% mutate(method = method_levels[1]),
                        all_sensitivity %>% mutate(method = method_levels[2])) %>%
  mutate(method = factor(method, method_levels))
with_pattern <- function(d) d %>% mutate(
  pattern = factor(sub(" .*", "", contrast), patterns))
with_reliability <- function(d) d %>% mutate(
  reliability = factor(ifelse((pattern == patterns[1] & pressure == "HP") |
    (pattern == patterns[2] & pressure == "LP"), "95% aid", "65% aid"), names(aid_colours)))
benefit <- function(d) d %>% mutate(rt_reduction_percent = 100 * (1-ratio),
  benefit_lower = 100 * (1-ratio_upper), benefit_upper = 100 * (1-ratio_lower))
export <- function(d, stem) write_csv(d, file.path(out, "data", paste0(stem, ".csv")))
method_scale <- function() scale_shape_manual(values = method_shapes, drop = FALSE)
effect_caption <- "Open diamonds: mixed model. Filled circles: participant sensitivity. Whiskers: pointwise 95% CIs.\nTests use Holm adjustment within outcome; displayed CIs are unadjusted."
mean_caption <- "Large points: equal-weight participant means; whiskers: pointwise 95% t intervals."

# Advice benefits: pressure defines the columns; reliability is explicit on x.
gain_data <- all_effects %>% filter(outcome == "accuracy", grepl("Automation - Manual$", contrast)) %>%
  with_pattern() %>% mutate(pressure = factor(ifelse(grepl(" HP ", contrast), "HP", "LP"), c("HP", "LP"))) %>% with_reliability()
rt_benefit_data <- all_effects %>% filter(outcome == "correct_rt", grepl("Automation - Manual$", contrast)) %>%
  with_pattern() %>% mutate(pressure = factor(ifelse(grepl(" HP ", contrast), "HP", "LP"), c("HP", "LP"))) %>% with_reliability() %>% benefit()
benefit_plot <- function(d, title, subtitle, rt = FALSE) {
  d <- d %>% mutate(value = if (rt) rt_reduction_percent else estimate,
                     lo = if (rt) benefit_lower else lower, hi = if (rt) benefit_upper else upper)
  ggplot(d, aes(reliability, value, colour = reliability, shape = method, group = method)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#87929D") +
    geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(width = .25), width = .09, linewidth = .8) +
    geom_point(position = position_dodge(width = .25), size = 3.8, stroke = 1) +
    facet_wrap(~pressure, labeller = as_labeller(pressure_labels)) +
    scale_colour_manual(values = aid_colours, guide = "none") + method_scale() +
    scale_y_continuous(breaks = scales::breaks_width(if (rt) 5 else 4)) + theme_findings() +
    labs(title = title, subtitle = subtitle, y = if (rt) "Reduction in correct RT (%)" else "Accuracy gain (percentage points)",
      caption = paste(effect_caption, if (rt)
        "Benefit = 100 x (1 - aided/manual geometric-mean RT ratio), from log-RT contrasts."
        else "Automation minus the same participants' manual accuracy at the same pressure.", sep = "\n"))
}
gain_plot <- benefit_plot(gain_data, "95% advice improves accuracy at both deadlines",
  "Positive values indicate higher accuracy with the aid; manual comparisons stay within participants.")
rt_benefit_plot <- benefit_plot(rt_benefit_data, "Speed benefits depend on pressure and reliability",
  "Positive = faster with the aid; negative = slower. Correct responses only.", TRUE)

# Manual pressure costs: means and contrasts keep the allocation groups separate.
manual_means <- means %>% filter(mode == "Manual", outcome %in% c("accuracy", "mean_correct_rt")) %>%
  mutate(pattern = factor(pattern, patterns), pressure = factor(pressure, c("HP", "LP")),
    across(c(mean, lower, upper), ~.x * ifelse(outcome == "accuracy", 100, 1)))
manual_effects <- all_effects %>% filter(outcome %in% c("accuracy", "correct_rt"),
    grepl("Manual HP - Manual LP$", contrast)) %>% with_pattern() %>% benefit() %>%
  mutate(display_estimate = ifelse(outcome == "accuracy", estimate, rt_reduction_percent),
    display_lower = ifelse(outcome == "accuracy", lower, benefit_lower),
    display_upper = ifelse(outcome == "accuracy", upper, benefit_upper))
manual_mean_plot <- function(which) {
  d <- filter(manual_means, outcome == which)
  ggplot(d, aes(pressure, mean, group = 1)) + geom_line(colour = colours[["Manual"]], linewidth = .7) +
    geom_errorbar(aes(ymin = lower, ymax = upper), colour = colours[["Manual"]], width = .08, linewidth = .8) +
    geom_point(colour = colours[["Manual"]], size = 3.6) +
    facet_wrap(~pattern, labeller = as_labeller(labels)) + theme_findings() +
    scale_x_discrete(labels = c(HP = "HP (1 s)", LP = "LP (3 s)")) +
    scale_y_continuous(breaks = scales::breaks_width(if (which == "accuracy") 2 else .05),
      labels = scales::label_number(accuracy = if (which == "accuracy") 1 else .01)) +
    labs(title = if (which == "accuracy") "Manual accuracy" else "Manual correct-response time",
      subtitle = "Participant means within each allocation group.",
      y = if (which == "accuracy") "Accuracy (%)" else "Arithmetic mean correct RT (s)")
}
manual_effect_plot <- function(which) {
  d <- filter(manual_effects, outcome == which)
  ggplot(d, aes(pattern, display_estimate, shape = method, group = method)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#87929D") +
    geom_errorbar(aes(ymin = display_lower, ymax = display_upper), colour = neutral,
      position = position_dodge(width = .25), width = .08, linewidth = .8) +
    geom_point(colour = neutral, size = 3.6, stroke = 1, position = position_dodge(width = .25)) +
    scale_x_discrete(labels = c(HP95_LP65 = "95% HP / 65% LP", HP65_LP95 = "65% HP / 95% LP")) +
    method_scale() + theme_findings() +
    labs(title = if (which == "accuracy") "Accuracy cost of high pressure" else "RT reduction under high pressure",
      subtitle = if (which == "accuracy") "Manual HP minus manual LP; below zero = lower accuracy."
        else "Manual HP relative to LP; above zero = shorter correct RT.",
      y = if (which == "accuracy") "Accuracy difference (percentage points)" else "Reduction in correct RT (%)")
}

# Compensation compares the SAME participants at manual LP and aided HP.
compensation_means <- means %>% filter(outcome == "accuracy", (mode == "Manual" & pressure == "LP") |
    (mode == "Automation" & pressure == "HP")) %>%
  mutate(pattern = factor(pattern, rev(patterns)),
    condition = factor(ifelse(mode == "Manual", "Manual LP", "Aided HP"), c("Manual LP", "Aided HP")),
    colour = ifelse(mode == "Manual", "Manual", ifelse(pattern == patterns[1], "95% aid", "65% aid")),
    across(c(mean, lower, upper), ~.x * 100))
gap_data <- all_effects %>% filter(outcome == "accuracy", grepl("Aided HP - Manual LP$", contrast)) %>%
  with_pattern() %>% mutate(reliability = factor(ifelse(pattern == patterns[1], "95% aid", "65% aid"), names(aid_colours)))
compensation_mean_plot <- ggplot(compensation_means, aes(condition, mean, group = pattern)) +
  geom_line(colour = "#A6AFB7", linewidth = .7) +
  geom_errorbar(aes(ymin = lower, ymax = upper, colour = colour), width = .08, linewidth = .8) +
  geom_point(aes(colour = colour), size = 4) +
  geom_text(aes(y = upper, label = sprintf("%.1f%%", mean), colour = colour), vjust = -1, size = 4, fontface = "bold") +
  facet_wrap(~pattern, labeller = as_labeller(labels)) +
  scale_colour_manual(values = colours, guide = "none") + scale_y_continuous(expand = expansion(mult = c(.1, .25))) +
  theme_findings() + labs(title = "Actual accuracy at the two deadlines",
    subtitle = "Same participants: manual at 3 s versus aided at 1 s.", y = "Accuracy (%)")
gap_labels <- gap_data %>% filter(method == method_levels[2]) %>%
  mutate(label = sprintf("%+.1f pp", estimate), label_y = ifelse(estimate >= 0, upper + 1.2, lower - 1.2))
gap_plot <- ggplot(gap_data, aes(reliability, estimate, colour = reliability, shape = method, group = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#87929D") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = .25), width = .08, linewidth = .8) +
  geom_point(position = position_dodge(width = .25), size = 3.8, stroke = 1) +
  geom_text(data = gap_labels, aes(x = reliability, y = label_y, label = label, colour = reliability),
    inherit.aes = FALSE, size = 4.3, fontface = "bold") +
  scale_x_discrete(labels = c("65% aid" = "65% advice in HP", "95% aid" = "95% advice in HP")) +
  scale_colour_manual(values = aid_colours, guide = "none") + method_scale() + theme_findings() +
  labs(title = "Does aided HP exceed manual LP?", subtitle = "Above zero = compensation; labels show participant estimates.",
    y = "Aided HP minus manual LP (percentage points)",
    caption = paste(effect_caption, "Each contrast uses the same participants. A nonsignificant difference would not establish equivalence.", sep = "\n"))

# Reliance and trust: clean cell means above direct reliability contrasts.
reliance_summary <- reliance_means %>% mutate(pressure = factor(pressure, c("HP", "LP")),
  reliability = factor(paste(reliability, "aid"), names(aid_colours)), across(c(mean, lower, upper), ~.x * 100))
trust_summary <- trust_means %>% mutate(pressure = factor(pressure, c("HP", "LP")),
  reliability = factor(paste(reliability, "aid"), names(aid_colours)))
aided_effects <- all_effects %>% filter(outcome %in% c("reliance", "trust"), grepl("(HP|LP) 95% - 65%$", contrast)) %>%
  mutate(pressure = factor(ifelse(grepl(" HP ", paste0(" ", contrast)), "HP", "LP"), c("HP", "LP")),
    panel = ifelse(outcome == "trust", "Trust", ifelse(grepl("^Incorrect", contrast), "Incorrect advice", "Correct advice")))
plotted$agreement <- reliance_summary
plotted$trust <- trust_summary
summary_plot <- function(d, title, trust = FALSE) {
  ggplot(d, aes(pressure, mean, colour = reliability, group = reliability)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = .3), width = .09, linewidth = .8) +
    geom_point(position = position_dodge(width = .3), size = 3.8) +
    scale_colour_manual(values = aid_colours, drop = FALSE) +
    scale_x_discrete(labels = c(HP = "HP (1 s)", LP = "LP (3 s)")) +
    scale_y_continuous(limits = if (trust) c(1, 5) else c(0, 100), breaks = if (trust) 1:5 else seq(0, 100, 25)) +
    theme_findings() + labs(title = title, subtitle = "Participant means and 95% CIs.",
      y = if (trust) "Trust (1-5)" else "Agreement (%)")
}
reliability_effect_plot <- function(panel_name) {
  d <- filter(aided_effects, panel == panel_name)
  agreement_max <- ceiling(max(aided_effects$upper[aided_effects$outcome == "reliance"])/10)*10
  agreement_min <- floor(min(0, aided_effects$lower[aided_effects$outcome == "reliance"])/10)*10
  ggplot(d, aes(pressure, estimate, shape = method, group = method)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#87929D") +
    geom_errorbar(aes(ymin = lower, ymax = upper), colour = neutral, width = .09,
      position = position_dodge(width = .25), linewidth = .8) +
    geom_point(colour = neutral, position = position_dodge(width = .25), size = 3.6, stroke = 1) +
    expand_limits(y = 0) +
    scale_y_continuous(limits = if (panel_name == "Trust") NULL else c(agreement_min, agreement_max),
                      breaks = scales::breaks_width(if (panel_name == "Trust") .5 else 10)) +
    scale_x_discrete(labels = c(HP = "HP (1 s)", LP = "LP (3 s)")) + method_scale() +
    theme_findings() + labs(title = "Effect of higher reliability", subtitle = "95% minus 65% advice at each pressure.",
      y = if (panel_name == "Trust") "Trust difference (points)" else "Agreement difference (pp)")
}
correct_summary_plot <- summary_plot(filter(reliance_summary, advice == "Correct"), "Agreement with correct advice")
incorrect_summary_plot <- summary_plot(filter(reliance_summary, advice == "Incorrect"), "Agreement with incorrect advice")
trust_summary_plot <- summary_plot(trust_summary, "Trust in the aid", TRUE)
correct_effect_plot <- reliability_effect_plot("Correct advice")
incorrect_effect_plot <- reliability_effect_plot("Incorrect advice")
trust_effect_plot <- reliability_effect_plot("Trust")

# Participant-level supporting figures retain the original detailed means.
accuracy_plot <- performance_plot("accuracy", "Accuracy under high and low time pressure", "Mean accuracy", TRUE)
rt_plot <- performance_plot("mean_correct_rt", "Speed on correct responses", "Mean correct RT (seconds)")
timeout_plot <- performance_plot("timeout_rate", "Deadline misses", "Timeouts", TRUE)
save <- function(plot, stem, width = 13, height = 7) {
  ggsave(file.path(out, paste0(stem, ".pdf")), plot, width = width, height = height, device = cairo_pdf, bg = "white")
  ggsave(file.path(out, paste0(stem, ".png")), plot, width = width, height = height, dpi = 300, bg = "white")
}
compact <- function(p) p + labs(caption = NULL) + theme(plot.title = element_text(size = 14),
  plot.subtitle = element_text(size = 10), legend.text = element_text(size = 10), strip.text = element_text(size = 10),
  axis.title.y = element_text(size = 10.5), axis.text = element_text(size = 10))
slide_theme <- theme(plot.title = element_text(size = 21, face = "bold", colour = "#1C2D3E"),
  plot.caption = element_text(size = 10, hjust = 0, colour = "#566372"), plot.margin = margin(14, 16, 14, 16))
compose <- function(p, title, caption) (p + plot_layout(guides = "collect") +
  plot_annotation(title = title, caption = caption, theme = slide_theme)) & theme(legend.position = "bottom")
manual_caption <- paste0(N, " participants; allocation groups remain separate. Left: participant means and pointwise 95% t CIs.\n",
  "Right: HP-versus-LP contrasts and pointwise 95% CIs. RT means are arithmetic; RT reductions come from log-RT ratios.")
manual_figure <- compose((compact(manual_mean_plot("accuracy")) | compact(manual_effect_plot("accuracy"))) /
  (compact(manual_mean_plot("mean_correct_rt")) | compact(manual_effect_plot("correct_rt"))),
  "High pressure reduces accuracy and shortens correct-response RT", manual_caption)
performance_figure <- compose((compact(gain_plot) + labs(title = "Accuracy gains over manual performance")) / compact(rt_benefit_plot),
  "95% advice improves accuracy at both deadlines",
  "Each benefit compares aided and manual trials from the same participants at the same deadline. Positive values favour the aid.\nPointwise 95% CIs; tests use Holm adjustment within outcome. RT benefits use geometric-mean ratios, not arithmetic-mean differences.")
compensation_figure <- compose(compact(compensation_mean_plot) | compact(gap_plot),
  "95% advice under high pressure exceeds manual low-pressure accuracy",
  "Left: participant means and pointwise 95% t CIs. Right: matched contrasts and pointwise 95% CIs; labels are participant estimates.\nManual LP is the baseline from the same allocation group. Above zero indicates higher aided-HP accuracy; tests use Holm adjustment within accuracy.")
reliance_figure <- compose((compact(correct_summary_plot) | compact(incorrect_summary_plot) | compact(trust_summary_plot)) /
  (compact(correct_effect_plot) | compact(incorrect_effect_plot) | compact(trust_effect_plot)),
  "Higher reliability increases trust and agreement - even with incorrect advice",
  "Top: equal-weight participant means and pointwise 95% t CIs. Bottom: 95%-minus-65% contrasts and pointwise 95% CIs.\nAgreement excludes timeouts; trust is the six-item mean. Reliability comparisons at fixed pressure are between groups; tests use Holm adjustment within outcome.")

save(accuracy_plot, "accuracy")
save(rt_plot, "correct_rt")
save(timeout_plot, "timeouts")
save(gain_plot, "accuracy_gains")
save(rt_benefit_plot, "rt_benefits")
save(gap_plot, "remaining_accuracy_gap", 10, 7)
save(compose(compact(correct_summary_plot) | compact(incorrect_summary_plot),
  "Agreement increases with reliability, including when advice is wrong",
  paste(mean_caption, "Agreement among answered trials; timeouts excluded. HP/LP cells at fixed reliability contain different participants.", sep = "\n")), "reliance", 13, 7)
save(trust_summary_plot + labs(title = "Higher reliability increases trust", caption = paste(mean_caption,
  "Six-item mean on the 1-5 scale. HP/LP cells at fixed reliability contain different participants.", sep = "\n")), "trust")
save(manual_figure, "manual_pressure_costs", 14, 10)
save(manual_figure, "slide_manual_pressure_costs", 16, 9)
save(performance_figure, "slide_1_performance", 16, 9)
save(compensation_figure, "slide_2_compensation", 16, 9)
save(reliance_figure, "slide_3_reliance_trust", 16, 9)
for (name in names(plotted)) export(plotted[[name]], paste0(name, "_plotted_means"))
export(gain_data, "accuracy_gain_estimates")
export(gap_data, "remaining_gap_estimates")
export(rt_benefit_data, "rt_benefit_estimates")
export(manual_means, "manual_pressure_means")
export(manual_effects, "manual_pressure_contrasts")
export(compensation_means, "compensation_means")
export(aided_effects, "reliability_effect_estimates")
# Participant-level supporting tables accompany the clean reliance/trust headlines.
export(reliance, "agreement_participant_values")
export(trust, "trust_participant_values")
captions <- c(
  "PRIMARY FIGURES: RECOMMENDED READING ORDER", "",
  "1. slide_manual_pressure_costs (manuscript-sized counterpart: manual_pressure_costs).",
  paste0("Manual performance under 1 s (HP) and 3 s (LP) deadlines in ", N, " participants. Allocation groups are shown separately (", paste(labels, collapse = "; "), "). Accuracy includes timeouts as incorrect. Left panels show equal-weight participant means with pointwise 95% t intervals. Right panels show matched HP-minus-LP accuracy contrasts and percentage RT reductions, with pointwise 95% intervals from mixed models and participant sensitivity analyses. RT means are arithmetic; RT reductions are 100 x (1 - exp(log HP/LP contrast)). Correct-RT comparisons condition on correct, valid responses under the respective deadlines."), "",
  "2. slide_1_performance (supporting panels: accuracy_gains and rt_benefits).",
  paste0("Automation benefits over the same participants' manual performance at the same deadline, arranged by pressure and aid reliability. Accuracy gains are percentage-point differences. Positive RT benefits indicate faster correct responses, calculated as 100 x (1 - aided/manual geometric-mean RT ratio). Whiskers are pointwise 95% confidence intervals, with RT interval endpoints reversed under the decreasing ratio transformation. Allocation groups: ", paste(labels, collapse = "; "), ". Reliability comparisons at fixed pressure are between allocation groups. Original condition means and participant detail remain in accuracy and correct_rt."), "",
  "3. slide_2_compensation (supporting contrast: remaining_accuracy_gap).",
  "Accuracy under aided HP is compared with manual LP in the same participants. Left panels show participant means and pointwise 95% t intervals; right panels show matched accuracy contrasts with pointwise 95% CIs. Positive differences indicate that aided HP exceeds manual LP. Numeric labels are participant-sensitivity point estimates, not mixed-model estimates. The manual LP baseline remains specific to each allocation group. A nonsignificant difference would not establish equivalence.", "",
  "4. slide_3_reliance_trust (supporting mean panels: reliance and trust).",
  "Agreement with correct advice, agreement with incorrect advice, and six-item mean trust. Upper panels show equal-weight participant means with pointwise 95% t intervals; lower panels show 95%-minus-65% reliability contrasts at each deadline, with pointwise 95% CIs. Agreement is conditional on responding and excludes timeouts. The task has no recorded pre-advice decision, so agreement is a behavioural proxy for reliance. Comparisons between reliabilities at fixed pressure, and between pressures at fixed reliability, involve different allocation groups; no connecting participant trajectories are drawn in these reorganised summaries.", "",
  "Across figures, blue denotes 95% reliability, orange 65%, and grey manual means. Reliability-difference and manual-pressure contrasts are neutral. Open diamonds show mixed-model estimates; filled circles show participant sensitivity estimates. The descriptive means are also participant summaries. Formal tests retain two-sided Holm adjustment within outcome; plotted intervals are pointwise and unadjusted. Model and participant estimates are shown together because model adequacy limitations remain. All fitted models and analyses were reused without refitting.")
writeLines(captions, file.path(out, "data", "primary_figure_captions.txt"))
inputs <- c(file.path(input, paste0(c("participant_performance", "descriptive_performance", "participant_reliance",
  "descriptive_reliance", "participant_trust", "descriptive_trust", "accuracy_contrasts", "correct_rt_contrasts",
  "planned_contrasts", "participant_sensitivity_contrasts", "cohort_counts", "input_manifest"), ".csv")), script)
write_csv(tibble(path = normalizePath(inputs), md5 = unname(tools::md5sum(inputs))), file.path(out, "data", "input_checksums.csv"))
artifacts <- sort(list.files(out, "\\.(pdf|png)$", full.names = TRUE))
write_csv(tibble(file = basename(artifacts), bytes = file.info(artifacts)$size, md5 = unname(tools::md5sum(artifacts))),
          file.path(out, "data", "artifact_manifest.csv"))
writeLines(capture.output(sessionInfo()), file.path(out, "data", "session_info.txt"))
message("Saved ", length(artifacts), " primary figure artifacts to ", out)
source(file.path(root, "plot_time_pressure_exploratory.R"))
plot_exploratory(input, out, root)
