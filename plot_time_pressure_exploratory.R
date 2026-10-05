# Standalone: Rscript plot_time_pressure_exploratory.R [base_analysis_dir] [base_plot_dir]
plot_exploratory <- function(base, plot_base, root) {
  suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr); library(ggplot2); library(patchwork)})
  cache <- file.path(tempdir(), "fontconfig-cache"); dir.create(cache, showWarnings = FALSE)
  Sys.setenv(XDG_CACHE_HOME = cache)
  input <- file.path(base, "exploratory"); out <- file.path(plot_base, "exploratory")
  stopifnot(file.exists(file.path(input, "COMPLETE.txt")))
  dir.create(file.path(out, "data"), recursive = TRUE, showWarnings = FALSE)
  manifest <- read_csv(file.path(input, "input_manifest.csv"), show_col_types = FALSE)
  stopifnot(identical(unname(tools::md5sum(manifest$path)), manifest$md5))
  consumed <- character()
  read <- function(name) {
    path <- file.path(input, paste0(name, ".csv")); consumed <<- c(consumed, path)
    read_csv(path, show_col_types = FALSE)
  }
  patterns <- c("HP95_LP65", "HP65_LP95")
  labels <- c(HP95_LP65 = "95% HP / 65% LP", HP65_LP95 = "65% HP / 95% LP")
  aid_colours <- c("65%" = "#C56924", "95%" = "#1976A3")
  method_colours <- c("Mixed model" = "#1976A3", "Participant sensitivity" = "#C56924")
  theme_x <- function() theme_minimal(base_size = 12, base_family = "Helvetica") + theme(
    text = element_text(colour = "#1C2D3E"), plot.title = element_text(size = 17, face = "bold", margin = margin(b = 7)),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)), plot.title.position = "plot",
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "#E4E9ED"),
    strip.text = element_text(face = "bold", size = 11), legend.position = "bottom", legend.title = element_blank(),
    plot.caption = element_text(size = 9, hjust = 0, colour = "#566372", margin = margin(t = 10)),
    plot.caption.position = "plot", axis.title.x = element_blank(), plot.margin = margin(14, 18, 12, 12),
    plot.background = element_rect(fill = "white", colour = NA))
  prepare <- function(d) d %>% mutate(pattern = factor(pattern, patterns), pressure = factor(pressure, c("HP", "LP")),
    x = as.numeric(pressure), point_x = x + (((as.integer(participant_id) * 37L) %% 71L) / 70 - .5) * .18)
  summary_mean <- function(d, value, extra = character()) d %>% group_by(across(all_of(c("pattern", "pressure", "reliability", extra)))) %>%
    summarise(n = n(), estimate = mean(.data[[value]]), se = sd(.data[[value]]) / sqrt(n), .groups = "drop") %>%
    mutate(lower = estimate - qt(.975, n-1)*se, upper = estimate + qt(.975, n-1)*se, x = as.numeric(pressure))
  export <- function(d, stem) write_csv(d, file.path(out, "data", paste0(stem, ".csv")))
  foot <- "Exploratory. Faint points/lines: paired participants; large points and whiskers: participant means and pointwise 95% t intervals."
  paired <- function(d, s, y, title, subtitle, zero = TRUE) {
    p <- ggplot(d, aes(point_x, .data[[y]]))
    if (zero) p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "#87929D")
    p + geom_line(aes(group = participant_id), colour = "#87929D", alpha = .2, linewidth = .3) +
      geom_point(aes(colour = reliability), alpha = .35, size = 1.3) +
      geom_errorbar(data = s, aes(x = x, ymin = lower, ymax = upper, colour = reliability), inherit.aes = FALSE, width = .08, linewidth = .8) +
      geom_point(data = s, aes(x, estimate, fill = reliability), inherit.aes = FALSE, shape = 21, size = 4, stroke = .8, colour = "white") +
      scale_colour_manual(values = aid_colours, guide = "none") + scale_fill_manual(values = aid_colours, labels = c("65% aid", "95% aid")) +
      scale_x_continuous(breaks = 1:2, labels = c("HP (1 s)", "LP (3 s)"), expand = expansion(add = .3)) +
      facet_wrap(~pattern, labeller = as_labeller(labels)) + theme_x() + labs(title = title, subtitle = subtitle, y = NULL, caption = foot)
  }
  sel <- prepare(read("H1_participant_selectivity")) %>% mutate(selectivity = selectivity * 100)
  sel_s <- summary_mean(sel, "selectivity"); export(sel_s, "H1_plotted_means")
  p1 <- paired(sel, sel_s, "selectivity", "H1  Selective agreement with advice",
    "Agreement with correct minus incorrect advice, among answered trials.") + labs(y = "Selectivity (percentage points)")

  decomposition_plot <- function(d, components, total, title, subtitle, colours, component_labels, caption) {
    long <- d %>% pivot_longer(all_of(components), names_to = "component", values_to = "value")
    s <- long %>% group_by(pattern, pressure, component) %>% summarise(estimate = mean(value)*100, .groups = "drop")
    net <- summary_mean(d, total) %>% mutate(estimate = estimate*100, lower = lower*100, upper = upper*100)
    p <- ggplot(s, aes(pressure, estimate, fill = component)) + geom_hline(yintercept = 0, colour = "#87929D", linewidth = .5) +
      geom_col(width = .52, alpha = .85) +
      geom_errorbar(data = net, aes(x = pressure, ymin = lower, ymax = upper), inherit.aes = FALSE, width = .12, linewidth = .8, colour = "#1C2D3E") +
      geom_point(data = net, aes(pressure, estimate), inherit.aes = FALSE, size = 3.3, shape = 23, fill = "white", stroke = 1.1) +
      facet_wrap(~pattern, labeller = as_labeller(labels)) + scale_fill_manual(values = colours, labels = component_labels) + theme_x() +
      labs(title = title, subtitle = subtitle, y = "Accuracy difference (percentage points)", caption = caption)
    list(plot = p, bars = s, net = net)
  }
  bench <- prepare(read("H2_participant_benchmark")) %>% mutate(spoiled = -spoiled, missed_correct_advice = -missed_correct_advice)
  b <- decomposition_plot(bench, c("rescued", "spoiled", "missed_correct_advice"), "net_gain",
    "H2  Value beyond following the aid", "Correcting aid errors can be offset by rejecting correct advice and timing out.",
    c(rescued = "#087F72", spoiled = "#BB503B", missed_correct_advice = "#BA9B53"),
    c(rescued = "Correctly rejected wrong advice", spoiled = "Rejected correct advice", missed_correct_advice = "Timeout on correct advice"),
    "Exploratory. Bars: mean contributions on all trials. Diamonds: total difference and pointwise 95% CI.\nThe benchmark uses realised aid accuracy and assumes timely responses; these are outcome categories, not observed decision changes.")
  p2 <- b$plot; export(b$bars, "H2_components"); export(b$net, "H2_net")

  cal <- prepare(read("H3_participant_calibration")) %>% pivot_longer(c(signed_error, absolute_error), names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric, c("signed_error", "absolute_error"), c("Signed estimation error", "Absolute estimation error")))
  cal_s <- summary_mean(cal, "value", "metric"); export(cal_s, "H3_plotted_means")
  p3 <- paired(cal, cal_s, "value", "H3  Awareness of aid reliability", "Rated aid accuracy compared with its realised block accuracy.") +
    facet_grid(metric ~ pattern, scales = "free_y", labeller = labeller(pattern = labels)) + labs(y = "Rating error (percentage points)")

  association_plot <- function(h, score, title, xlab) {
    d <- read(paste0(h, "_", score, "_predictions")) %>% mutate(pressure = factor(pressure, c("HP", "LP")))
    export(d, paste0(h, "_", score, "_curves"))
    # Trust panels share both axis spans so slopes can be compared visually.
    # Shift each window to its prediction range, preserving the original values.
    trust_axes <- score == "trust"
    if (trust_axes) {
      windows <- d %>% group_by(advice, reliability) %>%
        summarise(bottom = floor((min(lower)*100 - 1)/5)*5,
                  top = ceiling((max(upper)*100 + 1)/5)*5, .groups = "drop")
      span <- max(windows$top - windows$bottom)
      windows <- windows %>% mutate(bottom = pmax(0, pmin(bottom, 100-span)), top = bottom + span)
      x_windows <- d %>% group_by(reliability) %>%
        summarise(left = floor(min(score_deviation)*2)/2,
                  right = ceiling(max(score_deviation)*2)/2, .groups = "drop")
      x_span <- max(x_windows$right - x_windows$left)
      x_windows <- x_windows %>% mutate(right = left + x_span)
      windows <- left_join(windows, x_windows, by = "reliability")
      bounds <- bind_rows(windows %>% mutate(trust_bound = left, agreement_bound = bottom),
                          windows %>% mutate(trust_bound = right, agreement_bound = top))
      export(windows, "H4_trust_axis_windows")
    }
    p <- ggplot(d, aes(score_deviation, probability * 100, colour = pressure, fill = pressure)) +
      geom_ribbon(aes(ymin = lower*100, ymax = upper*100), alpha = .12, linewidth = 0, colour = NA) + geom_line(linewidth = .9) +
      scale_colour_manual(values = c(HP = "#C56924", LP = "#1976A3")) + scale_fill_manual(values = c(HP = "#C56924", LP = "#1976A3")) +
      theme_x() + theme(axis.title.x = element_text()) +
      labs(title = title, subtitle = "Conditional mixed-model associations within participants.", x = xlab, y = "Predicted agreement (%)",
        caption = "Exploratory associations, not causal effects. Bands: pointwise model 95% CIs; curves span observed 10th-90th percentile score deviations.\nBetween-participant rating is held at its mean, block position at midpoint, and random intercept at zero. See participant-cluster sensitivity tests.")
    labels <- labeller(advice = c(Correct = "Correct advice", Incorrect = "Incorrect advice"),
                      reliability = c("65%" = "65% aid", "95%" = "95% aid"))
    if (trust_axes) p +
      geom_blank(data = bounds, aes(x = trust_bound, y = agreement_bound), inherit.aes = FALSE) +
      facet_wrap(vars(advice, reliability), ncol = 2, scales = "free", labeller = labels) +
      scale_x_continuous(breaks = scales::breaks_width(.5), expand = expansion(mult = .03)) +
      scale_y_continuous(breaks = scales::breaks_width(5), expand = expansion(mult = 0)) +
      labs(subtitle = sprintf("Each panel spans %g trust points and %g agreement percentage points; axis ranges differ.", x_span, span),
        caption = "Exploratory associations, not causal effects. Equal axis spans permit slope comparisons; read each panel's axis labels.\nBands: pointwise model 95% CIs. Curves cover observed central score ranges; other predictors are held fixed. See participant-cluster sensitivity tests.")
    else p + facet_grid(advice ~ reliability, scales = "free_x", labeller = labels) + coord_cartesian(ylim = c(0,100))
  }
  p3a <- association_plot("H3", "perceived", "H3  Perceived reliability and agreement", "Block rating minus participant mean (percentage points)")
  p4 <- association_plot("H4", "trust", "H4  Trust and agreement under pressure", "Block trust minus participant mean (1-5 scale points)")

  contrasts <- read("exploratory_contrasts")
  lag <- contrasts %>% filter(hypothesis == "H5", grepl("After error - after correct advice$", contrast)) %>%
    mutate(pressure = factor(ifelse(grepl("^HP", contrast), "HP", "LP"), c("HP", "LP")),
      reliability = ifelse(grepl("95%", contrast), "95% aid", "65% aid"),
      method = factor(ifelse(analysis == "mixed_model", "Mixed model", "Participant sensitivity"), names(method_colours)))
  export(lag, "H5_plotted_contrasts")
  p5 <- ggplot(lag, aes(pressure, estimate, colour = method, shape = method)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#87929D") +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = .25), width = .08, linewidth = .8) +
    geom_point(position = position_dodge(width = .25), size = 3.5) + facet_wrap(~reliability) +
    scale_colour_manual(values = method_colours) + scale_shape_manual(values = c(18,16)) + theme_x() +
    labs(title = "H5  Adjustment after an aid error", subtitle = "Next-trial agreement: previous aid error minus previous correct advice.",
      y = "Agreement difference (percentage points)",
      caption = "Exploratory. Negative values indicate less agreement after an error. Pointwise 95% CIs.\nPrevious participant error and trial position are controlled; current advice is weighted by assigned reliability. Within-block consecutive answered trials only.")

  dec <- prepare(read("H6_participant_decomposition"))
  b <- decomposition_plot(dec, c("completion_component", "choice_component"), "total_gain",
    "H6  Where does the accuracy gain arise?", "Automation minus manual at the same pressure, decomposed within participants.",
    c(completion_component = "#BA9B53", choice_component = "#1976A3"),
    c(completion_component = "Response completion", choice_component = "Accuracy among responses"),
    "Exploratory. Bars: symmetric decomposition of the accuracy gain; diamonds: total gain and pointwise 95% CI.\nThe components sum exactly to each participant's gain. This is an accounting decomposition, not causal mediation.")
  p6 <- b$plot; export(b$bars, "H6_components"); export(b$net, "H6_net")
  save <- function(p, stem, width = 13, height = 7) {
    ggsave(file.path(out, paste0(stem, ".pdf")), p, device = cairo_pdf, width = width, height = height, bg = "white")
    ggsave(file.path(out, paste0(stem, ".png")), p, width = width, height = height, dpi = 300, bg = "white")
  }
  save(p1, "H1_selectivity"); save(p2, "H2_aid_benchmark"); save(p3, "H3_reliability_awareness", height = 9)
  save(p3a, "H3_perceived_reliability_association", height = 9); save(p4, "H4_trust_association", height = 9)
  save(p5, "H5_error_adjustment"); save(p6, "H6_accuracy_components")
  compact <- function(p) p + labs(caption = NULL) + theme(plot.title = element_text(size = 14), plot.subtitle = element_text(size = 10),
    strip.text = element_text(size = 10), axis.title = element_text(size = 10), legend.text = element_text(size = 9))
  slide <- function(left, right, title, caption, name) {
    p <- (compact(left) | compact(right)) + plot_annotation(title = title, caption = caption,
      theme = theme(plot.title = element_text(face = "bold", size = 20, colour = "#1C2D3E"),
                    plot.caption = element_text(hjust = 0, size = 10, colour = "#566372"), plot.margin = margin(15,15,15,15)))
    save(p & theme(legend.position = "bottom"), name, 16, 9)
  }
  slide(p1 + labs(subtitle = "Correct minus incorrect advice agreement.\nHigher values indicate more selective agreement."),
        p2 + labs(subtitle = "Human accuracy minus realised aid accuracy.\nDiamonds show the net difference."),
    "Exploratory: selective advice use and human contribution",
    "Participant means and pointwise 95% CIs. Advice agreement is a behavioural proxy; the always-follow benchmark assumes timely responses.", "slide_4_selectivity_benchmark")
  slide(p3 + labs(subtitle = "Perceived minus realised aid accuracy."), p4 + labs(subtitle = "Equal axis spans; ranges follow the observed values.\nBands are model 95% CIs."),
    "Exploratory: awareness, trust, and behaviour",
    "Left: paired participants and mean rating errors. Right: trust deviation from each participant's mean, over the observed central score range.\nPost-block ratings support associations, not causal mediation; participant-cluster sensitivity tests accompany model estimates.", "slide_5_awareness_trust")
  slide(p5 + labs(subtitle = "Next-trial agreement following wrong vs correct advice."),
        p6 + labs(subtitle = "Decomposition of automation-minus-manual accuracy."),
    "Exploratory: error adjustment and the source of accuracy gains",
    "Pointwise 95% CIs. Error-adjustment estimates control previous participant error and trial position.\nCompletion and choice-accuracy contributions sum to the total gain; neither decomposition nor lag associations establish causal mediation.", "slide_6_adjustment_completion")
  source_paths <- unique(c(consumed, file.path(root, "plot_time_pressure_exploratory.R")))
  write_csv(tibble(path = source_paths, md5 = unname(tools::md5sum(source_paths))), file.path(out, "data/input_checksums.csv"))
  files <- sort(list.files(out, "\\.(pdf|png)$", full.names = TRUE))
  write_csv(tibble(file = basename(files), bytes = file.info(files)$size, md5 = unname(tools::md5sum(files))), file.path(out, "data/artifact_manifest.csv"))
  message("Saved ", length(files), " exploratory figure artifacts to ", out)
}

if (sys.nframe() == 0) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]); root <- dirname(normalizePath(script))
  args <- commandArgs(trailingOnly = TRUE); stopifnot(length(args) <= 2)
  plot_exploratory(if (length(args) >= 1) args[1] else file.path(root, "analysis_outputs/semester2_2026_behavioural"),
    if (length(args) >= 2) args[2] else file.path(root, "plots/semester2_2026_behavioural"), root)
}
