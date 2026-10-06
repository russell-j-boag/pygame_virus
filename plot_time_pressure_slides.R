# One 16:9 figure per main question and exploratory hypothesis, from saved results.
# Rscript plot_time_pressure_slides.R [analysis_dir] [plot_dir]

# Cousineau normalization and Morey's sqrt(k/(k-1)) correction, within a
# complete repeated-measures set in EACH allocation group. RT inputs are
# participant mean log RTs; normalization occurs before back-transformation.
time_pressure_morey <- function(d, groups, levels, log_scale = FALSE) {
  k <- length(levels)
  stopifnot(k > 1, !anyDuplicated(levels), all(is.finite(d$value)),
    !anyDuplicated(d[c(groups, "participant_id", "level")]), setequal(d$level, levels))
  complete <- d %>% group_by(across(all_of(c(groups, "participant_id")))) %>%
    summarise(complete = n() == k && setequal(level, levels), .groups = "drop")
  if (!all(complete$complete)) stop("Morey intervals require every participant to have all specified conditions.")
  d <- d %>% mutate(analysis_value = value) %>%
    group_by(across(all_of(c(groups, "participant_id")))) %>% mutate(person_mean = mean(analysis_value)) %>%
    group_by(across(all_of(groups))) %>% mutate(grand_mean = mean(analysis_value),
      normalized = analysis_value-person_mean+grand_mean) %>% ungroup() %>%
    mutate(display_value = if (log_scale) exp(analysis_value) else analysis_value)
  s <- d %>% group_by(across(all_of(c(groups, "level")))) %>%
    summarise(n = n(), center = mean(analysis_value),
      se_analysis = sd(normalized)/sqrt(n)*sqrt(k/(k-1)), .groups = "drop") %>%
    mutate(k = k, morey_factor = sqrt(k/(k-1)), margin = qt(.975,n-1)*se_analysis,
      value = if (log_scale) exp(center) else center,
      lo = if (log_scale) exp(center-margin) else center-margin,
      hi = if (log_scale) exp(center+margin) else center+margin,
      interval = "Cousineau-Morey within-participant 95% CI", series = "Observed",
      normalization_groups = paste(groups, collapse = ";"), normalization_levels = paste(levels, collapse = ";"),
      analysis_scale = if (log_scale) "log RT" else "original units")
  list(summary = s, participants = d)
}

plot_time_pressure_slides <- function(base, plot_base, root) {
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(readr); library(ggplot2); library(patchwork)
  })
  cache <- file.path(tempdir(), "fontconfig-cache")
  dir.create(cache, showWarnings = FALSE)
  Sys.setenv(XDG_CACHE_HOME = cache)
  out <- file.path(plot_base, "presentation")
  dir.create(file.path(out, "data"), recursive = TRUE, showWarnings = FALSE)
  consumed <- character()
  read <- function(name, exploratory = FALSE) {
    path <- file.path(if (exploratory) file.path(base, "exploratory") else base, paste0(name, ".csv"))
    consumed <<- c(consumed, normalizePath(path, mustWork = TRUE))
    read_csv(path, show_col_types = FALSE)
  }
  for (extra in c(FALSE, TRUE)) {
    stopifnot(file.exists(file.path(if (extra) file.path(base, "exploratory") else base, "COMPLETE.txt")))
    manifest <- read("input_manifest", extra)
    stopifnot(all(file.exists(manifest$path)),
      identical(unname(tools::md5sum(manifest$path)), manifest$md5))
  }
  # Typography, grid and palette follow auto_reliability_virus_atc's current
  # R/paper_hypotheses_plots.R. Interval definitions follow THIS crossed design:
  # reliability at fixed pressure compares different allocation groups.
  slide_theme <- function() theme_minimal(base_size = 18, base_family = "Helvetica") + theme(
    text = element_text(colour = "#202A31"),
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E2E6E8", linewidth = .45),
    axis.text = element_text(size = 17, colour = "#303B44"),
    axis.title = element_text(size = 18), axis.title.x = element_text(margin = margin(t = 7)),
    axis.title.y = element_text(margin = margin(r = 7)),
    strip.text = element_text(face = "bold", size = 20),
    panel.spacing = grid::unit(.9, "lines"),
    plot.title = element_text(face = "bold", size = 24, lineheight = 1.02, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 14, lineheight = 1.08, margin = margin(b = 9)),
    plot.caption = element_text(size = 12.5, hjust = 0, lineheight = 1.12, margin = margin(t = 8)),
    plot.title.position = "plot", plot.caption.position = "plot",
    legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 15),
    legend.margin = margin(0, 0, 0, 0), legend.key.height = grid::unit(17, "pt"),
    plot.margin = margin(10, 14, 10, 10), plot.background = element_rect(fill = "white", colour = NA))
  old_theme <- theme_set(slide_theme())
  on.exit(theme_set(old_theme), add = TRUE)
  colours <- c("65%" = "#C56924", "95%" = "#1976A3")
  patterns <- c("HP95_LP65", "HP65_LP95")
  counts <- read("cohort_counts")
  n <- sum(counts$n_participants)
  group_labels <- setNames(paste0(c("95% HP / 65% LP", "65% HP / 95% LP"),
    "\n(n = ", counts$n_participants[match(patterns, counts$pattern)], ")"), patterns)
  pressure_labels <- c(HP = "High pressure (1 s)", LP = "Low pressure (3 s)")
  reliability_labels <- c("65%" = "65% aid", "95%" = "95% aid")
  prep <- function(d) d %>% mutate(pressure = factor(pressure, c("HP", "LP")),
    reliability = factor(reliability, c("65%", "95%")))
  group_colours <- setNames(c("#1976A3", "#C56924"), patterns)
  short_groups <- c(HP95_LP65 = "95% HP / 65% LP", HP65_LP95 = "65% HP / 95% LP")
  mean_caption <- "Observed participant means + ordinary pointwise 95% t CIs."
  morey_caption <- "Observed means + Cousineau-Morey within-participant 95% CIs."
  export <- function(d, stem) write_csv(d, file.path(out, "data", paste0(stem, ".csv")))
  catalog <- list(); figures <- list()
  save <- function(p, stem, set, hypothesis, question, message, notes, d, participants,
                   source_set = set, source_hypothesis = hypothesis) {
    stopifnot(!stem %in% names(figures))
    figures[[stem]] <<- p
    for (ext in c("png", "pdf")) ggsave(file.path(out, paste0(stem, ".", ext)), p,
      width = 12, height = 6.75, units = "in", dpi = 300, bg = "white",
      device = if (ext == "pdf") cairo_pdf else "png")
    export(d, paste0(stem, "_data"))
    export(participants, paste0(stem, "_participants"))
    analysis_origin <- if (source_set == "Exploratory") "Specified after inspecting the main results" else "Original main hypothesis set"
    if (set == "Main" && source_set == "Exploratory") notes <- paste0(
      "Promoted from exploratory ",source_hypothesis," to the main presentation set as ",hypothesis,
      "; specified after inspecting the main results. Source analyses and test adjustment families are unchanged. ",notes)
    catalog[[stem]] <<- tibble(order = length(figures), set, hypothesis, source_set, source_hypothesis, analysis_origin, question,
      key_message = message, filename = stem, width_in = 12, height_in = 6.75,
      png_width = 3600, png_height = 2025, notes)
  }
  compose <- function(p, title, subtitle, caption) p + plot_layout(guides = "collect") +
    plot_annotation(title = title, subtitle = subtitle, caption = caption, theme = slide_theme())
  ordinary_mean <- function(d, keys, interval = "Ordinary participant mean 95% t CI") {
    stopifnot(all(is.finite(d$value)), !anyDuplicated(d[c(keys, "participant_id")]))
    d %>% group_by(across(all_of(keys))) %>%
      summarise(n = n(), se_analysis = sd(value)/sqrt(n), value = mean(value), .groups = "drop") %>%
      mutate(lo = value-qt(.975,n-1)*se_analysis, hi = value+qt(.975,n-1)*se_analysis,
        interval = interval, series = "Observed", analysis_scale = "original units")
  }
  # Saved inferential results are used ONLY for reference-style annotations.
  # They never supply the plotted means or intervals.
  model_tests <- read("planned_contrasts")
  participant_tests <- read("participant_sensitivity_contrasts")
  exploratory_tests <- read("exploratory_contrasts", TRUE)
  test_caption <- "Model Holm tests: * p < .05; ns p >= .05; \u2020 participant test differs."
  annotations <- list()
  tests <- function(outcome, contrast, hypothesis = NULL) {
    keys <- tibble(test_outcome = outcome, contrast)
    m <- if (is.null(hypothesis)) model_tests else filter(exploratory_tests, .data$hypothesis == .env$hypothesis, analysis == "mixed_model")
    o <- if (is.null(hypothesis)) participant_tests else filter(exploratory_tests, .data$hypothesis == .env$hypothesis, analysis != "mixed_model")
    stopifnot(!anyDuplicated(m[c("outcome","contrast")]), !anyDuplicated(o[c("outcome","contrast")]))
    a <- keys %>% left_join(m %>% transmute(test_outcome = outcome, contrast, model_p_holm = p_holm), by = c("test_outcome","contrast")) %>%
      left_join(o %>% transmute(test_outcome = outcome, contrast, participant_p_holm = p_holm), by = c("test_outcome","contrast")) %>%
      mutate(hypothesis = if (is.null(hypothesis)) "Main" else hypothesis,
        p_holm = model_p_holm, participant_disagreement = is.finite(model_p_holm) & is.finite(participant_p_holm) &
          (model_p_holm < .05) != (participant_p_holm < .05),
        symbol = paste0(ifelse(!is.finite(p_holm), "NA", ifelse(p_holm < .05, "*", "ns")),
          ifelse(participant_disagreement, "\u2020", "")),
        test_source = "Saved model Holm test; participant sensitivity disagreement flagged")
    stopifnot(nrow(a) == nrow(keys), !anyNA(a$model_p_holm), !anyNA(a$participant_p_holm))
    a
  }
  place <- function(a, d, facets = character(), gap = .15) {
    ranges <- d %>% group_by(across(all_of(facets))) %>% summarise(bottom = min(lo), top = max(hi), .groups = "drop") %>%
      mutate(span = pmax(top-bottom, .1))
    if (length(facets)) a <- left_join(a, ranges, by = facets) else a <- bind_cols(a, ranges[rep(1,nrow(a)),])
    if (!"lane" %in% names(a)) a$lane <- 1
    if (!"bracket_colour" %in% names(a)) a$bracket_colour <- "#58636C"
    a %>% mutate(y = top+span*(.14+gap*(lane-1)), tip = y-span*.035, text_y = y+span*.015)
  }
  bracket <- function(p, a, stem) {
    stopifnot(all(is.finite(a$x1)), all(is.finite(a$x2)), all(a$x1 < a$x2))
    annotations[[stem]] <<- bind_rows(annotations[[stem]], a %>% mutate(annotation = "Bracket"))
    p + geom_segment(data = a, aes(x = x1, xend = x2, y = y, yend = y), inherit.aes = FALSE,
        colour = a$bracket_colour, linewidth = .65) +
      geom_segment(data = a, aes(x = x1, xend = x1, y = tip, yend = y), inherit.aes = FALSE,
        colour = a$bracket_colour, linewidth = .65) +
      geom_segment(data = a, aes(x = x2, xend = x2, y = tip, yend = y), inherit.aes = FALSE,
        colour = a$bracket_colour, linewidth = .65) +
      geom_text(data = a, aes(x = (x1+x2)/2, y = text_y, label = symbol), inherit.aes = FALSE,
        colour = a$bracket_colour, vjust = 0, size = 5)
  }
  # Plotted means/intervals come exclusively from participant summaries;
  # the saved model results above supply only significance annotations.
  participant <- read("participant_performance")
  add_outcomes <- function(d) bind_rows(
    d %>% mutate(outcome = "Accuracy (%)", value = 100*accuracy),
    d %>% mutate(outcome = "Correct RT (s)", value = mean_log_correct_rt))
  performance_summary <- function(d, levels, groups = "pattern") {
    result <- lapply(c("Accuracy (%)", "Correct RT (s)"), function(outcome_name) {
      z <- filter(d, outcome == outcome_name)
      time_pressure_morey(z, c(groups, "outcome"), levels, log_scale = outcome_name == "Correct RT (s)")
    })
    list(summary = bind_rows(lapply(result, `[[`, "summary")),
      participants = bind_rows(lapply(result, `[[`, "participants")))
  }
  # Main 1: display observed manual conditions, retaining allocation groups.
  manual <- participant %>% filter(mode == "Manual") %>% mutate(level = pressure) %>% add_outcomes()
  manual <- performance_summary(manual, c("LP", "HP"))
  manual$summary <- manual$summary %>% mutate(level = factor(level, c("LP", "HP")), pattern = factor(pattern, patterns))
  manual_panel <- function(outcome_name, title, ylab) {
    ggplot(filter(manual$summary, outcome == outcome_name), aes(level, value, colour = pattern, group = pattern)) +
      geom_line(linewidth = .7, position = position_dodge(.18)) +
      geom_errorbar(aes(ymin = lo, ymax = hi), width = .10, linewidth = 1.05, position = position_dodge(.18)) +
      geom_point(size = 4.2, position = position_dodge(.18)) +
      scale_colour_manual(values = group_colours, labels = short_groups) +
      scale_x_discrete(labels = c(LP = "LP (3 s)", HP = "HP (1 s)")) +
      labs(x = NULL, y = ylab, title = title) + slide_theme() + theme(plot.title = element_text(size = 20))
  }
  p <- manual_panel("Accuracy (%)", "Manual accuracy", "Accuracy (%)")
  q <- manual_panel("Correct RT (s)", "Manual response time", "Correct RT (s)")
  manual_annotations <- tests(rep(c("accuracy","correct_rt"), each = 2),
    rep(paste(patterns, "Manual HP - Manual LP"), 2)) %>%
    mutate(outcome = rep(c("Accuracy (%)","Correct RT (s)"), each = 2), pattern = rep(patterns,2),
      lane = rep(1:2,2), x1 = 1+rep(c(-.045,.045),2), x2 = 2+rep(c(-.045,.045),2),
      bracket_colour = unname(group_colours[pattern])) %>% place(manual$summary, "outcome")
  p <- bracket(p, filter(manual_annotations, outcome == "Accuracy (%)"), "01_main_H1_manual_pressure")
  q <- bracket(q, filter(manual_annotations, outcome == "Correct RT (s)"), "01_main_H1_manual_pressure")
  title <- "H1: High pressure lowers accuracy and shortens RT"
  save(compose(p | q, title, "Manual performance at 3 s and 1 s, measured in the same participants.",
    paste(morey_caption, "Normalised over HP/LP within each allocation group; RTs are geometric means.", test_caption, sep = "\n")),
    "01_main_H1_manual_pressure", "Main", "H1", "What does time pressure do to manual performance?", title,
    "Equal participant weighting. Two-condition normalization within allocation group, separately for accuracy and log RT. RT endpoints are back-transformed after normalization; geometric means describe correct valid responses. Morey intervals describe the within-group HP/LP pattern, not between-group differences or the CI of a paired contrast.",
    manual$summary, manual$participants)

  # Main 2: observed manual/aided means in each group's four-condition panel.
  # Normalizing over the same four cells keeps the baseline definition consistent.
  performance <- participant %>% mutate(level = paste(mode, pressure)) %>% add_outcomes()
  condition_levels <- c("Manual HP", "Automation HP", "Manual LP", "Automation LP")
  performance <- performance_summary(performance, condition_levels)
  performance$summary <- performance$summary %>% mutate(
    pattern = factor(pattern, patterns), outcome = factor(outcome, c("Accuracy (%)", "Correct RT (s)")),
    level = factor(level, condition_levels), pressure = ifelse(grepl("HP$", level), "HP", "LP"),
    condition_colour = ifelse(grepl("^Manual", level), "Manual", ifelse(
      (pattern == "HP95_LP65" & pressure == "HP") | (pattern == "HP65_LP95" & pressure == "LP"), "95%", "65%")))
  performance_colours <- c(Manual = "#727B88", colours)
  performance_figure <- function(outcome_name, test_outcome, stem, hypothesis, title, question, notes) {
    d <- filter(performance$summary, outcome == outcome_name)
    raw <- filter(performance$participants, outcome == outcome_name)
    p <- ggplot(d, aes(level, value)) +
      geom_line(aes(group = pressure), linewidth = .8, colour = "#969FA6") +
      geom_errorbar(aes(ymin = lo, ymax = hi, colour = condition_colour), width = .1, linewidth = 1.05) +
      geom_point(aes(colour = condition_colour), size = 4.2) +
      facet_wrap(~pattern, labeller = as_labeller(short_groups)) +
      scale_colour_manual(values = performance_colours, breaks = c("Manual", "65%", "95%"),
        labels = c("Manual", "65% aid", "95% aid")) +
      scale_x_discrete(labels = c("Manual HP" = "Manual\nHP", "Automation HP" = "Aided\nHP",
        "Manual LP" = "Manual\nLP", "Automation LP" = "Aided\nLP")) +
      scale_y_continuous(expand = expansion(mult = c(.08,.10))) +
      labs(title = title, subtitle = "Each line links observed manual and aided performance at the same deadline.", x = NULL, y = outcome_name,
        caption = paste(morey_caption, "Upper bracket compares the HP and LP aid effects. HP = 1 s; LP = 3 s.", test_caption, sep = "\n"))
    a <- expand_grid(pattern = patterns, pressure = c("HP","LP"))
    a <- bind_cols(a, tests(test_outcome, paste(a$pattern,a$pressure,"Automation - Manual"))) %>%
      mutate(pattern = factor(pattern, patterns), outcome = outcome_name, comparison = "Aided minus Manual",
        x1 = ifelse(pressure == "HP", 1, 3), x2 = x1+1) %>% place(d)
    # Endpoints are the centres of the two manual/aided pairs; the label makes
    # clear that this is a difference of paired effects, not a mean contrast.
    b <- tests(test_outcome, paste(patterns,"HP gain - LP gain")) %>%
      mutate(pattern = factor(patterns, patterns), outcome = outcome_name, comparison = "HP effect minus LP effect",
        lane = 2.5, x1 = 1.5, x2 = 3.5,
        symbol = paste(if (.env$test_outcome == "accuracy") "Gain difference:" else "RT-effect difference:", symbol)) %>% place(d)
    p <- bracket(p, bind_rows(a,b), stem)
    if (test_outcome == "correct_rt") p <- p + labs(caption = paste(morey_caption,
      "Geometric RTs; upper bracket compares log-RT effects across pressure. HP = 1 s; LP = 3 s.", test_caption, sep = "\n"))
    save(p, stem, "Main", hypothesis, question, title, notes, d, raw)
  }
  performance_figure("Accuracy (%)", "accuracy", "02_main_H2_accuracy_benefits", "H2",
    "H2: Accuracy benefits are larger where advice is 95%",
    "Do accuracy gains follow the pressure assigned to the reliable aid?",
    "Observed manual/aided accuracy means at HP/LP in each allocation group; four-condition Morey normalization within each group. Lower brackets compare aided versus manual at each deadline. Upper brackets compare HP-minus-LP gains: the contrast is positive in HP95_LP65 and negative in HP65_LP95, supported by both model and participant tests after the original within-accuracy Holm adjustment. No RT outcome is included in H2.")

  # Main 3: make the two actual paired conditions visible on the accuracy scale.
  comp <- participant %>% filter((mode == "Manual" & pressure == "LP") | (mode == "Automation" & pressure == "HP")) %>%
    mutate(level = ifelse(mode == "Manual", "Manual LP", "Aided HP"), value = 100*accuracy)
  comp <- time_pressure_morey(comp, "pattern", c("Manual LP", "Aided HP"))
  comp$summary <- comp$summary %>% mutate(pattern = factor(pattern, rev(patterns)),
    level = factor(level, c("Manual LP", "Aided HP")),
    condition_colour = ifelse(level == "Manual LP", "Manual", ifelse(pattern == "HP95_LP65", "95%", "65%")))
  title <- "H3: 95% advice compensates for the shorter deadline"
  p <- ggplot(comp$summary, aes(level, value, group = pattern)) +
    geom_line(colour = "#969FA6", linewidth = .8) +
    geom_errorbar(aes(ymin = lo, ymax = hi, colour = condition_colour), width = .1, linewidth = 1.05) +
    geom_point(aes(colour = condition_colour), size = 4.2) +
    facet_wrap(~pattern, labeller = as_labeller(c(HP95_LP65 = "95% aid in HP", HP65_LP95 = "65% aid in HP"))) +
    scale_colour_manual(values = performance_colours, guide = "none") +
    scale_x_discrete(labels = c("Manual LP" = "Manual LP (3 s)", "Aided HP" = "Aided HP (1 s)")) +
    scale_y_continuous(expand = expansion(mult = c(.12,.28))) +
    labs(title = title, subtitle = "Observed accuracy at 1 s with the aid versus 3 s without it, in the same participants.", x = NULL, y = "Accuracy (%)",
      caption = paste(morey_caption, "Normalised over the two displayed conditions within each allocation group; baselines remain group-specific.", sep = "\n"))
  a <- tests("accuracy", paste(patterns,"Aided HP - Manual LP")) %>% mutate(pattern = patterns, x1 = 1, x2 = 2) %>% place(comp$summary)
  p <- bracket(p, a, "03_main_H3_compensation") + labs(caption = paste(morey_caption,
    "Two paired conditions per allocation group; group-specific manual baselines.", test_caption, sep = "\n"))
  save(p, "03_main_H3_compensation", "Main", "H3", "Can the aid offset the shorter deadline?", title,
    "Observed paired accuracy means at manual LP and aided HP. Two-condition Morey normalization within allocation group; no cross-group baseline pooling. Morey intervals describe the pattern, not the uncertainty of the paired difference. Nonsignificance would not establish equivalence.", comp$summary, comp$participants)

  performance_figure("Correct RT (s)", "correct_rt", "04_main_H4_response_speed", "H4",
    "H4: Advice changes response speed across conditions",
    "How do time pressure and reliability alter correct-response RT effects?",
    "Observed geometric correct RT means at manual/aided x HP/LP; normalization and Morey intervals use participant mean log RTs within each allocation group before exponentiation. Lower brackets compare aided/manual log RT; upper brackets compare the log-RT effects across pressure. Original within-correct-RT Holm families and participant-disagreement flags are retained. These means condition on correct valid responses; accuracy belongs in H2.")

  # H5 and H6 are distinct hypotheses. Fixed-pressure reliability comparisons,
  # and fixed-reliability pressure comparisons, are between allocation groups.
  reliance <- read("participant_reliance") %>% mutate(panel = ifelse(advice == "Correct", "Correct advice", "Incorrect advice"), value = 100*agreement)
  trust <- read("participant_trust") %>% mutate(panel = "Trust", value = trust)
  rt_raw <- prep(bind_rows(reliance, trust))
  reliance_trust <- ordinary_mean(rt_raw, c("pattern", "pressure", "reliability", "panel"))
  mean_panel <- function(d, ylab, title, limits, breaks) ggplot(d, aes(pressure, value, colour = reliability, group = reliability)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = .1, linewidth = 1.05, position = position_dodge(.35)) +
    geom_point(size = 4.2, position = position_dodge(.35)) +
    scale_colour_manual(values = colours, labels = reliability_labels, drop = FALSE) +
    scale_x_discrete(labels = c(HP = "HP (1 s)", LP = "LP (3 s)")) +
    scale_y_continuous(breaks = breaks) + coord_cartesian(ylim = limits) +
    labs(x = NULL, y = ylab, title = title) + slide_theme() + theme(plot.title = element_text(size = 20))
  factorial_panel <- function(panel_name, stem) {
    d <- filter(reliance_trust, panel == panel_name)
    is_trust <- panel_name == "Trust"
    outcome <- if (is_trust) "trust" else "reliance"
    prefix <- if (is_trust) "Trust" else sub(" advice$", "", panel_name)
    # Each advice panel uses its own observed CI range. Scale the bracket
    # spacing with that range so annotations do not force a common 0-139 axis.
    if (is_trust) {
      bracket_y <- c(4.05,4.4,4.75,5.13)
      bracket_tip <- .035; text_offset <- .015
      limits <- c(1,5.6); breaks <- 1:5
    } else {
      span <- max(max(d$hi)-min(d$lo),1)
      bracket_y <- max(d$hi)+span*c(.14,.36,.58,.80)
      bracket_tip <- span*.035; text_offset <- span*.015
      limits <- c(min(d$lo)-span*.10,max(bracket_y)+span*.20)
      breaks <- pretty(range(d$lo,d$hi),n = 4)
      breaks <- breaks[breaks >= 0 & breaks <= 100]
    }
    p <- mean_panel(d, if (is_trust) "Trust (1-5)" else "Agreement (%)", panel_name,
      limits, breaks)
    # Short brackets compare reliability at fixed pressure.
    a <- tests(outcome, paste(prefix,c("HP","LP"),"95% - 65%")) %>%
      mutate(panel = panel_name, pressure = c("HP","LP"), comparison = "Reliability at fixed pressure",
        x1 = c(1,2)-.0875, x2 = x1+.175, y = bracket_y[1],
        tip = y-bracket_tip, text_y = y+text_offset, bracket_colour = "#58636C")
    # Coloured long brackets compare pressure within each reliability level.
    b <- tests(outcome, paste(prefix,c("65%","95%"),"HP - LP")) %>%
      mutate(panel = panel_name, reliability = c("65%","95%"), comparison = "Pressure at fixed reliability",
        x1 = 1+c(-.0875,.0875), x2 = 2+c(-.0875,.0875), y = bracket_y[2:3],
        tip = y-bracket_tip, text_y = y+text_offset,
        bracket_colour = unname(colours[reliability]), symbol = paste(reliability,"HP vs LP:",symbol))
    # Highest bracket compares the two reliability effects, the interaction.
    c <- tests(outcome, paste(prefix,"Reliability effect HP - LP")) %>%
      mutate(panel = panel_name, comparison = "Reliability by pressure interaction", x1 = 1, x2 = 2,
        y = bracket_y[4], tip = y-bracket_tip,
        text_y = y+text_offset, bracket_colour = "#58636C",
        symbol = paste("Reliability x pressure:",symbol))
    bracket(p, bind_rows(a,b,c), stem)
  }
  p <- factorial_panel("Correct advice", "05_main_H5_behavioural_reliance")
  q <- factorial_panel("Incorrect advice", "05_main_H5_behavioural_reliance")
  title <- "H5: Higher reliability increases advice agreement"
  save(compose(p | q, title, "Observed agreement under each deadline. Panels use separate y-axis ranges.",
    paste(mean_caption, "Brackets compare reliability, pressure, and their interaction; these comparisons are between groups.", test_caption, sep = "\n")),
    "05_main_H5_behavioural_reliance", "Main", "H5", "How do pressure and reliability affect agreement with correct and incorrect advice?", title,
    "Observed participant-block agreement among answered trials, stratified by advice correctness; trust is excluded. Each panel uses an independent y-axis range based on its observed confidence limits, with proportional space for significance brackets. Ordinary t intervals are used because fixed-pressure reliability and fixed-reliability pressure comparisons are between groups. Short brackets test reliability within pressure; coloured long brackets test pressure within reliability; the highest bracket tests their interaction. All ten saved reliance contrasts retain their existing Holm adjustment and participant-disagreement flags. No lines connect different participants.",
    filter(reliance_trust, panel != "Trust"), filter(rt_raw, panel != "Trust"))
  p <- factorial_panel("Trust", "06_main_H6_trust")
  title <- "H6: Trust is higher with reliable advice at both deadlines"
  p <- p + labs(title = title, subtitle = "Observed six-item trust ratings by aid reliability and time pressure.",
    caption = paste(mean_caption, "Brackets compare reliability, pressure, and whether pressure changes the reliability effect.", test_caption, sep = "\n")) +
    theme(plot.title = element_text(size = 24))
  save(p, "06_main_H6_trust", "Main", "H6", "Does pressure alter trust or its sensitivity to aid reliability?", title,
    "Observed six-item mean trust on the 1-5 scale, with ordinary participant t intervals for the between-group comparisons. Short brackets compare reliability within each pressure; coloured long brackets compare pressure within reliability; the highest bracket tests the pressure-by-reliability interaction. All five saved trust contrasts retain their original within-trust Holm family and participant-disagreement flags. Agreement outcomes are displayed separately in H5.",
    filter(reliance_trust, panel == "Trust"), filter(rt_raw, panel == "Trust"))

  # Main H7 retains the original exploratory H2 benchmark analysis.
  benchmark <- prep(read("H2_participant_benchmark", TRUE)) %>%
    mutate(Human = 100*human_accuracy, Aid = 100*aid_accuracy) %>%
    pivot_longer(c(Human,Aid), names_to = "level", values_to = "value")
  benchmark <- time_pressure_morey(benchmark, c("pattern","pressure","reliability"), c("Aid","Human"))
  benchmark$summary <- benchmark$summary %>% mutate(level = factor(level,c("Aid","Human")))
  title <- "H7: People outperform the 65% aid, but not the 95% aid"
  p <- ggplot(benchmark$summary, aes(level,value,colour = reliability,group = reliability)) +
    geom_line(linewidth = .8, position = position_dodge(.18)) +
    geom_errorbar(aes(ymin = lo,ymax = hi),width = .1,linewidth = 1.05,position = position_dodge(.18)) +
    geom_point(size = 4.2,position = position_dodge(.18)) +
    facet_wrap(~pressure,labeller = as_labeller(pressure_labels)) +
    scale_colour_manual(values = colours,labels = reliability_labels) +
    scale_x_discrete(labels = c(Aid = "Aid alone", Human = "Human + aid")) +
    labs(title = title,subtitle = "Observed human accuracy versus the aid's realised accuracy in the same blocks.",x = NULL,y = "Accuracy (%)",
      caption = paste("Morey 95% CIs compare human and aid within each cell; aid-alone assumes timely responses. Originally exploratory.",test_caption,sep = "\n"))
  a <- expand_grid(pressure = c("HP","LP"), reliability = c("65%","95%"))
  a <- bind_cols(a, tests("net_gain",paste(a$pressure,a$reliability,"versus zero"),"H2")) %>%
    mutate(x1 = 1+ifelse(reliability == "65%",-.045,.045), x2 = x1+1,
      bracket_colour = unname(colours[reliability])) %>% place(benchmark$summary,c("pressure","reliability"))
  # Fixed spacing keeps both groups' annotations clear on the common percent scale.
  a <- a %>% mutate(y = top+3,tip = y-.6,text_y = y+.3)
  p <- bracket(p,a,"07_main_H7_aid_benchmark")
  save(p,"07_main_H7_aid_benchmark","Main","H7","Do people add value beyond always following the aid?",title,
    "Observed human and realised aid accuracy within each participant-block. Two-condition Morey CIs apply to the human-versus-aid pair separately within each allocation-pressure cell; no cross-reliability inference follows from these intervals. Saved H2 net-gain-versus-zero model and participant tests annotate the same paired comparison, retaining the original exploratory H2 Holm families. Timeouts are incorrect human responses; aid-alone assumes timely responding.",benchmark$summary,benchmark$participants, source_set = "Exploratory", source_hypothesis = "H2")

  # Main H8 (original exploratory H6) is an observed accounting decomposition. CI of a paired total gain
  # comes from participant differences, not normalization over unlike components.
  dec <- prep(read("H6_participant_decomposition", TRUE))
  dec_long <- dec %>% pivot_longer(c(completion_component, choice_component, total_gain), names_to = "component", values_to = "value") %>%
    mutate(value = 100*value)
  decomposition <- ordinary_mean(dec_long, c("pattern", "pressure", "reliability", "component"), "Paired-effect 95% t CI") %>%
    mutate(kind = ifelse(component == "total_gain", "Total", "Component"))
  components <- filter(decomposition, kind == "Component"); total <- filter(decomposition, kind == "Total")
  title <- "H8: 95% aid gains mainly reflect more accurate choices"
  p <- ggplot(components, aes(reliability, value, fill = component)) +
    geom_hline(yintercept = 0, colour = "#87929D", linewidth = .5) + geom_col(width = .5) +
    geom_errorbar(data = total, aes(x = reliability, ymin = lo, ymax = hi), inherit.aes = FALSE, width = .12, linewidth = 1, colour = "#424D56") +
    geom_point(data = total, aes(reliability, value), inherit.aes = FALSE, shape = 23, size = 4.4, fill = "white", stroke = 1.1) +
    facet_wrap(~pressure, labeller = as_labeller(pressure_labels)) +
    scale_fill_manual(values = c(completion_component = "#BA9B53", choice_component = "#1976A3"),
      breaks = c("choice_component", "completion_component"), labels = c("Accuracy among responses", "Response completion")) +
    scale_x_discrete(labels = reliability_labels) + scale_y_continuous(breaks = seq(-2,14,2)) +
    labs(title = title, subtitle = "Observed aided-minus-manual accuracy, decomposed within each participant at the same deadline.",
      x = NULL, y = "Contribution to accuracy gain (pp)",
      caption = "Exploratory. Bars: observed mean contributions. Diamonds: mean paired gain + paired-effect 95% t CIs.\nCompletion and choice contributions sum exactly to each participant's gain; this is descriptive accounting.")
  a <- total %>% select(pattern,pressure,reliability,value,hi)
  a <- bind_cols(a,tests("accuracy",paste(a$pattern,a$pressure,"Automation - Manual"))) %>%
    mutate(annotation = "Total gain versus zero",text_y = hi+.6)
  annotations[["08_main_H8_accuracy_components"]] <- a
  p <- p + geom_text(data = a,aes(x = reliability,y = text_y,label = symbol),inherit.aes = FALSE,size = 5) +
    labs(caption = paste("Bars: observed contributions. Diamonds: paired gain + 95% t CI. Symbols test total gain. Originally exploratory.",
      test_caption,sep = "\n"))
  save(p, "08_main_H8_accuracy_components", "Main", "H8", "Do gains arise from completion or more accurate responses?", title,
    "Symmetric empirical decomposition of completion x answered-trial accuracy. Ordinary t CIs of participant difference scores describe paired gains; no Morey normalization over unlike components is appropriate. Component intervals are exported but not displayed. Symbols above the total diamonds use the saved main accuracy gain-versus-zero model tests, flagging disagreement with participant tests; no significance bracket compares the accounting components. This accounting identity is not causal mediation.", decomposition, dec_long, source_set = "Exploratory", source_hypothesis = "H6")

  # Main H9 (original exploratory H1) displays within-person differences. Its ordinary
  # t intervals quantify the mean paired effect; do not apply Morey a second time.
  sel <- prep(read("H1_participant_selectivity", TRUE)) %>% mutate(value = selectivity*100)
  sel_s <- ordinary_mean(sel, c("pattern", "pressure", "reliability"), "Paired-effect 95% t CI")
  title <- "H9: Agreement is less selective with 95% advice"
  p <- mean_panel(sel_s, "Correct minus incorrect agreement (pp)", title, c(0,75), seq(0,75,15)) +
    labs(subtitle = "Observed correct-minus-incorrect advice agreement, calculated within each participant.",
      caption = "Exploratory. Points: mean participant differences. Bars: paired-effect 95% t CIs; pp = percentage points.\nReliability comparisons are between groups. Selectivity is conditional on answering and does not measure verification.") +
    theme(plot.title = element_text(size = 24))
  a <- tests("selectivity", paste(c("HP","LP"),"95% - 65%"), "H1") %>%
    mutate(pressure = c("HP","LP"), x1 = c(1,2)-.0875, x2 = x1+.175) %>% place(sel_s)
  p <- bracket(p, a, "09_main_H9_selectivity") + labs(caption = paste(
    "Observed paired differences + 95% t CIs. Reliability comparisons are between groups. Originally exploratory.",
    test_caption, sep = "\n"))
  save(p, "09_main_H9_selectivity", "Main", "H9", "How selectively is advice followed?", title,
    "Empirical selectivity = correct-advice agreement minus incorrect-advice agreement within each participant-block. Ordinary t intervals are applied to these paired difference scores, retaining the correct uncertainty for each mean effect. They are not Morey intervals around condition means. Reliability comparisons at fixed pressure are between groups.", sel_s, sel, source_set = "Exploratory", source_hypothesis = "H1")

  # H3 Morey intervals apply ONLY to perceived versus realised accuracy within
  # the same participant-block, not to comparisons across reliability groups.
  ratings <- prep(read("H3_participant_calibration", TRUE)) %>%
    transmute(participant_id, pattern, pressure, reliability, Perceived = perceived, Realised = 100*aid_accuracy) %>%
    pivot_longer(c(Perceived, Realised), names_to = "level", values_to = "value")
  ratings <- time_pressure_morey(ratings, c("pattern", "pressure", "reliability"), c("Perceived", "Realised"))
  ratings$summary <- ratings$summary %>% mutate(level = factor(level, c("Perceived", "Realised")))
  title <- "H3: Reliability is recognised, but underestimated"
  p <- ggplot(ratings$summary, aes(reliability, value, colour = reliability, shape = level, group = level)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.35), width = .1, linewidth = 1.05) +
    geom_point(position = position_dodge(.35), size = 4.5, stroke = 1.2) +
    facet_wrap(~pressure, labeller = as_labeller(pressure_labels)) +
    scale_colour_manual(values = colours, guide = "none") + scale_shape_manual(values = c(Perceived = 16, Realised = 1)) +
    scale_x_discrete(labels = reliability_labels) + scale_y_continuous(breaks = seq(40,100,10)) +
    labs(title = title, subtitle = "Observed ratings and realised accuracy, paired within each participant-block.", x = NULL, y = "Aid accuracy (%)",
      caption = "Exploratory. Morey 95% CIs describe perceived-versus-realised pairs within each cell, not reliability-group differences.\nUnderestimation at LP 65% is uncertain after Holm adjustment; the saved between-group tests establish reliability recognition.")
  a <- expand_grid(pressure = c("HP","LP"),reliability = c("65%","95%"))
  a <- bind_cols(a,tests("signed_error",paste(a$pressure,a$reliability,"versus zero"),"H3")) %>%
    mutate(x1 = ifelse(reliability == "65%",1,2)-.0875,x2 = x1+.175) %>% place(ratings$summary,c("pressure","reliability"))
  a <- a %>% mutate(y = top+2.3,tip = y-.7,text_y = y+.2)
  b <- tests("perceived",paste(c("HP","LP"),"95% - 65%"),"H3") %>%
    mutate(pressure = c("HP","LP"),x1 = 1-.0875,x2 = 2-.0875,y = 110,tip = 109.3,text_y = 110.2,
      bracket_colour = "#58636C",symbol = paste("Perceived:",symbol))
  p <- bracket(p,bind_rows(a,b),"10_exploratory_H3_reliability_awareness") +
    coord_cartesian(ylim = c(40,115)) + labs(caption = paste(
      "Exploratory. Morey 95% CIs compare perceived/realised pairs within each cell, not reliability groups.",test_caption,sep = "\n"))
  save(p, "10_exploratory_H3_reliability_awareness", "Exploratory", "H3", "Do participants recognise and accurately estimate reliability?", title,
    "Two-condition Morey normalization separately for each pattern x pressure x reliability cell, over each participant's perceived and realised accuracy. Both centres remain the empirical means. These intervals are for paired estimation error, not between-group reliability discrimination. The saved ordinary/paired tests retain their original Holm families; LP 65% underestimation is not established after adjustment.", ratings$summary, ratings$participants)

  # H4: paired changes are the direct empirical view of a within-person question.
  # One point per person per advice panel; no fitted slopes or prediction bands.
  trust_cells <- read("H4_trust_participant_cells", TRUE) %>%
    left_join(participant %>% distinct(participant_id, pattern), by = "participant_id")
  changes <- trust_cells %>% select(participant_id, pattern, advice, pressure, score_within, agreement) %>%
    pivot_wider(names_from = pressure, values_from = c(score_within, agreement)) %>%
    mutate(trust_change = score_within_HP-score_within_LP, value = 100*(agreement_HP-agreement_LP),
      series = "Observed", interval = "None: individual paired changes",
      pattern = factor(pattern, patterns), advice = factor(advice, c("Correct", "Incorrect")))
  stopifnot(nrow(changes) == 2*n, all(is.finite(changes$trust_change)), all(is.finite(changes$value)))
  title <- "H4: Observed changes in trust and advice agreement"
  p <- ggplot(changes, aes(trust_change, value, colour = pattern)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#A5ADB3") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "#A5ADB3") +
    geom_point(size = 2.7, alpha = .7) +
    facet_wrap(~advice, labeller = as_labeller(c(Correct = "Correct advice", Incorrect = "Incorrect advice"))) +
    scale_colour_manual(values = group_colours, labels = short_groups) +
    labs(title = title, subtitle = "Each point is one participant's HP-minus-LP change across their two aided blocks.",
      x = "Change in trust (HP - LP)", y = "Change in agreement (pp)",
      caption = "Exploratory, unadjusted paired changes. Reliability also switches across blocks; ratings follow the block.\nThese points do not isolate a trust effect. The saved adjusted analyses show no robust trust association across methods.")
  save(p, "11_exploratory_H4_trust_association", "Exploratory", "H4", "Does within-person trust track advice following?", title,
    "One empirical point per participant for each advice correctness panel. X is HP-minus-LP six-item trust (equivalently the difference of within-person centred scores); Y is HP-minus-LP answered-trial agreement in percentage points. Colours identify opposite reliability assignments. These are unadjusted paired changes: pressure, reliability and block position vary concurrently. No fitted line, slope, interval or significance bracket is plotted; a two-condition bracket is not meaningful on individual change scatter points; Morey is not applicable to individual scatter points. Post-block ratings do not support causal inference.", changes, trust_cells)

  # H5: observed agreement after each trial history, paired within participant-
  # block. These raw means are explicitly distinguished from the adjusted model.
  lag_raw <- prep(read("H5_participant_sequence", TRUE)) %>% mutate(level = previous_aid_error, value = 100*agreement)
  lag <- time_pressure_morey(lag_raw, c("pattern", "pressure", "reliability"), c("Correct", "Error"))
  lag$summary <- lag$summary %>% mutate(level = factor(level, c("Correct", "Error")))
  title <- "H5: Observed agreement changes little after aid errors"
  p <- ggplot(lag$summary, aes(level, value, colour = reliability, group = reliability)) +
    geom_line(linewidth = .8) + geom_errorbar(aes(ymin = lo, ymax = hi), width = .1, linewidth = 1.05) +
    geom_point(size = 4.2) + facet_wrap(~pressure, labeller = as_labeller(pressure_labels)) +
    scale_colour_manual(values = colours, labels = reliability_labels) +
    scale_x_discrete(labels = c(Correct = "After correct advice", Error = "After wrong advice")) +
    scale_y_continuous(breaks = seq(50,100,10)) +
    labs(title = title, subtitle = "Observed next-trial agreement following the aid's previous correct or incorrect recommendation.",
      x = NULL, y = "Next-trial agreement (%)",
      caption = "Exploratory. Observed participant means + Morey 95% CIs over the two histories within each participant-block.\nConsecutive answered trials only. Unadjusted for current advice, previous response error and trial position.")
  history_pairs <- lag_raw %>% select(participant_id,pattern,pressure,reliability,level,value) %>%
    pivot_wider(names_from = level,values_from = value)
  a <- history_pairs %>% group_by(pattern,pressure,reliability) %>% group_modify(~{
    tt <- t.test(.x$Error-.x$Correct)
    tibble(estimate = unname(tt$estimate),lower = tt$conf.int[1],upper = tt$conf.int[2],p_raw = tt$p.value,n = nrow(.x))
  }) %>% ungroup() %>% mutate(p_holm = p.adjust(p_raw,"holm"),symbol = ifelse(p_holm < .05,"*","ns"),
    contrast = paste(pressure,reliability,"Observed after error - after correct advice"),hypothesis = "H5",
    test_source = "Observed paired t test; Holm across the four pressure x reliability history comparisons",
    x1 = 1,x2 = 2,bracket_colour = unname(colours[as.character(reliability)])) %>% place(lag$summary,c("pressure","reliability"))
  a <- a %>% mutate(y = top+2.2,tip = y-.5,text_y = y+.2)
  p <- bracket(p,a,"12_exploratory_H5_error_adjustment") + coord_cartesian(ylim = c(55,98)) +
    labs(caption = "Exploratory. Observed means + Morey 95% CIs over the two histories within each participant-block.\nPaired t tests (Holm across 4 cells): * p < .05; ns p >= .05. No covariate adjustment.")
  save(p, "12_exploratory_H5_error_adjustment", "Exploratory", "H5", "Does an aid error change agreement on the next trial?", title,
    "Equal-weight participant agreement proportions after correct/error advice in the same block, restricted to consecutive answered trials. Two-condition Morey normalization separately within pattern x pressure x reliability, over the two histories. Unlike the saved lag model, these empirical means do not standardise current advice correctness or adjust previous response error/progress; new paired t tests of the raw history differences are Holm-adjusted across four pressure x reliability cells; saved adjusted-model p-values are not attached to them. Morey intervals apply to history comparisons within a cell, not across reliabilities.", lag$summary, lag$participants)

  catalog <- bind_rows(catalog)
  stopifnot(nrow(catalog) == 12,
    identical(catalog$set, c(rep("Main",9),rep("Exploratory",3))),
    identical(catalog$hypothesis, c(paste0("H",1:9),paste0("H",3:5))))
  for (stem in names(annotations)) {
    i <- match(stem,catalog$filename)
    export(annotations[[stem]] %>% mutate(test_family = hypothesis,
      hypothesis = catalog$hypothesis[i],set = catalog$set[i]),paste0(stem,"_annotations"))
  }
  write_csv(catalog, file.path(out, "figure_catalog.csv"))
  # PDF pages have the same dimensions as the individual PNG/PDF exports.
  cairo_pdf(file.path(out, "all_hypotheses.pdf"), width = 12, height = 6.75, onefile = TRUE)
  tryCatch(invisible(lapply(figures, print)), finally = dev.off())
  cairo_pdf(file.path(out, "primary_hypotheses.pdf"), width = 12, height = 6.75, onefile = TRUE)
  tryCatch(invisible(lapply(figures[catalog$filename[catalog$set == "Main"]], print)), finally = dev.off())
  escape <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE)
  cards <- vapply(seq_len(nrow(catalog)), function(i) paste0(
    '<article><h2>', catalog$order[i], '. ', catalog$set[i], ' ', catalog$hypothesis[i], ': ', escape(catalog$question[i]), '</h2><a href="', catalog$filename[i], '.png"><img src="',
    catalog$filename[i], '.png" alt="', escape(catalog$key_message[i]), '"></a><p><a href="', catalog$filename[i],
    '.png">PNG</a> | <a href="', catalog$filename[i], '.pdf">PDF</a> | <a href="data/', catalog$filename[i],
    '_data.csv">Plotted data</a></p><p>', escape(catalog$notes[i]), '</p></article>'), character(1))
  writeLines(c('<!doctype html><html><head><meta charset="utf-8"><title>Time-pressure hypothesis figures</title>',
    '<style>body{font:18px Helvetica,Arial,sans-serif;color:#202A31;max-width:1200px;margin:32px auto;padding:0 24px}h1{font-size:32px}h2{font-size:24px}article{margin:40px 0 56px}img{width:100%;border:1px solid #e2e6e8}p{line-height:1.5}a{color:#1976a3}</style></head><body>',
    '<h1>Time-pressure hypothesis figures</h1>',
    sprintf('<p>%d participants. Nine main hypotheses (H1-H9), followed by three remaining exploratory hypotheses (original H3-H5). Each figure is 16:9: 3600 x 2025 PNG (300 dpi), plus vector PDF. Insert at full slide width without cropping.</p>', n),
    '<p><a href="primary_hypotheses.pdf">Nine primary figures (PDF)</a> | <a href="all_hypotheses.pdf">All twelve figures (PDF)</a> | <a href="figure_catalog.csv">Figure index and notes</a></p>',
    '<p>Main H7, H8 and H9 were originally exploratory H2, H6 and H1, respectively, specified after inspecting the main results. Their inclusion in the primary presentation set does not change their analysis history or the saved test adjustment families. The remaining exploratory figures retain their original H3-H5 identifiers.</p>',
    '<p>All figures show observed participant data. Morey intervals describe complete paired condition sets within allocation groups; between-group means and paired effects retain ordinary t intervals. Exploratory H4 shows individual paired changes; exploratory H5 shows unadjusted observed history means. Brackets use saved model Holm tests (* p &lt; .05; ns p &gt;= .05), with a dagger where the participant test differs. Exploratory H5 uses matching unadjusted paired tests with Holm correction across four cells. Main H8 symbols test total gain versus zero; exploratory H4 scatter points have no comparison bracket. Saved analyses are unchanged. CI overlap is not a formal test.</p>',
    cards, '</body></html>'), file.path(out, "index.html"))
  source_paths <- unique(c(consumed, normalizePath(file.path(root, "plot_time_pressure_slides.R"))))
  write_csv(tibble(path = source_paths, md5 = unname(tools::md5sum(source_paths))), file.path(out, "data/input_checksums.csv"))
  artifacts <- c(unlist(lapply(names(figures), function(stem) paste0(stem, c(".pdf", ".png")))), "all_hypotheses.pdf", "primary_hypotheses.pdf")
  write_csv(tibble(file = artifacts, bytes = file.info(file.path(out, artifacts))$size,
    md5 = unname(tools::md5sum(file.path(out, artifacts)))), file.path(out, "data/artifact_manifest.csv"))
  writeLines(capture.output(sessionInfo()), file.path(out, "data/session_info.txt"))
  message("Saved 9 main and 3 exploratory hypothesis figures (16:9 PNG/PDF), primary/combined PDFs and index to ", out)
  invisible(catalog)
}

if (sys.nframe() == 0) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
  root <- dirname(normalizePath(script)); args <- commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) <= 2)
  plot_time_pressure_slides(if (length(args) >= 1) args[1] else file.path(root, "analysis_outputs/semester2_2026_behavioural"),
    if (length(args) >= 2) args[2] else file.path(root, "plots/semester2_2026_behavioural"), root)
}
