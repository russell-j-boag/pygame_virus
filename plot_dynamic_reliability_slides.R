# One 16:9 figure per H1-H4 / E1-E2, using saved analysis results.
# H4 is promoted from original E3; source labels and adjustment families persist.
# Rscript plot_dynamic_reliability_slides.R [analysis_dir] [plot_dir]

# Morey (2008), doi:10.20982/tqmp.04.2.p061. Normalize separately within
# between-participant groups. These are condition-pattern CIs, not effect CIs.
dynamic_morey <- function(d, groups, levels, log_scale = FALSE) {
  k <- length(levels)
  stopifnot(k > 1, !anyDuplicated(levels), all(is.finite(d$value)),
    !anyDuplicated(d[c(groups, "participant_id", "level")]), setequal(d$level, levels))
  complete <- d %>% group_by(across(all_of(c(groups, "participant_id")))) %>%
    summarise(complete = n() == k && setequal(level, levels), .groups = "drop")
  if (!all(complete$complete)) stop("Morey intervals require complete repeated-measures cells.")
  d <- d %>% group_by(across(all_of(c(groups, "participant_id")))) %>%
    mutate(person_mean = mean(value)) %>% group_by(across(all_of(groups))) %>%
    mutate(normalized = value-person_mean+mean(value)) %>% ungroup()
  s <- d %>% group_by(across(all_of(c(groups, "level")))) %>%
    summarise(n = n(), center = mean(value),
      se_analysis = sd(normalized)/sqrt(n)*sqrt(k/(k-1)), .groups = "drop")
  stopifnot(all(s$n > 1))
  s <- s %>% mutate(k = k, morey_factor = sqrt(k/(k-1)),
    margin = qt(.975,n-1)*se_analysis,
    value = if (log_scale) exp(center) else center,
    lo = if (log_scale) exp(center-margin) else center-margin,
    hi = if (log_scale) exp(center+margin) else center+margin,
    interval = "Cousineau-Morey within-participant 95% CI",
    normalization_groups = paste(groups,collapse = ";"),
    normalization_levels = paste(levels,collapse = ";"),
    analysis_scale = if (log_scale) "log RT" else "original units")
  list(summary = s, participants = d)
}

dynamic_ordinary <- function(d, keys) {
  stopifnot(all(is.finite(d$value)), !anyDuplicated(d[c(keys,"participant_id")]))
  s <- d %>% group_by(across(all_of(keys))) %>%
    summarise(n = n(), se_analysis = sd(value)/sqrt(n), value = mean(value), .groups = "drop")
  stopifnot(all(s$n > 1))
  s %>% mutate(lo = value-qt(.975,n-1)*se_analysis, hi = value+qt(.975,n-1)*se_analysis,
    interval = "Ordinary participant mean 95% t CI", analysis_scale = "original units")
}

plot_dynamic_reliability_slides <- function(input, plot_base, root) {
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(readr); library(ggplot2); library(patchwork)
  })
  consumed <- character()
  read <- function(name) {
    path <- normalizePath(file.path(input,paste0(name,".csv")),mustWork = TRUE)
    consumed <<- c(consumed,path)
    read_csv(path,show_col_types = FALSE)
  }
  stopifnot(file.exists(file.path(input,"COMPLETE.txt")))
  # Validate before creating output directories or changing any artifacts.
  for (name in c("analysis_artifacts","input_manifest","source_checksums")) {
    m <- read(name)
    if (!all(file.exists(m$path)) || !identical(unname(tools::md5sum(m$path)),m$md5))
      stop("Stale or changed analysis inputs/artifacts: ",name,call. = FALSE)
  }
  out <- file.path(plot_base,"presentation")
  dir.create(file.path(out,"data"),recursive = TRUE,showWarnings = FALSE)
  unlink(file.path(out,"COMPLETE.txt"))
  cache <- file.path(tempdir(),"fontconfig-cache")
  dir.create(cache,showWarnings = FALSE); Sys.setenv(XDG_CACHE_HOME = cache)
  # Same typography, grid and colours as the current ATC/virus and time-pressure figures.
  slide_theme <- function() theme_minimal(base_size = 18,base_family = "Helvetica") + theme(
    text = element_text(colour = "#202A31"), panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "#E2E6E8",linewidth = .45),
    axis.text = element_text(size = 17,colour = "#303B44"), axis.title = element_text(size = 18),
    axis.title.x = element_text(margin = margin(t = 7)), axis.title.y = element_text(margin = margin(r = 7)),
    strip.text = element_text(face = "bold",size = 18), panel.spacing = grid::unit(.9,"lines"),
    plot.title = element_text(face = "bold",size = 24,lineheight = 1.02,margin = margin(b = 6)),
    plot.subtitle = element_text(size = 14,lineheight = 1.08,margin = margin(b = 9)),
    plot.caption = element_text(size = 12.5,hjust = 0,lineheight = 1.12,margin = margin(t = 8)),
    plot.title.position = "plot", plot.caption.position = "plot",
    legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 15),
    legend.margin = margin(0,0,0,0),legend.key.height = grid::unit(17,"pt"),
    plot.margin = margin(10,14,10,10),plot.background = element_rect(fill = "white",colour = NA))
  old_theme <- theme_set(slide_theme()); on.exit(theme_set(old_theme),add = TRUE)
  groups <- c("CAL65","CAL90"); phases <- c("P1","P2","P3")
  colours <- c(CAL65 = "#1976A3",CAL90 = "#C56924")
  counts <- read("cohort_counts"); N <- sum(counts$n_participants)
  labels <- setNames(paste0(groups," (n = ",counts$n_participants[match(groups,counts$group)],")"),groups)
  colour_scale <- function() scale_colour_manual(values = colours,breaks = groups,labels = labels,drop = FALSE)
  phase_labels <- c("P1\n95%","P2\n70%","P3\n95%")
  tests <- read("planned_contrasts"); subjective <- read("subjective_contrasts")
  diagnostics <- read("model_diagnostics")
  annotations <- list(); figures <- list(); catalog <- list()
  export <- function(d,stem) write_csv(d,file.path(out,"data",paste0(stem,".csv")))
  # No tests are refitted or re-adjusted for this reduced display subset.
  test <- function(outcome_name,contrast_name,family_name = "core") {
    d <- tests %>% filter(outcome == outcome_name,family == family_name,contrast == contrast_name)
    m <- filter(d,method == "Mixed model"); s <- filter(d,method == "Participant sensitivity")
    stopifnot(nrow(m) == 1,nrow(s) == 1)
    model_name <- paste0(outcome_name,if (family_name == "adaptation") "_trend" else "")
    diagnostic_ok <- diagnostics$inference_available[match(model_name,diagnostics$model)]
    ok <- isTRUE(m$inference_available) && isTRUE(diagnostic_ok) && is.finite(m$p_holm)
    other_ok <- isTRUE(s$inference_available) && is.finite(s$p_holm)
    disagreement <- ok && other_ok && ((m$p_holm < .05) != (s$p_holm < .05))
    tibble(outcome = outcome_name,family = family_name,contrast = contrast_name,
      model_estimate = m$estimate,model_p_holm = m$p_holm,participant_p_holm = s$p_holm,
      inference_available = ok,participant_disagreement = disagreement,
      symbol = paste0(if (!ok) "NA" else if (m$p_holm < .05) "*" else "ns",
        if (disagreement) "\u2020" else if (!other_ok) "\u2021" else ""),
      test_source = "Saved mixed-model Holm test; original outcome/family/method adjustment")
  }
  hc3_test <- function(outcome_name,contrast_name) {
    x <- subjective %>% filter(outcome == outcome_name,contrast == contrast_name)
    stopifnot(nrow(x) == 1)
    x %>% mutate(model_p_holm = p_holm,model_estimate = estimate,
      symbol = ifelse(!inference_available | !is.finite(p_holm),"NA",ifelse(p_holm < .05,"*","ns")),
      test_source = "Saved participant linear-model HC3 Holm test")
  }
  place <- function(a,d,facets = character(),gap = .13) {
    bounds <- d %>% group_by(across(all_of(facets))) %>%
      summarise(bottom = min(lo),top = max(hi),.groups = "drop") %>% mutate(span = pmax(top-bottom,.1))
    if (length(facets)) a <- left_join(a,bounds,by = facets) else a <- bind_cols(a,bounds[rep(1,nrow(a)),])
    if (!"lane" %in% names(a)) a$lane <- 1
    if (!"bracket_colour" %in% names(a)) a$bracket_colour <- "#58636C"
    a %>% mutate(y = top+span*(.13+gap*(lane-1)),tip = y-span*.025,text_y = y+span*.012)
  }
  bracket <- function(p,a,stem,panel) {
    stopifnot(all(is.finite(a$x1)),all(a$x1 < a$x2))
    annotations[[stem]] <<- bind_rows(annotations[[stem]],mutate(a,panel = panel,annotation = "Bracket"))
    p + geom_segment(data = a,aes(x = x1,xend = x2,y = y,yend = y),inherit.aes = FALSE,colour = a$bracket_colour,linewidth = .65) +
      geom_segment(data = a,aes(x = x1,xend = x1,y = tip,yend = y),inherit.aes = FALSE,colour = a$bracket_colour,linewidth = .65) +
      geom_segment(data = a,aes(x = x2,xend = x2,y = tip,yend = y),inherit.aes = FALSE,colour = a$bracket_colour,linewidth = .65) +
      geom_text(data = a,aes(x = (x1+x2)/2,y = text_y,label = symbol),inherit.aes = FALSE,colour = a$bracket_colour,vjust = 0,size = 5)
  }
  test_caption <- "Model Holm tests: * p < .05; ns p >= .05; \u2020 participant test differs."
  probability_breaks <- function(limits) {
    x <- pretty(limits,n = 5); x[x >= 0 & x <= 100]
  }
  morey_caption <- "Observed means + Cousineau-Morey within-participant 95% CIs, within each calibration group."
  compose <- function(p,title,subtitle,caption) p + plot_layout(guides = "collect") +
    plot_annotation(title = title,subtitle = subtitle,caption = caption,theme = slide_theme())
  save <- function(p,stem,hypothesis,question,notes,d,participants,source_hypothesis = hypothesis) {
    stopifnot(!stem %in% names(figures))
    figures[[stem]] <<- p
    for (ext in c("png","pdf")) ggsave(file.path(out,paste0(stem,".",ext)),p,
      width = 12,height = 6.75,units = "in",dpi = 300,bg = "white",device = if (ext == "pdf") cairo_pdf else "png")
    export(d,paste0(stem,"_data")); export(participants,paste0(stem,"_participants"))
    if (source_hypothesis != hypothesis) notes <- paste0("Promoted from exploratory ",source_hypothesis,
      " to the primary presentation set as ",hypothesis,". Original analysis and Holm families are retained. ",notes)
    catalog[[stem]] <<- tibble(order = length(figures),hypothesis,
      set = if (startsWith(hypothesis,"H")) "Main" else "Exploratory",
      source_hypothesis,source_set = if (startsWith(source_hypothesis,"H")) "Main" else "Exploratory",
      question,filename = stem,
      width_in = 12,height_in = 6.75,png_width = 3600,png_height = 2025,notes)
  }
  mean_panel <- function(s,levels,xlabels,title,ylab,join = TRUE) {
    s <- s %>% mutate(x = match(level,levels)+ifelse(group == "CAL65",-.045,.045))
    p <- ggplot(s,aes(x,value,colour = group,group = group))
    if (join) p <- p + geom_line(linewidth = .8)
    p + geom_errorbar(aes(ymin = lo,ymax = hi),width = .1,linewidth = 1.05) + geom_point(size = 4.2) +
      colour_scale() + scale_x_continuous(breaks = seq_along(levels),labels = xlabels,expand = expansion(add = .4)) +
      scale_y_continuous(breaks = if (grepl("(%)",ylab,fixed = TRUE)) probability_breaks else waiver(),
        expand = expansion(mult = c(.08,.12))) + labs(x = NULL,y = ylab,title = title) +
      theme(plot.title = element_text(size = 20))
  }
  within_tests <- function(outcome,contrast,family = "core",x1 = 1,x2 = 2,lane = 1) {
    bind_rows(lapply(seq_along(groups),function(i) test(outcome,paste0(groups[i],": ",contrast),family) %>%
      mutate(group = groups[i],x1 = x1+c(-.045,.045)[i],x2 = x2+c(-.045,.045)[i],
        lane = lane+i-1,bracket_colour = colours[[groups[i]]])))
  }
  perf <- read("participant_performance"); agreement <- read("participant_agreement")
  # H1 and H2: observed condition pattern plus the paired change that defines
  # the group interaction. Change-score intervals must NOT be Morey-normalized.
  paired_figure <- function(raw,levels,xlabels,outcome,prefix,comparison,stem,hypothesis,
                            title,subtitle,ylab,change_label,question) {
    raw <- mutate(raw,panel = "Condition means")
    s <- dynamic_morey(raw,"group",levels)
    p <- mean_panel(s$summary,levels,xlabels,"Observed condition means",ylab)
    a <- bind_rows(lapply(seq_along(groups),function(i)
      test(outcome,paste0(prefix,groups[i],": ",comparison)) %>%
        mutate(x1 = 1+c(-.045,.045)[i],x2 = 2+c(-.045,.045)[i],lane = i,bracket_colour = colours[[groups[i]]])))
    p <- bracket(p,place(a,s$summary),stem,"Condition means")
    delta <- raw %>% select(participant_id,group,level,value) %>% pivot_wider(names_from = level,values_from = value) %>%
      transmute(participant_id,group,value = .data[[levels[2]]]-.data[[levels[1]]],panel = "Paired change")
    ds <- dynamic_ordinary(delta,"group") %>% mutate(level = group)
    q <- mean_panel(ds,groups,groups,"Change within each participant",change_label,FALSE) +
      geom_hline(yintercept = 0,linetype = "dashed",colour = "#87929D")
    a <- test(outcome,paste0(prefix,"CAL90 - CAL65: ",comparison)) %>%
      mutate(x1 = .955,x2 = 2.045,symbol = paste("Group difference:",symbol))
    q <- bracket(q,place(a,ds),stem,"Paired change") + guides(colour = "none")
    save(compose(p | q,title,subtitle,paste(
      "Left: Morey 95% CIs over the two conditions. Right: ordinary 95% t CIs of paired changes.",
      test_caption,"Group brackets test the difference in changes; model-only findings need caution.",sep = "\n")),
      stem,hypothesis,question,
      "Equal participant weighting. Morey normalization over the two displayed conditions within calibration group. Right panel uses individual paired changes with ordinary t intervals, not normalized condition intervals. Brackets use the saved model response-scale contrasts and original Holm families; dagger marks disagreement with participant sensitivity. No model refit or new hypothesis test.",
      bind_rows(mutate(s$summary,panel = "Condition means"),mutate(ds,panel = "Paired change")),
      bind_rows(s$participants,delta))
  }
  paired_figure(agreement %>% filter(advice == "Incorrect",phase %in% c("P1","P2")) %>%
      transmute(participant_id,group,level = phase,value = 100*agreement),
    c("P1","P2"),phase_labels[1:2],"agreement","Incorrect: ","P2 - P1","01_h1_relative_competence","H1",
    "H1: Does relative competence change the response to the drop?",
    "Incorrect-advice agreement: the key test compares the P2-minus-P1 change between groups.",
    "Incorrect-advice agreement (%)","Agreement change (pp)",
    "Does CAL90 reduce incorrect-advice agreement more than CAL65 when reliability drops?")
  paired_figure(perf %>% filter(stage %in% c("Manual pre","P2")) %>%
      transmute(participant_id,group,level = stage,value = 100*accuracy),
    c("Manual pre","P2"),c("Manual pre","P2: 70% aid"),"accuracy","","P2 - Manual pre","02_h2_degraded_aid_benefit","H2",
    "H2: Does the degraded aid benefit CAL65 more?",
    "Compare accuracy in the 70% phase with each participant's manual-pre baseline.",
    "Accuracy (%)","Accuracy gain (pp)","Does the degraded aid benefit CAL65 more than CAL90?")

  # H3: one aligned panel per outcome, with recovery and residual contrasts.
  recovery_raw <- bind_rows(perf %>% filter(stage %in% phases) %>%
      transmute(participant_id,group,level = stage,value = 100*accuracy,panel = "Accuracy"),
    agreement %>% transmute(participant_id,group,level = phase,value = 100*agreement,
      panel = paste(advice,"advice")))
  recovery <- dynamic_morey(recovery_raw,c("group","panel"),phases)
  recovery_panels <- lapply(c("Accuracy","Correct advice","Incorrect advice"),function(panel_name) {
    s <- filter(recovery$summary,panel == panel_name)
    p <- mean_panel(s,phases,phase_labels,panel_name,if (panel_name == "Accuracy") "Accuracy (%)" else "Agreement (%)")
    prefix <- if (panel_name == "Accuracy") "" else paste0(sub(" advice","",panel_name),": ")
    a <- bind_rows(lapply(seq_along(groups),function(i) bind_rows(lapply(1:2,function(j)
      test(if (panel_name == "Accuracy") "accuracy" else "agreement",paste0(prefix,groups[i],": P3 - P",3-j)) %>%
        mutate(x1 = 3-j+c(-.045,.045)[i],x2 = 3+c(-.045,.045)[i],lane = (j-1)*2+i,bracket_colour = colours[[groups[i]]])))))
    bracket(p,place(a,s,gap = .12),"03_h3_recovery",panel_name)
  })
  save(compose(wrap_plots(recovery_panels,nrow = 1),"H3: Does behaviour recover when aid reliability returns?",
    "P3 versus P2 tests recovery; P3 versus P1 tests the remaining difference from the initial 95% phase.",
    paste(morey_caption,"Normalised over P1-P3 separately for each outcome; ns does not establish equivalence.",test_caption,sep = "\n")),
    "03_h3_recovery","H3","Does behaviour recover, and does P3 differ from P1?",
    "Three-phase Morey intervals within each calibration group and outcome. Four brackets per panel show P3-P2 and P3-P1 separately in CAL65 and CAL90. Different outcome panels use different y scales. Accuracy and advice agreement overlap mathematically and are not independent evidence. Fixed phase order confounds reliability with time and experience; nonsignificance is not equivalence.",
    recovery$summary,recovery$participants)

  # E1: keep all available participants. Incomplete incorrect-advice bins use
  # ordinary intervals rather than silently removing people to obtain Morey CIs.
  bins <- read("participant_trajectories") %>% mutate(value = 100*value,level = as.character(bin),
    panel = recode(outcome,Accuracy = "Accuracy",`Correct advice agreement` = "Correct advice",`Incorrect advice agreement` = "Incorrect advice"))
  bin_summary <- bind_rows(lapply(unique(bins$panel),function(panel_name) {
    d <- filter(bins,panel == panel_name)
    if (panel_name != "Incorrect advice") dynamic_morey(d,c("group","phase","panel"),as.character(1:4))$summary else
      dynamic_ordinary(d,c("group","phase","panel","level"))
  })) %>% mutate(x = (match(phase,phases)-1)*400+(as.integer(level)-.5)*100)
  adaptation_panels <- lapply(c("Accuracy","Correct advice","Incorrect advice"),function(panel_name) {
    d <- filter(bin_summary,panel == panel_name)
    p <- ggplot(d,aes(x,value,colour = group,group = interaction(group,phase))) +
      geom_vline(xintercept = c(400,800),linetype = "dashed",colour = "#A6AFB7") +
      geom_line(linewidth = .6) + geom_errorbar(aes(ymin = lo,ymax = hi),width = 16,linewidth = .65) +
      geom_point(size = 2.5) + colour_scale() +
      scale_x_continuous(breaks = c(200,600,1000),labels = phase_labels,expand = expansion(add = 55)) +
      scale_y_continuous(breaks = probability_breaks,expand = expansion(mult = c(.1,.13))) +
      labs(x = NULL,y = if (panel_name == "Accuracy") "Accuracy (%)" else "Agreement (%)",title = panel_name) +
      theme(plot.title = element_text(size = 20))
    if (panel_name == "Incorrect advice") {
      # Dedicated count rows keep labels clear of the wide bin intervals.
      count_floor <- min(d$lo)-.08*diff(range(c(d$lo,d$hi)))
      p <- p + geom_text(aes(y = count_floor-ifelse(group == "CAL90",4,0),label = n),
        size = 2.8,show.legend = FALSE)
    }
    a <- bind_rows(lapply(phases,function(phase_name) bind_rows(lapply(seq_along(groups),function(i)
      test(if (panel_name == "Accuracy") "accuracy" else "agreement",
        paste(if (panel_name == "Accuracy") "All" else sub(" advice","",panel_name),phase_name,groups[i],"end - start",sep = ": "),"adaptation") %>%
        mutate(phase = phase_name,x1 = (match(phase_name,phases)-1)*400+20,x2 = match(phase_name,phases)*400-20,
          lane = i,bracket_colour = colours[[groups[i]]])))))
    bracket(p,place(a,d),"05_e1_within_phase_adaptation",panel_name)
  })
  save(compose(wrap_plots(adaptation_panels,nrow = 1),"E1: Does behaviour adapt within each phase?",
    "Observed 100-trial bins; brackets test full-phase start-to-end change in the saved trend models.",
    paste("Accuracy / correct advice: Morey 95% CIs over four bins within each group and phase.",
      "Incorrect advice: ordinary 95% t CIs; coloured rows below show n per bin. Missing cells are not zero.",test_caption,sep = "\n")),
    "05_e1_within_phase_adaptation","E1","Does behaviour adapt within phases?",
    "Complete accuracy and correct-advice panels use four-bin Morey normalization within group and phase. Incorrect-advice bins retain all available participants with ordinary t intervals because some bins have no incorrect advice. Brackets refer to conditional logistic-model endpoints at trials 1 and 400, not to a test comparing the plotted first/last bin means. Dagger compares those tests with the saved participant linear-probability slope sensitivity. Curves join observed bins only; no fitted model line is implied.",
    bin_summary,bins)

  # E2: manual accuracy and the whole RT sequence, including manual pre/post.
  manual <- dynamic_morey(perf %>% filter(stage %in% c("Manual pre","Manual post")) %>%
    transmute(participant_id,group,level = stage,value = 100*accuracy),"group",c("Manual pre","Manual post"))
  stages <- c("Manual pre",phases,"Manual post")
  rt <- dynamic_morey(perf %>% transmute(participant_id,group,level = stage,value = mean_log_rt),"group",stages,TRUE)
  p <- mean_panel(manual$summary,c("Manual pre","Manual post"),c("Manual\npre","Manual\npost"),"Manual accuracy","Accuracy (%)")
  p <- bracket(p,place(within_tests("accuracy","Manual post - Manual pre","manual_change"),manual$summary),"06_e2_speed_and_manual_change","Manual accuracy")
  q <- mean_panel(rt$summary,stages,c("Manual\npre","P1\n95%","P2\n70%","P3\n95%","Manual\npost"),"Correct-response speed","Geometric correct RT (s)")
  a <- bind_rows(within_tests("correct_rt","P1 - Manual pre","speed",1,2),
    within_tests("correct_rt","P2 - P1","speed",2,3),within_tests("correct_rt","P3 - P2","speed",3,4),
    within_tests("correct_rt","Manual post - Manual pre","manual_change",1,5,3)) %>%
    mutate(x1 = x1+ifelse(lane <= 2,.055,0),x2 = x2-ifelse(lane <= 2,.055,0))
  q <- bracket(q,place(a,rt$summary),"06_e2_speed_and_manual_change","Correct RT")
  save(compose((p | q) + plot_layout(widths = c(1,1.7)),"E2: What changes in speed and manual performance?",
    "Manual pre/post accuracy and correct-response time across the full sequence.",
    paste("Observed means + Morey 95% CIs: two manual accuracy cells; five log-RT cells, then back-transformed.",
      "RT brackets compare adjacent aided stages and manual pre/post. Changes also include practice and fatigue.",test_caption,sep = "\n")),
    "06_e2_speed_and_manual_change","E2","What changes in response speed and manual performance accompany the sequence?",
    "Manual accuracy uses two-condition Morey intervals. RTs are equal-weight participant geometric means, normalized over all five log-RT stages before back-transformation. All correct finite positive RTs are retained. Accuracy timeouts count as incorrect. RT tests use the saved log-RT model; no between-group comparison is inferred from Morey interval overlap. RT brackets include P1-manual pre, P2-P1, P3-P2, and manual post-manual pre.",
    bind_rows(mutate(manual$summary,panel = "Manual accuracy"),mutate(rt$summary,panel = "Correct RT")),
    bind_rows(mutate(manual$participants,panel = "Manual accuracy"),mutate(rt$participants,panel = "Correct RT")))

  # H4 (original E3): between-subject group means; Morey is not applicable.
  ratings <- read("participant_ratings"); rating_summaries <- list(); rating_lines <- list()
  rating_panel <- function(value_name,title,ylab) {
    d <- ratings %>% transmute(participant_id,group,value = .data[[value_name]])
    s <- dynamic_ordinary(d,"group") %>% mutate(level = group,panel = value_name)
    rating_summaries[[value_name]] <<- s
    p <- mean_panel(s,groups,groups,title,ylab,FALSE) + theme(legend.position = "none",
      axis.text = element_text(size = 14),axis.title = element_text(size = 15),plot.title = element_text(size = 18))
    a <- hc3_test(value_name,"CAL90 - CAL65") %>% mutate(x1 = .955,x2 = 2.045)
    bracket(p,place(a,s),"04_h4_subjective_evaluation",paste(value_name,"group"))
  }
  association_panel <- function(value_name,title,ylab) {
    path <- normalizePath(file.path(input,"models",paste0(value_name,"_association.rds")),mustWork = TRUE)
    consumed <<- c(consumed,path); fit <- readRDS(path)
    line <- bind_rows(lapply(groups,function(g) {
      r <- range(ratings$agreement[ratings$group == g]); tibble(group = g,agreement = seq(r[1],r[2],length.out = 40))
    })) %>% mutate(agreement_10pp = (agreement-mean(ratings$agreement))/.1)
    line$value <- as.numeric(predict(fit,newdata = line)); line$panel <- value_name
    rating_lines[[value_name]] <<- line
    a <- hc3_test(value_name,"P2 incorrect-advice agreement: per 10 pp, adjusted for group") %>%
      mutate(panel = paste(value_name,"association"),annotation = "Slope label")
    annotations[["04_h4_subjective_evaluation"]] <<- bind_rows(annotations[["04_h4_subjective_evaluation"]],a)
    ggplot(ratings,aes(100*agreement,.data[[value_name]],colour = group)) +
      geom_point(size = 2.1,alpha = .6) + geom_line(data = line,aes(y = value),linewidth = .85) + colour_scale() +
      labs(x = "P2 incorrect-advice agreement (%)",y = ylab,title = paste0(title," (",a$symbol,")")) +
      theme(axis.text = element_text(size = 14),axis.title = element_text(size = 15),plot.title = element_text(size = 18))
  }
  p <- rating_panel("trust","Final trust by group","Mean trust (1-5)")
  q <- rating_panel("perceived_aid_error","Perceived aid accuracy error","Error (pp)") +
    geom_hline(yintercept = 0,linetype = "dashed",colour = "#87929D")
  r <- association_panel("trust","Group-adjusted trust association","Mean trust (1-5)")
  s <- association_panel("perceived_aid_error","Group-adjusted error association","Error (pp)")
  save(compose((p | q) / (r | s),"H4: How do final evaluations relate to behaviour?",
    "Ratings follow all 1,200 aided trials. Accuracy error = perceived minus realised whole-block aid accuracy.",
    paste("Top: observed group means + ordinary 95% t CIs. Bottom: participant points and saved group-adjusted slopes.",
      "HC3 Holm tests: * p < .05; ns p >= .05. Brackets test group differences; title symbols test slopes.",sep = "\n")),
    "04_h4_subjective_evaluation","H4","How do final evaluations relate to group and degraded-phase behaviour?",
    "Ratings are measured once after the whole aided sequence. Group means have ordinary t intervals; Morey normalization is not appropriate. Group brackets and slope symbols use saved participant linear-model HC3 tests with their original outcome-wise Holm adjustment. Association lines are group-adjusted common slopes from the saved models; their stars test the slope per 10 percentage points of P2 incorrect-advice agreement. No bracket is drawn between scatter points. Associations do not establish causation.",
    bind_rows(rating_summaries),ratings,source_hypothesis = "E3")
  export(bind_rows(rating_lines),"04_h4_subjective_evaluation_association_lines")

  presentation_order <- c("H1","H2","H3","H4","E1","E2")
  catalog <- bind_rows(catalog) %>% arrange(match(hypothesis,presentation_order)) %>% mutate(order = row_number())
  registry <- read("hypothesis_registry")
  stopifnot(identical(catalog$hypothesis,presentation_order),!anyDuplicated(catalog$source_hypothesis),
    setequal(catalog$source_hypothesis,registry$hypothesis))
  figures <- figures[catalog$filename]
  for (stem in names(annotations)) {
    i <- match(stem,catalog$filename)
    export(annotations[[stem]] %>% mutate(hypothesis = catalog$hypothesis[i],set = catalog$set[i],
      source_hypothesis = catalog$source_hypothesis[i]),paste0(stem,"_annotations"))
  }
  write_csv(catalog,file.path(out,"figure_catalog.csv"))
  for (bundle in c("all_hypotheses","primary_hypotheses")) {
    selected <- if (bundle == "all_hypotheses") figures else figures[catalog$filename[catalog$set == "Main"]]
    cairo_pdf(file.path(out,paste0(bundle,".pdf")),width = 12,height = 6.75,onefile = TRUE)
    tryCatch(invisible(lapply(selected,print)),finally = dev.off())
  }
  escape <- function(x) gsub("<","&lt;",gsub("&","&amp;",x,fixed = TRUE),fixed = TRUE)
  cards <- vapply(seq_len(nrow(catalog)),function(i) paste0('<article><h2>',catalog$hypothesis[i],": ",
    escape(catalog$question[i]),'</h2><a href="',catalog$filename[i],'.png"><img src="',catalog$filename[i],
    '.png" alt="',escape(catalog$question[i]),'"></a><p><a href="',catalog$filename[i],'.png">PNG</a> | <a href="',
    catalog$filename[i],'.pdf">PDF</a> | <a href="data/',catalog$filename[i],'_data.csv">Plotted data</a> | <a href="data/',
    catalog$filename[i],'_annotations.csv">Tests</a></p><p>',escape(catalog$notes[i]),'</p></article>'),character(1))
  writeLines(c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Dynamic reliability hypothesis figures</title>',
    '<style>body{font:18px Helvetica,Arial,sans-serif;color:#202a31;max-width:1200px;margin:32px auto;padding:0 24px}h1{font-size:32px}h2{font-size:24px}article{margin:40px 0 56px}img{width:100%;border:1px solid #e2e6e8}p{line-height:1.5}a{color:#1976a3}</style></head><body>',
    '<h1>Dynamic reliability: one figure per hypothesis</h1>',
    sprintf('<p>%d participants. Four primary hypotheses (H1-H4), followed by E1-E2. All hypotheses were specified after data collection. Each figure is 16:9: 3600 x 2025 PNG at 300 dpi, plus vector PDF. Insert at full slide width without cropping.</p>',N),
    '<p><a href="primary_hypotheses.pdf">H1-H4 PDF</a> | <a href="all_hypotheses.pdf">All six figures PDF</a> | <a href="figure_catalog.csv">Figure index and methods</a></p>',
    '<p>H4 was promoted from exploratory E3 to the primary presentation set. The catalog preserves its original label; the saved analysis and test adjustment families are unchanged.</p>',
    '<p>Observed participant means are equally weighted. Morey intervals describe within-group condition patterns; they are not confidence intervals for between-group differences or paired effects. CI overlap is not a significance test. Change scores and single ratings retain ordinary t intervals. <a href="https://www.tqmp.org/RegularArticles/vol04-2/p061/p061.pdf">Morey (2008)</a>.</p>',
    '<p>Brackets use saved mixed-model Holm tests with their original adjustment families (* p &lt; .05; ns p &gt;= .05). A dagger marks disagreement with the participant sensitivity test; a double dagger means participant inference is unavailable; NA means model inference is unavailable. H4 uses saved HC3 linear-model tests. No analyses are refitted. Conditional binary models show extra participant-cell variation; model-only findings need caution. All numerical tests are exported with each figure.</p>',
    '<p>Reliability follows a fixed phase order, so changes also include time and experience. Agreement does not identify advice-caused switching. Accuracy and advice agreement overlap and are not independent evidence. Nonsignificance does not establish recovery or equivalence.</p>',
    cards,'</body></html>'),file.path(out,"index.html"))
  paths <- unique(c(consumed,normalizePath(file.path(root,"plot_dynamic_reliability_slides.R"))))
  write_csv(tibble(path = paths,md5 = unname(tools::md5sum(paths))),file.path(out,"input_checksums.csv"))
  # Remove only generated artifacts superseded by this explicit renaming.
  # New figures and bundles have been successfully written before cleanup.
  obsolete <- c("04_e1_within_phase_adaptation","05_e2_speed_and_manual_change","06_e3_subjective_evaluation")
  for (stem in obsolete) unlink(c(file.path(out,paste0(stem,c(".png",".pdf"))),
    file.path(out,"data",paste0(stem,c("_data.csv","_participants.csv","_annotations.csv","_association_lines.csv")))))
  artifacts <- list.files(out,pattern = "\\.(pdf|png|csv|html)$",recursive = TRUE,full.names = TRUE)
  artifacts <- artifacts[basename(artifacts) != "plot_manifest.csv"]
  write_csv(tibble(path = normalizePath(artifacts),md5 = unname(tools::md5sum(artifacts))),file.path(out,"plot_manifest.csv"))
  writeLines(format(Sys.time(),tz = "UTC"),file.path(out,"COMPLETE.txt"))
  message("Saved six hypothesis figures (16:9 PNG/PDF), combined PDFs and index: ",out)
  invisible(catalog)
}

if (sys.nframe() == 0L) {
  script <- sub("^--file=","",grep("^--file=",commandArgs(),value = TRUE)[1])
  root <- dirname(normalizePath(script)); args <- commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) <= 2)
  plot_dynamic_reliability_slides(if (length(args)) args[1] else file.path(root,"analysis_outputs/semester2_2026_behavioural"),
    if (length(args) > 1) args[2] else file.path(root,"plots/semester2_2026_behavioural"),root)
}
