# Exploratory stimulus-response SDT; preserves existing advice-SDT and model outputs.
# Usage: Rscript analyse_time_pressure_task_sdt.R [input_csv] [analysis_dir] [plot_dir]
.task_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
.task_file <- if (length(.task_files)) tail(.task_files, 1)[[1]] else
  sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
TASK_SDT_ROOT <- dirname(normalizePath(.task_file))
source(file.path(TASK_SDT_ROOT, "analyse_time_pressure.R"), local = TRUE)
rm(.task_files, .task_file)

TASK_SDT_OUTCOMES <- c("dprime", "black_bias")

task_sdt_rates <- function(k, n, correction) {
  check(correction %in% c("loglinear", "extreme_only"), "Unknown rate correction")
  check(all(is.finite(k) & is.finite(n) & n >= 0 & k >= 0 & k <= n & k == floor(k) & n == floor(n)), "Invalid SDT counts")
  if (correction == "loglinear") ifelse(n > 0, (k + .5)/(n + 1), NA_real_) else
    ifelse(n == 0, NA_real_, ifelse(k == 0, .5/n, ifelse(k == n, 1-.5/n, k/n)))
}

task_sdt_scores <- function(counts, correction = "loglinear") {
  counts %>% mutate(correction = correction,
    assessable = n_signal > 0 & n_noise > 0,
    hit_rate = task_sdt_rates(hits, n_signal, .env$correction),
    false_alarm_rate = task_sdt_rates(false_alarms, n_noise, .env$correction),
    dprime = qnorm(hit_rate) - qnorm(false_alarm_rate),
    black_bias = (qnorm(hit_rate) + qnorm(false_alarm_rate))/2,
    criterion_c = -black_bias)
}

task_sdt_counts <- function(dat) {
  keys <- c("participant_id", "source_participant_id", "run_timestamp", "pattern", "pressure",
            "mode", "reliability", "condition_deadline_code", "block_idx")
  make_counts <- function(d, actor, response_column) {
    d %>% group_by(across(all_of(keys))) %>% summarise(
      actor = actor, n_scored = n(), n_signal = sum(stimulus == "BLACK"), n_noise = sum(stimulus == "WHITE"),
      hits = sum(stimulus == "BLACK" & .data[[response_column]] == "BLACK"),
      misses = sum(stimulus == "BLACK" & .data[[response_column]] == "WHITE"),
      false_alarms = sum(stimulus == "WHITE" & .data[[response_column]] == "BLACK"),
      correct_rejections = sum(stimulus == "WHITE" & .data[[response_column]] == "WHITE"), .groups = "drop")
  }
  human <- make_counts(filter(dat, !timeout), "human", "response")
  matched <- make_counts(filter(dat, mode == "Automation", !timeout), "aid_matched", "aid_label")
  all_aid <- make_counts(filter(dat, mode == "Automation"), "aid_all", "aid_label")
  bind_rows(human, matched, all_aid) %>% mutate(
    extreme_hit = hits == 0 | hits == n_signal,
    extreme_false_alarm = false_alarms == 0 | false_alarms == n_noise)
}

task_sdt_context <- function(dat) {
  dat %>% group_by(participant_id, source_participant_id, run_timestamp, pattern, pressure, mode, reliability,
                   condition_deadline_code, block_idx) %>% summarise(
    n_trials = n(), n_answered = sum(!timeout), n_timeouts = sum(timeout),
    n_signal_all = sum(stimulus == "BLACK"), n_noise_all = sum(stimulus == "WHITE"),
    n_signal_answered = sum(stimulus == "BLACK" & !timeout), n_noise_answered = sum(stimulus == "WHITE" & !timeout),
    completion = mean(!timeout), all_trial_accuracy = mean(accuracy),
    answered_accuracy = if (any(!timeout)) mean(accuracy[!timeout]) else NA_real_,
    black_completion = mean(!timeout[stimulus == "BLACK"]),
    white_completion = mean(!timeout[stimulus == "WHITE"]),
    n_nonpositive_answered_rt = sum(!timeout & is.finite(rt_s) & rt_s <= 0),
    n_missing_answered_rt = sum(!timeout & !is.finite(rt_s)), .groups = "drop")
}

task_sdt_contrasts <- function(scores) {
  human <- filter(scores, actor == "human")
  grid <- expand_grid(mode = c("Manual", "Automation"), pressure = c("HP", "LP"), pattern = patterns)
  methods <- performance_contrasts(grid)
  cell <- function(m, p, g) as.numeric(grid$mode == m & grid$pressure == p & grid$pattern == g)
  gain <- function(p, g) cell("Automation", p, g)-cell("Manual", p, g)
  methods[["HP aid gain: 95% - 65%"]] <- gain("HP", patterns[1])-gain("HP", patterns[2])
  methods[["LP aid gain: 95% - 65%"]] <- gain("LP", patterns[2])-gain("LP", patterns[1])
  family <- function(nm) ifelse(grepl("Manual HP - Manual LP", nm, fixed = TRUE), "H1",
    ifelse(grepl("Aided HP - Manual LP", nm, fixed = TRUE), "H3", "H2"))
  weight_rows <- bind_rows(lapply(names(methods), function(nm) mutate(grid, contrast = nm, weight = methods[[nm]], hypothesis = family(nm), domain = "performance")))
  # Difference tables retain explicitly named component scores, not human-only
  # counts/rates/criteria that could be mistaken for properties of a difference.
  aided <- filter(human, mode == "Automation") %>% select(
    participant_id, source_participant_id, run_timestamp, pattern, pressure, mode,
    reliability, condition_deadline_code, block_idx, correction,
    human_dprime = dprime, human_black_bias = black_bias)
  matched <- filter(scores, actor == "aid_matched")
  benchmark <- inner_join(aided, matched %>% select(participant_id, pressure, correction, all_of(TASK_SDT_OUTCOMES)),
    by = c("participant_id", "pressure", "correction")) %>%
    rename(aid_dprime = dprime, aid_black_bias = black_bias) %>%
    mutate(difference = "human minus matched aid", dprime = human_dprime-aid_dprime,
      black_bias = human_black_bias-aid_black_bias)
  manual <- filter(human, mode == "Manual")
  gains <- inner_join(aided, manual %>% select(participant_id, pressure, correction, all_of(TASK_SDT_OUTCOMES)),
    by = c("participant_id", "pressure", "correction")) %>%
    rename(manual_dprime = dprime, manual_black_bias = black_bias) %>%
    mutate(difference = "aided human minus manual human", dprime = human_dprime-manual_dprime,
      black_bias = human_black_bias-manual_black_bias)
  aided_grid <- expand_grid(pressure = c("HP", "LP"), reliability = c("65%", "95%"))
  bench_methods <- aided_contrasts(aided_grid)
  names(bench_methods) <- sub("^Trust ", "", names(bench_methods))
  for (p in c("HP", "LP")) for (r in c("65%", "95%"))
    bench_methods[[paste(p, r, "versus zero")]] <- as.numeric(aided_grid$pressure == p & aided_grid$reliability == r)
  weight_rows <- bind_rows(weight_rows, bind_rows(lapply(names(bench_methods), function(nm)
    mutate(aided_grid, contrast = nm, weight = bench_methods[[nm]], hypothesis = "H7", domain = "aid_benchmark"))))
  results <- bind_rows(lapply(c("loglinear", "extreme_only"), function(co) {
    bind_rows(lapply(TASK_SDT_OUTCOMES, function(value) {
      perf <- participant_sensitivity(filter(human, correction == co), value, grid, methods, value) %>%
        mutate(hypothesis = family(contrast), domain = "performance")
      bench <- participant_sensitivity(filter(benchmark, correction == co), value, aided_grid, bench_methods, value) %>%
        mutate(hypothesis = "H7", domain = "aid_benchmark")
      bind_rows(perf, bench) %>% mutate(correction = co, units = "SDT z units")
    }))
  })) %>% group_by(correction, hypothesis, outcome) %>% mutate(p_holm = p.adjust(p_raw, "holm"),
    adjustment_family = paste("Task SDT", hypothesis, outcome, correction)) %>% ungroup() %>%
    group_by(correction) %>% mutate(p_holm_all = p.adjust(p_raw, "holm")) %>% ungroup()
  list(contrasts = results, weights = weight_rows, gains = gains, benchmark = benchmark)
}

task_sdt_describe <- function(d, groups, outcomes) {
  d %>% pivot_longer(all_of(outcomes), names_to = "outcome", values_to = "value") %>%
    group_by(across(all_of(c(groups, "outcome")))) %>% summarise(n = n(), mean = mean(value),
      se = sd(value)/sqrt(n), .groups = "drop") %>%
    mutate(lower = mean-qt(.975,n-1)*se, upper = mean+qt(.975,n-1)*se)
}

task_sdt_theme <- function() {
  theme_minimal(base_size = 16, base_family = "Helvetica") + theme(
    text = element_text(colour = "#202A31"), panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"), plot.title = element_text(size = 23, face = "bold"),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    plot.caption = element_text(size = 10.5, hjust = 0, margin = margin(t = 9)),
    plot.title.position = "plot", plot.caption.position = "plot", plot.margin = margin(12,16,12,12))
}

task_sdt_plots <- function(scores, context, analysis, plots, return_bundle = FALSE) {
  dir.create(file.path(plots,"data"), recursive = TRUE, showWarnings = FALSE)
  human <- filter(scores, actor == "human", correction == "loglinear") %>%
    mutate(condition = ifelse(mode == "Manual", "Manual", paste(reliability,"aid")))
  ctx <- context %>% mutate(condition = ifelse(mode == "Manual", "Manual", paste(reliability,"aid")))
  colours <- c("Manual"="#737D85", "65% aid"="#C56924", "95% aid"="#1976A3")
  human$condition <- factor(human$condition, names(colours))
  ctx$condition <- factor(ctx$condition, names(colours))
  outcome_labels <- c(dprime="Task discrimination (d-prime)", black_bias="BLACK response bias (-c)")
  pattern_labels <- vapply(patterns, function(g) paste0(g," | n = ",n_distinct(filter(human,pattern==g)$participant_id)), character(1))
  names(pattern_labels) <- patterns
  figures <- list(); catalog <- list()
  save <- function(stem, title, p, data, annotations = NULL) {
    figures[[stem]] <<- p
    for (ext in c("png","pdf")) ggsave(file.path(plots,paste0(stem,".",ext)),p,width=12,height=6.75,dpi=300,bg="white",
      device=if(ext=="pdf") cairo_pdf else "png")
    write_csv(data,file.path(plots,"data",paste0(stem,"_data.csv")))
    if(!is.null(annotations))write_csv(annotations,file.path(plots,"data",paste0(stem,"_annotations.csv")))
    catalog[[stem]] <<- tibble(filename=stem,title=title,version="Exploratory task SDT")
  }
  overview <- task_sdt_describe(human,c("pattern","pressure","condition"),TASK_SDT_OUTCOMES) %>%
    mutate(pressure=factor(pressure,c("HP","LP")))
  for(value in TASK_SDT_OUTCOMES) {
    d <- filter(overview,outcome==value)
    title <- if(value=="dprime") "Task discrimination with and without advice" else "Category preference with and without advice"
    p <- ggplot(d,aes(pressure,mean,colour=condition)) +
      geom_hline(yintercept=0,colour="#AAB2B8",linetype="dotted") +
      geom_errorbar(aes(ymin=lower,ymax=upper),width=.08,position=position_dodge(.35),linewidth=.85) +
      geom_point(position=position_dodge(.35),size=3.6) +
      facet_wrap(~pattern,labeller=as_labeller(pattern_labels)) + scale_colour_manual(values=colours) +
      scale_x_discrete(labels=c(HP="High pressure (1 s)",LP="Low pressure (3 s)")) +
      labs(title=title,subtitle=if(value=="dprime") "Final responses scored against the stimulus; higher values indicate better category discrimination." else
        "Positive -c favours BLACK responses; this is category bias, not willingness to follow advice.",
        x=NULL,y=outcome_labels[[value]],colour=NULL,
        caption=paste0("Exploratory; all ",n_distinct(human$participant_id)," participants retained. Answered trials only; loglinear correction.\nParticipant means + pointwise 95% t CIs. Manual comparisons retain the same allocation group; see paired-contrast figures.")) + task_sdt_theme()
    save(if(value=="dprime")"01_task_discrimination" else "02_task_category_bias",title,p,d)
  }
  for(domain in c("gain","benchmark")) {
    raw <- filter(if(domain=="gain")analysis$gains else analysis$benchmark,correction=="loglinear")
    d <- task_sdt_describe(raw,c("pattern","pressure","reliability"),TASK_SDT_OUTCOMES)
    a <- filter(analysis$contrasts,correction=="loglinear",
      if(.env$domain=="gain") hypothesis=="H2" & grepl("Automation - Manual",contrast,fixed=TRUE) else hypothesis=="H7" & grepl("versus zero",contrast,fixed=TRUE))
    d$contrast <- if(domain=="gain")paste(d$pattern,d$pressure,"Automation - Manual") else paste(d$pressure,d$reliability,"versus zero")
    d <- left_join(d,a %>% select(outcome,contrast,p_raw,p_holm,adjustment_family),by=c("outcome","contrast")) %>%
      group_by(outcome) %>% mutate(label_y=upper+.08*max(.2,max(upper)-min(lower)),symbol=ifelse(p_holm<.05,"*","ns")) %>% ungroup() %>%
      mutate(pressure=factor(pressure,c("HP","LP")),condition=paste(reliability,"aid"),
             outcome=factor(outcome,TASK_SDT_OUTCOMES))
    title <- if(domain=="gain")"H2: Paired benefits and costs of the aid" else "H7: Participant-plus-aid versus aid alone"
    p <- ggplot(d,aes(pressure,mean,colour=condition)) + geom_hline(yintercept=0,linetype="dashed",colour="#737D85") +
      geom_errorbar(aes(ymin=lower,ymax=upper),width=.08,position=position_dodge(.35),linewidth=.85) +
      geom_point(position=position_dodge(.35),size=3.6) +
      geom_text(aes(y=label_y,label=symbol),position=position_dodge(.35),size=4.2,show.legend=FALSE) +
      facet_wrap(~outcome,scales="free_y",labeller=as_labeller(outcome_labels)) + scale_colour_manual(values=colours) +
      scale_x_discrete(labels=c(HP="High pressure (1 s)",LP="Low pressure (3 s)")) +
      scale_y_continuous(expand=expansion(mult=c(.12,.18))) +
      labs(title=title,subtitle=if(domain=="gain")"Automation minus manual at the same pressure, calculated within each participant." else
        "Human final decision minus the aid's recommendation, evaluated on identical human-answered trials.",
        x=NULL,y="Paired difference (SDT z units)",colour=NULL,
        caption=paste0("Exploratory; participant mean differences + pointwise 95% t CIs. * Holm p < .05; ns p >= .05 (nine ",if(domain=="gain")"H2" else "H7"," tests per outcome).\nPositive d-prime differences indicate a discrimination advantage; bias differences indicate a category-preference shift. Timeouts are excluded.")) + task_sdt_theme()
    save(if(domain=="gain")"03_H2_paired_aid_gain" else "05_H7_aid_benchmark",title,p,d,a)
  }
  a <- filter(analysis$contrasts,correction=="loglinear",hypothesis%in%c("H1","H3")) %>%
    mutate(group=ifelse(grepl(patterns[1],contrast,fixed=TRUE),patterns[1],patterns[2]),
      label=paste0(hypothesis,": ",ifelse(hypothesis=="H1","Manual HP - Manual LP","Aided HP - Manual LP"),"\n",group),
      symbol=ifelse(p_holm<.05,"*","ns"),outcome=factor(outcome,TASK_SDT_OUTCOMES)) %>% arrange(hypothesis,group,outcome)
  a$label <- factor(a$label,rev(unique(a$label)))
  p <- ggplot(a,aes(estimate,label)) + geom_vline(xintercept=0,linetype="dashed",colour="#737D85") +
    geom_errorbar(aes(xmin=lower,xmax=upper),orientation="y",width=.15,colour="#1976A3",linewidth=.9) +
    geom_point(size=3.5,colour="#1976A3") + geom_text(aes(x=upper,label=symbol),hjust=-.35,size=4) +
    facet_wrap(~outcome,scales="free_x",labeller=as_labeller(outcome_labels)) +
    scale_x_continuous(expand=expansion(mult=c(.12,.25))) +
    labs(title="H1 and H3: Pressure costs and the remaining gap",subtitle="Paired participant contrasts; H3 compares aided HP with manual LP in each allocation group.",
      x="Paired difference (SDT z units)",y=NULL,
      caption="Exploratory; pointwise 95% t CIs. * Holm p < .05; ns p >= .05 (two tests per hypothesis/outcome).\nA nonsignificant H3 gap does not establish equivalence or noninferiority. In HP95_LP65, the HP aid is 95%; in HP65_LP95, it is 65%.") +
    task_sdt_theme() + theme(axis.text.y=element_text(size=10))
  save("04_H1_H3_pressure_and_compensation","H1 and H3: Pressure costs and the remaining gap",p,a,a)
  d <- task_sdt_describe(ctx,c("pattern","pressure","condition"),c("completion","answered_accuracy","all_trial_accuracy")) %>%
    mutate(pressure=factor(pressure,c("HP","LP")),outcome=factor(outcome,c("completion","answered_accuracy","all_trial_accuracy")))
  p <- ggplot(d,aes(pressure,mean,colour=condition)) +
    geom_errorbar(aes(ymin=lower,ymax=upper),position=position_dodge(.35),width=.08,linewidth=.75) +
    geom_point(position=position_dodge(.35),size=3) +
    facet_grid(outcome~pattern,switch="y",labeller=labeller(pattern=as_labeller(pattern_labels),outcome=as_labeller(c(completion="Completion",answered_accuracy="Answered\naccuracy",all_trial_accuracy="All-trial\naccuracy")))) +
    scale_colour_manual(values=colours) + scale_y_continuous(labels=function(x)paste0(round(100*x),"%")) +
    scale_x_discrete(labels=c(HP="High pressure (1 s)",LP="Low pressure (3 s)")) +
    labs(title="Completion and accuracy alongside task SDT",subtitle="Task SDT conditions on answering; overall accuracy also includes the cost of timeouts.",x=NULL,y=NULL,colour=NULL,
      caption="Exploratory descriptive context; participant means + pointwise 95% t CIs. Timeouts are incorrect in all-trial accuracy.\nWithin every participant/block: all-trial accuracy = completion x answered accuracy. Conditional measures do not identify timeout costs.") + task_sdt_theme() +
    theme(strip.text.y.left=element_text(size=11,angle=0),strip.placement="outside",axis.text=element_text(size=11))
  save("06_completion_and_accuracy","Completion and accuracy alongside task SDT",p,d)
  catalog <- bind_rows(catalog) %>% arrange(filename)
  cairo_pdf(file.path(plots,"task_sdt_hypotheses.pdf"),width=12,height=6.75,onefile=TRUE)
  tryCatch(invisible(lapply(catalog$filename,function(nm)print(figures[[nm]]))),finally=dev.off())
  write_csv(catalog,file.path(plots,"figure_catalog.csv"))
  if (return_bundle) list(catalog = catalog, figures = figures) else catalog
}

run_task_sdt <- function(input, out, plots) {
  cache <- file.path(tempdir(),"task-sdt-font-cache");dir.create(cache,showWarnings=FALSE);Sys.setenv(XDG_CACHE_HOME=cache)
  check(file.exists(input),"Input CSV does not exist")
  # Only dedicated child directories can be refreshed by this script.
  for(p in c(out,plots))check(basename(p)=="task_sdt","Outputs must use dedicated task_sdt directories")
  dir.create(out,recursive=TRUE,showWarnings=FALSE);dir.create(plots,recursive=TRUE,showWarnings=FALSE)
  unlink(file.path(out,"COMPLETE.txt"))
  sources <- c(input,file.path(TASK_SDT_ROOT,c("analyse_time_pressure_task_sdt.R","analyse_time_pressure.R","R/participant_identity.R","config/participant_identity_corrections.csv","python/virus_task.py")))
  hashes <- tibble(path=normalizePath(sources),md5=unname(tools::md5sum(sources)))
  raw <- read_identity_csv(input)
  check(all(raw$stimulus%in%c("BLACK","WHITE")),"Invalid stimulus category")
  counts <- raw %>% count(participant_id,condition_deadline_code)
  check(all(counts$n==ifelse(counts$condition_deadline_code=="CAL_LP",300,400)),"Incomplete block")
  check(all(table(counts$participant_id)==5) && setequal(counts$condition_deadline_code,c("CAL_LP","M_HP","A_HP","M_LP","A_LP")),"Unexpected condition coverage")
  check(all(raw$trial_deadline_s==ifelse(grepl("_HP$",raw$condition_deadline_code),1,3)),"Deadline mismatch")
  check(all(raw$time_pressure_condition==ifelse(grepl("_HP$",raw$condition_deadline_code),"HP","LP")),"Pressure mismatch")
  dat <- prepare_trials(raw)
  check(all(!is.na(dat$pattern)),"Unknown allocation group")
  check(all(dat$reliability[dat$mode=="Automation"]==ifelse(
    (dat$pattern[dat$mode=="Automation"]==patterns[1]) == (dat$pressure[dat$mode=="Automation"]=="HP"),"95%","65%")),"Reliability assignment mismatch")
  counts <- task_sdt_counts(dat);context <- task_sdt_context(dat)
  check(nrow(counts)==8*n_distinct(dat$participant_id),"Missing scored participant/block; no participant silently omitted")
  scores <- bind_rows(task_sdt_scores(counts),task_sdt_scores(counts,"extreme_only"))
  write_csv(counts,file.path(out,"stimulus_response_counts.csv"));write_csv(scores,file.path(out,"participant_task_sdt.csv"))
  check(all(scores$assessable),"A stimulus category has no answered trials; SDT contrasts unavailable")
  check(all(context$n_answered>0),"A block has no answered trials")
  check(max(abs(context$all_trial_accuracy-context$completion*context$answered_accuracy))<1e-12,"Completion identity failed")
  analysis <- task_sdt_contrasts(scores)
  check(all(is.finite(analysis$contrasts$p_raw)),"Nonfinite contrast inference")
  sensitivity <- analysis$contrasts %>% select(hypothesis,outcome,contrast,correction,estimate,p_holm) %>%
    pivot_wider(names_from=correction,values_from=c(estimate,p_holm)) %>%
    mutate(same_direction=sign(estimate_loglinear)==sign(estimate_extreme_only),same_holm_decision=(p_holm_loglinear<.05)==(p_holm_extreme_only<.05))
  write_csv(context,file.path(out,"completion_accuracy_context.csv"))
  write_csv(analysis$contrasts,file.path(out,"contrasts.csv"));write_csv(analysis$weights,file.path(out,"contrast_weights.csv"))
  write_csv(analysis$gains,file.path(out,"participant_aid_gains.csv"));write_csv(analysis$benchmark,file.path(out,"participant_aid_benchmark.csv"))
  write_csv(sensitivity,file.path(out,"correction_sensitivity.csv"))
  write_csv(dat %>% distinct(participant_id,source_participant_id,run_timestamp,pattern),file.path(out,"cohort.csv"))
  bundle <- task_sdt_plots(scores,context,analysis,plots,return_bundle=TRUE)
  catalog <- bundle$catalog
  notes <- c(
    "Exploratory task-performance SDT, added after inspection of existing results; original H1-H9 tests and advice-SDT outputs are unchanged.",
    "Signal = BLACK stimulus; positive response = BLACK classification, regardless of key mapping. H=P(BLACK response|BLACK stimulus); F=P(BLACK response|WHITE stimulus).",
    "dprime=z(H)-z(F); black_bias=-c=(z(H)+z(F))/2. Positive black_bias favours BLACK. This bias is not advice acceptance.",
    "Human SDT uses all answered trials, regardless of RT; calibration is excluded. No QC candidate exclusions are applied.",
    "The current loglinear correction adds 0.5 to every confusion-table cell, giving (k+0.5)/(n+1). Extreme-only correction is a sensitivity analysis.",
    "aid_matched scores use the aid recommendation on exactly the trials the human answered. aid_all scores are descriptive benchmarks on all aided trials, not the H7 test input.",
    "Aided dprime describes the participant-plus-aid system under conventional equal-variance SDT; it does not isolate sensory ability or establish how evidence was integrated.",
    "Completion, stimulus-specific completion, answered accuracy, and all-trial accuracy are exported. Selection into answering can differ across conditions; conditional SDT does not measure timeout costs.",
    "Participants are equally weighted. Within-pattern comparisons use paired participant contrasts; cross-pattern comparisons use independent groups with Welch-Satterthwaite inference.",
    "H1: manual HP-LP within each pattern (2 tests). H2: 4 paired aided-manual gains, 2 within-pattern HP-LP gain contrasts, their between-pattern difference, and 2 reliability comparisons of gains (9 tests).",
    "H3: aided HP minus manual LP within each pattern (2 tests). A nonsignificant remaining gap does not establish equivalence or noninferiority.",
    "H7: human-minus-matched-aid scores, 4 cell tests versus zero plus 5 pressure/reliability contrasts (9 tests). Same-trial pairing is preserved.",
    "Holm families are separate by hypothesis, outcome, and correction. An additional Holm correction across all 44 tests per correction is exported. CIs are pointwise 95% t intervals.",
    "Higher black_bias or a larger black_bias difference is a category-preference shift, not inherently a performance benefit.",
    "In participant_aid_gains.csv and participant_aid_benchmark.csv, dprime and black_bias are differences; human_, manual_, and aid_ columns retain the component scores. criterion_c and response rates appear only in the absolute-score table.",
    "Simply recoding the existing advice-conditioned agreement outcome as correctness re-expresses its old dprime/bias; this analysis instead uses the stimulus-response table.",
    "Rate-correction reference: Hautus (1995), https://doi.org/10.3758/BF03203619.")
  writeLines(c(notes,"",capture.output(print(filter(analysis$contrasts,correction=="loglinear"),n=Inf,width=Inf))),file.path(out,"report.txt"))
  writeLines(capture.output(sessionInfo()),file.path(out,"session_info.txt"))
  esc <- function(x)gsub("<","&lt;",gsub("&","&amp;",x,fixed=TRUE),fixed=TRUE)
  tests <- filter(analysis$contrasts,correction=="loglinear")
  rows <- apply(as.data.frame(tests %>% transmute(hypothesis,outcome,contrast,estimate=sprintf("%.3f",estimate),CI=sprintf("[%.3f, %.3f]",lower,upper),Holm_p=format.pval(p_holm,digits=3,eps=.001))),1,function(r)paste0("<tr>",paste0("<td>",esc(r),"</td>",collapse=""),"</tr>"))
  cards <- vapply(seq_len(nrow(catalog)),function(i)paste0('<article><h2>',catalog$title[i],'</h2><img src="',catalog$filename[i],'.png" alt="',catalog$title[i],'"><p><a href="',catalog$filename[i],'.pdf">PDF</a> | <a href="',catalog$filename[i],'.png">PNG</a> | <a href="data/',catalog$filename[i],'_data.csv">Plotted data</a></p></article>'),character(1))
  writeLines(c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Task-performance SDT</title><style>body{font:17px system-ui,sans-serif;max-width:1250px;margin:32px auto;padding:0 24px;color:#243746}img{width:100%}article{margin:32px 0}table{border-collapse:collapse;font-size:12px}td,th{padding:8px;border-bottom:1px solid #ddd;text-align:left}a{color:#1976a3}</style></head><body><h1>Task-performance SDT</h1>',
    paste0('<p>Exploratory analyses of ',n_distinct(dat$participant_id),' participants. No QC exclusions applied. Separate from advice-acceptance SDT.</p>'),
    '<p><a href="task_sdt_hypotheses.pdf">All six figures (PDF)</a> | <a href="../index.html">Existing hypothesis figures</a> | <a href="../alternates/index.html">Advice-based SDT</a> | <a href="data/contrasts.csv">All contrasts</a> | <a href="data/correction_sensitivity.csv">Correction sensitivity</a> | <a href="data/report.txt">Methods and full results</a></p>',
    '<p>BLACK is the signal and BLACK classification the positive response. Task d-prime measures category discrimination; positive -c favours BLACK. Aided scores describe the participant-plus-aid system. Human scores exclude timeouts; completion and all-trial accuracy are shown separately. The aid-alone comparison uses exactly the human-answered trials.</p>',cards,
    '<h2>Exploratory contrasts: loglinear correction</h2><p>Pointwise 95% t intervals; Holm adjustment within each hypothesis and outcome. Reliability comparisons retain the two allocation groups. A nonsignificant H3 gap is not evidence of equivalence.</p><table><thead><tr><th>Hypothesis</th><th>Outcome</th><th>Contrast</th><th>Estimate</th><th>95% CI</th><th>Holm p</th></tr></thead><tbody>',rows,'</tbody></table></body></html>'),file.path(plots,"index.html"))
  for(name in c("contrasts.csv","correction_sensitivity.csv","report.txt","cohort.csv","stimulus_response_counts.csv","participant_task_sdt.csv","participant_aid_gains.csv","participant_aid_benchmark.csv","completion_accuracy_context.csv","contrast_weights.csv"))
    file.copy(file.path(out,name),file.path(plots,"data",name),overwrite=TRUE)
  check(identical(unname(tools::md5sum(hashes$path)),hashes$md5),"Input or source changed during generation")
  write_csv(hashes,file.path(out,"input_checksums.csv"));write_csv(hashes,file.path(plots,"data/input_checksums.csv"))
  artifacts <- list.files(plots,recursive=TRUE,full.names=TRUE)
  artifacts <- artifacts[file.info(artifacts)$isdir==FALSE & basename(artifacts)!="artifact_manifest.csv"]
  write_csv(tibble(path=substring(artifacts,nchar(plots)+2),bytes=file.info(artifacts)$size,md5=unname(tools::md5sum(artifacts))),file.path(plots,"data/artifact_manifest.csv"))
  writeLines(paste("Completed",format(Sys.time(),tz="UTC")),file.path(out,"COMPLETE.txt"))
  message("Task SDT complete: ",n_distinct(dat$participant_id)," participants; ",nrow(analysis$contrasts)," contrasts across both corrections; report: ",file.path(plots,"index.html"))
  invisible(list(scores=scores,analysis=analysis,catalog=catalog,figures=bundle$figures))
}

if(sys.nframe()==0) {
  args <- commandArgs(trailingOnly=TRUE);check(length(args)<=3,"Usage: Rscript analyse_time_pressure_task_sdt.R [input_csv] [analysis_dir] [plot_dir]")
  if (!length(args)) {
    source(file.path(TASK_SDT_ROOT,"plot_time_pressure_hypotheses.R"),local=TRUE)
    plot_time_pressure_hypotheses(file.path(TASK_SDT_ROOT,"analysis_outputs/semester2_2026_behavioural"),
      file.path(TASK_SDT_ROOT,"plots/semester2_2026_behavioural"),TASK_SDT_ROOT)
  } else run_task_sdt(if(length(args)>=1)args[1] else file.path(TASK_SDT_ROOT,"data/semester2_2026_data/data_virus_all.csv"),
    if(length(args)>=2)args[2] else file.path(TASK_SDT_ROOT,"analysis_outputs/semester2_2026_behavioural/task_sdt"),
    if(length(args)>=3)args[3] else file.path(TASK_SDT_ROOT,"plots/semester2_2026_behavioural/presentation/task_sdt"))
}
