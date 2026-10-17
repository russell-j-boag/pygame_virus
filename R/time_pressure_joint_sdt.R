# Matched aided-block summaries and five-contrast families for both SDT framings.
# Dependencies are loaded by plot_time_pressure_hypotheses.R; no model refitting.
time_pressure_joint_sdt <- function(advice, task, original_advice_tests, out) {
  dir.create(out, recursive=TRUE, showWarnings=FALSE)
  keys <- c("participant_id","pattern","pressure","reliability","correction")
  a <- advice %>% select(all_of(keys), bias=automation_bias, dprime) %>% mutate(domain="Advice")
  t <- task %>% filter(actor=="human",mode=="Automation") %>%
    select(all_of(keys),bias=black_bias,dprime) %>% mutate(domain="Task")
  stopifnot(!anyDuplicated(a[keys]),!anyDuplicated(t[keys]),nrow(a)==nrow(t),
    nrow(anti_join(a,t,by=keys))==0)
  scores <- bind_rows(a,t) %>% pivot_longer(c(bias,dprime),names_to="measure",values_to="value")
  grid <- expand_grid(pressure=c("HP","LP"),reliability=c("65%","95%"))
  methods <- aided_contrasts(grid);names(methods) <- sub("^Trust ","",names(methods))
  pieces <- list()
  for(co in c("loglinear","extreme_only"))for(dm in c("Advice","Task"))for(ms in c("bias","dprime")) {
    d <- filter(scores,correction==co,domain==dm,measure==ms)
    pieces[[length(pieces)+1]] <- participant_sensitivity(d,"value",grid,methods,ms) %>%
      mutate(domain=dm,measure=ms,correction=co,units="SDT z units",
        adjustment_family=paste(dm,ms,co,"five contrasts"),legacy_hypothesis=if(ms=="bias")"H5" else "H9")
  }
  tests <- bind_rows(pieces) %>% group_by(domain,measure,correction) %>%
    mutate(p_holm=p.adjust(p_raw,"holm")) %>% ungroup() %>% group_by(correction) %>%
    mutate(p_holm_all_20=p.adjust(p_raw,"holm")) %>% ungroup()
  # Promoting the alternate H5/H9 must not change its estimates, intervals or tests.
  check <- filter(tests,domain=="Advice") %>% mutate(outcome=ifelse(measure=="bias","automation_bias","dprime")) %>%
    inner_join(original_advice_tests,by=c("outcome","contrast","correction"),suffix=c("","_original"))
  stopifnot(nrow(check)==20)
  for(k in c("estimate","lower","upper","p_raw","p_holm"))
    stopifnot(max(abs(check[[k]]-check[[paste0(k,"_original")]]))<1e-12)
  means <- filter(scores,correction=="loglinear") %>% group_by(domain,measure,pattern,pressure,reliability) %>%
    summarise(n=n(),mean=mean(value),se=sd(value)/sqrt(n),.groups="drop") %>%
    mutate(lower=mean-qt(.975,n-1)*se,upper=mean+qt(.975,n-1)*se)
  sensitivity <- tests %>% select(domain,measure,contrast,correction,estimate,p_holm) %>%
    pivot_wider(names_from=correction,values_from=c(estimate,p_holm)) %>%
    mutate(same_direction=sign(estimate_loglinear)==sign(estimate_extreme_only),
      same_holm_decision=(p_holm_loglinear<.05)==(p_holm_extreme_only<.05))
  weights <- bind_rows(lapply(names(methods),function(nm)mutate(grid,contrast=nm,weight=methods[[nm]])))
  write_csv(scores,file.path(out,"participant_scores.csv"));write_csv(means,file.path(out,"descriptive_sdt.csv"))
  write_csv(tests,file.path(out,"contrasts.csv"));write_csv(weights,file.path(out,"contrast_weights.csv"))
  write_csv(sensitivity,file.path(out,"correction_sensitivity.csv"))
  notes <- c(
    "Joint task/advice SDT: exploratory outcomes. Presentation order does not change analysis history.",
    "Advice H5: bias=-c=(z(H)+z(F))/2, where H/F are agreement conditional on correct/incorrect advice.",
    "Advice H9: dprime=z(H)-z(F), using those same agreement rates.",
    "Task bias=-c and dprime use BLACK/WHITE stimuli and final responses. Positive task bias favours BLACK.",
    "Only aided, answered trials enter the matched five-contrast families. Calibration and manual blocks are excluded here.",
    "Task scores describe participant-plus-aid performance. The two framings summarise the same decisions; they are not independent evidence.",
    "Five contrasts per domain/measure/correction: 95%-65% at HP and LP; HP-LP at 65% and 95%; reliability-by-pressure interaction.",
    "Simple effects compare independent allocation groups (Welch). Interaction compares participant sums between patterns, retaining within-person covariance.",
    "Holm adjustment across five tests separately for each domain/measure/correction. Holm across all 20 tests per correction is also exported.",
    "Loglinear correction is primary; extreme-only correction is a sensitivity analysis. Sparse incorrect-advice cells remain a limitation.",
    "Intervals are pointwise 95% t CIs. Completion and all-trial accuracy are separate; these conditional scores exclude timeout costs.",
    "No inference compares or subtracts an advice SDT index from a task SDT index. Their reference categories differ.")
  writeLines(notes,file.path(out,"methods.txt"))
  list(scores=scores,means=means,tests=tests,sensitivity=sensitivity,notes=notes)
}

time_pressure_joint_sdt_plots <- function(joint) {
  colours <- c("65%"="#C56924","95%"="#1976A3")
  theme_joint <- function() theme_minimal(base_size=14,base_family="Helvetica") + theme(
    text=element_text(colour="#202A31"),panel.grid.minor=element_blank(),
    panel.grid.major.x=element_blank(),strip.text=element_text(face="bold",size=14),
    axis.title=element_text(size=13),axis.text=element_text(size=12),legend.position="bottom",
    plot.title=element_text(size=21,face="bold"),plot.subtitle=element_text(size=12),
    plot.caption=element_text(size=10,hjust=0),plot.title.position="plot",plot.caption.position="plot",
    plot.margin=margin(10,14,10,10))
  limits <- lapply(c("bias","dprime"),function(ms) {
    d <- filter(joint$means,measure==ms);r <- range(c(0,d$lower,d$upper));r+c(-1,1)*.09*diff(r)
  });names(limits) <- c("bias","dprime")
  labels <- c("Advice.bias"="Advice acceptance (-c)","Task.bias"="BLACK response preference (-c)",
    "Advice.dprime"="Advice selectivity (d-prime)","Task.dprime"="Stimulus discrimination (d-prime)")
  panel <- function(dm,ms) {
    d <- filter(joint$means,domain==dm,measure==ms) %>% mutate(pressure=factor(pressure,c("HP","LP")))
    ggplot(d,aes(pressure,mean,colour=reliability)) + geom_hline(yintercept=0,linetype="dotted",colour="#9BA5AD") +
      geom_errorbar(aes(ymin=lower,ymax=upper),width=.08,position=position_dodge(.3),linewidth=.8) +
      geom_point(size=3.3,position=position_dodge(.3)) + scale_colour_manual(values=colours,labels=c("65% aid","95% aid")) +
      scale_x_discrete(labels=c(HP="HP (1 s)",LP="LP (3 s)")) + coord_cartesian(ylim=limits[[ms]]) +
      labs(title=labels[[paste(dm,ms,sep=".")]],x=NULL,y=if(ms=="bias")"Bias (-c)" else "Discrimination (d-prime)",colour=NULL) +
      theme_joint() + theme(plot.title=element_text(size=15))
  }
  overview <- ((panel("Advice","bias") | panel("Task","bias")) /
    (panel("Advice","dprime") | panel("Task","dprime"))) + plot_layout(guides="collect") +
    plot_annotation(title="Aid following and task performance: two SDT perspectives",
      subtitle="Left: agreement with correct/incorrect advice. Right: final BLACK/WHITE classification.",
      caption=paste("Exploratory; aided answered trials only. Participant means + pointwise 95% t CIs; loglinear correction.",
        "Axes are aligned within rows; the indices describe different decisions. Tests are in the matched contrast figure.",sep="\n"),theme=theme_joint())
  tests <- filter(joint$tests,correction=="loglinear")
  contrast_names <- c("HP 95% - 65%","LP 95% - 65%","65% HP - LP","95% HP - LP","Reliability effect HP - LP")
  contrast_labels <- c("Reliability: HP","Reliability: LP","Pressure: 65% aid","Pressure: 95% aid","Reliability x pressure")
  contrast_limits <- lapply(c("bias","dprime"),function(ms) {
    d <- filter(tests,measure==ms);r <- range(c(0,d$lower,d$upper));r+c(-1,1)*.13*diff(r)
  });names(contrast_limits) <- c("bias","dprime")
  test_panel <- function(dm,ms) {
    d <- filter(tests,domain==dm,measure==ms) %>% mutate(contrast=factor(contrast,rev(contrast_names)),
      symbol=ifelse(p_holm<.05,"*","ns"))
    ggplot(d,aes(estimate,contrast)) + geom_vline(xintercept=0,linetype="dashed",colour="#9BA5AD") +
      geom_errorbar(aes(xmin=lower,xmax=upper),orientation="y",width=.15,linewidth=.7,colour="#1976A3") +
      geom_point(size=2.7,colour="#1976A3") + geom_text(aes(x=upper,label=symbol),hjust=-.35,size=3.2) +
      scale_y_discrete(labels=setNames(contrast_labels,contrast_names)) + coord_cartesian(xlim=contrast_limits[[ms]]) +
      labs(title=labels[[paste(dm,ms,sep=".")]],x="Difference (SDT z units)",y=NULL) +
      theme_joint() + theme(plot.title=element_text(size=15),axis.text.y=element_text(size=10),panel.grid.major.y=element_blank())
  }
  contrasts <- ((test_panel("Advice","bias") | test_panel("Task","bias")) /
    (test_panel("Advice","dprime") | test_panel("Task","dprime"))) +
    plot_annotation(title="Matched reliability and pressure contrasts",
      subtitle="Reliability = 95% minus 65%; pressure = HP minus LP; interaction = reliability effect at HP minus LP.",
      caption=paste("Exploratory; loglinear correction; pointwise 95% t CIs. * Holm p < .05; ns p >= .05 (five tests separately in each panel).",
        "Simple effects compare allocation groups; the interaction retains within-person pairing. This figure does not test differences between the SDT framings.",sep="\n"),theme=theme_joint())
  list(overview=overview,contrasts=contrasts)
}
