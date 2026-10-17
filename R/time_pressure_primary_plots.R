# Plot-only consumers of the revised primary analysis bundle.
primary_plot_theme <- function() theme_minimal(base_size=12,base_family="Helvetica") + theme(
  text=element_text(colour="#25323D"),panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
  plot.title=element_text(size=14,face="bold"),plot.subtitle=element_text(size=10),
  axis.title=element_text(size=11),axis.text=element_text(size=10),legend.position="bottom",
  plot.margin=margin(8,12,8,8),plot.caption=element_text(size=9,hjust=0),strip.text=element_text(face="bold"))
primary_p_label <- function(p) ifelse(is.na(p),"unavailable",ifelse(p<.001,"***",ifelse(p<.01,"**",ifelse(p<.05,"*","ns"))))
primary_p_text <- function(p) ifelse(is.na(p),"p unavailable",ifelse(p<.001,"p < .001",sprintf("p = %.3f",p)))
primary_cell_index <- function(d) match(paste(d$pressure,d$reliability),c("HP 65%","HP 95%","LP 65%","LP 95%"))
primary_trust_predictions <- function(model,associations,raw,component) {
  # Predict the exact quantities on the scatter axes from the saved H7 model.
  # Difference/mean vectors retain covariance between the two block predictions.
  mf <- model.frame(model);beta <- lme4::fixef(model)
  fixed_terms <- delete.response(terms(lme4::nobars(formula(model))))
  grand_trust <- mean(associations$trust)
  bind_rows(lapply(sort(unique(as.character(raw$pattern))),function(pattern) {
    participants <- filter(raw,.data$pattern==.env$pattern)
    grid <- tibble(pattern=pattern,x=seq(min(participants$x),max(participants$x),length.out=60))
    positions <- filter(associations,.data$pattern==.env$pattern) %>%
      group_by(pressure) %>% summarise(position=mean(block_position),.groups="drop")
    block <- function(pressure) {
      reliability <- if((pattern=="HP95_LP65" && pressure=="HP") ||
        (pattern=="HP65_LP95" && pressure=="LP"))"95%" else "65%"
      nd <- data.frame(pressure=factor(pressure,levels=levels(mf$pressure)),
        reliability=factor(reliability,levels=levels(mf$reliability)),
        trust_within=if(component=="trust_within")grid$x*(if(pressure=="HP").5 else -.5) else 0,
        trust_between=if(component=="trust_between")grid$x-grand_trust else 0,
        block_position=positions$position[positions$pressure==pressure])
      model.matrix(fixed_terms,nd,contrasts.arg=attr(lme4::getME(model,"X"),"contrasts"))[,names(beta),drop=FALSE]
    }
    hp <- block("HP");lp <- block("LP")
    L <- if(component=="trust_within")hp-lp else (hp+lp)/2
    ci <- bind_rows(lapply(seq_len(nrow(L)),function(i)
      lmerTest::contest1D(model,L[i,],ddf="Satterthwaite",confint=TRUE)))
    mutate(grid,estimate=ci$Estimate,se=ci[["Std. Error"]],df=ci$df,lower=ci$lower,upper=ci$upper,
      block_position_HP=positions$position[positions$pressure=="HP"],
      block_position_LP=positions$position[positions$pressure=="LP"],
      grand_mean_trust=grand_trust,n_participants=nrow(participants))
  }))
}
primary_plot_build <- function(bundle,registry,out,association_models) {
  d <- bundle$data; results <- bundle$results; estimates <- bundle$estimates
  primary <- results %>% filter(analysis=="mixed_model",correction %in% c("none","loglinear"))
  colours <- c("65%"="#C56924","95%"="#1976A3","Manual"="#697681")
  outcome_labels <- c(advice_bias="Advice acceptance (-c)",task_bias="BLACK preference (-c)",
    advice_dprime="Advice selectivity (d-prime)",task_dprime="Task discrimination (d-prime)",
    task_dprime_benchmark="Task d-prime beyond the aid")
  annotations <- list(); plotdata <- list(); rawdata <- list(); figures <- list()
  select_tests <- function(h,o,ids=NULL) {
    t <- primary %>% filter(hypothesis==h,outcome==o)
    if(!is.null(ids))t <- t[match(ids,t$contrast_id),]
    stopifnot(!anyNA(t$contrast_id)); t
  }
  register <- function(stem,h,o,t,raw,means) {
    key <- paste(stem,o,sep="__")
    if("estimate" %in% names(means)) {
      means$display_estimate <- if(o=="correct_rt" && h!="H1")exp(means$estimate) else means$estimate
      means$display_lower <- if(o=="correct_rt" && h!="H1")exp(means$lower) else means$lower
      means$display_upper <- if(o=="correct_rt" && h!="H1")exp(means$upper) else means$upper
    }
    if("value" %in% names(raw))raw$display_value <- if(o=="correct_rt" && h!="H1")exp(raw$value) else raw$value
    if(nrow(t))annotations[[key]] <<- t %>% mutate(filename=stem,annotation_type=case_when(
      contrast_id=="interaction"~"difference_of_differences",grepl("^cell_|^compensation_",contrast_id)~"zero_reference",
      grepl("^trust_",contrast_id)~"adjusted_slope",TRUE~"bracket"))
    rawdata[[key]] <<- as_tibble(raw) %>% mutate(filename=stem,outcome=o)
    plotdata[[key]] <<- as_tibble(means) %>% mutate(filename=stem,outcome=o)
  }
  stem_for <- function(order)registry$filename[match(order,registry$order)]
  add_figure <- function(order,p,caption,subtitle=NULL) {
    row <- registry[match(order,registry$order),]
    title <- paste(strwrap(row$title,width=88),collapse="\n")
    figures[[row$filename]] <<- p + plot_annotation(title=title,subtitle=subtitle,
      caption=paste(strwrap(paste0("N = ",nrow(d$cohort),"; reviewed participant exclusions applied. ",caption),width=153),collapse="\n"),theme=primary_plot_theme()+theme(plot.title=element_text(size=19,face="bold"),plot.subtitle=element_text(size=11)))
  }
  common_caption <- "Faint points: participants; open diamonds: model estimates with pointwise 95% CIs. Holm within hypothesis and outcome: *** p<.001, ** p<.01, * p<.05, ns p>=.05."
  cell_panel <- function(raw,means,t,title,ylab,ratio=FALSE,zero=FALSE,brackets=TRUE) {
    raw <- mutate(raw,x=primary_cell_index(raw)); means <- mutate(means,x=match(contrast_id,names(primary_cells(primary_grid()))),reliability=rep(c("65%","95%"),2)[x])
    if(ratio) {raw$value <- exp(raw$value);for(k in c("estimate","lower","upper"))means[[k]] <- exp(means[[k]])}
    base <- if(ratio)1 else 0
    rg <- range(c(raw$value,means$lower,means$upper,if(zero)base),finite=TRUE);span <- max(diff(rg),.15)
    axis_breaks <- pretty(rg,n=5);axis_breaks <- axis_breaks[axis_breaks>=rg[1] & axis_breaks<=rg[2]]
    top <- rg[2]+.1*span; tick <- .025*span
    # These pairings are design cells, not participant-paired significance tests.
    links <- tibble(contrast_id=c("reliability_HP","reliability_LP","pressure_65","pressure_95","interaction"),
      x1=c(1,3,1,2,1),x2=c(2,4,3,4,4),text=c("HP","LP","65%","95%","Interaction")) %>% inner_join(t,by="contrast_id")
    if(!brackets)links <- links[0,]
    links <- links %>% mutate(y=top+seq_len(n())*.13*span,label=paste0(text,": ",primary_p_label(p_holm)))
    zeros <- t %>% filter(grepl("^cell_",contrast_id)) %>% mutate(x=match(contrast_id,means$contrast_id),
      y=means$upper[x]+.035*span,label=primary_p_label(p_holm))
    stopifnot(setequal(c(links$contrast_id,zeros$contrast_id),t$contrast_id),
      !anyDuplicated(c(links$contrast_id,zeros$contrast_id)))
    p <- ggplot()+geom_hline(yintercept=base,linetype="dotted",colour="#AEB7BE")+
      geom_line(data=raw,aes(x,value,group=participant_id),colour="#75818A",alpha=.10,linewidth=.25)+
      geom_point(data=raw,aes(x,value,colour=reliability),position=position_jitter(width=.065,height=0,seed=26),alpha=.22,size=1.1)+
      geom_errorbar(data=means,aes(x,ymin=lower,ymax=upper,colour=reliability),width=.10,linewidth=.8)+
      geom_point(data=means,aes(x,estimate,colour=reliability),shape=23,fill="white",stroke=1.1,size=3.5)+
      scale_colour_manual(values=colours,guide="none")+scale_x_continuous(breaks=1:4,labels=c("HP\n65%","HP\n95%","LP\n65%","LP\n95%"),limits=c(.65,4.35))+
      scale_y_continuous(breaks=axis_breaks)+
      labs(title=title,x=NULL,y=ylab)+primary_plot_theme()
    if(nrow(zeros))p <- p+geom_text(data=zeros,aes(x,y,label=label),size=3.2,vjust=0)
    if(nrow(links))p <- p+geom_segment(data=links,aes(x=x1,xend=x2,y=y,yend=y),linewidth=.4)+
      geom_segment(data=links,aes(x=x1,xend=x1,y=y,yend=y-tick),linewidth=.4)+
      geom_segment(data=links,aes(x=x2,xend=x2,y=y,yend=y-tick),linewidth=.4)+
      geom_text(data=links,aes(x=(x1+x2)/2,y=y+.018*span,label=label),vjust=0,size=3.1)
    p+coord_cartesian(ylim=c(rg[1]-.05*span,if(nrow(links))max(links$y)+.11*span else top+.09*span),clip="on")
  }
  human <- filter(d$task_scores,actor=="human",correction=="loglinear")
  advice <- filter(d$advice_scores,correction=="loglinear")
  perfraw <- function(o)if(o=="task_dprime")human %>% mutate(value=dprime) else d$performance %>% mutate(value=.data[[o]])
  gainraw <- function(o) {
    x <- perfraw(o)
    inner_join(filter(x,mode=="Automation") %>% select(participant_id,pressure,reliability,pattern,aided=value),
      filter(x,mode=="Manual") %>% select(participant_id,pressure,manual=value),by=c("participant_id","pressure")) %>% mutate(value=aided-manual)
  }
  # H1: manual HP/LP values plus one averaged pressure contrast per outcome.
  panels <- list()
  for(o in c("accuracy","task_dprime","correct_rt")) {
    raw <- perfraw(o) %>% filter(mode=="Manual") %>% mutate(x=ifelse(pressure=="HP",1,2))
    name <- paste(o,if(o=="task_dprime")"loglinear" else "none",sep="_")
    m <- estimates[[paste0(name,"_manual")]] %>% mutate(x=ifelse(contrast_id=="manual_HP",1,2))
    if(o=="accuracy")raw$value <- raw$value*100
    if(o=="correct_rt"){raw$value <- exp(raw$value);for(k in c("estimate","lower","upper"))m[[k]] <- exp(m[[k]])}
    t <- select_tests("H1",o);rg <- range(c(raw$value,m$lower,m$upper));span <- diff(rg); yy <- rg[2]+.1*span
    p <- ggplot(raw,aes(x,value))+geom_line(aes(group=participant_id),alpha=.13,colour="#697681")+
      geom_point(position=position_jitter(width=.05,height=0,seed=26),colour="#697681",alpha=.22,size=1.1)+
      geom_errorbar(data=m,aes(x=x,y=NULL,ymin=lower,ymax=upper),inherit.aes=FALSE,width=.10,linewidth=.8)+
      geom_point(data=m,aes(x,estimate),inherit.aes=FALSE,shape=23,fill="white",size=3.4)+
      annotate("segment",x=1,xend=2,y=yy,yend=yy)+annotate("segment",x=c(1,2),xend=c(1,2),y=yy,yend=yy-.025*span)+
      annotate("text",x=1.5,y=yy+.04*span,label=primary_p_label(t$p_holm),size=4)+
      scale_x_continuous(breaks=1:2,labels=c("HP (1 s)","LP (3 s)"),limits=c(.6,2.4))+
      labs(title=switch(o,accuracy="All-trial accuracy",task_dprime="Task discrimination",correct_rt="Correct-response speed"),x=NULL,
        y=switch(o,accuracy="Accuracy (%)",task_dprime="Task d-prime",correct_rt="Geometric mean RT (s)"))+primary_plot_theme()+
      coord_cartesian(ylim=c(rg[1]-.03*span,yy+.13*span))
    panels[[o]] <- p;register(stem_for(1),"H1",o,t,raw,m)
  }
  add_figure(1,wrap_plots(panels,nrow=1),paste(common_caption,"Manual HP-LP contrasts weight both allocation patterns equally. SDT excludes timeouts."))
  for(order in 2:3) {
    panels <- list()
    for(o in if(order==2)c("accuracy","task_dprime") else "correct_rt") {
      raw <- gainraw(o);if(o=="accuracy")raw$value <- raw$value*100
      t <- select_tests("H2",o,c(names(primary_cells(primary_grid())),names(primary_five(primary_grid()))));m <- filter(t,grepl("^cell_",contrast_id))
      panels[[o]] <- cell_panel(raw,m,t,if(o=="accuracy")"All-trial accuracy gain" else if(o=="task_dprime")"Task discrimination gain" else "Correct-response RT: aided / manual",
        if(o=="accuracy")"Aided minus manual (percentage points)" else if(o=="task_dprime")"Aided minus manual (d-prime)" else "Geometric mean RT ratio",ratio=o=="correct_rt",zero=TRUE)
      register(stem_for(order),"H2",o,t,raw,m)
    }
    add_figure(order,wrap_plots(panels,nrow=1),paste(common_caption,"Cell labels test no aid effect; brackets compare gains. Interaction compares the two reliability effects.",
      if(order==3)"Ratios below 1 indicate faster responses. Tests use log ratios; moderation contrasts are ratios of ratios." else "Accuracy includes timeouts; task d-prime uses answered trials."))
  }
  panels <- list()
  for(o in c("accuracy","task_dprime")) {
    x <- perfraw(o)
    raw <- inner_join(filter(x,mode=="Automation",pressure=="HP") %>% select(participant_id,pattern,aided=value),
      filter(x,mode=="Manual",pressure=="LP") %>% select(participant_id,manual=value),by="participant_id") %>%
      mutate(value=(aided-manual)*if(o=="accuracy")100 else 1,x=match(pattern,patterns))
    t <- select_tests("H2",o,c("compensation_1","compensation_2")) %>% mutate(x=1:2)
    span <- diff(range(c(raw$value,t$lower,t$upper)));top <- max(raw$value,t$upper)+.08*span
    t$annotation_y <- top
    panels[[o]] <- ggplot(raw,aes(x,value))+geom_hline(yintercept=0,linetype="dotted")+
      geom_point(position=position_jitter(width=.08,height=0,seed=26),alpha=.22,size=1.2)+
      geom_errorbar(data=t,aes(x=x,y=NULL,ymin=lower,ymax=upper),inherit.aes=FALSE,width=.12)+
      geom_point(data=t,aes(x,estimate),inherit.aes=FALSE,shape=23,fill="white",size=3.4)+
      geom_text(data=t,aes(x=x,y=annotation_y,label=primary_p_label(p_holm)),inherit.aes=FALSE,size=3.5)+
      scale_x_continuous(breaks=1:2,labels=c("HP95 / LP65\ngroup","HP65 / LP95\ngroup"),limits=c(.6,2.4))+
      labs(title=if(o=="accuracy")"Accuracy\ncompensation" else "Discrimination\ncompensation",x=NULL,
        y=if(o=="accuracy")"Aided HP minus manual LP (pp)" else "Aided HP minus manual LP (d-prime)")+primary_plot_theme()
    register(stem_for(4),"H2",o,t,raw,t)
  }
  components <- d$decomposition %>% select(participant_id,pressure,reliability,completion_component,choice_component,total_gain) %>%
    pivot_longer(c(completion_component,choice_component),names_to="component",values_to="value") %>% mutate(value=100*value,x=primary_cell_index(pick(everything())))
  cm <- components %>% group_by(x,component) %>% summarise(value=mean(value),.groups="drop")
  panels$components <- ggplot(cm,aes(x,value,fill=component))+geom_col(width=.6,position="stack")+
    geom_hline(yintercept=0,colour="#697681")+scale_x_continuous(breaks=1:4,labels=c("HP\n65%","HP\n95%","LP\n65%","LP\n95%"))+
    scale_fill_manual(values=c(choice_component="#6C75A3",completion_component="#A9B5BE"),labels=c("Choice","Completion"),name=NULL)+
    labs(title="Sources of accuracy gains",subtitle="Descriptive; no component tests",x=NULL,y="Contribution to gain (pp)")+primary_plot_theme()
  register(stem_for(4),"H2","decomposition",tibble(),components,cm)
  add_figure(4,wrap_plots(panels,nrow=1),paste(common_caption,"Zero compensation means aided HP equals manual LP; nonsignificance does not establish equivalence. Decomposition is exact accounting, not causal mediation."))
  # H3: accuracy and matched-task benchmarks.
  panels <- list()
  for(o in c("accuracy_benchmark","task_dprime_benchmark")) {
    raw <- if(o=="accuracy_benchmark")mutate(d$benchmark,value=100*net_gain) else
      inner_join(filter(human,mode=="Automation") %>% select(participant_id,pressure,reliability,human=dprime),
        filter(d$task_scores,actor=="aid_matched",correction=="loglinear") %>% select(participant_id,pressure,aid=dprime),by=c("participant_id","pressure")) %>% mutate(value=human-aid)
    t <- select_tests("H3",o); m <- filter(t,grepl("^cell_",contrast_id))
    panels[[o]] <- cell_panel(raw,m,t,if(o=="accuracy_benchmark")"All trials: accuracy beyond aid" else "Human-answered trials: d-prime beyond aid",
      if(o=="accuracy_benchmark")"Human minus aid (percentage points)" else "Human minus matched aid (d-prime)",zero=TRUE)
    register(stem_for(5),"H3",o,t,raw,m)
  }
  add_figure(5,wrap_plots(panels,nrow=1),paste(common_caption,"Accuracy assumes the aid responds on time on every trial. Task SDT uses exactly matched human-answered trials; these benchmarks have different populations."))
  # H4/H5: promoted paired SDT panels; all five comparisons in each panel.
  sdt_panels <- list()
  for(order in 6:7) {
    h <- if(order==6)"H4" else "H5";panels <- list()
    for(domain in c("advice","task")) {
      o <- paste(domain,if(order==6)"bias" else "dprime",sep="_")
      raw <- if(domain=="advice")advice else filter(human,mode=="Automation")
      raw$value <- if(order==7)raw$dprime else if(domain=="advice")raw$automation_bias else raw$black_bias
      m <- estimates[[paste(h,o,"loglinear",sep="_")]];t <- select_tests(h,o)
      label <- switch(o,advice_bias="Advice acceptance (-c)",task_bias="BLACK response preference (-c)",advice_dprime="Advice selectivity (d-prime)",task_dprime="Final stimulus discrimination (d-prime)")
      panels[[o]] <- cell_panel(raw,m,t,label,if(order==6)"Bias (-c)" else "Discrimination (d-prime)")
      sdt_panels[[o]] <- cell_panel(raw,m,t[0,],label,if(order==6)"Bias (-c)" else "Discrimination (d-prime)",brackets=FALSE)
      register(stem_for(order),h,o,t,raw,m)
    }
    add_figure(order,wrap_plots(panels,nrow=1),paste(common_caption,"Answered aided trials; loglinear correction. The two panels summarise the same decisions and measure different constructs. Interaction = (95%-65%) at HP minus LP."))
  }
  panels <- list()
  for(o in c("trust","perceived","signed_error")) {
    raw <- if(o=="trust")d$trust else d$ratings;raw$value <- raw[[o]]
    m <- estimates[[paste("H6",o,sep="_")]];t <- select_tests("H6",o)
    panels[[o]] <- cell_panel(raw,m,t,switch(o,trust="Reported trust",perceived="Perceived reliability",signed_error="Reliability calibration error"),
      switch(o,trust="Six-item mean (1-5)",perceived="Perceived correct advice (%)",signed_error="Perceived minus realised (pp)"),zero=o=="signed_error")
    register(stem_for(8),"H6",o,t,raw,m)
  }
  add_figure(8,wrap_plots(panels,nrow=1),paste(common_caption,"Trust and perceived reliability are separate outcomes. Positive calibration error indicates overestimation. Each outcome has its own Holm family."))
  # H7: raw change/mean scatters with predictions from the same adjusted models.
  panels <- association_predictions <- list()
  for(o in c("advice_bias","advice_dprime"))for(component in c("trust_within","trust_between")) {
    a <- filter(d$associations,correction=="loglinear") %>% mutate(value=if(o=="advice_bias")automation_bias else dprime)
    raw <- if(component=="trust_within")a %>% select(participant_id,pattern,pressure,trust,value) %>%
      pivot_wider(names_from=pressure,values_from=c(trust,value)) %>% mutate(x=trust_HP-trust_LP,y=value_HP-value_LP) else
      a %>% group_by(participant_id,pattern) %>% summarise(x=mean(trust),y=mean(value),.groups="drop")
    t <- select_tests("H7",o,component)
    pred <- primary_trust_predictions(association_models[[t$model]],a,raw,component) %>%
      mutate(outcome=o,component=component,model=t$model,inference_available=t$inference_available)
    if(!t$inference_available)pred$lower <- pred$upper <- NA_real_
    association_predictions[[paste(o,component)]] <- pred
    title <- paste(if(o=="advice_bias")"Acceptance" else "Discrimination",if(component=="trust_within")"within person" else "between people",sep=": ")
    subtitle <- if(t$inference_available)sprintf("Adjusted slope %.3f [%.3f, %.3f]; Holm %s",t$estimate,t$lower,t$upper,primary_p_text(t$p_holm)) else "Adjusted inference unavailable; see model diagnostics"
    panels[[paste(o,component)]] <- ggplot(raw,aes(x,y,colour=pattern))+geom_hline(yintercept=0,linetype="dotted",colour="#BBC3C9")+
      geom_ribbon(data=pred,aes(x=x,ymin=lower,ymax=upper,fill=pattern,group=pattern),inherit.aes=FALSE,alpha=.12,colour=NA,show.legend=FALSE)+
      geom_line(data=pred,aes(x,estimate,group=pattern),linewidth=.8)+
      geom_point(alpha=.55,size=1.8)+scale_colour_manual(values=c("HP95_LP65"="#1976A3","HP65_LP95"="#C56924"),name="Allocation pattern")+
      scale_fill_manual(values=c("HP95_LP65"="#1976A3","HP65_LP95"="#C56924"),guide="none")+
      labs(title=title,subtitle=subtitle,x=if(component=="trust_within")"Trust difference (HP minus LP)" else "Mean trust across aided blocks",
        y=if(component=="trust_within")"SDT difference (HP minus LP)" else "Mean SDT score")+primary_plot_theme()
    register(stem_for(9),"H7",paste(o,component,sep="__"),t,raw,t)
  }
  add_figure(9,wrap_plots(panels,ncol=2,guides="collect"),"Points: participants; lines/bands: adjusted mixed-model fits and pointwise 95% CIs by allocation pattern, over its observed trust range. Left: predicted HP-LP difference; right: mean of HP/LP predictions. Block position is held at its pattern/pressure mean. Holm across within/between slopes per SDT outcome. Post-block associations are not causal effects.")
  write_csv(bind_rows(association_predictions),file.path(out,"data",paste0(stem_for(9),"_fits.csv")))
  # H8: raw history patterns alongside the adjusted, standardised primary contrasts.
  raw <- d$sequence_raw %>% select(participant_id,pressure,reliability,previous_aid_error,agreement) %>%
    pivot_wider(names_from=previous_aid_error,values_from=agreement) %>% mutate(value=100*(Error-Correct))
  t <- select_tests("H8","agreement");m <- filter(t,grepl("^cell_",contrast_id))
  effect <- cell_panel(raw,m,t,"Change after an aid error","After error minus after correct (pp)",zero=TRUE)
  rr <- mutate(d$sequence_raw,value=100*agreement)
  mm <- estimates$H8_history %>% separate(contrast_id,into=c("pressure","reliability","previous_aid_error"),sep="_",remove=FALSE)
  history <- ggplot(rr,aes(previous_aid_error,value))+geom_line(aes(group=participant_id),alpha=.09)+geom_point(alpha=.16,size=.8,
    position=position_jitter(width=.04,height=0,seed=26))+geom_errorbar(data=mm,aes(y=NULL,ymin=lower,ymax=upper),width=.12,linewidth=.8)+
    geom_point(data=mm,aes(y=estimate),shape=23,fill="white",size=3)+facet_grid(pressure~reliability)+
    labs(title="Raw patterns and adjusted predictions",x="Previous advice",y="Current agreement (%)")+primary_plot_theme()
  register(stem_for(10),"H8","agreement",t,raw,m)
  add_figure(10,effect|history,paste(common_caption,"Raw points are unadjusted. Model effects control current advice, previous response error and progress, and weight current advice to assigned reliability. Only consecutive answered trials within a block are eligible."))
  add_figure(11,wrap_plots(sdt_panels[c("advice_bias","task_bias","advice_dprime","task_dprime")],ncol=2),"Descriptive overview: raw participants and model estimates with pointwise 95% CIs. All inferential comparisons are annotated in H4/H5. Different SDT frames are not independent evidence.")
  overview_means <- bind_rows(estimates[c("H4_advice_bias_loglinear","H4_task_bias_loglinear","H5_advice_dprime_loglinear","H5_task_dprime_loglinear")])
  write_csv(overview_means,file.path(out,"data",paste0(stem_for(11),"_data.csv")))
  tt <- filter(primary,hypothesis %in% c("H4","H5")) %>% mutate(label=factor(contrast_id,rev(names(primary_five(primary_grid())))),symbol=primary_p_label(p_holm),
    outcome=factor(outcome,c("advice_bias","task_bias","advice_dprime","task_dprime")))
  pp <- ggplot(tt,aes(estimate,label))+geom_vline(xintercept=0,linetype="dotted")+geom_errorbar(aes(xmin=lower,xmax=upper),orientation="y",width=.15)+
    geom_point(shape=23,fill="white",size=2.7)+geom_text(aes(x=upper,label=symbol),hjust=-.25,size=3)+facet_wrap(~outcome,scales="free_x",ncol=2,labeller=as_labeller(outcome_labels))+
    scale_y_discrete(labels=c(reliability_HP="95%-65% at HP",reliability_LP="95%-65% at LP",pressure_65="HP-LP at 65%",pressure_95="HP-LP at 95%",interaction="Interaction"))+
    scale_x_continuous(expand=expansion(mult=c(.1,.22)))+labs(x="Model contrast (SDT units)",y=NULL)+primary_plot_theme()
  add_figure(12,pp,"Same 20 tests as H4/H5, shown as contrasts with pointwise 95% CIs; no additional hypothesis family. Holm separately within each hypothesis and outcome.")
  write_csv(tt,file.path(out,"data",paste0(stem_for(12),"_data.csv")))
  context <- human %>% select(participant_id,pressure,mode,dprime,black_bias) %>% pivot_longer(c(dprime,black_bias),names_to="measure",values_to="value")
  pp <- ggplot(context,aes(pressure,value,colour=mode))+geom_boxplot(outlier.shape=NA,width=.5)+
    geom_point(position=position_jitterdodge(jitter.width=.1,dodge.width=.5,seed=26),alpha=.22,size=.7)+
    facet_wrap(~measure,scales="free_y",labeller=as_labeller(c(black_bias="BLACK preference (-c)",dprime="Task discrimination (d-prime)")))+
    scale_colour_manual(values=c(Manual="#697681",Automation="#6C75A3"),labels=c(Manual="Manual",Automation="Aided"))+
    labs(x=NULL,y="Task SDT score",colour=NULL)+primary_plot_theme()
  add_figure(13,pp,"Descriptive raw score distributions across manual and aided blocks; no additional tests. Boxes show medians and quartiles; points are participants. Loglinear correction; answered trials only.")
  write_csv(context,file.path(out,"data",paste0(stem_for(13),"_data.csv")))
  sensitivity <- results %>% filter(analysis=="mixed_model",correction %in% c("loglinear","extreme_only")) %>%
    select(hypothesis,outcome,contrast_id,correction,estimate,p_holm) %>% pivot_wider(names_from=correction,values_from=c(estimate,p_holm))
  pp <- ggplot(sensitivity,aes(estimate_loglinear,estimate_extreme_only,colour=hypothesis))+geom_abline(slope=1,intercept=0,colour="#A7B0B8")+
    geom_point(size=1.8,alpha=.7)+facet_wrap(~outcome,scales="free",ncol=3,labeller=as_labeller(outcome_labels))+labs(x="Loglinear model estimate",y="Extreme-only model estimate",colour=NULL)+primary_plot_theme()
  add_figure(14,pp,"Each point is one planned SDT contrast or trust slope. Diagonal = identical estimates. Correction-specific Holm tests and model diagnostics are in the data tables; missing inference is not nonsignificance.")
  write_csv(sensitivity,file.path(out,"data",paste0(stem_for(14),"_data.csv")))
  comparison <- primary %>% select(hypothesis,outcome,contrast_id,estimate,se,p_holm) %>% inner_join(
    results %>% filter(analysis!="mixed_model",correction %in% c("none","loglinear")) %>% select(hypothesis,outcome,contrast_id,sensitivity=estimate,sensitivity_p=p_holm),
    by=c("hypothesis","outcome","contrast_id")) %>% mutate(x=estimate/se,y=sensitivity/se,
      decision=case_when(is.na(p_holm)|is.na(sensitivity_p)~"Unavailable",(p_holm<.05)==(sensitivity_p<.05)~"Same decision",TRUE~"Different decision"))
  pp <- ggplot(comparison,aes(x,y,colour=decision))+geom_abline(slope=1,intercept=0,colour="#A7B0B8")+geom_point(size=1.7)+
    facet_wrap(~hypothesis,scales="free",ncol=4)+scale_colour_manual(values=c("Same decision"="#1976A3","Different decision"="#C56924","Unavailable"="#697681"))+
    labs(x="Model estimate / model SE",y="Participant estimate / model SE",colour=NULL)+primary_plot_theme()
  add_figure(15,pp,"All 97 primary contrasts are compared with participant sensitivities. Both coordinates use the SAME model SE only to make different outcomes readable; these are not participant t statistics. Colours compare separate within-outcome Holm decisions. RT comparisons use log ratios.")
  write_csv(comparison,file.path(out,"data",paste0(stem_for(15),"_data.csv")))
  cc <- d$performance %>% select(participant_id,pressure,mode,completion,answered_accuracy,accuracy) %>% pivot_longer(c(completion,answered_accuracy,accuracy),names_to="measure",values_to="value")
  pp <- ggplot(cc,aes(pressure,100*value,colour=mode))+geom_boxplot(outlier.shape=NA,width=.5)+
    geom_point(position=position_jitterdodge(jitter.width=.1,dodge.width=.5,seed=26),alpha=.2,size=.6)+
    facet_wrap(~measure,nrow=1,labeller=as_labeller(c(accuracy="All-trial accuracy",answered_accuracy="Answered-trial accuracy",completion="Completion")))+
    scale_colour_manual(values=c(Manual="#697681",Automation="#6C75A3"),labels=c(Manual="Manual",Automation="Aided"))+labs(x=NULL,y="Percentage",colour=NULL)+primary_plot_theme()
  add_figure(16,pp,"Descriptive raw participant context. All-trial accuracy equals completion times answered accuracy; SDT describes answered trials. Boxes show medians and quartiles. No additional inferential tests.")
  write_csv(cc,file.path(out,"data",paste0(stem_for(16),"_data.csv")))
  # Superseded probability-based measures, refitted on the included cohort.
  hist <- bundle$historical;panels <- list()
  for(a in c("Correct","Incorrect")) {
    raw <- hist$participants %>% filter(advice==a) %>% mutate(value=100*agreement)
    t <- hist$agreement_tests %>% filter(startsWith(contrast_id,paste0(a,"__"))) %>% mutate(contrast_id=sub("^[^_]+__","",contrast_id))
    m <- hist$agreement_means %>% filter(startsWith(contrast_id,paste0(a,"__"))) %>% mutate(contrast_id=sub("^[^_]+__","",contrast_id))
    panels[[a]] <- cell_panel(raw,m,t,paste(a,"advice"),"Agreement (%)")
  }
  add_figure(17,wrap_plots(panels,nrow=1),"Historical probability measure, refreshed on the current cohort. Raw participant points; model means and pointwise 95% CIs. Holm across the ten conditional-agreement comparisons. These tests are separate from the 97 primary comparisons; primary SDT bias and discrimination are H4/H5.")
  write_csv(hist$agreement_means,file.path(out,"data",paste0(stem_for(17),"_data.csv")))
  write_csv(hist$participants,file.path(out,"data",paste0(stem_for(17),"_participants.csv")))
  write_csv(hist$agreement_tests,file.path(out,"data",paste0(stem_for(17),"_annotations.csv")))
  pp <- cell_panel(mutate(hist$selectivity,value=100*selectivity),hist$selectivity_means,hist$selectivity_tests,
    "Agreement on correct minus incorrect advice","Selectivity (percentage points)")
  add_figure(18,pp,"Historical probability measure, refreshed on the current cohort. Raw participant points; model estimates and pointwise 95% CIs. Holm across five selectivity comparisons. These tests are separate from the 97 primary comparisons; the primary discrimination measures are in H5.")
  write_csv(hist$selectivity_means,file.path(out,"data",paste0(stem_for(18),"_data.csv")))
  write_csv(hist$selectivity,file.path(out,"data",paste0(stem_for(18),"_participants.csv")))
  write_csv(hist$selectivity_tests,file.path(out,"data",paste0(stem_for(18),"_annotations.csv")))
  # Preserve every table used by the figures, including explicit adjusted history predictions.
  plotdata$H8_history_predictions <- mm %>% mutate(filename=stem_for(10),outcome="agreement")
  rawdata$H8_history_participants <- rr %>% mutate(filename=stem_for(10),outcome="agreement")
  ann <- bind_rows(annotations)
  ann$outcome <- sub("__trust_(within|between)$","",ann$outcome)
  for(stem in unique(registry$filename[registry$role=="Core"])) {
    write_csv(filter(ann,filename==stem),file.path(out,"data",paste0(stem,"_annotations.csv")))
    write_csv(bind_rows(plotdata) %>% filter(filename==stem),file.path(out,"data",paste0(stem,"_data.csv")))
    write_csv(bind_rows(rawdata) %>% filter(filename==stem),file.path(out,"data",paste0(stem,"_participants.csv")))
  }
  write_csv(ann,file.path(out,"data/annotation_coverage.csv"))
  write_csv(sensitivity,file.path(out,"data/correction_sensitivity.csv"))
  write_csv(comparison,file.path(out,"data/model_participant_comparison.csv"))
  list(figures=figures,annotations=ann)
}
