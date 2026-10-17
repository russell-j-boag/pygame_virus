# Retrospective hypothesis framework; full canonical cohort, no QC filtering.
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lme4)
  library(lmerTest); library(emmeans); library(ggplot2)
})
ao_check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
ao_levels <- c("Manual", "Aid-first", "Stimulus-first")
ao_read <- function(path) read_csv(path, show_col_types = FALSE, progress = FALSE,
  col_types = cols(run_timestamp = col_character()))
ao_hashes <- function(paths) tibble(path = normalizePath(paths, mustWork = TRUE),
  md5 = unname(tools::md5sum(paths)))
ao_verify <- function(x) ao_check(all(file.exists(x$path)) &&
  identical(unname(tools::md5sum(x$path)), x$md5), "Changed or missing inputs/artifacts")
ao_condition <- function(x) unname(c(MANUAL="Manual", AIDFIRST="Aid-first", STIMFIRST="Stimulus-first")[x])
ao_bool <- function(x) tolower(as.character(x)) %in% c("true", "1")
ao_score <- function(h, nh, f, nf, correction="loglinear") {
  ao_check(all(is.finite(c(h,nh,f,nf))) && all(nh>0 & nf>0) &&
    all(h>=0 & h<=nh & f>=0 & f<=nf), "Invalid or empty SDT class")
  rate <- function(k,n) if(correction=="loglinear") (k+.5)/(n+1) else {
    ao_check(correction=="extreme_only", "Unknown SDT correction")
    ifelse(k==0,.5/n,ifelse(k==n,1-.5/n,k/n))
  }
  H <- rate(h,nh); F <- rate(f,nf)
  tibble(hit_rate=H,false_alarm_rate=F,dprime=qnorm(H)-qnorm(F),
    bias=(qnorm(H)+qnorm(F))/2,criterion_c=-(qnorm(H)+qnorm(F))/2)
}
ao_counts <- function(d, domain, decision, answer, signal, positive) {
  d$answer <- answer; d$signal <- signal; d$positive <- positive
  d %>% filter(answer) %>% group_by(participant_id,condition) %>%
    summarise(h=sum(positive & signal),nh=sum(signal),f=sum(positive & !signal),nf=sum(!signal),
      .groups="drop") %>% mutate(domain=domain,decision=decision)
}
ao_prepare <- function(root) {
  manifest_path <- file.path(root,"data/collation_manifest.csv")
  manifest <- ao_read(manifest_path)
  ao_check(nrow(manifest)==180 && !anyDuplicated(manifest[c("participant_id","export_type")]),
    "Expected one canonical run per participant and export family")
  ao_verify(transmute(manifest,path=source_file,md5=source_md5))
  files <- c(trials="data_virus_all.csv",questionnaire="data_virus_postblock_all.csv",sliders="data_virus_sliders_all.csv")
  data <- lapply(names(files),function(kind) {
    canonical <- ao_read(file.path(root,"data",files[[kind]]))
    sources <- filter(manifest,export_type==kind)
    original <- bind_rows(lapply(sources$source_file,ao_read))
    keys <- intersect(c("participant_id","run_timestamp","block_idx","trial","question_idx"),names(canonical))
    normalise <- function(x) {
      x <- x %>% select(all_of(names(canonical))) %>% arrange(across(all_of(keys)))
      for(n in names(x)) if(is.character(x[[n]])) {
        b <- tolower(x[[n]]); if(all(is.na(b) | b %in% c("true","false"))) x[[n]] <- b=="true"
      }
      as.data.frame(x)
    }
    ao_check(isTRUE(all.equal(normalise(canonical),normalise(original),check.attributes=FALSE,tolerance=1e-12)),
      paste("Canonical data differ from exact source exports:",kind))
    runs <- distinct(canonical,participant_id,run_timestamp)
    ao_check(nrow(runs)==60 && setequal(runs$participant_id,1:60) &&
      nrow(anti_join(runs,sources,by=c("participant_id","run_timestamp")))==0,"Cohort/run mismatch")
    canonical
  }); names(data) <- names(files)
  d <- data$trials %>% mutate(condition=ao_condition(condition_code),subject=factor(participant_id))
  ao_check(nrow(d)==46800 && all(count(d,participant_id,condition)$n==260) && !anyNA(d$condition),"Incomplete trial cells")
  ao_check(identical(unique(d$run_timestamp[d$participant_id==59]),"20260924_110831"),"Wrong p59 run")
  ao_check(!anyDuplicated(d[c("participant_id","global_trial")]),"Duplicate trials")
  ao_check(all(d$stimulus %in% c("BLACK","WHITE")),"Invalid stimulus")
  for(j in 1:2) {
    r <- d[[paste0("decision",j,"_response")]]
    ao_check(all(is.na(r) | r %in% c("BLACK","WHITE","TIMEOUT")),"Invalid response label")
    d[[paste0("answered",j)]] <- r %in% c("BLACK","WHITE")
    d[[paste0("correct",j)]] <- !is.na(r) & r==d$stimulus
    ao_check(all(d[[paste0("correct",j)]]==ao_bool(d[[paste0("decision",j,"_correct")]])),"Correctness flag mismatch")
  }
  d$paired <- d$answered1 & d$answered2
  d$aided <- d$condition!="Manual"
  ao_check(all(d$aid_label[d$aided] %in% c("BLACK","WHITE")),"Invalid advice label")
  d$advice_correct <- !is.na(d$aid_label) & d$aid_label==d$stimulus
  ao_check(all(d$advice_correct[d$aided]==ao_bool(d$aid_correct[d$aided])),"Advice correctness mismatch")
  d$agree1 <- !is.na(d$decision1_response) & d$decision1_response==d$aid_label
  d$agree2 <- !is.na(d$decision2_response) & d$decision2_response==d$aid_label
  d$switched <- d$decision1_response!=d$decision2_response
  d$beneficial <- !d$correct1 & d$correct2
  d$harmful <- d$correct1 & !d$correct2
  counts <- bind_rows(lapply(1:2,function(j) ao_counts(d,"task",paste0("D",j),d[[paste0("answered",j)]],
    d$stimulus=="BLACK",d[[paste0("decision",j,"_response")]]=="BLACK")),
    ao_counts(filter(d,aided),"advice","D2",d$answered2[d$aided],d$advice_correct[d$aided],d$agree2[d$aided]),
    ao_counts(filter(d,aided),"aid_task","D2",d$answered2[d$aided],d$stimulus[d$aided]=="BLACK",d$aid_label[d$aided]=="BLACK"))
  ao_check(nrow(counts)==600,"Missing SDT cells")
  scores <- bind_rows(lapply(c("loglinear","extreme_only"),function(cor) bind_cols(counts,
    ao_score(counts$h,counts$nh,counts$f,counts$nf,cor)) %>% mutate(correction=cor)))
  matched_counts <- bind_rows(lapply(1:2,function(j) ao_counts(d,"task",paste0("D",j),d$paired,
    d$stimulus=="BLACK",d[[paste0("decision",j,"_response")]]=="BLACK")))
  matched_scores <- bind_cols(matched_counts,ao_score(matched_counts$h,matched_counts$nh,matched_counts$f,matched_counts$nf))
  denoms <- d %>% group_by(participant_id,run_timestamp,condition) %>% summarise(n_trials=n(),
    n_answered1=sum(answered1),n_answered2=sum(answered2),n_paired=sum(paired),
    n_d1_timeout=sum(!answered1),n_d2_timeout=sum(!answered2),.groups="drop")
  trust <- data$questionnaire %>% mutate(condition=ao_condition(condition_code))
  ao_check(all(trust$response>=1 & trust$response<=5) && all(trust$scale_min==1 & trust$scale_max==5) &&
    all(count(trust,participant_id,condition)$n==6) &&
    !anyDuplicated(trust[c("participant_id","condition","question_idx")]) && setequal(trust$question_idx,1:6),"Invalid six-item trust cells")
  slider <- data$sliders %>% mutate(condition=ao_condition(condition_code))
  ao_check(nrow(slider)==180 && all(slider$response_percent>=0 & slider$response_percent<=100) &&
    all(slider$question_key==ifelse(slider$condition=="Manual","perc_self_correct","perc_auto_correct")),"Invalid rating cells")
  list(trials=d,counts=counts,scores=scores,matched_scores=matched_scores,denominators=denoms,
    trust=trust %>% group_by(participant_id,condition) %>% summarise(response=mean(response),.groups="drop"),
    perceived=transmute(slider,participant_id,condition,response=response_percent),
    selection=distinct(d,participant_id,run_timestamp) %>% arrange(participant_id) %>%
      mutate(included=TRUE,exclusions_applied=FALSE,reason="Full canonical cohort; participant QC not applied"),
    manifest=manifest,inputs=c(manifest_path,file.path(root,"data",files),manifest$source_file))
}
ao_specs <- function(p, correction="loglinear") {
  d <- p$trials; s <- filter(p$scores,correction==!!correction)
  score <- function(domain,variable) s %>% filter(.data$domain==!!domain,decision=="D2") %>%
    transmute(participant_id,condition,response=.data[[variable]])
  binary <- function(x,variable) transmute(x,participant_id,condition,response=as.numeric(.data[[variable]]))
  task <- score("task","dprime"); aid <- score("aid_task","dprime")
  advantage <- inner_join(task,aid,by=c("participant_id","condition"),suffix=c("_human","_aid")) %>%
    transmute(participant_id,condition,response=response_human-response_aid)
  acc_adv <- d %>% filter(aided) %>% group_by(participant_id,condition) %>%
    summarise(response=100*(mean(correct2)-mean(advice_correct)),.groups="drop")
  uptake <- d %>% filter(condition=="Stimulus-first",paired,!agree1) %>%
    mutate(condition=ifelse(advice_correct,"Correct advice","Incorrect advice"))
  specs <- list()
  add <- function(h,o,data,binary=FALSE,lev=ao_levels,methods="three",primary=TRUE) {
    data <- mutate(data,subject=factor(participant_id),condition=factor(condition,levels=lev))
    ao_check(!anyNA(data) && all(count(data,subject)$n>0) && n_distinct(data$subject)==60,"Incomplete model data")
    ao_check(nrow(distinct(data,subject,condition))==60*length(lev),"Missing participant-by-condition model cell")
    specs[[o]] <<- list(hypothesis=h,outcome=o,data=data,binary=binary,levels=lev,methods=methods,primary=primary)
  }
  add("H1","accuracy",binary(d,"correct2"),TRUE)
  add("H1","task_dprime",task)
  add("H2","accuracy_advantage",acc_adv,lev=ao_levels[-1],methods="advantage")
  add("H2","task_dprime_advantage",advantage,lev=ao_levels[-1],methods="advantage")
  add("H3","advice_bias",score("advice","bias"),lev=ao_levels[-1],methods="timing")
  add("H3","task_bias",score("task","bias"),methods="timing")
  add("H4","advice_dprime",score("advice","dprime"),lev=ao_levels[-1],methods="timing")
  add("H5","switching",binary(filter(d,paired),"switched"),TRUE)
  add("H5","beneficial",binary(filter(d,paired),"beneficial"),TRUE)
  add("H5","harmful",binary(filter(d,paired),"harmful"),TRUE)
  add("H6","selective_uptake",binary(uptake,"agree2"),TRUE,c("Incorrect advice","Correct advice"),"uptake")
  add("H7","perceived",p$perceived,methods="ratings")
  add("H7","trust",p$trust,lev=ao_levels[-1],methods="timing")
  add("S3","correct_advice_accuracy",binary(filter(d,!aided | advice_correct),"correct2"),TRUE,primary=FALSE)
  add("S3","incorrect_advice_accuracy",binary(filter(d,!aided | !advice_correct),"correct2"),TRUE,primary=FALSE)
  if(correction!="loglinear") specs <- specs[c("task_dprime","task_dprime_advantage","advice_bias","task_bias","advice_dprime")]
  specs
}
ao_methods <- function(spec, levels=spec$levels) {
  v <- function(name) as.numeric(levels==name)
  timing <- list(SF_minus_AF=v("Stimulus-first")-v("Aid-first"))
  switch(spec$methods,three=c(list(AF_minus_Manual=v("Aid-first")-v("Manual"),
    SF_minus_Manual=v("Stimulus-first")-v("Manual")),timing),timing=timing,
    advantage=c(list(human_minus_aid_AF=v("Aid-first"),human_minus_aid_SF=v("Stimulus-first")),timing),
    uptake=list(correct_minus_incorrect=v("Correct advice")-v("Incorrect advice")),
    ratings=c(list(mean_aid_minus_self=(v("Aid-first")+v("Stimulus-first"))/2-v("Manual")),timing))
}
ao_control <- function(binary) {
  constructor <- if(binary) glmerControl else lmerControl
  settings <- if(binary) list(optimizer="bobyqa",calc.derivs=TRUE,optCtrl=list(maxfun=300000,rhoend=1e-9)) else
    list(optimizer="nloptwrap",calc.derivs=TRUE,optCtrl=list(maxeval=300000,xtol_abs=1e-8,ftol_abs=1e-8))
  for(key in c("check.conv.nobsmax","check.conv.nparmax")) if(key %in% names(formals(constructor))) settings[[key]] <- Inf
  do.call(constructor,settings)
}
ao_fit <- function(spec,correction,out) {
  name <- paste(spec$outcome,correction,sep="__"); d <- spec$data
  message("Fitting ",name," (",nrow(d)," observations)")
  form <- response ~ condition + (1|subject); environment(form) <- asNamespace("stats")
  warnings <- character(); error <- ""
  m <- tryCatch(withCallingHandlers(if(spec$binary) glmer(form,data=d,family=binomial(),control=ao_control(TRUE)) else
    lmerTest::lmer(form,data=d,control=ao_control(FALSE)), warning=function(w) {
      warnings <<- c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e){error <<- conditionMessage(e);NULL})
  cv <- if(!is.null(m)) m@optinfo$conv else NULL
  singular <- if(!is.null(m)) isSingular(m,tol=1e-4) else NA
  rank <- if(!is.null(m)) !is.null(attr(getME(m,"X"),"col.dropped")) else NA
  gradient <- tryCatch(max(abs(solve(chol(m@optinfo$derivs$Hessian),m@optinfo$derivs$gradient))),error=function(e) NA_real_)
  converged <- !is.null(m) && is.null(cv$lme4$messages) && all(cv$opt==0) && all(is.finite(fixef(m))) && is.finite(gradient)
  usable <- converged && !singular && !rank
  diag <- tibble(model=name,outcome=spec$outcome,correction=correction,n_observations=nrow(d),n_participants=n_distinct(d$subject),
    converged=converged,singular=singular,rank_deficient=rank,inference_available=usable,max_scaled_gradient=gradient,
    convergence_message=paste(cv$lme4$messages,collapse=" | "),error=error,warnings=paste(unique(warnings),collapse=" | "))
  saveRDS(m,file.path(out,"models",paste0(name,".rds")))
  write_csv(d,file.path(out,"model_data",paste0(name,".csv")))
  raw <- d %>% group_by(participant_id,condition) %>% summarise(value=mean(response)*ifelse(spec$binary,100,1),n=n(),.groups="drop") %>%
    mutate(outcome=spec$outcome,correction=correction,model=name)
  methods <- ao_methods(spec); scale <- if(spec$binary) 100 else 1
  weights <- bind_rows(lapply(names(methods),function(k) tibble(comparison=k,condition=spec$levels,weight=methods[[k]]))) %>%
    mutate(model=name,outcome=spec$outcome,correction=correction)
  means <- tibble(condition=spec$levels,estimate=NA_real_,se=NA_real_,df=NA_real_,lower=NA_real_,upper=NA_real_)
  tests <- tibble(comparison=names(methods),estimate=NA_real_,se=NA_real_,df=NA_real_,lower=NA_real_,upper=NA_real_,p_raw=NA_real_)
  if(!is.null(m)) {
    writeLines(capture.output(summary(m)),file.path(out,"models",paste0(name,"_summary.txt")))
    if(spec$binary) {
      checks <- d %>% mutate(predicted=fitted(m)) %>% group_by(subject,condition) %>% summarise(n=n(),
        observed=sum(response),expected=sum(predicted),variance=sum(predicted*(1-predicted)),.groups="drop")
      diag$conditional_cell_dispersion <- mean((checks$observed-checks$expected)^2/pmax(checks$variance,1e-10))
      write_csv(checks,file.path(out,"diagnostics",paste0(name,"_cells.csv")))
    } else {
      write_csv(tibble(fitted=fitted(m),residual=residuals(m)),file.path(out,"diagnostics",paste0(name,"_residuals.csv")))
      q <- ggplot(tibble(residual=residuals(m)),aes(sample=residual))+stat_qq()+stat_qq_line()+theme_minimal()+labs(title=name)
      ggsave(file.path(out,"diagnostics",paste0(name,"_qq.png")),q,width=7,height=5,dpi=110)
    }
    em <- emmeans(m,~condition,lmer.df="satterthwaite",lmerTest.limit=Inf)
    if(spec$binary) em <- regrid(em,transform="response")
    parse <- function(x,est) {
      lo <- intersect(c("lower.CL","asymp.LCL"),names(x)); hi <- intersect(c("upper.CL","asymp.UCL"),names(x))
      tibble(estimate=x[[est]]*scale,se=x$SE*scale,df=x$df,lower=x[[lo]]*scale,upper=x[[hi]]*scale)
    }
    a <- as.data.frame(summary(em,infer=c(TRUE,FALSE)))
    means <- bind_cols(tibble(condition=as.character(a$condition)),parse(a,if(spec$binary) "prob" else "emmean"))
    b <- as.data.frame(summary(contrast(em,methods),infer=c(TRUE,TRUE),adjust="none"))
    tests <- bind_cols(tibble(comparison=b$contrast),parse(b,"estimate"),tibble(p_raw=if(usable)b$p.value else NA_real_))
    if(!usable) { means$lower <- means$upper <- NA_real_; tests$lower <- tests$upper <- NA_real_ }
  }
  tests <- tests %>% mutate(hypothesis=spec$hypothesis,outcome=spec$outcome,model=name,correction=correction,
    contrast_id=paste(spec$hypothesis,spec$outcome,comparison,sep="__"),family=paste(spec$hypothesis,spec$outcome,sep="__"),
    primary=spec$primary,inference_available=usable,analysis="mixed_model")
  paired <- bind_rows(lapply(names(methods),function(k) {
    a <- raw %>% mutate(weight=methods[[k]][match(condition,spec$levels)]) %>% group_by(participant_id) %>%
      summarise(value=sum(value*weight),.groups="drop")
    n <- nrow(a); se <- sd(a$value)/sqrt(n); estimate <- mean(a$value); df <- n-1
    tibble(comparison=k,estimate=estimate,se=se,df=df,lower=estimate-qt(.975,df)*se,upper=estimate+qt(.975,df)*se,
      p_raw=if(se>0)2*pt(-abs(estimate/se),df) else if(estimate==0)1 else 0)
  })) %>% mutate(hypothesis=spec$hypothesis,outcome=spec$outcome,model=name,correction=correction,
    contrast_id=paste(spec$hypothesis,spec$outcome,comparison,sep="__"),family=paste(spec$hypothesis,spec$outcome,sep="__"),
    primary=spec$primary,inference_available=TRUE,analysis="participant_paired")
  list(tests=tests,paired=paired,means=mutate(means,model=name,outcome=spec$outcome,correction=correction,inference_available=usable),
    raw=raw,diagnostics=diag,weights=weights)
}
ao_holm <- function(x) x %>% group_by(analysis,correction,family) %>% mutate(p_holm=p.adjust(p_raw,"holm"),
  family_size=n(),significant=!is.na(p_holm) & p_holm<.05) %>% ungroup()
ao_install <- function(stage,out) {
  dir.create(dirname(out),recursive=TRUE,showWarnings=FALSE)
  old <- NULL
  if(dir.exists(out)) {
    archive <- file.path(dirname(out),"archive");dir.create(archive,showWarnings=FALSE)
    old <- tempfile(paste0(basename(out),"_",format(Sys.time(),"%Y%m%d_%H%M%S"),"_"),tmpdir=archive)
    ao_check(file.rename(out,old),"Could not archive previous output")
  }
  if(!file.rename(stage,out)) {
    if(!is.null(old)) file.rename(old,out)
    stop("Publication failed; previous report restored")
  }
  invisible(old)
}
ao_artifacts <- function(out) {
  paths <- list.files(out,recursive=TRUE,full.names=TRUE)
  paths <- paths[!basename(paths) %in% c("artifact_manifest.csv","VALIDATED.txt")]
  tibble(path=substring(paths,nchar(out)+2),md5=unname(tools::md5sum(paths)))
}
