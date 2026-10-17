# Revised primary framework, agreed 2026-10-17 after inspection of earlier results.
# Rscript analyse_time_pressure_primary.R [analysis_dir] [combined_csv]
.primary_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
.primary_file <- if (length(.primary_files)) tail(.primary_files, 1)[[1]] else
  sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1])
PRIMARY_ROOT <- dirname(normalizePath(.primary_file))
source(file.path(PRIMARY_ROOT, "analyse_time_pressure_task_sdt.R"), local=TRUE)
source(file.path(PRIMARY_ROOT, "analyse_time_pressure_sdt.R"), local=TRUE)
source(file.path(PRIMARY_ROOT, "analyse_time_pressure_exploratory.R"), local=TRUE)
suppressPackageStartupMessages(library(sandwich))
rm(.primary_files, .primary_file)

primary_grid <- function() expand_grid(pressure=c("HP","LP"), reliability=c("65%","95%"))
primary_five <- function(g) {
  cell <- function(p,r) as.numeric(g$pressure==p & g$reliability==r)
  list(reliability_HP=cell("HP","95%")-cell("HP","65%"),
       reliability_LP=cell("LP","95%")-cell("LP","65%"),
       pressure_65=cell("HP","65%")-cell("LP","65%"),
       pressure_95=cell("HP","95%")-cell("LP","95%"),
       interaction=cell("HP","95%")-cell("HP","65%")-cell("LP","95%")+cell("LP","65%"))
}
primary_cells <- function(g) setNames(lapply(seq_len(4), function(i)
  as.numeric(g$pressure==primary_grid()$pressure[i] & g$reliability==primary_grid()$reliability[i])),
  c("cell_HP65","cell_HP95","cell_LP65","cell_LP95"))
primary_performance <- function(g, compensation=TRUE) {
  cell <- function(m,p,z) as.numeric(g$mode==m & g$pressure==p & g$pattern==z)
  gain <- function(p,r) {
    z <- if ((p=="HP" && r=="95%") || (p=="LP" && r=="65%")) patterns[1] else patterns[2]
    cell("Automation",p,z)-cell("Manual",p,z)
  }
  cg <- primary_grid(); a <- lapply(seq_len(4),function(i)gain(cg$pressure[i],cg$reliability[i]))
  names(a) <- names(primary_cells(cg))
  five <- lapply(primary_five(cg),function(w)Reduce(`+`,Map(`*`,a,w)))
  h2 <- c(a,five)
  if(compensation) for(i in 1:2) h2[[paste0("compensation_",i)]] <- cell("Automation","HP",patterns[i])-cell("Manual","LP",patterns[i])
  list(H1=list(manual_pressure=.5*(cell("Manual","HP",patterns[1])-cell("Manual","LP",patterns[1])+
    cell("Manual","HP",patterns[2])-cell("Manual","LP",patterns[2]))), H2=h2,
    manual=list(manual_HP=.5*(cell("Manual","HP",patterns[1])+cell("Manual","HP",patterns[2])),
                manual_LP=.5*(cell("Manual","LP",patterns[1])+cell("Manual","LP",patterns[2]))))
}
primary_source_paths <- function(root) file.path(root,c("analyse_time_pressure_primary.R","analyse_time_pressure.R",
  "analyse_time_pressure_task_sdt.R","analyse_time_pressure_sdt.R","analyse_time_pressure_exploratory.R",
  "R/participant_identity.R","config/participant_identity_corrections.csv","config/participant_exclusions.csv","config/hypothesis_figures.csv"))

primary_fit <- function(name, formula, d, binary_outcome, out) {
  message("Primary fit: ",name," (",nrow(d)," rows)")
  # Formulas need only data columns and base/statistics functions. Do not serialize
  # the enclosing analysis workspace into every fitted model.
  environment(formula) <- asNamespace("stats")
  warnings <- character()
  m <- withCallingHandlers(if(binary_outcome) glmer(formula,data=d,family=binomial(),control=strict_model_control(TRUE)) else
    lmerTest::lmer(formula,data=d,control=strict_model_control(FALSE)),warning=function(w){warnings <<- c(warnings,conditionMessage(w));invokeRestart("muffleWarning")})
  cv <- m@optinfo$conv; singular <- isSingular(m,tol=1e-4)
  rank <- !is.null(attr(getME(m,"X"),"col.dropped"))
  gradient <- tryCatch(max(abs(solve(chol(m@optinfo$derivs$Hessian),m@optinfo$derivs$gradient))),error=function(e)NA_real_)
  converged <- is.null(cv$lme4$messages) && all(cv$opt==0) && all(is.finite(fixef(m)))
  diag <- tibble(model=name,n_observations=nrow(d),n_participants=n_distinct(d$subject),converged=converged,
    singular=singular,rank_deficient=rank,inference_available=converged && !singular && !rank,
    max_scaled_gradient=gradient,convergence_message=paste(cv$lme4$messages,collapse=" | "),warnings=paste(unique(warnings),collapse=" | "))
  if(binary_outcome) {
    cells <- d %>% mutate(.p=fitted(m),.y=model.response(model.frame(m))) %>%
      group_by(across(all_of(intersect(c("subject","mode","pressure","pattern","reliability","advice","previous_aid_error"),all.vars(formula))))) %>%
      summarise(n=n(),observed=sum(.y),expected=sum(.p),variance=sum(.p*(1-.p)),.groups="drop")
    diag$conditional_cell_dispersion <- mean((cells$observed-cells$expected)^2/pmax(cells$variance,1e-10))
    write_csv(cells,file.path(out,paste0(name,"_cell_checks.csv")))
  } else {
    qq <- ggplot(tibble(residual=residuals(m)),aes(sample=residual))+stat_qq()+stat_qq_line()+theme_minimal()+labs(title=paste(name,"residuals"))
    ggsave(file.path(out,paste0(name,"_residual_qq.png")),qq,width=7,height=5,dpi=110)
  }
  saveRDS(m,file.path(out,"models",paste0(name,".rds")))
  writeLines(capture.output(summary(m)),file.path(out,paste0(name,"_summary.txt")))
  list(model=m,diagnostics=diag)
}

primary_table <- function(em,methods,hypothesis,outcome,model,correction="none",scale=1,usable=TRUE,analysis="mixed_model") {
  x <- as.data.frame(summary(contrast(em,methods),infer=c(TRUE,TRUE),adjust="none"))
  lo <- intersect(c("lower.CL","asymp.LCL"),names(x)); hi <- intersect(c("upper.CL","asymp.UCL"),names(x))
  tibble(hypothesis=hypothesis,outcome=outcome,model=model,correction=correction,analysis=analysis,
    contrast_id=x$contrast,estimate=x$estimate*scale,lower=x[[lo]]*scale,upper=x[[hi]]*scale,se=x$SE*scale,df=x$df,
    p_raw=if(usable)x$p.value else NA_real_,inference_available=usable)
}

run_time_pressure_primary <- function(base,input,root=PRIMARY_ROOT) {
  base <- normalizePath(base); input <- normalizePath(input)
  stage <- tempfile(".primary_build_",tmpdir=base);dir.create(file.path(stage,"models"),recursive=TRUE)
  on.exit(if(dir.exists(stage))unlink(stage,recursive=TRUE),add=TRUE)
  manifest <- read_csv(file.path(base,"input_manifest.csv"),show_col_types=FALSE)
  check(identical(unname(tools::md5sum(manifest$path)),manifest$md5),"Historical source files have changed")
  raw <- read_identity_csv(input); dat <- prepare_trials(raw)
  check(nrow(dat)==1600*n_distinct(dat$participant_id),"Incomplete experimental cohort")
  original <- bind_rows(lapply(manifest$path[manifest$kind=="trials"],read_identity_csv))
  keys <- c("participant_id","run_timestamp","block_idx","trial")
  cols <- intersect(names(raw),names(original))
  x <- raw %>% select(all_of(cols)) %>% arrange(across(all_of(keys)))
  y <- original %>% select(all_of(cols)) %>% arrange(across(all_of(keys)))
  check(isTRUE(all.equal(as.data.frame(x),as.data.frame(y),check.attributes=FALSE)),"Combined CSV differs from original runs")
  # Apply reviewed participant/run decisions after identity correction, before any
  # trial summary, SDT score, rating, model, or participant sensitivity is formed.
  policy <- read_csv(file.path(root,"config/participant_exclusions.csv"),show_col_types=FALSE,
    col_types=cols(run_timestamp=col_character()))
  identity_keys <- c("participant_id","source_participant_id","run_timestamp")
  check(!anyDuplicated(policy$participant_id) && all(policy$decision=="exclude") &&
    all(!is.na(policy$reason) & nzchar(policy$reason)),"Invalid participant exclusion decisions")
  input_cohort <- dat %>% distinct(participant_id,source_participant_id,run_timestamp,pattern) %>% mutate(run_timestamp=as.character(run_timestamp))
  check(nrow(anti_join(policy,input_cohort,by=identity_keys))==0,"Exclusion decision does not match a source run")
  selection <- input_cohort %>% left_join(policy,by=identity_keys) %>% mutate(included=is.na(decision),
    decision=coalesce(decision,"include"),reason=coalesce(reason,"Retained"))
  exclusions <- filter(selection,!included)
  dat <- droplevels(filter(dat,participant_id %in% selection$participant_id[selection$included]))
  check(nrow(dat)==1600*sum(selection$included),"Excluded cohort trial count mismatch")
  cohort_note <- sprintf("%d source participants; excluded %s under reviewed QC decisions; %d included in all current analyses and figures.",
    nrow(selection),paste(sprintf("%03d",exclusions$participant_id),collapse=", "),n_distinct(dat$participant_id))
  perf <- dat %>% group_by(participant_id,subject,pattern,pressure,mode,reliability) %>% summarise(
    answered_accuracy=mean(accuracy[!timeout]),accuracy=mean(accuracy),
    correct_rt=mean(log(rt_s[valid_correct_rt])),completion=mean(!timeout),.groups="drop")
  counts <- task_sdt_counts(dat)
  scores <- bind_rows(lapply(c("loglinear","extreme_only"),function(co)task_sdt_scores(counts,co))) %>% mutate(subject=factor(participant_id))
  acount <- dat %>% filter(mode=="Automation",!timeout) %>% group_by(participant_id,pattern,pressure,reliability,advice) %>%
    summarise(n_answered=n(),n_agree=sum(agreement),.groups="drop")
  advice <- bind_rows(lapply(c("loglinear","extreme_only"),function(co)sdt_scores(acount,co))) %>% mutate(subject=factor(participant_id))
  meta <- dat %>% filter(mode=="Automation") %>% distinct(participant_id,subject,run_timestamp,block_idx,pressure,reliability,pattern)
  q <- bind_rows(lapply(manifest$path[manifest$kind=="trust"],read_identity_csv)) %>% filter(participant_id %in% dat$participant_id)
  check(!anyDuplicated(q[c("participant_id","run_timestamp","block_idx","question_idx")]) && all(q$response %in% 1:5),"Invalid trust items")
  coverage <- q %>% group_by(participant_id,run_timestamp,block_idx) %>% summarise(valid=n()==6 && setequal(question_idx,1:6),.groups="drop")
  check(all(coverage$valid) && nrow(coverage)==nrow(meta),"Incomplete trust coverage")
  trust <- q %>% group_by(participant_id,run_timestamp,block_idx) %>% summarise(trust=mean(response),.groups="drop") %>% inner_join(meta,by=c("participant_id","run_timestamp","block_idx"))
  sliders <- sub("_ALL.csv$","_POSTBLOCK_SLIDERS_ALL.csv",manifest$path[manifest$kind=="trials"])
  rating <- bind_rows(lapply(sliders,read_identity_csv)) %>% filter(question_key=="perc_auto_correct") %>%
    select(participant_id,run_timestamp,block_idx,perceived=response_percent) %>% inner_join(meta,by=c("participant_id","run_timestamp","block_idx"))
  bench <- benchmark_components(dat)
  rating <- rating %>% left_join(select(bench,participant_id,pressure,aid_accuracy),by=c("participant_id","pressure")) %>%
    mutate(signed_error=perceived-100*aid_accuracy,absolute_error=abs(signed_error))
  check(nrow(rating)==nrow(meta) && !anyDuplicated(rating[c("participant_id","block_idx")]) && all(rating$perceived>=0 & rating$perceived<=100),"Invalid slider coverage")
  decom <- completion_components(dat)
  seqdat <- make_sequence(dat)
  sequence_raw <- seqdat %>% group_by(participant_id,pattern,pressure,reliability,previous_aid_error) %>% summarise(n=n(),agreement=mean(agreement),.groups="drop")
  write_csv(seqdat %>% select(all_of(keys),previous_trial,previous_aid_error,previous_response_error,advice,agreement,progress),file.path(stage,"sequence_eligibility.csv"))
  inputs <- unique(c(input,manifest$path,sliders,primary_source_paths(root)))
  write_csv(tibble(path=normalizePath(inputs),md5=unname(tools::md5sum(inputs))),file.path(stage,"input_checksums.csv"))
  tables <- list(); diagnostics <- list(); weights <- list(); estimates <- list(); audit <- list()
  fit <- function(name,formula,d,binary=FALSE) {
    z <- primary_fit(name,formula,d,binary,stage); diagnostics[[name]] <<- z$diagnostics; z
  }
  add <- function(em,methods,h,o,m,co="none",scale=1,usable=TRUE,d=NULL,value=NULL,analysis="mixed_model") {
    key <- paste(h,o,co,analysis,sep="__")
    tables[[key]] <<- primary_table(em,methods,h,o,m,co,scale,usable,analysis)
    weights[[key]] <<- bind_rows(lapply(names(methods),function(id) as.data.frame(em) %>% mutate(
      hypothesis=h,outcome=o,model=m,correction=co,analysis=analysis,contrast_id=id,weight=methods[[id]])))
    audit[[key]] <<- list(em=em,methods=methods,scale=scale,usable=usable)
    if(!is.null(d)) {
      ss <- participant_sensitivity(d,value,as.data.frame(em),methods,o,scale) %>%
        transmute(hypothesis=h,outcome=o,model=m,correction=co,analysis="participant_sensitivity",contrast_id=contrast,
          estimate,lower,upper,se,df,p_raw,inference_available=TRUE)
      tables[[paste0(key,"__sensitivity")]] <<- ss
    }
  }
  emmean <- function(m,spec,...) emmeans(m,spec,lmer.df="satterthwaite",lmerTest.limit=Inf,...)
  # Shared performance models; H1 is the equally weighted manual pressure contrast.
  for(o in c("accuracy","correct_rt","task_dprime")) for(co in if(o=="task_dprime")c("loglinear","extreme_only") else "none") {
    name <- paste(o,co,sep="_")
    d <- if(o=="accuracy")dat else if(o=="correct_rt")filter(dat,valid_correct_rt) else filter(scores,actor=="human",correction==co)
    response <- switch(o,accuracy="accuracy",correct_rt="log(rt_s)",task_dprime="dprime")
    z <- fit(name,as.formula(paste(response,"~ mode * pressure * pattern + (1|subject)")),d,o=="accuracy")
    em <- emmean(z$model,~mode*pressure*pattern);if(o=="accuracy")em <- regrid(em,transform="response")
    scale <- if(o=="accuracy")100 else 1
    participant <- if(o=="task_dprime")d else perf;value <- if(o=="task_dprime")"dprime" else o
    methods <- primary_performance(as.data.frame(em),o!="correct_rt")
    for(h in c("H1","H2"))add(em,methods[[h]],h,o,name,co,scale,z$diagnostics$inference_available,participant,value)
    estimates[[paste0(name,"_manual")]] <- primary_table(em,methods$manual,"H1",o,name,co,scale,z$diagnostics$inference_available)
    estimates[[paste0(name,"_cells")]] <- as.data.frame(em) %>% mutate(model=name,outcome=o,correction=co)
  }
  # All aided scores and ratings use the same four-cell model and five comparisons.
  for(co in c("loglinear","extreme_only")) {
    human <- filter(scores,actor=="human",mode=="Automation",correction==co)
    matched <- filter(scores,actor=="aid_matched",correction==co)
    taskbench <- human %>% select(participant_id,subject,pattern,pressure,reliability,human_dprime=dprime) %>%
      inner_join(select(matched,participant_id,pressure,aid_dprime=dprime),by=c("participant_id","pressure")) %>% mutate(value=human_dprime-aid_dprime)
    for(o in c("task_dprime_benchmark","advice_bias","task_bias","advice_dprime","task_dprime")) {
      d <- if(o=="task_dprime_benchmark")taskbench else if(grepl("^advice",o))filter(advice,correction==co) else human
      value <- switch(o,task_dprime_benchmark="value",advice_bias="automation_bias",task_bias="black_bias",advice_dprime="dprime",task_dprime="dprime")
      h <- if(o=="task_dprime_benchmark")"H3" else if(grepl("bias",o))"H4" else "H5"
      name <- paste(h,o,co,sep="_");z <- fit(name,as.formula(paste(value,"~ pressure * reliability + (1|subject)")),d)
      em <- emmean(z$model,~pressure*reliability); methods <- primary_five(as.data.frame(em))
      if(h=="H3")methods <- c(primary_cells(as.data.frame(em)),methods)
      add(em,methods,h,o,name,co,1,z$diagnostics$inference_available,d,value)
      estimates[[name]] <- primary_table(em,primary_cells(as.data.frame(em)),h,o,name,co,1,z$diagnostics$inference_available)
    }
  }
  for(o in c("accuracy_benchmark","trust","perceived","signed_error")) {
    h <- if(o=="accuracy_benchmark")"H3" else "H6";d <- if(h=="H3")bench else if(o=="trust")trust else rating
    value <- if(h=="H3")"net_gain" else o;scale <- if(h=="H3")100 else 1;name <- paste(h,o,sep="_")
    z <- fit(name,as.formula(paste(value,"~ pressure * reliability + (1|subject)")),d);em <- emmean(z$model,~pressure*reliability)
    methods <- if(o=="signed_error")primary_cells(as.data.frame(em)) else primary_five(as.data.frame(em))
    if(h=="H3")methods <- c(primary_cells(as.data.frame(em)),methods)
    add(em,methods,h,o,name,"none",scale,z$diagnostics$inference_available,d,value)
    estimates[[name]] <- primary_table(em,primary_cells(as.data.frame(em)),h,o,name,"none",scale,z$diagnostics$inference_available)
  }
  trust_scores <- trust %>% group_by(participant_id) %>% mutate(trust_between=mean(trust),trust_within=trust-mean(trust)) %>%
    ungroup() %>% mutate(trust_between=trust_between-mean(trust_between),block_position=block_idx-3.5)
  associations <- inner_join(advice,select(trust_scores,participant_id,pressure,trust,trust_between,trust_within,block_position),by=c("participant_id","pressure"))
  for(co in c("loglinear","extreme_only"))for(o in c("advice_bias","advice_dprime")) {
    d <- filter(associations,correction==co) %>% mutate(value=if(o=="advice_bias")automation_bias else dprime)
    # Pressure x allocation pattern spans the same four aided-condition means
    # as pressure x reliability, without aliasing the pattern main effect.
    name <- paste("H7",o,co,sep="_")
    f <- value ~ pressure * pattern + (trust_within + trust_between) * pattern + block_position + (1|subject)
    environment(f) <- asNamespace("stats")
    z <- fit(name,f,d);lmfit <- lm(nobars(f),d);vc <- sandwich::vcovCL(lmfit,cluster=d$participant_id,type="HC1")
    saveRDS(lmfit,file.path(stage,"models",paste0(name,"_sensitivity.rds")))
    for(component in c("trust_within","trust_between"))for(kind in c("mixed_model","participant_cluster_sensitivity")) {
      em <- if(kind=="mixed_model")emtrends(z$model,~pattern,var=component,lmer.df="satterthwaite") else
        emtrends(lmfit,~pattern,var=component,vcov.=vc,df=n_distinct(d$participant_id)-1)
      g <- as.data.frame(em)
      methods <- setNames(lapply(patterns,function(p)as.numeric(g$pattern==p)),paste(component,patterns,sep="_"))
      methods[[paste0(component,"_group_difference")]] <- as.numeric(g$pattern==patterns[1])-as.numeric(g$pattern==patterns[2])
      # add() keys distinguish slope components before combining the Holm family.
      temp_o <- paste(o,component,sep="__")
      add(em,methods,"H7",temp_o,name,co,1,if(kind=="mixed_model")z$diagnostics$inference_available else TRUE,analysis=kind)
    }
    # Retain the preceding additive specification as a separate sensitivity,
    # matching the group-interaction update in dynamic_reliability.
    common_name <- paste0(name,"_common")
    common <- fit(common_name,value ~ pressure * reliability + trust_within + trust_between + block_position + (1|subject),d)
    for(component in c("trust_within","trust_between")) {
      em <- emtrends(common$model,~1,var=component,lmer.df="satterthwaite")
      add(em,setNames(list(1),component),"H7",paste(o,component,sep="__"),common_name,co,1,
        common$diagnostics$inference_available,analysis="common_slope_sensitivity")
    }
  }
  f <- agreement ~ pressure * reliability * advice + previous_aid_error * pressure * reliability + previous_response_error + progress + (1|subject)
  environment(f) <- asNamespace("stats")
  z <- fit("H8_history",f,seqdat,TRUE)
  seqdat <- seqdat %>% group_by(participant_id,block_idx) %>% mutate(block_weight=1/n()) %>% ungroup()
  lmfit <- lm(nobars(f),seqdat,weights=block_weight);vc <- sandwich::vcovCL(lmfit,cluster=seqdat$participant_id,type="HC1")
  saveRDS(lmfit,file.path(stage,"models/H8_history_sensitivity.rds"))
  for(kind in c("mixed_model","participant_cluster_sensitivity")) {
    spec <- ~previous_aid_error*pressure*reliability*advice
    at <- list(previous_response_error=mean(seqdat$previous_response_error),progress=0)
    em <- if(kind=="mixed_model")regrid(emmean(z$model,spec,at=at),transform="response") else emmeans(lmfit,spec,vcov.=vc,df=n_distinct(seqdat$participant_id)-1,at=at)
    methods <- lag_contrasts(as.data.frame(em));names(methods) <- c(names(primary_cells(primary_grid())),"pressure_65","pressure_95","interaction")
    add(em,methods,"H8","agreement","H8_history","none",100,if(kind=="mixed_model")z$diagnostics$inference_available else TRUE,analysis=kind)
    if(kind=="mixed_model") {
      g <- as.data.frame(em); hist <- list()
      for(p in c("HP","LP"))for(r in c("65%","95%"))for(e in c("Correct","Error"))hist[[paste(p,r,e,sep="_")]] <-
        as.numeric(g$pressure==p & g$reliability==r & g$previous_aid_error==e)*ifelse(g$advice=="Correct",if(r=="95%") .95 else .65,if(r=="95%") .05 else .35)
      estimates$H8_history <- primary_table(em,hist,"H8","agreement","H8_history","none",100,z$diagnostics$inference_available)
    }
  }
  results <- bind_rows(tables) %>% mutate(outcome=sub("__trust_(within|between)$","",outcome)) %>%
    group_by(hypothesis,outcome,correction,analysis) %>% mutate(family=paste(hypothesis,outcome,correction,analysis,sep=" / "),
      family_size=n(),p_holm=p.adjust(p_raw,"holm",n=n())) %>% ungroup() %>%
    mutate(ratio=ifelse(outcome=="correct_rt",exp(estimate),NA_real_),ratio_lower=ifelse(outcome=="correct_rt",exp(lower),NA_real_),
      ratio_upper=ifelse(outcome=="correct_rt",exp(upper),NA_real_),
      units=case_when(hypothesis=="H7"~"SDT units per trust point",outcome %in% c("accuracy","accuracy_benchmark","agreement")~"percentage points",outcome=="correct_rt"~"log ratio; plotted as ratio",
        outcome=="trust"~"trust points",outcome %in% c("perceived","signed_error")~"rating percentage points",TRUE~"SDT units"))
  # Refresh superseded probability measures on the SAME included cohort. Their
  # historical 10- and 5-test families stay separate from the 105 primary tests.
  hz <- fit("historical_agreement",agreement ~ pressure * reliability * advice + (1|subject),filter(dat,mode=="Automation",!timeout),TRUE)
  he <- regrid(emmean(hz$model,~pressure*reliability*advice),transform="response");hg <- as.data.frame(he)
  cg <- primary_grid();index <- match(paste(hg$pressure,hg$reliability),paste(cg$pressure,cg$reliability))
  hm <- list();hmean <- list()
  for(a in c("Incorrect","Correct")) {
    for(id in names(primary_five(cg)))hm[[paste(a,id,sep="__")]] <- primary_five(cg)[[id]][index]*as.numeric(hg$advice==a)
    for(id in names(primary_cells(cg)))hmean[[paste(a,id,sep="__")]] <- primary_cells(cg)[[id]][index]*as.numeric(hg$advice==a)
  }
  sign_advice <- ifelse(hg$advice=="Correct",1,-1)
  sm <- lapply(primary_five(cg),function(w)w[index]*sign_advice)
  smeans <- lapply(primary_cells(cg),function(w)w[index]*sign_advice)
  ht <- primary_table(he,hm,"Historical_H5","agreement","historical_agreement",scale=100,usable=hz$diagnostics$inference_available) %>% mutate(p_holm=p.adjust(p_raw,"holm"),family_size=n())
  st <- primary_table(he,sm,"Historical_H9","selectivity","historical_agreement",scale=100,usable=hz$diagnostics$inference_available) %>% mutate(p_holm=p.adjust(p_raw,"holm"),family_size=n())
  hp <- acount %>% mutate(agreement=n_agree/n_answered)
  hs <- hp %>% select(participant_id,pattern,pressure,reliability,advice,agreement) %>% pivot_wider(names_from=advice,values_from=agreement) %>% mutate(selectivity=Correct-Incorrect)
  hpt <- participant_sensitivity(hp,"agreement",hg,hm,"agreement",100)
  hst <- participant_sensitivity(hs,"selectivity",cg,primary_five(cg),"selectivity",100)
  historical <- list(participants=hp,selectivity=hs,agreement_tests=ht,selectivity_tests=st,
    agreement_means=primary_table(he,hmean,"Historical_H5","agreement","historical_agreement",scale=100),
    selectivity_means=primary_table(he,smeans,"Historical_H9","selectivity","historical_agreement",scale=100),
    agreement_sensitivity=hpt,selectivity_sensitivity=hst)
  for(nm in names(historical))write_csv(historical[[nm]],file.path(stage,paste0("historical_",nm,".csv")))
  saveRDS(list(em=he,agreement=hm,selectivity=sm),file.path(stage,"historical_audit.rds"))
  data <- list(performance=perf,task_counts=counts,task_scores=scores,advice_counts=acount,advice_scores=advice,trust=trust,
    ratings=rating,benchmark=bench,decomposition=decom,sequence_raw=sequence_raw,associations=associations,
    cohort=dat %>% distinct(participant_id,source_participant_id,run_timestamp,pattern),cohort_selection=selection,exclusions=exclusions)
  for(nm in names(data))write_csv(data[[nm]],file.path(stage,paste0(nm,".csv")))
  write_csv(results,file.path(stage,"contrasts.csv"));write_csv(bind_rows(diagnostics),file.path(stage,"model_diagnostics.csv"))
  write_csv(filter(results,analysis=="common_slope_sensitivity"),file.path(stage,"common_slope_contrasts.csv"))
  write_csv(bind_rows(weights),file.path(stage,"contrast_weights.csv"));saveRDS(estimates,file.path(stage,"estimates.rds"));saveRDS(audit,file.path(stage,"contrast_audit.rds"))
  saveRDS(list(data=data,results=results,estimates=estimates,diagnostics=bind_rows(diagnostics),historical=historical),file.path(stage,"bundle.rds"))
  writeLines(c("Revised primary framework agreed 2026-10-17 after inspection of previous results; not preregistered.",
    cohort_note,
    "Recorded ID 083 was reused for two different participant runs: 20261015_125827 retains analysis ID 083; 20261015_130522 has analysis ID 084. Both retained with the allocation actually administered under recorded ID 083. This is an identity correction, not a participant exclusion.",
    "Two-sided tests; Holm separately within hypothesis x outcome x correction x analysis. Sensitivities never replace primary tests.",
    "Pointwise 95% model confidence intervals. Binary predictions condition on zero participant random effect; response-scale contrasts follow regridding.",
    "H1 manual effect equally weights allocation patterns. At fixed pressure/reliability the aided simple contrasts compare allocation groups.",
    "RT uses correct, positive, finite responses, log-scale models, geometric-mean ratios; ratios below one indicate shorter response time.",
    "Task dprime and bias use answered BLACK/WHITE classifications. Advice SDT uses agreement on correct/incorrect advice. Loglinear primary; extreme-only sensitivity.",
    "H3 accuracy: human all-trial accuracy minus realised advice accuracy, assuming timely advice. H3 task dprime: matched human-answered trials.",
    "H4 advice -c = acceptance; task -c = BLACK preference. H5 advice dprime = selectivity; task dprime = participant-plus-aid discrimination.",
    "SDT frames summarise the same decisions and are not independent replication. Answered-trial selection remains relevant.",
    "H2 compensation is a derived contrast; completion/choice is descriptive accounting, not causal mediation.",
    "H7 revised 2026-10-18: within- and between-person trust slopes vary by allocation pattern, adjusting for pressure x pattern and block position. Pressure x pattern spans the same four condition means as pressure x reliability.",
    "H7 has six tests per SDT outcome: four group slopes and two HP95_LP65-minus-HP65_LP95 slope differences, Holm-adjusted together. Both SDT corrections and participant-cluster sensitivities use this specification. Post-block trust associations are not causal effects.",
    "H7 additive models remain as common_slope_sensitivity with separate two-test Holm families per SDT outcome and correction. The group-interaction update followed inspection of common-slope plots and is retrospective.",
    "H8 excludes current/previous timeouts and nonconsecutive trials, controls previous response errors and progress, and weights current advice by assigned reliability.",
    "Random-intercept models are accompanied by participant contrast/cluster sensitivities and dispersion/residual diagnostics. Numerical convergence does not establish adequacy.",
    "Singular, nonconverged or rank-deficient models have no primary p-values. Missing inference is not nonsignificance."),file.path(stage,"methods.txt"))
  writeLines(capture.output(sessionInfo()),file.path(stage,"session_info.txt"))
  writeLines(paste("Analysis completed",format(Sys.time(),tz="UTC")),file.path(stage,"COMPLETE.txt"))
  destination <- file.path(base,"primary")
  if(dir.exists(destination)) {
    archive <- file.path(base,"primary_archive");dir.create(archive,showWarnings=FALSE)
    check(file.rename(destination,tempfile(paste0("primary_",format(Sys.time(),"%Y%m%d_%H%M%S"),"_"),tmpdir=archive)),"Cannot archive preceding primary analysis")
  }
  check(file.rename(stage,destination),"Cannot publish primary analysis")
  message("Primary analyses: ",destination);invisible(destination)
}
if(sys.nframe()==0) {
  args <- commandArgs(trailingOnly=TRUE);stopifnot(length(args)<=2)
  run_time_pressure_primary(if(length(args)>=1)args[1] else file.path(PRIMARY_ROOT,"analysis_outputs/semester2_2026_behavioural"),
    if(length(args)>=2)args[2] else file.path(PRIMARY_ROOT,"data/semester2_2026_data/data_virus_all.csv"))
}
