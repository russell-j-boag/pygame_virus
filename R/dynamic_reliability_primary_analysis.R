# Statistical implementation of the revised, retrospective H1-H7 contract.
dpr_specs <- function(grid, pairs, dimension="stage", prefix="", advice_value=NA_character_) {
  weights <- list(); metadata <- list()
  for (pair in pairs) {
    left <- pair[1]; right <- if(length(pair)>1)pair[2] else NA_character_
    base <- function(g) {
      w <- as.numeric(grid$group==g & grid[[dimension]]==left)
      if(!is.na(right))w <- w-as.numeric(grid$group==g & grid[[dimension]]==right)
      w
    }
    for (g in c(GROUPS,"CAL90 - CAL65")) {
      id <- paste(prefix,g,left,ifelse(is.na(right),"zero",right),sep="__")
      weights[[id]] <- if(g=="CAL90 - CAL65")base("CAL90")-base("CAL65") else base(g)
      metadata[[id]] <- tibble(contrast_id=id,group=g,phase=left,advice=advice_value,
        comparison=if(is.na(right))paste(left,"versus zero") else paste(left,"minus",right))
    }
  }
  list(weights=weights, metadata=bind_rows(metadata))
}
dpr_trend_specs <- function(grid) {
  weights <- list(); metadata <- list()
  for(a in if("advice" %in% names(grid))ADVICE else "All")for(p in c("P1","P2","P3"))for(g in c(GROUPS,"CAL90 - CAL65")) {
    cell <- function(gr)as.numeric(grid$group==gr & grid$phase==p &
      if(a=="All")TRUE else grid$advice==a)*ifelse(grid$progress==1,1,-1)
    id <- paste(a,p,g,"end_start",sep="__")
    weights[[id]] <- if(g=="CAL90 - CAL65")cell("CAL90")-cell("CAL65") else cell(g)
    metadata[[id]] <- tibble(contrast_id=id,group=g,phase=p,advice=a,comparison=paste(p,"end minus start"))
  }
  list(weights=weights, metadata=bind_rows(metadata))
}
dpr_association_specs <- function(grid,predictor,scope,common=FALSE) {
  if(common)return(list(weights=setNames(list(1),predictor),metadata=tibble(contrast_id=predictor,
    group="Adjusted common slope",phase=scope,advice=NA_character_,comparison=paste("Per unit",predictor))))
  groups <- c(GROUPS,"CAL90 - CAL65");ids <- paste(predictor,groups,sep="__")
  weights <- lapply(groups,function(g)if(g=="CAL90 - CAL65")as.numeric(grid$group=="CAL90")-as.numeric(grid$group=="CAL65") else as.numeric(grid$group==g))
  list(weights=setNames(weights,ids),metadata=tibble(contrast_id=ids,group=groups,phase=scope,advice=NA_character_,
    comparison=ifelse(groups=="CAL90 - CAL65",paste("Slope difference per unit",predictor),paste("Slope per unit",predictor))))
}
dpr_fit <- function(name, formula, data, binary_outcome, out, cached=NULL) {
  environment(formula) <- asNamespace("stats")
  warnings <- character(); origin <- "new fit"; model <- NULL
  if(!is.null(cached) && file.exists(cached)) {
    candidate <- readRDS(cached)
    wanted <- model.frame(lme4::subbars(formula),data,na.action=na.omit)
    actual <- model.frame(candidate)
    if(identical(all.vars(formula),all.vars(formula(candidate))) &&
       identical(names(actual),names(wanted)) &&
       isTRUE(all.equal(as.data.frame(actual),as.data.frame(wanted),check.attributes=FALSE,tolerance=1e-12)) &&
       identical(paste(deparse(formula(candidate)),collapse=""),paste(deparse(formula),collapse=""))) {
      model <- candidate; origin <- paste("validated identical fit:",cached)
    }
  }
  message("Revised model ",name," (",nrow(data)," rows; ",if(is.null(model))"fitting" else "identical validated fit",")")
  if(is.null(model))model <- tryCatch(withCallingHandlers(
    if(binary_outcome)glmer(formula,data=data,family=binomial(),control=strict_control(TRUE)) else
      lmerTest::lmer(formula,data=data,REML=TRUE,control=strict_control(FALSE)),
    warning=function(w){warnings <<- c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=identity)
  if(inherits(model,"error"))return(list(model=NULL,diagnostics=tibble(model=name,n_observations=nrow(data),
    n_participants=n_distinct(data$participant_id),converged=FALSE,singular=NA,rank_deficient=NA,
    inference_available=FALSE,warnings=conditionMessage(model),origin=origin)))
  cv <- model@optinfo$conv
  converged <- is.null(cv$lme4$messages) && all(cv$opt==0) && all(is.finite(fixef(model)))
  singular <- isSingular(model,tol=1e-4); deficient <- !is.null(attr(getME(model,"X"),"col.dropped"))
  gradient <- tryCatch(max(abs(solve(chol(model@optinfo$derivs$Hessian),model@optinfo$derivs$gradient))),error=function(e)NA_real_)
  diag <- tibble(n_observations=nobs(model),n_participants=n_distinct(data$participant_id),
    converged,singular,rank_deficient=deficient,inference_available=converged && !singular && !deficient,
    max_scaled_gradient=gradient,warnings=paste(unique(c(warnings,cv$lme4$messages)),collapse=" | "),origin,model=name)
  keys <- intersect(c("subject","group","stage","phase","advice"),all.vars(formula))
  checks <- data %>% mutate(.p=fitted(model),.y=model.response(model.frame(model)),.res=residuals(model,type="pearson")) %>%
    group_by(across(all_of(keys))) %>% summarise(n=n(),observed=sum(.y),expected=sum(.p),
      variance=sum(.p*(1-.p)),residual_sd=sd(.res),.groups="drop")
  if(binary_outcome)diag$conditional_cell_dispersion <- mean((checks$observed-checks$expected)^2/pmax(checks$variance,1e-10))
  else {
    spread <- data %>% mutate(.res=residuals(model)) %>% group_by(across(all_of(setdiff(keys,"subject")))) %>%
      summarise(n=n(),residual_sd=sd(.res),.groups="drop")
    valid_sd <- spread$residual_sd[is.finite(spread$residual_sd) & spread$residual_sd>0]
    diag$residual_sd_max_min_ratio <- if(length(valid_sd))max(valid_sd)/min(valid_sd) else NA_real_
    write_csv(spread,file.path(out,paste0(name,"_residual_cells.csv")))
    p <- ggplot(tibble(residual=residuals(model)),aes(sample=residual))+stat_qq(alpha=.2,size=.5)+stat_qq_line()+
      theme_minimal()+labs(title=paste(name,"residual Q-Q plot"))
    ggsave(file.path(out,paste0(name,"_residual_qq.png")),p,width=7,height=5,dpi=110)
  }
  write_csv(checks,file.path(out,paste0(name,"_cell_diagnostics.csv")))
  saveRDS(model,file.path(out,"models",paste0(name,".rds")))
  writeLines(capture.output(summary(model)),file.path(out,paste0(name,"_summary.txt")))
  list(model=model,diagnostics=diag)
}
dpr_hc3_fit <- function(name,formula,data,out) {
  environment(formula) <- asNamespace("stats"); model <- lm(formula,data=data)
  v <- sandwich::vcovHC(model,type="HC3")
  ok <- model$rank==ncol(model.matrix(model)) && df.residual(model)>0 && all(is.finite(v))
  diag <- tibble(n_observations=nobs(model),n_participants=n_distinct(data$participant_id),
    converged=TRUE,singular=FALSE,rank_deficient=model$rank<ncol(model.matrix(model)),inference_available=ok,
    max_leverage=max(hatvalues(model)),max_cooks_distance=max(cooks.distance(model)),origin="new participant regression; HC3",model=name)
  saveRDS(model,file.path(out,"models",paste0(name,".rds")))
  writeLines(capture.output(summary(model)),file.path(out,paste0(name,"_summary.txt")))
  list(model=model,vcov=v,diagnostics=diag)
}
dpr_table <- function(em,spec,scale=1,usable=TRUE) {
  x <- as.data.frame(summary(contrast(em,spec$weights),infer=c(TRUE,TRUE),adjust="none"))
  lo <- intersect(c("lower.CL","asymp.LCL"),names(x));hi <- intersect(c("upper.CL","asymp.UCL"),names(x))
  left_join(tibble(contrast_id=x$contrast,estimate=x$estimate*scale,se=x$SE*scale,df=x$df,
    lower=if(usable)x[[lo]]*scale else NA_real_,upper=if(usable)x[[hi]]*scale else NA_real_,
    p_raw=if(usable)x$p.value else NA_real_,inference_available=usable),spec$metadata,by="contrast_id")
}
dpr_means <- function(em,scale,usable) {
  x <- as.data.frame(summary(em,infer=c(TRUE,FALSE)))
  value <- intersect(c("emmean","prob","response","rate"),names(x))[1]
  lo <- intersect(c("lower.CL","asymp.LCL"),names(x));hi <- intersect(c("upper.CL","asymp.UCL"),names(x))
  x$estimate <- x[[value]]*scale
  x$lower <- if(usable)x[[lo]]*scale else NA_real_; x$upper <- if(usable)x[[hi]]*scale else NA_real_
  x$inference_available <- usable
  as_tibble(x)
}
dpr_participant <- function(data,value,grid,spec,scale=1) {
  keys <- intersect(c("group","stage","phase","advice","progress"),names(grid))
  bind_rows(lapply(names(spec$weights),function(id) {
    active <- as_tibble(grid[keys]) %>% mutate(weight=spec$weights[[id]]) %>% filter(weight!=0)
    cells <- inner_join(data,active,by=keys)
    required <- count(active,group,name="required")
    x <- cells %>% group_by(participant_id,group) %>% summarise(n=n(),value=sum(.data[[value]]*weight),.groups="drop") %>%
      left_join(required,by="group") %>% filter(n==required,is.finite(value))
    z <- x %>% group_by(group) %>% summarise(n=n(),estimate=mean(value),variance=var(value)/n(),.groups="drop")
    ok <- nrow(z)==n_distinct(active$group) && all(z$n>1) && all(is.finite(z$variance))
    estimate <- if(ok)sum(z$estimate) else NA_real_; se <- if(ok)sqrt(sum(z$variance)) else NA_real_
    df <- if(ok && se>0)sum(z$variance)^2/sum(z$variance^2/(z$n-1)) else Inf
    p <- if(!ok)NA_real_ else if(se==0)ifelse(estimate==0,1,0) else 2*pt(-abs(estimate/se),df)
    lower <- (estimate-qt(.975,df)*se)*scale;upper <- (estimate+qt(.975,df)*se)*scale
    left_join(tibble(contrast_id=id,estimate=estimate*scale,se=se*scale,df,lower,upper,
      p_raw=p,inference_available=ok,n_participants=nrow(x)),spec$metadata,by="contrast_id")
  }))
}
dpr_analyse_sample <- function(trials,scores,advice,ratings,out,cached,sample_name) {
  tables <- means <- audit <- diagnostics <- model_index <- list()
  add <- function(em,spec,h,o,name,correction="none",scale=1,ok=TRUE,participant=NULL,value=NULL,analysis="primary") {
    key <- paste(h,o,name,analysis,sep="__")
    tables[[key]] <<- dpr_table(em,spec,scale,ok) %>% mutate(hypothesis=h,outcome=o,model=name,correction,analysis=analysis)
    if(!is.null(participant))tables[[paste0(key,"__participant")]] <<-
      dpr_participant(participant,value,as.data.frame(em),spec,scale) %>%
      mutate(hypothesis=h,outcome=o,model=name,correction,analysis="participant_sensitivity")
    audit[[key]] <<- list(em=em,spec=spec,scale=scale,model=name,hypothesis=h,outcome=o,correction=correction,usable=ok,analysis=analysis)
  }
  fit <- function(name,formula,d,binary=FALSE,cache_name=NULL) {
    z <- dpr_fit(name,formula,d,binary,out,if(!is.null(cache_name))file.path(cached,"models",paste0(cache_name,".rds")) else NULL)
    diagnostics[[name]] <<- z$diagnostics
    model_index[[name]] <<- tibble(model=name,path=paste(sample_name,"models",paste0(name,".rds"),sep="/"),formula=paste(deparse(formula),collapse=""))
    if(is.null(z$model))stop("Model failed; preceding analysis preserved. See diagnostics for ",name,call.=FALSE)
    z
  }
  emmean <- function(m,spec,...)emmeans(m,spec,lmer.df="satterthwaite",lmerTest.limit=Inf,...)
  perf <- trials %>% group_by(participant_id,subject,group,stage) %>% summarise(answered_accuracy=mean(accuracy[!timeout]),
    accuracy=mean(accuracy),correct_rt=mean(log_rt[valid_correct_rt]),completion=mean(!timeout),.groups="drop")
  human <- filter(scores,agent=="participant")
  for(o in c("accuracy","correct_rt","task_dprime"))for(co in if(o=="task_dprime")c("loglinear","extreme_only") else "none") {
    name <- paste("performance",o,co,sep="_")
    d <- if(o=="accuracy")trials else if(o=="correct_rt")filter(trials,valid_correct_rt) else filter(human,correction==co)
    response <- switch(o,accuracy="accuracy",correct_rt="log_rt",task_dprime="dprime")
    formula <- as.formula(paste(response,"~ group * stage + (1 | subject)"))
    z <- fit(name,formula,d,o=="accuracy",if(o=="task_dprime")NULL else o)
    em <- emmean(z$model,~group*stage);if(o=="accuracy")em <- regrid(em,transform="response")
    scale <- if(o=="accuracy")100 else 1; ok <- z$diagnostics$inference_available
    raw <- if(o=="task_dprime")d else perf;value <- if(o=="task_dprime")"dprime" else o
    add(em,dpr_specs(as.data.frame(em),lapply(c("P1","P2","P3"),function(p)c(p,"Manual pre"))),
      "H1",o,name,co,scale,ok,raw,value)
    add(em,dpr_specs(as.data.frame(em),list(c("Manual post","Manual pre"))),"S2",o,name,co,scale,ok,raw,value)
    means[[name]] <- dpr_means(em,scale,ok)
  }
  aided <- filter(trials,!is.na(phase)); answered <- filter(aided,!timeout)
  benchmark <- aided %>% group_by(participant_id,subject,group,phase) %>%
    summarise(human_accuracy=mean(accuracy),aid_accuracy=mean(aid_label==stimulus),value=human_accuracy-aid_accuracy,.groups="drop")
  taskbench <- inner_join(filter(human,!is.na(phase)) %>% select(participant_id,subject,group,phase,correction,human=dprime),
    filter(scores,agent=="aid_matched") %>% select(participant_id,phase,correction,aid=dprime),
    by=c("participant_id","phase","correction")) %>% mutate(value=human-aid)
  for(o in c("accuracy_benchmark","task_dprime_benchmark"))for(co in if(o=="accuracy_benchmark")"none" else c("loglinear","extreme_only")) {
    d <- if(o=="accuracy_benchmark")benchmark else filter(taskbench,correction==co)
    name <- paste("benchmark",o,co,sep="_");z <- fit(name,value~group*phase+(1|subject),d)
    em <- emmean(z$model,~group*phase);scale <- if(o=="accuracy_benchmark")100 else 1
    add(em,dpr_specs(as.data.frame(em),as.list(c("P1","P2","P3")),"phase"),"H2",o,name,co,scale,
      z$diagnostics$inference_available,d,"value")
    means[[name]] <- dpr_means(em,scale,z$diagnostics$inference_available)
  }
  for(o in c("advice_bias","task_bias","advice_dprime","task_dprime"))for(co in c("loglinear","extreme_only")) {
    d <- if(startsWith(o,"advice"))filter(advice,phase!="FULL",correction==co) else filter(human,!is.na(phase),correction==co)
    d <- d %>% mutate(phase=factor(phase,c("P1","P2","P3")),
      value=if(o=="advice_bias")advice_bias else if(o=="task_bias")task_bias else dprime)
    name <- paste("aided",o,co,sep="_");z <- fit(name,value~group*phase+(1|subject),d);em <- emmean(z$model,~group*phase)
    for(h in c("H3","H4"))add(em,dpr_specs(as.data.frame(em),
      if(h=="H3")list(c("P2","P1")) else list(c("P3","P2"),c("P3","P1")),"phase"),
      h,o,name,co,1,z$diagnostics$inference_available,d,"value")
    means[[name]] <- dpr_means(em,1,z$diagnostics$inference_available)
  }
  trajectories <- list(); slopes <- list()
  for(o in c("accuracy","agreement")) {
    d <- if(o=="accuracy")aided else answered
    formula <- if(o=="accuracy")accuracy~group*phase*progress+(1|subject) else agreement~group*phase*advice*progress+(1|subject)
    name <- paste0(o,"_trend");z <- fit(name,formula,d,TRUE,name)
    spec <- if(o=="accuracy")~group*phase*progress else ~group*phase*advice*progress
    em <- regrid(emmean(z$model,spec,at=list(progress=c(0,1))),transform="response")
    keys <- intersect(c("participant_id","subject","group","phase","advice"),all.vars(formula))
    keys <- unique(c("participant_id",keys))
    sl <- d %>% group_by(across(all_of(keys))) %>% summarise(slope=if(n_distinct(progress)>=2)
      unname(coef(lm(.data[[o]]~progress))[2]) else NA_real_,.groups="drop")
    # Linear-probability slopes remain an explicitly different sensitivity estimand.
    pp <- bind_rows(mutate(sl,progress=0,value=0),mutate(sl,progress=1,value=slope))
    add(em,dpr_trend_specs(as.data.frame(em)),"H5",o,name,"none",100,z$diagnostics$inference_available,pp,"value")
    curves <- regrid(emmean(z$model,spec,at=list(progress=seq(0,1,length.out=41))),transform="response")
    means[[name]] <- dpr_means(curves,100,z$diagnostics$inference_available)
    trajectories[[o]] <- d %>% group_by(across(all_of(c(keys,"bin")))) %>%
      summarise(value=mean(.data[[o]]),n=n(),progress=mean(progress),.groups="drop") %>% mutate(outcome=o)
    slopes[[o]] <- mutate(sl,outcome=o)
  }
  for(o in c("trust","perceived","signed_error")) {
    name <- paste0("ratings_",o);z <- dpr_hc3_fit(name,reformulate("group",response=o),ratings,out)
    diagnostics[[name]] <- z$diagnostics;model_index[[name]] <- tibble(model=name,path=paste(sample_name,"models",paste0(name,".rds"),sep="/"),formula=paste(o,"~ group"))
    em <- emmeans(z$model,~group,vcov.=z$vcov,df=df.residual(z$model));grid <- as.data.frame(em)
    weights <- list(group_difference=as.numeric(grid$group=="CAL90")-as.numeric(grid$group=="CAL65"))
    meta <- tibble(contrast_id="group_difference",group="CAL90 - CAL65",phase=NA_character_,advice=NA_character_,comparison="CAL90 minus CAL65")
    if(o=="signed_error")for(g in GROUPS) {
      weights[[paste0("zero_",g)]] <- as.numeric(grid$group==g)
      meta <- bind_rows(meta,tibble(contrast_id=paste0("zero_",g),group=g,phase=NA_character_,advice=NA_character_,comparison="Signed error versus zero"))
    }
    add(em,list(weights=weights,metadata=meta),"H6",o,name,ok=z$diagnostics$inference_available)
    means[[name]] <- dpr_means(em,1,z$diagnostics$inference_available)
  }
  associations <- predictions <- list()
  for(scope in c("FULL","P2"))for(co in c("loglinear","extreme_only"))for(o in c("trust","signed_error"))for(predictor in c("advice_bias","advice_dprime"))for(common in c(FALSE,TRUE)) {
    h <- if(scope=="FULL")"H7" else "S7"
    d <- filter(advice,phase==scope,correction==co) %>% select(participant_id,group,advice_bias,dprime) %>%
      left_join(select(ratings,participant_id,trust,signed_error),by="participant_id") %>%
      mutate(score=if(predictor=="advice_bias")advice_bias else dprime,centred_score=score-mean(score))
    check(!anyNA(d),"Missing rating/SDT association cell")
    analysis <- if(common)"common_slope_sensitivity" else "primary"
    name <- paste0(paste(h,o,predictor,co,sep="_"),if(common)"_common" else "")
    formula <- reformulate(if(common)c("group","centred_score") else "group * centred_score",response=o)
    z <- dpr_hc3_fit(name,formula,d,out)
    diagnostics[[name]] <- z$diagnostics;model_index[[name]] <- tibble(model=name,path=paste(sample_name,"models",paste0(name,".rds"),sep="/"),formula=paste(deparse(formula),collapse=""))
    em <- emtrends(z$model,if(common)~1 else ~group,var="centred_score",vcov.=z$vcov,df=df.residual(z$model))
    add(em,dpr_association_specs(as.data.frame(em),predictor,scope,common),h,o,name,co,ok=z$diagnostics$inference_available,analysis=analysis)
    associations[[name]] <- mutate(d,hypothesis=h,outcome=o,predictor,correction=co,value=.data[[o]],model=name,analysis=analysis)
    grid <- bind_rows(lapply(GROUPS,function(g)tibble(group=factor(g,GROUPS),
      score=seq(min(d$score[d$group==g]),max(d$score[d$group==g]),length.out=60)))) %>% mutate(centred_score=score-mean(d$score))
    X <- model.matrix(delete.response(terms(z$model)),grid);pred <- drop(X%*%coef(z$model));se <- sqrt(rowSums((X%*%z$vcov)*X))
    predictions[[name]] <- mutate(grid,hypothesis=h,outcome=o,predictor,correction=co,model=name,analysis=analysis,estimate=pred,
      lower=if(z$diagnostics$inference_available)pred-qt(.975,df.residual(z$model))*se else NA_real_,
      upper=if(z$diagnostics$inference_available)pred+qt(.975,df.residual(z$model))*se else NA_real_)
  }
  results <- bind_rows(tables) %>% mutate(sample=sample_name) %>%
    group_by(sample,hypothesis,outcome,correction,analysis) %>% mutate(family_size=n(),p_holm=p.adjust(p_raw,"holm",n=n())) %>% ungroup() %>%
    mutate(ratio=ifelse(outcome=="correct_rt",exp(estimate),NA_real_),ratio_lower=ifelse(outcome=="correct_rt",exp(lower),NA_real_),
      ratio_upper=ifelse(outcome=="correct_rt",exp(upper),NA_real_))
  combined <- filter(results,hypothesis %in% c("H3","H4")) %>% group_by(sample,outcome,correction,analysis) %>%
    mutate(p_holm_joint_H3_H4=p.adjust(p_raw,"holm",n=9)) %>% ungroup() %>%
    select(sample,hypothesis,outcome,correction,analysis,contrast_id,p_holm_joint_H3_H4)
  results <- left_join(results,combined,by=c("sample","hypothesis","outcome","correction","analysis","contrast_id"))
  data <- list(performance=perf,task_scores=scores,advice_scores=advice,ratings=ratings,benchmark=benchmark,task_benchmark=taskbench,
    trajectories=bind_rows(trajectories),slopes=bind_rows(slopes),associations=bind_rows(associations),association_predictions=bind_rows(predictions))
  for(name in names(data))write_csv(data[[name]],file.path(out,paste0(name,".csv")))
  list(data=data,results=results,estimates=means,audit=audit,diagnostics=bind_rows(diagnostics)%>%mutate(sample=sample_name),
    model_index=bind_rows(model_index)%>%mutate(sample=sample_name))
}
dpr_methods <- function()c(
  "Revised H1-H7 framework agreed 17 October 2026 after inspecting earlier results; retrospective, not preregistered.",
  "Primary: QC-retained cohort. Sensitivity: all participants. Calibration, near-chance and anticipatory rules only; manual target deviation remains diagnostic.",
  "H1: aided phases versus manual pre; each group and CAL90-minus-CAL65 changes. Nine tests each for accuracy, task d-prime and correct RT.",
  "H2: human-minus-aid performance in each phase and between groups. Nine tests each for all-trial accuracy and matched-human-answered task d-prime.",
  "H3: P2-P1 drop. Three tests per advice/task bias/discrimination outcome. H4: P3-P2 recovery and P3-P1 residual difference, six tests per outcome.",
  "H5: within-phase end-start changes in each group and group differences. Accuracy: nine tests. Agreement: 18 tests across correct/incorrect advice together.",
  "H6: CAL90-CAL65 trust, perceived reliability and signed calibration error; signed error also versus zero in both groups. H7: CAL65 slope, CAL90 slope and their difference for each SDT predictor; six-test Holm family per rating outcome, 12 H7 tests and 125 primary tests per cohort.",
  "H7 uses whole-block pooled advice-correctness confusion counts before SDT correction/transformation, not averages of phase SDT. P2 associations are supporting S7.",
  "Repeated outcomes: participant-random-intercept mixed models. Accuracy/agreement: logistic; correct positive finite RT: linear model of log RT; SDT/benchmark differences: linear mixed models.",
  "Identical historical trial-level fits may be reused only after exact formula, model-frame and source/artifact checks. All revised contrast families are recomputed.",
  "Single final ratings: participant-level regression with HC3 covariance and residual-df t inference. H7 and supporting S7 use separate rating ~ group * centred SDT score models, allowing group-specific slopes and a direct slope-difference test. These are between-participant associations, not causal effects.",
  "Common-slope rating ~ group + centred SDT score fits are retained as common_slope_sensitivity, with separate two-test Holm families per rating and scope. The H7/S7 interaction update was requested after viewing the common-slope plots; it is retrospective.",
  "All tests two-sided. Holm separately within hypothesis/outcome/sample/correction/method. H3/H4 also export a joint nine-test adjustment per outcome. No study-wide error-control claim.",
  "Primary CIs are ordinary pointwise model-based 95% intervals. Model probabilities condition on zero participant random effect; they are not population-marginal probabilities.",
  "Accuracy/agreement contrasts use response-scale percentage points. RT contrasts use log ratios and are displayed as ratios; between-group moderation is a ratio of ratios.",
  "Participant sensitivities use equally weighted individual scores, paired changes within groups and Welch comparisons between groups. H5 linear-probability slopes differ from nonlinear model endpoint estimands.",
  "Loglinear SDT correction (k+0.5)/(n+1) primary; extreme-only correction sensitivity. Advice -c is acceptance; task -c is BLACK preference; advice d-prime is selective following; task d-prime is final stimulus discrimination.",
  "SDT includes answered trials regardless of RT; timeouts excluded. Accuracy includes timeouts as incorrect. The aid accuracy benchmark assumes timely advice on every trial; task benchmark uses precisely human-answered trials.",
  "Convergence, singularity, rank, gradient, residual and conditional cell diagnostics are recorded. Unavailable primary inference remains unavailable; sensitivities never silently replace it.",
  "Fixed phase order includes time, practice and fatigue; groups differ in calibrated visual difficulty. Shared responses are not independent evidence. Nonsignificance does not establish equivalence or full recovery.",
  "S1 full-sequence context; S2 manual-post minus manual-pre (three tests per outcome); S3 completion/answered accuracy; S4 correction sensitivity; S5 model/participant sensitivity; S6 cohort sensitivity; S7 P2 rating associations.",
  "Historical figures retain their original tests and correction families. Their labels are not revised H1-H7 labels.")
