# Independent revised-model, source-count, contract and rendered-artifact checks.
# Rscript tests/validate_dynamic_reliability_primary.R [analysis_dir] [presentation_dir] [--analysis-only]
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr);library(lme4);library(lmerTest)})
script <- normalizePath(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]));root <- dirname(dirname(script))
args <- commandArgs(trailingOnly=TRUE);analysis_only <- "--analysis-only" %in% args;args <- args[args!="--analysis-only"];stopifnot(length(args)<=2)
base <- if(length(args))normalizePath(args[1]) else file.path(root,"analysis_outputs/semester2_2026_behavioural")
out <- if(length(args)>1)normalizePath(args[2]) else file.path(root,"plots/semester2_2026_behavioural/presentation")
primary <- file.path(base,"primary")
read <- function(p)read_csv(p,show_col_types=FALSE)
sha <- function(p)digest::digest(file=p,algo="sha256")
near <- function(a,b,tol=1e-8) {
  a <- unname(a);b <- unname(b)
  stopifnot(length(a)==length(b),length(a)>0,identical(is.na(a),is.na(b)))
  valid <- !is.na(a);a <- a[valid];b <- b[valid]
  stopifnot(identical(is.infinite(a),is.infinite(b)),all(a[is.infinite(a)]==b[is.infinite(b)]))
  z <- is.finite(a);stopifnot(all(abs(a[z]-b[z])<=tol*pmax(1,abs(b[z]))))
}
source(file.path(root,"plot_dynamic_reliability_hypotheses.R"))
dpr_verify_analysis(base)
bundle <- readRDS(file.path(primary,"bundle.rds"));audits <- readRDS(file.path(primary,"contrast_audit.rds"))
results <- read(file.path(primary,"contrasts.csv"));registry <- read(file.path(root,"config/hypothesis_figures.csv"))
inputs <- read(file.path(primary,"input_checksums.csv"))
raw <- read(inputs$path[basename(inputs$path)=="data_virus_all.csv"][1])
qc <- read(inputs$path[basename(inputs$path)=="participant_summary.csv"][1])
excluded <- qc$participant_id[qc$calibration_target=="flag" | qc$near_chance_or_worse=="flag" | qc$anticipatory=="flag"]
stopifnot(setequal(bundle$membership$participant_id,unique(raw$participant_id)),
  setequal(bundle$membership$participant_id[!bundle$membership$in_primary],excluded),
  all(qc$participant_id[qc$manual_target=="flag" & !qc$exclude_by_rules] %in% bundle$membership$participant_id[bundle$membership$in_primary]))
required <- registry %>% filter(role=="Core") %>% separate_rows(contrast_ids,sep="\\|") %>% rename(contrast_id=contrast_ids)
stopifnot(nrow(required)==206,!anyDuplicated(required[c("hypothesis","outcome","contrast_id")]),
  n_distinct(registry$filename)==22,n_distinct(registry$filename[registry$role=="Core"])==12)
trial <- filter(raw,block!="CALIBRATION") %>% mutate(group=calibration_target_group,
  stage=case_when(block=="MANUAL" & manual_segment=="PRE_AUTOMATION"~"Manual pre",
    block=="MANUAL"~"Manual post",reliability_phase_idx==1~"P1",reliability_phase_idx==2~"P2",TRUE~"P3"),
  phase=ifelse(block=="AUTOMATION",stage,NA_character_),answered=response!="TIMEOUT",
  accuracy=as.numeric(answered & response==stimulus),agreement=ifelse(block=="AUTOMATION" & answered,as.numeric(response==aid_label),NA_real_),
  advice=ifelse(block=="AUTOMATION",ifelse(aid_label==stimulus,"Correct","Incorrect"),NA_character_),
  progress=(trial_in_reliability_phase-1)/399,x=(trial_in_reliability_phase-200.5)/399,
  valid_correct_rt=accuracy==1 & is.finite(rt_s) & rt_s>0,
  log_rt=ifelse(accuracy==1 & is.finite(rt_s) & rt_s>0,log(pmax(rt_s,.Machine$double.xmin)),NA_real_))
rate <- function(k,n,co)if(co=="loglinear")(k+.5)/(n+1) else ifelse(k==0,.5/n,ifelse(k==n,1-.5/n,k/n))
models <- new.env(parent=emptyenv())
getmodel <- function(sample,name) {
  key <- paste(sample,name,sep="/")
  if(!exists(key,models,inherits=FALSE))assign(key,readRDS(file.path(primary,sample,"models",paste0(name,".rds"))),models)
  get(key,models,inherits=FALSE)
}
hc3 <- function(m) {
  X <- model.matrix(m);bread <- solve(crossprod(X));omega <- residuals(m)^2/(1-hatvalues(m))^2
  bread%*%crossprod(X,X*omega)%*%bread
}
for(sample_name in c("primary","all_participants")) {
  ids <- bundle$membership$participant_id[if(sample_name=="primary")bundle$membership$in_primary else TRUE]
  tr <- filter(trial,participant_id %in% ids);d <- bundle$data[[sample_name]]
  for(name in c("performance","task_scores","advice_scores","ratings","benchmark","task_benchmark"))
    stopifnot(setequal(d[[name]]$participant_id,ids))
  index <- filter(bundle$model_index,sample==sample_name)
  for(name in index$model) {
    m <- getmodel(sample_name,name)
    if(inherits(m,"merMod"))stopifnot(setequal(as.integer(as.character(model.frame(m)$subject)),ids))
    else stopifnot(nobs(m)==length(ids))
  }
  # Check every saved fitted mean/curve directly against the model, not emmeans.
  for(name in names(bundle$estimates[[sample_name]])) {
    z <- bundle$estimates[[sample_name]][[name]];m <- getmodel(sample_name,name)
    B <- if(inherits(m,"merMod"))fixef(m) else coef(m)
    X <- model.matrix(delete.response(terms(m)),z,contrasts.arg=attr(model.matrix(m),"contrasts"))[,names(B),drop=FALSE]
    V <- if(inherits(m,"merMod"))as.matrix(vcov(m)) else hc3(m)
    predicted <- drop(X%*%B);J <- X
    if(inherits(m,"glmerMod")) {predicted <- plogis(predicted);J <- X*as.numeric(predicted*(1-predicted))}
    scale <- if(inherits(m,"glmerMod") || name=="benchmark_accuracy_benchmark_none")100 else 1
    degrees <- if(inherits(m,"glmerMod"))rep(Inf,nrow(z)) else if(inherits(m,"merMod"))
      vapply(seq_len(nrow(z)),function(i)lmerTest::contest1D(m,L=X[i,])$df,numeric(1)) else rep(df.residual(m),nrow(z))
    se <- sqrt(rowSums((J%*%V)*J));near(z$estimate,predicted*scale);near(z$df,degrees,tol=1e-6)
    near(z$lower,ifelse(z$inference_available,(predicted-qt(.975,degrees)*se)*scale,NA_real_),tol=1e-6)
    near(z$upper,ifelse(z$inference_available,(predicted+qt(.975,degrees)*se)*scale,NA_real_),tol=1e-6)
  }
  for(name in index$model[grepl("^(H7|S7)_",index$model)]) {
    m <- getmodel(sample_name,name);parts <- strsplit(name,"_",fixed=TRUE)[[1]]
    pred <- d$association_predictions %>% filter(model==name)
    common <- endsWith(name,"_common")
    stopifnot(("group:centred_score" %in% attr(terms(m),"term.labels"))==!common)
    X <- model.matrix(delete.response(terms(m)),pred,contrasts.arg=attr(model.matrix(m),"contrasts"))
    fitted <- drop(X%*%coef(m));se <- sqrt(rowSums((X%*%hc3(m))*X));near(pred$estimate,fitted)
    usable <- filter(bundle$diagnostics,sample==sample_name,model==name)$inference_available
    near(pred$lower,if(usable)fitted-qt(.975,df.residual(m))*se else rep(NA_real_,nrow(pred)))
    near(pred$upper,if(usable)fitted+qt(.975,df.residual(m))*se else rep(NA_real_,nrow(pred)))
  }
  # Reconstruct confusion counts directly from stimulus/response/advice semantics.
  for(i in seq_len(nrow(d$task_scores))) {
    r <- d$task_scores[i,];x <- filter(tr,participant_id==r$participant_id,stage==r$stage)
    if(r$agent!="aid_all")x <- filter(x,answered)
    decision <- if(r$agent=="participant")x$response else x$aid_label
    h <- sum(x$stimulus=="BLACK" & decision=="BLACK");miss <- sum(x$stimulus=="BLACK" & decision=="WHITE")
    fa <- sum(x$stimulus=="WHITE" & decision=="BLACK");cr <- sum(x$stimulus=="WHITE" & decision=="WHITE")
    near(c(r$hits,r$misses,r$false_alarms,r$correct_rejections),c(h,miss,fa,cr))
    zh <- qnorm(rate(h,h+miss,r$correction));zf <- qnorm(rate(fa,fa+cr,r$correction))
    near(r$dprime,zh-zf);near(r$criterion_c,-(zh+zf)/2);near(r$task_bias,(zh+zf)/2)
  }
  for(i in seq_len(nrow(d$advice_scores))) {
    r <- d$advice_scores[i,];x <- filter(tr,participant_id==r$participant_id,!is.na(phase),answered)
    if(r$phase!="FULL")x <- filter(x,phase==r$phase)
    good <- x$aid_label==x$stimulus
    k <- c(sum(x$response[good]==x$aid_label[good]),sum(x$response[!good]==x$aid_label[!good]))
    n <- c(sum(good),sum(!good));z <- qnorm(rate(k,n,r$correction))
    near(c(r$n_agree_Correct,r$n_agree_Incorrect,r$n_answered_Correct,r$n_answered_Incorrect),c(k,n))
    near(r$advice_bias,mean(z));near(r$dprime,z[1]-z[2])
  }
  perf <- tr %>% group_by(participant_id,stage) %>% summarise(answered_accuracy=mean(accuracy[answered]),
    correct_rt=mean(log(rt_s[accuracy==1 & is.finite(rt_s) & rt_s>0])),accuracy=mean(accuracy),completion=mean(answered),.groups="drop")
  pp <- left_join(d$performance,perf,by=c("participant_id","stage"),suffix=c("","_expected"))
  for(v in c("accuracy","completion","answered_accuracy","correct_rt"))near(pp[[v]],pp[[paste0(v,"_expected")]])
  bench <- filter(tr,!is.na(phase)) %>% group_by(participant_id,phase) %>% summarise(expected=mean(accuracy)-mean(aid_label==stimulus),.groups="drop")
  near(left_join(d$benchmark,bench,by=c("participant_id","phase"))$value,left_join(d$benchmark,bench,by=c("participant_id","phase"))$expected)
  for(i in seq_len(nrow(d$task_benchmark))) {
    r <- d$task_benchmark[i,];s <- filter(d$task_scores,participant_id==r$participant_id,phase==r$phase,correction==r$correction)
    near(r$value,s$dprime[s$agent=="participant"]-s$dprime[s$agent=="aid_matched"])
  }
  raw_manifest <- read(file.path(base,"input_manifest.csv")) %>% filter(participant_id %in% ids)
  q <- bind_rows(lapply(raw_manifest$path[raw_manifest$kind=="trust"],read))
  sliders <- bind_rows(lapply(raw_manifest$path[raw_manifest$kind=="sliders"],read))
  rr <- d$ratings %>% left_join(q %>% group_by(participant_id) %>% summarise(expected_trust=mean(response),.groups="drop"),by="participant_id") %>%
    left_join(filter(sliders,question_key=="perc_auto_correct") %>% select(participant_id,expected_perceived=response_percent),by="participant_id") %>%
    left_join(filter(tr,!is.na(phase)) %>% group_by(participant_id) %>% summarise(expected_realised=100*mean(aid_label==stimulus),.groups="drop"),by="participant_id")
  near(rr$trust,rr$expected_trust);near(rr$perceived,rr$expected_perceived);near(rr$signed_error,rr$expected_perceived-rr$expected_realised)
  for(i in seq_len(nrow(d$associations))) {
    r <- d$associations[i,];a <- filter(d$advice_scores,participant_id==r$participant_id,phase==if(r$hypothesis=="H7")"FULL" else "P2",correction==r$correction)
    near(r$score,if(r$predictor=="advice_bias")a$advice_bias else a$dprime)
  }
  # Independently reconstruct model contrasts from X, coefficients, covariance and weights.
  for(a in audits[[sample_name]]) {
    m <- getmodel(sample_name,a$model);r <- filter(results,sample==sample_name,analysis==a$analysis,
      hypothesis==a$hypothesis,outcome==a$outcome,correction==a$correction,model==a$model)
    r <- r[match(names(a$spec$weights),r$contrast_id),];stopifnot(!anyNA(r$contrast_id))
    curvature <- "estimand" %in% names(a$spec$metadata) && all(a$spec$metadata$estimand=="curvature")
    stopifnot(a$scale==if(a$outcome %in% c("accuracy","agreement","accuracy_benchmark") && !curvature)100 else 1)
    # Derive the intended weights independently from the registered contrast IDs.
    grid <- as.data.frame(a$em@grid)
    for(id in names(a$spec$weights)) {
      parts <- strsplit(id,"__",fixed=TRUE)[[1]]
      if(a$hypothesis %in% c("H7","S7")) {
        wanted <- if(a$analysis=="common_slope_sensitivity")1 else
          if(parts[2]=="CAL90 - CAL65")as.numeric(grid$group=="CAL90")-as.numeric(grid$group=="CAL65") else as.numeric(grid$group==parts[2])
      } else if(a$hypothesis=="H6") {
        wanted <- if(id=="group_difference")as.numeric(grid$group=="CAL90")-as.numeric(grid$group=="CAL65") else
          as.numeric(grid$group==sub("^zero_","",id))
      } else {
        g <- if(a$hypothesis=="H5")parts[3] else parts[2]
        one <- function(group_name) {
          if(a$hypothesis=="H5")return(as.numeric(grid$group==group_name & grid$phase==parts[2] &
            if(parts[1]=="All")TRUE else grid$advice==parts[1]) * {
              position <- if("x" %in% names(grid))grid$x+.5 else grid$progress
              if(parts[4]=="curvature")ifelse(position==.5,-4,2) else
                if(parts[4]=="middle_start")as.numeric(position==.5)-as.numeric(position==0) else
                if(parts[4]=="end_middle")as.numeric(position==1)-as.numeric(position==.5) else
                as.numeric(position==1)-as.numeric(position==0)
            })
          dim <- if(a$hypothesis %in% c("H1","S2"))"stage" else "phase"
          as.numeric(grid$group==group_name & grid[[dim]]==parts[3])-
            if(parts[4]=="zero")0 else as.numeric(grid$group==group_name & grid[[dim]]==parts[4])
        }
        wanted <- if(g=="CAL90 - CAL65")one("CAL90")-one("CAL65") else one(g)
      }
      near(a$spec$weights[[id]],wanted)
    }
    if(a$hypothesis %in% c("H7","S7")) {
      B <- coef(m);V <- hc3(m)
      if(a$analysis=="common_slope_sensitivity")X <- matrix(as.numeric(names(B)=="centred_score"),nrow=1) else {
        low <- data.frame(group=factor(grid$group,levels=levels(model.frame(m)$group)),centred_score=0)
        high <- transform(low,centred_score=1)
        X <- model.matrix(delete.response(terms(m)),high)-model.matrix(delete.response(terms(m)),low)
      }
      theta <- drop(X%*%B);covariance <- X%*%V%*%t(X);df <- df.residual(m)
    } else {
      grid <- as.data.frame(a$em@grid)
      X <- model.matrix(delete.response(terms(m)),grid,contrasts.arg=attr(model.matrix(m),"contrasts"))
      B <- if(inherits(m,"merMod"))fixef(m) else coef(m)
      X <- X[,names(B),drop=FALSE]
      V <- if(inherits(m,"merMod"))as.matrix(vcov(m)) else hc3(m)
      theta <- drop(X%*%B)
      if(inherits(m,"glmerMod")) {
        if(curvature)J <- X else {theta <- plogis(theta);J <- X*as.numeric(theta*(1-theta))}
        covariance <- J%*%V%*%t(J);df <- Inf
      } else {covariance <- X%*%V%*%t(X);df <- if(inherits(m,"merMod"))NA_real_ else df.residual(m)}
    }
    for(j in seq_len(nrow(r))) {
      w <- a$spec$weights[[j]];estimate <- sum(w*theta)*a$scale
      se <- sqrt(drop(t(w)%*%covariance%*%w))*a$scale
      degrees <- if(is.na(df))lmerTest::contest1D(m,L=drop(w%*%X))$df else df
      near(r$estimate[j],estimate);near(r$se[j],se);near(r$df[j],degrees,tol=1e-6)
      if(a$usable) {
        near(r$lower[j],estimate-qt(.975,degrees)*se,tol=1e-6);near(r$upper[j],estimate+qt(.975,degrees)*se,tol=1e-6)
        near(r$p_raw[j],2*pt(-abs(estimate/se),degrees),tol=1e-6)
      } else stopifnot(is.na(r$p_raw[j]),is.na(r$lower[j]),is.na(r$upper[j]))
    }
    # Participant contrasts: independently calculate one change per person, then t tests.
    rs <- filter(results,sample==sample_name,analysis==if(a$hypothesis=="H5")"linear_participant_sensitivity" else "participant_sensitivity",hypothesis==a$hypothesis,
      outcome==a$outcome,correction==a$correction,model==a$model)
    if(nrow(rs))for(i in seq_len(nrow(rs))) {
      z <- rs[i,];meta <- a$spec$metadata[match(z$contrast_id,a$spec$metadata$contrast_id),]
      if(a$hypothesis=="H5") {
        x <- filter(tr,!is.na(phase),phase==meta$phase)
        if(a$outcome=="agreement")x <- filter(x,answered,advice==meta$advice)
        if(a$outcome=="correct_rt")x <- filter(x,valid_correct_rt)
        response <- if(a$outcome=="correct_rt")"log_rt" else a$outcome
        values <- x %>% group_by(participant_id,group) %>%
          summarise(value=if(n_distinct(progress)>=2)coef(lm(.data[[response]]~progress))[2] else NA_real_,.groups="drop") %>% filter(is.finite(value))
      } else {
        src <- if(a$hypothesis=="H2") {if(a$outcome=="accuracy_benchmark")d$benchmark else filter(d$task_benchmark,correction==a$correction)} else
          if(a$hypothesis %in% c("H3","H4")) {
            if(startsWith(a$outcome,"advice"))filter(d$advice_scores,phase!="FULL",correction==a$correction) else filter(d$task_scores,agent=="participant",!is.na(phase),correction==a$correction)
          } else if(a$outcome=="task_dprime")filter(d$task_scores,agent=="participant",correction==a$correction) else d$performance
        value <- if(a$hypothesis=="H2")"value" else if(a$outcome %in% c("task_dprime","advice_dprime"))"dprime" else a$outcome
        grid <- as.data.frame(a$em@grid);keys <- intersect(c("group","phase","stage"),names(grid))
        w <- a$spec$weights[[z$contrast_id]]
        active <- as_tibble(grid[keys]) %>% mutate(weight=w) %>% filter(weight!=0)
        values <- inner_join(src,active,by=keys) %>% group_by(participant_id,group) %>%
          summarise(value=sum(.data[[value]]*weight),.groups="drop")
        # Group contrast weights already carry the subtraction sign.
        if(z$group=="CAL90 - CAL65")values$value[values$group=="CAL65"] <- -values$value[values$group=="CAL65"]
      }
      if(z$group=="CAL90 - CAL65")tt <- t.test(values$value[values$group=="CAL90"],values$value[values$group=="CAL65"])
      else tt <- t.test(values$value[values$group==z$group])
      estimate <- if(length(tt$estimate)==2)unname(tt$estimate[1]-tt$estimate[2]) else unname(tt$estimate)
      near(z$estimate,estimate*a$scale);near(z$lower,tt$conf.int[1]*a$scale);near(z$upper,tt$conf.int[2]*a$scale);near(z$p_raw,tt$p.value)
    }
  }
  # Audit all H5 rows, scheduled x and RT eligibility against raw trials.
  h5 <- filter(results,sample==sample_name,hypothesis=="H5",analysis=="primary")
  stopifnot(setequal(h5$estimand,c("middle_start","end_middle","curvature")),nrow(h5)==108,all(h5$family_size==ifelse(h5$outcome=="agreement",54,27)),
    all(h5$inference_scale[h5$estimand=="curvature"]==ifelse(h5$outcome[h5$estimand=="curvature"]=="correct_rt","log_rt_quadratic","log_odds_quadratic")))
  for(o in c("accuracy","agreement","correct_rt")) {
    name <- paste0(o,"_quadratic_trend");m <- getmodel(sample_name,name)
    source <- filter(tr,!is.na(phase))
    if(o=="agreement")source <- filter(source,answered)
    if(o=="correct_rt")source <- filter(source,valid_correct_rt)
    mf <- model.frame(m)
    stopifnot(nrow(mf)==nrow(source),any(grepl("I(x^2)",attr(terms(m),"term.labels"),fixed=TRUE)))
    near(mf$x,source$x)
    near(model.response(mf),source[[if(o=="correct_rt")"log_rt" else o]])
    bins <- source %>% mutate(bin=ceiling(trial_in_reliability_phase/100)) %>%
      group_by(across(all_of(c("participant_id","group","phase",if(o=="agreement")"advice","bin")))) %>%
      summarise(expected_value=mean(.data[[if(o=="correct_rt")"log_rt" else o]]),expected_n=n(),expected_progress=mean(progress),.groups="drop")
    actual <- filter(d$trajectories,outcome==o)
    joined_bins <- inner_join(actual,bins,by=c("participant_id","group","phase",if(o=="agreement")"advice","bin"))
    stopifnot(nrow(joined_bins)==nrow(bins),nrow(actual)==nrow(bins))
    near(joined_bins$value,joined_bins$expected_value);near(joined_bins$n,joined_bins$expected_n)
    near(joined_bins$progress,joined_bins$expected_progress)
    # Finite differences independently verify the exact polynomial derivatives,
    # response-scale Jacobians and their covariance-based intervals.
    shape <- filter(d$shape_summaries,outcome==o);B <- fixef(m);V <- as.matrix(vcov(m))
    design <- function(g)model.matrix(delete.response(terms(m)),g,
      contrasts.arg=attr(model.matrix(m),"contrasts"))[,names(B),drop=FALSE]
    X <- design(shape);h <- 1e-5
    D <- (design(transform(shape,x=x+h))-design(transform(shape,x=x-h)))/(2*h)
    eta <- drop(X%*%B);slope <- drop(D%*%B)
    near(shape$model_estimate,slope,tol=1e-7)
    response_slope <- function(b) {
      e <- drop(X%*%b);v <- drop(D%*%b)
      if(o=="correct_rt")exp(e)*v else 100*plogis(e)*(1-plogis(e))*v
    }
    J <- sapply(seq_along(B),function(j){hi <- lo <- B;hi[j]<-hi[j]+h;lo[j]<-lo[j]-h;(response_slope(hi)-response_slope(lo))/(2*h)})
    near(shape$response_estimate,response_slope(B),tol=1e-7)
    for(kind in c("model","response")) {
      M <- if(kind=="model")D else J;se <- sqrt(rowSums((M%*%V)*M))
      df <- if(o!="correct_rt")rep(Inf,nrow(M)) else vapply(seq_len(nrow(M)),function(i)lmerTest::contest1D(m,L=M[i,])$df,numeric(1))
      near(shape[[paste0(kind,"_se")]],se,tol=1e-6)
      near(shape[[paste0(kind,"_df")]],df,tol=1e-5)
      value <- shape[[paste0(kind,"_estimate")]]
      near(shape[[paste0(kind,"_lower")]],ifelse(shape$inference_available,value-qt(.975,df)*se,NA_real_),tol=1e-6)
      near(shape[[paste0(kind,"_upper")]],ifelse(shape$inference_available,value+qt(.975,df)*se,NA_real_),tol=1e-6)
    }
    Q <- (design(transform(shape,x=.5))-2*design(transform(shape,x=0))+design(transform(shape,x=-.5)))/.5
    curvature <- drop(Q%*%B)
    middle_slope <- drop((design(transform(shape,x=.5))-design(transform(shape,x=-.5)))%*%B)
    turn <- ifelse(abs(curvature)>1e-12,.5-middle_slope/(2*curvature),NA_real_)
    near(shape$quadratic,curvature);near(shape$turning_progress,turn)
    stopifnot(identical(unname(shape$reversal_within_phase),unname(!is.na(turn) & turn>0 & turn<1)))
  }
  rp <- filter(results,sample==sample_name,analysis=="primary",correction %in% c("none","loglinear"),grepl("^H",hypothesis))
  stopifnot(nrow(rp)==206,nrow(inner_join(required,rp,by=c("hypothesis","outcome","model","contrast_id")))==206)
  association_tests <- filter(results,sample==sample_name,hypothesis %in% c("H7","S7"))
  stopifnot(all(association_tests$family_size[association_tests$analysis=="primary"]==6),
    all(association_tests$family_size[association_tests$analysis=="common_slope_sensitivity"]==2),
    nrow(filter(association_tests,analysis=="primary"))==48,
    nrow(filter(association_tests,analysis=="common_slope_sensitivity"))==16)
}
common <- read_csv(file.path(primary,"common_slope_contrasts.csv"),col_types=spec(results),show_col_types=FALSE)
stopifnot(isTRUE(all.equal(common,filter(results,analysis=="common_slope_sensitivity"),check.attributes=FALSE)))
for(d in split(results,interaction(results$sample,results$hypothesis,results$outcome,results$correction,results$analysis,drop=TRUE))) {
  near(d$p_holm,p.adjust(d$p_raw,"holm",n=nrow(d)));stopifnot(all(d$family_size==nrow(d)))
}
phase <- filter(results,hypothesis %in% c("H3","H4"))
for(d in split(phase,interaction(phase$sample,phase$outcome,phase$correction,phase$analysis,drop=TRUE))) {
  stopifnot(nrow(d)==9);near(d$p_holm_joint_H3_H4,p.adjust(d$p_raw,"holm",n=9))
}
rt <- filter(results,outcome=="correct_rt",estimand!="curvature")
stopifnot(all(is.na(filter(results,estimand=="curvature")$ratio)))
near(rt$ratio,exp(rt$estimate));near(rt$ratio_lower,exp(rt$lower));near(rt$ratio_upper,exp(rt$upper))
# Stale-input rejection occurs before plotting or model fitting.
fixture <- tempfile("dpr_stale_");dir.create(file.path(fixture,"primary"),recursive=TRUE)
writeLines("fixture",file.path(fixture,"primary/COMPLETE.txt"))
write_csv(tibble(path=inputs$path[1],sha256=paste(rep("0",64),collapse="")),file.path(fixture,"primary/input_checksums.csv"))
write_csv(tibble(path=character(),sha256=character()),file.path(fixture,"primary/artifact_manifest.csv"))
failure <- tryCatch({dpr_verify_analysis(fixture);NULL},error=identity);unlink(fixture,recursive=TRUE)
stopifnot(inherits(failure,"error"),grepl("before plotting",conditionMessage(failure)))
cat("PASS: both cohorts; independent raw confusion counts, pooled SDT, performance, benchmarks and ratings; 206 primary comparisons/cohort; direct coefficient/covariance/df/HC3 tests; participant sensitivities; Holm families, signs, RT ratios and stale-input rejection.\n")
if(analysis_only)quit(status=0)
catalog <- read(file.path(out,"figure_catalog.csv"));aa <- read(file.path(out,"data/revised_annotations.csv"))
stopifnot(nrow(catalog)==44,sum(catalog$role=="Core")==24,!anyDuplicated(catalog$filename))
for(sample_name in c("primary","all_participants")) {
  cat <- filter(catalog,sample==sample_name)
  stopifnot(identical(sub("^sensitivity_","",cat$filename),unique(registry$filename)),
    all(cat$n_participants==if(sample_name=="primary")sum(bundle$membership$in_primary) else nrow(bundle$membership)))
  an <- filter(aa,sample==sample_name,grepl("^H",hypothesis))
  stopifnot(nrow(an)==206,!anyDuplicated(an[c("hypothesis","outcome","contrast_id")]))
  supported <- registry %>% filter(annotation=="supporting_tests") %>% separate_rows(contrast_ids,sep="\\|") %>% rename(contrast_id=contrast_ids)
  actual <- filter(aa,sample==sample_name,hypothesis %in% c("S2","S7"))
  stopifnot(nrow(supported)==21,nrow(actual)==21,
    nrow(inner_join(supported,actual,by=c("hypothesis","outcome","model","contrast_id")))==21)
}
joined <- inner_join(aa,results,by=c("sample","hypothesis","outcome","model","correction","analysis","contrast_id"),suffix=c("","_source"))
stopifnot(nrow(joined)==nrow(aa))
for(v in c("estimate","lower","upper","p_raw","p_holm","family_size"))near(joined[[v]],joined[[paste0(v,"_source")]])
labels <- function(p)ifelse(is.na(p),"unavailable",ifelse(p<.001,"***",ifelse(p<.01,"**",ifelse(p<.05,"*","ns"))))
stopifnot(all(aa$symbol==labels(aa$p_holm)))
for(stem in catalog$filename[catalog$role!="Historical"]) {
  stopifnot(file.exists(file.path(out,"data",paste0(stem,"_data.csv"))),file.exists(file.path(out,"data",paste0(stem,"_participants.csv"))))
  path <- file.path(out,"data",paste0(stem,"_annotations.csv"))
  if(file.exists(path)) {
    a <- read_csv(path,col_types=spec(aa),show_col_types=FALSE);expected <- filter(aa,filename==stem)
    stopifnot(isTRUE(all.equal(a,expected,check.attributes=FALSE)))
  }
}
# Check exported plotted coordinates against the independently verified source tables.
rows_equal <- function(actual,expected,keys,values) {
  for(k in keys)if(is.numeric(actual[[k]]) && is.numeric(expected[[k]])) {
    actual[[k]] <- signif(actual[[k]],12);expected[[k]] <- signif(expected[[k]],12)
  }
  stopifnot(nrow(actual)==nrow(expected),!anyDuplicated(actual[keys]),!anyDuplicated(expected[keys]))
  z <- inner_join(actual,select(expected,all_of(c(keys,values))),by=keys,suffix=c("","_source"))
  stopifnot(nrow(z)==nrow(actual))
  for(v in values)near(z[[v]],z[[paste0(v,"_source")]])
}
for(i in which(catalog$role!="Historical")) {
  row <- catalog[i,];sample_name <- row$sample;d <- bundle$data[[sample_name]];means <- bundle$estimates[[sample_name]]
  plotted <- read(file.path(out,"data",paste0(row$filename,"_data.csv")))
  points <- read(file.path(out,"data",paste0(row$filename,"_participants.csv")))
  for(o in unique(points$outcome)) {
    raw <- filter(points,outcome==o);fit <- filter(plotted,outcome==o)
    if(row$order %in% c(1:3,13)) {
      t <- filter(results,sample==sample_name,hypothesis==row$hypothesis,outcome==o,analysis=="primary",
        correction %in% c("none","loglinear"),group %in% c("CAL65","CAL90"))
      rows_equal(fit,t,c("group","phase"),c("estimate","lower","upper"))
      if(row$order==3)src <- if(o=="accuracy_benchmark")mutate(d$benchmark,value=100*value) else filter(d$task_benchmark,correction=="loglinear") else {
        src <- if(o=="task_dprime")filter(d$task_scores,agent=="participant",correction=="loglinear") %>% mutate(value=dprime) else mutate(d$performance,value=.data[[o]])
        baseline <- filter(src,stage=="Manual pre") %>% select(participant_id,baseline=value)
        src <- src %>% filter(stage %in% if(row$order==13)"Manual post" else c("P1","P2","P3")) %>%
          left_join(baseline,by="participant_id") %>% mutate(phase=stage,value=(value-baseline)*if(o=="accuracy")100 else 1)
      }
      rows_equal(raw,src,c("participant_id","group","phase"),"value")
    } else if(row$order %in% 4:7) {
      src <- if(startsWith(o,"advice"))d$advice_scores else filter(d$task_scores,agent=="participant")
      src <- src %>% filter(correction=="loglinear",phase %in% if(row$order<6)c("P1","P2") else c("P1","P2","P3")) %>%
        mutate(value=if(endsWith(o,"dprime"))dprime else .data[[o]])
      rows_equal(raw,src,c("participant_id","group","phase"),"value")
      expected <- means[[paste0("aided_",o,"_loglinear")]] %>% filter(phase %in% unique(raw$phase))
      rows_equal(fit,expected,c("group","phase"),c("estimate","lower","upper"))
    } else if(row$order %in% c(8,9,9.5)) {
      src <- filter(d$trajectories,outcome==o) %>% mutate(value=if(o=="correct_rt")exp(value) else 100*value,advice=if(o!="agreement")"All" else as.character(advice))
      expected <- means[[paste0(o,"_quadratic_trend")]] %>% mutate(advice=if(o!="agreement")"All" else as.character(advice))
      if(o=="correct_rt")for(v in c("estimate","lower","upper"))expected[[v]] <- exp(expected[[v]])
      rows_equal(raw,src,c("participant_id","group","phase","advice","bin"),c("value","progress","n"))
      rows_equal(fit,expected,c("group","phase","advice","progress"),c("estimate","lower","upper"))
    } else if(row$order==10) {
      rows_equal(raw,mutate(d$ratings,value=.data[[o]]),c("participant_id","group"),"value")
      rows_equal(fit,means[[paste0("ratings_",o)]],"group",c("estimate","lower","upper"))
    } else if(row$order %in% c(11,18)) {
      key <- strsplit(o,"__",fixed=TRUE)[[1]]
      src <- filter(d$associations,hypothesis==row$hypothesis,outcome==key[1],predictor==key[2],correction=="loglinear",analysis=="primary")
      expected <- filter(d$association_predictions,hypothesis==row$hypothesis,outcome==key[1],predictor==key[2],correction=="loglinear",analysis=="primary")
      rows_equal(raw,src,c("participant_id","group"),c("score","value"))
      rows_equal(fit,expected,c("group","score"),c("estimate","lower","upper"))
    } else if(row$order %in% c(12,14)) {
      src <- if(o=="task_dprime")filter(d$task_scores,agent=="participant",correction=="loglinear") %>% mutate(value=dprime) else mutate(d$performance,value=.data[[o]])
      if(o=="correct_rt")src$value <- exp(src$value)
      if(o %in% c("accuracy","completion","answered_accuracy"))src$value <- src$value*100
      rows_equal(raw,src,c("participant_id","group","stage"),"value")
      if(row$order==12) {
        expected <- means[[paste("performance",o,if(o=="task_dprime")"loglinear" else "none",sep="_")]]
        if(o=="correct_rt")for(v in c("estimate","lower","upper"))expected[[v]] <- exp(expected[[v]])
      } else expected <- src %>% group_by(group,stage) %>% summarise(estimate=mean(value),se=sd(value)/sqrt(n()),n=n(),.groups="drop") %>%
        mutate(lower=estimate-qt(.975,n-1)*se,upper=estimate+qt(.975,n-1)*se)
      rows_equal(fit,expected,c("group","stage"),c("estimate","lower","upper"))
    } else if(row$order %in% 15:17) {
      x <- filter(results,sample==if(row$order==17)"primary" else sample_name,analysis=="primary",grepl("^H",hypothesis),
        correction %in% if(row$order==15)"loglinear" else c("none","loglinear"))
      y <- filter(results,sample==if(row$order==17)"all_participants" else sample_name,
        analysis==if(row$order==16)"participant_sensitivity" else "primary",
        correction %in% if(row$order==15)"extreme_only" else c("none","loglinear"))
      keys <- c("hypothesis","outcome","contrast_id")
      expected <- inner_join(select(x,all_of(keys),estimate_reference=estimate,p_reference=p_holm),
        select(y,all_of(keys),estimate_comparison=estimate,p_comparison=p_holm),by=keys)
      # Panel labels and the underlying analysed outcomes are both preserved.
      raw$outcome <- raw$source_outcome
      rows_equal(raw,expected,keys,c("estimate_reference","estimate_comparison","p_reference","p_comparison"))
      expected <- mutate(expected,decision=case_when(is.na(p_reference) | is.na(p_comparison)~"Unavailable",
        p_reference<.05 & p_comparison>=.05~"Primary only",p_reference>=.05 & p_comparison<.05~"Comparison only",TRUE~"Same decision"))
      rows_equal(fit,count(expected,hypothesis,decision),c("hypothesis","decision"),"n")
    }
  }
}
historical <- read(file.path(out,"data/historical_manifest.csv"))
stopifnot(identical(vapply(file.path(out,historical$path),sha,character(1),USE.NAMES=FALSE),historical$sha256))
pi <- read(file.path(out,"data/input_checksums.csv"));stopifnot(identical(vapply(pi$path,sha,character(1),USE.NAMES=FALSE),pi$sha256))
for(path in list.files(primary,pattern="\\.(csv|txt)$",recursive=TRUE,full.names=TRUE))
  stopifnot(sha(path)==sha(file.path(out,"data/analysis",substring(path,nchar(primary)+2))))
for(stem in catalog$filename) {
  con <- file(file.path(out,paste0(stem,".png")),"rb");seek(con,16)
  size <- readBin(con,"integer",n=2,size=4,endian="big");close(con);stopifnot(identical(size,c(3600L,2025L)))
}
expected_pdfs <- c(paste0(catalog$filename,".pdf"),"core_hypotheses.pdf","all_hypotheses.pdf","sensitivity_hypotheses.pdf")
stopifnot(setequal(list.files(out,pattern="\\.pdf$"),expected_pdfs))
for(path in file.path(out,expected_pdfs)) {
  info <- system2("pdfinfo",shQuote(path),stdout=TRUE);pages <- as.integer(sub("^Pages:\\s+","",grep("^Pages:",info,value=TRUE)))
  want <- if(basename(path)=="core_hypotheses.pdf")12 else if(basename(path) %in% c("all_hypotheses.pdf","sensitivity_hypotheses.pdf"))22 else 1
  stopifnot(pages==want,any(grepl("^Page size: +864 x 486 pts",info)))
  if(grepl("H05[abc]_",basename(path))) {
    label <- gsub("[[:space:]]+"," ",paste(system2("pdftotext",c(shQuote(path),"-"),stdout=TRUE),collapse=" "))
    for(wanted in c("S-M","M-E","100-trial bins","middle minus start","end minus middle"))
      stopifnot(grepl(wanted,label,fixed=TRUE))
    stopifnot(!grepl("Early change",label,fixed=TRUE),!grepl("50-trial bins",label,fixed=TRUE))
  }
  bbox <- system2("pdftotext",c("-bbox",shQuote(path),"-"),stdout=TRUE);words <- bbox[grepl("<word xMin=",bbox)]
  coord <- function(name)as.numeric(sub(paste0('.*',name,'="([^"]+)".*'),'\\1',words))
  if(!length(words) || min(coord("xMin"))<0 || min(coord("yMin"))<0 || max(coord("xMax"))>864 || max(coord("yMax"))>486)
    stop("Text outside PDF page: ",basename(path),call.=FALSE)
}
html <- paste(readLines(file.path(out,"index.html")),collapse="\n")
links <- regmatches(html,gregexpr('(?:href|src)="[^"]+"',html,perl=TRUE))[[1]]
links <- sub('^[^=]+="','',sub('"$','',links))
for(link in links)if(startsWith(link,"#"))stopifnot(grepl(paste0('id="',substring(link,2),'"'),html,fixed=TRUE)) else
  stopifnot(file.exists(file.path(out,link)))
for(stem in catalog$filename)stopifnot(grepl(paste0(stem,".png"),html,fixed=TRUE))
if(file.exists(file.path(out,"data/artifact_manifest.csv"))) {
  m <- read(file.path(out,"data/artifact_manifest.csv"));paths <- file.path(out,m$path)
  stopifnot(identical(vapply(paths,sha,character(1),USE.NAMES=FALSE),m$sha256),all(file.info(paths)$size==m$bytes))
  actual <- list.files(out,recursive=TRUE,full.names=TRUE);actual <- actual[actual!=file.path(out,"data/artifact_manifest.csv")]
  stopifnot(setequal(substring(actual,nchar(out)+2),m$path))
}
cat("PASS: complete registry-to-annotation mapping; 44 PNGs, 47 PDFs; 12/22/22 bundle pages; historical bytes, source table copies, text bounds, HTML links and provenance.\n")
