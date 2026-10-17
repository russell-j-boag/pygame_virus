# Independent numerical, source-data and required-annotation checks.
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr);library(emmeans)})
args <- commandArgs(trailingOnly=TRUE)
script <- sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
root <- dirname(dirname(normalizePath(script)))
base <- if(length(args)>=1)args[1] else file.path(root,"analysis_outputs/semester2_2026_behavioural")
plots <- if(length(args)>=2)args[2] else file.path(root,"plots/semester2_2026_behavioural/presentation")
source(file.path(root,"R/participant_identity.R"))
check <- function(ok,msg)if(!isTRUE(ok))stop(msg,call.=FALSE)
close <- function(x,y,tol=1e-8) isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=tol,check.attributes=FALSE))
read <- function(file)read_csv(file,show_col_types=FALSE)
pdir <- file.path(base,"primary");b <- readRDS(file.path(pdir,"bundle.rds"));r <- read(file.path(pdir,"contrasts.csv"))
registry <- read(file.path(root,"config/hypothesis_figures.csv"))
hashes <- read(file.path(pdir,"input_checksums.csv"))
check(identical(unname(tools::md5sum(hashes$path)),hashes$md5),"Primary input hashes changed")
rawpath <- hashes$path[basename(hashes$path)=="data_virus_all.csv"]
raw_all <- read_identity_csv(rawpath)
original <- bind_rows(lapply(hashes$path[grepl("_b00_ALL\\.csv$",hashes$path)],read_identity_csv))
shared <- intersect(names(raw_all),names(original));identity_keys <- c("participant_id","run_timestamp","block_idx","trial")
xx <- raw_all %>% select(all_of(shared)) %>% arrange(across(all_of(identity_keys)))
yy <- original %>% select(all_of(shared)) %>% arrange(across(all_of(identity_keys)))
check(isTRUE(all.equal(as.data.frame(xx),as.data.frame(yy),check.attributes=FALSE)),"Combined data differ from original runs")
policy <- read(file.path(root,"config/participant_exclusions.csv"))
selection <- read(file.path(pdir,"cohort_selection.csv"))
excluded <- read(file.path(pdir,"exclusions.csv"))
source_cohort <- raw_all %>% distinct(participant_id,source_participant_id,run_timestamp)
check(nrow(selection)==nrow(source_cohort) && setequal(selection$participant_id,source_cohort$participant_id),"Selection must account for every source participant")
check(setequal(selection$participant_id[!selection$included],policy$participant_id) &&
  setequal(excluded$participant_id,policy$participant_id),"Applied exclusions differ from approved decisions")
key <- c("participant_id","source_participant_id","run_timestamp")
check(nrow(anti_join(policy,excluded,by=key))==0,"Exclusion decision applied to the wrong run")
check(all(excluded$reason==policy$reason[match(excluded$participant_id,policy$participant_id)]),"Exclusion reasons changed")
raw <- raw_all %>% filter(condition_deadline_code!="CAL_LP",!participant_id %in% policy$participant_id)
n_participants <- n_distinct(raw$participant_id)
check(nrow(raw)==1600*n_participants,"Unexpected trial count")
check(setequal(raw$participant_id,b$data$cohort$participant_id),"Cohort identity mismatch")
for(name in names(b$data))if("participant_id" %in% names(b$data[[name]]) && !name %in% c("cohort_selection","exclusions"))
  check(!any(b$data[[name]]$participant_id %in% policy$participant_id),paste("Excluded participant leaked into",name))
check(all(b$diagnostics$n_participants==n_participants),"Models were not all refitted on the included cohort")
check(all((raw %>% count(participant_id,condition_deadline_code))$n==400),"Incomplete study blocks")
check(setequal(b$data$cohort$participant_id[b$data$cohort$source_participant_id==83],c(83,84)),"Identity correction changed")
counts <- function(x,response,actor) x %>% group_by(participant_id,condition_deadline_code) %>% summarise(
  actor=actor,n_scored=n(),n_signal=sum(stimulus=="BLACK"),n_noise=sum(stimulus=="WHITE"),
  hits=sum(stimulus=="BLACK" & .data[[response]]=="BLACK"),false_alarms=sum(stimulus=="WHITE" & .data[[response]]=="BLACK"),.groups="drop")
answered <- filter(raw,response!="TIMEOUT");aided <- filter(raw,condition_deadline_code %in% c("A_HP","A_LP"))
tc <- bind_rows(counts(answered,"response","human"),counts(filter(aided,response!="TIMEOUT"),"aid_label","aid_matched"),counts(aided,"aid_label","aid_all"))
joined <- inner_join(tc,b$data$task_counts,by=c("participant_id","condition_deadline_code","actor"),suffix=c("","_saved"))
check(nrow(joined)==8*n_participants,"Task count coverage")
for(k in c("n_scored","n_signal","n_noise","hits","false_alarms"))check(close(joined[[k]],joined[[paste0(k,"_saved")]]),paste("Task counts",k))
for(co in c("loglinear","extreme_only")) {
  ss <- filter(b$data$task_scores,correction==co)
  rate <- function(k,n)if(co=="loglinear")(k+.5)/(n+1) else ifelse(k==0,.5/n,ifelse(k==n,1-.5/n,k/n))
  zh <- qnorm(rate(ss$hits,ss$n_signal));zf <- qnorm(rate(ss$false_alarms,ss$n_noise))
  check(close(ss$dprime,zh-zf) && close(ss$black_bias,(zh+zf)/2),paste("Task SDT formula",co))
}
ac <- aided %>% filter(response!="TIMEOUT") %>% mutate(pressure=time_pressure_condition,advice=ifelse(stimulus==aid_label,"Correct","Incorrect")) %>%
  group_by(participant_id,pressure,advice) %>% summarise(n_answered=n(),n_agree=sum(response==aid_label),.groups="drop")
acj <- inner_join(ac,b$data$advice_counts,by=c("participant_id","pressure","advice"),suffix=c("","_saved"))
check(nrow(acj)==4*n_participants && close(acj$n_answered,acj$n_answered_saved) && close(acj$n_agree,acj$n_agree_saved),"Advice SDT source counts")
for(co in c("loglinear","extreme_only")) {
  a <- filter(b$data$advice_scores,correction==co)
  rate <- function(k,n)if(co=="loglinear")(k+.5)/(n+1) else ifelse(k==0,.5/n,ifelse(k==n,1-.5/n,k/n))
  zh <- qnorm(rate(a$n_agree_Correct,a$n_answered_Correct));zf <- qnorm(rate(a$n_agree_Incorrect,a$n_answered_Incorrect))
  check(close(a$dprime,zh-zf) && close(a$automation_bias,(zh+zf)/2),"Advice score formula/sign")
}
pp <- raw %>% group_by(participant_id,pressure=time_pressure_condition,mode=ifelse(grepl("^A_",condition_deadline_code),"Automation","Manual")) %>%
  summarise(accuracy=mean(response==stimulus),answered_accuracy=mean(response[response!="TIMEOUT"]==stimulus[response!="TIMEOUT"]),
    completion=mean(response!="TIMEOUT"),correct_rt=mean(log(rt_s[response==stimulus & is.finite(rt_s) & rt_s>0])),.groups="drop")
pj <- inner_join(pp,b$data$performance,by=c("participant_id","pressure","mode"),suffix=c("","_saved"))
check(nrow(pj)==4*n_participants && close(pj$accuracy,pj$accuracy_saved) && close(pj$correct_rt,pj$correct_rt_saved),"Performance source summaries")
check(close(pj$answered_accuracy,pj$answered_accuracy_saved) && close(pj$completion,pj$completion_saved),"Completion and answered accuracy")
check(close(pj$accuracy,pj$completion*pj$answered_accuracy),"Accuracy equals completion times answered accuracy")
check(close(b$data$decomposition$total_gain,b$data$decomposition$completion_component+b$data$decomposition$choice_component),"Decomposition identity")
bench <- aided %>% group_by(participant_id,pressure=time_pressure_condition) %>% summarise(net_gain=mean(response==stimulus)-mean(aid_label==stimulus),.groups="drop")
bj <- inner_join(bench,b$data$benchmark,by=c("participant_id","pressure"),suffix=c("","_saved"))
check(close(bj$net_gain,bj$net_gain_saved),"All-trial benchmark")
qpaths <- hashes$path[grepl("_POSTBLOCK_ALL\\.csv$",hashes$path)]
q <- bind_rows(lapply(qpaths,read_identity_csv)) %>% group_by(participant_id,block_idx) %>% summarise(trust=mean(response),n=n(),.groups="drop")
qj <- inner_join(q,b$data$trust,by=c("participant_id","block_idx"),suffix=c("","_saved"))
check(nrow(qj)==2*n_participants && all(qj$n==6) && close(qj$trust,qj$trust_saved),"Trust source scores")
sp <- hashes$path[grepl("_POSTBLOCK_SLIDERS_ALL\\.csv$",hashes$path)]
sl <- bind_rows(lapply(sp,read_identity_csv)) %>% filter(question_key=="perc_auto_correct") %>% select(participant_id,block_idx,response_percent)
sj <- inner_join(sl,b$data$ratings,by=c("participant_id","block_idx"))
check(nrow(sj)==2*n_participants && close(sj$response_percent,sj$perceived) && close(sj$signed_error,sj$perceived-100*sj$aid_accuracy),"Perceived reliability source scores")
seq <- aided %>% arrange(participant_id,run_timestamp,block_idx,trial) %>% group_by(participant_id,run_timestamp,block_idx) %>%
  mutate(previous_trial=lag(trial),previous_response=lag(response)) %>% ungroup() %>% filter(response!="TIMEOUT",previous_response!="TIMEOUT",trial==previous_trial+1)
saved <- read(file.path(pdir,"sequence_eligibility.csv"));keys <- c("participant_id","run_timestamp","block_idx","trial")
check(nrow(seq)==nrow(saved) && nrow(anti_join(seq,saved,by=keys))==0,"History eligibility bridges timeout or block boundary")
check(all(abs((b$data$associations %>% group_by(participant_id,correction) %>% summarise(x=sum(trust_within),.groups="drop"))$x)<1e-12),"Within-person trust centering")
# Numeric reconstruction uses saved model covariance and explicit linear weights,
# not emmeans::contrast() or the production contrast constructor.
audit <- readRDS(file.path(pdir,"contrast_audit.rds"));total <- 0
for(key in names(audit)) {
  a <- audit[[key]];parts <- strsplit(key,"__",fixed=TRUE)[[1]]
  h <- parts[1];o <- parts[2];co <- parts[length(parts)-1];analysis <- parts[length(parts)]
  rr <- filter(r,hypothesis==h,outcome==o,correction==co,.data$analysis==.env$analysis,contrast_id %in% names(a$methods))
  rr <- rr[match(names(a$methods),rr$contrast_id),]
  grid <- a$em;mean <- as.numeric(grid@linfct %*% grid@bhat)
  cov <- as.matrix(vcov(grid));w <- do.call(rbind,a$methods)
  expected <- as.vector(w %*% mean)*a$scale;se <- sqrt(diag(w %*% cov %*% t(w)))*a$scale
  check(close(expected,rr$estimate) && close(se,rr$se),paste("Model contrast arithmetic",key))
  check(close(rr$lower,expected-qt(.975,rr$df)*se) && close(rr$upper,expected+qt(.975,rr$df)*se),paste("Intervals",key))
  if(a$usable)check(close(rr$p_raw,2*pt(-abs(expected/se),rr$df)),paste("P values",key)) else check(all(is.na(rr$p_raw)),"Invalid model has primary p")
  total <- total+nrow(rr)
}
families <- r %>% group_by(hypothesis,outcome,correction,analysis) %>% group_split()
for(f in families)check(close(f$p_holm,p.adjust(f$p_raw,"holm",n=nrow(f))) && all(f$family_size==nrow(f)),"Holm family mismatch")
# Independently recover group slopes as finite changes in an explicit reference
# grid, rather than reusing the production emtrends() contrasts. Check both SDT
# corrections, the clustered sensitivity, and the retained additive models.
h7 <- filter(r,hypothesis=="H7")
check(nrow(h7)==56 && all(h7$family_size[h7$analysis=="common_slope_sensitivity"]==2) &&
  all(h7$family_size[h7$analysis!="common_slope_sensitivity"]==6),"H7 family coverage")
common <- read_csv(file.path(pdir,"common_slope_contrasts.csv"),col_types=spec(r),show_col_types=FALSE)
check(isTRUE(all.equal(common,filter(r,analysis=="common_slope_sensitivity"),check.attributes=FALSE)),"Common-slope export mismatch")
for(co in c("loglinear","extreme_only"))for(o in c("advice_bias","advice_dprime"))for(kind in c("mixed_model","participant_cluster_sensitivity","common_slope_sensitivity")) {
  rr <- filter(h7,outcome==o,correction==co,analysis==kind)
  path <- file.path(pdir,"models",paste0(unique(rr$model),if(kind=="participant_cluster_sensitivity")"_sensitivity" else "",".rds"))
  model <- readRDS(path)
  is_common <- kind=="common_slope_sensitivity"
  at <- list(trust_within=c(0,1),trust_between=c(0,1),block_position=0)
  spec <- if(is_common)~pressure*reliability*trust_within*trust_between else ~pressure*pattern*trust_within*trust_between
  em <- if(kind=="participant_cluster_sensitivity")emmeans(model,spec,at=at,
    vcov.=sandwich::vcovCL(model,cluster=b$data$associations$participant_id[b$data$associations$correction==co],type="HC1"),df=n_participants-1) else
    emmeans(model,spec,at=at,lmer.df="satterthwaite")
  gd <- as.data.frame(em)
  vectors <- list()
  for(component in c("trust_within","trust_between")) {
    other <- setdiff(c("trust_within","trust_between"),component)
    if(is_common) {
      pick <- gd$pressure=="HP" & gd$reliability=="95%" & gd[[other]]==0
      vectors[[component]] <- em@linfct[pick & gd[[component]]==1,]-em@linfct[pick & gd[[component]]==0,]
    } else {
      for(pattern in c("HP95_LP65","HP65_LP95")) {
        slope <- function(pressure) {
          pick <- gd$pressure==pressure & gd$pattern==pattern & gd[[other]]==0
          em@linfct[pick & gd[[component]]==1,]-em@linfct[pick & gd[[component]]==0,]
        }
        check(close(slope("HP"),slope("LP")),"H7 slope must be specific to allocation group, not pressure")
        vectors[[paste(component,pattern,sep="_")]] <- slope("HP")
      }
      vectors[[paste0(component,"_group_difference")]] <- vectors[[paste0(component,"_HP95_LP65")]]-vectors[[paste0(component,"_HP65_LP95")]]
    }
  }
  check(setequal(names(vectors),rr$contrast_id),"H7 slope identities")
  rr <- rr[match(names(vectors),rr$contrast_id),]
  L <- do.call(rbind,vectors);estimate <- drop(L %*% em@bhat);se <- sqrt(diag(L %*% em@V %*% t(L)))
  # Cluster sensitivity uses participant-count df, overriding lm residual df.
  df <- if(kind=="participant_cluster_sensitivity")rep(n_participants-1,nrow(L)) else
    apply(L,1,function(w)em@dffun(w,em@dfargs))
  check(close(rr$estimate,estimate) && close(rr$se,se) && close(rr$df,df),
    paste("H7 group slope/difference estimate, covariance or df:",co,o,kind))
  if(!is_common)check(qr(L[c(1,2,4,5),])$rank==4,"H7 does not allow four independently varying group slopes")
}
pri <- filter(r,analysis=="mixed_model",correction %in% c("none","loglinear"))
check(nrow(pri)==105,"Expected 105 primary tests")
check(identical(as.integer(table(pri$hypothesis)),c(3L,31L,18L,10L,10L,14L,12L,7L)),"Hypothesis test counts")
check(!anyDuplicated(pri[c("hypothesis","outcome","contrast_id")]),"Duplicate primary tests")
contract <- registry %>% filter(role=="Core",!is.na(contrast_ids),contrast_ids!="") %>%
  separate_rows(contrast_ids,sep="\\|") %>% rename(contrast_id=contrast_ids) %>% select(filename,hypothesis,outcome,model,contrast_id)
check(nrow(contract)==105,"Registry must require all 105 tests exactly once")
ann <- read(file.path(plots,"data/annotation_coverage.csv"))
check(nrow(ann)==105 && !anyDuplicated(ann[c("filename","outcome","contrast_id")]),"Missing/duplicated annotations")
join_keys <- c("filename","hypothesis","outcome","model","contrast_id")
check(nrow(anti_join(contract,ann,by=join_keys))==0 && nrow(anti_join(ann,contract,by=join_keys))==0,"Required annotation contract mismatch")
aj <- inner_join(ann,pri,by=c("hypothesis","outcome","model","contrast_id"),suffix=c("","_source"))
for(k in c("estimate","lower","upper","p_raw","p_holm","family_size"))check(close(aj[[k]],aj[[paste0(k,"_source")]]),paste("Annotation does not match source",k))
for(stem in unique(contract$filename)) {
  a <- read(file.path(plots,"data",paste0(stem,"_annotations.csv")))
  check(nrow(a)==sum(ann$filename==stem),paste("Per-figure annotation export",stem))
  values <- read(file.path(plots,"data",paste0(stem,"_data.csv")))
  values$outcome <- sub("__trust_(within|between)$","",values$outcome)
  if(grepl("^H01",stem))for(ii in seq_len(nrow(values))) {
    v <- values[ii,];a <- audit[[paste(v$hypothesis,v$outcome,v$correction,"mixed_model",sep="__")]]
    gd <- as.data.frame(a$em);pressure <- sub("^manual_","",v$contrast_id)
    w <- .5*as.numeric(gd$mode=="Manual" & gd$pressure==pressure)
    expected <- as.numeric(w %*% a$em@linfct %*% a$em@bhat)*a$scale
    se <- sqrt(as.numeric(w %*% vcov(a$em) %*% w))*a$scale
    target <- c(expected,expected-qt(.975,v$df)*se,expected+qt(.975,v$df)*se)
    if(v$outcome=="correct_rt")target <- exp(target)
    check(close(c(v$estimate,v$lower,v$upper),target),paste("Manual model means",v$outcome,pressure))
  }
  if(!grepl("^H01",stem)) {
    compare <- values %>% filter(!is.na(contrast_id),!is.na(hypothesis)) %>%
      inner_join(pri,by=c("hypothesis","outcome","contrast_id"),suffix=c("","_source"))
    for(k in c("estimate","lower","upper"))check(close(compare[[k]],compare[[paste0(k,"_source")]]),paste("Plotted model value",stem,k))
    if(grepl("^H02b",stem))check(close(values$display_estimate,exp(values$estimate)) && close(values$display_lower,exp(values$lower)),"RT plotting scale")
  }
  # Means in H4-H6 are not themselves the tested contrasts. Reconstruct these
  # independently from their model reference grid and covariance as well.
  for(ii in which(values$hypothesis %in% c("H4","H5","H6") & grepl("^cell_",values$contrast_id))) {
    v <- values[ii,];key <- paste(v$hypothesis,v$outcome,v$correction,"mixed_model",sep="__")
    g <- audit[[key]]$em;gd <- as.data.frame(g);parts <- strsplit(v$contrast_id,"_",fixed=TRUE)[[1]]
    pressure <- substr(parts[2],1,2);reliability <- paste0(substr(parts[2],3,4),"%")
    w <- as.numeric(gd$pressure==pressure & gd$reliability==reliability)
    expected <- as.numeric(w %*% g@linfct %*% g@bhat)*audit[[key]]$scale
    se <- sqrt(as.numeric(w %*% vcov(g) %*% w))*audit[[key]]$scale
    check(close(v$estimate,expected) && close(v$lower,expected-qt(.975,v$df)*se) && close(v$upper,expected+qt(.975,v$df)*se),paste("Plotted model mean",stem,v$outcome,v$contrast_id))
  }
}
for(h in c("H4","H5"))check(all(table(filter(ann,hypothesis==h)$outcome)==5),paste(h,"must show five comparisons in EACH panel"))
check(sum(ann$hypothesis=="H7" & ann$annotation_type=="adjusted_slope")==8 &&
  sum(ann$hypothesis=="H7" & ann$annotation_type=="slope_difference")==4,"H7 group slopes and differences must all be annotated")
comparison <- read(file.path(plots,"data/model_participant_comparison.csv"))
check(nrow(comparison)==105 && !anyDuplicated(comparison[c("hypothesis","outcome","contrast_id")]),
  "Participant sensitivity comparison must cover exactly the primary contrasts")
# Reconstruct the scatter fits from an independent condition reference grid.
# In particular, a within-person band needs covariance of HP minus LP, rather
# than adding two marginal interval widths or treating raw rows as independent.
h7_stem <- unique(registry$filename[registry$hypothesis=="H7" & !is.na(registry$hypothesis)])
fits <- read(file.path(plots,"data",paste0(h7_stem,"_fits.csv")))
scatter <- read(file.path(plots,"data",paste0(h7_stem,"_participants.csv")))
check(nrow(fits)==480 && !anyDuplicated(fits[c("outcome","component","pattern","x")]),"H7 prediction coverage")
association_paths <- character()
for(o in c("advice_bias","advice_dprime")) {
  model_name <- paste0("H7_",o,"_loglinear")
  model_path <- file.path(pdir,"models",paste0(model_name,".rds"))
  association_paths <- c(association_paths,normalizePath(model_path))
  model <- readRDS(model_path)
  check(setequal(as.integer(as.character(model.frame(model)$subject)),raw$participant_id),"H7 fit uses another cohort")
  em <- emmeans(model,~pressure*pattern*trust_within*trust_between,
    at=list(trust_within=c(0,1),trust_between=c(0,1),block_position=0),lmer.df="satterthwaite")
  gd <- as.data.frame(em);beta <- lme4::fixef(model)
  a <- filter(b$data$associations,correction=="loglinear") %>%
    mutate(value=if(o=="advice_bias")automation_bias else dprime)
  paired <- a %>% select(participant_id,pattern,pressure,trust,value) %>%
    pivot_wider(names_from=pressure,values_from=c(trust,value))
  for(component in c("trust_within","trust_between")) {
    observed <- filter(scatter,outcome==paste(.env$o,.env$component,sep="__"))
    expected <- paired %>% mutate(x=if(component=="trust_within")trust_HP-trust_LP else (trust_HP+trust_LP)/2,
      y=if(component=="trust_within")value_HP-value_LP else (value_HP+value_LP)/2)
    joined <- inner_join(expected,observed,by=c("participant_id","pattern"),suffix=c("","_saved"))
    check(nrow(observed)==n_participants && nrow(joined)==n_participants &&
      close(joined$x,joined$x_saved) && close(joined$y,joined$y_saved),"H7 raw scatter values/cohort")
    for(pattern in unique(as.character(a$pattern))) {
      pred <- fits %>% filter(outcome==o,.data$component==.env$component,.data$pattern==.env$pattern) %>% arrange(x)
      obs <- filter(expected,.data$pattern==.env$pattern)
      check(nrow(pred)==60 && close(pred$x,seq(min(obs$x),max(obs$x),length.out=60)) &&
        all(pred$n_participants==nrow(obs)) && all(pred$model==model_name),"H7 fit range or allocation pattern")
      pos_hp <- mean(a$block_position[a$pattern==pattern & a$pressure=="HP"])
      pos_lp <- mean(a$block_position[a$pattern==pattern & a$pressure=="LP"])
      check(close(pred$block_position_HP,rep(pos_hp,60)) && close(pred$block_position_LP,rep(pos_lp,60)) &&
        close(pred$grand_mean_trust,rep(mean(a$trust),60)),"H7 reference covariates")
      baseline <- gd$pattern==pattern & gd$trust_within==0 & gd$trust_between==0
      hp <- em@linfct[gd$pressure=="HP" & baseline,]
      lp <- em@linfct[gd$pressure=="LP" & baseline,]
      other <- setdiff(c("trust_within","trust_between"),component)
      slope_vector <- em@linfct[gd$pressure=="HP" & gd$pattern==pattern & gd[[component]]==1 & gd[[other]]==0,]-hp
      L <- matrix(if(component=="trust_within")hp-lp else (hp+lp)/2,nrow=60,ncol=length(beta),byrow=TRUE,
        dimnames=list(NULL,names(beta)))
      L <- L + outer(if(component=="trust_within")pred$x else pred$x-mean(a$trust),slope_vector)
      L[,"block_position"] <- if(component=="trust_within")pos_hp-pos_lp else (pos_hp+pos_lp)/2
      estimate <- drop(L %*% beta);se <- sqrt(diag(L %*% em@V %*% t(L)))
      df <- apply(L,1,function(w)em@dffun(w,em@dfargs))
      check(close(pred$estimate,estimate) && close(pred$se,se) && close(pred$df,df),"H7 fitted line/covariance/df")
      if(all(pred$inference_available))check(close(pred$lower,estimate-qt(.975,df)*se) &&
        close(pred$upper,estimate+qt(.975,df)*se),"H7 pointwise confidence ribbons") else
        check(all(is.na(pred$lower) & is.na(pred$upper)),"Unavailable H7 inference has ribbons")
      slope <- filter(pri,hypothesis=="H7",outcome==.env$o,contrast_id==paste(component,pattern,sep="_"))$estimate
      check(close((tail(pred$estimate,1)-head(pred$estimate,1))/(tail(pred$x,1)-head(pred$x,1)),slope),"H7 line slope sign differs from primary test")
    }
  }
}
diag <- b$diagnostics
check(all(is.na(r$p_raw[r$analysis %in% c("mixed_model","common_slope_sensitivity") & r$model %in% diag$model[!diag$inference_available]])),"Unusable model leaked p-values")
ha <- readRDS(file.path(pdir,"historical_audit.rds"))
for(pair in c("agreement","selectivity")) {
  ht <- b$historical[[paste0(pair,"_tests")]];w <- do.call(rbind,ha[[pair]])
  expected <- as.numeric(w %*% ha$em@linfct %*% ha$em@bhat)*100
  se <- sqrt(diag(w %*% vcov(ha$em) %*% t(w)))*100
  check(close(ht$estimate,expected) && close(ht$se,se) && close(ht$p_holm,p.adjust(ht$p_raw,"holm")),"Historical measures not refitted correctly")
  hp <- b$historical[[if(pair=="agreement")"participants" else "selectivity"]]
  check(setequal(hp$participant_id,raw$participant_id),"Historical figure contains another cohort")
}
catalog <- read(file.path(plots,"figure_catalog.csv"))
check(nrow(catalog)==18 && sum(catalog$role=="Core")==10,"Figure count")
for(stem in catalog$filename)for(ext in c("pdf","png"))check(file.info(file.path(plots,paste0(stem,".",ext)))$size>1000,paste("Missing figure",stem,ext))
check(all(file.exists(file.path(plots,"data",paste0(catalog$filename,"_data.csv")))),"Missing plotted data export")
for(nm in c("core_hypotheses","all_hypotheses")) {
  info <- system2(Sys.which("pdfinfo"),shQuote(file.path(plots,paste0(nm,".pdf"))),stdout=TRUE)
  pages <- as.integer(sub("^Pages:\\s+","",grep("^Pages:",info,value=TRUE)))
  check(pages==if(nm=="core_hypotheses")10 else 18,"Combined PDF page count")
}
html <- paste(readLines(file.path(plots,"index.html")),collapse="\n")
check(grepl("Common-slope sensitivity comparison",html,fixed=TRUE),"H7 report omits the common-slope comparison")
links <- regmatches(html,gregexpr('(?:href|src)="[^"]+"',html,perl=TRUE))[[1]]
links <- sub('^[^=]+="','',sub('"$','',links)); links <- links[!grepl("^(#|https?:)",links)]
check(all(file.exists(file.path(plots,links))),"Broken report link")
ph <- read(file.path(plots,"data/input_checksums.csv"))
check(all(association_paths %in% ph$path),"H7 fitted models missing from plot provenance")
check(identical(unname(tools::md5sum(ph$path)),ph$md5),"Plot source/analysis hashes changed")
artifact_manifest <- file.path(plots,"data/artifact_manifest.csv")
if(file.exists(artifact_manifest)) {
  am <- read(artifact_manifest)
  # A fresh validation log is allowed to be written by this invocation.
  am <- filter(am,path!="validation.log")
  check(identical(unname(tools::md5sum(file.path(plots,am$path))),am$md5),"Artifact hashes changed")
}
cat("PASS:",n_participants,"participants; independent source scores, benchmark and history eligibility.\n")
cat("PASS:",total,"mixed-model/cluster contrasts independently reconstructed; all Holm families.\n")
cat("PASS: H7 group slopes and direct differences for both corrections and clustered sensitivity; separate common-slope families.\n")
cat("PASS: all 105 required primary annotations, including five in each H4/H5 panel.\n")
cat("PASS: four H7 scatter panels; fitted slopes and 480 pointwise confidence predictions independently reconstructed.\n")
cat("PASS: 18 standalone PDF/PNG pairs; 10/18-page combined PDFs; report links.\n")
