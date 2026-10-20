# Focused regressions for response-scale intervals, missing pairs and unavailable inference.
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(emmeans)})
script <- normalizePath(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]))
root <- dirname(dirname(script));source(file.path(root,"analyse_dynamic_reliability_primary.R"))
grid <- expand.grid(group=GROUPS,stage=c("Manual pre","P1"))
spec <- dpr_specs(grid,list(c("P1","Manual pre")))
raw <- expand_grid(participant_id=1:6,stage=c("Manual pre","P1")) %>%
  mutate(group=ifelse(participant_id<=3,"CAL65","CAL90"),value=ifelse(stage=="P1",participant_id,0)) %>%
  filter(!(participant_id==1 & stage=="Manual pre"))
a <- dpr_participant(raw,"value",grid,spec,1);b <- dpr_participant(raw,"value",grid,spec,100)
for(name in c("estimate","se","lower","upper"))stopifnot(isTRUE(all.equal(a[[name]]*100,b[[name]])))
stopifnot(isTRUE(all.equal(a$p_raw,b$p_raw)),identical(a$n_participants,c(2L,3L,5L)),
  isTRUE(all.equal(a$estimate,c(2.5,5,2.5))))
for(i in 1:3) {
  t <- if(i==1)t.test(c(2,3)) else if(i==2)t.test(c(4,5,6)) else t.test(c(4,5,6),c(2,3))
  stopifnot(abs(a$lower[i]-t$conf.int[1])<1e-10,abs(a$upper[i]-t$conf.int[2])<1e-10)
}
fit <- lm(value~group*stage,raw);em <- emmeans(fit,~group*stage)
sp <- dpr_specs(as.data.frame(em),list(c("P1","Manual pre")))
unavailable <- dpr_table(em,sp,100,FALSE)
stopifnot(all(!unavailable$inference_available),all(is.na(unavailable$p_raw)),
  all(is.na(unavailable$lower)),all(is.na(unavailable$upper)),all(is.finite(unavailable$estimate)))
bad <- tibble(participant_id=1,group="CAL65",phase="FULL",advice=c("Correct","Incorrect"),
  n_answered=c(10,0),n_agree=c(8,0))
stopifnot(inherits(tryCatch({sdt_scores(bad);NULL},error=identity),"error"))
cat("PASS: percentage-point CI scaling; paired eligibility without zero imputation; independent t intervals; unavailable inference masks; empty SDT denominators rejected.\n")
# Known unequal slopes must remain unequal; the interaction is their signed difference.
x <- expand_grid(group=factor(GROUPS,GROUPS),centred_score=seq(-2,2,length.out=15)) %>%
  mutate(value=ifelse(group=="CAL65",1+centred_score,2+3*centred_score)+.15*cos(centred_score*4))
m <- lm(value~group*centred_score,x);V <- sandwich::vcovHC(m,type="HC3")
tr <- emtrends(m,~group,var="centred_score",vcov.=V,df=df.residual(m))
sp <- dpr_association_specs(as.data.frame(tr),"advice_bias","FULL")
z <- dpr_table(tr,sp)
stopifnot(max(abs(z$estimate-c(1,3,2)))<1e-8,identical(z$group,c(GROUPS,"CAL90 - CAL65")))
X <- model.matrix(m);bread <- solve(crossprod(X));omega <- residuals(m)^2/(1-hatvalues(m))^2
independent_V <- bread%*%crossprod(X,X*omega)%*%bread
stopifnot(abs(z$se[3]-sqrt(independent_V["groupCAL90:centred_score","groupCAL90:centred_score"]))<1e-10)
common <- lm(value~group+centred_score,x)
stopifnot(abs(coef(common)["centred_score"]-2)<1e-8)
cat("PASS: unequal group slopes, signed interaction contrast and independent HC3 variance; additive model remains a separate common-slope sensitivity.\n")
# Scheduled midpoint centring and polynomial estimands have known values.
stopifnot(identical((c(1,200.5,400)-200.5)/399,c(-.5,0,.5)))
g <- expand.grid(group=GROUPS,phase=c("P1","P2","P3"),x=c(-.5,0,.5))
g$linear <- ifelse(g$group=="CAL65",.4,.8)
g$quadratic <- ifelse(g$phase=="P1",0,ifelse(g$phase=="P2",-.4,1))*ifelse(g$group=="CAL65",1,2)
g$eta <- 1+g$linear*g$x+g$quadratic*g$x^2
for(kind in c("end_start","curvature")) {
  sp <- dpr_trend_specs(g,kind)
  for(id in names(sp$weights)) {
    meta <- filter(sp$metadata,contrast_id==id)
    cell <- g[g$phase==meta$phase & g$x==0,]
    v <- if(kind=="curvature")cell$quadratic else cell$linear
    expected <- if(meta$group=="CAL90 - CAL65")v[2]-v[1] else v[match(meta$group,cell$group)]
    stopifnot(abs(sum(sp$weights[[id]]*g$eta)-expected)<1e-12)
  }
}
# Probability curvature can exist without a quadratic logit coefficient.
p <- plogis(1+c(-.5,0,.5)*2)
stopifnot(abs(sum(c(2,-4,2)*p))>.01,abs(sum(c(2,-4,2)*(1+c(-.5,0,.5)*2)))<1e-12)
# Actual mixed-model derivatives: linear, flattening, and reversing shapes.
set.seed(1005)
d <- expand_grid(subject=factor(1:24),phase=factor(c("P1","P2","P3")),x=seq(-.5,.5,length.out=21)) %>%
  mutate(group=factor(ifelse(as.integer(subject)<=12,"CAL65","CAL90"),GROUPS),
    linear=ifelse(phase=="P1",.8,ifelse(phase=="P2",.4,0)),
    quadratic=ifelse(phase=="P1",0,ifelse(phase=="P2",-.4,1)),
    log_rt=1+linear*x+quadratic*x^2+rnorm(24,0,.3)[as.integer(subject)]+rnorm(n(),0,.015))
m <- lmerTest::lmer(log_rt~group*phase*(x+I(x^2))+(1|subject),d)
z <- dpr_shape(m,"correct_rt")
for(i in seq_len(nrow(z))) {
  r <- z[i,];nd <- data.frame(group=r$group,phase=r$phase,x=r$x,subject=d$subject[1])
  h <- 1e-5;left <- transform(nd,x=x-h);right <- transform(nd,x=x+h)
  pred <- function(x)as.numeric(predict(m,x,re.form=NA))
  stopifnot(abs(r$model_estimate-(pred(right)-pred(left))/(2*h))<1e-8,
    abs(r$response_estimate-(exp(pred(right))-exp(pred(left)))/(2*h))<1e-8)
}
stopifnot(all(filter(z,phase=="P3")$reversal_within_phase),
  all(abs(filter(z,phase=="P2",position=="End")$model_estimate)<.05),
  all(abs(filter(z,phase=="P1")$quadratic)<.05))
unavailable <- dpr_shape(m,"correct_rt",FALSE)
stopifnot(all(is.na(unavailable$model_lower)),all(is.na(unavailable$response_upper)),
  all(is.finite(unavailable$model_estimate)),!any(grepl("p_raw|p_holm",names(z))))
# Unobserved advice bins remain absent, and insufficient individual data cannot supply inference.
sparse <- tibble(participant_id=1,group="CAL65",phase="P1",progress=0,value=1)
sg <- expand.grid(group=GROUPS,phase=c("P1","P2","P3"),progress=c(0,1))
ss <- dpr_participant(sparse,"value",sg,dpr_trend_specs(sg))
stopifnot(all(!ss$inference_available),all(is.na(ss$p_raw)))
cat("PASS: scheduled centring; net-change and quadratic weights; signed group differences; logistic-scale distinction; exact model/response derivatives; linear, flattening and reversal shapes; missing cells and unavailable shape intervals.\n")
# Acquisition-source drift may be recorded; raw, fit and analysis drift must fail.
fixture <- tempfile('h5_provenance_');dir.create(file.path(fixture,'python'),recursive=TRUE)
writeLines('old task',file.path(fixture,'python/virus_task.py'))
writeLines('analysis',file.path(fixture,'analysis.R'))
writeLines('raw',file.path(fixture,'raw.csv'))
writeLines('fit',file.path(fixture,'fit.rds'))
manifest <- function(paths,name)readr::write_csv(tibble(path=paths,md5=unname(tools::md5sum(paths))),file.path(fixture,paste0(name,'.csv')))
manifest(file.path(fixture,c('python/virus_task.py','analysis.R')),'source_checksums')
manifest(file.path(fixture,'raw.csv'),'input_manifest');manifest(file.path(fixture,'fit.rds'),'analysis_artifacts')
writeLines('new task',file.path(fixture,'python/virus_task.py'))
audit <- dpr_verify_historical(fixture,fixture)
stopifnot(sum(audit$md5!=audit$current_md5)==1,all(audit$acquisition_only[audit$md5!=audit$current_md5]))
for(file in c('raw.csv','fit.rds','analysis.R')) {
  old <- readLines(file.path(fixture,file));writeLines('changed',file.path(fixture,file))
  fail <- tryCatch(dpr_verify_historical(fixture,fixture),error=identity)
  stopifnot(inherits(fail,'error'));writeLines(old,file.path(fixture,file))
}
unlink(fixture,recursive=TRUE)
cat('PASS: acquisition-source audit and strict rejection of raw, model and analysis changes.\n')
# The two half-phase contrasts partition the original endpoint change.
g <- expand.grid(group=GROUPS,phase=c("P1","P2","P3"),x=c(-.5,0,.5))
g$linear <- ifelse(g$group=="CAL65",.4,.8)
g$quadratic <- ifelse(g$phase=="P1",0,ifelse(g$phase=="P2",-.4,1))*ifelse(g$group=="CAL65",1,2)
g$eta <- 1+g$linear*g$x+g$quadratic*g$x^2
specs <- lapply(c("middle_start","end_middle","end_start","curvature"),function(k)dpr_trend_specs(g,k))
for(i in seq_along(specs[[1]]$weights)) {
  first <- specs[[1]]$weights[[i]];second <- specs[[2]]$weights[[i]]
  stopifnot(identical(first+second,specs[[3]]$weights[[i]]),identical(2*(second-first),specs[[4]]$weights[[i]]))
  meta <- specs[[1]]$metadata[i,];cells <- g[g$phase==meta$phase & g$x==0,]
  for(kind in c("middle_start","end_middle")) {
    values <- .5*cells$linear+if(kind=="middle_start")(-.25*cells$quadratic) else .25*cells$quadratic
    wanted <- if(meta$group=="CAL90 - CAL65")values[2]-values[1] else values[match(meta$group,cells$group)]
    w <- if(kind=="middle_start")first else second
    stopifnot(abs(sum(w*g$eta)-wanted)<1e-12)
  }
  stopifnot(abs(sum(first*plogis(g$eta))+sum(second*plogis(g$eta))-sum(specs[[3]]$weights[[i]]*plogis(g$eta)))<1e-12,
    abs(exp(sum(first*g$eta))*exp(sum(second*g$eta))-exp(sum(specs[[3]]$weights[[i]]*g$eta)))<1e-12)
}
stopifnot(identical(c(-.5,0,.5)*399+200.5,c(1,200.5,400)))
cat('PASS: analytic half-phase estimates; signed group differences; probability-scale additivity; RT ratio multiplication; midpoint at 200.5.\n')
# Revised-cache corruption/data drift cannot be silently reused.
fixture <- tempfile("h5_cache_");dir.create(file.path(fixture,"primary"),recursive=TRUE)
writeLines("complete",file.path(fixture,"primary/COMPLETE.txt"))
writeLines("raw",file.path(fixture,"raw.csv"))
paths <- unlist(lapply(c("primary","all_participants"),function(s)paste0(s,"/models/",c("accuracy","agreement","correct_rt"),"_quadratic_trend.rds")))
paths <- c(paths,paste0(c("primary","all_participants"),"/models/correct_rt_trend.rds"))
for(p in paths) {dir.create(dirname(file.path(fixture,"primary",p)),recursive=TRUE,showWarnings=FALSE);writeLines("fixture",file.path(fixture,"primary",p))}
readr::write_csv(task_hashes(file.path(fixture,"raw.csv")),file.path(fixture,"primary/input_checksums.csv"))
artifacts <- c(paths,"input_checksums.csv")
readr::write_csv(tibble(path=artifacts,sha256=vapply(file.path(fixture,"primary",artifacts),task_sha,character(1),USE.NAMES=FALSE)),file.path(fixture,"primary/artifact_manifest.csv"))
stopifnot(!is.null(dpr_trend_cache(fixture,fixture)))
writeLines("changed",file.path(fixture,"raw.csv"));stopifnot(is.null(dpr_trend_cache(fixture,fixture)))
writeLines("raw",file.path(fixture,"raw.csv"));writeLines("changed",file.path(fixture,"primary",paths[1]))
stopifnot(is.null(dpr_trend_cache(fixture,fixture)))
unlink(fixture,recursive=TRUE)
cat("PASS: revised-cache hash and data guards for six quadratic and two linear RT fits.\n")
# Formula and observation matching are required even for a hash-verified cache.
fixture <- tempfile('h5_frame_');dir.create(file.path(fixture,'models'),recursive=TRUE)
candidate <- file.path(fixture,'candidate.rds');saveRDS(m,candidate)
d$participant_id <- as.integer(d$subject)
z <- dpr_fit('same',formula(m),d,FALSE,fixture,candidate)
stopifnot(startsWith(z$diagnostics$origin,'validated identical fit:'))
changed <- d;changed$log_rt[1] <- changed$log_rt[1]+.1
z <- dpr_fit('changed_data',formula(m),changed,FALSE,fixture,candidate)
stopifnot(z$diagnostics$origin=='new fit')
z <- dpr_fit('changed_formula',log_rt~group*phase*x+(1|subject),d,FALSE,fixture,candidate)
stopifnot(z$diagnostics$origin=='new fit')
unlink(fixture,recursive=TRUE)
cat('PASS: formula and observation mismatches force refitting even with a candidate cache.\n')
