# Independent reconstruction of scores, denominators, contrasts and uncertainty.
# Rscript tests/validate_aid_onset_primary.R [analysis_dir] [figure_dir]
script <- sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
ROOT <- dirname(dirname(normalizePath(script)))
source(file.path(ROOT,"R/aid_onset_primary.R"))
args <- commandArgs(trailingOnly=TRUE)
out <- if(length(args)) args[1] else file.path(ROOT,"analysis_outputs/semester2_2026_hypotheses/primary")
read <- function(name) read.csv(file.path(out,paste0(name,".csv")),stringsAsFactors=FALSE)
near <- function(x,y,tol=1e-8) {
  x <- unname(x);y <- unname(y)
  stopifnot(length(x)==length(y),identical(is.na(x),is.na(y)))
  ok <- is.finite(x) & is.finite(y)
  stopifnot(all(is.finite(x)==is.finite(y)),all(abs(x[ok]-y[ok])<tol))
}
ao_verify(read("input_checksums"))
artifacts <- read("artifact_manifest");ao_verify(transform(artifacts,path=file.path(out,path)))
inputs <- read("input_checksums")
stopifnot(!any(grepl("participant_qc|semester2_2026_qc|participant_exclusions",inputs$path)))
selection <- read("selection")
stopifnot(nrow(selection)==60,identical(selection$participant_id,1:60),all(selection$included),!any(selection$exclusions_applied),
  selection$run_timestamp[selection$participant_id==59]=="20260924_110831")
raw <- read.csv(file.path(ROOT,"data/data_virus_all.csv"),stringsAsFactors=FALSE)
raw$condition <- c(MANUAL="Manual",AIDFIRST="Aid-first",STIMFIRST="Stimulus-first")[raw$condition_code]
raw$c1 <- raw$decision1_response==raw$stimulus;raw$c1[is.na(raw$c1)] <- FALSE
raw$c2 <- raw$decision2_response==raw$stimulus;raw$c2[is.na(raw$c2)] <- FALSE
raw$a1 <- raw$decision1_response %in% c("BLACK","WHITE")
raw$a2 <- raw$decision2_response %in% c("BLACK","WHITE")
counts <- read("counts");scores <- read("scores");denoms <- read("denominators")
for(i in seq_len(nrow(counts))) {
  r <- counts[i,];x <- raw[raw$participant_id==r$participant_id & raw$condition==r$condition,]
  ans <- x[[paste0("decision",substring(r$decision,2),"_response")]]
  x <- x[ans %in% c("BLACK","WHITE"),];ans <- ans[ans %in% c("BLACK","WHITE")]
  signal <- if(r$domain=="advice") x$aid_label==x$stimulus else x$stimulus=="BLACK"
  positive <- if(r$domain=="advice") ans==x$aid_label else if(r$domain=="aid_task") x$aid_label=="BLACK" else ans=="BLACK"
  near(c(r$h,r$nh,r$f,r$nf),c(sum(positive & signal),sum(signal),sum(positive & !signal),sum(!signal)))
}
for(i in seq_len(nrow(scores))) {
  r <- scores[i,]
  correct <- function(k,n) if(r$correction=="loglinear") (k+.5)/(n+1) else if(k==0) .5/n else if(k==n) 1-.5/n else k/n
  H <- correct(r$h,r$nh);F <- correct(r$f,r$nf)
  near(c(r$hit_rate,r$false_alarm_rate,r$dprime,r$bias,r$criterion_c),c(H,F,qnorm(H)-qnorm(F),(qnorm(H)+qnorm(F))/2,-(qnorm(H)+qnorm(F))/2))
}
for(i in seq_len(nrow(denoms))) {
  r <- denoms[i,];x <- raw[raw$participant_id==r$participant_id & raw$condition==r$condition,]
  near(unlist(r[c("n_trials","n_answered1","n_answered2","n_paired","n_d1_timeout","n_d2_timeout")]),
    c(nrow(x),sum(x$a1),sum(x$a2),sum(x$a1 & x$a2),sum(!x$a1),sum(!x$a2)))
}
context <- read("decision_context")
for(i in seq_len(nrow(context))) {
  r <- context[i,];x <- raw[raw$participant_id==r$participant_id & raw$condition==r$condition & raw$a1 & raw$a2,]
  response <- x[[paste0("decision",substring(r$decision,2),"_response")]]
  if(r$outcome=="accuracy") value <- 100*mean(response==x$stimulus) else {
    H <- (sum(response=="BLACK" & x$stimulus=="BLACK")+.5)/(sum(x$stimulus=="BLACK")+1)
    F <- (sum(response=="BLACK" & x$stimulus=="WHITE")+.5)/(sum(x$stimulus=="WHITE")+1)
    value <- if(r$outcome=="task_dprime") qnorm(H)-qnorm(F) else (qnorm(H)+qnorm(F))/2
  }
  near(r$value,value)
}
opportunities <- read("selective_uptake_denominators")
for(i in seq_len(nrow(opportunities))) {
  r <- opportunities[i,];x <- raw[raw$participant_id==r$participant_id & raw$condition=="Stimulus-first",]
  x <- x[x$a1 & x$a2 & x$decision1_response!=x$aid_label & (x$aid_label==x$stimulus)==r$advice_correct,]
  near(c(r$n_opportunities,r$n_followed),c(nrow(x),sum(x$decision2_response==x$aid_label)))
}
# Independently check every participant cell, including human/aid matched support.
cells <- read("raw");slider <- read.csv(file.path(ROOT,"data/data_virus_sliders_all.csv"))
trust <- read.csv(file.path(ROOT,"data/data_virus_postblock_all.csv"))
for(i in seq_len(nrow(cells))) {
  r <- cells[i,];x <- raw[raw$participant_id==r$participant_id,]
  if(r$outcome=="selective_uptake") x <- x[x$condition=="Stimulus-first" & x$a1 & x$a2 & x$decision1_response!=x$aid_label &
    (x$aid_label==x$stimulus)==(r$condition=="Correct advice"),] else x <- x[x$condition==r$condition,]
  o <- r$outcome
  sc <- scores[scores$participant_id==r$participant_id & scores$condition==r$condition & scores$decision=="D2" & scores$correction==r$correction,]
  value <- switch(o,decision1_rt=mean(log(x$decision1_rt_s[x$c1 & is.finite(x$decision1_rt_s) & x$decision1_rt_s>=.2])),accuracy=100*mean(x$c2),accuracy_advantage=100*mean(as.numeric(x$c2)-as.numeric(x$aid_label==x$stimulus)),
    task_dprime=sc$dprime[sc$domain=="task"],task_bias=sc$bias[sc$domain=="task"],advice_dprime=sc$dprime[sc$domain=="advice"],
    advice_bias=sc$bias[sc$domain=="advice"],task_dprime_advantage=sc$dprime[sc$domain=="task"]-sc$dprime[sc$domain=="aid_task"],
    switching=100*mean((x$decision1_response!=x$decision2_response)[x$a1 & x$a2]),
    beneficial=100*mean((!x$c1 & x$c2)[x$a1 & x$a2]),harmful=100*mean((x$c1 & !x$c2)[x$a1 & x$a2]),
    selective_uptake=100*mean(x$decision2_response==x$aid_label),
    correct_advice_accuracy=100*mean(x$c2[if(r$condition=="Manual") rep(TRUE,nrow(x)) else x$aid_label==x$stimulus]),
    incorrect_advice_accuracy=100*mean(x$c2[if(r$condition=="Manual") rep(TRUE,nrow(x)) else x$aid_label!=x$stimulus]),
    perceived=slider$response_percent[slider$participant_id==r$participant_id & slider$condition_code==unique(x$condition_code)],
    trust=mean(trust$response[trust$participant_id==r$participant_id & trust$condition_code==unique(x$condition_code)]))
  near(r$value,value)
}
tests <- read("contrasts");paired <- read("participant_sensitivity");weights <- read("weights");means <- read("means");diag <- read("diagnostics")
for(g in split(weights,interaction(weights$model,weights$comparison,drop=TRUE))) {
  expected <- switch(g$comparison[1],AF_minus_Manual=c(Manual=-1,`Aid-first`=1),
    SF_minus_Manual=c(Manual=-1,`Stimulus-first`=1),SF_minus_AF=c(`Aid-first`=-1,`Stimulus-first`=1),
    human_minus_aid_AF=c(`Aid-first`=1),human_minus_aid_SF=c(`Stimulus-first`=1),
    correct_minus_incorrect=c(`Incorrect advice`=-1,`Correct advice`=1),
    mean_aid_minus_self=c(Manual=-1,`Aid-first`=.5,`Stimulus-first`=.5))
  w <- expected[g$condition];w[is.na(w)] <- 0;near(g$weight,w)
}
primary <- subset(tests,primary & correction=="loglinear")
stopifnot(nrow(primary)==28,!anyDuplicated(primary$contrast_id),all(diag$n_participants==60))
registry <- read("hypothesis_figures")
ids <- unlist(strsplit(paste(registry$contrast_ids[registry$section=="core"],collapse=";"),";",fixed=TRUE))
stopifnot(length(ids)==29,setequal(unique(ids),primary$contrast_id),
  sum(ids=="H1__task_dprime__SF_minus_AF")==2,length(unique(registry$figure))==16)
for(name in unique(tests$model)) {
  m <- readRDS(file.path(out,"models",paste0(name,".rds")))
  tt <- tests[tests$model==name,];ww <- weights[weights$model==name,];mm <- means[means$model==name,]
  dd <- diag[diag$model==name,]
  if(!is.null(m)) {
    saved_data <- read.csv(file.path(out,"model_data",paste0(name,".csv")))
    near(model.response(model.frame(m)),saved_data$response)
    stopifnot(all(as.character(model.frame(m)$subject)==as.character(saved_data$subject)),
      all(as.character(model.frame(m)$condition)==saved_data$condition))
    stopifnot(nlevels(getME(m,"flist")$subject)==60,nrow(model.frame(m))==dd$n_observations,
      identical(isSingular(m,tol=1e-4),dd$singular),length(getME(m,"cnms"))==1,identical(unname(getME(m,"cnms")[[1]]),"(Intercept)"))
    em <- emmeans(m,~condition,lmer.df="satterthwaite",lmerTest.limit=Inf)
    binary <- inherits(m,"glmerMod");if(binary) em <- regrid(em,transform="response")
    a <- as.data.frame(summary(em,infer=c(TRUE,FALSE)));scale <- if(binary) 100 else 1
    near(mm$estimate,a[[if(binary) "prob" else "emmean"]]*scale)
    lo <- intersect(c("lower.CL","asymp.LCL"),names(a));hi <- intersect(c("upper.CL","asymp.UCL"),names(a))
    if(dd$inference_available) {near(mm$lower,a[[lo]]*scale);near(mm$upper,a[[hi]]*scale)}
    for(k in seq_len(nrow(tt))) {
      w <- ww[ww$comparison==tt$comparison[k],];w <- w$weight[match(as.character(a$condition),w$condition)]
      ct <- as.data.frame(summary(contrast(em,list(check=w)),infer=c(TRUE,TRUE),adjust="none"))
      near(tt$estimate[k],ct$estimate*scale);near(tt$se[k],ct$SE*scale);near(tt$df[k],ct$df)
      # Also check estimate and covariance multiplication without contrast().
      near(tt$estimate[k],sum(w*a[[if(binary) "prob" else "emmean"]])*scale)
      near(tt$se[k],sqrt(as.numeric(t(w)%*%vcov(em)%*%w))*scale)
      if(dd$inference_available) {
        near(tt$p_raw[k],ct$p.value);near(tt$lower[k],ct[[lo]]*scale);near(tt$upper[k],ct[[hi]]*scale)
      } else stopifnot(is.na(tt$p_raw[k]),is.na(tt$lower[k]),is.na(tt$upper[k]))
    }
  } else stopifnot(!dd$inference_available,all(is.na(tt$p_raw)))
}
for(i in seq_len(nrow(paired))) {
  r <- paired[i,];w <- weights[weights$model==r$model & weights$comparison==r$comparison,]
  x <- cells[cells$model==r$model,];x$value <- x$value*w$weight[match(x$condition,w$condition)]
  z <- aggregate(value~participant_id,x,sum)$value;se <- sd(z)/sqrt(length(z));df <- length(z)-1
  near(c(r$estimate,r$se,r$df,r$lower,r$upper,r$p_raw),
    c(mean(z),se,df,mean(z)-qt(.975,df)*se,mean(z)+qt(.975,df)*se,2*pt(-abs(mean(z)/se),df)))
}
for(x in list(tests,paired,read("historical_family_sensitivity"))) {
  for(g in split(x,interaction(x$analysis,x$correction,x$family,drop=TRUE))) {
    near(g$p_holm,p.adjust(g$p_raw,"holm"));stopifnot(all(g$family_size==nrow(g)))
  }
}
# New RT eligibility/aggregation is reconstructed independently from trial fields.
rt <- read("rt_participant_cells")
stopifnot(nrow(rt)==180,all(rt$n_eligible>0),sum(tests$outcome=="decision1_rt")==3)
for(i in seq_len(nrow(rt))) {
  a <- rt[i,];d <- raw[raw$participant_id==a$participant_id & raw$condition==a$condition,]
  eligible <- d$c1 & is.finite(d$decision1_rt_s) & d$decision1_rt_s>=.2
  near(a$n_eligible,sum(eligible));near(a$n_trials,nrow(d));near(a$n_correct,sum(d$c1))
  near(a$n_nonfinite_correct,sum(d$c1 & !is.finite(d$decision1_rt_s)))
  near(a$n_fast_correct,sum(d$c1 & is.finite(d$decision1_rt_s) & d$decision1_rt_s<.2,na.rm=TRUE))
  near(a$mean_log_rt,mean(log(d$decision1_rt_s[eligible])))
  near(a$geometric_mean_s,exp(mean(log(d$decision1_rt_s[eligible]))))
}
for(kind in c("rt_ratios","rt_estimates")) {
  a <- read(kind)
  near(a$ratio_or_seconds,exp(a$estimate));near(a$backtransformed_lower,exp(a$lower));near(a$backtransformed_upper,exp(a$upper))
  reference <- if(kind=="rt_estimates") means[means$outcome=="decision1_rt",] else
    rbind(tests[tests$outcome=="decision1_rt",],paired[paired$outcome=="decision1_rt",])
  near(a$estimate,reference$estimate);near(a$p_holm,reference$p_holm)
}
fixture <- tibble(participant_id=1,condition="Manual",answered1=TRUE,
  correct1=c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE),
  decision1_rt_s=c(.199,.2,0,-1,NA,1,Inf,100))
z <- ao_rt_cells(fixture);near(z$n_eligible,2);near(z$geometric_mean_s,sqrt(.2*100))
stopifnot(inherits(try(ao_rt_cells(transform(fixture,correct1=FALSE)),silent=TRUE),"try-error"))
# Trust cells reproduce questionnaire means, H3 final-response bias and H5a denominators.
tc <- read("trust_association_cells");ct <- read("trust_associations")
stopifnot(nrow(tc)==360,nrow(ct)==12,sum(ct$main)==4,all(ct$n_participants==60),
  !anyDuplicated(tc[c("participant_id","condition","outcome","correction")]))
for(i in seq_len(nrow(tc))) {
  a <- tc[i,];d <- raw[raw$participant_id==a$participant_id & raw$condition==a$condition,]
  q <- trust[trust$participant_id==a$participant_id & trust$condition_code==d$condition_code[1],]
  near(a$trust,mean(q$response));stopifnot(nrow(q)==6)
  if(a$outcome=="trust_revision") {
    d <- d[d$a1 & d$a2,];near(a$value,100*mean(d$decision1_response!=d$decision2_response))
  } else {
    d <- d[d$a2,];signal <- d$aid_label==d$stimulus;agree <- d$decision2_response==d$aid_label
    rate <- function(k,n) if(a$correction=="loglinear") (k+.5)/(n+1) else if(k==0) .5/n else if(k==n) 1-.5/n else k/n
    near(a$value,(qnorm(rate(sum(agree & signal),sum(signal)))+qnorm(rate(sum(agree & !signal),sum(!signal))))/2)
  }
  near(a$n_trials,nrow(d))
}
for(i in seq_len(nrow(ct))) {
  a <- ct[i,];d <- tc[tc$outcome==a$outcome & tc$condition==a$condition & tc$correction==a$correction,]
  test <- cor.test(d$trust,d$value,method=a$analysis,exact=FALSE)
  near(a$estimate,unname(test$estimate));near(a$p_raw,test$p.value)
  if(a$analysis=="pearson") near(c(a$lower,a$upper),test$conf.int)
}
for(g in split(ct,interaction(ct$outcome,ct$correction,ct$analysis,drop=TRUE))) {
  near(g$p_holm,p.adjust(g$p_raw,"holm"));stopifnot(all(g$family_size==2))
}
for(method in c("pearson","spearman")) {
  z <- ao_correlation(rep(1,5),1:5,method)
  stopifnot(!z$inference_available,is.na(z$p_raw),is.na(z$estimate))
  stopifnot(!ao_correlation(c(1,2,NA,Inf),1:4,method)$inference_available)
}
# Asymmetric and extreme SDT fixtures, sign geometry, empty-class failure.
for(cor in c("loglinear","extreme_only")) {
  a <- ao_score(0,200,40,40,cor);b <- ao_score(200,200,0,40,cor)
  near(a$dprime,-b$dprime);near(a$bias,-b$bias)
}
stopifnot(inherits(try(ao_score(0,0,1,2),silent=TRUE),"try-error"))
# The analysis has no QC dependency: even a policy excluding everyone is ignored.
fake <- tempfile("qc_independence_");dir.create(fake);invisible(file.symlink(file.path(ROOT,"data"),file.path(fake,"data")))
dir.create(file.path(fake,"config"));write.csv(data.frame(participant_id=1:60,decision="exclude"),file.path(fake,"config/participant_exclusions.csv"),row.names=FALSE)
p <- ao_prepare(fake);stopifnot(all(p$selection$participant_id==1:60),all(p$selection$included))
stopifnot(isTRUE(all.equal(as.data.frame(p$counts),counts,check.attributes=FALSE,tolerance=1e-12)),
  isTRUE(all.equal(as.data.frame(p$scores),scores,check.attributes=FALSE,tolerance=1e-12)))
unlink(fake,recursive=TRUE)
writeLines(c(paste("artifact_manifest_md5",unname(tools::md5sum(file.path(out,"artifact_manifest.csv"))))),file.path(out,"VALIDATED.txt"))
cat("PASS: canonical sources; all 60 participants; independent counts, scores and cells; 28 original tests plus seven additions; model covariance/CIs; Holm families; QC independence.\n")
if(length(args)>=2) {
  source(file.path(ROOT,"R/aid_onset_primary_plots.R"))
  stopifnot(identical(ao_significance(c(.0009,.001,.009,.01,.049,.05,NA)),c("***","**","**","*","*","ns","unavailable")),
    identical(ao_significance(.0001,FALSE),"unavailable"))
  plots <- args[2]
  pa <- read.csv(file.path(plots,"artifact_manifest.csv"));ao_verify(transform(pa,path=file.path(plots,path)))
  catlog <- read.csv(file.path(plots,"figure_catalog.csv"))
  stopifnot(nrow(catlog)==16,sum(catlog$section=="core")==8,all(file.exists(file.path(plots,paste0(catlog$figure,".pdf")))))
  for(fig in unique(registry$figure[registry$section %in% c("core","extension_rt") | registry$order==11])) {
    annotations <- read.csv(file.path(plots,"data",paste0(fig,"_annotations.csv")))
    expected <- unlist(strsplit(paste(registry$contrast_ids[registry$figure==fig],collapse=";"),";",fixed=TRUE))
    stopifnot(identical(annotations$contrast_id,expected))
    z <- tests[match(annotations$contrast_id,ifelse(tests$correction=="loglinear",tests$contrast_id,"")),]
    near(annotations$estimate,z$estimate);near(annotations$p_holm,z$p_holm)
    stopifnot(!any(annotations$significant[!annotations$inference_available]))
    plotted <- read.csv(file.path(plots,"data",paste0(fig,"_estimates.csv")))
    z <- means[match(paste(plotted$model,plotted$condition),paste(means$model,means$condition)),]
    if(all(plotted$outcome=="decision1_rt")) z <- transform(z,estimate=exp(estimate),lower=exp(lower),upper=exp(upper))
    near(plotted$estimate,z$estimate);near(plotted$lower,z$lower);near(plotted$upper,z$upper)
    dots <- read.csv(file.path(plots,"data",paste0(fig,"_participants.csv")))
    z <- cells[match(paste(dots$model,dots$condition,dots$participant_id),paste(cells$model,cells$condition,cells$participant_id)),]
    near(dots$value,if(all(dots$outcome=="decision1_rt")) exp(z$value) else z$value)
    # Check bracket positions against the saved contrast weights, independently
    # of the plotting lookup; pooled and zero-reference tests are not pairwise.
    for(i in seq_len(nrow(annotations))) {
      a <- annotations[i,];w <- weights[weights$model==a$model & weights$comparison==a$comparison & weights$weight!=0,]
      shown <- plotted[plotted$model==a$model,]
      locations <- shown$x[match(w$condition,shown$condition)]
      type <- if(nrow(w)==1) "zero_reference" else if(nrow(w)==2) "bracket" else "pooled_bracket"
      stopifnot(a$annotation_type==type)
      expected_x1 <- if(type=="zero_reference") locations else sum(locations[w$weight<0]*abs(w$weight[w$weight<0]))
      expected_x2 <- sum(locations[w$weight>0]*w$weight[w$weight>0])
      near(c(a$x1,a$x2),c(expected_x1,expected_x2))
      expected_symbol <- if(!a$inference_available || is.na(a$p_holm)) "unavailable" else
        if(a$p_holm<.001) "***" else if(a$p_holm<.01) "**" else if(a$p_holm<.05) "*" else "ns"
      stopifnot(a$symbol==expected_symbol,grepl(expected_symbol,a$label,fixed=TRUE),
        a$axis_upper>a$text_y,a$text_y>a$y,a$y-a$tick>max(dots$value[dots$model==a$model],na.rm=TRUE))
      if(type=="pooled_bracket") near(c(a$group_x1,a$group_x2),range(locations[w$weight>0]))
    }
    # Inspect the actual ggplot layers as well as annotation CSVs: exported
    # bracket data alone must not pass if the drawn line layers are missing.
    rows <- registry[registry$figure==fig,]
    for(i in seq_len(nrow(rows))) {
      row <- rows[i,];tt <- annotations[annotations$model==row$model,]
      panel <- ao_panel(row,cells,means,tt)
      pp <- panel$plot[[1]];built <- ggplot_build(pp)
      segments <- which(vapply(pp$layers,function(layer) inherits(layer$geom,"GeomSegment"),logical(1)))
      drawn <- bind_rows(built$data[segments])
      n_links <- sum(tt$annotation_type!="zero_reference");n_pooled <- sum(tt$annotation_type=="pooled_bracket")
      stopifnot(nrow(drawn)==3*n_links+3*n_pooled)
      for(j in which(tt$annotation_type!="zero_reference")) {
        a <- tt[j,]
        stopifnot(any(abs(drawn$x-a$x1)<1e-9 & abs(drawn$xend-a$x2)<1e-9 &
          abs(drawn$y-a$y)<1e-9 & abs(drawn$yend-a$y)<1e-9))
      }
      text_layers <- which(vapply(pp$layers,function(layer) inherits(layer$geom,"GeomText"),logical(1)))
      labels <- unlist(lapply(built$data[text_layers],`[[`,"label"),use.names=FALSE)
      stopifnot(identical(sort(labels),sort(tt$label)))
    }
  }
  for(stem in catlog$figure[catlog$section=="correlation"]) {
    dots <- read.csv(file.path(plots,"data",paste0(stem,"_participants.csv")))
    shown <- read.csv(file.path(plots,"data",paste0(stem,"_annotations.csv")))
    curves <- read.csv(file.path(plots,"data",paste0(stem,"_estimates.csv")))
    stopifnot(nrow(dots)==120,nrow(shown)==2,nrow(curves)==200)
    stopifnot(identical(shown$contrast_id,strsplit(registry$contrast_ids[registry$figure==stem],";",fixed=TRUE)[[1]]))
    z <- tc[match(paste(dots$participant_id,dots$condition,dots$outcome,dots$correction),
      paste(tc$participant_id,tc$condition,tc$outcome,tc$correction)),]
    near(dots$value,z$value);near(dots$trust,z$trust)
    z <- ct[ct$main,];z <- z[match(shown$contrast_id,z$contrast_id),]
    near(shown$estimate,z$estimate);near(shown$lower,z$lower);near(shown$upper,z$upper);near(shown$p_holm,z$p_holm)
    for(condition in unique(curves$condition)) {
      d <- dots[dots$condition==condition,];c <- curves[curves$condition==condition,]
      pred <- predict(lm(value~trust,d),c,interval="confidence")
      near(c$fit,pred[,"fit"]);near(c$lwr,pred[,"lwr"]);near(c$upr,pred[,"upr"])
    }
    piece <- ao_trust_panel(registry[registry$figure==stem,],tc,ct)
    for(panel_index in 1:2) {
      panel <- piece$plot[[panel_index]]
      # Every condition has participant points and a saved linear confidence band.
      geoms <- vapply(panel[[1]]$layers,function(l) class(l$geom)[1],character(1))
      stopifnot(all(c("GeomPoint","GeomRibbon","GeomLine") %in% geoms))
    }
  }
  for(stem in catlog$figure) {
    connection <- file(file.path(plots,paste0(stem,".png")),"rb")
    seek(connection,where=16);size <- readBin(connection,"integer",n=2,size=4,endian="big");close(connection)
    stopifnot(identical(size,c(3600L,2025L)))
    info <- system2("pdfinfo",shQuote(file.path(plots,paste0(stem,".pdf"))),stdout=TRUE)
    stopifnot(any(grepl("^Pages: +1$",info)),any(grepl("^Page size: +864 x 486 pts",info)))
  }
  for(stem in catlog$figure[catlog$order %in% c(9,10)]) {
    dots <- read.csv(file.path(plots,"data",paste0(stem,"_participants.csv")))
    z <- context[match(paste(dots$participant_id,dots$condition,dots$decision,dots$outcome),
      paste(context$participant_id,context$condition,context$decision,context$outcome)),]
    near(dots$value,z$value)
    shown <- read.csv(file.path(plots,"data",paste0(stem,"_estimates.csv")))
    # Panels are written in registry order, six condition/decision rows each.
    outcomes <- registry$outcome[registry$figure==stem]
    for(i in seq_along(outcomes)) for(j in 1:6) {
      r <- shown[(i-1)*6+j,];v <- context$value[context$outcome==outcomes[i] & context$condition==r$condition & context$decision==r$decision]
      se <- sd(v)/sqrt(length(v));delta <- qt(.975,length(v)-1)*se
      near(c(r$estimate,r$se,r$lower,r$upper),c(mean(v),se,mean(v)-delta,mean(v)+delta))
    }
  }
  correction <- read.csv(file.path(plots,"data/12_s4_sdt_correction_annotations.csv"))
  z <- tests[match(paste(correction$contrast_id,correction$correction),paste(tests$contrast_id,tests$correction)),]
  near(correction$estimate,z$estimate);near(correction$lower,z$lower);near(correction$upper,z$upper);near(correction$p_holm,z$p_holm)
  inference <- read.csv(file.path(plots,"data/13_s5_inference_sensitivity_annotations.csv"))
  stopifnot(nrow(inference)==56)
  for(method in unique(inference$method)) {
    shown <- inference[inference$method==method,];z <- if(method=="Mixed model") tests else paired
    z <- z[z$correction=="loglinear",];z <- z[match(shown$contrast_id,z$contrast_id),]
    near(shown$p_holm,z$p_holm);near(shown$estimate,z$estimate)
  }
  for(stem in c("aid_onset_core_hypotheses","aid_onset_all_hypotheses")) {
    info <- system2("pdfinfo",shQuote(file.path(plots,paste0(stem,".pdf"))),stdout=TRUE)
    stopifnot(any(grepl(paste0("^Pages: +",if(stem=="aid_onset_core_hypotheses")11 else 16,"$"),info)))
  }
  cat("PASS: figure artifacts, catalog and saved annotations.\n")
}
