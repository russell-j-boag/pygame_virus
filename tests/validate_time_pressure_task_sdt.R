# Independent source-count, contrast, plot, and provenance reconciliation.
# Usage: Rscript tests/validate_time_pressure_task_sdt.R [input_csv] [analysis_dir] [plot_dir]
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr)})
script <- sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
root <- dirname(dirname(normalizePath(script)));args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)<=3)
input <- if(length(args)>=1)args[1] else file.path(root,"data/semester2_2026_data/data_virus_all.csv")
out <- if(length(args)>=2)args[2] else file.path(root,"analysis_outputs/semester2_2026_behavioural/task_sdt")
plots <- if(length(args)>=3)args[3] else file.path(dirname(out),"hypothesis_components/presentation/task_sdt")
near <- function(a,b,tol=1e-10)stopifnot(isTRUE(all.equal(as.numeric(a),as.numeric(b),tolerance=tol,check.attributes=FALSE)))
read <- function(p)read_csv(p,show_col_types=FALSE)
stopifnot(file.exists(file.path(out,"COMPLETE.txt")))
raw <- read(input);raw <- filter(raw,condition_deadline_code!="CAL_LP")
stopifnot(all(raw$stimulus%in%c("BLACK","WHITE")),all(raw$response%in%c("BLACK","WHITE","TIMEOUT")))
counts <- read(file.path(out,"stimulus_response_counts.csv"));scores <- read(file.path(out,"participant_task_sdt.csv"))
context <- read(file.path(out,"completion_accuracy_context.csv"));tests <- read(file.path(out,"contrasts.csv"))
cohort <- read(file.path(out,"cohort.csv"));n_people <- n_distinct(raw$participant_id)
stopifnot(nrow(raw)==1600*n_people,nrow(counts)==8*n_people,nrow(scores)==16*n_people,nrow(context)==4*n_people,
          nrow(cohort)==n_people,nrow(tests)==88)
stopifnot(!anyDuplicated(counts[c("participant_id","condition_deadline_code","actor")]),
          !anyDuplicated(scores[c("participant_id","condition_deadline_code","actor","correction")]))
corrections <- read(file.path(root,"config/participant_identity_corrections.csv"))
for(i in seq_len(nrow(cohort))) {
  c <- cohort[i,];r <- filter(raw,participant_id==c$participant_id)
  stopifnot(all(r$source_participant_id==c$source_participant_id),all(r$run_timestamp==c$run_timestamp),
            all(r$automation_reliability_pattern==c$pattern))
  correction <- filter(corrections,source_participant_id==c$source_participant_id,run_timestamp==c$run_timestamp)
  stopifnot(c$participant_id==if(nrow(correction))correction$participant_id else c$source_participant_id)
}
# Recompute all tables from response labels rather than exported accuracy/agreement.
expected_scores <- list()
for(i in seq_len(nrow(counts))) {
  c <- counts[i,];r <- filter(raw,participant_id==c$participant_id,condition_deadline_code==c$condition_deadline_code)
  stopifnot(nrow(r)==400,all(r$source_participant_id==c$source_participant_id),all(r$run_timestamp==c$run_timestamp))
  if(c$actor!="aid_all")r <- filter(r,response!="TIMEOUT")
  response <- if(c$actor=="human")r$response else r$aid_label
  signal <- r$stimulus=="BLACK"
  want <- c(n_scored=nrow(r),n_signal=sum(signal),n_noise=sum(!signal),hits=sum(signal & response=="BLACK"),
    misses=sum(signal & response=="WHITE"),false_alarms=sum(!signal & response=="BLACK"),correct_rejections=sum(!signal & response=="WHITE"))
  for(k in names(want))near(c[[k]],want[[k]])
  stopifnot(c$extreme_hit==(c$hits%in%c(0,c$n_signal)),c$extreme_false_alarm==(c$false_alarms%in%c(0,c$n_noise)))
  for(co in c("loglinear","extreme_only")) {
    rate <- function(k,n)if(co=="loglinear")(k+.5)/(n+1) else if(k==0).5/n else if(k==n)1-.5/n else k/n
    h <- rate(want[['hits']],want[['n_signal']]);f <- rate(want[['false_alarms']],want[['n_noise']])
    d <- qnorm(h)-qnorm(f);bias <- (qnorm(h)+qnorm(f))/2
    s <- filter(scores,participant_id==c$participant_id,condition_deadline_code==c$condition_deadline_code,actor==c$actor,correction==co)
    stopifnot(nrow(s)==1,s$assessable)
    near(s$hit_rate,h);near(s$false_alarm_rate,f);near(s$dprime,d);near(s$black_bias,bias);near(s$criterion_c,-bias)
    expected_scores[[length(expected_scores)+1]] <- mutate(c,correction=co,dprime=d,black_bias=bias)
  }
}
expected <- bind_rows(expected_scores)
# Check paired exports against independently reconstructed component scores.
for(kind in c("gains","benchmark")) {
  exported <- read(file.path(out,paste0("participant_aid_",kind,".csv")))
  stopifnot(nrow(exported)==4*n_people,
    !anyDuplicated(exported[c("participant_id","pressure","correction")]),
    !any(c("criterion_c","hit_rate","false_alarm_rate","hits")%in%names(exported)))
  for(i in seq_len(nrow(exported))) {
    r <- exported[i,]
    h <- filter(expected,participant_id==r$participant_id,pressure==r$pressure,correction==r$correction,
      actor=="human",mode=="Automation")
    b <- if(kind=="gains")filter(expected,participant_id==r$participant_id,pressure==r$pressure,
      correction==r$correction,actor=="human",mode=="Manual") else filter(expected,
      participant_id==r$participant_id,pressure==r$pressure,correction==r$correction,actor=="aid_matched")
    stopifnot(nrow(h)==1,nrow(b)==1)
    for(value in c("dprime","black_bias")) {
      near(r[[paste0("human_",value)]],h[[value]])
      near(r[[paste0(if(kind=="gains")"manual_" else "aid_",value)]],b[[value]])
      near(r[[value]],h[[value]]-b[[value]])
    }
  }
}
for(i in seq_len(nrow(context))) {
  c <- context[i,];r <- filter(raw,participant_id==c$participant_id,condition_deadline_code==c$condition_deadline_code)
  answered <- r$response!="TIMEOUT";ok <- r$response==r$stimulus
  values <- c(n_trials=nrow(r),n_answered=sum(answered),n_timeouts=sum(!answered),n_signal_all=sum(r$stimulus=="BLACK"),n_noise_all=sum(r$stimulus=="WHITE"),
    n_signal_answered=sum(r$stimulus=="BLACK" & answered),n_noise_answered=sum(r$stimulus=="WHITE" & answered),
    completion=mean(answered),all_trial_accuracy=mean(ok),answered_accuracy=mean(ok[answered]),
    black_completion=mean(answered[r$stimulus=="BLACK"]),white_completion=mean(answered[r$stimulus=="WHITE"]),
    n_nonpositive_answered_rt=sum(answered & is.finite(r$rt_s) & r$rt_s<=0),n_missing_answered_rt=sum(answered & !is.finite(r$rt_s)))
  for(k in names(values))near(c[[k]],values[[k]])
  near(c$all_trial_accuracy,c$completion*c$answered_accuracy)
}
# Independent participant-wide differences and direct t.test calls; no production contrast weights.
wide <- filter(expected,actor=="human") %>% select(participant_id,pattern,correction,condition_deadline_code,dprime,black_bias) %>%
  pivot_longer(c(dprime,black_bias),names_to="outcome",values_to="value") %>% pivot_wider(names_from=condition_deadline_code,values_from=value) %>%
  mutate(HP_gain=A_HP-M_HP,LP_gain=A_LP-M_LP)
matched <- inner_join(filter(expected,actor=="human",mode=="Automation"),
  filter(expected,actor=="aid_matched") %>% select(participant_id,pressure,correction,dprime,black_bias),
  by=c("participant_id","pressure","correction"),suffix=c("","_aid")) %>% mutate(dprime=dprime-dprime_aid,black_bias=black_bias-black_bias_aid)
for(i in seq_len(nrow(tests))) {
  t <- tests[i,];nm <- t$contrast
  d <- filter(wide,correction==t$correction,outcome==t$outcome)
  if(t$hypothesis!="H7") {
    if(startsWith(nm,"Difference in HP-minus-LP gains")) {
      x <- filter(d,pattern=="HP95_LP65") %>% transmute(v=HP_gain-LP_gain) %>% pull(v)
      y <- filter(d,pattern=="HP65_LP95") %>% transmute(v=HP_gain-LP_gain) %>% pull(v)
    } else if(grepl("aid gain: 95% - 65%",nm,fixed=TRUE)) {
      p <- substr(nm,1,2);g <- if(p=="HP")"HP95_LP65" else "HP65_LP95"
      x <- filter(d,pattern==g)[[paste0(p,"_gain")]];y <- filter(d,pattern!=g)[[paste0(p,"_gain")]]
    } else {
      g <- if(startsWith(nm,"HP95_LP65"))"HP95_LP65" else "HP65_LP95";d <- filter(d,pattern==g);y <- NULL
      if(grepl("Manual HP - Manual LP",nm,fixed=TRUE))x <- d$M_HP-d$M_LP else
      if(grepl("Aided HP - Manual LP",nm,fixed=TRUE))x <- d$A_HP-d$M_LP else
      if(grepl("HP gain - LP gain",nm,fixed=TRUE))x <- d$HP_gain-d$LP_gain else
      if(grepl(" HP Automation - Manual",nm,fixed=TRUE))x <- d$HP_gain else x <- d$LP_gain
    }
  } else {
    d <- filter(matched,correction==t$correction);d$v <- d[[t$outcome]]
    if(grepl("versus zero",nm,fixed=TRUE)) {
      x <- filter(d,pressure==substr(nm,1,2),reliability==substr(nm,4,6))$v;y <- NULL
    } else if(nm=="Reliability effect HP - LP") {
      sums <- d %>% group_by(participant_id,pattern) %>% summarise(v=sum(v),.groups="drop")
      x <- filter(sums,pattern=="HP95_LP65")$v;y <- filter(sums,pattern=="HP65_LP95")$v
    } else if(grepl("95% - 65%",nm,fixed=TRUE)) {
      x <- filter(d,pressure==substr(nm,1,2),reliability=="95%")$v;y <- filter(d,pressure==substr(nm,1,2),reliability=="65%")$v
    } else {
      x <- filter(d,reliability==substr(nm,1,3),pressure=="HP")$v;y <- filter(d,reliability==substr(nm,1,3),pressure=="LP")$v
    }
  }
  reference <- if(is.null(y))t.test(x) else t.test(x,y)
  near(t$estimate,if(is.null(y))mean(x) else mean(x)-mean(y));near(t$p_raw,reference$p.value)
  near(t$lower,reference$conf.int[1]);near(t$upper,reference$conf.int[2]);near(t$df,reference$parameter)
}
for(d in split(tests,interaction(tests$hypothesis,tests$outcome,tests$correction))) {
  stopifnot(nrow(d)==if(d$hypothesis[1]%in%c("H1","H3"))2 else 9)
  near(d$p_holm,p.adjust(d$p_raw,"holm"))
}
for(d in split(tests,tests$correction))near(d$p_holm_all,p.adjust(d$p_raw,"holm"))
sensitivity <- read(file.path(out,"correction_sensitivity.csv"));stopifnot(nrow(sensitivity)==44)
for(i in seq_len(nrow(sensitivity))) {
  s <- sensitivity[i,];d <- filter(tests,hypothesis==s$hypothesis,outcome==s$outcome,contrast==s$contrast)
  for(co in c("loglinear","extreme_only")) {
    near(s[[paste0("estimate_",co)]],filter(d,correction==co)$estimate)
    near(s[[paste0("p_holm_",co)]],filter(d,correction==co)$p_holm)
  }
  stopifnot(s$same_direction==(sign(s$estimate_loglinear)==sign(s$estimate_extreme_only)),
    s$same_holm_decision==((s$p_holm_loglinear<.05)==(s$p_holm_extreme_only<.05)))
}
# Plotted data and annotations must reproduce independently recomputed participant values.
catalog <- read(file.path(plots,"figure_catalog.csv"));stopifnot(nrow(catalog)==6)
for(stem in catalog$filename) {
  d <- read(file.path(plots,"data",paste0(stem,"_data.csv")))
  if(startsWith(stem,"04")) {
    reference <- filter(tests,correction=="loglinear",hypothesis%in%c("H1","H3"))
    reference <- reference[match(paste(d$outcome,d$contrast),paste(reference$outcome,reference$contrast)),]
    near(d$estimate,reference$estimate);near(d$lower,reference$lower);near(d$upper,reference$upper);near(d$p_holm,reference$p_holm)
  } else for(i in seq_len(nrow(d))) {
    r <- d[i,]
    if(startsWith(stem,"06")) {
      participant <- filter(context,pattern==r$pattern,pressure==r$pressure,
        if(r$condition=="Manual")mode=="Manual" else mode=="Automation" & reliability==substr(r$condition,1,3))
      values <- participant[[r$outcome]]
    } else if(startsWith(stem,"01") || startsWith(stem,"02")) {
      participant <- filter(expected,actor=="human",correction=="loglinear",pattern==r$pattern,pressure==r$pressure,
        if(r$condition=="Manual")mode=="Manual" else mode=="Automation" & reliability==substr(r$condition,1,3))
      values <- participant[[r$outcome]]
    } else if(startsWith(stem,"03")) {
      participant <- filter(wide,correction=="loglinear",outcome==r$outcome,pattern==r$pattern)
      values <- participant[[paste0(r$pressure,"_gain")]]
    } else {
      participant <- filter(matched,correction=="loglinear",pattern==r$pattern,pressure==r$pressure)
      values <- participant[[r$outcome]]
    }
    se <- sd(values)/sqrt(length(values));near(r$n,length(values));near(r$mean,mean(values));near(r$se,se)
    near(r$lower,mean(values)-qt(.975,length(values)-1)*se);near(r$upper,mean(values)+qt(.975,length(values)-1)*se)
    if("p_holm"%in%names(r))near(r$p_holm,filter(tests,correction=="loglinear",outcome==r$outcome,contrast==r$contrast)$p_holm)
  }
  annotation_path <- file.path(plots,"data",paste0(stem,"_annotations.csv"))
  if(file.exists(annotation_path)) {
    a <- read(annotation_path)
    for(i in seq_len(nrow(a))) {
      r <- a[i,];ref <- filter(tests,correction=="loglinear",outcome==r$outcome,contrast==r$contrast)
      near(r$estimate,ref$estimate);near(r$p_holm,ref$p_holm)
    }
  }
}
# Formula sign, label symmetry, extreme rates, and empty-category checks.
source(file.path(root,"analyse_time_pressure_task_sdt.R"),local=TRUE)
fixture <- tibble(n_signal=100,n_noise=100,hits=80,false_alarms=20)
s <- task_sdt_scores(fixture);near(s$black_bias,0);stopifnot(s$dprime>0)
swap <- task_sdt_scores(mutate(fixture,hits=100-false_alarms,false_alarms=100-fixture$hits))
near(s$dprime,swap$dprime);near(s$black_bias,-swap$black_bias)
bias <- task_sdt_scores(mutate(fixture,hits=90,false_alarms=40));stopifnot(bias$black_bias>0,bias$criterion_c<0)
perfect <- task_sdt_scores(mutate(fixture,hits=100,false_alarms=0));stopifnot(is.finite(perfect$dprime))
empty <- task_sdt_scores(tibble(n_signal=0,n_noise=100,hits=0,false_alarms=20));stopifnot(!empty$assessable,is.na(empty$dprime))
stopifnot(inherits(try(task_sdt_rates(101,100,"loglinear"),silent=TRUE),"try-error"))
for(path in c(file.path(out,"input_checksums.csv"),file.path(plots,"data/input_checksums.csv"))) {
  m <- read(path);stopifnot(identical(unname(tools::md5sum(m$path)),m$md5))
}
manifest <- read(file.path(plots,"data/artifact_manifest.csv"))
stopifnot(identical(unname(tools::md5sum(file.path(plots,manifest$path))),manifest$md5),
  all(file.info(file.path(plots,manifest$path))$size==manifest$bytes))
files <- list.files(plots,recursive=TRUE,full.names=TRUE);files <- files[!file.info(files)$isdir & basename(files)!="artifact_manifest.csv"]
stopifnot(setequal(substring(files,nchar(plots)+2),manifest$path))
pdfs <- list.files(plots,pattern="\\.pdf$",full.names=TRUE);pngs <- list.files(plots,pattern="\\.png$",full.names=TRUE)
stopifnot(length(pdfs)==7,length(pngs)==6)
for(path in pdfs) {
  info <- system2("pdfinfo",shQuote(path),stdout=TRUE)
  pages <- as.integer(sub("^Pages:\\s+","",grep("^Pages:",info,value=TRUE)))
  stopifnot(pages==if(basename(path)=="task_sdt_hypotheses.pdf")6 else 1)
}
for(path in pngs) {im <- png::readPNG(path,native=TRUE);stopifnot(identical(dim(im),c(2025L,3600L)));rm(im)}
html <- paste(readLines(file.path(plots,"index.html")),collapse="\n")
links <- regmatches(html,gregexpr('(?:href|src)="[^"]+"',html,perl=TRUE))[[1]]
links <- sub('^[^=]+="','',sub('"$','',links));stopifnot(all(file.exists(file.path(plots,links))))
cat("PASS:",n_people,"participants;",nrow(counts),"independently reconstructed confusion tables;",nrow(scores),"SDT scores;",nrow(tests),"direct t-test reconciliations; six figures and HTML links; input/artifact hashes.\n")
cat("Correction-sensitive Holm decisions:",sum(!sensitivity$same_holm_decision),"of",nrow(sensitivity),"\n")
