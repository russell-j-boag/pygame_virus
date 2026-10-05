# Independent numerical reconciliation of a completed live analysis and figure bundle.
# Usage: Rscript tests/validate_dynamic_reliability_outputs.R [analysis_dir] [plot_dir]
suppressPackageStartupMessages({library(readr); library(dplyr); library(tidyr)})
args <- commandArgs(trailingOnly=TRUE)
script <- normalizePath(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])); root <- dirname(dirname(script))
input <- if(length(args)) args[1] else file.path(root,"analysis_outputs/semester2_2026_behavioural")
plots <- if(length(args)>1) args[2] else file.path(root,"plots/semester2_2026_behavioural")
read <- function(name) read_csv(file.path(input,paste0(name,".csv")),show_col_types=FALSE)
near <- function(x,y,tol=1e-9) stopifnot(isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=tol)))
stopifnot(file.exists(file.path(input,"COMPLETE.txt")),file.exists(file.path(plots,"COMPLETE.txt")))
for(name in c("input_manifest","source_checksums","analysis_artifacts")) {
  m <- read(name); stopifnot(all(file.exists(m$path)),identical(unname(tools::md5sum(m$path)),m$md5))
}
m <- read("input_manifest")
raw <- bind_rows(lapply(m$path[m$kind=="trials"],read_csv,show_col_types=FALSE))
raw <- raw %>% mutate(stage=case_when(block_idx==2~"Manual pre",block_idx==4~"Manual post",block_idx==3~paste0("P",reliability_phase_idx)),
  accuracy=as.integer(response==stimulus),group=calibration_target_group)
p <- read("participant_performance") %>% arrange(participant_id,stage)
independent <- raw %>% filter(!is.na(stage)) %>% group_by(participant_id,stage) %>% summarise(n=n(),
  rt=mean(rt_s[accuracy==1 & is.finite(rt_s) & rt_s>0]),accuracy=mean(accuracy),.groups="drop") %>% arrange(participant_id,stage)
near(p$n,independent$n); near(p$accuracy,independent$accuracy); near(p$mean_correct_rt,independent$rt)
stopifnot(sum(p$n)==1600*n_distinct(raw$participant_id))
a <- read("participant_agreement") %>% arrange(participant_id,phase,advice)
ia <- raw %>% filter(block_idx==3,response!="TIMEOUT") %>% mutate(phase=paste0("P",reliability_phase_idx),
  advice=ifelse(aid_label==stimulus,"Correct","Incorrect")) %>% group_by(participant_id,phase,advice) %>%
  summarise(n=n(),agreement=mean(response==aid_label),.groups="drop") %>% arrange(participant_id,phase,advice)
near(a$n,ia$n); near(a$agreement,ia$agreement)
c <- read("planned_contrasts")
stopifnot(all(c$lower[c$inference_available]<=c$estimate[c$inference_available]+1e-10),
  all(c$upper[c$inference_available]>=c$estimate[c$inference_available]-1e-10))
for(k in unique(paste(c$outcome,c$family,c$method))) {
  x <- c[paste(c$outcome,c$family,c$method)==k,]; near(x$p_holm,p.adjust(x$p_raw,"holm",n=nrow(x)))
}
# Reconcile all core accuracy participant contrasts with independently computed paired/Welch t tests.
wide <- p %>% select(participant_id,group,stage,accuracy) %>% pivot_wider(names_from=stage,values_from=accuracy)
for(i in which(c$outcome=="accuracy" & c$family=="core" & c$method=="Participant sensitivity")) {
  parts <- strsplit(c$contrast[i],": ",fixed=TRUE)[[1]]; pair <- strsplit(parts[2]," - ",fixed=TRUE)[[1]]
  delta <- wide[[pair[1]]]-wide[[pair[2]]]
  t <- if(parts[1]=="CAL90 - CAL65") t.test(delta[wide$group=="CAL90"],delta[wide$group=="CAL65"]) else t.test(delta[wide$group==parts[1]])
  near(c$lower[i],t$conf.int[1]*100); near(c$upper[i],t$conf.int[2]*100); near(c$p_raw[i],t$p.value)
}
for(advice_type in c("Correct","Incorrect")) {
  wide <- a %>% filter(advice==advice_type) %>% select(participant_id,group,phase,agreement) %>%
    pivot_wider(names_from=phase,values_from=agreement)
  for(i in which(c$outcome=="agreement" & c$family=="core" & c$method=="Participant sensitivity" & startsWith(c$contrast,paste0(advice_type,":")))) {
    parts <- strsplit(c$contrast[i],": ",fixed=TRUE)[[1]]; pair <- strsplit(parts[3]," - ",fixed=TRUE)[[1]]
    delta <- wide[[pair[1]]]-wide[[pair[2]]]
    t <- if(parts[2]=="CAL90 - CAL65") t.test(delta[wide$group=="CAL90"],delta[wide$group=="CAL65"]) else t.test(delta[wide$group==parts[2]])
    near(c$lower[i],t$conf.int[1]*100); near(c$upper[i],t$conf.int[2]*100); near(c$p_raw[i],t$p.value)
  }
}
for(v in c("accuracy","agreement")) {
  fit <- readRDS(file.path(input,"models",paste0(v,"_trend.rds")))
  curves <- read(paste0(v,"_trend_curves")); curves$subject <- levels(model.frame(fit)$subject)[1]
  near(curves$prob,predict(fit,newdata=curves,type="response",re.form=NA),1e-7)
}
trust <- bind_rows(lapply(m$path[m$kind=="trust"],read_csv,show_col_types=FALSE)) %>%
  group_by(participant_id) %>% summarise(trust=mean(response),.groups="drop") %>% arrange(participant_id)
near(read("participant_ratings") %>% arrange(participant_id) %>% pull(trust),trust$trust)
pm <- read_csv(file.path(plots,"plot_manifest.csv"),show_col_types=FALSE)
stopifnot(all(file.exists(pm$path)),identical(unname(tools::md5sum(pm$path)),pm$md5))
sources <- read_csv(file.path(plots,"plot_sources.csv"),show_col_types=FALSE)
stopifnot(identical(unname(tools::md5sum(sources$path)),sources$md5))
pdfs <- list.files(plots,"\\.pdf$",full.names=TRUE); pngs <- list.files(plots,"\\.png$",full.names=TRUE)
stopifnot(length(pdfs)==9,length(pngs)==9,all(file.info(c(pdfs,pngs))$size>1000))
for(path in pdfs) {
  info <- system2("pdfinfo",shQuote(path),stdout=TRUE)
  stopifnot(any(grepl("^Pages:[[:space:]]+1$",info)))
}
for(name in c("accuracy_benefit_contrasts","agreement_drop_contrasts","recovery_contrasts","rt_benefit_contrasts","manual_change_contrasts","adaptation_contrasts")) {
  x <- read_csv(file.path(plots,"data",paste0(name,".csv")),show_col_types=FALSE)
  ix <- match(paste(x$outcome,x$family,x$method,x$contrast),paste(c$outcome,c$family,c$method,c$contrast))
  stopifnot(!anyNA(ix)); near(x$estimate,c$estimate[ix]); near(x$lower,c$lower[ix]); near(x$upper,c$upper[ix])
}
# Negative integration checks: reject each kind of changed provenance before writing figures.
for(kind in c("analysis_artifacts","input_manifest","source_checksums")) {
  tmp <- tempfile("dynamic-stale-"); dir.create(tmp)
  file.copy(file.path(input,c("COMPLETE.txt",paste0(c("analysis_artifacts","input_manifest","source_checksums"),".csv"))),tmp)
  altered <- read(kind); altered$md5[1] <- paste(rep("0",32),collapse="")
  write_csv(altered,file.path(tmp,paste0(kind,".csv")))
  logfile <- file.path(tmp,"failure.log"); target <- file.path(tmp,"figures")
  status <- suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
    shQuote(c(file.path(root,"plot_dynamic_reliability_results.R"),tmp,target)),stdout=logfile,stderr=logfile))
  stopifnot(status!=0,!dir.exists(target),any(grepl("Stale or changed",readLines(logfile))))
  unlink(tmp,recursive=TRUE)
}
cat("PASS: provenance and stale-input rejection, independent raw summaries, all core participant tests, predicted curves, trust, figure data and nine one-page PDFs.\n")
