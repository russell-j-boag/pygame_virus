# Meaningful data-integrity and statistical-regression checks; never edits raw data.
# Usage: Rscript tests/test_dynamic_reliability_analysis.R
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1]))
root <- dirname(dirname(script))
source(file.path(root,"analyse_dynamic_reliability.R"))
near <- function(x,y,tol=1e-8) stopifnot(isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=tol)))
fails <- function(expr,pattern) {
  error <- tryCatch({force(expr); NULL},error=identity)
  stopifnot(inherits(error,"error"),grepl(pattern,conditionMessage(error)))
}
raw_path <- sort(list.files(file.path(root,"output/semester2_2026_data"),"_b00_ALL\\.csv$",full.names=TRUE))[1]
raw <- read_table(raw_path)
prepared <- prepare_trials(raw)
stopifnot(nrow(prepared)==1900,all(table(prepared$stage)==c(200,400,400,400,200)))
bad <- raw; bad$correct[1] <- !as.logical(bad$correct[1])
fails(prepare_trials(bad),"correctness disagrees")
fails(prepare_trials(bind_rows(raw,raw[1,])),"Duplicate trial keys")
bad <- raw; bad$run_timestamp <- "20990101_010101"
fails(prepare_trials(bind_rows(raw,bad)),"duplicate participant runs")
fails(prepare_trials(raw[-501,]),"Incomplete global")
bad <- raw; bad$reliability_phase_label[501] <- "P2_70"
fails(prepare_trials(bad),"phase metadata")
bad <- raw; bad$key_black[1] <- bad$key_white[1]
fails(prepare_trials(bad),"Allocation")
fails(binary(c("true","maybe"),"test"),"Invalid binary")

# Timeout accuracy and missing agreement are distinct from invalid RT handling.
bad <- raw; idx <- which(bad$block=="AUTOMATION")[1:3]
bad$response[idx[1]] <- "TIMEOUT"; bad$correct[idx[1]] <- FALSE; bad$rt_s[idx[1]] <- NA_real_
bad$response[idx[2:3]] <- bad$stimulus[idx[2:3]]; bad$correct[idx[2:3]] <- TRUE; bad$rt_s[idx[2:3]] <- c(0,-.01)
z <- prepare_trials(bad)
stopifnot(z$accuracy[idx[1]]==0,is.na(z$agreement[idx[1]]),!z$valid_correct_rt[idx[1]],
  all(z$accuracy[idx[2:3]]==1),all(!is.na(z$agreement[idx[2:3]])),all(!z$valid_correct_rt[idx[2:3]]))

# Trust provenance and completeness: exact same-run filenames are required.
scratch <- tempfile("dynamic-tests-"); dir.create(scratch)
paths <- c(raw_path,sub("_ALL.csv$","_POSTBLOCK_ALL.csv",raw_path),sub("_ALL.csv$","_POSTBLOCK_SLIDERS_ALL.csv",raw_path))
stopifnot(all(file.copy(paths,scratch)))
invisible(load_cohort(scratch))
q <- read_table(paths[2]); q$participant_id <- q$participant_id+1
write_csv(q,file.path(scratch,basename(paths[2])))
fails(load_cohort(scratch),"identity/design")
q <- read_table(paths[2]); write_csv(q[-1,],file.path(scratch,basename(paths[2])))
fails(load_cohort(scratch),"six-item")
write_csv(q,file.path(scratch,basename(paths[2])))
s <- read_table(paths[3]); s$question_key[3] <- "perc_self_correct"
write_csv(s,file.path(scratch,basename(paths[3])))
fails(load_cohort(scratch),"slider")
unlink(scratch,recursive=TRUE)

# Known paired effects with heterogeneous participant baselines, and a Welch interaction.
grid <- expand.grid(group=GROUPS,stage=STAGES,stringsAsFactors=FALSE)
weights <- performance_weights(grid)
stopifnot(length(weights)==18,all(vapply(weights,function(w)sum(w)==0,logical(1))))
changes <- c(-.10,.02,.04,.12,.03,.05,.15,.17)
people <- tibble(participant_id=1:8,group=rep(GROUPS,each=4),baseline=c(.5,.7,.6,.75,.8,.6,.7,.75),delta=changes)
cells <- merge(people,data.frame(stage=STAGES)) %>% mutate(value=baseline+ifelse(stage=="P2",delta,0))
res <- participant_results(cells,"value",grid,weights,"accuracy","core")
for(g in GROUPS) {
  t <- t.test(changes[people$group==g]); r <- filter(res,contrast==paste0(g,": P2 - Manual pre"))
  near(r$estimate,mean(changes[people$group==g])*100); near(r$lower,t$conf.int[1]*100); near(r$upper,t$conf.int[2]*100); near(r$p_raw,t$p.value)
}
t <- t.test(changes[5:8],changes[1:4]); r <- filter(res,contrast=="CAL90 - CAL65: P2 - Manual pre")
near(r$estimate,(mean(changes[5:8])-mean(changes[1:4]))*100); near(r$lower,t$conf.int[1]*100); near(r$upper,t$conf.int[2]*100); near(r$p_raw,t$p.value)
stopifnot(all(res$lower<=res$estimate & res$upper>=res$estimate))
# Missing cells exclude that participant from the specific contrast, rather than impute zero.
incomplete <- cells %>% filter(!(participant_id==1 & stage=="P2"))
r <- participant_results(incomplete,"value",grid,weights,"accuracy","core") %>% filter(contrast=="CAL65: P2 - Manual pre")
stopifnot(r$n_participants==3); near(r$estimate,mean(changes[2:4])*100)

# Independently known logistic probabilities: 25% versus 75% is 50 pp, not an odds ratio.
binary_fixture <- expand.grid(group=GROUPS,stage=STAGES,rep=1:40) %>%
  mutate(y=as.integer(rep<=ifelse(stage=="P2",30,10)))
fit <- list(model=glm(y~group*stage,data=binary_fixture,family=binomial()),diagnostics=tibble(inference_available=TRUE))
scratch <- tempfile("dynamic-contrasts-"); dir.create(scratch)
r <- model_results(fit,~group*stage,grid,weights,"accuracy","core",scratch)
near(filter(r,contrast=="CAL65: P2 - Manual pre")$estimate,50,1e-6)
unavailable <- fit; unavailable$diagnostics$inference_available <- FALSE
r <- model_results(unavailable,~group*stage,grid,weights,"accuracy","core",scratch)
stopifnot(all(is.na(r$p_raw)),all(is.na(r$lower)),all(is.na(r$upper)))
unavailable$model <- NULL
r <- model_results(unavailable,~group*stage,grid,weights,"accuracy","core",scratch)
stopifnot(nrow(r)==18,all(!r$inference_available),all(is.na(r$estimate)))
unlink(scratch,recursive=TRUE)

# Explicit multiplicity families and RT back-transformation.
d <- tibble(outcome=c("accuracy","accuracy","correct_rt"),family=c("core","core","speed"),method="Mixed model",
  estimate=c(1,2,log(.8)),lower=c(0,1,log(.7)),upper=c(2,3,log(.9)),p_raw=c(.01,.04,.03))
a <- adjust_results(d); near(a$p_holm,c(.02,.04,.03)); near(a$ratio[3],.8); near(a$ratio_lower[3],.7); near(a$ratio_upper[3],.9)
cat("PASS: data identity, sequence, outcome handling, paired/Welch contrasts, missing cells, response-scale probabilities, failed fits, Holm families and RT transforms.\n")
