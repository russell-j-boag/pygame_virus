# Independent statistical and artifact checks for the six presentation figures.
# Rscript tests/validate_dynamic_reliability_slides.R [analysis_dir] [plot_dir]
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})
script <- normalizePath(sub("^--file=","",grep("^--file=",commandArgs(),value = TRUE)[1]))
root <- dirname(dirname(script)); args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args)) args[1] else file.path(root,"analysis_outputs/semester2_2026_behavioural")
out <- file.path(if (length(args) > 1) args[2] else file.path(root,"plots/semester2_2026_behavioural"),"presentation")
read <- function(stem,base = input) read_csv(file.path(base,paste0(stem,".csv")),show_col_types = FALSE)
near <- function(x,y) stopifnot(isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance = 1e-9)))
stopifnot(file.exists(file.path(out,"COMPLETE.txt")))
for (name in c("input_checksums","plot_manifest")) {
  m <- read(name,out)
  stopifnot(all(file.exists(m$path)),identical(unname(tools::md5sum(m$path)),m$md5))
}
catalog <- read("figure_catalog",out)
stopifnot(identical(catalog$hypothesis,c("H1","H2","H3","H4","E1","E2")),
  identical(catalog$set,c(rep("Main",4),rep("Exploratory",2))),
  identical(catalog$source_hypothesis,c("H1","H2","H3","E3","E1","E2")),
  identical(catalog$source_set,c(rep("Main",3),rep("Exploratory",3))),
  all(grepl("^[0-9]{2}_[he][1234]_",catalog$filename)),all(catalog$width_in == 12),all(catalog$height_in == 6.75))
contrasts <- read("planned_contrasts"); subjective <- read("subjective_contrasts")
perf <- read("participant_performance"); agreement <- read("participant_agreement"); bins <- read("participant_trajectories")
ratings <- read("participant_ratings")
for (stem in catalog$filename) {
  s <- read(paste0(stem,"_data"),file.path(out,"data"))
  raw <- read(paste0(stem,"_participants"),file.path(out,"data"))
  a <- read(paste0(stem,"_annotations"),file.path(out,"data"))
  i <- match(stem,catalog$filename)
  stopifnot(nrow(a) > 0,all(a$inference_available),all(a$hypothesis == catalog$hypothesis[i]),
    all(a$set == catalog$set[i]),all(a$source_hypothesis == catalog$source_hypothesis[i]))
  # Exact saved tests, original Holm families and sensitivity disagreements.
  for (i in seq_len(nrow(a))) {
    z <- a[i,]
    if (z$family == "subjective") {
      m <- subjective %>% filter(outcome == z$outcome,contrast == z$contrast)
      stopifnot(nrow(m) == 1); near(z$model_p_holm,m$p_holm)
    } else {
      d <- contrasts %>% filter(outcome == z$outcome,family == z$family,contrast == z$contrast)
      m <- filter(d,method == "Mixed model"); p <- filter(d,method == "Participant sensitivity")
      stopifnot(nrow(m) == 1,nrow(p) == 1)
      near(z$model_p_holm,m$p_holm); near(z$participant_p_holm,p$p_holm)
      stopifnot(z$participant_disagreement == ((m$p_holm < .05) != (p$p_holm < .05)),
        grepl("\u2020",z$symbol,fixed = TRUE) == z$participant_disagreement)
    }
    stopifnot(grepl("*",z$symbol,fixed = TRUE) == (m$p_holm < .05))
  }
  for (i in seq_len(nrow(s))) {
    z <- s[i,]
    if (grepl("Morey",z$interval)) {
      keys <- strsplit(z$normalization_groups,";",fixed = TRUE)[[1]]
      levels <- strsplit(z$normalization_levels,";",fixed = TRUE)[[1]]
      d <- raw
      for (key in keys) d <- d[d[[key]] == z[[key]],]
      if ("panel" %in% names(raw)) d <- d[d$panel == z$panel,]
      # Matrix row centering independently checks all k=2/3/4/5 condition sets.
      mat <- as.matrix(pivot_wider(select(d,participant_id,level,value),names_from = level,values_from = value)[levels])
      stopifnot(!anyNA(mat),nrow(mat) == z$n,ncol(mat) == z$k)
      centered <- sweep(mat,1,rowMeans(mat))
      se <- sd(centered[,z$level])/sqrt(nrow(mat))*sqrt(ncol(mat)/(ncol(mat)-1))
      mean <- mean(mat[,z$level]); margin <- qt(.975,nrow(mat)-1)*se
      transform <- if (z$analysis_scale == "log RT") exp else identity
      near(z$value,transform(mean)); near(z$lo,transform(mean-margin)); near(z$hi,transform(mean+margin))
      near(z$se_analysis,se)
      # With two conditions Morey halfwidth is paired-effect halfwidth / sqrt(2).
      if (ncol(mat) == 2) near(margin,diff(t.test(mat[,2]-mat[,1])$conf.int)/2/sqrt(2))
    } else {
      if (stem == "04_h4_subjective_evaluation") {
        values <- ratings[[z$panel]][ratings$group == z$group]
      } else {
        d <- raw[raw$group == z$group & raw$panel == z$panel,]
        if ("phase" %in% names(z)) d <- d[d$phase == z$phase & d$level == z$level,]
        values <- d$value
      }
      stopifnot(length(values) == z$n)
      tt <- t.test(values); near(z$value,mean(values)); near(c(z$lo,z$hi),tt$conf.int)
    }
  }
  # PNG dimensions directly from its IHDR header; no image package required.
  png <- file(file.path(out,paste0(stem,".png")),"rb")
  header <- readBin(png,"raw",16)
  stopifnot(identical(as.integer(header[1:8]),c(137L,80L,78L,71L,13L,10L,26L,10L)),
    identical(readBin(png,"integer",2,size = 4,endian = "big"),c(3600L,2025L)))
  close(png)
}
# Check the plotted participant values against the source summaries, including
# change scores, log-RT scale, missing incorrect-advice bins and all 67 people.
for (stem in catalog$filename) {
  d <- read(paste0(stem,"_participants"),file.path(out,"data"))
  stopifnot(setequal(d$participant_id,perf$participant_id))
  if (grepl("h1_",stem)) {
    source <- agreement %>% filter(advice == "Incorrect",phase %in% c("P1","P2")) %>%
      transmute(participant_id,group,level = phase,value = 100*agreement)
  } else if (grepl("h2_",stem)) {
    source <- perf %>% filter(stage %in% c("Manual pre","P2")) %>%
      transmute(participant_id,group,level = stage,value = 100*accuracy)
  } else if (grepl("e1_",stem)) {
    z <- d %>% arrange(participant_id,outcome,phase,bin)
    source <- bins %>% arrange(participant_id,outcome,phase,bin)
    near(z$value,100*source$value); near(z$n_trials,source$n_trials)
    stopifnot(nrow(z) == nrow(source)); next
  } else if (grepl("h3_",stem)) {
    source <- bind_rows(perf %>% filter(stage %in% c("P1","P2","P3")) %>%
        transmute(participant_id,group,level = stage,value = 100*accuracy,panel = "Accuracy"),
      agreement %>% transmute(participant_id,group,level = phase,value = 100*agreement,panel = paste(advice,"advice"))) %>%
      arrange(participant_id,panel,level)
    z <- arrange(d,participant_id,panel,level); near(z$value,source$value); next
  } else if (grepl("e2_",stem)) {
    rt <- d %>% filter(panel == "Correct RT") %>% arrange(participant_id,level)
    source <- perf %>% arrange(participant_id,stage)
    near(rt$value,source$mean_log_rt)
    manual <- d %>% filter(panel == "Manual accuracy") %>% arrange(participant_id,level)
    near(manual$value,100*filter(source,stage %in% c("Manual pre","Manual post"))$accuracy); next
  } else if (grepl("h4_",stem)) {
    stopifnot(identical(names(d),names(ratings)),nrow(d) == nrow(ratings))
    for (column in names(d)) {
      if (is.numeric(d[[column]])) near(d[[column]],ratings[[column]]) else stopifnot(identical(d[[column]],ratings[[column]]))
    }
    next
  } else next
  means <- d %>% filter(panel == "Condition means") %>% arrange(participant_id,level)
  source <- source %>% arrange(participant_id,level); near(means$value,source$value)
  levels <- if (grepl("h1_",stem)) c("P1","P2") else c("Manual pre","P2")
  source <- source %>% pivot_wider(names_from = level,values_from = value) %>% arrange(participant_id)
  delta <- d %>% filter(panel == "Paired change") %>% arrange(participant_id)
  near(delta$value,source[[levels[2]]]-source[[levels[1]]])
}
lines <- read("04_h4_subjective_evaluation_association_lines",file.path(out,"data"))
for (v in unique(lines$panel)) {
  fit <- readRDS(file.path(input,"models",paste0(v,"_association.rds")))
  d <- filter(lines,panel == v); near(d$value,predict(fit,newdata = d))
}
pdfs <- c(paste0(catalog$filename,".pdf"),"primary_hypotheses.pdf","all_hypotheses.pdf")
for (i in seq_along(pdfs)) {
  info <- system2("pdfinfo",shQuote(file.path(out,pdfs[i])),stdout = TRUE)
  n <- c(rep(1,6),4,6)[i]
  stopifnot(any(grepl(paste0("^Pages:[[:space:]]+",n,"$"),info)),
    any(grepl("^Page size:[[:space:]]+864 x 486 pts",info)))
}
# Standalone plotting must also reject stale results before creating outputs.
for (kind in c("analysis_artifacts","input_manifest","source_checksums")) {
  tmp <- tempfile("dynamic-slide-stale-"); dir.create(tmp)
  file.copy(file.path(input,c("COMPLETE.txt",paste0(c("analysis_artifacts","input_manifest","source_checksums"),".csv"))),tmp)
  m <- read(kind); m$md5[1] <- paste(rep("0",32),collapse = "")
  write_csv(m,file.path(tmp,paste0(kind,".csv")))
  target <- file.path(tmp,"figures"); log <- file.path(tmp,"failure.log")
  status <- suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
    shQuote(c(file.path(root,"plot_dynamic_reliability_slides.R"),tmp,target)),stdout = log,stderr = log))
  stopifnot(status != 0,!dir.exists(target),any(grepl("Stale or changed",readLines(log))))
  unlink(tmp,recursive = TRUE)
}
cat("PASS: six hypotheses, source reconciliation, Morey/ordinary CIs, saved Holm annotations, sensitivity flags, log RT, association lines, provenance rejection and 16:9 PNG/PDF dimensions.\n")
