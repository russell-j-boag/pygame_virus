# Scientific validation of empirical presentation figures and their annotations.
# Run from the repository root after Rscript plot_time_pressure_slides.R.
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})
source("plot_time_pressure_slides.R")
base <- "analysis_outputs/semester2_2026_behavioural"
out <- "plots/semester2_2026_behavioural/presentation"
read <- function(name, exploratory = FALSE) read_csv(file.path(
  if (exploratory) file.path(base, "exploratory") else base, paste0(name, ".csv")), show_col_types = FALSE)
catalog <- read_csv(file.path(out, "figure_catalog.csv"), show_col_types = FALSE)
stopifnot(nrow(catalog) == 12, !anyDuplicated(catalog$filename),
  identical(catalog$set, c(rep("Main",9),rep("Exploratory",3))), identical(catalog$hypothesis[10:12],paste0("H",3:5)),
  identical(catalog$hypothesis[1:9],paste0("H",1:9)),
  identical(catalog$source_set,c(rep("Main",6),rep("Exploratory",6))),
  identical(catalog$source_hypothesis,c(paste0("H",1:6),"H2","H6","H1","H3","H4","H5")),
  all(catalog$analysis_origin[7:12] == "Specified after inspecting the main results"),
  identical(catalog$filename[7:12],c("07_main_H7_aid_benchmark","08_main_H8_accuracy_components","09_main_H9_selectivity",
    "10_exploratory_H3_reliability_awareness","11_exploratory_H4_trust_association","12_exploratory_H5_error_adjustment")),
  all(grepl("^[0-9]{2}_(main|exploratory)_H[1-9]_",catalog$filename)),
  all(catalog$width_in/catalog$height_in == 16/9))
plotted <- function(i, kind = "data") read_csv(file.path(out,"data",paste0(catalog$filename[i],"_",kind,".csv")),show_col_types = FALSE)
near <- function(x,y) stopifnot(length(x) == length(y), length(x) > 0, all(is.finite(x)),all(is.finite(y)),max(abs(x-y)) < 1e-9)
perf <- read("participant_performance")
add_outcomes <- function(d) bind_rows(d %>% mutate(outcome = "Accuracy (%)",value = 100*accuracy),
  d %>% mutate(outcome = "Correct RT (s)",value = mean_log_correct_rt))

# Independent matrix calculation, from source participant scores, rather than
# calling the production Morey function to generate expected results.
check_morey <- function(i, raw, groups, levels) {
  shown <- plotted(i); inputs <- plotted(i,"participants")
  keys <- c(groups,"participant_id","level")
  expected_inputs <- raw %>% select(all_of(keys),expected_value = value)
  joined <- left_join(inputs,expected_inputs,by = keys)
  stopifnot(nrow(joined) == nrow(raw), !anyDuplicated(raw[keys]))
  near(joined$analysis_value,joined$expected_value)
  split_key <- do.call(interaction,c(raw[groups],list(drop = TRUE)))
  for (x in split(raw,split_key)) {
    k <- length(levels)
    stopifnot(k > 1)
    mat <- t(vapply(split(x,x$participant_id),function(z) {
      stopifnot(nrow(z) == k,setequal(z$level,levels))
      z$value[match(levels,z$level)]
    },numeric(k)))
    norm <- sweep(mat,1,rowMeans(mat),"-")+mean(mat)
    center <- colMeans(mat)
    se <- apply(norm,2,sd)/sqrt(nrow(mat))*sqrt(k/(k-1))
    margin <- qt(.975,nrow(mat)-1)*se
    matches <- rep(TRUE,nrow(shown))
    for (g in groups) matches <- matches & shown[[g]] == x[[g]][1]
    p <- shown[matches,]; p <- p[match(levels,p$level),]
    log_scale <- "outcome" %in% groups && x$outcome[1] == "Correct RT (s)"
    transform <- if (log_scale) exp else identity
    near(p$value,transform(center)); near(p$lo,transform(center-margin)); near(p$hi,transform(center+margin))
    near(p$se_analysis,se)
    stopifnot(all(p$n == nrow(mat)),all(p$k == k),all(p$interval == "Cousineau-Morey within-participant 95% CI"),
      all(p$normalization_groups == paste(groups,collapse = ";")),all(p$normalization_levels == paste(levels,collapse = ";")))
    near(p$morey_factor,rep(sqrt(k/(k-1)),k))
    # The two-condition bars have half the variance of the raw difference CI.
    if (k == 2) near(p$se_analysis,rep(sd(mat[,2]-mat[,1])/sqrt(nrow(mat))/sqrt(2),2))
  }
  stopifnot(all(shown$series == "Observed"))
}
check_morey(1,perf %>% filter(mode == "Manual") %>% mutate(level = pressure) %>% add_outcomes(),
  c("pattern","outcome"),c("LP","HP"))
check_morey(2,perf %>% mutate(level = paste(mode,pressure)) %>% add_outcomes() %>% filter(outcome == "Accuracy (%)"),
  c("pattern","outcome"),c("Manual HP","Automation HP","Manual LP","Automation LP"))
check_morey(4,perf %>% mutate(level = paste(mode,pressure)) %>% add_outcomes() %>% filter(outcome == "Correct RT (s)"),
  c("pattern","outcome"),c("Manual HP","Automation HP","Manual LP","Automation LP"))
check_morey(3,perf %>% filter((mode == "Manual" & pressure == "LP") | (mode == "Automation" & pressure == "HP")) %>%
  mutate(level = ifelse(mode == "Manual","Manual LP","Aided HP"),value = 100*accuracy),"pattern",c("Manual LP","Aided HP"))
benchmark <- read("H2_participant_benchmark",TRUE) %>% mutate(Human = 100*human_accuracy,Aid = 100*aid_accuracy) %>%
  pivot_longer(c(Human,Aid),names_to = "level",values_to = "value")
check_morey(7,benchmark,c("pattern","pressure","reliability"),c("Aid","Human"))
ratings <- read("H3_participant_calibration",TRUE) %>% mutate(Perceived = perceived,Realised = 100*aid_accuracy) %>%
  pivot_longer(c(Perceived,Realised),names_to = "level",values_to = "value")
check_morey(10,ratings,c("pattern","pressure","reliability"),c("Perceived","Realised"))
lag_raw <- read("H5_participant_sequence",TRUE) %>% mutate(level = previous_aid_error,value = 100*agreement)
check_morey(12,lag_raw,c("pattern","pressure","reliability"),c("Correct","Error"))

# Strict complete panels and participant-offset invariance protect normalization.
fixture <- expand_grid(pattern = c("A","B"),participant_id = 1:5,level = c("one","two")) %>%
  mutate(value = participant_id*2+ifelse(level == "two",participant_id/3,0)+ifelse(pattern == "B",30,0))
a <- time_pressure_morey(fixture,"pattern",c("one","two"))$summary
b <- time_pressure_morey(mutate(fixture,value = value+100*participant_id),"pattern",c("one","two"))$summary
near(a$se_analysis,b$se_analysis)
stopifnot(inherits(try(time_pressure_morey(fixture[-1,],"pattern",c("one","two")),silent = TRUE),"try-error"),
  inherits(try(time_pressure_morey(bind_rows(fixture,fixture[1,]),"pattern",c("one","two")),silent = TRUE),"try-error"))

check_ordinary <- function(i,raw,keys,interval) {
  expected <- raw %>% group_by(across(all_of(keys))) %>% group_modify(~{
    tt <- t.test(.x$value)
    tibble(expected_mean = unname(tt$estimate),expected_lo = tt$conf.int[1],expected_hi = tt$conf.int[2],expected_n = nrow(.x))
  }) %>% ungroup()
  d <- left_join(plotted(i),expected,by = keys)
  stopifnot(nrow(d) == nrow(expected),all(d$interval == interval))
  near(d$value,d$expected_mean); near(d$lo,d$expected_lo); near(d$hi,d$expected_hi); near(d$n,d$expected_n)
}
keys <- c("pattern","pressure","reliability")
rel <- read("participant_reliance") %>% mutate(panel = ifelse(advice == "Correct","Correct advice","Incorrect advice"),value = 100*agreement)
trust <- read("participant_trust") %>% mutate(panel = "Trust",value = trust)
check_ordinary(5,rel,c(keys,"panel"),"Ordinary participant mean 95% t CI")
check_ordinary(6,trust,c(keys,"panel"),"Ordinary participant mean 95% t CI")
# Original main H1-H6 retain their individual outcomes; promoted H7-H9 retain source provenance.
stopifnot(all(plotted(2)$outcome == "Accuracy (%)"),all(plotted(4)$outcome == "Correct RT (s)"),
  setequal(plotted(5)$panel,c("Correct advice","Incorrect advice")),all(plotted(6)$panel == "Trust"),
  identical(catalog$filename[1:6],c("01_main_H1_manual_pressure","02_main_H2_accuracy_benefits",
    "03_main_H3_compensation","04_main_H4_response_speed","05_main_H5_behavioural_reliance","06_main_H6_trust")))
sel <- read("H1_participant_selectivity",TRUE) %>% mutate(value = 100*selectivity)
check_ordinary(9,sel,keys,"Paired-effect 95% t CI")
dec <- read("H6_participant_decomposition",TRUE) %>%
  pivot_longer(c(completion_component,choice_component,total_gain),names_to = "component",values_to = "value") %>% mutate(value = 100*value)
check_ordinary(8,dec,c(keys,"component"),"Paired-effect 95% t CI")
accounting <- plotted(8) %>% select(all_of(keys),component,value) %>% pivot_wider(names_from = component,values_from = value)
near(accounting$total_gain,accounting$completion_component+accounting$choice_component)

# Trust change is independently reconstructed from uncentred six-item scores.
trust_change <- trust %>% select(participant_id,pattern,pressure,trust) %>% pivot_wider(names_from = pressure,values_from = trust) %>%
  transmute(participant_id,pattern,expected_trust = HP-LP)
agreement_change <- rel %>% select(participant_id,pattern,pressure,advice,agreement) %>%
  pivot_wider(names_from = pressure,values_from = agreement) %>% mutate(expected_agreement = 100*(HP-LP))
checked <- plotted(11) %>% left_join(trust_change,by = c("participant_id","pattern")) %>%
  left_join(agreement_change,by = c("participant_id","pattern","advice"))
stopifnot(nrow(checked) == 2*n_distinct(perf$participant_id),all(checked$interval == "None: individual paired changes"),
  !file.exists(file.path(out,"data",paste0(catalog$filename[11],"_annotations.csv"))))
near(checked$trust_change,checked$expected_trust); near(checked$value,checked$expected_agreement)

# Each saved test annotation must match the exact hypothesis/outcome/contrast;
# participant disagreement uses the saved Holm family, not CI overlap.
model <- read("planned_contrasts"); sensitivity <- read("participant_sensitivity_contrasts")
extra <- read("exploratory_contrasts",TRUE)
expected_counts <- c(4,6,2,6,10,5,4,4,2,6,0,4)
for (i in 1:10) {
  d <- plotted(i,"annotations")
  stopifnot(nrow(d) == expected_counts[i])
  stopifnot(all(d$hypothesis == catalog$hypothesis[i]),all(d$set == catalog$set[i]))
  h <- unique(d$test_family)
  m <- if (h == "Main") model else filter(extra,hypothesis == h,analysis == "mixed_model")
  o <- if (h == "Main") sensitivity else filter(extra,hypothesis == h,analysis != "mixed_model")
  j <- d %>% left_join(m %>% select(test_outcome = outcome,contrast,expected_p = p_holm),by = c("test_outcome","contrast")) %>%
    left_join(o %>% select(test_outcome = outcome,contrast,expected_participant_p = p_holm),by = c("test_outcome","contrast"))
  near(j$model_p_holm,j$expected_p); near(j$p_holm,j$expected_p); near(j$participant_p_holm,j$expected_participant_p)
  expected_disagreement <- (j$expected_p < .05) != (j$expected_participant_p < .05)
  symbols <- paste0(ifelse(j$expected_p < .05,"*","ns"),ifelse(expected_disagreement,"\u2020",""))
  stopifnot(all(j$participant_disagreement == expected_disagreement),all(sub("^.*: ","",j$symbol) == symbols))
  if (i != 8) stopifnot(all(j$x1 < j$x2),all(is.finite(j$text_y)),all(j$annotation == "Bracket"))
}
# Check bracket endpoint/facet mappings, beyond checking the test values alone.
for (i in c(2,4)) {
  a <- plotted(i,"annotations")
  simple <- filter(a,comparison == "Aided minus Manual")
  interaction <- filter(a,comparison == "HP effect minus LP effect")
  stopifnot(nrow(simple) == 4,nrow(interaction) == 2,
    all(simple$contrast == paste(simple$pattern,simple$pressure,"Automation - Manual")),
    all(simple$x1 == ifelse(simple$pressure == "HP",1,3)),all(simple$x2 == simple$x1+1),
    all(interaction$contrast == paste(interaction$pattern,"HP gain - LP gain")),
    all(interaction$x1 == 1.5 & interaction$x2 == 3.5),
    all(a$test_outcome == if (i == 2) "accuracy" else "correct_rt"))
}
# H2 explicitly tests the stated gain pattern and its direction in both groups.
h2 <- filter(sensitivity,outcome == "accuracy",contrast %in% paste(c("HP95_LP65","HP65_LP95"),"HP gain - LP gain"))
stopifnot(nrow(h2) == 2,all(h2$p_holm < .05),
  h2$estimate[grepl("^HP95",h2$contrast)] > 0,h2$estimate[grepl("^HP65",h2$contrast)] < 0)
a <- plotted(3,"annotations");stopifnot(all(a$contrast == paste(a$pattern,"Aided HP - Manual LP")),all(a$x1 == 1 & a$x2 == 2))
for (i in c(5,6)) {
  a <- plotted(i,"annotations")
  a <- a %>% mutate(prefix = ifelse(panel == "Trust","Trust",sub(" advice$","",panel)))
  reliability <- filter(a,comparison == "Reliability at fixed pressure")
  pressure <- filter(a,comparison == "Pressure at fixed reliability")
  interaction <- filter(a,comparison == "Reliability by pressure interaction")
  stopifnot(nrow(reliability) == if (i == 5) 4 else 2,nrow(pressure) == nrow(reliability),
    nrow(interaction) == if (i == 5) 2 else 1,
    all(reliability$contrast == paste(reliability$prefix,reliability$pressure,"95% - 65%")),
    all(pressure$contrast == paste(pressure$prefix,pressure$reliability,"HP - LP")),
    all(interaction$contrast == paste(interaction$prefix,"Reliability effect HP - LP")))
  near(reliability$x1,ifelse(reliability$pressure == "HP",1,2)-.0875);near(reliability$x2,reliability$x1+.175)
  near(pressure$x1,1+ifelse(pressure$reliability == "65%",-.0875,.0875));near(pressure$x2,pressure$x1+1)
}
a <- plotted(10,"annotations")
signed <- filter(a,test_outcome == "signed_error"); perceived <- filter(a,test_outcome == "perceived")
stopifnot(nrow(signed) == 4,nrow(perceived) == 2,
  all(signed$contrast == paste(signed$pressure,signed$reliability,"versus zero")),
  all(perceived$contrast == paste(perceived$pressure,"95% - 65%")))
near(signed$x1,ifelse(signed$reliability == "65%",1,2)-.0875);near(perceived$x1,rep(1-.0875,2))

# Exploratory H5 displays UNADJUSTED observed history means. Its new tests must be exactly
# paired across histories, equal-weight participants, Holm over all four cells.
pairs <- lag_raw %>% select(participant_id,pattern,pressure,reliability,level,value) %>% pivot_wider(names_from = level,values_from = value)
expected <- pairs %>% group_by(pattern,pressure,reliability) %>% group_modify(~{
  tt <- t.test(.x$Error,.x$Correct,paired = TRUE)
  tibble(expected_estimate = unname(tt$estimate),expected_lo = tt$conf.int[1],expected_hi = tt$conf.int[2],expected_p_raw = tt$p.value)
}) %>% ungroup() %>% mutate(expected_p_holm = p.adjust(expected_p_raw,"holm"))
history <- plotted(12,"annotations") %>% left_join(expected,by = keys)
stopifnot(nrow(history) == 4,all(history$x1 == 1 & history$x2 == 2),
  all(grepl("Observed paired t test",history$test_source)),all(history$symbol == ifelse(history$expected_p_holm < .05,"*","ns")))
near(history$estimate,history$expected_estimate);near(history$lower,history$expected_lo);near(history$upper,history$expected_hi)
near(history$p_raw,history$expected_p_raw);near(history$p_holm,history$expected_p_holm)

# Export dimensions, finite estimates and independently hashed provenance.
expected_exports <- c("all_hypotheses.pdf","primary_hypotheses.pdf",paste0(catalog$filename,".pdf"),paste0(catalog$filename,".png"))
stopifnot(setequal(list.files(out,pattern = "\\.(pdf|png)$"),expected_exports))
for (i in 1:12) {
  d <- plotted(i)
  stopifnot(all(d$series == "Observed"),all(is.finite(d$value)))
  if (i != 11) stopifnot(all(is.finite(d$lo)),all(is.finite(d$hi)),all(d$lo <= d$value & d$value <= d$hi))
  f <- file(file.path(out,paste0(catalog$filename[i],".png")),"rb")
  signature <- readBin(f,"raw",n = 16); dims <- readBin(f,"integer",n = 2,size = 4,endian = "big");close(f)
  stopifnot(identical(as.integer(signature[1:8]),c(137L,80L,78L,71L,13L,10L,26L,10L)),identical(dims,c(3600L,2025L)))
}
for (name in c("input_checksums","artifact_manifest")) {
  h <- read_csv(file.path(out,"data",paste0(name,".csv")),show_col_types = FALSE)
  paths <- if (name == "input_checksums") h$path else file.path(out,h$file)
  stopifnot(identical(unname(tools::md5sum(paths)),h$md5))
  if (name == "artifact_manifest") stopifnot(nrow(h) == 26,all(h$bytes > 0))
}
receipt <- c("All 12 empirical presentation figures and 53 significance annotations passed validation.",
  "Passed: source participant values, independent matrix Morey calculations, complete-panel checks, participant-offset invariance, log-RT back-transformation, between-group and paired-effect t CIs, raw trust changes, exact accounting identities, annotation endpoints and source tests, participant-disagreement flags, independent unadjusted H5 paired/Holm tests, 16:9 PNG dimensions and provenance hashes.")
writeLines(receipt,file.path(out,"data/validation.txt"));cat(paste(receipt,collapse = "\n"),"\n")
