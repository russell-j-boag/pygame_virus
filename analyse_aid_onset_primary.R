# Rscript analyse_aid_onset_primary.R [analysis_dir]
# Rebuilds all revised primary/supporting tests without participant exclusions.
script <- sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
ROOT <- dirname(normalizePath(script))
source(file.path(ROOT,"R/aid_onset_primary.R"))
args <- commandArgs(trailingOnly=TRUE)
ao_check(length(args)<=1,"Usage: Rscript analyse_aid_onset_primary.R [analysis_dir]")
OUT <- if(length(args)) args[1] else file.path(ROOT,"analysis_outputs/semester2_2026_hypotheses/primary")
dir.create(dirname(OUT),recursive=TRUE,showWarnings=FALSE)
stage <- tempfile(".primary_build_",tmpdir=dirname(OUT))
for(dir in c("models","model_data","diagnostics")) dir.create(file.path(stage,dir),recursive=TRUE)
run <- function() {
  on.exit(if(dir.exists(stage)) unlink(stage,recursive=TRUE))
  p <- ao_prepare(ROOT)
  inputs <- ao_hashes(c(p$inputs,file.path(ROOT,c("R/aid_onset_primary.R","analyse_aid_onset_primary.R","config/hypothesis_figures.csv"))))
  write_csv(inputs,file.path(stage,"input_checksums.csv"))
  for(name in c("selection","manifest","counts","scores","matched_scores","denominators")) write_csv(p[[name]],file.path(stage,paste0(name,".csv")))
  all <- list()
  for(cor in c("loglinear","extreme_only")) {
    specs <- ao_specs(p,cor)
    for(spec in specs) all[[paste(spec$outcome,cor)]] <- ao_fit(spec,cor,stage)
  }
  tests <- ao_holm(bind_rows(lapply(all,`[[`,"tests")))
  paired <- ao_holm(bind_rows(lapply(all,`[[`,"paired")))
  ao_check(nrow(filter(tests,primary,correction=="loglinear"))==28,"Expected 28 unique primary comparisons")
  write_csv(tests,file.path(stage,"contrasts.csv"));write_csv(paired,file.path(stage,"participant_sensitivity.csv"))
  for(name in c("means","raw","diagnostics","weights")) write_csv(bind_rows(lapply(all,`[[`,name)),file.path(stage,paste0(name,".csv")))
  legacy <- bind_rows(tests,paired) %>% filter(correction=="loglinear",outcome %in% c("advice_bias","advice_dprime","beneficial","harmful")) %>%
    mutate(family=ifelse(outcome %in% c("advice_bias","advice_dprime"),"legacy_joint_advice_SDT","legacy_joint_revision_quality")) %>% ao_holm()
  write_csv(legacy,file.path(stage,"historical_family_sensitivity.csv"))
  uptake <- p$trials %>% filter(condition=="Stimulus-first",paired,!agree1) %>% group_by(participant_id,advice_correct) %>%
    summarise(n_opportunities=n(),n_followed=sum(agree2),.groups="drop")
  write_csv(uptake,file.path(stage,"selective_uptake_denominators.csv"))
  support <- p$trials %>% filter(paired) %>% group_by(participant_id,condition) %>%
    summarise(D1=100*mean(correct1),D2=100*mean(correct2),.groups="drop") %>%
    pivot_longer(c(D1,D2),names_to="decision",values_to="value") %>% mutate(outcome="accuracy")
  support <- bind_rows(support,transmute(p$matched_scores,participant_id,condition,decision,outcome="task_dprime",value=dprime),
    transmute(p$matched_scores,participant_id,condition,decision,outcome="task_bias",value=bias))
  write_csv(support,file.path(stage,"decision_context.csv"))
  file.copy(file.path(ROOT,"config/hypothesis_figures.csv"),file.path(stage,"hypothesis_figures.csv"))
  writeLines(capture.output(sessionInfo()),file.path(stage,"session_info.txt"))
  writeLines(c("Retrospective reorganisation after inspection of prior results; not preregistered.",
    "All 60 canonical participants included. No participant QC policy is read or applied.",
    "28 unique primary comparisons; H4 task discrimination reuses the H1 task-dprime timing test.",
    "All tests two-sided. Holm separately within hypothesis/outcome; sensitivities separate.",
    "Binary models: participant random intercept, condition fixed effect, response-scale differences in percentage points.",
    "Binary means are conditional predictions at zero participant random effect, not population-marginal probabilities.",
    "Linear models: participant random intercept, Satterthwaite degrees of freedom. Cell and contrast CIs pointwise 95%.",
    "Singular/nonconverged/rank-deficient models have unavailable primary inference; paired results never substitute.",
    "Task H=BLACK|BLACK and F=BLACK|WHITE. Advice H=agree|correct advice and F=agree|incorrect advice.",
    "Loglinear correction on all cells; extreme-only correction is sensitivity. Positive task -c means BLACK preference.",
    "All answered rows enter SDT regardless of RT. Timeouts count incorrect in accuracy and are omitted from SDT.",
    "Human-aid accuracy compares all-trial human accuracy with realised advice correctness, assuming timely aid response.",
    "Human-aid task dprime uses identical human-final-answered rows for both agents.",
    "Revisions and D1-D2 context use valid response pairs; denominators exported.",
    "Aid-first D1 already includes advice exposure. Selective uptake uses only Stimulus-first initial disagreements.",
    "Correct/incorrect-advice uptake opportunities imply different initial correctness; the contrast is not causal.",
    "Shared responses across accuracy, SDT and revisions do not supply independent evidence; nonsignificance is not equivalence.",
    "Manual perceived rating concerns self; aided perceived ratings concern the aid. Trust is the mean of six positive items."),
    file.path(stage,"analysis_contract.txt"))
  ao_verify(inputs)
  writeLines(paste("Completed",format(Sys.time(),tz="UTC"),"UTC"),file.path(stage,"COMPLETE.txt"))
  write_csv(ao_artifacts(stage),file.path(stage,"artifact_manifest.csv"))
  ao_install(stage,OUT)
  message("Saved revised analysis to ",OUT,". Run tests/validate_aid_onset_primary.R before plotting.")
}
run()
