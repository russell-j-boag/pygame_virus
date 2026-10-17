# Revised H1-H7 framework agreed 2026-10-17 after inspection of earlier results.
# Rscript analyse_dynamic_reliability_primary.R [analysis_dir] [combined_csv] [qc_dir]
.dpr_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
.dpr_file <- if (length(.dpr_files)) tail(.dpr_files, 1)[[1]] else
  sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1])
DPR_ROOT <- dirname(normalizePath(.dpr_file))
source(file.path(DPR_ROOT, "analyse_dynamic_reliability.R"), local=TRUE)
source(file.path(DPR_ROOT, "analyse_dynamic_reliability_sdt.R"), local=TRUE)
source(file.path(DPR_ROOT, "analyse_dynamic_reliability_task_sdt.R"), local=TRUE)
source(file.path(DPR_ROOT, "R/dynamic_reliability_cohorts.R"), local=TRUE)
source(file.path(DPR_ROOT, "R/dynamic_reliability_primary_analysis.R"), local=TRUE)
rm(.dpr_files, .dpr_file)

run_dynamic_reliability_primary <- function(base, input, qc, root=DPR_ROOT) {
  base <- normalizePath(base); input <- normalizePath(input); qc <- normalizePath(qc)
  for (name in c("input_manifest", "source_checksums", "analysis_artifacts"))
    dynamic_verify_md5(file.path(base, paste0(name, ".csv")))
  selected <- task_read_inputs(input, qc)
  manifest <- read_csv(file.path(base, "input_manifest.csv"), show_col_types=FALSE)
  check(setequal(manifest$participant_id, selected$membership$participant_id), "Historical and QC cohorts differ")
  trial_paths <- manifest$path[manifest$kind == "trials"]
  raw_dir <- unique(dirname(trial_paths)); check(length(raw_dir)==1, "Expected one raw export directory")
  original <- load_cohort(raw_dir)
  raw <- read_csv(input, show_col_types=FALSE)
  prepared <- prepare_trials(raw)
  keys <- c("participant_id", "global_trial")
  cols <- c(keys, "run_timestamp", "stage", "phase", "group", "response", "stimulus", "aid_label", "rt_s", "accuracy", "agreement", "progress")
  x <- prepared %>% select(all_of(cols)) %>% arrange(across(all_of(keys)))
  y <- original$trials %>% select(all_of(cols)) %>% arrange(across(all_of(keys)))
  check(isTRUE(all.equal(x, y, check.attributes=FALSE, tolerance=1e-12)), "Combined CSV differs from historical source trials")
  check(identical(original$manifest$md5, manifest$md5) && identical(original$manifest$path, manifest$path), "Raw run/rating manifest changed")
  sources <- file.path(root, c("analyse_dynamic_reliability_primary.R", "R/dynamic_reliability_primary_analysis.R",
    "analyse_dynamic_reliability.R", "analyse_dynamic_reliability_sdt.R", "analyse_dynamic_reliability_task_sdt.R",
    "R/dynamic_reliability_cohorts.R", "config/hypothesis_figures.csv"))
  provenance <- task_hashes(unique(c(sources, selected$inputs, manifest$path, file.path(base, "input_manifest.csv"))))
  stage <- tempfile(".primary_build_", tmpdir=base); dir.create(stage)
  message("Revised analysis staging directory: ", stage)
  # Keep a failed build for diagnosis; it never replaces the preceding analysis.
  counts <- task_counts(selected$trials)
  scores <- bind_rows(task_scores(counts), task_scores(counts, "extreme_only")) %>%
    mutate(stage=factor(recode(stage, PRE="Manual pre", POST="Manual post"), STAGES),
      phase=factor(ifelse(stage %in% c("P1", "P2", "P3"), as.character(stage), NA), c("P1", "P2", "P3")),
      group=factor(group, GROUPS), subject=factor(participant_id), task_bias=-criterion_c)
  trials <- filter(original$trials, !is.na(stage))
  acounts <- filter(trials, !is.na(agreement)) %>% group_by(participant_id, group, phase, advice) %>%
    summarise(n_answered=n(), n_agree=sum(agreement), .groups="drop")
  pooled <- acounts %>% group_by(participant_id, group, advice) %>%
    summarise(n_answered=sum(n_answered), n_agree=sum(n_agree), .groups="drop") %>% mutate(phase="FULL")
  acounts <- bind_rows(mutate(acounts, phase=as.character(phase)), pooled)
  advice <- bind_rows(sdt_scores(acounts), sdt_scores(acounts, "extreme_only")) %>%
    mutate(subject=factor(participant_id), group=factor(group, GROUPS), advice_bias=automation_bias)
  aid_accuracy <- filter(trials, !is.na(phase)) %>% group_by(participant_id) %>%
    summarise(realised_aid_accuracy=100*mean(aid_label==stimulus), .groups="drop")
  ratings <- original$ratings %>% left_join(aid_accuracy, by="participant_id") %>%
    mutate(perceived=perceived_aid_accuracy, signed_error=perceived_aid_accuracy-realised_aid_accuracy)
  bundle <- list(membership=selected$membership, data=list(), results=list(), estimates=list(),
    diagnostics=list(), audit=list(), model_index=list())
  for (sample_name in c("primary", "all_participants")) {
    ids <- selected$membership$participant_id[if (sample_name=="primary") selected$membership$in_primary else TRUE]
    pick <- function(d) droplevels(filter(d, participant_id %in% ids))
    out <- file.path(stage, sample_name); dir.create(file.path(out, "models"), recursive=TRUE)
    cached <- if (sample_name=="primary") file.path(base, "hypothesis_components/primary/analysis") else base
    if (dir.exists(cached)) for (name in c("input_manifest", "source_checksums", "analysis_artifacts"))
      dynamic_verify_md5(file.path(cached, paste0(name, ".csv")))
    z <- dpr_analyse_sample(pick(trials), pick(scores), pick(advice), pick(ratings), out, cached, sample_name)
    bundle$data[[sample_name]] <- z$data
    for (name in c("results", "estimates", "diagnostics", "audit", "model_index")) bundle[[name]][[sample_name]] <- z[[name]]
    write_csv(pick(counts), file.path(out, "task_counts.csv"))
    write_csv(pick(acounts), file.path(out, "advice_counts.csv"))
  }
  bundle$results <- bind_rows(bundle$results); bundle$diagnostics <- bind_rows(bundle$diagnostics)
  bundle$model_index <- bind_rows(bundle$model_index)
  for (sample_name in c("primary", "all_participants")) {
    r <- filter(bundle$results, sample==sample_name, analysis=="primary", correction %in% c("none", "loglinear"), grepl("^H", hypothesis))
    check(nrow(r)==125, "The revised framework must contain 125 primary comparisons per cohort")
  }
  write_csv(bundle$membership, file.path(stage, "cohort_membership.csv"))
  write_csv(bundle$results, file.path(stage, "contrasts.csv"))
  write_csv(filter(bundle$results,analysis=="common_slope_sensitivity"),file.path(stage,"common_slope_contrasts.csv"))
  write_csv(bundle$diagnostics, file.path(stage, "model_diagnostics.csv"))
  write_csv(bundle$model_index, file.path(stage, "model_index.csv"))
  saveRDS(bundle$audit, file.path(stage, "contrast_audit.rds"))
  bundle$audit <- NULL
  saveRDS(bundle, file.path(stage, "bundle.rds"))
  writeLines(dpr_methods(), file.path(stage, "methods.txt"))
  writeLines(capture.output(sessionInfo()), file.path(stage, "session_info.txt"))
  check(identical(vapply(provenance$path, task_sha, character(1), USE.NAMES=FALSE), provenance$sha256), "Sources changed during analysis")
  write_csv(provenance, file.path(stage, "input_checksums.csv"))
  files <- list.files(stage, recursive=TRUE, full.names=TRUE)
  write_csv(tibble(path=substring(files,nchar(stage)+2), sha256=vapply(files,task_sha,character(1),USE.NAMES=FALSE)),
    file.path(stage,"artifact_manifest.csv"))
  writeLines("Computed revised H1-H7 analyses; inspect model diagnostics and validate before plotting.", file.path(stage,"COMPLETE.txt"))
  destination <- file.path(base,"primary")
  if (dir.exists(destination)) {
    archive <- file.path(base,"archive"); dir.create(archive,showWarnings=FALSE)
    stopifnot(file.rename(destination,tempfile("primary_",tmpdir=archive)))
  }
  stopifnot(file.rename(stage,destination))
  message("Revised primary analyses saved: ",destination)
  invisible(destination)
}
if (sys.nframe()==0L) {
  args <- commandArgs(trailingOnly=TRUE); stopifnot(length(args)<=3)
  defaults <- file.path(DPR_ROOT,c("analysis_outputs/semester2_2026_behavioural","data/data_virus_all.csv","analysis_outputs/semester2_2026_qc"))
  defaults[seq_along(args)] <- args
  run_dynamic_reliability_primary(defaults[1],defaults[2],defaults[3])
}
