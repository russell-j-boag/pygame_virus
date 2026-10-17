# Preserve the all-participant fits and fit the QC-retained cohort with the same
# analysis code, model specification, diagnostics and contrast families.
dynamic_verify_md5 <- function(path) {
  m <- readr::read_csv(path, show_col_types = FALSE)
  if (!all(c("path", "md5") %in% names(m)) || !all(file.exists(m$path)) ||
      !identical(unname(tools::md5sum(m$path)), m$md5))
    stop("Stale or changed analysis inputs/artifacts: ", path, call. = FALSE)
  invisible(m)
}
dynamic_prepare_cohorts <- function(base, input, qc, root) {
  for (name in c("input_manifest", "source_checksums", "analysis_artifacts"))
    dynamic_verify_md5(file.path(base, paste0(name, ".csv")))
  data <- task_read_inputs(input, qc)
  membership <- data$membership
  manifest <- readr::read_csv(file.path(base, "input_manifest.csv"), show_col_types = FALSE)
  stopifnot(setequal(manifest$participant_id, membership$participant_id))
  # The saved behavioural fits and current collation must describe identical runs/trials.
  raw <- readr::read_csv(input, show_col_types = FALSE)
  original <- dplyr::bind_rows(lapply(manifest$path[manifest$kind == "trials"], readr::read_csv, show_col_types = FALSE))
  keys <- c("participant_id", "run_timestamp", "global_trial")
  columns <- c(keys, "block", "stimulus", "response", "aid_label", "correct", "rt_s",
    "calibration_target_group", "calibration_target_accuracy", "key_black", "key_white",
    "main_block_order", "manual_segment", "reliability_phase_label", "automation_reliability_group")
  x <- raw %>% dplyr::select(dplyr::all_of(columns)) %>% dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
  y <- original %>% dplyr::select(dplyr::all_of(columns)) %>% dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
  stopifnot(isTRUE(all.equal(as.data.frame(x), as.data.frame(y), check.attributes = FALSE, tolerance = 1e-12)))
  components <- file.path(base, "hypothesis_components")
  dir.create(components, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(membership, file.path(components, "cohort_membership.csv"))
  primary <- file.path(components, "primary", "analysis")
  provenance <- unique(c(data$inputs, file.path(root, "R/dynamic_reliability_cohorts.R"),
    file.path(base, "input_manifest.csv")))
  cached <- file.exists(file.path(primary, "COMPLETE.txt")) &&
    file.exists(file.path(primary, "cohort_selection_checksums.csv"))
  if (cached) cached <- tryCatch({
    for (name in c("input_manifest", "source_checksums", "analysis_artifacts", "cohort_selection_checksums"))
      dynamic_verify_md5(file.path(primary, paste0(name, ".csv")))
    pm <- readr::read_csv(file.path(primary, "input_manifest.csv"), show_col_types = FALSE)
    setequal(pm$participant_id, membership$participant_id[membership$in_primary])
  }, error = function(e) FALSE)
  if (!cached) {
    if (dir.exists(primary)) {
      stopifnot(file.exists(file.path(primary, "input_manifest.csv")))
      archive <- file.path(components, "archive"); dir.create(archive, showWarnings = FALSE)
      stopifnot(file.rename(primary, tempfile("primary_analysis_", tmpdir = archive)))
    }
    selected <- manifest[manifest$participant_id %in% membership$participant_id[membership$in_primary], ]
    selection <- file.path(components, "primary", "selected_raw")
    if (dir.exists(selection)) unlink(selection, recursive = TRUE)
    dir.create(selection, recursive = TRUE)
    stopifnot(all(file.symlink(selected$path, file.path(selection, basename(selected$path)))))
    log <- file.path(components, "primary", "fit.log")
    message("Fitting QC-retained behavioural cohort: ", sum(membership$in_primary), " participants. Log: ", log)
    status <- system2(file.path(R.home("bin"), "Rscript"),
      shQuote(c(file.path(root, "analyse_dynamic_reliability.R"), selection, primary)), stdout = log, stderr = log)
    if (status != 0) stop("QC-retained fit failed; see ", log, call. = FALSE)
    note <- paste0("Primary cohort applies calibration, near-chance and anticipatory QC exclusions: ",
      sum(membership$in_primary), " retained of ", nrow(membership),
      ". Manual target deviation is diagnostic only. Selection includes post-exposure performance; compare the all-participant sensitivity.")
    # Amend the report's cohort statement, without changing numerical analysis.
    for (name in c("results_report.txt", "results_report.html")) {
      path <- file.path(primary, name)
      text <- readLines(path, warn = FALSE)
      text <- gsub("Calibration/manual performance screens do not exclude participants.", note, text, fixed = TRUE)
      writeLines(text, path)
    }
    readr::write_csv(membership, file.path(primary, "cohort_membership.csv"))
    readr::write_csv(tibble::tibble(path = normalizePath(provenance), md5 = unname(tools::md5sum(provenance))),
      file.path(primary, "cohort_selection_checksums.csv"))
    files <- list.files(primary, recursive = TRUE, full.names = TRUE)
    files <- files[!basename(files) %in% c("COMPLETE.txt", "analysis_artifacts.csv")]
    readr::write_csv(tibble::tibble(path = normalizePath(files), md5 = unname(tools::md5sum(files))),
      file.path(primary, "analysis_artifacts.csv"))
  }
  list(primary = primary, all_participants = base, membership = membership,
    input_paths = provenance, components = components)
}
if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({library(dplyr); library(readr)})
  script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
  root <- dirname(dirname(script)); source(file.path(root, "analyse_dynamic_reliability_task_sdt.R"))
  dynamic_prepare_cohorts(file.path(root, "analysis_outputs/semester2_2026_behavioural"),
    file.path(root, "data/data_virus_all.csv"), file.path(root, "analysis_outputs/semester2_2026_qc"), root)
}
