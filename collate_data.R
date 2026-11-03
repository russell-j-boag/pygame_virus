# Rscript collate_data.R [input_dir] [output_dir]
# Retain every collected complete run, with identical identities across exports.
suppressPackageStartupMessages({library(dplyr); library(readr)})
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
root <- dirname(normalizePath(script))
source(file.path(root, "R/participant_identity.R"))
args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args)) args[1] else "output/semester2_2026_data"
output <- if (length(args) > 1) args[2] else "data"
if (!dir.exists(input)) stop("Input directory does not exist: ", input)
files <- sort(list.files(input, "^results_.*_b00_ALL\\.csv$", full.names = TRUE))
if (!length(files)) stop("No complete trial exports found")

read_export <- function(path) {
  name <- basename(path)
  id <- as.integer(sub("^results_p([0-9]+)_.*$", "\\1", name))
  stamp <- sub("^results_p[0-9]+_([0-9]{8}_[0-9]{6})_.*$", "\\1", name)
  d <- read_csv(path, show_col_types = FALSE)
  if (!nrow(d) || anyNA(d$participant_id) || any(d$participant_id != id))
    stop("Filename/recorded participant mismatch: ", path)
  # Historical trust questionnaires lack timestamps; the exact same-run filename supplies it.
  if (!"run_timestamp" %in% names(d)) d$run_timestamp <- stamp
  if (anyNA(d$run_timestamp) || any(d$run_timestamp != stamp)) stop("Run mismatch: ", path)
  apply_participant_identity(d)
}

manifest <- list(); products <- list(); run_ids <- NULL
for (kind in c("trials", "trust", "sliders")) {
  paths <- switch(kind, trials = files,
    trust = sub("_ALL.csv$", "_POSTBLOCK_ALL.csv", files),
    sliders = sub("_ALL.csv$", "_POSTBLOCK_SLIDERS_ALL.csv", files))
  if (!all(file.exists(paths))) stop("Missing same-run ", kind, " exports")
  parts <- lapply(paths, read_export)
  dat <- apply_participant_identity(bind_rows(parts))
  ids <- distinct(dat, participant_id, source_participant_id, run_timestamp)
  if (kind == "trials") run_ids <- ids
  else if (!identical(ids, run_ids)) stop("Mismatched ", kind, " cohort")
  for (i in seq_along(parts)) {
    manifest[[length(manifest)+1L]] <- distinct(parts[[i]], participant_id, source_participant_id, run_timestamp) %>%
      mutate(kind = kind, path = normalizePath(paths[i]),
        md5 = unname(tools::md5sum(paths[i])), n_rows = nrow(parts[[i]]))
  }
  if (kind == "trials") {
    if (!"trial" %in% names(dat)) dat$trial <- dat$trial_idx
    if (!"trial_idx" %in% names(dat)) dat$trial_idx <- dat$trial
    products[["data_virus_all.csv"]] <- dat
    products[["data_virus.csv"]] <- select(dat,
      participant_id, source_participant_id, run_timestamp, block, block_idx,
      any_of(c("condition_code", "calibration_target_group", "calibration_target_accuracy",
        "main_block_order", "manual_segment", "reliability_phase_idx", "trial_in_reliability_phase",
        "reliability_phase_label", "aid_reliability_level", "automation_reliability_group",
        "trial_deadline_s", "rt_timing_version", "rt_backend_version", "stimulus_onset_s",
        "response_timestamp_s", "n_ignored_pre_onset_keydowns", "response_readout_delay_s")),
      trial, vblack_prop, stimulus, aid_label, aid_correct, response, rt_s, correct, feedback)
  } else if (kind == "trust") products[["data_virus_postblock_all.csv"]] <- dat
  else {
    if (!"question_key" %in% names(dat)) dat$question_key <- dat$slider_key
    if (!"slider_key" %in% names(dat)) dat$slider_key <- dat$question_key
    if (!"response_percent" %in% names(dat)) dat$response_percent <- dat$response
    if (!"response" %in% names(dat)) dat$response <- dat$response_percent
    products[["data_virus_sliders_all.csv"]] <- dat
  }
  message(kind, ": ", nrow(ids), " participants, ", nrow(dat), " rows")
}
# Validate every source family before replacing any canonical product.
dir.create(output, recursive = TRUE, showWarnings = FALSE)
for (name in names(products)) write_csv(products[[name]], file.path(output, name))
write_csv(bind_rows(manifest), file.path(output, "input_manifest.csv"))
sources <- normalizePath(c(script, file.path(root, "R/participant_identity.R"), IDENTITY_CORRECTIONS_FILE))
write_csv(tibble(path = sources, md5 = unname(tools::md5sum(sources))), file.path(output, "source_checksums.csv"))
