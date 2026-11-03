# Rscript prepare_clean_data.R [all_collected_dir] [clean_dir]
# Export reviewed participant/run selections without fitting behavioural models.
suppressPackageStartupMessages({library(dplyr); library(readr)})
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
root <- dirname(normalizePath(script))
source(file.path(root, "R/participant_identity.R"))
source(file.path(root, "R/participant_selection.R"))
args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args)) args[1] else file.path(root, "data/semester2_2026_data")
output <- if (length(args) > 1) args[2] else file.path(input, "clean")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
input <- normalizePath(input); output <- normalizePath(output)
stopifnot(input != output)
policy <- file.path(root, "config/participant_exclusions.csv")
files <- c("data_virus_all.csv", "data_virus.csv", "data_virus_postblock_all.csv", "data_virus_sliders_all.csv")
trials <- read_identity_csv(file.path(input, files[1]))
cohort <- distinct(trials, participant_id, source_participant_id, run_timestamp)
selection <- select_reviewed_participants(cohort, policy)
write_csv(selection, file.path(output, "cohort_selection.csv"))
write_csv(filter(selection, !included), file.path(output, "exclusions.csv"))
write_csv(filter(selection, included), file.path(output, "cohort.csv"))
for (filename in files) {
  all <- read_identity_csv(file.path(input, filename))
  stopifnot(setequal(all$participant_id, selection$participant_id))
  clean <- filter(all, participant_id %in% selection$participant_id[selection$included])
  write_csv(clean, file.path(output, filename))
  message(filename, ": ", n_distinct(clean$participant_id), " participants, ", nrow(clean), " rows")
}
manifest <- read_csv(file.path(input, "input_manifest.csv"), show_col_types = FALSE)
stopifnot(identical(unname(tools::md5sum(manifest$path)), manifest$md5))
write_csv(filter(manifest, participant_id %in% selection$participant_id[selection$included]),
  file.path(output, "input_manifest.csv"))
paths <- normalizePath(c(file.path(input, files), file.path(input, "input_manifest.csv"),
  policy, IDENTITY_CORRECTIONS_FILE, script, file.path(root, "R/participant_identity.R"),
  file.path(root, "R/participant_selection.R")))
write_csv(tibble(path = paths, md5 = unname(tools::md5sum(paths))), file.path(output, "input_checksums.csv"))
writeLines(c(sprintf("%d collected participants; %d excluded; %d retained.", nrow(selection),
  sum(!selection$included), sum(selection$included)),
  paste("Excluded:", paste(selection$participant_id[!selection$included], collapse = ", ")),
  "Participant exclusions only; all retained trials (including calibration, timeouts and historical nonpositive RTs) are preserved.",
  "QC uses the parent all-collected CSVs. Behavioural/model fitting has not been rerun by this exporter."),
  file.path(output, "COMPLETE.txt"))
