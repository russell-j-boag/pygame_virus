# Verify all four clean products and their source/decision provenance.
suppressPackageStartupMessages({library(dplyr); library(readr)})
policy <- read_csv("config/participant_exclusions.csv", show_col_types = FALSE)
selection <- read_csv("data/clean/cohort_selection.csv", show_col_types = FALSE)
stopifnot(nrow(selection) == 93, sum(selection$included) == 86,
  setequal(selection$participant_id[!selection$included], c("23a","49a","51a","65","75a","79","81")),
  all(selection$included[selection$participant_id %in% c("23b","49b","51b","75b")]))
keys <- c("participant_id", "source_participant_id", "run_timestamp")
for (filename in c("data_virus_all.csv", "data_virus.csv", "data_virus_postblock_all.csv", "data_virus_sliders_all.csv")) {
  all <- read_csv(file.path("data", filename), show_col_types = FALSE)
  actual <- read_csv(file.path("data/clean", filename), show_col_types = FALSE)
  expected <- anti_join(all, policy, by = keys)
  stopifnot(isTRUE(all.equal(actual, expected)), n_distinct(actual$participant_id) == 86)
}
hashes <- read_csv("data/clean/input_checksums.csv", show_col_types = FALSE)
stopifnot(identical(unname(tools::md5sum(hashes$path)), hashes$md5))
qc <- read_csv("analysis_outputs/semester2_2026_qc/participant_summary.csv", show_col_types = FALSE)
stopifnot(setequal(qc$participant_id, selection$participant_id),
  setequal(qc$participant_id[qc$exclude_by_rules], policy$participant_id))
message("PASS: 93 collected / 86 clean; seven timestamp-specific exclusions; four replacements retained; source values and QC agree.")
