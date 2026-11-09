# Independently verify clean data, run-specific selection, and source provenance.
suppressPackageStartupMessages({library(dplyr); library(readr)})
source("R/participant_selection.R")
base <- "data/semester2_2026_data"
clean <- file.path(base, "clean")
policy <- read_csv("config/participant_exclusions.csv", show_col_types = FALSE)
selection <- read_csv(file.path(clean, "cohort_selection.csv"), show_col_types = FALSE)
keys <- c("participant_id", "source_participant_id", "run_timestamp")
source_cohort <- read_csv(file.path(base, "data_virus_all.csv"), show_col_types = FALSE) %>%
  distinct(across(all_of(keys)))
expected_cohort <- anti_join(source_cohort, policy, by = keys)
stopifnot(!anyDuplicated(source_cohort$participant_id),
  nrow(selection) == nrow(source_cohort),
  setequal(select(selection, all_of(keys)), source_cohort),
  sum(selection$included) == nrow(expected_cohort),
  setequal(select(filter(selection, included), all_of(keys)), expected_cohort),
  setequal(select(filter(selection, !included), all_of(keys)), select(policy, all_of(keys))),
  setequal(selection$participant_id[!selection$included], c("4a", "10a", "48a", "83a")),
  all(selection$included[selection$participant_id %in% c("4b", "10b", "48b", "83b", "84")]))
for (filename in c("data_virus_all.csv", "data_virus.csv", "data_virus_postblock_all.csv", "data_virus_sliders_all.csv")) {
  all <- read_csv(file.path(base, filename), show_col_types = FALSE)
  actual <- read_csv(file.path(clean, filename), show_col_types = FALSE)
  expected <- anti_join(all, policy, by = keys)
  stopifnot(isTRUE(all.equal(actual, expected)),
    n_distinct(actual$participant_id) == nrow(expected_cohort))
}
hashes <- read_csv(file.path(clean, "input_checksums.csv"), show_col_types = FALSE)
stopifnot(identical(unname(tools::md5sum(hashes$path)), hashes$md5))
# A policy targeting an unknown timestamp must fail instead of excluding a replacement.
fixture <- select(selection, all_of(keys))
fixture$run_timestamp[fixture$participant_id == "4a"] <- "wrong_timestamp"
stopifnot(inherits(try(select_reviewed_participants(fixture, "config/participant_exclusions.csv"), silent = TRUE), "try-error"))
receipt <- sprintf("PASS: %d collected / %d clean; four run-specific exclusions; replacements retained; all four CSVs and provenance match.",
  nrow(source_cohort), nrow(expected_cohort))
writeLines(receipt, file.path(clean, "validation.txt"))
message(receipt)
