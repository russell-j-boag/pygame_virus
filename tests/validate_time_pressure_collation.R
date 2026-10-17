# Independent source-to-combined audit for the current Semester 2 refresh.
suppressPackageStartupMessages({library(dplyr); library(readr)})
input <- "output/semester2_2026_data"
output <- "data/semester2_2026_data"
corrections <- read_csv("config/participant_identity_corrections.csv", show_col_types = FALSE)
files <- sort(list.files(input, "^results_.*_b00_ALL\\.csv$", full.names = TRUE))
expected_n <- 87L
audit <- character()
for (kind in c("trials", "summary", "trust", "sliders")) {
  paths <- switch(kind, trials = files, summary = files,
    trust = sub("_ALL.csv$", "_POSTBLOCK_ALL.csv", files),
    sliders = sub("_ALL.csv$", "_POSTBLOCK_SLIDERS_ALL.csv", files))
  filename <- switch(kind, trials = "data_virus_all.csv", summary = "data_virus.csv",
    trust = "data_virus_postblock_all.csv", sliders = "data_virus_sliders_all.csv")
  raw <- bind_rows(lapply(paths, read_csv, show_col_types = FALSE))
  raw$source_participant_id <- raw$participant_id
  # Independently apply the explicitly documented timestamp-specific correction.
  for (j in seq_len(nrow(corrections))) {
    c <- corrections[j,]
    selected <- raw$source_participant_id == c$source_participant_id & raw$run_timestamp == c$run_timestamp
    stopifnot(any(selected))
    raw$participant_id[selected] <- c$participant_id
  }
  actual <- read_csv(file.path(output, filename), show_col_types = FALSE)
  per_person <- switch(kind, trials = 1900L, summary = 1900L, trust = 12L, sliders = 5L)
  stopifnot(n_distinct(actual$participant_id) == expected_n,
    setequal(actual$participant_id, seq_len(expected_n)), nrow(actual) == expected_n * per_person,
    all(count(actual, participant_id)$n == per_person))
  keys <- c("participant_id", "run_timestamp", "block_idx", if (kind %in% c("trials", "summary")) "trial" else "question_idx")
  stopifnot(!anyDuplicated(actual[keys]), all(names(actual) %in% names(raw)))
  expected <- raw %>% select(all_of(names(actual))) %>% arrange(across(all_of(keys)))
  actual <- actual %>% arrange(across(all_of(keys)))
  stopifnot(isTRUE(all.equal(expected, actual, tolerance = 1e-12, check.attributes = FALSE)))
  for (pid in c(83, 84)) {
    d <- filter(actual, participant_id == pid)
    stopifnot(all(d$source_participant_id == 83),
      all(d$run_timestamp == if (pid == 83) "20261015_125827" else "20261015_130522"),
      all(d$automation_reliability_pattern == "HP65_LP95"))
  }
  if (kind == "trials") {
    counts <- count(actual, participant_id, condition_deadline_code)
    stopifnot(nrow(counts) == 5 * expected_n,
      all(counts$n == ifelse(counts$condition_deadline_code == "CAL_LP", 300, 400)),
      all(actual$trial_deadline_s == ifelse(grepl("_HP$", actual$condition_deadline_code), 1, 3)),
      all(!filter(actual, participant_id %in% c(83,84))$keymap_flip))
  }
  audit <- c(audit, sprintf("%s: %d participants, %d rows; all normalized source values and run identities match.",
    filename, expected_n, nrow(actual)))
}
for (name in c("input_manifest", "source_checksums")) {
  m <- read_csv(file.path(output, paste0(name, ".csv")), show_col_types = FALSE)
  stopifnot(identical(unname(tools::md5sum(m$path)), m$md5))
  if (name == "input_manifest") stopifnot(nrow(m) == 3 * expected_n)
}
writeLines(audit, file.path(output, "validation.txt"))
cat(paste(audit, collapse = "\n"), "\n")
