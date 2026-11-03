# Independent source-to-combined reconciliation; no production collation imports.
suppressPackageStartupMessages({library(dplyr); library(readr)})
args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args)) args[1] else "output/semester2_2026_data"
output <- if (length(args) > 1) args[2] else "data"
corrections <- read_csv("config/participant_identity_corrections.csv", show_col_types = FALSE)
files <- sort(list.files(input, "^results_.*_b00_ALL\\.csv$", full.names = TRUE))
expected_n <- length(files); audit <- character()
for (kind in c("trials", "summary", "trust", "sliders")) {
  paths <- switch(kind, trials = files, summary = files,
    trust = sub("_ALL.csv$", "_POSTBLOCK_ALL.csv", files),
    sliders = sub("_ALL.csv$", "_POSTBLOCK_SLIDERS_ALL.csv", files))
  filename <- switch(kind, trials = "data_virus_all.csv", summary = "data_virus.csv",
    trust = "data_virus_postblock_all.csv", sliders = "data_virus_sliders_all.csv")
  raw <- bind_rows(lapply(paths, function(path) {
    d <- read_csv(path, show_col_types = FALSE)
    stamp <- sub("^results_p[0-9]+_([0-9]{8}_[0-9]{6})_.*$", "\\1", basename(path))
    if (!"run_timestamp" %in% names(d)) d$run_timestamp <- stamp
    stopifnot(all(d$run_timestamp == stamp))
    d
  }))
  raw$source_participant_id <- raw$participant_id
  raw$participant_id <- as.character(raw$participant_id)
  for (j in seq_len(nrow(corrections))) {
    c <- corrections[j,]
    selected <- raw$source_participant_id == c$source_participant_id & raw$run_timestamp == c$run_timestamp
    stopifnot(any(selected)); raw$participant_id[selected] <- c$participant_id
  }
  if (kind %in% c("trials", "summary")) raw$trial_idx <- raw$trial
  if (kind == "sliders") {
    raw$slider_key <- raw$question_key
    raw$response <- raw$response_percent
  }
  actual <- read_csv(file.path(output, filename), show_col_types = FALSE)
  per_person <- switch(kind, trials = 1900L, summary = 1900L, trust = 6L, sliders = 4L)
  stopifnot(n_distinct(actual$participant_id) == expected_n,
    setequal(actual$participant_id, raw$participant_id), nrow(actual) == expected_n * per_person,
    all(count(actual, participant_id)$n == per_person))
  keys <- c("participant_id", "run_timestamp", "block_idx", if (kind %in% c("trials", "summary")) "trial" else "question_idx")
  stopifnot(!anyDuplicated(actual[keys]), all(names(actual) %in% names(raw)))
  expected <- raw %>% select(all_of(names(actual))) %>% arrange(across(all_of(keys)))
  actual <- actual %>% arrange(across(all_of(keys)))
  stopifnot(isTRUE(all.equal(expected, actual, tolerance = 1e-12, check.attributes = FALSE)))
  if (kind == "trials") {
    stopifnot(all(actual$trial_deadline_s == 5),
      all(actual$calibration_target_group == ifelse(actual$source_participant_id %% 2, "CAL65", "CAL90")),
      all(actual$key_black == ifelse(((actual$source_participant_id-1) %/% 2) %% 2, "J", "D")))
    counts <- count(actual, participant_id, block_idx)
    stopifnot(nrow(counts) == 4*expected_n, all(counts$n == c(300,200,1200,200)[counts$block_idx]))
  }
  audit <- c(audit, sprintf("%s: %d participants, %d rows; source values and run identities match.", filename, expected_n, nrow(actual)))
}
for (name in c("input_manifest", "source_checksums")) {
  m <- read_csv(file.path(output, paste0(name, ".csv")), show_col_types = FALSE)
  stopifnot(identical(unname(tools::md5sum(m$path)), m$md5))
  if (name == "input_manifest") stopifnot(nrow(m) == 3*expected_n)
}
writeLines(audit, file.path(output, "validation.txt"))
cat(paste(audit, collapse = "\n"), "\n")
