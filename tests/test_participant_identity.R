# Regression checks for chronological labels and run-specific exclusions.
suppressPackageStartupMessages({library(dplyr); library(readr)})
source("R/participant_identity.R")
source("R/participant_selection.R")
corrections <- read_identity_corrections()
fixture <- transmute(corrections, participant_id = source_participant_id, run_timestamp)
x <- apply_participant_identity(fixture)
stopifnot(identical(x$participant_id, corrections$participant_id),
  identical(x$source_participant_id, fixture$participant_id),
  identical(apply_participant_identity(x), x))
fails <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))
fails(apply_participant_identity(rbind(fixture, transform(fixture[1,], run_timestamp = "unknown"),
  transform(fixture[1,], run_timestamp = "another_unknown"))))
collision <- corrections; collision$participant_id[2] <- collision$participant_id[1]
fails(apply_participant_identity(fixture, collision))
fails(apply_participant_identity(transform(x, participant_id = as.character(source_participant_id))))
path <- tempfile(fileext = ".csv")
write_csv(bind_rows(corrections, corrections), path)
fails(read_identity_corrections(path)); unlink(path)
trials <- read_identity_csv("data/data_virus_all.csv")
cohort <- distinct(trials, participant_id, source_participant_id, run_timestamp)
selection <- select_reviewed_participants(cohort, "config/participant_exclusions.csv")
stopifnot(all(selection$included[selection$participant_id %in% c("23b", "49b", "51b", "75b", "79b")]),
  !any(selection$included[selection$participant_id %in% c("65a", "65b", "79a")]))
bad <- cohort; bad$run_timestamp[bad$participant_id == "75a"] <- "wrong_timestamp"
fails(select_reviewed_participants(bad, "config/participant_exclusions.csv"))
message("PASS: identity corrections, idempotence, collision rejection and run-specific selection.")
