source("R/participant_identity.R")
corrections <- read_identity_corrections()
fixture <- data.frame(participant_id = c(83, 83, 85),
  run_timestamp = c("20261015_125827", "20261015_130522", "20261016_100000"),
  keymap_flip = c(FALSE, FALSE, FALSE), score = c(1, 0, 1))
x <- apply_participant_identity(fixture)
stopifnot(identical(x$participant_id, c(83, 84, 85)),
  identical(x$source_participant_id, fixture$participant_id),
  identical(x$keymap_flip, fixture$keymap_flip),
  identical(x$run_timestamp, fixture$run_timestamp),
  identical(apply_participant_identity(x), x),
  nrow(aggregate(score ~ participant_id, x, mean)) == 3)
fails <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))
fails(apply_participant_identity(rbind(fixture, transform(fixture[3,], run_timestamp = "different"))))
fails(apply_participant_identity(rbind(fixture, transform(fixture[3,], participant_id = 84))))
fails(apply_participant_identity(transform(x, participant_id = c(83, 83, 85))))
path <- tempfile(fileext = ".csv")
readr::write_csv(rbind(corrections, corrections), path)
fails(read_identity_corrections(path))
unlink(path)
# Same-run trial, trust and slider files must map identically.
paths <- paste0("output/semester2_2026_data/results_p083_20261015_130522_b00_",
  c("ALL.csv", "POSTBLOCK_ALL.csv", "POSTBLOCK_SLIDERS_ALL.csv"))
for (path in paths) {
  d <- read_identity_csv(path)
  stopifnot(all(d$participant_id == 84), all(d$source_participant_id == 83),
    all(d$run_timestamp == "20261015_130522"))
  if ("keymap_flip" %in% names(d)) stopifnot(all(!d$keymap_flip))
}
message("Passed timestamp-specific correction, idempotence, separate participants, collision/duplicate rejection and all three export types.")
