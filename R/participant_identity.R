# Analysis identity corrections never change source exports or administered allocation.
.identity_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
IDENTITY_CORRECTIONS_FILE <- normalizePath(file.path(
  dirname(tail(.identity_files, 1)[[1]]), "..", "config", "participant_identity_corrections.csv"))
rm(.identity_files)

read_identity_corrections <- function(path = IDENTITY_CORRECTIONS_FILE) {
  x <- readr::read_csv(path, col_types = readr::cols(
    source_participant_id = readr::col_integer(), run_timestamp = readr::col_character(),
    participant_id = readr::col_integer(), reason = readr::col_character()))
  needed <- c("source_participant_id", "run_timestamp", "participant_id", "reason")
  if (!all(needed %in% names(x)) || anyNA(x[needed]) ||
      any(x$source_participant_id < 1 | x$participant_id < 1) ||
      any(!nzchar(x$run_timestamp) | !nzchar(x$reason)) ||
      anyDuplicated(x[c("source_participant_id", "run_timestamp")]) ||
      anyDuplicated(x$participant_id)) stop("Invalid or conflicting identity corrections")
  x
}

apply_participant_identity <- function(x, corrections = read_identity_corrections()) {
  if (!all(c("participant_id", "run_timestamp") %in% names(x)))
    stop("Identity correction requires participant_id and run_timestamp")
  had_source <- "source_participant_id" %in% names(x)
  if (!had_source) x$source_participant_id <- x$participant_id
  key <- paste(x$source_participant_id, x$run_timestamp, sep = "/")
  index <- match(key, paste(corrections$source_participant_id, corrections$run_timestamp, sep = "/"))
  expected <- x$source_participant_id
  changed <- !is.na(index)
  expected[changed] <- corrections$participant_id[index[changed]]
  if (anyNA(expected) || anyNA(x$run_timestamp) ||
      (had_source && any(x$participant_id != expected)))
    stop("Inconsistent recorded and analysis participant identities")
  x$participant_id <- expected
  runs <- unique(as.data.frame(x[c("participant_id", "source_participant_id", "run_timestamp")]))
  if (anyDuplicated(runs$participant_id))
    stop("Unexplained duplicate participant run or analysis-ID collision")
  x
}

read_identity_csv <- function(path) {
  apply_participant_identity(readr::read_csv(path, show_col_types = FALSE))
}
