#!/usr/bin/env Rscript

input_path <- file.path("data", "data_virus_all_sem1_calibration.csv")
last_n <- 150L

if (!file.exists(input_path)) {
  stop("Missing input file: ", input_path)
}

dat <- read.csv(input_path, stringsAsFactors = FALSE)
required_cols <- c(
  "participant_id",
  "difficulty_mode",
  "trial",
  "delta_stair_realised"
)
missing_cols <- setdiff(required_cols, names(dat))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

condition_col <- if ("condition_code" %in% names(dat)) {
  "condition_code"
} else if ("block" %in% names(dat)) {
  "block"
} else {
  stop("Missing required condition column: condition_code")
}

correct_col <- if ("decision2_correct" %in% names(dat)) {
  "decision2_correct"
} else if ("correct" %in% names(dat)) {
  "correct"
} else {
  stop("Missing required correctness column: decision2_correct")
}

cal <- dat[
  dat[[condition_col]] == "CALIBRATION" & dat$difficulty_mode == "staircase",
  c(required_cols, correct_col)
]
cal$trial <- as.integer(cal$trial)
cal$delta_stair_realised <- as.numeric(cal$delta_stair_realised)
cal$correct <- cal[[correct_col]] %in% TRUE
cal <- cal[order(cal$participant_id, cal$trial), ]

participants <- sort(unique(cal$participant_id))
summaries <- lapply(participants, function(pid) {
  rows <- cal[cal$participant_id == pid, ]
  if (nrow(rows) < last_n) {
    stop("Participant ", pid, " has fewer than ", last_n, " calibration rows")
  }

  last_rows <- utils::tail(rows, last_n)
  data.frame(
    participant_id = pid,
    n_calibration_trials = nrow(rows),
    n_trials_summarised = nrow(last_rows),
    mean_delta = mean(last_rows$delta_stair_realised, na.rm = TRUE),
    sd_delta = sd(last_rows$delta_stair_realised, na.rm = TRUE),
    accuracy = mean(last_rows$correct, na.rm = TRUE)
  )
})
summary_df <- do.call(rbind, summaries)

mean_delta <- mean(summary_df$mean_delta, na.rm = TRUE)
sd_delta <- sd(summary_df$mean_delta, na.rm = TRUE)
mean_accuracy <- mean(summary_df$accuracy, na.rm = TRUE)

cat("Prior calibration delta summary\n")
cat("input_file:", input_path, "\n")
cat("n_participants:", nrow(summary_df), "\n")
cat("n_last_trials_per_participant:", last_n, "\n")
cat(sprintf("mean_delta: %.12f\n", mean_delta))
cat(sprintf("sd_delta_across_participant_means: %.12f\n", sd_delta))
cat(sprintf("mean_last_window_accuracy: %.4f\n", mean_accuracy))
cat("\nFirst 10 participant summaries:\n")
print(utils::head(summary_df, 10), row.names = FALSE)
