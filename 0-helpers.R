# Helper function to run pre-task instructions
run_instructions <- function(
    conda_env  = "r-pygame",
    script     = "python/instructions.py",
    extra_args = NULL
) {
  
  # Pygame script must exist
  stopifnot(file.exists(script))
  
  # Load reticulate + activate env
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }
  reticulate::use_condaenv(conda_env, required = TRUE)
  
  # Python binary
  py_bin <- reticulate::py_config()$python
  message("Using Python: ", py_bin)
  
  # Base args
  args <- c(script)
  
  # Optional passthrough arguments
  if (!is.null(extra_args)) {
    stopifnot(is.character(extra_args))
    args <- c(args, extra_args)
  }
  
  # Run instructions
  status <- system2(py_bin, args)
  invisible(status)
}


# Helper function to run virus task
run_task <- function(
    block      = NULL,
    aid_onset_ms = NULL,
    reliability_group = NULL,
    conda_env  = "r-pygame",
    script     = "python/virus_task.py",
    extra_args = NULL
) {
  
  valid_blocks <- c(
    "CALIBRATION",
    "AUTOMATION"
  )
  valid_reliability_groups <- c("high", "low")
  valid_aid_onsets <- c(-500, 0, 500)
  if (!is.null(reliability_group)) {
    reliability_group <- tolower(reliability_group)
  }
  
  # Validate block if provided
  if (!is.null(block) && !block %in% valid_blocks) {
    stop(
      "Invalid block. Must be one of: ",
      paste(valid_blocks, collapse = ", ")
    )
  }
  if (!is.null(aid_onset_ms) && is.null(block)) {
    stop("'aid_onset_ms' can only be used when 'block' is specified.")
  }
  if (!is.null(aid_onset_ms) && block != "AUTOMATION") {
    stop("'aid_onset_ms' can only be used with block = 'AUTOMATION'.")
  }
  if (!is.null(aid_onset_ms) && !as.numeric(aid_onset_ms) %in% valid_aid_onsets) {
    stop(
      "Invalid aid_onset_ms. Must be one of: ",
      paste(valid_aid_onsets, collapse = ", ")
    )
  }
  if (!is.null(reliability_group) && is.null(block)) {
    stop("'reliability_group' can only be used when 'block' is specified.")
  }
  if (!is.null(reliability_group) && !reliability_group %in% valid_reliability_groups) {
    stop(
      "Invalid reliability_group. Must be one of: ",
      paste(valid_reliability_groups, collapse = ", ")
    )
  }
  if (!is.null(reliability_group) && block != "AUTOMATION") {
    stop("'reliability_group' can only be used with block = 'AUTOMATION'.")
  }
  
  # Pygame script must exist
  stopifnot(file.exists(script))
  
  # Load reticulate + activate env
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }
  reticulate::use_condaenv(conda_env, required = TRUE)
  
  # Python binary
  py_bin <- reticulate::py_config()$python
  message("Using Python: ", py_bin)
  
  # Base args
  args <- c(script)
  
  # Add block argument
  if (!is.null(block)) {
    args <- c(args, "--block", block)
  }
  
  # Add aid-onset selector for automation-block variants
  if (!is.null(aid_onset_ms)) {
    args <- c(args, "--aid-onset-ms", as.character(as.integer(aid_onset_ms)))
  }
  if (!is.null(reliability_group)) {
    args <- c(args, "--reliability-group", reliability_group)
  }

  # Optional passthrough arguments
  if (!is.null(extra_args)) {
    stopifnot(is.character(extra_args))
    args <- c(args, extra_args)
  }
  
  # Run task
  status <- system2(py_bin, args)
  invisible(status)
}
