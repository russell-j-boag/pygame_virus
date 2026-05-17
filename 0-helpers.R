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
    deadline_s = NULL,
    reliability_group = NULL,
    conda_env  = "r-pygame",
    script     = "python/virus_task.py",
    extra_args = NULL
) {
  
  valid_blocks <- c(
    "CALIBRATION",
    "MANUAL",
    "AUTOMATION"
  )
  valid_reliability_groups <- c("high", "low")
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
  if (!is.null(deadline_s) && is.null(block)) {
    stop("'deadline_s' can only be used when 'block' is specified.")
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
  
  # Add deadline argument for blocks with 3s and 6s variants
  if (!is.null(deadline_s)) {
    args <- c(args, "--deadline-s", as.character(deadline_s))
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
