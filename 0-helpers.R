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
    conda_env  = "r-pygame",
    script     = "python/virus_task.py",
    extra_args = NULL
) {
  
  valid_blocks <- c(
    "TRAINING",
    "CALIBRATION",
    "MANUAL",
    "AUTOMATION1",
    "AUTOMATION2"
  )
  
  # Validate block if provided
  if (!is.null(block) && !block %in% valid_blocks) {
    stop(
      "Invalid block. Must be one of: ",
      paste(valid_blocks, collapse = ", ")
    )
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
  
  # Optional passthrough arguments
  if (!is.null(extra_args)) {
    stopifnot(is.character(extra_args))
    args <- c(args, extra_args)
  }
  
  # Run task
  status <- system2(py_bin, args)
  invisible(status)
}
