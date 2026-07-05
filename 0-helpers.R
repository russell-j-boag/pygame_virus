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
    aid_condition = NULL,
    conda_env  = "r-pygame",
    script     = "python/virus_task.py",
    extra_args = NULL
) {
  
  valid_blocks <- c(
    "AUTOMATION"
  )
  valid_aid_conditions <- c("manual", "aid_first", "stimulus_first")
  if (!is.null(aid_condition)) {
    aid_condition <- tolower(aid_condition)
  }
  
  # Validate block if provided
  if (!is.null(block) && !block %in% valid_blocks) {
    stop(
      "Invalid block. Must be one of: ",
      paste(valid_blocks, collapse = ", ")
    )
  }
  if (!is.null(aid_condition) && is.null(block)) {
    stop("'aid_condition' can only be used when 'block' is specified.")
  }
  if (!is.null(aid_condition) && block != "AUTOMATION") {
    stop("'aid_condition' can only be used with block = 'AUTOMATION'.")
  }
  if (!is.null(aid_condition) && !aid_condition %in% valid_aid_conditions) {
    stop(
      "Invalid aid_condition. Must be one of: ",
      paste(valid_aid_conditions, collapse = ", ")
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
  
  # Add aid-condition selector for scheduled automation-block variants
  if (!is.null(aid_condition)) {
    args <- c(args, "--aid-condition", aid_condition)
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
