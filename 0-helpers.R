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
    aid_condition = NULL,
    conda_env  = "r-pygame",
    script     = "python/virus_task.py",
    extra_args = NULL
) {

  valid_aid_conditions <- c("practice", "manual", "aid_first", "stimulus_first")
  if (!is.null(aid_condition)) {
    aid_condition <- tolower(aid_condition)
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

  # Add selector for a single scheduled main condition
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
