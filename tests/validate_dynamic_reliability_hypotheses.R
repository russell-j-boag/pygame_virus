# Compatibility entrypoint for the revised H1-H7 report validator.
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1]))
source(file.path(dirname(script), "validate_dynamic_reliability_primary.R"))
