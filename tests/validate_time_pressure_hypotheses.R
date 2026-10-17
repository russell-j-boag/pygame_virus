# Compatibility entrypoint for the canonical primary hypothesis report.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1])
source(file.path(dirname(normalizePath(script)), "validate_time_pressure_primary.R"))
