# Rscript plot_aid_onset_hypotheses.R [analysis_dir] [figure_dir]
script <- sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
ROOT <- dirname(normalizePath(script))
source(file.path(ROOT,"R/aid_onset_primary.R"))
source(file.path(ROOT,"R/aid_onset_primary_plots.R"))
args <- commandArgs(trailingOnly=TRUE)
ao_check(length(args)<=2,"Usage: Rscript plot_aid_onset_hypotheses.R [analysis_dir] [figure_dir]")
ao_build_plots(ROOT,
  if(length(args)) args[1] else file.path(ROOT,"analysis_outputs/semester2_2026_hypotheses/primary"),
  if(length(args)>=2) args[2] else file.path(ROOT,"plots/semester2_2026_key_findings/presentation"))
