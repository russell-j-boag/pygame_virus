suppressPackageStartupMessages({library(dplyr); library(tidyr)})
source("R/behavioural_trial_selection.R")
source("analyse_time_pressure_exploratory.R")
# Threshold includes historical nonpositive responses, but never removes timeouts.
x <- tibble(rt_s=c(-.01,0,.199999,.2,.200001,.5,NA,0,.1,NA),
  timeout=c(rep(FALSE,7),rep(TRUE,3)))
y <- mark_behavioural_trials(x)
stopifnot(identical(y$included_behavioural,c(FALSE,FALSE,FALSE,rep(TRUE,7))))
# Fast current and immediately previous responses are ineligible for H8.
d <- tibble(participant_id="83b",run_timestamp="run",block_idx=2L,trial=1:7,
  mode="Automation",advice="Correct",accuracy=1L,agreement=1L,timeout=FALSE,
  rt_s=c(.4,.1,.4,.2,.4,.1,.4))
kept <- filter(mark_behavioural_trials(d),included_behavioural)
history <- make_sequence(kept)
stopifnot(identical(history$trial,c(4L,5L)),all(history$previous_trial==history$trial-1L))
# Original block/run boundaries remain distinct after filtering.
stopifnot(nrow(make_sequence(bind_rows(kept,mutate(kept,block_idx=3L))))==4L,
  nrow(make_sequence(bind_rows(kept,mutate(kept,run_timestamp="other"))))==4L)
message("PASS: strict 200 ms boundary, nonpositive RTs, timeout retention, and unbridged history gaps.")
