suppressPackageStartupMessages(library(dplyr))
source("R/behavioural_trial_selection.R")
x <- tibble(rt_s=c(-.01,0,.199999,.2,.200001,.5,NA,0,.1,NA),
  timeout=c(rep(FALSE,7),rep(TRUE,3)), trial_in_reliability_phase=1:10)
y <- mark_behavioural_trials(x)
stopifnot(identical(y$included_behavioural,c(FALSE,FALSE,FALSE,rep(TRUE,7))))
kept <- filter(y,included_behavioural)
stopifnot(identical(kept$trial_in_reliability_phase,4:10),all(kept$included_behavioural[kept$timeout]))
message("PASS: strict 200 ms boundary, nonpositive RT exclusion, timeout retention and unchanged trial positions.")
