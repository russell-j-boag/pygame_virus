# Behavioural trial exclusion agreed 2026-11-03. Does not alter raw or QC data.
BEHAVIOURAL_MIN_RT_S <- 0.200

mark_behavioural_trials <- function(dat) {
  stopifnot(all(c("timeout", "rt_s") %in% names(dat)))
  # TIMEOUT is a trial outcome, not an anticipatory response, even when an RT
  # was recorded. Exactly 200 ms is eligible. Historical zero/negative RTs are fast.
  dat %>% dplyr::mutate(
    excluded_fast_rt = !timeout & is.finite(rt_s) & rt_s < BEHAVIOURAL_MIN_RT_S,
    included_behavioural = !excluded_fast_rt)
}
