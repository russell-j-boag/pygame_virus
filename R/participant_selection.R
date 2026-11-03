# Run-specific reviewed exclusions. QC callers must use the unfiltered source cohort.
select_reviewed_participants <- function(cohort, policy_path) {
  keys <- c("participant_id", "source_participant_id", "run_timestamp")
  policy <- readr::read_csv(policy_path, show_col_types = FALSE,
    col_types = readr::cols(participant_id = readr::col_character(),
      source_participant_id = readr::col_integer(), run_timestamp = readr::col_character()))
  cohort <- dplyr::mutate(cohort, participant_id = as.character(participant_id),
    run_timestamp = as.character(run_timestamp))
  if (anyDuplicated(cohort$participant_id) || anyDuplicated(policy$participant_id) ||
      anyNA(policy) || !all(policy$decision == "exclude") || any(!nzchar(policy$reason)))
    stop("Invalid cohort or participant exclusion decisions")
  if (nrow(dplyr::anti_join(policy, cohort, by = keys)))
    stop("Exclusion decision does not match a source run")
  dplyr::left_join(cohort, policy, by = keys) %>%
    dplyr::mutate(included = is.na(decision), decision = dplyr::coalesce(decision, "include"),
      reason = dplyr::coalesce(reason, "Retained"))
}
