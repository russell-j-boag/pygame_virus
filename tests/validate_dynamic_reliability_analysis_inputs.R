# Verify ingestion and cohort membership without fitting empirical models.
source("analyse_dynamic_reliability.R")
all <- load_cohort("output/semester2_2026_data")
clean <- load_cohort("data/clean")
stopifnot(n_distinct(all$trials$participant_id) == 93,
  n_distinct(clean$trials$participant_id) == 86, nrow(clean$ratings) == 86,
  all(c("23b", "49b", "51b", "75b") %in% clean$ratings$participant_id))
source("analyse_dynamic_reliability_task_sdt.R")
x <- task_read_inputs("data/data_virus_all.csv", "analysis_outputs/semester2_2026_qc")
stopifnot(nrow(x$membership) == 93, sum(x$membership$in_primary) == 86,
  setequal(x$membership$participant_id[x$membership$in_primary], clean$ratings$participant_id))
message("PASS: behavioural loader and task-SDT/primary membership agree on 86 retained participants; no empirical models fitted.")
