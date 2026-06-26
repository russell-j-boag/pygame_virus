# Clear workspace
rm(list = ls())

# Load libraries and functions
library("brms")
library("dplyr")
library("readr")
library("tidyr")
library("ggplot2")
library("pander")
library("emmeans")

options(digits = 2)
options(mc.cores = 4)
theme_set(theme_classic())

make_fixef_table <- function(model) {
  out <- as.data.frame(fixef(model, summary = TRUE))
  out$term <- rownames(out)
  rownames(out) <- NULL
  out <- out[, c("term", "Estimate", "Est.Error", "Q2.5", "Q97.5")]
  names(out) <- c("term", "estimate", "sd", "lower_95", "upper_95")
  out
}

ensure_design_columns <- function(data) {
  if (!"aid_condition" %in% names(data)) {
    data$aid_condition <- NA_character_
  }
  if (!"aid_accuracy_setting" %in% names(data)) {
    data$aid_accuracy_setting <- NA_real_
  }
  data
}

ensure_trial_current_columns <- function(data) {
  if (!"decision2_correct" %in% names(data) && "correct" %in% names(data)) {
    data$decision2_correct <- data$correct
  }
  if (!"decision2_rt_s" %in% names(data) && "rt_s" %in% names(data)) {
    data$decision2_rt_s <- data$rt_s
  }
  data
}

factor_aid_condition <- function(aid_condition) {
  condition <- case_when(
    !is.na(aid_condition) & aid_condition == "manual" ~ "Manual",
    !is.na(aid_condition) & aid_condition == "aid_first" ~ "Aid first",
    !is.na(aid_condition) & aid_condition == "stimulus_first_change" ~ "Stimulus first, change allowed",
    TRUE ~ NA_character_
  )
  factor(
    condition,
    levels = c("Manual", "Aid first", "Stimulus first, change allowed")
  )
}

# Load current data
dat <- read_csv("data/data_virus.csv", show_col_types = FALSE)
str(dat)

# Recode to the current three-condition design.
dat <- dat %>%
  ensure_design_columns() %>%
  ensure_trial_current_columns() %>%
  mutate(
    C = as.integer(decision2_correct),
    subjects = factor(participant_id),
    aid_condition = factor_aid_condition(aid_condition)
  ) %>%
  group_by(subjects) %>%
  mutate(Trial = dplyr::row_number()) %>%
  ungroup() %>%
  filter(
    block == "AUTOMATION",
    !is.na(C),
    !is.na(decision2_rt_s),
    !is.na(aid_condition)
  )

str(dat)
head(dat)
tail(dat)

# Subject-level summaries
accs <- dat %>%
  group_by(subjects, aid_condition) %>%
  summarise(acc = mean(C), .groups = "drop") %>%
  arrange(subjects, aid_condition)
accs

rt_dat <- dat %>%
  filter(C == 1, decision2_rt_s > 0) %>%
  mutate(log_rt = log(decision2_rt_s))

RTs <- rt_dat %>%
  group_by(subjects, aid_condition) %>%
  summarise(rt = mean(decision2_rt_s), .groups = "drop") %>%
  arrange(subjects, aid_condition)
RTs

# -------------------------------------------------------------------------
# Accuracy model
# -------------------------------------------------------------------------

acc_formula <- bf(C ~ aid_condition + (1 | subjects))

acc_priors <- c(
  prior(normal(0, 1.5), class = "Intercept"),
  prior(normal(0, 1), class = "b")
)

acc_brms <- brm(
  formula = acc_formula,
  data = dat,
  family = bernoulli(link = "probit"),
  prior = acc_priors,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 202103,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  file = "linear_models/acc_brms_model_aid_condition"
)

save(acc_brms, file = "linear_models/acc_model_aid_condition.RData")

print(load("linear_models/acc_model_aid_condition.RData"))
print(summary(acc_brms))
pandoc.table(make_fixef_table(acc_brms))

acc_onset_emm <- emmeans(
  acc_brms,
  ~ aid_condition,
  epred = TRUE
)
print(acc_onset_emm)
pandoc.table(as.data.frame(acc_onset_emm))

acc_onset_pairs <- pairs(
  acc_onset_emm,
  adjust = "holm"
)
print(acc_onset_pairs)
pandoc.table(as.data.frame(acc_onset_pairs))

write_csv(as.data.frame(acc_onset_emm), "linear_models/acc_emmeans_aid_condition.csv")
write_csv(as.data.frame(acc_onset_pairs), "linear_models/acc_condition_pairs.csv")

# -------------------------------------------------------------------------
# RT model
# -------------------------------------------------------------------------

rt_formula <- bf(log_rt ~ aid_condition + (1 | subjects))

rt_priors <- c(
  prior(normal(0, 1), class = "Intercept"),
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 2.5), class = "sigma")
)

RT_brms <- brm(
  formula = rt_formula,
  data = rt_dat,
  family = gaussian(),
  prior = rt_priors,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 202103,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  file = "linear_models/RT_brms_model_aid_condition"
)

save(RT_brms, file = "linear_models/RT_model_aid_condition.RData")

print(load("linear_models/RT_model_aid_condition.RData"))
print(summary(RT_brms))
pandoc.table(make_fixef_table(RT_brms))

rt_onset_emm_log <- emmeans(
  RT_brms,
  ~ aid_condition
)
print(rt_onset_emm_log)
pandoc.table(as.data.frame(rt_onset_emm_log))

rt_onset_emm_sec <- emmeans(
  RT_brms,
  ~ aid_condition,
  type = "response"
)
print(rt_onset_emm_sec)
pandoc.table(as.data.frame(rt_onset_emm_sec))

rt_onset_pairs_log <- pairs(
  rt_onset_emm_log,
  adjust = "holm"
)
print(rt_onset_pairs_log)
pandoc.table(as.data.frame(rt_onset_pairs_log))

write_csv(as.data.frame(rt_onset_emm_log), "linear_models/rt_emmeans_aid_condition_log.csv")
write_csv(as.data.frame(rt_onset_emm_sec), "linear_models/rt_emmeans_aid_condition_seconds.csv")
write_csv(as.data.frame(rt_onset_pairs_log), "linear_models/rt_condition_pairs_log.csv")
