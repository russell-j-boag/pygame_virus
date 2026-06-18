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
  if (!"aid_onset_condition" %in% names(data)) {
    data$aid_onset_condition <- NA_character_
  }
  if (!"aid_onset_ms" %in% names(data)) {
    data$aid_onset_ms <- NA_real_
  }
  if (!"automation_reliability_group" %in% names(data)) {
    data$automation_reliability_group <- NA_character_
  }
  data
}

derive_reliability_group <- function(data) {
  data %>%
    mutate(
      automation_reliability_group = case_when(
        !is.na(automation_reliability_group) & automation_reliability_group != "" ~
          as.character(automation_reliability_group),
        suppressWarnings(as.numeric(aid_reliability_level)) >= 0.90 ~ "high",
        suppressWarnings(as.numeric(aid_reliability_level)) < 0.90 &
          !is.na(suppressWarnings(as.numeric(aid_reliability_level))) ~ "low",
        TRUE ~ NA_character_
      )
    )
}

factor_aid_onset <- function(aid_onset_condition, aid_onset_ms) {
  onset_ms <- suppressWarnings(as.numeric(aid_onset_ms))
  onset <- case_when(
    !is.na(aid_onset_condition) & aid_onset_condition == "before" ~ "Aid before",
    !is.na(aid_onset_condition) & aid_onset_condition == "simultaneous" ~ "Aid simultaneous",
    !is.na(aid_onset_condition) & aid_onset_condition == "after" ~ "Aid after",
    onset_ms < 0 ~ "Aid before",
    onset_ms == 0 ~ "Aid simultaneous",
    onset_ms > 0 ~ "Aid after",
    TRUE ~ NA_character_
  )
  factor(onset, levels = c("Aid before", "Aid simultaneous", "Aid after"))
}

factor_reliability_group <- function(x) {
  factor(x, levels = c("high", "low"))
}

# Load current data
dat <- read_csv("data/data_virus.csv", show_col_types = FALSE)
str(dat)

# Recode to the new automation-only onset design.
dat <- dat %>%
  ensure_design_columns() %>%
  derive_reliability_group() %>%
  mutate(
    C = as.integer(correct),
    subjects = factor(participant_id),
    aid_onset = factor_aid_onset(aid_onset_condition, aid_onset_ms),
    automation_reliability_group = factor_reliability_group(automation_reliability_group)
  ) %>%
  group_by(subjects) %>%
  mutate(Trial = dplyr::row_number()) %>%
  ungroup() %>%
  filter(
    block == "AUTOMATION",
    !is.na(C),
    !is.na(rt_s),
    !is.na(aid_onset),
    !is.na(automation_reliability_group)
  )

str(dat)
head(dat)
tail(dat)

# Subject-level summaries
accs <- dat %>%
  group_by(subjects, automation_reliability_group, aid_onset) %>%
  summarise(acc = mean(C), .groups = "drop") %>%
  arrange(subjects, automation_reliability_group, aid_onset)
accs

rt_dat <- dat %>%
  filter(C == 1, rt_s > 0) %>%
  mutate(log_rt = log(rt_s))

RTs <- rt_dat %>%
  group_by(subjects, automation_reliability_group, aid_onset) %>%
  summarise(rt = mean(rt_s), .groups = "drop") %>%
  arrange(subjects, automation_reliability_group, aid_onset)
RTs

# -------------------------------------------------------------------------
# Accuracy model
# -------------------------------------------------------------------------

acc_formula <- bf(C ~ aid_onset * automation_reliability_group + (1 | subjects))

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
  file = "linear_models/acc_brms_model_aid_onset"
)

save(acc_brms, file = "linear_models/acc_model_aid_onset.RData")

print(load("linear_models/acc_model_aid_onset.RData"))
print(summary(acc_brms))
pandoc.table(make_fixef_table(acc_brms))

acc_onset_emm <- emmeans(
  acc_brms,
  ~ aid_onset | automation_reliability_group,
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

acc_reliability_emm <- emmeans(
  acc_brms,
  ~ automation_reliability_group | aid_onset,
  epred = TRUE
)

acc_reliability_pairs <- pairs(
  acc_reliability_emm,
  adjust = "holm"
)
print(acc_reliability_pairs)
pandoc.table(as.data.frame(acc_reliability_pairs))

write_csv(as.data.frame(acc_onset_emm), "linear_models/acc_emmeans_aid_onset.csv")
write_csv(as.data.frame(acc_onset_pairs), "linear_models/acc_onset_pairs_by_reliability.csv")
write_csv(as.data.frame(acc_reliability_pairs), "linear_models/acc_reliability_pairs_by_onset.csv")

# -------------------------------------------------------------------------
# RT model
# -------------------------------------------------------------------------

rt_formula <- bf(log_rt ~ aid_onset * automation_reliability_group + (1 | subjects))

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
  file = "linear_models/RT_brms_model_aid_onset"
)

save(RT_brms, file = "linear_models/RT_model_aid_onset.RData")

print(load("linear_models/RT_model_aid_onset.RData"))
print(summary(RT_brms))
pandoc.table(make_fixef_table(RT_brms))

rt_onset_emm_log <- emmeans(
  RT_brms,
  ~ aid_onset | automation_reliability_group
)
print(rt_onset_emm_log)
pandoc.table(as.data.frame(rt_onset_emm_log))

rt_onset_emm_sec <- emmeans(
  RT_brms,
  ~ aid_onset | automation_reliability_group,
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

rt_reliability_emm_log <- emmeans(
  RT_brms,
  ~ automation_reliability_group | aid_onset
)

rt_reliability_pairs_log <- pairs(
  rt_reliability_emm_log,
  adjust = "holm"
)
print(rt_reliability_pairs_log)
pandoc.table(as.data.frame(rt_reliability_pairs_log))

write_csv(as.data.frame(rt_onset_emm_log), "linear_models/rt_emmeans_aid_onset_log.csv")
write_csv(as.data.frame(rt_onset_emm_sec), "linear_models/rt_emmeans_aid_onset_seconds.csv")
write_csv(as.data.frame(rt_onset_pairs_log), "linear_models/rt_onset_pairs_by_reliability_log.csv")
write_csv(as.data.frame(rt_reliability_pairs_log), "linear_models/rt_reliability_pairs_by_onset_log.csv")
