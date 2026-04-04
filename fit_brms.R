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

make_hypothesis_table <- function(hypothesis_summary) {
  out <- as.data.frame(hypothesis_summary$hypothesis)
  keep_cols <- intersect(
    c("Hypothesis", "Estimate", "CI.Lower", "CI.Upper", "Evid.Ratio", "Post.Prob", "Star"),
    names(out)
  )
  out <- out[, keep_cols, drop = FALSE]
  names(out) <- c(
    "hypothesis",
    "estimate",
    "lower_95",
    "upper_95",
    "evidence_ratio",
    "posterior_prob",
    "star"
  )[seq_along(keep_cols)]
  out
}

get_condition_weights <- function(data, prefix) {
  counts <- data %>%
    count(condition, name = "n")
  
  n_correct <- counts %>%
    filter(condition == paste0(prefix, "_Correct")) %>%
    pull(n)
  
  n_incorrect <- counts %>%
    filter(condition == paste0(prefix, "_Incorrect")) %>%
    pull(n)
  
  c(
    correct = n_correct / (n_correct + n_incorrect),
    incorrect = n_incorrect / (n_correct + n_incorrect)
  )
}

# Load current data
dat <- read_csv("data/data_virus.csv")
str(dat)

# Recode to cleaner single-factor condition structure
dat <- dat %>%
  mutate(
    C = as.integer(correct),
    subjects = factor(participant_id),
    
    block = factor(
      block,
      levels = c("CALIBRATION", "MANUAL", "AUTOMATION1", "AUTOMATION2"),
      labels = c("Calibration", "Manual", "Auto95", "Auto65")
    ),
    
    aid_correct = case_when(
      aid_correct %in% c(TRUE, 1, "1", "TRUE", "True", "true") ~ "Correct",
      aid_correct %in% c(FALSE, 0, "0", "FALSE", "False", "false") ~ "Incorrect",
      TRUE ~ NA_character_
    ),
    
    condition = case_when(
      block == "Calibration" ~ "Calibration",
      block == "Manual" ~ "Manual",
      block == "Auto95" & aid_correct == "Correct" ~ "Auto95_Correct",
      block == "Auto95" & aid_correct == "Incorrect" ~ "Auto95_Incorrect",
      block == "Auto65" & aid_correct == "Correct" ~ "Auto65_Correct",
      block == "Auto65" & aid_correct == "Incorrect" ~ "Auto65_Incorrect",
      TRUE ~ NA_character_
    ),
    
    condition = factor(
      condition,
      levels = c(
        "Calibration",
        "Manual",
        "Auto95_Correct",
        "Auto95_Incorrect",
        "Auto65_Correct",
        "Auto65_Incorrect"
      )
    )
  ) %>%
  group_by(subjects) %>%
  mutate(Trial = dplyr::row_number()) %>%
  ungroup() %>%
  filter(!is.na(C), !is.na(rt_s), !is.na(condition))

str(dat)
head(dat)
tail(dat)

# Subject-level summaries
accs <- dat %>%
  group_by(subjects, condition) %>%
  summarise(acc = mean(C), .groups = "drop") %>%
  arrange(subjects, condition)
accs 

rt_dat <- dat %>%
  filter(C == 1) %>%
  mutate(log_rt = log(rt_s))

RTs <- rt_dat %>%
  group_by(subjects, condition) %>%
  summarise(rt = mean(rt_s), .groups = "drop") %>%
  arrange(subjects, condition)
RTs

# -------------------------------------------------------------------------
# Accuracy model
# -------------------------------------------------------------------------

acc_formula <- bf(C ~ condition + (1 | subjects))

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
  file = "linear_models/acc_brms_model_condition"
)

save(acc_brms, file = "linear_models/acc_model_condition.RData")

print(load("linear_models/acc_model_condition.RData"))
print(summary(acc_brms))
pandoc.table(make_fixef_table(acc_brms))

acc_hypotheses <- hypothesis(
  acc_brms,
  c(
    "conditionManual = 0",
    "conditionAuto95_Correct = 0",
    "conditionAuto95_Incorrect = 0",
    "conditionAuto65_Correct = 0",
    "conditionAuto65_Incorrect = 0"
  )
)
pandoc.table(make_hypothesis_table(acc_hypotheses))

# Estimated marginal means on probability scale
acc_emm <- emmeans(
  acc_brms,
  ~ condition,
  epred = TRUE
)
print(acc_emm)
pandoc.table(as.data.frame(acc_emm))

# All pairwise condition comparisons
acc_pairs <- pairs(
  acc_emm,
  adjust = "holm"
)
print(acc_pairs)
pandoc.table(as.data.frame(acc_pairs))

# Frequency-weighted pooled contrasts for accuracy
w95_acc <- get_condition_weights(dat, "Auto95")
w65_acc <- get_condition_weights(dat, "Auto65")

acc_targeted_contrasts <- contrast(
  acc_emm,
  method = list(
    "Manual - Calibration" = c(-1, 1, 0, 0, 0, 0),
    "Manual - Auto95 pooled (freq-weighted)" =
      c(0, 1, -w95_acc["correct"], -w95_acc["incorrect"], 0, 0),
    "Manual - Auto65 pooled (freq-weighted)" =
      c(0, 1, 0, 0, -w65_acc["correct"], -w65_acc["incorrect"]),
    "Auto95: Correct - Incorrect" = c(0, 0, 1, -1, 0, 0),
    "Auto65: Correct - Incorrect" = c(0, 0, 0, 0, 1, -1),
    "Auto95 pooled - Auto65 pooled (freq-weighted within block)" =
      c(0, 0, w95_acc["correct"], w95_acc["incorrect"], -w65_acc["correct"], -w65_acc["incorrect"])
  ),
  adjust = "holm"
)
print(acc_targeted_contrasts)
pandoc.table(as.data.frame(acc_targeted_contrasts))

cat("\nAccuracy weights:\n")
cat("Auto95:", round(w95_acc["correct"], 4), "(Correct),", round(w95_acc["incorrect"], 4), "(Incorrect)\n")
cat("Auto65:", round(w65_acc["correct"], 4), "(Correct),", round(w65_acc["incorrect"], 4), "(Incorrect)\n")

write_csv(as.data.frame(acc_emm), "linear_models/acc_emmeans_condition.csv")
write_csv(as.data.frame(acc_pairs), "linear_models/acc_pairs_condition.csv")
write_csv(as.data.frame(acc_targeted_contrasts), "linear_models/acc_targeted_contrasts_condition.csv")

# -------------------------------------------------------------------------
# RT model
# -------------------------------------------------------------------------

rt_formula <- bf(log_rt ~ condition + (1 | subjects))

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
  file = "linear_models/RT_brms_model_condition"
)

save(RT_brms, file = "linear_models/RT_model_condition.RData")

print(load("linear_models/RT_model_condition.RData"))
print(summary(RT_brms))
pandoc.table(make_fixef_table(RT_brms))

rt_hypotheses <- hypothesis(
  RT_brms,
  c(
    "conditionManual = 0",
    "conditionAuto95_Correct = 0",
    "conditionAuto95_Incorrect = 0",
    "conditionAuto65_Correct = 0",
    "conditionAuto65_Incorrect = 0"
  )
)
pandoc.table(make_hypothesis_table(rt_hypotheses))

# Estimated marginal means on log scale
rt_emm_log <- emmeans(
  RT_brms,
  ~ condition
)
print(rt_emm_log)
pandoc.table(as.data.frame(rt_emm_log))

# Back-transformed estimated marginal means in seconds
rt_emm_sec <- emmeans(
  RT_brms,
  ~ condition,
  type = "response"
)
print(rt_emm_sec)
pandoc.table(as.data.frame(rt_emm_sec))

# All pairwise condition comparisons on log scale
rt_pairs_log <- pairs(
  rt_emm_log,
  adjust = "holm"
)
print(rt_pairs_log)
pandoc.table(as.data.frame(rt_pairs_log))

# Frequency-weighted pooled contrasts for RT
# Use the RT dataset because the RT model is fit only to correct-response trials
w95_rt <- get_condition_weights(rt_dat, "Auto95")
w65_rt <- get_condition_weights(rt_dat, "Auto65")

rt_targeted_contrasts_log <- contrast(
  rt_emm_log,
  method = list(
    "Manual - Calibration" = c(-1, 1, 0, 0, 0, 0),
    "Manual - Auto95 pooled (freq-weighted)" =
      c(0, 1, -w95_rt["correct"], -w95_rt["incorrect"], 0, 0),
    "Manual - Auto65 pooled (freq-weighted)" =
      c(0, 1, 0, 0, -w65_rt["correct"], -w65_rt["incorrect"]),
    "Auto95: Correct - Incorrect" = c(0, 0, 1, -1, 0, 0),
    "Auto65: Correct - Incorrect" = c(0, 0, 0, 0, 1, -1),
    "Auto95 pooled - Auto65 pooled (freq-weighted within block)" =
      c(0, 0, w95_rt["correct"], w95_rt["incorrect"], -w65_rt["correct"], -w65_rt["incorrect"])
  ),
  adjust = "holm"
)
print(rt_targeted_contrasts_log)
pandoc.table(as.data.frame(rt_targeted_contrasts_log))

cat("\nRT weights:\n")
cat("Auto95:", round(w95_rt["correct"], 4), "(Correct),", round(w95_rt["incorrect"], 4), "(Incorrect)\n")
cat("Auto65:", round(w65_rt["correct"], 4), "(Correct),", round(w65_rt["incorrect"], 4), "(Incorrect)\n")

write_csv(as.data.frame(rt_emm_log), "linear_models/rt_emmeans_condition_log.csv")
write_csv(as.data.frame(rt_emm_sec), "linear_models/rt_emmeans_condition_seconds.csv")
write_csv(as.data.frame(rt_pairs_log), "linear_models/rt_pairs_condition_log.csv")
write_csv(as.data.frame(rt_targeted_contrasts_log), "linear_models/rt_targeted_contrasts_condition_log.csv")
