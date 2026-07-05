rm(list = ls())

library("dplyr")
library("ggplot2")
library("broom")
library("zoo")
library("readr")

TARGET_ACC <- 0.85
BURNIN_TRIALS <- 20

files <- list.files(
  "output",
  pattern = "^results_.*_b00_PRACTICE\\.csv$",
  full.names = TRUE
)

if (!length(files)) {
  stop("No practice calibration results files found in output/.")
}

latest_file <- files[which.max(file.info(files)$mtime)]
print(latest_file)

dat <- read_csv(latest_file, show_col_types = FALSE)

if (!"decision2_correct" %in% names(dat) && "correct" %in% names(dat)) {
  dat <- dat %>%
    mutate(decision2_correct = correct)
}
if (!"condition_code" %in% names(dat) && "block" %in% names(dat)) {
  dat <- dat %>%
    mutate(condition_code = block)
}

# Keep only practice calibration staircase trials
cal <- dat %>%
  filter(condition_code == "PRACTICE", difficulty_mode == "staircase") %>%
  arrange(trial) %>%
  mutate(
    correct_num = as.integer(decision2_correct),
    stair_trial = row_number(),                  # trial index within staircase
    difficulty = delta_stair_realised                # or delta_stair_realised; see note below
  )

if (nrow(cal) == 0) {
  message("No practice calibration trials found in latest results file; skipping stationarity checks.")
  quit(save = "no", status = 0)
}

# Define burn-in period
burn_in <- BURNIN_TRIALS
cal_post <- cal %>%
  filter(stair_trial > burn_in)

if (nrow(cal_post) == 0) {
  message("No post-burn-in calibration trials available; skipping stationarity checks.")
  quit(save = "no", status = 0)
}

# Visual check difficulty across trials
ggplot(cal, aes(stair_trial, difficulty)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_vline(xintercept = burn_in, linetype = "dashed") +
  labs(
    x = "Staircase trial",
    y = "Difficulty",
    title = "Staircase difficulty over trials"
  ) + 
  theme_classic()


# Rolling accuracy
cal <- cal %>%
  mutate(
    acc_roll_10 = zoo::rollmean(correct_num, k = 10, fill = NA, align = "right")
  )

ggplot(cal, aes(stair_trial, acc_roll_10)) +
  geom_line() +
  geom_hline(yintercept = TARGET_ACC, linetype = "dashed") +
  geom_vline(xintercept = burn_in, linetype = "dashed") +
  labs(
    x = "Staircase trial",
    y = "Rolling mean accuracy (10 trials)",
    title = "Rolling accuracy during calibration"
  ) + 
  ylim(c(0, 1)) +
  theme_classic()


# Test slope of difficulty vs trial
fit_diff <- lm(difficulty ~ stair_trial, data = cal_post)
summary(fit_diff)
tidy(fit_diff, conf.int = TRUE)


# cal_post <- cal_post %>%
#   mutate(trial_post = row_number())
# 
# fit_diff <- lm(difficulty ~ trial_post, data = cal_post)
# summary(fit_diff)
# tidy(fit_diff, conf.int = TRUE)


# Test slope of correctness vs trial
fit_acc_trend <- glm(correct_num ~ stair_trial,
                     data = cal_post,
                     family = binomial())

summary(fit_acc_trend)
tidy(fit_acc_trend, conf.int = TRUE)

# Estimate mean accuracy after burn-in
acc_mean <- cal_post %>%
  summarise(
    n = n(),
    n_correct = sum(correct_num),
    mean_acc = mean(correct_num)
  )

acc_mean

# Compare mean accuracy to target accuracy
acc_test <- binom.test(
  x = sum(cal_post$correct_num),
  n = nrow(cal_post),
  p = TARGET_ACC
)

acc_test

# Confidence interval around mean accuracy
prop.test(
  x = sum(cal_post$correct_num),
  n = nrow(cal_post),
  p = TARGET_ACC,
  correct = FALSE
)

# or
binom.test(
  x = sum(cal_post$correct_num),
  n = nrow(cal_post)
)$conf.int

# Example write-up:
# After excluding the first 20 practice calibration trials as burn-in,
# we tested whether the staircase had stabilized. A linear model 
# showed no reliable remaining trend in staircase difficulty over 
# trial number. A logistic regression likewise showed no remaining 
# trend in correctness over trial number. Mean post-burn-in accuracy 
# was close to the calibration target under an exact
# binomial test.
