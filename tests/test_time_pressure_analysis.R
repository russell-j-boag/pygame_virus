# Run from the project root: Rscript tests/test_time_pressure_analysis.R
source("analyse_time_pressure.R")
expect_error <- function(expr) stopifnot(inherits(tryCatch({force(expr); NULL}, error = identity), "error"))

# Timeout accuracy is retained even when correctness is missing and RT is present.
raw <- tibble(participant_id = 1, run_timestamp = "test", condition_deadline_code = "A_HP",
              block_idx = 2, trial = 1:5, response = c("BLACK", "TIMEOUT", "WHITE", "BLACK", "TIMEOUT"),
              correct = c("TRUE", NA, "FALSE", "TRUE", NA), rt_s = c(.5, 1.001, .6, 0, NA),
              stimulus = "BLACK", aid_label = "BLACK", aid_correct = "TRUE",
              automation_reliability_pattern = "HP95_LP65", aid_accuracy_setting = .95,
              time_pressure_condition = "HP", trial_deadline_s = 1)
d <- prepare_trials(raw)
stopifnot(identical(d$accuracy, c(1L, 0L, 0L, 1L, 0L)), sum(d$valid_correct_rt) == 1,
          sum(!is.na(d$agreement)) == 3, mean(d$agreement, na.rm = TRUE) == 2/3)
expect_error(prepare_trials(bind_rows(raw, raw[1, ])))
bad <- raw; bad$correct[1] <- NA; expect_error(prepare_trials(bad))
bad <- raw; bad$correct[2] <- "TRUE"; expect_error(prepare_trials(bad))
bad <- raw; bad$aid_label[1] <- "WHITE"; expect_error(prepare_trials(bad))

# Known four-cell means test difference-in-differences signs and zero baselines.
grid <- expand.grid(mode = c("Manual", "Automation"), pressure = c("LP", "HP"), pattern = patterns)
mu <- with(grid, ifelse(mode == "Manual", ifelse(pressure == "HP", .70, .80),
                        ifelse(pattern == patterns[1], ifelse(pressure == "HP", .85, .81),
                               ifelse(pressure == "HP", .70, .90))))
w <- performance_contrasts(grid)
stopifnot(length(w) == 11,
          abs(sum(w[["HP95_LP65 HP gain - LP gain"]] * mu) - .14) < 1e-12,
          abs(sum(w[["HP65_LP95 HP gain - LP gain"]] * mu) + .10) < 1e-12,
          abs(sum(w[["HP95_LP65 Aided HP - Manual LP"]] * mu) - .05) < 1e-12,
          abs(sum(w[[names(w)[11]]] * mu) - .24) < 1e-12)

# Compare generic participant-score tests against independent paired/Welch tests.
set.seed(12)
synthetic <- bind_rows(lapply(1:12, function(i) {
  g <- if (i <= 6) patterns[1] else patterns[2]
  grid[grid$pattern == g, ] %>% mutate(participant_id = i, accuracy = mu[grid$pattern == g] + rnorm(4, 0, .025))
}))
tests <- participant_sensitivity(synthetic, "accuracy", grid, w, "accuracy", 100)
wide <- synthetic %>% mutate(cell = paste(mode, pressure, sep = "_")) %>%
  select(participant_id, pattern, cell, accuracy) %>% pivot_wider(names_from = cell, values_from = accuracy) %>%
  mutate(hp_gain = Automation_HP - Manual_HP, diff_gain = hp_gain - (Automation_LP - Manual_LP))
t1 <- t.test(wide$hp_gain[wide$pattern == patterns[1]])
t2 <- t.test(wide$diff_gain[wide$pattern == patterns[1]], wide$diff_gain[wide$pattern == patterns[2]])
r1 <- tests[tests$contrast == "HP95_LP65 HP Automation - Manual", ]
r2 <- tests[tests$contrast == names(w)[11], ]
stopifnot(abs(r1$estimate - t1$estimate * 100) < 1e-10,
          abs(r1$lower - t1$conf.int[1] * 100) < 1e-10,
          abs(r1$upper - t1$conf.int[2] * 100) < 1e-10,
          abs(r1$p_raw - t1$p.value) < 1e-10,
          abs(r2$lower - t2$conf.int[1] * 100) < 1e-10,
          abs(r2$p_raw - t2$p.value) < 1e-10,
          all(tests$lower <= tests$estimate & tests$estimate <= tests$upper))

# Aided comparisons must retain within-person pairing where it exists.
ag <- expand.grid(pressure = c("LP", "HP"), reliability = c("65%", "95%"), advice = c("Incorrect", "Correct"))
aw <- aided_contrasts(ag, TRUE)
stopifnot(length(aw) == 10, all(vapply(aw, function(z) sum(z) == 0, logical(1))))
message("Passed timeout/RT, missing-data, duplicate, advice, contrast-sign, CI-scale, paired and Welch checks.")
