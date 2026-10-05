source("analyse_time_pressure.R")
source("analyse_time_pressure_exploratory.R")

# Each aid/human outcome, including timeouts on correct AND incorrect advice.
d <- tibble(participant_id = 1L, subject = factor(1), run_timestamp = "r1", block_idx = 2L,
  trial = 1:6, pattern = "HP95_LP65", pressure = "HP", reliability = "95%", mode = "Automation",
  advice = c("Incorrect", "Correct", "Correct", "Incorrect", "Correct", "Incorrect"),
  accuracy = c(1, 0, 0, 0, 1, 0), timeout = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
  agreement = c(0, 0, NA, NA, 1, 1))
b <- benchmark_components(d)
stopifnot(b$rescued == 1/6, b$spoiled == 1/6, b$missed_correct_advice == 1/6,
          abs(b$net_gain + 1/6) < 1e-12,
          abs(b$net_gain - b$rescued + b$spoiled + b$missed_correct_advice) < 1e-12)

# Lagging does not bridge timeouts, missing trials, different blocks, or runs.
seq <- make_sequence(d)
stopifnot(identical(seq$trial, c(2L, 6L)), all(seq$previous_trial == seq$trial - 1),
          as.character(seq$previous_aid_error[1]) == "Error", seq$previous_response_error[1] == 0)
two_blocks <- bind_rows(d, mutate(d, block_idx = 3L))
stopifnot(nrow(make_sequence(two_blocks)) == 4)
gap <- d %>% filter(trial != 5)
stopifnot(identical(make_sequence(gap)$trial, 2L))
two_runs <- bind_rows(d, mutate(d, run_timestamp = "r2"))
stopifnot(nrow(make_sequence(two_runs)) == 4)

# Completion and conditional choice components sum exactly to total accuracy gain.
m <- d %>% mutate(mode = "Manual", timeout = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE), accuracy = c(1,1,0,1,0,0))
dec <- completion_components(bind_rows(d, m))
stopifnot(abs(dec$total_gain - dec$completion_component - dec$choice_component) < 1e-12)

# Selectivity and reliability-standardised lag contrasts have the intended signs.
g <- expand.grid(pressure = c("LP", "HP"), reliability = c("65%", "95%"), advice = c("Incorrect", "Correct"))
y <- with(g, ifelse(advice == "Correct", .9, .3) - ifelse(pressure == "HP" & advice == "Correct", .1, 0))
w <- selectivity_contrasts(g)
stopifnot(abs(sum(w[["65% HP - LP"]] * y) + .1) < 1e-12,
          abs(sum(w[["Reliability effect HP - LP"]] * y)) < 1e-12)
lg <- expand.grid(previous_aid_error = c("Correct", "Error"), pressure = c("LP", "HP"),
                   reliability = c("65%", "95%"), advice = c("Incorrect", "Correct"))
ly <- with(lg, ifelse(advice == "Correct", .9, .3) - ifelse(previous_aid_error == "Error", .1, 0))
lw <- lag_contrasts(lg)
stopifnot(length(lw) == 7, abs(sum(lw[[1]] * ly) + .1) < 1e-12,
          all(vapply(lw, function(z) abs(sum(z)) < 1e-12, logical(1))))

# Generic contrast export scales estimate, uncertainty and SE once, with zero tests.
fit <- lm(mpg ~ factor(cyl), data = mtcars)
em <- emmeans(fit, ~cyl)
tab <- exploratory_table(em, list(zero = c(1,0,0), difference = c(-1,1,0)), "Htest", "test", "test", "pp", 100)
expected <- as.data.frame(confint(contrast(em, list(zero = c(1,0,0), difference = c(-1,1,0)))))
stopifnot(max(abs(tab$estimate - expected$estimate * 100)) < 1e-10,
          max(abs(tab$lower - expected$lower.CL * 100)) < 1e-10)
message("Exploratory unit checks passed: accounting identities, lag boundaries, selectivity, reliability weights and CI scaling.")
