# Independent source, covariance, model-contrast and provenance checks.
# Rscript tests/validate_aid_onset_slides.R [figure_dir]
suppressPackageStartupMessages(library(lme4))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 1L)
out <- if (length(args)) args[[1]] else "plots/semester2_2026_key_findings/presentation"
base <- "analysis_outputs/semester2_2026_presentation_followups"
old <- "analysis_outputs/semester2_2026_hypotheses"
read <- function(path) read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
close <- function(a, b) stopifnot(length(a) == length(b), all(is.finite(a)), all(is.finite(b)), max(abs(a-b)) < 1e-9)
pclose <- function(a, b) {
  stopifnot(identical(a == 0, b == 0))
  if (any(a > 0)) close(log(a[a > 0]), log(b[b > 0]))
}
catalog <- read(file.path(out, "figure_catalog.csv"))
source <- read("data/semester2_2026_averaged_within_participants.csv")
source <- source[order(source$subject_no), ]
raw <- read("data/data_virus_all.csv")
primary <- read(file.path(old, "primary_hypothesis_tests.csv"))
paired <- read(file.path(old, "participant_level_sensitivity_tests.csv"))
cells <- read(file.path(base, "participant_cells.csv"))
tests <- read(file.path(base, "contrasts.csv"))
stopifnot(identical(catalog$hypothesis, paste0("H", 1:8)),
  all(catalog$width_in/catalog$height_in == 16/9),
  all(catalog$png_width == 3600), all(catalog$png_height == 2025),
  identical(catalog$normalization_k, c(2L, 3L, 5L, 5L, 3L, 2L, 2L, 2L)),
  all(grepl("[Ee]xploratory", catalog$analysis_origin[5:8])),
  catalog$source_hypothesis[8] == "Exploratory", catalog$filename[8] == "08_h8_trust",
  nrow(cells) == 720, nrow(tests) == 9)

# Reconstruct every new participant cell without either generator's helpers.
boolean <- function(x) tolower(as.character(x)) %in% c("true", "1")
d1 <- boolean(raw$decision1_correct); d2 <- boolean(raw$decision2_correct)
advice <- boolean(raw$aid_correct)
agree1 <- boolean(raw$decision1_matches_aid); agree2 <- boolean(raw$decision2_matches_aid)
aided <- raw$aid_condition != "manual"
stopifnot(nrow(raw) == 46800, all(agree1[aided] == (d1[aided] == advice[aided])),
  all(agree2[aided] == (d2[aided] == advice[aided])),
  identical(unique(raw$run_timestamp[raw$participant_id == 59]), "20260924_110831"))
reconstructed <- list()
for (v in unique(cells$variable)) {
  if (grepl("^(beneficial|harmful)_", v)) {
    condition <- sub("^(beneficial|harmful)_", "", v)
    eligible <- raw$aid_condition == condition
    response <- if (startsWith(v, "beneficial_")) !d1 & d2 else d1 & !d2
  } else if (grepl("^(reject_correct|accept_incorrect)_", v)) {
    condition <- sub("^(reject_correct|accept_incorrect)_", "", v)
    eligible <- raw$aid_condition == condition & (advice == startsWith(v, "reject_correct_"))
    response <- if (startsWith(v, "reject_correct_")) !agree2 else agree2
  } else {
    stopifnot(v %in% c("uptake_correct", "uptake_incorrect"))
    eligible <- raw$aid_condition == "stimulus_first" & !agree1 & (advice == (v == "uptake_correct"))
    response <- agree2
  }
  counts <- tapply(as.integer(response[eligible]), raw$participant_id[eligible], sum)
  denominators <- table(raw$participant_id[eligible])
  stopifnot(setequal(names(counts), as.character(1:60)), all(denominators > 0))
  idx <- as.character(1:60)
  z <- cells[cells$variable == v, ]; z <- z[order(z$subject_no), ]
  close(z$successes, counts[idx]); close(z$opportunities, as.numeric(denominators[idx]))
  close(z$value, 100*counts[idx]/as.numeric(denominators[idx]))
  reconstructed[[v]] <- z$value
}
stopifnot(sum(cells$opportunities[cells$hypothesis == "H7"]) == 5015)

# Check new GLMM contrasts directly from beta and covariance, without emmeans.
for (stem in c("h5_beneficial", "h5_harmful", "h7_selective_uptake")) {
  m <- readRDS(file.path(base, "models", paste0(stem, ".rds")))
  mf <- model.frame(m); lev <- levels(mf$level)
  design <- model.matrix(~level, data.frame(level = factor(lev, lev)))
  stopifnot(identical(colnames(design), names(fixef(m))), !isSingular(m),
    length(unique(mf$subject)) == 60, length(getME(m, "theta")) == 1L)
  expected_response <- if (stem == "h5_beneficial") as.integer(!d1 & d2) else
    if (stem == "h5_harmful") as.integer(d1 & !d2) else as.integer(agree2[raw$aid_condition == "stimulus_first" & !agree1])
  close(model.response(mf), expected_response)
  rows <- tests[tests$source_file == paste0("models/", stem, ".rds"), ]
  for (j in seq_len(nrow(rows))) {
    r <- rows[j, ]
    contrast <- design[match(r$first_level, lev), ] - design[match(r$second_level, lev), ]
    estimate <- sum(contrast*fixef(m))
    se <- sqrt(drop(contrast %*% vcov(m) %*% contrast))
    close(r$estimate, exp(estimate))
    close(r$conf_low, exp(estimate-qnorm(.975)*se)); close(r$conf_high, exp(estimate+qnorm(.975)*se))
    pclose(r$p_value_raw, 2*pnorm(-abs(estimate/se)))
  }
}
for (h in c("H5", "H7")) {
  indices <- which(tests$hypothesis == h)
  raw_p <- numeric(length(indices))
  for (j in seq_along(indices)) {
    r <- tests[indices[j], ]
    panel_cells <- cells[cells$hypothesis == h & cells$panel == r$panel, ]
    first <- panel_cells[panel_cells$level == r$first_level, ]; first <- first[order(first$subject_no), ]
    second <- panel_cells[panel_cells$level == r$second_level, ]; second <- second[order(second$subject_no), ]
    tt <- t.test(first$value-second$value)
    close(r$paired_difference_pp, tt$estimate)
    close(c(r$paired_conf_low_pp, r$paired_conf_high_pp), tt$conf.int)
    pclose(r$participant_p_raw, tt$p.value); raw_p[j] <- tt$p.value
  }
  method <- if (h == "H5") "holm" else "none"
  pclose(tests$p_value_adjusted[indices], p.adjust(tests$p_value_raw[indices], method))
  pclose(tests$participant_p_adjusted[indices], p.adjust(raw_p, method))
}
for (source_h in c("H3", "H4")) {
  r <- tests[tests$hypothesis == "H6" & tests$source_hypothesis == source_h, ]
  m <- primary[primary$hypothesis == source_h & primary$contrast == r$source_contrast, ]
  close(c(r$estimate, r$conf_low, r$conf_high), 1/c(m$estimate, m$conf_high, m$conf_low))
  pclose(r$p_value_raw, m$p_value_raw); pclose(r$p_value_adjusted, m$p_value_adjusted)
}
interaction <- read(file.path(base, "h6_interaction.csv"))
original_interaction <- read(file.path(old, "timing_advice_interaction.csv"))
original_interaction <- original_interaction[grepl(":", original_interaction$term, fixed = TRUE), ]
close(c(interaction$odds_ratio, interaction$conf_low, interaction$conf_high),
  1/c(original_interaction$odds_ratio, original_interaction$conf_high, original_interaction$conf_low))
pclose(interaction$p_value, original_interaction$p_value)
stopifnot(sum(tests$participant_disagreement) == 2,
  all(tests$panel[tests$participant_disagreement] == "Harmful revisions"),
  all(tests$second_level[tests$participant_disagreement] == "Manual"))

summaries <- list(); annotation_count <- 0L
for (i in seq_len(nrow(catalog))) {
  entry <- catalog[i, ]; stem <- entry$filename
  d <- read(file.path(out, "data", paste0(stem, "_data.csv")))
  people <- read(file.path(out, "data", paste0(stem, "_participants.csv")))
  a <- read(file.path(out, "data", paste0(stem, "_annotations.csv")))
  is_followup <- entry$hypothesis %in% c("H5", "H6", "H7")
  for (normalization in unique(d$normalization_set)) {
    s <- d[d$normalization_set == normalization, ]; pp <- people[people$normalization_set == normalization, ]
    variables <- strsplit(s$normalization_levels[1], ";", fixed = TRUE)[[1]]
    k <- length(variables)
    m <- if (is_followup) do.call(cbind, reconstructed[variables]) else as.matrix(source[, variables])
    if (entry$hypothesis %in% paste0("H", 2:4)) m <- m*100
    # Covariance projection removes participant intercept variance independently.
    C <- diag(k)-matrix(1/k, k, k)
    se <- sqrt(diag(C %*% cov(m) %*% C)/nrow(m) * k/(k-1))
    margin <- qt(.975, nrow(m)-1)*se; j <- match(s$variable, variables)
    close(s$estimate, colMeans(m)[j]); close(s$se, se[j])
    close(s$lower, (colMeans(m)-margin)[j]); close(s$upper, (colMeans(m)+margin)[j])
    close(s$morey_factor, rep(sqrt(k/(k-1)), nrow(s)))
    stopifnot(all(s$n == 60), all(s$k == k), nrow(pp) == 60*k,
      setequal(pp$subject_no, 1:60), !anyDuplicated(pp[c("subject_no", "variable")]))
    for (v in variables) {
      z <- pp[pp$variable == v, ]; z <- z[order(z$subject_no), ]
      close(z$value, m[, v])
    }
  }
  stopifnot(all(d$lower > entry$y_min), all(d$upper < entry$y_max),
    all(d$label_y > entry$y_min), all(a$text_y < entry$y_max), all(a$tip > max(d$upper)))
  source_h <- if (entry$hypothesis == "H8") "Exploratory" else entry$hypothesis
  expected <- if (is_followup) tests[tests$hypothesis == entry$hypothesis, ] else primary[primary$hypothesis == source_h, ]
  if (entry$hypothesis == "H8") stopifnot(a$hypothesis == "H8", a$source_hypothesis == "Exploratory")
  key <- function(z) if (is_followup) paste(z$panel, z$contrast) else z$contrast
  j <- match(key(a), key(expected))
  stopifnot(nrow(a) == nrow(expected), !anyNA(j))
  for (column in c("estimate", "conf_low", "conf_high")) close(a[[column]], expected[[column]][j])
  for (column in c("p_value_raw", "p_value_adjusted")) pclose(a[[column]], expected[[column]][j])
  flags <- (a$p_value_adjusted < .05) != (a$participant_p_adjusted < .05)
  stopifnot(identical(a$symbol, paste0(ifelse(a$p_value_adjusted < .05, "*", "ns"), ifelse(flags, "\u2020", ""))))
  for (row in seq_len(nrow(a))) {
    panel <- if (is_followup) d[d$panel == a$panel[row], ] else d
    panel <- panel[order(panel$x), ]
    stopifnot(setequal(panel$condition[c(a$x1[row], a$x2[row])], c(a$first_level[row], a$second_level[row])))
  }
  annotation_count <- annotation_count+nrow(a)
  for (ext in c("pdf", "png")) stopifnot(file.exists(file.path(out, paste0(stem, ".", ext))))
  summaries[[entry$hypothesis]] <- d
}
stopifnot(annotation_count == 20L)
manual_h3 <- subset(summaries$H3, condition == "Manual"); manual_h4 <- subset(summaries$H4, condition == "Manual")
close(unlist(manual_h3[c("estimate", "lower", "upper")]), unlist(manual_h4[c("estimate", "lower", "upper")]))
stopifnot(catalog$y_min[3] == catalog$y_min[4], catalog$y_max[3] == catalog$y_max[4])
for (path in c(file.path(base, "input_checksums.csv"), file.path(out, "data/input_checksums.csv"))) {
  manifest <- read(path)
  stopifnot(all(file.exists(manifest$path)), identical(unname(tools::md5sum(manifest$path)), manifest$md5))
}
for (stem in c("05_h5_overall_accuracy", "06_exploratory_trust", "08_exploratory_trust"))
  stopifnot(!file.exists(file.path(out, paste0(stem, ".png"))), !file.exists(file.path(out, paste0(stem, ".pdf"))))
checks <- c("PASS: eight primary presentation figures in H1-H8 order; exploratory origins retained.",
  "PASS: 720 new participant cells independently reconstructed from raw trials; 5015 H7 opportunities.",
  "PASS: all Morey intervals checked by covariance projection; identical H3/H4 Manual baseline.",
  "PASS: three GLMMs checked from fixed effects/covariance; H5/H7 paired sensitivities and adjustment families.",
  "PASS: H6 accuracy-to-error transformations and saved exploratory interaction.",
  "PASS: all 20 bracket tests, exact endpoints and both H5 sensitivity-disagreement flags.",
  "PASS: source checksums; retired filenames absent from active set.",
  "These are numerical checks. PDF/PNG rendering and visual QA are performed separately.")
writeLines(checks, file.path(out, "data/validation_checks.txt"))
cat(paste(checks, collapse = "\n"), "\n")
