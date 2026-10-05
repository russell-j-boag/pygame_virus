# pygame_virus

Experimental task built in Pygame for studying decision-making and automation use.
Based on Bartlett & McCarley RDC task.

## Structure
- Python experiment code in `/python`
- Output data saved to `/output`
- Plots saved to `/plots`
- Analysis conducted in R

## Getting started
- Run `0-setup_pygame.R` to install the required versions of python and pygame
- Use `1-run_virus_task.R` to launch task and pre-task instructions using the helper functions

## Screenshot review deck

Use the `r-pygame` Python interpreter to render static screenshots and assemble a PowerPoint review deck:

```sh
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python python/capture_instruction_screenshots.py --overwrite
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python python/capture_virus_task_screenshots.py --overwrite --participant 1
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python python/build_screenshot_deck.py --overwrite
```

By default, the screenshot scripts render at `1512x982`, matching the current participant-facing display aspect ratio. Pass `--resolution current` to detect the active display size, or pass another explicit `WIDTHxHEIGHT` value to both screenshot commands.
The deck builder sizes the PowerPoint slides to the screenshot aspect ratio rather than PowerPoint's default slide shape.
The generated `instruction_screenshots/`, `virus_task_screenshots/`, and `screenshots_review.pptx` artifacts are local review outputs and are ignored by git.
The deck builder requires `pandoc` and Pillow.

## Current task design

This design extends Wanghuan's reliability-drop design by additionally manipulating participants' unaided performance level through calibration. Calibration always occurs first. Participants are assigned deterministically from participant ID to one of two calibration targets:

| Calibration group | Target unaided accuracy |
| --- | ---: |
| `CAL65` | 65% |
| `CAL90` | 90% |

After calibration, participants complete the first 200 trials of the manual comparison block, the full-length aided reliability-drop block, and then the remaining 200 manual comparison trials. The manual trials provide direct unaided-performance comparisons at the calibration-derived difficulty before and after the aided sequence.

| Code | Mode | Response window | Trials |
| --- | --- | ---: | ---: |
| `CAL` | Manual calibration | 5 s | 300 |
| `MAN` | Manual comparison, pre-automation segment | 5 s | 200 |
| `REL_DROP` | Aided reliability drop | 5 s | 1200 |
| `MAN` | Manual comparison, post-automation segment | 5 s | 200 |

The manual and aided blocks both use the calibration-derived fixed difficulty for the participant. In the aided block, the aid appears with the stimulus, and aid onset is not manipulated.

The aided block uses the reliability sequence `95% -> 70% -> 95%`. Each phase contains 400 trials:

| Phase | Aid accuracy | Trials |
| --- | ---: | ---: |
| 1 | 95% | 400 |
| 2 | 70% | 400 |
| 3 | 95% | 400 |

Participant-facing automation instructions are qualitative rather than numeric. Participants are told that the aid's recommendations may be correct or incorrect, but they are not told whether, when, or how the aid's reliability changes over time.

## Research questions

The critical phase is the 70% aided block. For `CAL65` participants, the 70% aid remains potentially useful because it is 5 percentage points more accurate than their calibrated unaided performance. For `CAL90` participants, the 70% aid is 20 percentage points less accurate than their own calibrated performance and should therefore be discounted or ignored.

This design tests whether participants respond only to absolute changes in aid reliability, or whether they learn the relative value of the aid compared with their own competence. It also tests whether prior exposure to a highly reliable aid produces over-reliance when the aid later becomes only moderately reliable, especially when the aid is no longer objectively useful.

## Counterbalancing

- Calibration target: participant IDs alternate between `CAL65` and `CAL90`.
- Main block sequence: all participants complete `CAL -> MAN/PRE_AUTOMATION -> REL_DROP -> MAN/POST_AUTOMATION`.
- Key mapping: standard for the first two participants within each four-participant cycle, flipped for the next two participants.

This gives a complete four-participant counterbalance over calibration target and key mapping:

| Participant cycle position | Calibration group | Key mapping | Main block sequence |
| ---: | --- | --- | --- |
| 1 | `CAL65` | `D = V-BLACK`, `J = V-WHITE` | `SPLIT_MANUAL` |
| 2 | `CAL90` | `D = V-BLACK`, `J = V-WHITE` | `SPLIT_MANUAL` |
| 3 | `CAL65` | `J = V-BLACK`, `D = V-WHITE` | `SPLIT_MANUAL` |
| 4 | `CAL90` | `J = V-BLACK`, `D = V-WHITE` | `SPLIT_MANUAL` |

The standard key mapping is `D = V-BLACK` and `J = V-WHITE`. The flipped key mapping is `J = V-BLACK` and `D = V-WHITE`.

Participants report perceived self accuracy after calibration and after each manual segment. The 1200-trial aided block is not interrupted by questionnaires; after the full aided reliability-drop block, participants report perceived automation accuracy, perceived self accuracy, and trust in the aid for the whole aided sequence.

## Output fields

The main trial output files include fields that identify the design cell:

| Field | Meaning |
| --- | --- |
| `block` | `CALIBRATION`, `MANUAL`, or `AUTOMATION` |
| `condition_code` | `CAL`, `MAN`, or `REL_DROP` |
| `calibration_target_group` | Participant-ID assigned calibration group, `CAL65` or `CAL90` |
| `calibration_target_accuracy` | Calibration target accuracy, `0.65` or `0.90` |
| `main_block_order` | Fixed main-block sequence label, `SPLIT_MANUAL` |
| `manual_segment` | Manual segment label, `PRE_AUTOMATION` or `POST_AUTOMATION`; blank for non-manual rows |
| `reliability_phase_idx` | Aided reliability phase index, `1`-`3` |
| `trial_in_reliability_phase` | Trial index within the current reliability phase |
| `reliability_phase_label` | Phase label such as `P1_95`, `P2_70`, or `P3_95` |
| `aid_reliability_level` | Current aided-phase aid accuracy level |
| `automation_reliability_group` | High/low grouping derived from aid reliability |
| `trial_deadline_s` | Fixed response window in seconds |
| `rt_s` | Response time in seconds; this is the canonical RT field |

Post-block output files keep the block-level design fields plus `reliability_phase_label`, `postblock_scope`, `trial_deadline_s`, and the question/response fields. For automation post-block ratings, `reliability_phase_label` is `DROP95_70_95`; single-phase fields such as `reliability_phase_idx`, `trial_in_reliability_phase`, `aid_reliability_level`, and `automation_reliability_group` are omitted because the ratings refer to the complete aided block rather than one 400-trial phase.

Single-block runs can be selected for the current task sequence:

```r
run_task(block = "CAL")
run_task(block = "MAN/PRE_AUTOMATION")
run_task(block = "REL_DROP")
run_task(block = "MAN/POST_AUTOMATION")
```

Legacy aliases `CALIBRATION`, `MANUAL`, and `AUTOMATION` remain accepted for development checks.

## Author
Russell J. Boag

## Behavioural analysis

The frequentist pipeline analyses relative competence, the reliability drop and
recovery, with exploratory time courses, correct RT, manual pre/post changes and
subjective evaluations. All hypotheses were specified **after data collection**.
The legacy `fit_brms.R` uses obsolete aid-onset factors and must not be used for
this design.

Run from this repository:

```sh
Rscript analyse_dynamic_reliability.R
Rscript plot_dynamic_reliability_results.R
Rscript tests/test_dynamic_reliability_analysis.R
Rscript tests/validate_dynamic_reliability_outputs.R
```

Required packages: `dplyr`, `tidyr`, `readr`, `lme4`, `lmerTest`, `emmeans`,
`ggplot2`, `patchwork`, and `sandwich`. PDF structural validation uses `pdfinfo`;
rendered review uses `pdftoppm`. The analysis does not install packages or launch
the participant task.

The analysis accepts `[input_dir] [output_dir]`, defaulting to
`output/semester2_2026_data` and `analysis_outputs/semester2_2026_behavioural`.
The plotting script accepts `[analysis_dir] [plot_dir]`, defaulting to the same
analysis directory and `plots/semester2_2026_behavioural`. Paths supplied on the
command line are relative to the calling directory. Generated outputs are ignored
by Git; task exports and existing descriptive plots are not modified.

### Questions and estimands

| Label | Question | Central comparison |
| --- | --- | --- |
| H1 | Does CAL90 reduce agreement with incorrect advice more than CAL65 at the drop? | Group difference in P2 minus P1 agreement; a negative CAL90-minus-CAL65 interaction matches this prediction. |
| H2 | Does the degraded aid benefit CAL65 more than CAL90? | P2 minus manual-pre accuracy within each group and the difference in those gains. |
| H3 | Does behaviour change when reliability recovers, and does it differ from its initial level? | P3 minus P2 and P3 minus P1 accuracy and agreement. |
| E1 | Does behaviour adapt within phases? | Phase-specific first-to-last-trial model contrasts and participant linear probability slopes. |
| E2 | What changes in response speed and manual performance accompany the sequence? | Log-RT contrasts and manual-post minus manual-pre accuracy/RT. |
| E3 | How do final evaluations relate to group and degraded-phase behaviour? | Trust and perceived aid accuracy error: group differences and group-adjusted associations with P2 incorrect-advice agreement. |

The core accuracy registry has 18 contrasts: P1/P2/P3 minus manual-pre and the
three pairwise phase differences, each within CAL65, within CAL90 and as
CAL90-minus-CAL65 differences in change. Core agreement has 18: the three phase
differences, each within/between groups, separately for correct and incorrect
advice. Manual-post baseline sensitivity has nine accuracy contrasts. Correct RT
uses the same 18 contrasts as accuracy. Manual pre/post families have three
contrasts per outcome. Adaptation has nine accuracy and 18 agreement contrasts.

### Cohort, outcomes and models

The current cohort contains 67 complete participants (34 CAL65, 33 CAL90), but
sample size is discovered from exports. Every retained run must have 300
calibration trials, 200 manual-pre trials, three 400-trial aided phases, and 200
manual-post trials. The pipeline validates allocation, key mapping, sequence,
trial identities, response correctness, advice correctness and rating scopes.
Duplicate complete runs are rejected. Missing or invalid questionnaire items are
reported as input errors rather than silently excluding participants; resolve
such inputs explicitly before rerunning.

Questionnaires and sliders are loaded from the **exact same-run filenames** as
the trial export. Legacy trust exports do not contain a timestamp column, so
their provenance relies on the filename and participant/design checks. Input
hashes and source hashes are recorded. Calibration's final 150 trials are a
descriptive manipulation check; calibration and manual accuracy thresholds do
not exclude otherwise valid participants.

- **Accuracy:** all 1,600 experimental trials per participant; timeouts count as
  incorrect. Answered trials with invalid RT remain in accuracy and agreement.
- **Correct RT:** correct responses with finite positive `rt_s`; log RT for
  modelling. Sub-100-ms and beyond-deadline responses are audited, not trimmed.
- **Agreement:** `response == aid_label` on answered aided trials, separately by
  advice correctness. Timeouts are excluded from this denominator.
- **Trust:** mean of six valid 1–5 items, once after the full aided sequence.
- **Perceived aid error:** perceived aid accuracy minus the participant's realised
  whole-block aid accuracy, in percentage points. Manual self-ratings are shown
  descriptively. These exports contain no automation self-accuracy rating or
  phase-specific subjective ratings.

Accuracy uses a logistic mixed model and correct RT a linear mixed model of log
RT, both with `group * stage + (1 | participant)`. Stage distinguishes manual-pre,
P1, P2, P3 and manual-post. Agreement uses a logistic mixed model with
`group * phase * advice_correctness + (1 | participant)`. Every mixed model has
participant random intercepts only. The binary outcomes overlap: among answered
trials, agreement equals accuracy for correct advice and one minus accuracy for
incorrect advice; they are not independent evidence streams.

Binary contrasts are percentage-point differences after response-scale
regridding. Model means condition on a zero participant random effect and are
not population-averaged probabilities. Correct-RT effects are geometric-mean
ratios; figures express benefits as `100 * (1 - ratio)`, with interval endpoints
reversed appropriately. Descriptive RT profiles show arithmetic means.
Between-group RT interactions exponentiate to ratios of ratios, not differences
in percentage speed benefits; those comparisons remain in the numerical report,
while the speed-benefit figure shows the interpretable within-group reductions.

Participant sensitivity analyses use equally weighted individual contrast
scores: paired one-sample tests within groups, Welch-Satterthwaite comparisons
between groups. A participant missing a required analysis cell contributes to
other contrasts but not that contrast. These analyses preserve participant-level
dependence; their estimands differ from conditional mixed-model estimates.

Time-course models add phase-specific linear progress and its full interactions
to the aided accuracy/agreement formula. Progress runs from 0 to 1 independently
within each phase. Curves show predicted probabilities; contrasts compare phase
end with start. Sensitivities use individual linear probability slopes, which
are a robustness check rather than an identical nonlinear estimand. Descriptive
trajectories use fixed nonoverlapping 100-trial bins, equal participant weights,
and displayed contributor counts. Empty advice cells are missing, not zero.
No curve crosses a phase boundary, and no precise learning time is estimated.

Subjective outcomes use participant-level linear models with HC3 standard
errors: a group-only model, and a separate association model with group plus
grand-mean-centred P2 incorrect-advice agreement (scaled per 10 percentage points).
The latter estimates a common group-adjusted slope. One rating per participant
does not warrant a participant random intercept.

All tests are two-sided. Holm correction is separate within outcome, contrast
family and method; subjective tests form a two-test family per outcome.
Confidence intervals are pointwise and unadjusted. Do not select whichever
method produces significance or treat the separate families as study-wide
multiplicity control.

### Diagnostics, interpretation and deliverables

The pipeline records convergence, singularity, rank deficiency, gradients,
extreme fitted probabilities, conditional binomial cell discrepancies and RT
residual diagnostics. Failed fits do not supply inferential intervals or tests.
Conditional cell simulations hold fitted parameters fixed and do not refit;
they are descriptive diagnostics, not calibrated goodness-of-fit tests. DHARMa
availability is reported explicitly, but it is not required or run.

The current fits show extra participant-cell variation despite convergence.
Interpret small model-only findings cautiously and read all participant
sensitivities alongside the models. Random-intercept-only fits do not model all
individual differences in phase responses or serial dependence. Correct RT also
conditions on correctness, and its residual spread differs across cells.

Everyone receives the same 95% → 70% → 95% order: phase differences include time,
practice and fatigue. Calibration groups also differ in visual difficulty.
Agreement is not proof of advice-caused switching or a pure measure of reliance.
Neither a nonsignificant P3-minus-P1 contrast nor a nonsignificant aided-minus-
manual contrast establishes equivalence. Manual-post is a sensitivity baseline,
not an untreated control. Rating associations are not causal effects.

The analysis bundle contains an HTML/text report with central comparisons,
the hypothesis registry, model objects, diagnostics, participant summaries,
contrast weights, key results, model-versus-participant comparisons, source
manifests and session information. `COMPLETE.txt` means computation completed,
not that every model passed or every hypothesis was supported. Plotting rejects
changed raw inputs, analysis sources or analysis artifacts.

The figure bundle has nine PDF/300-dpi PNG pairs: sequence accuracy, automation
benefits, relative competence, recovery, adaptation trajectories, correct RT,
manual pre/post, subjective evaluation, and descriptive manual self-ratings.
Every panel has exported source data. The additional manual self-rating figure
keeps the subjective-evaluation figure legible. Titles are question-based so they
cannot retain stale conclusions after a future rerun. Structural validation
does not substitute for rendered visual inspection.
Open `plots/semester2_2026_behavioural/index.html` to browse all nine figures and
their PDF/PNG downloads.
