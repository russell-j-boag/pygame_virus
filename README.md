# pygame_virus

Experimental task built in Pygame for studying decision-making and automation use.
Based on Bartlett & McCarley RDC task.

## Structure
- Python experiment code in `/python`
- Output data saved to `/output`
- Plots saved to `/plots`
- Analysis conducted in R

## Getting started
- Run `0-setup_pygame.R` to install Python, Pygame, and the pinned Psychtoolbox keyboard backend in `r-pygame`
- Use `1-run_virus_task.R` to launch task and pre-task instructions using the helper functions

## Response timing for new task runs

Trial responses use the asynchronous native keyboard queue from
`psychtoolbox==3.0.22.2`, on Windows x64 or macOS (Intel/Apple Silicon).
Pygame continues drawing the task at its existing animation rate. Keyboard events
are timestamped by the native backend independently of drawing; the frame which
retrieves an event does not determine its RT. See the
[PTB event timing documentation](https://psychtoolbox.org/docs/KbEventGet) and
[Python package builds](https://pypi.org/project/psychtoolbox/3.0.22.2/).

Stimulus onset is the first `GetSecs()` reading immediately after the first
stimulus `pygame.display.flip()`. RT is the original keyboard-event timestamp
minus that onset, in the **same PTB clock**. Calculations and CSV exports retain
floating-point seconds without rounding to integer milliseconds. The deadline
also starts at this onset. An event timestamped before the deadline remains
eligible when retrieved after it; an event at or after the deadline does not.
Timeouts have missing RT, response timestamp, and correctness.

The old loop could process a key before recording/drawing stimulus onset,
producing negative RTs. New runs ignore presses timestamped before onset and
continue the trial without a penalty or restart. A held response key must be
released before a fresh press of that key is eligible; the other key remains
available. The earliest eligible press wins (native queue order breaks exact
timestamp ties). There is no post-onset input flush, no minimum-RT threshold,
and no clamping or absolute-value conversion. Valid responses below 200 ms are
retained. Option/Alt+Q remains the hard-quit shortcut.

New trial exports include:

| Column | Meaning |
| --- | --- |
| `rt_timing_version` | `ptb_queue_flip_v1`; identifies the corrected acquisition convention. |
| `rt_backend_version` | Pinned native backend version, `3.0.22.2`. |
| `stimulus_onset_s` | PTB timestamp immediately after the first stimulus flip. |
| `response_timestamp_s` | Original PTB key-down timestamp, missing for timeouts. |
| `n_ignored_pre_onset_keydowns` | D/J key-down events after the initial held-state snapshot and before onset; excludes the preceding fixation period. |
| `response_readout_delay_s` | Time from the original keypress timestamp until the task retrieved it; this delay is not added to RT. |

Absolute timestamps are meaningful within their recording session, not as wall
clock dates or for comparing clocks across computers. `collate_data.R` retains
these optional columns in both trial CSVs. Historical exports keep missing audit
fields and their original RTs; neither existing data nor exclusion rules are
changed by this fix.

Before collecting new data, run the native check **on each collection computer**
using its `r-pygame` Python interpreter (the examples use an activated environment):

```sh
python python/check_response_timing.py --preflight
python python/check_response_timing.py
```

The first command checks queue creation, startup, state queries, reads, stop,
and release. The second opens a separate diagnostic window: press and release
D or J for each of eight prompts. It inserts 0/8/20/50 ms main-thread stalls and
records original event timestamps plus retrieval delays in a new JSON file under
`analysis_outputs/response_timing/`. `--output PATH` selects a new output file;
existing files are never overwritten. This is a diagnostic, not a participant run.
Esc cancels it. A timeout is reported as missing response evidence, not a passed
keypress check.

On macOS, Input Monitoring permission may be needed for the application launching
Python, such as Terminal or RStudio; restart that application after granting it.
Task startup fails visibly if the pinned backend or native queue is unavailable;
there is no silent fallback to frame-based response timing. Native checks must
run outside restrictive sandboxes that prevent access to system input APIs.

Software tests exercise delayed retrieval, onset/deadline boundaries, held keys,
both key mappings, every block/phase, cleanup, and sub-millisecond CSV/collation
precision:

```sh
python -m unittest discover -s tests -p 'test_response_timing.py'
python -m unittest discover -s tests -p 'test_participant_qc.py'
```

The input timestamps are independent of the rendering loop, but the display
timestamp is still a software estimate of onset. These checks do not establish
physical ±1 ms accuracy for the keyboard/display. Native checks on one operating
system do not establish validation on the other; run the diagnostic on both
Windows and macOS collection machines.

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
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python participant_qc/run_qc.py
Rscript analyse_dynamic_reliability_primary.R
Rscript plot_dynamic_reliability_hypotheses.R
Rscript tests/validate_dynamic_reliability_primary.R
Rscript tests/test_dynamic_reliability_analysis.R
Rscript tests/validate_dynamic_reliability_outputs.R
Rscript tests/validate_dynamic_reliability_slides.R
```

Required packages: `dplyr`, `tidyr`, `readr`, `lme4`, `lmerTest`, `emmeans`,
`ggplot2`, `patchwork`, `sandwich`, `jsonlite`, and `digest`. PDF validation uses
`pdfinfo`, `pdftotext` and `pdfunite`;
rendered review uses `pdftoppm`. The analysis does not install packages or launch
the participant task.

The analysis accepts `[input_dir] [output_dir]`, defaulting to
`output/semester2_2026_data` and `analysis_outputs/semester2_2026_behavioural`.
The plotting script accepts `[analysis_dir] [plot_dir]`, defaulting to the same
analysis directory and `plots/semester2_2026_behavioural`. Paths supplied on the
command line are relative to the calling directory. Generated outputs are ignored
by Git; task exports and existing descriptive plots are not modified.

### Historical questions and estimands

These labels describe the preserved original analysis. The revised primary H1-H7
framework below supersedes their presentation numbering and correction families.

| Label | Question | Central comparison |
| --- | --- | --- |
| H1 | Does CAL90 reduce agreement with incorrect advice more than CAL65 at the drop? | Group difference in P2 minus P1 agreement; a negative CAL90-minus-CAL65 interaction matches this prediction. |
| H2 | Does the degraded aid benefit CAL65 more than CAL90? | P2 minus manual-pre accuracy within each group and the difference in those gains. |
| H3 | Does behaviour change when reliability recovers, and does it differ from its initial level? | P3 minus P2 and P3 minus P1 accuracy and agreement. |
| H4 (original E3) | How do final evaluations relate to group and degraded-phase behaviour? | Trust and perceived aid accuracy error: group differences and group-adjusted associations with P2 incorrect-advice agreement. |
| E1 | Does behaviour adapt within phases? | Phase-specific first-to-last-trial model contrasts and participant linear probability slopes. |
| E2 | What changes in response speed and manual performance accompany the sequence? | Log-RT contrasts and manual-post minus manual-pre accuracy/RT. |

H4 is promoted from exploratory E3 to the primary presentation set. The saved
analysis registry retains E3 as its source label; the figure catalog records
both labels. Its estimates and original Holm adjustment family are retained.

The core accuracy registry has 18 contrasts: P1/P2/P3 minus manual-pre and the
three pairwise phase differences, each within CAL65, within CAL90 and as
CAL90-minus-CAL65 differences in change. Core agreement has 18: the three phase
differences, each within/between groups, separately for correct and incorrect
advice. Manual-post baseline sensitivity has nine accuracy contrasts. Correct RT
uses the same 18 contrasts as accuracy. Manual pre/post families have three
contrasts per outcome. Adaptation has nine accuracy and 18 agreement contrasts.

### Cohort, outcomes and models

The complete cohort contains 89 participants (45 CAL65, 44 CAL90); sample size
is discovered from exports. The unified hypothesis report applies the agreed QC
rules for its 82-person primary sample and retains all 89 as sensitivity. Every retained run must have 300
calibration trials, 200 manual-pre trials, three 400-trial aided phases, and 200
manual-post trials. The pipeline validates allocation, key mapping, sequence,
trial identities, response correctness, advice correctness and rating scopes.
Duplicate complete runs are rejected. Missing or invalid questionnaire items are
reported as input errors rather than silently excluding participants; resolve
such inputs explicitly before rerunning.

Questionnaires and sliders are loaded from the **exact same-run filenames** as
the trial export. Legacy trust exports do not contain a timestamp column, so
their provenance relies on the filename and participant/design checks. Input
hashes and source hashes are recorded. The base raw-data analysis retains all
complete participants. The revised analysis runner applies the agreed QC rules to its primary cohort;
manual target deviation never causes exclusion. Plotting consumes saved fits.

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

### Revised primary hypotheses: adaptation and recovery

The canonical report is **`plots/semester2_2026_behavioural/presentation/index.html`**.
Its story is: **How does relative human-aid competence shape the benefits of
assistance, adaptation to a reliability drop, and recovery when reliability returns?**
The H1-H7 framework was agreed on 17 October 2026 after inspecting earlier results;
it is retrospective, not preregistered. It replaces the earlier presentation
numbering with **11 core pages**, while preserving historical analyses.

| Hypothesis | Primary question and comparisons | Figures / tests |
| --- | --- | --- |
| H1 | Aid benefits versus manual pre, in each phase and calibration group, with group differences in benefit | H1a accuracy/task d-prime, H1b correct RT; nine tests per outcome |
| H2 | Human value beyond following the aid: human-minus-aid accuracy and task d-prime in each phase and between groups | One figure; nine tests per outcome |
| H3 | P2-P1 changes after the reliability drop: advice acceptance, BLACK preference, advice selectivity and task discrimination | H3a bias, H3b discrimination; three tests per outcome |
| H4 | P3-P2 recovery and P3-P1 residual change in the same four SDT outcomes | H4a bias, H4b discrimination; six tests per outcome |
| H5 | Within-phase start-to-end accuracy and agreement changes, within groups and between groups | H5a accuracy: nine tests; H5b agreement: 18 tests across correct/incorrect advice together |
| H6 | Final trust, perceived reliability and signed perceived-minus-realised error: group differences, plus error versus zero in each group | One figure; five tests |
| H7 | Associations of final trust/error with whole-block advice acceptance and selectivity within each group, and group differences in slopes | One figure; 12 tests: two group slopes and their difference for each rating/predictor |

There are **125 primary comparisons per cohort**. `config/hypothesis_figures.csv`
is the panel-level contract for ordering, filenames, model, outcome, exact contrasts,
correction family and historical sources. The report exports that contract and
`hypothesis_mapping.csv`; it does not silently relabel historical tests as new ones.

**Cohorts.** The primary sample has 82 participants (38 CAL65, 44 CAL90), excluding
23, 49, 51, 65, 75, 79 and 81 under calibration, near-chance and anticipatory rules.
Manual target deviation remains diagnostic. All 89 participants (45 CAL65, 44 CAL90)
form a matched sensitivity. Membership is obtained from validated QC, not hardcoded.
All models, ratings, SDT outcomes and plots use the same cohort for each variant.
Selection is retrospective and includes post-exposure performance; compare both samples.

**Models.** Accuracy and agreement use logistic mixed models; correct positive
finite RT uses a linear mixed model of log RT. Repeated SDT scores and human-minus-aid
differences use linear mixed models. All include participant random intercepts.
Performance uses `group * stage`, aided SDT/benchmark models use `group * phase`,
and H5 adds phase-specific linear progress and its full interactions, including
advice correctness for agreement. New SDT and benchmark models are fitted. Existing
trial-level fits may be reused only when source/artifact checks, exact formulas
and model frames agree; all revised contrasts are recomputed.

Final ratings are measured once per participant, so H6/H7 use participant-level
linear models with HC3 covariance and residual-df t inference. H6 uses group-only
models. H7 fits separate `rating ~ group * centred_SDT_score` models for acceptance
and selectivity. Each estimates the CAL65 slope, CAL90 slope, and CAL90-minus-CAL65
slope difference, using HC3 covariance and pointwise 95% intervals. Whole-block SDT is calculated
from **pooled raw confusion counts before correction**, not averaged phase scores.
Phase-2 associations are supporting S7 and use the same interaction specification.
The previous additive `rating ~ group + centred_SDT_score` models remain as
`common_slope_sensitivity` results, with their own two-test Holm families per rating
and scope. Both sets of fitted models and prediction tables are saved; the report
includes expandable common-slope comparison tables. The interaction update followed
inspection of the common-slope plots and is retrospective. There are no within-person trust effects,
phase-specific ratings, or causal interpretations of these associations.

**Estimands and inference.** Accuracy includes timeouts as incorrect. SDT excludes
timeouts but retains answered trials regardless of RT. Advice -c measures acceptance;
task -c measures BLACK preference; advice d-prime describes selective following;
task d-prime describes final stimulus discrimination. Loglinear `(k+0.5)/(n+1)`
correction is primary; extreme-only correction is a separately labelled sensitivity.
The H2 accuracy benchmark assumes timely advice on every trial; its task-SDT benchmark
uses exactly human-answered trials for both human and aid.

All tests are two-sided, with Holm adjustment within hypothesis, outcome, sample,
correction and inferential method. H3/H4 also export a joint nine-test adjustment
per SDT outcome. H5 agreement retains one 18-test family. H7 and supporting S7 each
have six tests per rating outcome: two predictors multiplied by two group slopes
and their direct difference. A significant slope in only one group does not establish
a slope difference; the interaction contrast tests that. Participant sensitivities use paired changes within groups and
Welch comparisons between groups; H5 participant linear-probability slopes differ
from nonlinear model endpoint estimands. Sensitivities never replace primary inference.
No study-wide multiplicity-control claim is made.

Accuracy/agreement contrasts are response-scale percentage points. Model probabilities
condition on zero participant random effect. RT effects are ratios from log contrasts;
group differences are ratios of ratios. All primary intervals are pointwise 95% fitted-model
intervals, not Morey intervals. Numerical convergence does not establish adequacy:
convergence, singularity, rank, gradients, conditional cell dispersion, residual spread
and Q-Q diagnostics are saved. Unavailable primary inference is labelled unavailable,
not nonsignificant. Fixed order includes time, practice and fatigue; groups differ in
calibrated visual difficulty. Shared responses are not independent evidence, and
nonsignificance does not establish equivalence or full recovery.

**Fit, validate and plot.**

```sh
Rscript analyse_dynamic_reliability_primary.R
Rscript tests/test_dynamic_reliability_primary.R
Rscript tests/validate_dynamic_reliability_primary.R --analysis-only
Rscript plot_dynamic_reliability_hypotheses.R
Rscript tests/validate_dynamic_reliability_primary.R
```

The analysis accepts `[analysis_dir] [combined_csv] [qc_dir]`, defaults to the current
semester-2 locations, and writes a separate `analysis_outputs/semester2_2026_behavioural/primary/`
collection containing both cohorts, model objects, data, diagnostics, contrast audits,
125 primary tests per cohort, supporting/sensitivity tests and source/artifact hashes.
It reconciles current collation with exact historical raw runs and rating files.
Previous revised analyses are archived on rerun; failed builds remain unpublished.
Raw exports, canonical CSVs, historical analyses and old component outputs are preserved.

Plotting accepts `[analysis_dir] [plot_dir] [combined_csv] [qc_dir] [--stage-only]`.
It **never silently refits models** and rejects stale inputs/specifications/artifacts.
With `--stage-only`, it retains a validated staging directory for rendered review;
`dpr_publish_report(stage, plot_base)` then verifies hashes and publishes, restoring
the preceding report if the directory move fails. Successful publication archives
the preceding presentation intact. Existing slides/SDT plotting entrypoints forward
to the revised builder; the revised analysis must be current first.

The report contains `core_hypotheses.pdf` (11 pages), `all_hypotheses.pdf` (21 pages),
and `sensitivity_hypotheses.pdf` (21 corresponding all-participant pages). There is
one gallery, with the matching sensitivity expandable below every primary figure.
All figures have 12 x 6.75-inch vector PDFs and 3600 x 2025 PNGs. New figures share
white backgrounds, typography, faint participant data, open diamonds for fitted
estimates and ordinary pointwise intervals. CAL65 is blue and CAL90 orange.
Annotations consistently use `***`, `**`, `*`, `ns` or `unavailable`, with exact tests
available beside the figure. Descriptive supporting means use filled circles.

Seven supporting pages show full-sequence context, manual pre/post comparisons
(three tests per outcome), completion/answered accuracy, correction sensitivity,
model/participant sensitivity, cohort sensitivity, and Phase-2 evaluation associations.
Three historical pages retain original incorrect-agreement, recovery and rating-agreement
figures byte-for-byte with their original correction families and labels.

The independent validator reconstructs raw confusion counts, pooled scores, performance,
benchmarks and ratings; checks model contrasts from coefficients/covariance, HC3 arithmetic,
Satterthwaite df, participant tests, transformations and Holm families; and requires every
one of the 125 primary annotations per cohort. It also verifies saved figure data,
cohort identities, source copies, historical bytes, report links, PDF counts/dimensions,
text bounds and input/artifact hashes. Rendered visual inspection is a separate step.
`tests/validate_dynamic_reliability_hypotheses.R` is a compatibility entrypoint for this
validator; the older component validators remain available for historical component work.

### Historical advice-SDT component methods

The historical advice SDT analyses remain in each cohort's `hypothesis_components/`
subdirectory, and original all-participant outputs remain in `sdt_alternates/`.
Their old H1/H3 labels and nine-test paired/Welch families are historical; revised
H3/H4 now use the mixed models and families above. The former H4 associations used
P2 acceptance as a predictor; revised H7 uses pooled whole-block acceptance and
selectivity. H = agreement with correct advice and F = agreement with incorrect
advice: `-c = (z(H)+z(F))/2`, `d-prime = z(H)-z(F)`. Both need nonempty advice cells;
missing denominators are not zero rates. Neither index identifies deliberate verification
or advice-caused switching, and acceptance is not necessarily excessive reliance.

### Stimulus-based SDT: final performance with and without aid

Task SDT is integrated into the revised models above. Earlier paired/Welch
component outputs remain in `hypothesis_components/task_sdt/`; the revised model
scores and tests are in `primary/` and copied to `presentation/data/analysis/`. For isolated component work the runner accepts
`[trials_csv] [qc_dir] [analysis_dir] [plot_dir]`; the validator accepts
`[analysis_dir] [plot_dir]`. Running the analysis entry point with no arguments
builds the unified report.

Here the signal is the actual **BLACK stimulus**, and the decision is the
semantic BLACK/WHITE response, regardless of the counterbalanced physical keys:

- `H = P(response BLACK | stimulus BLACK)`;
- `F = P(response BLACK | stimulus WHITE)`;
- `d-prime = z(H) - z(F)` measures final-response discrimination;
- `c = -(z(H) + z(F))/2` measures colour-response criterion; positive c favours WHITE.

During aided phases, task d-prime describes the effective participant-plus-aid
system, not pure visual sensitivity. Criterion concerns colour responses, not
advice reliance. Simply changing agreement to correctness while retaining
correct/incorrect advice as the signal classes would only reparameterize the
existing advice SDT (`new d-prime = 2 * old -c`, `new -c = old d-prime / 2`).
The new analysis instead uses actual stimulus classes, following
[Stanislaw and Todorov (1999)](https://doi.org/10.3758/BF03207704).

The primary sample applies the **three agreed QC exclusion rules** consistently
across the unified report; manual target deviation remains diagnostic. The current cohort
retains 82 participants (38 CAL65, 44 CAL90), excluding IDs 23, 49, 51, 65, 75,
79 and 81. A separate sensitivity retains all 89 (45 CAL65, 44 CAL90). Membership
is read from the validated QC report, not hardcoded. Canonical CSVs and the original all-participant behavioural fits remain unfiltered;
new retained-cohort fits and both SDT analyses are stored separately. These are
retrospective exclusions that include post-exposure performance, so selection
can affect comparisons; inspect both samples.

Each participant receives separate scores for manual pre, the three aided
phases, and manual post. Adaptive calibration is not a performance baseline.
All answered trials are retained, including short, nonpositive or missing RTs.
Timeouts are excluded from SDT and reported separately; all-trial accuracy counts
them as incorrect. Every rate uses the loglinear `(k + 0.5)/(n + 1)` correction,
with the existing extreme-only correction as a sensitivity. Missing stimulus
classes fail validation instead of becoming zero rates.

The aid-only benchmark is scored against the same actual stimuli. Inferential
participant-minus-aid comparisons use exactly the same answered trials for both
decisions (`aid_matched`); a separate descriptive `aid_all` estimate includes
trials on which the participant timed out. Both are exported with integer
confusion counts, corrected rates and SDT scores.

| Contrast family | Comparisons | Tests per outcome |
| --- | --- | --- |
| Manual benefit/cost | Each aided phase minus manual pre | 9 |
| Reliability: H1/H3 performance complements | Phase 2 minus Phase 1; Phase 3 minus Phase 2; Phase 3 minus Phase 1 | 9 |
| Aid benchmark | Participant minus matched aid in each phase | 9 |
| Manual context | Manual post minus manual pre | 3 |

Every comparison has a paired participant-change test within each group and a
Welch CAL90-minus-CAL65 comparison of those changes. Participants receive equal
weights; tests are two-sided with pointwise 95% t intervals. Holm correction is
separate by family, outcome, sample and rate correction. d-prime is the main
performance measure and c is secondary. Tables additionally provide Holm
correction across both outcomes within each family and across all 60 contrasts.
These families were specified for this exploratory extension after inspection
of the original analyses; they are not preregistered hypotheses.

Each sample has seven 16:9 PDF/300-dpi PNG figure pairs plus a combined PDF:
task-SDT sequence, manual benefit/cost, H1 drop, H3 recovery, aid-only benchmark,
manual pre/post, and all-trial accuracy/timeouts. Trajectory intervals use
[Cousineau-Morey normalization](https://doi.org/10.20982/tqmp.04.2.p061) within
calibration group over all five participant stages. Ordinary mean intervals
are also exported; aid summaries normalize over their three phases. Contrast
intervals are ordinary paired/Welch intervals. Plot markers `[S]` and `[C]` flag
a change in direction or Holm decision across samples or rate corrections,
respectively; exact estimates and p-values appear in `sensitivity_comparison.csv`.
Holm p-values can change even in a group whose members and raw test are unchanged,
because the adjustment also depends on other tests in its shared family.
All plotted data and annotations are saved beside the figures.

The validator independently reconstructs confusion matrices from trials,
recalculates all scores, checks the exclusion union and diagnostic-only cases,
uses `t.test()` to verify all 240 contrasts, reconstructs Morey intervals from
participant-by-stage matrices, and checks figure data, dimensions, HTML links
and artifact hashes. Rendered PDF inspection is a separate visual check.

Task SDT reuses the same responses as accuracy and advice SDT; it provides
complementary interpretation rather than independent evidence. Fixed sequence
confounds aid/reliability changes with time, practice and fatigue. Manual
pre/post are not randomized control conditions, and CAL65/CAL90 have different
calibrated visual difficulty. Nonsignificant Phase 3-minus-Phase 1 results do not
establish equivalence or complete recovery.

The original supporting figure bundle has nine PDF/300-dpi PNG pairs: sequence accuracy, automation
benefits, relative competence, recovery, adaptation trajectories, correct RT,
manual pre/post, subjective evaluation, and descriptive manual self-ratings.
Every panel has exported source data. The additional manual self-rating figure
keeps the subjective-evaluation figure legible. Titles are question-based so they
cannot retain stale conclusions after a future rerun. Structural validation
does not substitute for rendered visual inspection.
Open `plots/semester2_2026_behavioural/index.html` to browse the nine supporting
figures and follow the link to the presentation bundle.

## Participant QC and exclusion review

The dedicated `participant_qc/` pipeline produces review evidence without
removing participants, rewriting canonical data, or rerunning analyses. Run
from this repository using the existing `r-pygame` environment:

```sh
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python participant_qc/run_qc.py
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python -m unittest discover -s tests -p 'test_participant_qc.py'
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python tests/validate_participant_qc.py
```

Open `analysis_outputs/semester2_2026_qc/index.html`. The runner accepts
`--input`, `--config`, and `--output-dir`; defaults are `data/data_virus_all.csv`,
`participant_qc/config.json`, and the report directory above. NumPy and
Matplotlib are already available in `r-pygame`; validation also uses Pillow and
the local Poppler `pdfinfo` executable. No packages are installed by the pipeline.

| Screen | Rule | Use |
| --- | --- | --- |
| Calibration | Accuracy in trials 151-300 differs from assigned target by more than 10 percentage points. | Exclusion criterion |
| Manual target | Either full manual segment differs from target by more than 10 points. | Diagnostic summary only |
| Near-chance or worse | All-trial accuracy <=55% in any whole scope. | Exclusion criterion |
| Anticipatory responding | Within the same scope, >=20% of all trials have RT <200 ms, at least 20 responses are fast, and fast-response accuracy is <=55%. | Exclusion criterion |

Whole scopes are full calibration, manual pre/post, each 400-trial aided phase,
all 1,200 aided trials, and all 1,600 experimental trials. Calibration's final
150 trials form an additional target-check scope. Exactly 200 ms is not fast;
finite nonpositive RTs remain visible and count as fast. Fast proportions use
all scope trials, while fast accuracy uses only fast answered trials. Timeouts
are incorrect for all-trial accuracy and never fast. No trial trimming occurs.

The configured rules are descriptive, retrospective screens. Near-chance flags
do not establish equivalence to chance. Wilson intervals provide descriptive
binomial context; they do not model serial dependence or adaptive calibration.
Manual target deviation stays in summaries and figures but never causes exclusion;
manual-post deviations may reflect exposure, practice, or fatigue.
Manual/aid ordering is not an exclusion criterion. The remaining reasons stay
separate in the report. Invalid or incomplete
scopes are **not assessable**, never silently marked clear; missing schema,
ambiguous multiple runs, and duplicate trial identities stop generation.

Reports contain participant and scope metrics, all screening results, a
flag-only table, integrity findings, 50-trial bins, cohort figures, and PDF/PNG
diagnostics for every participant. `exclusion_candidates.csv` contains one row
per participant meeting at least one of the three exclusion criteria;
`exclusion_summary_table.csv` exports the final criterion counts and unique total,
and `exclusion_summary.json` records
the unique total, percentage, participant IDs, remaining sample if applied, and
overlaps by number of distinct exclusion criteria, plus diagnostic-only cases.
`n_flagged_rules` and `flagged_any_rule` cover all four screens;
`n_exclusion_rules`, `exclude_by_rules`, and `exclusion_reasons` cover only the
three exclusion criteria. Repeated flags across scopes count
once per exclusion criterion and once in the overall total. Unassessable checks remain
separate. The QC runner itself applies no exclusions; the unified hypothesis
analysis runner consumes its decisions for the retained primary cohort. Anticipatory plots adapt
`hmm_pmdc/scripts/plot_anticipatory_response_audit.py` without importing or
modifying that repository. They preserve phase shading, RT scatter on a
linear/log scale, fast-response and accuracy panels, and peer median/IQR bands;
they additionally show calibration and fast-only accuracy. Open fast-accuracy
points scale with their fast-response counts; empty subsets remain missing.

Matched-comparison pages cover every anticipatory-flagged participant, with up
to two flagged participants plus a comparator matching calibration group and
key mapping. Candidates must have complete assessable data and no anticipatory
flag. The nearest candidate to the matching subgroup medians of experimental
accuracy and positive median RT is selected by summed absolute MAD-standardised
distance; zero-MAD metrics contribute zero, and ties use participant ID. Other
screening flags do not automatically disqualify a comparator. Peer bands use
other complete participants in the same calibration group without anticipatory
flags; comparator choices and all peer identities are exported. Missing
comparators are reported explicitly. No HMM-specific exclusion decisions are
imported, and no participant IDs are hard-coded into screening or plotting.

Input, configuration, source, and artifact hashes are recorded in `manifest.json`.
Generation uses a staging directory and publishes only after completion. A
previous report is preserved in a sibling `.previous_<timestamp>` directory;
failed builds remain unpublished for inspection. The independent validator
checks raw-source values, all flags and bins, Wilson intervals, peer bands,
comparator ranking, local report links, checksums, and PDF/PNG structure. Rendered
visual review is still required after plotting changes. Its optional
`--expect-current-cohort` switch asserts the reviewed 89-person snapshot; normal
generation and validation discover sample size dynamically.
