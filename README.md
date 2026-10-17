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

## Current task design

The task uses a three-block within-participant design after a combined practice/calibration phase. Before the first main block, each participant completes a 60-trial `PRACTICE` block to familiarise them with the 2-decision trial sequence and calibrate stimulus difficulty. Each participant then completes one `MANUAL`, one `AIDFIRST`, and one `STIMFIRST` block. Each trial has two keypress decision phases; the second decision is the final answer.

| Code | Mode | Trial structure | Trials |
| --- | --- | --- | ---: |
| `PRACTICE` | Manual-style practice/calibration | fixation -> masked aid preview 1000 ms -> fixation -> stimulus until decision 1 -> fixation -> masked final preview 1000 ms -> fixation -> HUD-only decision 2 -> feedback | 60 |
| `MANUAL` | Manual control | fixation -> masked aid preview 1000 ms -> fixation -> stimulus until decision 1 -> fixation -> masked final preview 1000 ms -> fixation -> HUD-only decision 2 -> feedback | 260 |
| `AIDFIRST` | Automation | fixation -> aid preview 1000 ms -> fixation -> stimulus until decision 1 -> fixation -> masked final preview 1000 ms -> fixation -> HUD-only decision 2 -> feedback | 260 |
| `STIMFIRST` | Automation | fixation -> masked aid preview 1000 ms -> fixation -> stimulus until decision 1 -> fixation -> aid final preview 1000 ms -> fixation -> HUD-only decision 2 -> feedback | 260 |

The `PRACTICE` block uses an adaptive staircase targeting 75% final-decision accuracy. It starts at the prior across-participant fixed-delta distribution (`delta = 0.040324718919`, SD `0.014615991726`). The first 20 practice trials are treated as burn-in with a larger annealed step size, and the final 40 trials are used to calculate the participant-specific delta mean and SD used for all subsequent main blocks. Single-block runs that skip practice use the same prior fixed-delta defaults directly; `derive_prior_calibration_delta.R` reproduces these fallback constants from the local prior data file. The participant calibration target is separate from the automated aid, which retains a single global reliability of 85% in the `AIDFIRST` and `STIMFIRST` blocks. The `PRACTICE` and `MANUAL` blocks show the masked aid string `#####` instead of a real recommendation during masked preview screens. Real aid recommendations display as `BLACK` or `WHITE`; these indicate that the aid recommends the `V-BLACK` or `V-WHITE` response, respectively. Final-decision response screens show only the progress HUD and response prompt.

Responses are made with the `D` and `J` keys. The standard key mapping is `D = V-BLACK` and `J = V-WHITE`; the flipped key mapping is `J = V-BLACK` and `D = V-WHITE`. On the second decision screen, participants use the same keys to confirm their first response or switch to the other response.

Participant-facing automation instructions are qualitative rather than numeric. Participants are told that the aid is reasonably reliable but not perfect in automation blocks, and that automation advice errors remain possible.

## Counterbalancing

The block order uses the full set of six permutations of the three scheduled condition blocks:

| Order | Sequence |
| --- | --- |
| `O1` | `MANUAL -> AIDFIRST -> STIMFIRST` |
| `O2` | `STIMFIRST -> AIDFIRST -> MANUAL` |
| `O3` | `MANUAL -> STIMFIRST -> AIDFIRST` |
| `O4` | `AIDFIRST -> STIMFIRST -> MANUAL` |
| `O5` | `AIDFIRST -> MANUAL -> STIMFIRST` |
| `O6` | `STIMFIRST -> MANUAL -> AIDFIRST` |

Block order and key mapping are assigned deterministically from participant ID in a 12-participant cycle. Each block order appears once with the standard key mapping and once with the flipped key mapping before the cycle repeats.

For the planned sample of `N = 60`, this gives:

- 10 participants per block order overall.
- 30 participants per key mapping overall.
- 5 participants per full `block order x key mapping` cell.
- 20 appearances of each condition in each serial position.

## Output fields

The main trial and post-block output files include fields that identify the design cell:

| Field | Meaning |
| --- | --- |
| `participant_id`, `run_timestamp` | Participant number and timestamp identifying the run |
| `key_black`, `key_white`, `keymap_flip` | Participant-specific D/J mapping for `V-BLACK` and `V-WHITE` |
| `condition_code` | `PRACTICE`, `MANUAL`, `AIDFIRST`, or `STIMFIRST` |
| `aid_condition` | `manual`, `aid_first`, or `stimulus_first` |
| `aid_accuracy_setting` | `0.85` for aided automation blocks, or blank for `MANUAL` |
| `staircase_target_accuracy` | `0.75` for `PRACTICE` staircase trials, or blank for fixed-difficulty main blocks |
| `trial_deadline_s` | Blank in the current self-paced design |
| `preview_display`, `preview_label` | Display type and visible value shown on the initial preview screen |
| `decision1_display`, `decision2_display` | Display type for each decision phase; second decisions use `blank_response` |
| `decision2_preview_display`, `decision2_preview_label` | Display type and visible value shown on the final preview screen before decision 2 |
| `decision2_label` | Blank in the current HUD-only second decision screen |
| `decision1_response`, `decision1_correct`, `decision1_rt_s`, `decision1_matches_aid` | First automation classification fields |
| `decision2_response`, `decision2_correct`, `decision2_rt_s`, `decision2_matches_aid` | Second automation classification fields |
| `changed_response` | Whether decision 2 differs from decision 1 |

The current schema intentionally omits older compatibility aliases such as `condition_deadline_code`, `initial_*`, `final_*`, and generic final-response fields (`response`, `correct`, `rt_s`). Timing fields are recorded in seconds only.

Practice trials are saved separately as `results_p###_<timestamp>_b00_PRACTICE.csv` with `condition_code = PRACTICE`. They are not included in `b00_ALL.csv`, final performance scoring, or post-block measures.

Practice calibration also writes `delta_p###_<timestamp>_b00_PRACTICE.csv`. This summary records `staircase_target_accuracy`, excludes the first 20 burn-in trials, and reports the delta mean/SD from the final 40 practice trials that set the main-block difficulty. Legacy practice files without target metadata are interpreted as having used the previous 85% target by `check_calibration_stationarity.R`.

To run practice/calibration or one main condition on its own, specify the condition:

```r
run_task(aid_condition = "practice")
run_task(aid_condition = "manual")
run_task(aid_condition = "aid_first")
run_task(aid_condition = "stimulus_first")
```

## Screenshot review deck

Use the `r-pygame` interpreter to regenerate the local screenshot review deck:

```bash
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python python/capture_instruction_screenshots.py --overwrite
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python python/capture_virus_task_screenshots.py --overwrite
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python python/build_screenshot_deck.py --overwrite
```

This writes `instruction_screenshots/`, `virus_task_screenshots/`, and `screenshots_review.pptx`. These generated review artifacts are ignored by git.

## Author
Russell J. Boag

## Participant exclusion audit

The standalone `participant_qc/` pipeline adapts the dynamic-reliability and
time-pressure audits to this study's two-decision design. It produces an
exclusion-candidate list and supporting evidence. **It does not remove participants
or trials, rewrite canonical CSVs, or rerun behavioural analyses.**

```sh
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python participant_qc/run_qc.py
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python -m unittest discover -s tests -p 'test_participant_qc.py'
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python tests/validate_participant_qc.py --expect-current-cohort --receipt analysis_outputs/semester2_2026_qc_validation/validation_receipt.json
```

Open `analysis_outputs/semester2_2026_qc/index.html`. The runner accepts
`--input`, `--manifest`, `--config` and `--output-dir`; their defaults are
`data/data_virus_all.csv`, `data/collation_manifest.csv`,
`participant_qc/config.json`, and `analysis_outputs/semester2_2026_qc/`.
NumPy, Matplotlib and Pygame are available in `r-pygame`; validation also uses
Pillow and Poppler's `pdfinfo`. No package installation is performed.
The optional `--expect-current-cohort` validator flag checks the initial
60-participant snapshot; omit it for future cohorts. The generator's sample size
and candidate membership are data-driven.

### Agreed first-pass criteria

These are retrospective descriptive screens agreed on 2026-10-17, not
preregistered exclusions or proof of equivalence to chance.

| Screen | Scope | Use |
| --- | --- | --- |
| All-trial accuracy **<=55%** | Each main condition, pooled aided trials, and all main trials; Decision 1 and Decision 2 separately | Exclusion candidate |
| RT **<200 ms** on **>=20% of all scope trials**, **>=20 fast responses**, and fast-response accuracy **<=55%**, jointly in the same decision and scope | Same main scopes and decisions | Exclusion candidate |
| Absolute accuracy deviation from recorded calibration target **>10 percentage points** | Practice trials 21-60, Decision 2 | Diagnostic only |
| Same target-deviation screen | Whole Manual block, Decision 2 | Diagnostic only |
| Low-accuracy and anticipatory screens above | Whole 60-trial Practice block, both decisions separately | Diagnostic only |

The main conditions contain 260 trials each, pooled aided contains 520, and all
main trials contains 780. The current calibration target is 75% for all 60
participants; it is read from their recorded practice metadata and cross-checked
against their calibration summaries, never inferred from today's task defaults.
The final-40 target check excludes the first 20 practice trials; whole-practice
diagnostics retain all 60. Missing target metadata remains unassessable.

Both decisions' low-accuracy and anticipatory screens can create candidates,
including Aid-first and Decision 2. **Aid-first Decision 1 can be prepared from
the preceding advice, and Decision 2 can be prepared before its response screen.**
Their short RTs therefore require interpretation. The two decisions are never
combined into a single denominator or scored against each other's correctness.
Neither switching frequency nor advice agreement is a separate exclusion rule.

Timeouts count as incorrect and never fast. Exactly 200 ms is not fast; finite
nonpositive answered RTs count as fast and are reported separately. Fast-rate
denominators include all scope trials; fast accuracy includes only fast answered
trials. Empty fast subsets have missing accuracy. No trial trimming occurs.
Wilson intervals are descriptive and do not account for adaptive calibration
or serial dependence.

### Run provenance, reports and comparisons

The audit verifies canonical/raw agreement and source MD5s in the collation
manifest. Practice and calibration-summary files must match each canonical
participant's exact run timestamp. Missing files do not fall back to an older
run or get selected by modification time. The selected replacement p59 is
`20260924_110831`; the historical `20260903_120309` run remains preserved and
does not enter the audit. No participant-wide exclusion list is hard-coded.

Missing schema, duplicate trial identities and conflicting or ambiguous run
provenance stop generation. Incomplete/invalid scopes remain unassessable;
affected pooled scopes inherit those failures. Response, correctness, key
mapping, block order, advice, preview timing, decision-transition and calibration
metadata checks are exported. A missing RT in one decision does not silently
invalidate the other decision's otherwise valid measurements.

Outputs include participant and decision/scope summaries, all screening checks,
flagged checks, integrity findings, candidate unions and overlaps, source hashes,
10-trial practice bins and 50-trial main bins (with the final 10-trial bin).
The HTML report provides a searchable participant table, four cohort figure
families, PDF/PNG diagnostics for every participant and a comparison page for
every anticipatory candidate. Each person counts once in the candidate total;
repeated flags across scopes and decisions count once per exclusion criterion.
Diagnostic-only flags and incomplete assessments remain separate.

Individual plots follow administered order, offsetting main `global_trial` by
60 for display because the raw main sequence restarts at 1 after practice.
Decision 1 and Decision 2 have separate RT, fast-rate, accuracy and fast-only
accuracy panels. Peer bands use other complete participants with the same
calibration target and no main-task anticipatory flag, aligned by condition,
within-block trial and decision. These are descriptive references across block
orders, not adjustments for order effects.

Comparators additionally match key mapping. Select the smallest sum of absolute
MAD-standardised distances from subgroup medians of overall accuracy and positive
median RT for both decisions. Zero-MAD terms contribute zero; participant ID
breaks ties. Other exclusion or diagnostic flags do not silently disqualify a
comparator. All pools, rankings and missing matches are exported.

Builds use staging and publish only after successful generation and unchanged
input/source checks. Earlier reports are retained as `.previous_<timestamp>`
siblings. The manifest hashes input data, configuration, implementation and
generated artifacts. The independent validator imports no QC scoring code: it
recomputes metrics, Wilson intervals, flags, unions, peers and comparators from
source, and checks hashes, local links, PDF pages and PNG integrity. After plot
changes, inspect rendered PDFs as well as running automated validation. Keep
validation receipts outside the generated bundle.

The first validated audit selected **010, 013, 020, 022 and 027** (5/60, 8.3%;
55 would remain if applied). All five met the anticipatory rule; 022 and 027 also
met the low-accuracy rule. All candidate checks arose in Decision 1. Eleven
participants had practice-target flags and 15 had Manual-target flags; their
overlap and overlap with candidates leave **18 diagnostic-only participants**.
All scopes were assessable. Replacement p59 had diagnostic target deviations
and **was not an exclusion candidate**. No exclusions were applied.

## Revised hypothesis analysis and presentation

The current report uses all 60 canonical participants, including replacement p59
(`20260924_110831`). **Participant exclusions are not applied.** The participant-QC
audit is independent of this analysis and is never read by it.

```sh
Rscript analyse_aid_onset_primary.R
Rscript tests/validate_aid_onset_primary.R
Rscript plot_aid_onset_hypotheses.R
Rscript tests/validate_aid_onset_slides.R
```

Open `plots/semester2_2026_key_findings/presentation/index.html`. It contains eight
core figures and five supporting figures, individual 16:9 vector PDFs and
3600 x 2025 PNGs at 300 dpi, an eight-page `aid_onset_core_hypotheses.pdf`, and a
13-page `aid_onset_all_hypotheses.pdf`. Each figure links to its plotted values,
model estimates and tests. The complete preceding gallery, including alternate
SDT figures, is archived under `plots/semester2_2026_key_findings/archive/` before
a successful replacement. Earlier analysis outputs remain intact.

`config/hypothesis_figures.csv` is the ordered panel registry and historical
crosswalk. This framework is a **retrospective reorganisation**, specified after
inspection of earlier results; promotion to the primary presentation does not
make an exploratory outcome preregistered.

| Order | Question | Primary comparisons |
| --- | --- | --- |
| H1 | Final performance: accuracy and task d-prime | AF - Manual, SF - Manual, SF - AF for each outcome (6) |
| H2 | Human performance beyond the realised aid: accuracy and task d-prime | Human - aid at each timing, and SF - AF in advantage (6) |
| H3 | Advice acceptance bias and task BLACK-response bias | SF - AF for each (2) |
| H4 | Advice-following and task discrimination | Advice d-prime SF - AF (1); reuse the exact H1 task timing test |
| H5a | Switching frequency | Three condition comparisons (3) |
| H5b | Beneficial and harmful revisions | Three condition comparisons for each outcome (6) |
| H6 | Selective uptake after initial disagreement, Stimulus-first only | Correct - incorrect advice (1) |
| H7 | Subjective reliability and trust | Mean aid rating - Manual self-rating; SF - AF aid rating; SF - AF trust (3) |

There are **28 unique primary comparisons**, all two-sided. Holm adjustment is
separate within each hypothesis/outcome. H4's repeated task-discrimination panel
uses the same H1 model, contrast ID and three-test-family adjusted p-value.

The five supporting figures show initial/final performance, task bias across
both decisions, final accuracy conditional on advice correctness (with an
overall Manual baseline), SDT correction sensitivity, and model-versus-participant
inference. Initial/final comparisons are descriptive and use matched valid
responses; Aid-first's initial response already follows advice exposure.

### Estimands and models

Task SDT uses H = BLACK response given BLACK stimulus and F = BLACK response
given WHITE stimulus. Advice SDT uses H = agreement given correct advice and
F = agreement given incorrect advice. Both use `d-prime = z(H) - z(F)` and
`-c = (z(H) + z(F))/2`. Positive task -c means BLACK preference; positive advice
-c means acceptance tendency, not necessarily inappropriate reliance. Final
agreement does not demonstrate active uptake or deliberate verification.

Primary rates use `(k + 0.5)/(n + 1)` in every cell. Extreme-only correction is
analysed separately as a sensitivity. All answered rows enter SDT regardless of
RT. Timeouts count as incorrect for accuracy and are omitted from SDT. Empty
stimulus/advice classes or invalid responses stop the pipeline. Human-versus-aid
accuracy compares all-trial human accuracy with realised advice correctness,
assuming a timely aid response. Task d-prime compares human and aid on exactly
the human's final-answered rows. Revisions use all valid initial/final response
pairs, not just switch trials; exported denominators make this explicit.

Binary outcomes use logistic mixed models with condition fixed effects and a
participant random intercept. Contrasts are differences of estimated probabilities
in percentage points, computed after response-scale regridding. Displayed binary
means condition on a zero participant random effect; they are not population-
marginal probabilities. SDT, benchmark differences and ratings use linear mixed
models with participant random intercepts and Satterthwaite degrees of freedom.
H6 replaces condition with advice correctness in the restricted Stimulus-first
sample. It contrasts opportunities that also differ in initial correctness,
so it does not isolate a causal effect of advice correctness.

Figures show participant values, fitted cell means and pointwise 95% CIs.
Significance brackets follow the sibling repositories: `*** p < .001`,
`** p < .01`, `* p < .05`, and `ns p >= .05`, using the saved Holm-adjusted
model tests. H2 labels each human-minus-aid cell against zero and brackets the
timing difference. H7 groups both aided ratings for the mean-aid versus self
comparison. Bracket endpoints, symbols and source contrast IDs are exported
with each figure; unavailable inference never receives a significance star.
Contrast intervals are also pointwise; interval overlap is not a test.
Singular, nonconverged or rank-deficient fits yield unavailable primary inference
and no significance annotations. Paired participant tests are separately labelled
sensitivities, never fallback primary tests. Conditional cell dispersion and
linear-model residual plots accompany the results. Several revision models have
elevated dispersion; inspect their sensitivities before interpreting uncertainty.
The historical joint two-outcome advice-SDT and six-contrast revision-quality
Holm families are exported as additional sensitivities.

Perceived reliability intentionally compares different rating targets: self
in Manual, aid in the aided conditions. Trust is the mean of six positive 1-5
items, with no Manual trust cell. Shared responses across accuracy, SDT and
revision outcomes do not provide independent evidence. Nonsignificance does not
establish equivalence.

### Reproducibility and legacy entry points

The new results live only in
`analysis_outputs/semester2_2026_hypotheses/primary/`. They contain cohort selection,
raw-source provenance, SDT counts/scores, model data and saved fits, contrast
weights, model diagnostics, participant sensitivities, decision-context values,
and artifact checksums. The analysis verifies each of the 180 canonical source
exports against `data/collation_manifest.csv` and checks all three collated data
families against those exports before computing outcomes. The validator checks
independent counts, scores, cells, model covariance/intervals, Holm families,
all 28 comparisons and exact H1/H4 reuse. A synthetic policy excluding everyone
also confirms that this pipeline ignores participant exclusion files.

Plotting requires the validation receipt and unchanged analysis/source checksums;
it never refits models. Generation is staged before the public gallery is
replaced, and failures preserve the preceding gallery. Model and plotting scripts
accept explicit output paths for isolated reproducibility checks:

```sh
Rscript analyse_aid_onset_primary.R /tmp/aid_onset_analysis
Rscript tests/validate_aid_onset_primary.R /tmp/aid_onset_analysis
Rscript plot_aid_onset_hypotheses.R /tmp/aid_onset_analysis /tmp/aid_onset_figures
Rscript tests/validate_aid_onset_primary.R /tmp/aid_onset_analysis /tmp/aid_onset_figures
```

With no arguments, `plot_aid_onset_slides.R` and `analyse_aid_onset_sdt.R` now
build the unified gallery from the validated primary results. Their validation
entry points likewise validate the unified report. Explicit legacy outputs must
be outside the canonical presentation gallery; the SDT component requires both
output paths to prevent an accidental write into that gallery:

```sh
Rscript plot_aid_onset_slides.R /tmp/aid_onset_legacy_slides
Rscript analyse_aid_onset_sdt.R /tmp/aid_onset_legacy_sdt /tmp/aid_onset_legacy_sdt_plots
Rscript tests/validate_aid_onset_sdt.R /tmp/aid_onset_legacy_sdt /tmp/aid_onset_legacy_sdt_plots
```

`test_aid_onset_hypotheses.R`, `analyse_aid_onset_followups.R`, and
`aid_onset_research_questions.R` remain the historical analysis workflow. Their
original H1-H5 tests and exploratory extensions are retained for provenance;
use the new primary entry point for the reorganised framework. The original
H3/H4 conditional-accuracy outcomes appear as supporting S3, original H1 perceived
reliability appears in H7, and prior exploratory H5/H6/H7/H8 presentation outcomes
map to H5b/H3-H4/H6/H7 respectively. The former alternate advice SDT figures are
now core outcomes alongside their task-SDT analogues.
