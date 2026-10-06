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

By default, the screenshot scripts render PNGs at `1512x982`. Pass `--resolution current` to use the active display size instead, or pass another explicit `WIDTHxHEIGHT` value to both screenshot commands.
The deck builder uses a `1512x982` slide canvas by default and requires the screenshot folders to match that aspect ratio. Pass `--slide-resolution screenshots` to size the PowerPoint slides to the existing screenshot aspect ratio instead.
The generated `instruction_screenshots/`, `virus_task_screenshots/`, and `screenshots_review.pptx` artifacts are local review outputs and are ignored by git.
The deck builder requires `pandoc` and Pillow.

## Current task design

The task uses a two-level time-pressure design. High pressure (`HP`) uses a 1 s response deadline and low pressure (`LP`) uses a 3 s response deadline.

Each participant completes one calibration block only. All participants are calibrated to 77% accuracy under the LP deadline. The resulting participant-specific stimulus difficulty is then reused for every post-calibration manual and automation block, regardless of that block's pressure deadline.

| Code | Mode | Deadline | Trials |
| --- | --- | ---: | ---: |
| `CAL_LP` | Calibration | 3 s | 300 |
| `M_HP` | Manual | 1 s | 400 |
| `A_HP` | Automation | 1 s | 400 |
| `M_LP` | Manual | 3 s | 400 |
| `A_LP` | Automation | 3 s | 400 |

Automation reliability is a between-subjects, pressure-contingent factor:

| Reliability pattern | HP aid accuracy | LP aid accuracy |
| --- | ---: | ---: |
| `HP95_LP65` | 95% | 65% |
| `HP65_LP95` | 65% | 95% |

Participant-facing automation instructions are qualitative rather than numeric. Before each automation block, the high-reliability aid is described as highly reliable but not perfect; the low-reliability aid is described as reasonably reliable with relatively common possible errors.

## Counterbalancing

Counterbalancing is assigned deterministically from participant ID in a 96-participant cycle. Calibration is always first and always uses `CAL_LP`. The four post-calibration blocks are ordered using the full factorial set of all `4! = 24` permutations of the crossed manual/automation x HP/LP cells:

| Factor | Levels |
| --- | --- |
| Post-calibration block order | 24 permutations of `M_HP`, `A_HP`, `A_LP`, `M_LP` |
| Reliability pattern | `HP95_LP65`, `HP65_LP95` |
| Key mapping | standard, flipped |

The post-calibration order set is:

| Order | Sequence |
| --- | --- |
| `O01` | `M_HP -> A_HP -> A_LP -> M_LP` |
| `O02` | `M_HP -> A_HP -> M_LP -> A_LP` |
| `O03` | `M_HP -> A_LP -> A_HP -> M_LP` |
| `O04` | `M_HP -> A_LP -> M_LP -> A_HP` |
| `O05` | `M_HP -> M_LP -> A_HP -> A_LP` |
| `O06` | `M_HP -> M_LP -> A_LP -> A_HP` |
| `O07` | `A_HP -> M_HP -> A_LP -> M_LP` |
| `O08` | `A_HP -> M_HP -> M_LP -> A_LP` |
| `O09` | `A_HP -> A_LP -> M_HP -> M_LP` |
| `O10` | `A_HP -> A_LP -> M_LP -> M_HP` |
| `O11` | `A_HP -> M_LP -> M_HP -> A_LP` |
| `O12` | `A_HP -> M_LP -> A_LP -> M_HP` |
| `O13` | `A_LP -> M_HP -> A_HP -> M_LP` |
| `O14` | `A_LP -> M_HP -> M_LP -> A_HP` |
| `O15` | `A_LP -> A_HP -> M_HP -> M_LP` |
| `O16` | `A_LP -> A_HP -> M_LP -> M_HP` |
| `O17` | `A_LP -> M_LP -> M_HP -> A_HP` |
| `O18` | `A_LP -> M_LP -> A_HP -> M_HP` |
| `O19` | `M_LP -> M_HP -> A_HP -> A_LP` |
| `O20` | `M_LP -> M_HP -> A_LP -> A_HP` |
| `O21` | `M_LP -> A_HP -> M_HP -> A_LP` |
| `O22` | `M_LP -> A_HP -> A_LP -> M_HP` |
| `O23` | `M_LP -> A_LP -> M_HP -> A_HP` |
| `O24` | `M_LP -> A_LP -> A_HP -> M_HP` |

These 24 orders are crossed with reliability pattern and key mapping, giving `24 x 2 x 2 = 96` allocation cells per cycle. The cycle is intentionally not shuffled: for each order, the task assigns `HP95_LP65` standard/flipped, then `HP65_LP95` standard/flipped. Participant 1 is assigned to `O01 x HP95_LP65 x standard` and receives:

```text
CAL_LP -> M_HP -> A_HP -> A_LP -> M_LP
```

For the planned sample of `N = 96`, the full allocation cycle is completed exactly once. This gives 4 participants per post-calibration order, 48 per reliability pattern, 48 per key mapping, and 1 participant per full `order x reliability pattern x key mapping` cell. Across the full cycle, each post-calibration condition appears 24 times in each serial position. The full order set intentionally includes sequences with adjacent manual/manual, adjacent automation/automation, adjacent HP/HP, and adjacent LP/LP blocks.

The standard key mapping is `D = V-BLACK` and `J = V-WHITE`. The flipped key mapping is `J = V-BLACK` and `D = V-WHITE`.

The participant-specific allocation table is generated from the allocation functions in `python/virus_task.py`.

## Output fields

The main trial and post-block output files include fields that identify the design cell:

| Field | Meaning |
| --- | --- |
| `block` | `CALIBRATION`, `MANUAL`, or `AUTOMATION` |
| `condition_deadline_code` | `CAL_LP`, `M_HP`, `A_HP`, `M_LP`, or `A_LP` |
| `time_pressure_condition` | `HP` or `LP` |
| `automation_reliability_pattern` | `HP95_LP65`, `HP65_LP95`, `single_block`, or `none` |
| `automation_reliability_group` | `high`, `low`, or `none` |
| `aid_accuracy_setting` | `0.95`, `0.65`, or blank for manual/calibration |
| `trial_deadline_s` | Response deadline in seconds |

Seconds are the canonical exported timing unit for deadlines and response times. The fixed LP calibration metadata (`CAL_LP`, 3 s) is part of the task design and is not repeated as separate calibration columns in every row.

Single-block calibration runs must use the 3 s LP calibration deadline. The task will stop with an error if a 1 s HP calibration deadline is requested. Single-block automation runs require an explicit reliability group, for example:

```r
run_task(block = "CALIBRATION", deadline_s = 3)
run_task(block = "MANUAL", deadline_s = 1)
run_task(block = "AUTOMATION", deadline_s = 1, reliability_group = "high")
run_task(block = "AUTOMATION", deadline_s = 3, reliability_group = "low")
```

## Behavioural analysis

The current frequentist pipeline covers accuracy, correct RT, behavioural aid
agreement, and six-item mean trust. `fit_brms.R` is a legacy analysis with obsolete
block definitions and must not be used for this design.

Run from this project (R packages: dplyr, tidyr, readr, lme4, lmerTest, emmeans,
ggplot2, patchwork, and sandwich):

```sh
Rscript analyse_time_pressure.R
Rscript plot_time_pressure_results.R
Rscript tests/test_time_pressure_analysis.R
Rscript tests/validate_time_pressure_outputs.R
Rscript tests/test_time_pressure_exploratory.R
Rscript tests/validate_time_pressure_exploratory_outputs.R
Rscript tests/validate_time_pressure_slides.R
```

The analysis defaults to `output/semester2_2026_data` and writes to
`analysis_outputs/semester2_2026_behavioural`. Optional positional arguments are
`[input_dir] [output_dir]`. The plotting script accepts `[analysis_dir] [plot_dir]`
and defaults to `plots/semester2_2026_behavioural`. Generated outputs are ignored
by Git. The 2026-10-05 run contains 69 participants (35 HP95_LP65, 34 HP65_LP95),
not the full planned sample of 96.

The analysis reads raw trial exports and same-run questionnaires, rejects duplicate
participant runs, records input checksums, and checks block order, deadlines,
reliability, and key mapping using the task's allocation functions. This check
uses the `r-pygame` interpreter named in AGENTS.md; `TIME_PRESSURE_PYTHON` can
override its absolute path on another machine. Calibration and pilot folders
are excluded from the models. Timeouts count as incorrect, while correct-RT
analysis requires finite positive RTs. Behavioural agreement excludes timeouts
and is stratified by advice correctness; it is not evidence of an advice-caused
change in a participant's decision. Trust requires all six valid items per block.

Accuracy and correct-RT models use mode × pressure × assignment pattern;
agreement uses pressure × reliability × advice correctness, and trust uses
pressure × reliability. Every model has participant random intercepts only.
Accuracy/agreement use logistic mixed models. Correct RT uses a linear mixed
model of log RT; descriptive RT figures display arithmetic means, while model
estimates are geometric means/ratios. Trust uses a linear mixed model.

Planned contrasts test automation-minus-manual gains, differences in gains
between pressures and assignment groups, and aided HP minus manual LP. The last
contrast estimates the remaining performance gap; nonsignificance is not evidence
of equivalence. Reliability comparisons at fixed pressure compare assignment
groups. All tests are two-sided with Holm adjustment separately within outcome;
95% confidence intervals are pointwise.

The current accuracy and agreement models converge but show extra participant-cell
variation. Therefore the pipeline also reports paired participant contrast scores
within each assignment group and Welch-Satterthwaite comparisons across groups,
with equal participant weighting. These sensitivity tests allow arbitrary
within-person dependence and must be read alongside the model results.
They are added because of diagnostic evidence, not used to select whichever
analysis yields significance. DHARMa diagnostics are unavailable in the local
environment; the implemented binomial cell checks are explicitly diagnostic.

### Six exploratory extensions

The default analysis and plotting commands also run the six added questions,
writing to an `exploratory/` subdirectory beneath their respective output folders.
They are explicitly exploratory because the main results were inspected before
these questions were specified. The primary analysis outputs remain separate.
The presentation now includes original exploratory H2, H6 and H1 in the primary
set as H7, H8 and H9, respectively. The source analyses below retain their original
identifiers and exploratory provenance.
The extensions require the additional R package `sandwich`.

1. **Selective agreement:** correct-minus-incorrect advice agreement, and its
   pressure/reliability contrasts, using the existing logistic model plus paired
   participant selectivity scores.
2. **Value beyond following the aid:** aided accuracy minus realised block aid
   accuracy. Exact decomposition into correctly rejected wrong advice, rejected
   correct advice, and timeouts on correct advice. The aid benchmark assumes a
   timely response; these categories are not observed changes of mind.
3. **Perceived reliability:** same-run aid-accuracy sliders yield signed error,
   absolute error, and perceived separation of the reliability levels. An
   additional agreement model separates participant mean ratings from each
   block's deviation and controls pressure, reliability, advice correctness and
   block position. Rating slopes are per 10 percentage points.
4. **Trust and behaviour:** analogous within/between-participant trust slopes,
   stratified by pressure and advice correctness. Ratings follow the block, so
   these are associations, not causal mediation or prospective predictions.
5. **Adjustment after errors:** agreement on the immediately following trial,
   controlling previous participant error, current advice correctness and linear
   trial position. Lags are formed within participant/run/block before excluding
   current or previous timeouts; missing trials never get bridged. Reported
   probability differences weight current advice by assigned reliability.
6. **Completion versus choice accuracy:** separate logistic models of timeout
   and accuracy among answered trials; an exact symmetric decomposition splits
   each paired overall accuracy gain into completion and conditional-choice
   contributions. This is descriptive accounting, not causal mediation.

All new mixed models retain participant random intercepts only. Cell-level
models and trial models use full convergence checks even above lme4's default
observation-count limit. Logistic models use bobyqa with a stopping radius of
1e-9; Gaussian models use nloptwrap with absolute tolerances of 1e-8.
Cell-level questions use paired/Welch participant sensitivities. Rating-association
sensitivities use equal-weight participant/block/advice means with covariance
clustered by participant. The lag sensitivity uses a linear probability model
with equal total weight per participant block and participant-cluster covariance.
Model rating slopes are log odds; their sensitivity counterparts are probability
differences, so magnitudes should not be compared directly.

Holm adjustment is applied within each exploratory question separately for mixed
models and sensitivity analyses. `p_holm_all_six` additionally adjusts across all
six questions within each analysis family. Pointwise confidence intervals remain
unadjusted. Small effects supported only by the random-intercept models require
caution given the existing participant-cell variation and the limited 69-person
sample. Reliability at fixed pressure remains a between-group comparison.

The extensions can also be rerun independently after a valid base analysis:

```sh
Rscript analyse_time_pressure_exploratory.R
Rscript plot_time_pressure_exploratory.R
```

See `exploratory/exploratory_report.html`, `exploratory_contrasts.csv`, the
hypothesis registry, model diagnostics, participant summaries and input hashes.
The exploratory supporting bundle contains seven individual figures and three
older composites, in PDF and 300-dpi PNG. For presentations, use the corresponding
hypothesis figures in `presentation/` described below. Every figure exports its
plotted values.

Start with `results_report.html` or `results_report.txt`, then inspect
`planned_contrasts.csv`, `participant_sensitivity_contrasts.csv`, and
`model_diagnostics.csv`. The primary plots include nine standalone figures and four
16:9 composites, each exported as PDF and 300-dpi PNG, plus their plotted values
and provenance. `COMPLETE.txt` indicates a completed analysis run, not that all
statistical assumptions hold. Nonconverged or singular fits have p-values withheld.

`rt_benefits.pdf` and `.png` match the accuracy-gain plot, showing the four
automation-versus-manual contrasts for both the mixed model and participant
sensitivity analysis. Positive values indicate faster correct responses with the
aid: `100 * (1 - aided/manual geometric-mean RT ratio)`. Pointwise 95% CIs are
transformed from the log-RT contrast intervals; these are not arithmetic-mean RT
differences. Plotted estimates are in `data/rt_benefit_estimates.csv`.

### Presentation figures: one per question

Use `plots/semester2_2026_behavioural/presentation/index.html` to browse the
recommended figures, `presentation/primary_hypotheses.pdf` for primary H1-H9,
or `presentation/all_hypotheses.pdf` for the complete set of twelve figures.
The main plotting command builds these automatically. Rebuild just this set
from the saved analyses without refitting:

```sh
Rscript plot_time_pressure_slides.R
Rscript tests/validate_time_pressure_slides.R
```

The standalone script accepts `[analysis_dir] [plot_dir]`. Each of the twelve
figures is 12 x 6.75 inches (16:9), exported as a 3600 x 2025 PNG at 300 dpi and
a vector PDF. Insert a PNG at full slide width without cropping. The style
follows the latest `auto_reliability_virus_atc/R/paper_hypotheses_plots.R`:
Helvetica, 24-point finding titles, 17-20-point axes/panel labels, light horizontal
gridlines, and short captions. Plotted values, input hashes, and an artifact
manifest accompany the figures; `figure_catalog.csv` contains the question,
message, and interpretation notes for each.

The primary set contains nine hypotheses, each with its own H1-H9 figure:

- **H1 — Time-pressure costs:** Participants would respond faster but less accurately under high pressure than low pressure in the Manual condition.
- **H2 — Reliability-dependent accuracy benefits:** Accuracy gains over Manual would be larger under high pressure in the 95% HP / 65% LP group, and larger under low pressure in the 65% HP / 95% LP group.
- **H3 — Compensation for time pressure:** Reliable advice under high pressure would offset the accuracy cost of the shorter deadline, allowing performance to match or exceed Manual performance under low pressure.
- **H4 — Response speed:** Automation would change correct-response RT relative to Manual, with the size and direction of this effect depending on time pressure and aid reliability.
- **H5 — Behavioural reliance:** Agreement with automated advice would vary with time pressure and aid reliability, with potentially different effects for correct and incorrect advice.
- **H6 — Trust:** Reported trust would vary with aid reliability and time pressure, including whether time pressure altered participants' sensitivity to reliability.
- **H7 — Value beyond following the aid:** Participants' aided accuracy would differ from the realised aid-alone benchmark depending on time pressure and aid reliability.
- **H8 — Completion versus choice accuracy:** Automation-related accuracy gains would reflect changes in response completion and accuracy among answered trials, with their contributions varying with time pressure and aid reliability.
- **H9 — Selective reliance:** The difference between agreement with correct and incorrect advice would vary with time pressure and aid reliability.

H7, H8 and H9 were originally exploratory H2, H6 and H1, respectively, specified
after inspecting the main results. Their inclusion in the primary presentation
set preserves this history and the existing test adjustment families. The three
remaining exploratory figures retain their original H3-H5 identifiers. Filenames
include both the set and hypothesis ID; the catalog records the original set,
original hypothesis and analysis history alongside the current identifiers.

| File stem | Main comparison |
| --- | --- |
| `01_main_H1_manual_pressure` | Observed manual HP/LP accuracy and geometric correct-RT means |
| `02_main_H2_accuracy_benefits` | Observed manual/aided accuracy and the difference in gains between pressures |
| `03_main_H3_compensation` | Observed aided-HP and manual-LP accuracy in the same participants |
| `04_main_H4_response_speed` | Observed geometric correct RT and changes in the aid effect across pressure |
| `05_main_H5_behavioural_reliance` | Correct/incorrect advice agreement, with reliability, pressure and interaction tests |
| `06_main_H6_trust` | Six-item trust means, with reliability, pressure and interaction tests |
| `07_main_H7_aid_benchmark` | Observed human accuracy versus realised aid accuracy |
| `08_main_H8_accuracy_components` | Completion and conditional-choice contributions to accuracy gains |
| `09_main_H9_selectivity` | Correct-minus-incorrect advice agreement |
| `10_exploratory_H3_reliability_awareness` | Perceived versus realised aid accuracy |
| `11_exploratory_H4_trust_association` | Individual HP-minus-LP changes in trust and advice agreement |
| `12_exploratory_H5_error_adjustment` | Observed next-trial agreement after wrong versus correct advice |

All plotted points, lines and intervals now describe **observed participant
data**, with equal participant weighting. The figures contain no fitted means,
prediction bands or model-versus-participant coefficient overlays. Blue/orange
identify 95%/65% reliability where colour denotes reliability. In the manual and
trust-change figures, colour instead identifies the two allocation patterns;
the legends make this distinction explicit.

**Cousineau-Morey 95% intervals** are computed from complete repeated-measures
sets, normalising each person's scores and applying `sqrt(k / (k - 1))` to the
standard error. Normalisation stays within allocation groups and the specified
comparison set; centres remain the original observed means:

- Manual pressure: HP/LP pairs, separately by allocation group and outcome.
- Main H2 accuracy and H4 speed: the same four manual/aided x HP/LP cells within
  each allocation group, separately by outcome and displayed in separate figures.
- Compensation: manual LP and aided HP pairs within each allocation group.
- Main H7 / exploratory H3: human-versus-aid or perceived-versus-realised pairs within each
  participant-block. These intervals do not support between-reliability comparisons.
- Exploratory H5: the two previous-advice histories within each participant-block.

Correct RTs are geometric means: normalisation and interval construction use
participant mean log RTs, followed by exponentiation. The reliability/trust
mean panels retain ordinary t intervals because reliability at fixed pressure
is a between-group comparison. Main H9 selectivity and H8 total gains retain t
intervals on the actual paired difference scores, which directly quantify the
mean observed effect. Morey correction is not applied again to those effects.
Exploratory H4 shows individual paired changes without a fitted line or interval; pressure
and reliability change together, so the scatter does not isolate a trust effect.

**Significance brackets** follow the reference project: `*` means model Holm
`p < .05`, `ns` means `p >= .05`, and a dagger flags disagreement with the matching
participant test. All saved tests retain their existing adjustment families.
Main H2 and H4 include upper brackets comparing the two pressure-specific aid
effects; the endpoints sit at the centres of the corresponding manual/aided
pairs. Main H5 and H6 show reliability comparisons, pressure comparisons, and
their interaction. Exploratory H3 brackets the between-group perceived-reliability comparison;
its significance comes from the saved test, not the Morey bar overlap.
Exploratory H5 uses new paired t tests matching its unadjusted observed history means,
Holm-adjusted across the four pressure x reliability cells. These differ from
the saved covariate-adjusted lag tests. Main H8 symbols test total gain against zero;
there is no significance bracket between its accounting components. Exploratory H4 scatter
points have no two-condition comparison bracket. CI overlap is never the formal
test. Main H7-H9 retain a brief note of their exploratory origin; the three
remaining exploratory figures remain explicitly labelled exploratory.

Each figure exports its participant inputs and plotted values; each annotation
exports its current hypothesis ID, original test family, contrast, p-values,
adjustment source, dagger flag and position.
The presentation validator independently reconstructs the Morey intervals and
checks every displayed test against the matching source or raw paired scores.

The earlier detailed figures and multi-question composites remain supporting
outputs outside `presentation/`. Their own captions define their symbols and
intervals. They include individual observations, full condition means, absolute
rating errors, association curves, and the original exploratory H2 benchmark
component decomposition (now primary H7). All
plotting commands reuse the saved analyses without refitting.

## Author
Russell J. Boag
