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
The exploratory figure bundle contains seven individual figures and three 16:9
composites, in PDF and 300-dpi PNG. Every figure exports its plotted values.

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

The primary figures now follow this presentation order:

1. `slide_manual_pressure_costs`: manual HP/LP accuracy and RT means, with direct
   pressure contrasts beside them. Allocation groups retain separate baselines.
2. `slide_1_performance`: accuracy and RT benefits, arranged by pressure and aid
   reliability. Each benefit uses the same participants' manual condition.
3. `slide_2_compensation`: actual manual-LP and aided-HP accuracy, followed by the
   direct compensation contrast. Numeric contrast labels are participant estimates.
4. `slide_3_reliance_trust`: correct-advice agreement, incorrect-advice agreement,
   and trust means above direct reliability contrasts. The two agreement mean
   panels share a 0-100% scale; their contrast panels also share a common scale.

Blue denotes 95% advice, orange 65%, and grey manual means. Open diamonds denote
mixed-model contrasts and filled circles participant sensitivity estimates.
Headline means are participant summaries; their CIs and the contrast CIs are
pointwise 95% intervals. Formal tests retain Holm adjustment within outcome.
No participant lines connect the different groups in the reorganised aided means.
Full condition means and participant detail remain in `accuracy`, `correct_rt`
and `timeouts`; participant agreement/trust values are also exported as CSVs.
`manual_pressure_costs` provides a manuscript-sized counterpart to the new slide.
All four manuscript captions and estimand details are in
`data/primary_figure_captions.txt`. Plotting reuses the saved analyses without refitting.

## Author
Russell J. Boag
