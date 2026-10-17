# pygame_virus

Experimental task built in Pygame for studying decision-making and automation use.
Based on Bartlett & McCarley RDC task.

## Structure
- Python experiment code in `/python`
- Output data saved to `/output`
- Plots saved to `/plots`
- Analysis conducted in R

## Getting started
- Run `0-setup_pygame.R` to install Python, Pygame, and the pinned native keyboard backend (`psychtoolbox==3.0.22.2`).
- Use `1-run_virus_task.R` to launch task and pre-task instructions using the helper functions

## Semester 2 2026 source archive

The source bundle used for Semester 2 2026 is preserved in
[source_archives/semester2_2026](source_archives/semester2_2026/README.md).
The existing ZIP was moved without recompression or code changes. Its manifest
records the ZIP and all 11 member-file SHA-256 hashes and the verified source
revision. It retains the historical response-timing implementation. The original
working ZIP differed from the committed ZIP, but all member contents are identical;
both ZIP checksums are recorded in the manifest.

Future bundles must be built from the current repository source and include the
updated timing module, diagnostic, dependency setup, and instructions below.
Use a distinct release name and source manifest; never overwrite the semester
archive or use its extracted folder as the basis for a new bundle.

## Response timing

New trials use `rt_timing_version = ptb_queue_flip_v1`. A native Psychtoolbox
keyboard queue starts before trial preparation. Stimulus onset is recorded with
`GetSecs()` immediately after the first response-eligible display flip; RT is the
original key-down timestamp minus that onset, in full-precision seconds. Animation
and delayed event retrieval do not supply the response timestamp.

The first eligible D/J press wins. Pre-onset presses and repeated key-downs are
ignored; a held key must be released before it can respond again, while the other
key remains available. HP and LP deadlines remain 1 s and 3 s from stimulus onset.
A key timestamped before the deadline counts even if retrieved afterward; a key
at or after the deadline does not. New timeout rows have missing RT. Alt/Option+Q
and ordinary interface controls still use Pygame. The native queue stops before
feedback and is released on completion or error. Backend failure stops the task
with a timing error; there is no frame-based fallback.

Raw block files, combined participant files, and both collated trial CSVs retain:

| Field | Meaning |
| --- | --- |
| `rt_timing_version` | `ptb_queue_flip_v1` for new trials |
| `rt_backend_version` | Psychtoolbox package version |
| `stimulus_onset_s` | Native clock immediately after the first stimulus flip |
| `response_timestamp_s` | Accepted source key-down timestamp; missing on timeout |
| `n_ignored_pre_onset_keydowns` | Response-key downs between the initial held-state snapshot and onset |
| `response_readout_delay_s` | Retrieval time minus key-down time; missing on timeout |

These clock readings have a backend-defined origin, not a calendar origin.
`aid_onset_ms` remains the configured delay; `aid_onset_ms_rel` records actual aid
presentation after its first visible flip in the same clock domain. Simultaneous
aid has relative onset zero, advanced aid has a negative onset, and aid that never
appears has missing realised onset. A negative aid onset is distinct from RT.

Timing fields are optional when collating historical files. Existing negative,
zero, and timeout RT values are retained unchanged; old rows are not assigned the
new timing version. Participant identity corrections and matching of trial, trust,
and slider exports by run are unchanged. This runtime fix does not reconstruct
historical keypress times or change analysis/QC exclusions.

Run these checks with the collection computer's `r-pygame` Python (the local Mac
path is shown below):

```sh
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python -m unittest discover -s tests -p 'test_response_timing.py'
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python -m unittest discover -s tests -p 'test_participant_qc.py'
Rscript tests/test_participant_identity.R
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python python/check_response_timing.py --preflight
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python python/check_response_timing.py
```

The automated timing tests cover both deadline boundaries, 8/20/50 ms readout
stalls, held/early/repeated keys, both mappings and reliability patterns, all five
conditions, the predictable 96-cell allocation, signed aid onsets, cleanup, and
actual R collation of temporary mixed historical/new exports, including a 0.5 ms
RT difference. They do not write participant data or saved analyses.

Run native checks separately on each Windows or macOS collection computer. The
preflight checks native APIs and queue lifecycle; the interactive diagnostic asks
for D/J presses under 0/8/20/50 ms stalls and saves a separate JSON report under
`analysis_outputs/response_timing/` (or a new `--output` path). Timeouts do not
validate physical key input. On macOS, enable Input Monitoring for the launching
application (Terminal or R/RStudio), restart it, and run outside sandboxes that
deny native input access. Imports, `--help`, and screenshot tools do not start
keyboard capture. These checks establish numerical precision and timestamped
acquisition, not physical screen-to-keypress accuracy within ±1 ms.

The adapter and diagnostic were copied locally from the
`pygame_virus_dynamic_reliability` working tree on 17 October 2026 (source HEAD
`20f2a6644621`, with the timing implementation still uncommitted). The copied
files retain these source snapshot SHA-256 hashes:

```text
1121a271421c5c3fd1c9caabf74914ad71c1cc17c53f583f785fa66114edc0c6  python/response_timing.py
0b7cbe044ce4521024f984b3ba8ada06554dba37fadf222770b37e92e484d7a2  python/check_response_timing.py
```

Trial integration and tests are adapted to this repository. There are no runtime
imports from a sibling checkout.

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
Rscript tests/validate_time_pressure_hypotheses.R
```

The analysis defaults to `output/semester2_2026_data` and writes to
`analysis_outputs/semester2_2026_behavioural`. Optional positional arguments are
`[input_dir] [output_dir]`. The plotting script accepts `[analysis_dir] [plot_dir]`
and defaults to `plots/semester2_2026_behavioural`. Generated outputs are ignored
by Git. The 2026-10-17 refresh includes 87 analysis participants (44 HP95_LP65,
43 HP65_LP95), not the full planned sample of 96.

### Recorded participant ID correction (2026-10-17)

Two complete runs have recorded participant ID 83: `20261015_125827` and
`20261015_130522`; no run was recorded as participant 84. At the researcher's
instruction, these are treated as two different people, assuming an experimenter
ID-entry error. This identity assumption has not been independently confirmed.
The earlier run retains analysis ID 83 and the later run receives analysis ID 84.
The timestamp-specific mapping is in `config/participant_identity_corrections.csv`.

Raw exports remain unchanged. Combined CSVs and analysis manifests retain
`source_participant_id` and `run_timestamp`; their `participant_id` is the analysis
ID. Both runs actually received the allocation for recorded ID 83: order O21,
HP65_LP95, standard key mapping. Analysis ID 84 does **not** imply the planned
flipped mapping for task ID 84. The refresh therefore has 86 distinct allocation
cells, with the ID-83 cell represented twice; observed key mappings are 45 standard
and 42 flipped. Allocation validation uses the recorded ID and the live task
functions. Correction conflicts, analysis-ID collisions, and unexplained duplicate
runs fail explicitly rather than silently selecting or merging a run.

Refresh combined data and descriptive figures from the repository root:

```sh
Rscript collate_data.R output/semester2_2026_data data/semester2_2026_data
Rscript plot_staircase.R data/semester2_2026_data/data_virus_all.csv plots/semester2_2026_data semester2_2026 'Semester 2 2026' cohort data/semester2_2026_data/data_virus_sliders_all.csv
Rscript plot_calibration_manual_accuracy.R data/semester2_2026_data/data_virus_all.csv plots/semester2_2026_data calibration_manual_hp_lp_accuracy 'Calibration vs Manual HP/LP accuracy (Time-pressure task)'
Rscript plot_calibration_accuracy_by_group.R
Rscript plot_calibration_accuracy_by_group.R data/semester2_2026_data/data_virus_all.csv plots/semester2_2026_data/group semester2_2026_group_calibration_accuracy_distribution_compact compact
Rscript tests/test_participant_identity.R
```

Then run the behavioural pipeline above. The main analysis also refits the
exploratory models; the main plotting command regenerates exploratory figures
and the unified hypothesis report, including both SDT framings. The expected totals are 165,300
trials (including calibration), 1,044 trust-item rows, and 435 slider rows.

The analysis reads raw trial exports and same-run questionnaires, rejects duplicate
participant runs after documented identity correction, records input checksums, and checks block order, deadlines,
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
The hypothesis catalog retains historical presentation IDs H7, H8 and H9 for
original exploratory H2, H6 and H1, respectively. The source analyses below retain their original
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
caution given the existing participant-cell variation and the current 87-person
sample. Reliability at fixed pressure remains a between-group comparison.

In the 2026-10-17 refresh, all four main models and eight of nine exploratory
models passed the numerical checks. The perceived-reliability/agreement model
(`H3_perceived_agreement`) had a scaled gradient of 0.00204145, just above the
unchanged 0.002 threshold. Its inferential p-values are withheld, its prediction
figure is labelled accordingly, and participant-cluster sensitivity results are
retained. Numerical convergence does not resolve the extra participant-cell
variation described above.

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

### Primary hypotheses and figures

The canonical collection is **`plots/semester2_2026_behavioural/presentation/`**.
Open `index.html`, `core_hypotheses.pdf` (ten main figures), or
`all_hypotheses.pdf` (18 figures including supporting and historical views).
The revised framework was agreed on **17 October 2026 after inspection of the
previous results**. These are the primary analyses for the revised question list;
they are not preregistered. The current analysis includes **84 of 87 source
participants**, excluding **004, 010 and 048** under the reviewed QC decisions.
Both runs recorded as 083 remain included: `20261015_125827` retains analysis ID
083, and `20261015_130522` is analysis ID 084. Both keep the allocation actually
administered under recorded ID 083; this identity correction is not an exclusion.

Fit the primary analyses, then build the report:

```sh
Rscript analyse_time_pressure_primary.R
Rscript plot_time_pressure_hypotheses.R
Rscript tests/validate_time_pressure_primary.R
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python -m participant_qc.run_qc
```

The analysis accepts `[analysis_dir] [combined_csv]` and writes a separate
`analysis_outputs/semester2_2026_behavioural/primary/` collection. It reconstructs
scores, trust ratings, perceived reliability and history eligibility from source
files and fits the revised models. Reviewed participant/run decisions are stored
in `config/participant_exclusions.csv`, applied after identity correction and
before calculating any scores or fitting models. `primary/cohort_selection.csv`
accounts for all source participants and `primary/exclusions.csv` records the
applied reasons. An unknown run or invalid decision stops the analysis.
Previous primary analyses are archived on rerun; source data remain intact.

The plotting command accepts `[analysis_dir] [plot_dir] [combined_csv] [--stage-only]`.
It consumes saved primary results, rejects stale input/specification hashes, and
validates before publishing. It never silently refits models. `--stage-only`
retains a validated staging directory for visual review; the
`primary_publish_report(stage, plot_base)` function then publishes it. The preceding
presentation directory is moved intact into `plots/semester2_2026_behavioural/archive/`.
Standalone vector PDFs and 300-dpi PNGs use descriptive hypothesis-based filenames.
The existing slides, SDT and broader behavioural plotting entrypoints still
forward to this canonical builder; the primary analysis must be current first.

| Hypothesis | Question and contribution | Tests per primary outcome |
| --- | --- | --- |
| H1 | Manual pressure costs: accuracy, task discrimination and correct RT | One equally pattern-weighted HP-LP contrast each |
| H2 | Aid benefits: accuracy/task-dprime gains, RT effects; compensation and descriptive completion/choice decomposition | Accuracy and task dprime: 11 each; RT: nine |
| H3 | Human value beyond following the aid: all-trial accuracy and matched-human-answered task dprime | Nine each |
| H4 | Response bias: advice acceptance and BLACK category preference | Five each |
| H5 | Discrimination: advice selectivity and final stimulus discrimination | Five each |
| H6 | Reported trust, perceived reliability and signed calibration error | Five, five and four respectively |
| H7 | Within-person and between-person trust associations with advice bias/dprime | Two slopes per SDT outcome |
| H8 | Adjusted next-trial agreement after aid errors | Seven |

This gives **97 primary comparisons** across ten figures: H1, H2a-H2c, H3-H8.
The recurring five comparisons are 95%-65% at HP and LP, HP-LP at 65% and 95%,
and their interaction. H2/H3 additionally test four condition effects against
zero. H2 compensation compares aided HP with manual LP separately by allocation
pattern; it is part of the aid-benefit question, not independent evidence.
Completion/choice decomposition is descriptive accounting, not causal mediation.

`config/hypothesis_figures.csv` is the panel-level contract for order, filenames,
model, outcome, required contrast IDs, correction family and historical IDs.
`hypothesis_mapping.csv` records the merge/reframing crosswalk. Former alternate
H5 and H9 become the paired aid/task **H4 bias** and **H5 discrimination** figures.
Each panel shows all five comparisons, including nonsignificant results. The
four-panel SDT overview and contrast overview are supporting synthesis. Original
conditional-agreement figures retain their original test definitions and separate
families in the historical section, with models and participant data regenerated
on the same included cohort. No full-cohort figure is copied into the current report.

**Models and tests.** Trial-level accuracy and history agreement use logistic
mixed models; correct positive finite RTs use a log-RT mixed model; SDT scores,
ratings and participant human-minus-aid differences use linear mixed models.
All include participant random intercepts. Performance models use
`mode * pressure * allocation pattern`; aided-score models use
`pressure * reliability`. H1 averages the two allocation patterns equally.
Simple aided-condition comparisons at fixed pressure/reliability compare
allocation groups; participant sensitivities retain the correct within-person
covariance and between-group structure.

H7 separates person-mean trust from block deviations and adjusts for
`pressure * reliability` and block position. Both common slopes are primary;
there are no extra pressure-specific slope tests. Trust is measured after each
block, so these are associations, not causal effects. H7 scatter lines and shaded
pointwise 95% confidence bands come from these saved mixed models, with separate
lines by allocation pattern over its observed trust range. Within-person panels
predict the HP-minus-LP difference with trust deviations set to plus/minus half
the displayed trust difference. Between-person panels average HP/LP predictions
with trust deviations set to zero and person-mean trust set to the displayed
value. Block position is fixed at its observed pattern-by-pressure mean. Bands
use the full fixed-effect covariance and Satterthwaite degrees of freedom for the
mean or difference; they are not individual prediction intervals. The plotted
predictions and reference values are exported in `H07_trust_and_aid_sdt_fits.csv`.
The sensitivity comparison scatters retain equality lines, since their purpose
is to compare estimates from alternative analysis methods.

H8 controls current advice
correctness, previous response error and trial progress; it standardises current
advice correctness to assigned reliability. Its lags are computed before filtering
and never bridge timeouts, missing trials or block boundaries.

Tests are two-sided, with **Holm correction separately within hypothesis and
outcome**. H2's accuracy/task-dprime families include the compensation contrasts
across subfigures. Participant and correction sensitivities have separately
labelled families and never silently replace primary tests. All figures use
pointwise 95% model CIs with raw participant data visible. Accuracy/agreement
contrasts are response-scale percentage points; RT contrasts are ratios derived
from log contrasts. Holm-adjusted p-values drive every significance annotation.
An unavailable test is labelled unavailable, not nonsignificant.

Loglinear SDT correction `(k + 0.5)/(n + 1)` is primary; extreme-only correction
is a sensitivity analysis. Both frames exclude timeouts but retain answered
trials with nonpositive RT. Advice -c is acceptance and advice dprime is selectivity;
task -c is BLACK preference and task dprime is final stimulus discrimination.
These summarise the same decisions, not independent replication. All-trial
accuracy includes timeout costs. H3's accuracy benchmark assumes timely advice
on every trial; its task-SDT benchmark uses exactly human-answered trials.
Nonsignificant compensation/benchmark differences do not establish equivalence.

**Validation and audit.** The validator independently reconstructs source counts,
SDT scores, performance, benchmark differences, history eligibility, model contrast
arithmetic, CIs and Holm adjustments. It requires all 97 registry annotations,
checks each against its model and plotted figure, verifies report links and PDF
page counts, and checks input hashes. `tests/validate_time_pressure_hypotheses.R`
is a compatibility entrypoint for this validator. The older component validators
remain available for historical component outputs.

The report exports plotted model values, participant data, annotation tables,
methods, model diagnostics, correction sensitivity and participant sensitivity.
Convergence, singularity, rank, residual and conditional dispersion checks are
retained; numerical convergence does not establish model adequacy. Singular,
nonconverged or rank-deficient fits do not supply primary p-values. Rendered visual
checks and protected-file hashes are retained in
`analysis_outputs/semester2_2026_behavioural/primary_validation/`.

## Author
Russell J. Boag

## Participant exclusion audit

The standalone `participant_qc/` pipeline adapts the audit from
`pygame_virus_dynamic_reliability` to this study's single LP calibration, four
counterbalanced experimental blocks, and 1 s HP / 3 s LP deadlines. It generates
review evidence and an exclusion-candidate list; **it does not remove participants
or trials, rewrite canonical CSVs, or rerun behavioural analyses**.

```bash
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python participant_qc/run_qc.py
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python -m unittest discover -s tests -p 'test_participant_qc.py'
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python tests/validate_participant_qc.py --expect-current-cohort
```

Open `analysis_outputs/semester2_2026_qc/index.html` for the report. The generator
accepts `--input`, `--config`, `--output-dir`, and `--primary-analysis-dir`; defaults are
`data/semester2_2026_data/data_virus_all.csv`, `participant_qc/config.json`, and
`analysis_outputs/semester2_2026_qc/`. NumPy and Matplotlib are available in
`r-pygame`; validation also uses Pillow and `pdfinfo`. The optional validator
`--expect-current-cohort` checks the first 87-person snapshot; omit it for future
cohorts. Sample size is otherwise data-driven. The primary-analysis directory
defaults to `analysis_outputs/semester2_2026_behavioural/primary/`. When completed
primary outputs are present, the audit verifies their input and exclusion hashes
and records the actually applied selection. Run the primary analysis first after
changing exclusion decisions. The QC audit itself always retains all source
participants, including those excluded from the hypothesis analyses.

### Agreed first-pass criteria

These are retrospective descriptive screens, not preregistered exclusions or
proof of equivalence to chance. The thresholds were retained from the sibling
audit by agreement on 2026-10-17.

| Screen | Scope | Use |
| --- | --- | --- |
| Absolute accuracy deviation from 77% **>10 percentage points** | Final 150 `CAL_LP` trials | Exclusion candidate |
| Same target-deviation screen | Whole `M_LP` block | Diagnostic only |
| All-trial accuracy **<=55%** | Each whole scope below | Exclusion candidate |
| RT **<200 ms** on **>=20% of all trials**, **>=20 fast responses**, and fast-response accuracy **<=55%**, jointly within the same scope | Each whole scope below | Exclusion candidate |

Whole scopes are full `CAL_LP` (300 trials), each of `M_HP`, `A_HP`, `M_LP`, and
`A_LP` (400 each), the 800 pooled aided trials, and all 1,600 experimental trials.
The calibration tail is used only for the calibration-target screen. There is no
manual-target screen for `M_HP`. An independent near-chance or anticipatory flag
in `M_LP` can still select a candidate even though its target-deviation flag is
diagnostic only. High HP timeout frequency alone is not a separate criterion.

Accuracy includes timeouts as incorrect. Fast-rate denominators include all
trials; fast accuracy uses only fast answered trials. Exactly 200 ms is not fast;
finite zero and negative answered RTs are counted as fast and reported separately.
Empty fast-response subsets have missing accuracy, not zero. No trials are trimmed.
Answered-trial accuracy and timeout frequency are shown alongside all-trial
accuracy to aid interpretation of HP performance. Wilson intervals are descriptive
and do not account for serial dependence or adaptive calibration.

The task can record a keypress timestamp and then label that same frame as a
`TIMEOUT`. Such rows retain their timeout classification even when `rt_s` is
present. Scope metrics record these cases and answered RTs beyond nominal deadlines
separately; neither creates an additional exclusion rule.

### Report, comparisons, and provenance

The report includes an exclusion-summary table, unique candidate IDs and rule
overlaps, a searchable review table, four cohort figures, diagnostic PDFs/PNGs for
every participant, and matched comparison pages for every anticipatory candidate.
CSV/JSON exports include participant/scope summaries, all checks, flagged checks,
integrity findings, 50-trial bins, reference pools, comparator ranking, and plot
sources. Each participant is counted once in candidate totals; diagnostic-only
flags and unassessable checks are separate. Missing or invalid scopes are never
reported as clear. Missing schema, duplicate trial identities, or unexplained
multiple participant runs stop generation.

Individual traces follow administered chronological order and display the relevant
deadline. Peer bands use other complete participants with the same reliability
pattern and no anticipatory flag, aligned by **condition and within-block trial**.
They are descriptive references across block orders, not adjustments for order
effects. Comparators additionally match key mapping; among eligible participants,
select the smallest sum of absolute MAD-standardised distances from the subgroup
medians of experimental accuracy and positive median RT. Zero-MAD terms contribute
zero; participant ID breaks ties. Other exclusion or diagnostic flags do not
silently remove comparison participants. All pool IDs, rankings, and missing
comparators are exported.

Recorded IDs and run timestamps remain alongside analysis IDs. The audit validates
`config/participant_identity_corrections.csv`, including the later recorded-p083
run assigned analysis ID 084. Allocation is checked against `python/virus_task.py`
using the **recorded** ID, preserving that run's administered allocation.

Builds use a staging directory and are published only when generation completes.
Previous reports are retained as `.previous_<timestamp>` siblings. The manifest
hashes the combined input, QC configuration/source, identity corrections, task
source, and all generated artifacts. The independent validator recomputes metrics,
intervals, flags, candidate unions, reference bands, and comparator selections;
it also checks provenance, local HTML links, PDF page counts, and PNG integrity.
After plot changes, inspect rendered PDFs as well as running automated checks.

The first validated 87-participant audit (2026-10-17) selected **004, 010, and 048**
as candidates (3/87, 3.4%; 84 would remain if applied). All three met the
anticipatory criterion; 004 also met the near-chance criterion. No participant
failed the calibration-target criterion. Fourteen participants had an `M_LP`
target-deviation diagnostic flag, including candidate 010; the other 13 were
diagnostic-only. All scopes were assessable. The initial audit did not apply exclusions.

The subsequent user-approved selection excludes 004, 010 and 048 from all current
hypothesis tests and figures, leaving **84 participants**. The QC index, summary
JSON and `exclusion_summary.txt` document the applied decisions. The identity
section and `identity_corrections_summary.csv` explicitly record both p083 runs,
their analysis IDs (083/084), retained status and unchanged administered allocation.
Identity reuse does not add an exclusion or reduce the 87-person source count.
Validation receipts and visual-review evidence are saved separately in
`analysis_outputs/semester2_2026_qc_validation/`.

## Historical task-performance SDT component: stimulus versus final response

The section below documents the earlier component analysis and its historical
hypothesis IDs, test families and results. The revised primary H1-H8 framework
above supersedes its public figure sequence and primary inference.

`analyse_time_pressure_task_sdt.R` supplies the original stimulus-response SDT
component alongside the original advice-based H5/H9 analysis. It retains all 87 participants, including
the three QC candidates, and preserves existing analyses, figures, and adjustment
families. These outcomes were added after inspecting the existing results.

```bash
Rscript plot_time_pressure_hypotheses.R
Rscript tests/validate_time_pressure_task_sdt.R
```

Task analysis tables are saved in
`analysis_outputs/semester2_2026_behavioural/task_sdt/`. Its six component figures
remain in the historical component collection. The canonical report instead uses
the revised primary models and consolidated figures. The component validator defaults to the internal
`hypothesis_components/presentation/task_sdt/` outputs. For an isolated component
run, the explicit three-argument `analyse_time_pressure_task_sdt.R` interface
accepts `[input_csv] [analysis_dir] [plot_dir]`; both output directory basenames
must be `task_sdt`.

### Definitions and estimands

BLACK is the signal category and a BLACK classification is the positive response,
independent of physical key mapping. Hits are BLACK responses to BLACK stimuli;
false alarms are BLACK responses to WHITE stimuli. For every participant/block:

- `dprime = qnorm(H) - qnorm(F)` measures task-category discrimination.
- `black_bias = -c = (qnorm(H) + qnorm(F))/2`; positive values favour BLACK.
- `criterion_c = -black_bias` is also exported.

Human scores use answered trials, including finite nonpositive RTs; they do not
condition on response correctness or agreement. Calibration is excluded. The
primary loglinear correction is `(k + 0.5)/(n + 1)` for each stimulus-specific rate;
zero/one-rate-only correction is exported as a sensitivity analysis. Both stimulus
categories must have answered trials; otherwise the script stops inferential
output rather than silently dropping the participant. Four-cell confusion counts
and extreme-rate diagnostics are retained.

Aided scores describe the **participant-plus-aid system**, under conventional
equal-variance SDT. They do not isolate sensory ability or establish how visual and
advice evidence were integrated. Category bias is distinct from advice acceptance;
a larger bias difference is not inherently a benefit. Merely replacing agreement
with correctness while retaining advice-correctness conditioning would re-express
the old SDT measures, rather than provide this stimulus-response analysis.

### Contrasts and figures

The six figures show task discrimination, category bias, paired aided-manual gains,
H1/H3 pressure contrasts, H7 aid-alone comparisons, and completion/accuracy context.
Overview plots retain separate allocation groups so each manual baseline matches
the corresponding aided participants.

Inference uses equally weighted participant scores. Within-pattern comparisons
retain pairing; between-pattern comparisons use independent groups and
Welch-Satterthwaite degrees of freedom. Fresh two-sided tests are calculated for
both `dprime` and `black_bias`:

| Family | Contrasts per outcome/correction |
| --- | --- |
| H1 | Manual HP minus manual LP in each allocation group: 2 |
| H2 | Four paired aided-manual gains; two within-pattern HP-minus-LP gain contrasts; their between-pattern difference; 95%-minus-65% gain at each pressure: 9 |
| H3 | Aided HP minus manual LP in each allocation group: 2 |
| H7 | Four human-minus-aid cell tests versus zero and five pressure/reliability contrasts: 9 |

Holm adjustment is separate within each hypothesis, outcome, and correction.
An additional adjustment across all 44 tests per correction is exported. Plot
intervals are pointwise 95% t intervals. H3 estimates the remaining gap; a
nonsignificant contrast does not establish equivalence or noninferiority.

The H7 benchmark scores the aid's recommendations on **exactly the trials that the
human answered**, using the same correction and within-participant subtraction.
Aid scores over all trials are exported separately as descriptive context, not
substituted into these matched-trial tests. Completion rates, completion within
each stimulus category, answered accuracy, and all-trial accuracy are also
exported. Timeouts count as incorrect for all-trial accuracy. Conditional SDT
cannot measure timeout costs, and selection into answering may differ across
conditions.

### Reproducibility and first-pass results

Outputs include participant confusion tables and scores, paired gains and
benchmarks, cohort/run identities, contrast weights, all 88 tests across the two
corrections, correction sensitivity, plotted data/annotations, methods, session
information, and input/source/artifact hashes. The validator independently
reconstructs the counts from the source CSV and reconciles all contrasts with
direct `t.test` calculations, then checks plot data, corrections, HTML links, and
PDF/PNG integrity. Render and inspect the PDFs after plot changes.

In `participant_aid_gains.csv` and `participant_aid_benchmark.csv`, `dprime` and
`black_bias` hold paired differences. The `human_`, `manual_`, and `aid_` columns
retain their component scores; absolute criteria and rates are in
`participant_task_sdt.csv`.

For the initial 87-person analysis, the 95% aid increases answered-trial task
`dprime` relative to manual under HP (about +0.72) and LP (about +0.79); both
Holm-adjusted tests are significant. The 65% aid has no detectable paired change
under either pressure. Final decisions outperform the 65% aid alone and fall
short of the 95% aid alone on matched answered trials. These are exploratory,
conditional discrimination results. All 44 Holm decisions agree between the
loglinear and extreme-only corrections; this does not remove other SDT assumptions
or uncertainty about timeout selection.
