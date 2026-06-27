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

The task uses a three-block within-participant design. There is no calibration block. Before the first main block, each participant completes a 20-trial `PRACTICE` block to familiarise them with the 2-decision trial sequence. Each participant then completes one `MANUAL`, one `AIDFIRST`, and one `STIMFIRST` block. Each trial has two mouse-click decision phases; the second decision is the final answer.

| Code | Mode | Trial structure | Trials |
| --- | --- | --- | ---: |
| `PRACTICE` | Manual-style practice | fixation -> masked aid preview 1000 ms -> fixation -> stimulus until decision 1 -> fixation -> masked placeholder until decision 2 -> feedback | 20 |
| `MANUAL` | Manual control | fixation -> masked aid preview 1000 ms -> fixation -> stimulus until decision 1 -> fixation -> masked placeholder until decision 2 -> feedback | 260 |
| `AIDFIRST` | Automation | fixation -> aid preview 1000 ms -> fixation -> stimulus until decision 1 -> fixation -> masked placeholder until decision 2 -> feedback | 260 |
| `STIMFIRST` | Automation | fixation -> masked aid preview 1000 ms -> fixation -> stimulus until decision 1 -> fixation -> aid until decision 2 -> feedback | 260 |

All blocks sample stimulus difficulty from a fixed delta distribution derived from a prior 80%-calibrated virus task dataset. The current task uses the across-participant mean (`delta = 0.040324718919`) and SD (`0.014615991726`) of each prior participant's last 150 calibration-trial staircase deltas; `derive_prior_calibration_delta.R` reproduces these constants from the local prior data file. The automated aid uses a single global reliability of 85% in the `AIDFIRST` and `STIMFIRST` blocks. The `PRACTICE` and `MANUAL` blocks show the masked aid string `#####` instead of a real recommendation during masked preview and final-decision screens. Real aid recommendations display as `BLACK` or `WHITE`; these indicate that the aid recommends the `V-BLACK` or `V-WHITE` response, respectively.

Responses are made with fixed mouse-click buttons. The left button is `V-BLACK` and maps to the internally stored `BLACK` response. The right button is `V-WHITE` and maps to the internally stored `WHITE` response. On the second decision screen, the same two buttons are shown with parenthetical text indicating whether each option would confirm or switch the participant's initial decision.

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

Block order is assigned deterministically from participant ID as `(participant_id - 1) %% 6`, so the order advances every participant.

For the planned sample of `N = 60`, this gives:

- 10 participants per block order overall.
- 20 appearances of each condition in each serial position.

## Output fields

The main trial and post-block output files include fields that identify the design cell:

| Field | Meaning |
| --- | --- |
| `condition_code` | `MANUAL`, `AIDFIRST`, or `STIMFIRST` |
| `aid_condition` | `manual`, `aid_first`, or `stimulus_first_change` |
| `aid_accuracy_setting` | `0.85` for aided automation blocks, or blank for `MANUAL` |
| `trial_deadline_s` | Blank in the current self-paced design |
| `preview_display`, `preview_label` | Display type and visible value shown on the initial preview screen |
| `decision1_display`, `decision2_display` | Display type for each decision phase |
| `decision2_label` | Visible value shown during the second decision phase, such as `#####`, `BLACK`, or `WHITE` |
| `decision1_response`, `decision1_correct`, `decision1_rt_s`, `decision1_matches_aid` | First automation classification fields |
| `decision2_response`, `decision2_correct`, `decision2_rt_s`, `decision2_matches_aid` | Second automation classification fields |
| `changed_response` | Whether decision 2 differs from decision 1 |

The current schema intentionally omits older compatibility aliases such as `condition_deadline_code`, `initial_*`, `final_*`, and generic final-response fields (`response`, `correct`, `rt_s`). Timing fields are recorded in seconds only.

Practice trials are saved separately as `results_p###_<timestamp>_b00_PRACTICE.csv` with `condition_code = PRACTICE`. They are not included in `b00_ALL.csv`, final performance scoring, or post-block measures.

Single-block runs require an explicit aid condition, for example:

```r
run_task(block = "AUTOMATION", aid_condition = "manual")
run_task(block = "AUTOMATION", aid_condition = "stimulus_first_change")
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
