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

The generated `instruction_screenshots/`, `virus_task_screenshots/`, and `screenshots_review.pptx` artifacts are local review outputs and are ignored by git.
The deck builder requires `pandoc` and Pillow.

## Current task design

This design extends Wanghuan's reliability-drop design by additionally manipulating participants' unaided performance level through calibration. Calibration always occurs first. Participants are assigned deterministically from participant ID to one of two calibration targets:

| Calibration group | Target unaided accuracy |
| --- | ---: |
| `CAL65` | 65% |
| `CAL85` | 85% |

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

The critical phase is the 70% aided block. For `CAL65` participants, the 70% aid remains potentially useful because it is 5 percentage points more accurate than their calibrated unaided performance. For `CAL85` participants, the 70% aid is 15 percentage points less accurate than their own calibrated performance and should therefore be discounted or ignored.

This design tests whether participants respond only to absolute changes in aid reliability, or whether they learn the relative value of the aid compared with their own competence. It also tests whether prior exposure to a highly reliable aid produces over-reliance when the aid later becomes only moderately reliable, especially when the aid is no longer objectively useful.

## Counterbalancing

- Calibration target: participant IDs alternate between `CAL65` and `CAL85`.
- Main block sequence: all participants complete `CAL -> MAN/PRE_AUTOMATION -> REL_DROP -> MAN/POST_AUTOMATION`.
- Key mapping: standard for the first two participants within each four-participant cycle, flipped for the next two participants.

This gives a complete four-participant counterbalance over calibration target and key mapping:

| Participant cycle position | Calibration group | Key mapping | Main block sequence |
| ---: | --- | --- | --- |
| 1 | `CAL65` | `D = V-BLACK`, `J = V-WHITE` | `SPLIT_MANUAL` |
| 2 | `CAL85` | `D = V-BLACK`, `J = V-WHITE` | `SPLIT_MANUAL` |
| 3 | `CAL65` | `J = V-BLACK`, `D = V-WHITE` | `SPLIT_MANUAL` |
| 4 | `CAL85` | `J = V-BLACK`, `D = V-WHITE` | `SPLIT_MANUAL` |

The standard key mapping is `D = V-BLACK` and `J = V-WHITE`. The flipped key mapping is `J = V-BLACK` and `D = V-WHITE`.

Participants report perceived self accuracy after calibration and after each manual segment. After each aided reliability phase, participants report perceived automation accuracy, perceived self accuracy, and trust in the aid.

## Output fields

The main trial and post-block output files include fields that identify the design cell:

| Field | Meaning |
| --- | --- |
| `block` | `CALIBRATION`, `MANUAL`, or `AUTOMATION` |
| `condition_code` | `CAL`, `MAN`, or `REL_DROP` |
| `condition_deadline_code` | Compatibility alias for `condition_code` |
| `calibration_target_group` | Participant-ID assigned calibration group, `CAL65` or `CAL85` |
| `calibration_target_accuracy` | Calibration target accuracy, `0.65` or `0.85` |
| `main_block_order` | Fixed main-block sequence label, `SPLIT_MANUAL` |
| `manual_segment` | Manual segment label, `PRE_AUTOMATION` or `POST_AUTOMATION`; blank for non-manual rows |
| `dynamic_reliability_family` | Compatibility label for the aided sequence, `DROP95_70_95` |
| `reliability_block_idx` | Compatibility alias for aided reliability phase index, `1`-`3` |
| `trial_in_reliability_block` | Compatibility alias for trial index within the current reliability phase |
| `reliability_phase_idx` | Aided reliability phase index, `1`-`3` |
| `trial_in_reliability_phase` | Trial index within the current reliability phase |
| `reliability_phase_label` | Phase label such as `P1_95`, `P2_70`, or `P3_95` |
| `aid_reliability_level` | Current aided-phase aid accuracy level |
| `aid_accuracy_setting` | Accuracy setting used to generate the aid recommendation on the current trial |
| `automation_reliability_group` | High/low grouping derived from aid reliability |
| `trial_deadline_ms` | Fixed response window in milliseconds |
| `trial_deadline_s` | Fixed response window in seconds |

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
