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

Calibration is unchanged and always occurs first. After calibration, participants complete one dynamic automation block in which aid reliability changes across 50-trial mini-blocks. The experimental goal is to test whether participants learn to track the aid's reliability as it changes over time.

| Code | Mode | Response window | Trials |
| --- | --- | ---: | ---: |
| `CAL` | Manual calibration | 10 s | 300 |
| `DYNREL` | Dynamic automation | 6 s | 1200 |

The automation block uses the calibration-derived fixed difficulty for the participant. The aid appears with the stimulus, and aid onset is not manipulated.

Automation reliability changes every 50 trials. The dynamic automation block contains 24 reliability mini-blocks, so each participant sees six mini-blocks at each aid-accuracy level:

| Aid accuracy level | Mini-blocks per participant | Trials per mini-block |
| --- | ---: | ---: |
| 65% | 6 | 50 |
| 75% | 6 | 50 |
| 85% | 6 | 50 |
| 95% | 6 | 50 |

Participant-facing automation instructions are qualitative rather than numeric. Participants are told that the aid's reliability may change over time, but they are not told the numeric reliability levels or the current mini-block's reliability.

## Counterbalancing

Dynamic reliability trajectories are assigned deterministically from participant ID. The base 24-mini-block schedule is:

```text
65, 85, 75, 95, 85, 65, 95, 75,
75, 65, 95, 85, 65, 75, 85, 95,
95, 75, 85, 65, 75, 95, 65, 85
```

The trajectory family is `(participant_id - 1) %% 4`, with four cyclic rotations of the base schedule. This preserves six mini-blocks per reliability level for every participant while counterbalancing which reliability levels occur early.

- Key mapping: standard for participant IDs 1-8 within each 16-ID keymap cycle, flipped for participant IDs 9-16.

The standard key mapping is `D = V-BLACK` and `J = V-WHITE`. The flipped key mapping is `J = V-BLACK` and `D = V-WHITE`.

After each 50-trial automation mini-block, participants report perceived automation accuracy and self accuracy and complete the trust questionnaire.

## Output fields

The main trial and post-block output files include fields that identify the design cell:

| Field | Meaning |
| --- | --- |
| `block` | `CALIBRATION` or `AUTOMATION` |
| `condition_code` | `CAL` or `DYNREL` |
| `condition_deadline_code` | Compatibility alias for `condition_code` |
| `dynamic_reliability_family` | Participant-ID assigned trajectory family, `F1`-`F4` |
| `reliability_block_idx` | Automation mini-block index, `1`-`24` |
| `trial_in_reliability_block` | Trial index within the current 50-trial reliability mini-block |
| `aid_reliability_level` | Current mini-block aid accuracy level |
| `aid_accuracy_setting` | Accuracy setting used to generate the aid recommendation on the current trial |
| `trial_deadline_ms` | Fixed response window in milliseconds |
| `trial_deadline_s` | Fixed response window in seconds |

Single-block runs can be selected for calibration-only or automation-only checks:

```r
run_task(block = "CALIBRATION")
run_task(block = "AUTOMATION")
```

## Author
Russell J. Boag
