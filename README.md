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

The task uses a mixed design. Calibration is unchanged and always occurs first. After calibration, participants complete three automation blocks that manipulate when the automated aid and stimulus information become available. There is no post-calibration manual block. Automation decision phases remain on screen until the required response is made.

| Code | Mode | Trial structure | Trials |
| --- | --- | --- | ---: |
| `SIM` | Automation | fixation -> masked aid preview 1000 ms -> fixation -> aid + stimulus until decision 1 -> fixation -> masked placeholder until decision 2 -> feedback | 400 |
| `AIDFIRST` | Automation | fixation -> aid preview 1000 ms -> fixation -> stimulus until decision 1 -> fixation -> masked placeholder until decision 2 -> feedback | 400 |
| `STIMFIRST` | Automation | fixation -> masked aid preview 1000 ms -> fixation -> stimulus until decision 1 -> fixation -> aid until decision 2 -> feedback | 400 |

The calibration block contains 300 manual staircase trials with a 10 s response window and targets 85% unaided accuracy. Automation blocks use the calibration-derived fixed difficulty for the participant. The automated aid uses a single global reliability of 85% in all automation blocks.

Participant-facing automation instructions are qualitative rather than numeric. Participants are told that the aid is reasonably reliable but not perfect, and that automation advice errors remain possible.

## Counterbalancing

The post-calibration block order uses balanced rotations of the three aid-condition blocks:

| Order | Sequence |
| --- | --- |
| `O1` | `SIM -> AIDFIRST -> STIMFIRST` |
| `O2` | `AIDFIRST -> STIMFIRST -> SIM` |
| `O3` | `STIMFIRST -> SIM -> AIDFIRST` |

Block order and key mapping are assigned deterministically from participant ID:

- Block order: `(participant_id - 1) %% 3`, so the order advances every participant.
- Key mapping: standard for participant IDs 1-3 within each 6-ID counterbalancing cycle, flipped for participant IDs 4-6.

The full joint cycle for order and key mapping is 6 participants:

| Participant IDs in cycle | Key mapping | Order |
| --- | --- | --- |
| 1 | standard | `O1` |
| 2 | standard | `O2` |
| 3 | standard | `O3` |
| 4 | flipped | `O1` |
| 5 | flipped | `O2` |
| 6 | flipped | `O3` |

For the planned sample of `N = 60`, this gives:

- 20 participants per block order overall.
- 30 standard-key and 30 flipped-key participants.
- 10 participants in each order x key mapping cell.

The standard key mapping is `D = V-BLACK` and `J = V-WHITE`. The flipped key mapping is `J = V-BLACK` and `D = V-WHITE`.

## Output fields

The main trial and post-block output files include fields that identify the design cell:

| Field | Meaning |
| --- | --- |
| `block` | `CALIBRATION` or `AUTOMATION` |
| `condition_code` | `CAL`, `SIM`, `AIDFIRST`, or `STIMFIRST` |
| `condition_deadline_code` | Compatibility alias for `condition_code` |
| `automation_reliability_group` | Compatibility field; `none` in the current design |
| `aid_accuracy_setting` | `0.85`, or blank for calibration |
| `aid_condition` | `simultaneous`, `aid_first`, `stimulus_first_change`, or blank for calibration |
| `trial_deadline_ms` | Fixed response window in milliseconds |
| `trial_deadline_s` | Fixed response window in seconds |
| `decision1_display`, `decision2_display` | Display type for each automation decision phase |
| `decision1_response`, `decision1_correct`, `decision1_rt_s`, `decision1_rt_ms`, `decision1_matches_aid` | First automation classification fields |
| `decision2_response`, `decision2_correct`, `decision2_rt_s`, `decision2_rt_ms`, `decision2_matches_aid` | Second automation classification fields |
| `initial_response`, `initial_correct`, `initial_rt_s`, `initial_rt_ms` | Compatibility aliases for decision 1 |
| `final_response`, `final_correct`, `final_rt_s`, `final_rt_ms` | Compatibility aliases for decision 2 |
| `changed_response` | Whether decision 2 differs from decision 1 |
| `response`, `correct`, `rt_s`, `rt_ms` | Primary-analysis aliases for decision 2 |

Single-block automation runs require an explicit aid condition, for example:

```r
run_task(block = "AUTOMATION", aid_condition = "simultaneous")
run_task(block = "AUTOMATION", aid_condition = "stimulus_first_change")
```

## Author
Russell J. Boag
