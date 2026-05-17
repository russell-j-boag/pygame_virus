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

The task uses a mixed design. Calibration is unchanged and always occurs first. After calibration, participants complete three automation blocks that manipulate aid onset relative to stimulus onset. There is no post-calibration manual block and response deadline is not manipulated; the automation blocks all use the same 6 s response window.

| Code | Mode | Aid onset | Response window | Trials |
| --- | --- | ---: | ---: | ---: |
| `AB500` | Automation | 500 ms before stimulus | 6 s | 400 |
| `AS0` | Automation | simultaneous with stimulus | 6 s | 400 |
| `AA500` | Automation | 500 ms after stimulus | 6 s | 400 |

The calibration block contains 300 manual staircase trials with a 10 s response window. Automation blocks use the calibration-derived fixed difficulty for the participant.

Automation reliability is a between-subjects factor:

| Reliability group | Aid accuracy |
| --- | ---: |
| `high` | 95% |
| `low` | 65% |

Participant-facing automation instructions are qualitative rather than numeric. The high-reliability group is told that the aid is highly reliable but not perfect. The low-reliability group is told that the aid is reasonably reliable and that errors may be relatively common.

## Counterbalancing

The post-calibration block order uses balanced rotations of the three aid-onset conditions:

| Order | Sequence |
| --- | --- |
| `O1` | `AB500 -> AS0 -> AA500` |
| `O2` | `AS0 -> AA500 -> AB500` |
| `O3` | `AA500 -> AB500 -> AS0` |

Reliability group, block order, and key mapping are assigned deterministically from participant ID:

- Reliability: `high` for odd participant IDs, `low` for even participant IDs.
- Block order: `floor((participant_id - 1) / 2) %% 3`, so each adjacent high/low pair receives the same order and the order advances every two participants.
- Key mapping: standard for participant IDs 1-8 within each 16-ID keymap cycle, flipped for participant IDs 9-16.

The full joint cycle for reliability, order, and key mapping is 48 participants. The first 16 assignments are:

| Participant IDs in cycle | Reliability | Key mapping | Order |
| --- | --- | --- | --- |
| 1 | `high` | standard | `O1` |
| 2 | `low` | standard | `O1` |
| 3 | `high` | standard | `O2` |
| 4 | `low` | standard | `O2` |
| 5 | `high` | standard | `O3` |
| 6 | `low` | standard | `O3` |
| 7 | `high` | standard | `O1` |
| 8 | `low` | standard | `O1` |
| 9 | `high` | flipped | `O2` |
| 10 | `low` | flipped | `O2` |
| 11 | `high` | flipped | `O3` |
| 12 | `low` | flipped | `O3` |
| 13 | `high` | flipped | `O1` |
| 14 | `low` | flipped | `O1` |
| 15 | `high` | flipped | `O2` |
| 16 | `low` | flipped | `O2` |

For the planned sample of `N = 96`, this gives:

- 48 participants in the high-reliability group and 48 in the low-reliability group.
- 32 participants per block order overall.
- 16 high-reliability and 16 low-reliability participants per order.
- 48 standard-key and 48 flipped-key participants.

The standard key mapping is `D = V-BLACK` and `J = V-WHITE`. The flipped key mapping is `J = V-BLACK` and `D = V-WHITE`.

## Output fields

The main trial and post-block output files include fields that identify the design cell:

| Field | Meaning |
| --- | --- |
| `block` | `CALIBRATION` or `AUTOMATION` |
| `condition_code` | `CAL`, `AB500`, `AS0`, or `AA500` |
| `condition_deadline_code` | Compatibility alias for `condition_code` |
| `automation_reliability_group` | `high`, `low`, or `none` |
| `aid_accuracy_setting` | `0.95`, `0.65`, or blank for calibration |
| `aid_onset_condition` | `before`, `simultaneous`, `after`, or blank for calibration |
| `aid_onset_ms` | Configured aid onset relative to stimulus onset |
| `aid_onset_ms_rel` | Realized aid onset relative to stimulus onset |
| `trial_deadline_ms` | Fixed response window in milliseconds |
| `trial_deadline_s` | Fixed response window in seconds |

Single-block automation runs require an explicit reliability group and aid onset, for example:

```r
run_task(block = "AUTOMATION", aid_onset_ms = -500, reliability_group = "high")
run_task(block = "AUTOMATION", aid_onset_ms = 500, reliability_group = "low")
```

## Author
Russell J. Boag
