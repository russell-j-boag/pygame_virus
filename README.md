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

The task uses a mixed design. Calibration is unchanged and always occurs first. After calibration, participants complete four post-calibration blocks crossing task mode with response deadline:

| Code | Mode | Deadline | Trials |
| --- | --- | ---: | ---: |
| `M3` | Manual | 3 s | 400 |
| `M6` | Manual | 6 s | 400 |
| `A3` | Automation | 3 s | 400 |
| `A6` | Automation | 6 s | 400 |

Automation reliability is a between-subjects factor:

| Reliability group | Aid accuracy |
| --- | ---: |
| `high` | 95% |
| `low` | 65% |

Participant-facing automation instructions are qualitative rather than numeric. The high-reliability group is told that the aid is highly reliable but not perfect. The low-reliability group is told that the aid is reasonably reliable and that errors may be relatively common.

## Counterbalancing

The post-calibration block order uses a 4-cell Williams Latin square. The four order sequences are:

| Order | Sequence |
| --- | --- |
| `O1` | `M3 -> M6 -> A6 -> A3` |
| `O2` | `M6 -> A3 -> M3 -> A6` |
| `O3` | `A3 -> A6 -> M6 -> M3` |
| `O4` | `A6 -> M3 -> A3 -> M6` |

Reliability group, block order, and key mapping are assigned deterministically from participant ID in a 16-participant cycle:

| Participant IDs in cycle | Reliability | Key mapping | Order |
| --- | --- | --- | --- |
| 1 | `high` | standard | `O1` |
| 2 | `low` | standard | `O1` |
| 3 | `high` | standard | `O2` |
| 4 | `low` | standard | `O2` |
| 5 | `high` | standard | `O3` |
| 6 | `low` | standard | `O3` |
| 7 | `high` | standard | `O4` |
| 8 | `low` | standard | `O4` |
| 9 | `high` | flipped | `O1` |
| 10 | `low` | flipped | `O1` |
| 11 | `high` | flipped | `O2` |
| 12 | `low` | flipped | `O2` |
| 13 | `high` | flipped | `O3` |
| 14 | `low` | flipped | `O3` |
| 15 | `high` | flipped | `O4` |
| 16 | `low` | flipped | `O4` |

The cycle repeats every 16 participants. For the planned sample of `N = 96`, this gives:

- 48 participants in the high-reliability group and 48 in the low-reliability group.
- 24 participants per block order overall.
- 12 high-reliability and 12 low-reliability participants per order.
- 48 standard-key and 48 flipped-key participants.
- 6 participants per full `reliability x order x keymap` cell.

The standard key mapping is `D = V-BLACK` and `J = V-WHITE`. The flipped key mapping is `J = V-BLACK` and `D = V-WHITE`.

## Output fields

The main trial and post-block output files include fields that identify the design cell:

| Field | Meaning |
| --- | --- |
| `block` | `CALIBRATION`, `MANUAL`, or `AUTOMATION` |
| `condition_deadline_code` | `CAL`, `M3`, `M6`, `A3`, or `A6` |
| `automation_reliability_group` | `high`, `low`, or `none` |
| `aid_accuracy_setting` | `0.95`, `0.65`, or blank for manual/calibration |
| `trial_deadline_ms` | Response deadline in milliseconds |
| `trial_deadline_s` | Response deadline in seconds |

Single-block automation runs require an explicit reliability group, for example:

```r
run_task(block = "AUTOMATION", deadline_s = 3, reliability_group = "high")
run_task(block = "AUTOMATION", deadline_s = 6, reliability_group = "low")
```

## Author
Russell J. Boag
