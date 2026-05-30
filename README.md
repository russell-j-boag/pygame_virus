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

The task uses a two-level time-pressure design. High pressure (`HP`) uses a 2 s response deadline and low pressure (`LP`) uses a 4 s response deadline.

Each participant completes one calibration block only. Half of participants are calibrated to 80% accuracy under the HP deadline and half are calibrated to 80% accuracy under the LP deadline. The resulting participant-specific stimulus difficulty is then reused for every post-calibration manual and automation block, regardless of that block's pressure deadline.

| Code | Mode | Deadline | Trials |
| --- | --- | ---: | ---: |
| `CAL_HP` | Calibration, assigned to half of participants | 2 s | 300 |
| `CAL_LP` | Calibration, assigned to half of participants | 4 s | 300 |
| `M_HP` | Manual | 2 s | 400 |
| `A_HP` | Automation | 2 s | 400 |
| `M_LP` | Manual | 4 s | 400 |
| `A_LP` | Automation | 4 s | 400 |

Automation reliability is a between-subjects, pressure-contingent factor:

| Reliability pattern | HP aid accuracy | LP aid accuracy |
| --- | ---: | ---: |
| `HP95_LP65` | 95% | 65% |
| `HP65_LP95` | 65% | 95% |

Participant-facing automation instructions are qualitative rather than numeric. Before each automation block, the high-reliability aid is described as highly reliable but not perfect; the low-reliability aid is described as reasonably reliable with relatively common possible errors.

## Counterbalancing

Counterbalancing is assigned deterministically from participant ID in a 32-participant cycle. Calibration is always first. The four post-calibration blocks are ordered using a balanced Latin square for the crossed manual/automation x HP/LP cells:

| Factor | Levels |
| --- | --- |
| Calibration deadline | `CAL_HP`, `CAL_LP` |
| Post-calibration block order | 4 Latin-square orders of `M_HP`, `A_HP`, `M_LP`, `A_LP` |
| Reliability pattern | `HP95_LP65`, `HP65_LP95` |
| Key mapping | standard, flipped |

The Latin-square orders are:

| Order | Sequence |
| --- | --- |
| `O1` | `M_HP -> A_HP -> A_LP -> M_LP` |
| `O2` | `A_HP -> M_LP -> M_HP -> A_LP` |
| `O3` | `M_LP -> A_LP -> A_HP -> M_HP` |
| `O4` | `A_LP -> M_HP -> M_LP -> A_HP` |

These four orders are crossed with calibration deadline, reliability pattern, and key mapping, giving `4 x 2 x 2 x 2 = 32` allocation cells per cycle. For example, a participant assigned to `CAL_LP` with order `O4` receives:

```text
CAL_LP -> A_LP -> M_HP -> M_LP -> A_HP
```

For the planned sample of `N = 96`, the 32-participant cycle repeats three times. This gives 24 participants per Latin-square order, 48 participants per calibration deadline, 48 per reliability pattern, 48 per key mapping, and 3 participants per full `order x calibration x reliability pattern x key mapping` cell.

The standard key mapping is `D = V-BLACK` and `J = V-WHITE`. The flipped key mapping is `J = V-BLACK` and `D = V-WHITE`.

The participant-specific allocation table is generated from the allocation functions in `python/virus_task.py`.

## Output fields

The main trial and post-block output files include fields that identify the design cell:

| Field | Meaning |
| --- | --- |
| `block` | `CALIBRATION`, `MANUAL`, or `AUTOMATION` |
| `condition_deadline_code` | `CAL_HP`, `M_HP`, `A_HP`, `CAL_LP`, `M_LP`, or `A_LP` |
| `time_pressure_condition` | `HP` or `LP` |
| `calibration_time_pressure_condition` | Participant's assigned calibration pressure, `HP` or `LP` |
| `calibration_condition_deadline_code` | Participant's assigned calibration code, `CAL_HP` or `CAL_LP` |
| `calibration_trial_deadline_ms` | Assigned calibration deadline in milliseconds |
| `calibration_trial_deadline_s` | Assigned calibration deadline in seconds |
| `automation_reliability_pattern` | `HP95_LP65`, `HP65_LP95`, `single_block`, or `none` |
| `automation_reliability_group` | `high`, `low`, or `none` |
| `aid_accuracy_setting` | `0.95`, `0.65`, or blank for manual/calibration |
| `trial_deadline_ms` | Response deadline in milliseconds |
| `trial_deadline_s` | Response deadline in seconds |

Single-block calibration runs must use the participant's assigned calibration deadline. The task will stop with an error if the entered participant ID is assigned to the other calibration deadline. Single-block automation runs require an explicit reliability group, for example:

```r
run_task(block = "CALIBRATION", deadline_s = 2) # only for CAL_HP-assigned participants
run_task(block = "CALIBRATION", deadline_s = 4) # only for CAL_LP-assigned participants
run_task(block = "MANUAL", deadline_s = 2)
run_task(block = "AUTOMATION", deadline_s = 2, reliability_group = "high")
run_task(block = "AUTOMATION", deadline_s = 4, reliability_group = "low")
```

## Author
Russell J. Boag
