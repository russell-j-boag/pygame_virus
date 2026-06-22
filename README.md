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

By default, the screenshot scripts use the current display size so the PNGs match the participant-facing display aspect ratio. If the shell cannot access the display, pass the display size explicitly, for example `--resolution 1440x900`, to both screenshot commands.
The deck builder sizes the PowerPoint slides to the screenshot aspect ratio rather than PowerPoint's default slide shape.
The generated `instruction_screenshots/`, `virus_task_screenshots/`, and `screenshots_review.pptx` artifacts are local review outputs and are ignored by git.
The deck builder requires `pandoc` and Pillow.

## Current task design

The task uses a two-level time-pressure design. High pressure (`HP`) uses a 1.5 s response deadline and low pressure (`LP`) uses a 3 s response deadline.

Each participant completes one calibration block only. All participants are calibrated to 80% accuracy under the LP deadline. The resulting participant-specific stimulus difficulty is then reused for every post-calibration manual and automation block, regardless of that block's pressure deadline.

| Code | Mode | Deadline | Trials |
| --- | --- | ---: | ---: |
| `CAL_LP` | Calibration | 3 s | 300 |
| `M_HP` | Manual | 1.5 s | 400 |
| `A_HP` | Automation | 1.5 s | 400 |
| `M_LP` | Manual | 3 s | 400 |
| `A_LP` | Automation | 3 s | 400 |

Automation reliability is a between-subjects, pressure-contingent factor:

| Reliability pattern | HP aid accuracy | LP aid accuracy |
| --- | ---: | ---: |
| `HP95_LP65` | 95% | 65% |
| `HP65_LP95` | 65% | 95% |

Participant-facing automation instructions are qualitative rather than numeric. Before each automation block, the high-reliability aid is described as highly reliable but not perfect; the low-reliability aid is described as reasonably reliable with relatively common possible errors.

## Counterbalancing

Counterbalancing is assigned deterministically from participant ID in a 16-participant cycle. Calibration is always first and always uses `CAL_LP`. The four post-calibration blocks are ordered using a balanced Latin square for the crossed manual/automation x HP/LP cells:

| Factor | Levels |
| --- | --- |
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

These four orders are crossed with reliability pattern and key mapping, giving `4 x 2 x 2 = 16` allocation cells per cycle. The cycle is intentionally not shuffled: participant numbers advance through block order first, then reliability pattern, then key mapping. For example, a participant assigned to order `O4` receives:

```text
CAL_LP -> A_LP -> M_HP -> M_LP -> A_HP
```

Within each 16-participant cycle, there are 4 participants per Latin-square order, 8 per reliability pattern, 8 per key mapping, and 1 per full `order x reliability pattern x key mapping` cell.

The 16-participant cycle is:

| Participant | Order | Reliability pattern | Key mapping |
| ---: | --- | --- | --- |
| `1` | `O1` | `HP95_LP65` | standard |
| `2` | `O1` | `HP95_LP65` | flipped |
| `3` | `O1` | `HP65_LP95` | standard |
| `4` | `O1` | `HP65_LP95` | flipped |
| `5` | `O2` | `HP95_LP65` | standard |
| `6` | `O2` | `HP95_LP65` | flipped |
| `7` | `O2` | `HP65_LP95` | standard |
| `8` | `O2` | `HP65_LP95` | flipped |
| `9` | `O3` | `HP95_LP65` | standard |
| `10` | `O3` | `HP95_LP65` | flipped |
| `11` | `O3` | `HP65_LP95` | standard |
| `12` | `O3` | `HP65_LP95` | flipped |
| `13` | `O4` | `HP95_LP65` | standard |
| `14` | `O4` | `HP95_LP65` | flipped |
| `15` | `O4` | `HP65_LP95` | standard |
| `16` | `O4` | `HP65_LP95` | flipped |

For the planned sample of `N = 96`, the 16-participant cycle repeats six times. This gives 24 participants per Latin-square order, 48 per reliability pattern, 48 per key mapping, and 6 participants per full `order x reliability pattern x key mapping` cell.

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

Single-block calibration runs must use the 3 s LP calibration deadline. The task will stop with an error if a 1.5 s HP calibration deadline is requested. Single-block automation runs require an explicit reliability group, for example:

```r
run_task(block = "CALIBRATION", deadline_s = 3)
run_task(block = "MANUAL", deadline_s = 1.5)
run_task(block = "AUTOMATION", deadline_s = 1.5, reliability_group = "high")
run_task(block = "AUTOMATION", deadline_s = 3, reliability_group = "low")
```

## Author
Russell J. Boag
