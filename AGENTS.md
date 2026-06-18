# Project Notes

- Use the `r-pygame` conda environment for Pygame checks and task runs. The local interpreter is `/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python`.
- The default `python3` on this machine may not have `pygame` installed, so use the `r-pygame` interpreter for import smoke checks and runtime validation.

# Current Task Design

- The current task uses high pressure (`HP`) as a 1.5 s deadline and low pressure (`LP`) as a 3 s deadline.
- Each participant completes exactly one calibration block: `CAL_LP` at the 3 s low-pressure deadline.
- The participant's single calibration delta is reused for all post-calibration manual and automation blocks, regardless of whether the test block is HP or LP. Do not reintroduce dual HP/LP calibration.
- The 16-participant allocation cycle crosses 4 Latin-square post-calibration block orders with reliability pattern and key mapping.
- The allocation cycle must remain human-predictable and unshuffled: for each order `O1`-`O4`, assign `HP95_LP65` standard/flipped, then `HP65_LP95` standard/flipped. Participant 1 is `O1 x HP95_LP65 x standard`.
- Within each 16-participant cycle, there are 4 participants per Latin-square order, 8 per reliability pattern, 8 per key mapping, and 1 per full `order x reliability pattern x key mapping` cell.
- For the planned sample of 96 participants, the 16-participant cycle repeats six times, giving 6 participants per full allocation cell.
- Single-block `CALIBRATION` runs must use the 3 s LP calibration deadline. The task code rejects the 1.5 s HP deadline because `CAL_HP` is not an active block.
- Use the allocation functions in `python/virus_task.py` as the source of truth for counterbalancing tables and smoke checks.
