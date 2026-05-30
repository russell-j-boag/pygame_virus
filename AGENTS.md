# Project Notes

- Use the `r-pygame` conda environment for Pygame checks and task runs. The local interpreter is `/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python`.
- The default `python3` on this machine may not have `pygame` installed, so use the `r-pygame` interpreter for import smoke checks and runtime validation.

# Current Task Design

- The current task uses high pressure (`HP`) as a 2 s deadline and low pressure (`LP`) as a 4 s deadline.
- Each participant completes exactly one calibration block: `CAL_HP` for half of participants and `CAL_LP` for half of participants.
- The participant's single calibration delta is reused for all post-calibration manual and automation blocks, regardless of whether the test block is HP or LP. Do not reintroduce dual HP/LP calibration.
- The 32-participant allocation cycle crosses 4 Latin-square post-calibration block orders with calibration deadline, reliability pattern, and key mapping.
- For the planned sample of 96 participants, the 32-participant cycle repeats three times, giving 3 participants per full allocation cell.
- Single-block `CALIBRATION` runs must use the participant's assigned calibration deadline. The task code rejects the other deadline for that participant.
- Use the allocation functions in `python/virus_task.py` as the source of truth for counterbalancing tables and smoke checks.
