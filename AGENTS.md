# Project Notes

- Use the `r-pygame` conda environment for Pygame checks and task runs. The local interpreter is `/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python`.
- The default `python3` on this machine may not have `pygame` installed, so use the `r-pygame` interpreter for import smoke checks and runtime validation.

# Current Task Design

- The current task uses high pressure (`HP`) as a 1 s deadline and low pressure (`LP`) as a 3 s deadline.
- Each participant completes exactly one calibration block: `CAL_LP` at the 3 s low-pressure deadline.
- The participant's single calibration delta is reused for all post-calibration manual and automation blocks, regardless of whether the test block is HP or LP. Do not reintroduce dual HP/LP calibration.
- The 96-participant allocation cycle crosses all 24 post-calibration order permutations with reliability pattern and key mapping.
- The allocation cycle must remain human-predictable and unshuffled: for each order `O01`-`O24`, assign `HP95_LP65` standard/flipped, then `HP65_LP95` standard/flipped. Participant 1 is `O01 x HP95_LP65 x standard`.
- Within each 96-participant cycle, there are 4 participants per post-calibration order, 48 per reliability pattern, 48 per key mapping, and 1 per full `order x reliability pattern x key mapping` cell.
- For the planned sample of 96 participants, the full factorial allocation cycle is completed exactly once.
- Single-block `CALIBRATION` runs must use the 3 s LP calibration deadline. The task code rejects the 1 s HP deadline because `CAL_HP` is not an active block.
- Use the allocation functions in `python/virus_task.py` as the source of truth for counterbalancing tables and smoke checks.

# Source Bundle Provenance

- The frozen source bundle used for Semester 2 2026 is in `source_archives/semester2_2026/`. Read its `README.md` and `manifest.json` before working on source bundles. Preserve the ZIP byte for byte; never rebuild or overwrite this historical record with current code.
- The local `source_archives/semester2_2026/extracted/` folder is an ignored copy of the historical bundle, not a source for future releases. Preserve it as well.
- Build future bundles from the current repository source, including the updated `python/virus_task.py`, `python/response_timing.py`, `python/check_response_timing.py`, and `0-setup_pygame.R`, alongside the normal launcher, helpers, instructions, fonts, project file, and other required assets. Include the response-timing setup and validation instructions from `README.md`.
- Future bundle names and manifests must distinguish them from the Semester 2 2026 record and identify their source revision, any uncommitted changes, and timing convention (`ptb_queue_flip_v1` for the current implementation). Check the bundled files against the intended source and validate the timing dependency/imports before distribution.
