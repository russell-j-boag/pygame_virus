# Project Notes

- Use the `r-pygame` conda environment for Pygame checks and task runs. The local interpreter is `/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python`.
- The default `python3` on this machine may not have `pygame` installed, so use the `r-pygame` interpreter for import smoke checks and runtime validation.

# Current Design Snapshot

- The live task is a reliability-drop/recovery design: `CAL -> MAN/PRE_AUTOMATION -> REL_DROP -> MAN/POST_AUTOMATION`.
- Calibration assigns participants to `CAL65` or `CAL90`; the manual comparison trials are split into two 200-trial segments around the aided block.
- Counterbalancing uses a four-participant cycle crossing calibration target with key mapping: `CAL65`/standard, `CAL90`/standard, `CAL65`/flipped, `CAL90`/flipped.
- The aided reliability sequence is `95% -> 70% -> 95%`, with 400 trials per phase and output label `DROP95_70_95`.
- Do not interrupt the 1200-trial aided block with self-rated accuracy or aid-trust questions; show automation post-block ratings only after the full aided block.
- Preserve output metadata for `calibration_target_group`, `calibration_target_accuracy`, `main_block_order`, `manual_segment`, `reliability_phase_label`, and `automation_reliability_group` when changing task outputs or collation.

# Source Bundle Provenance

- The frozen source bundle used for Semester 2 2026 is in `source_archives/semester2_2026/`. Read its `README.md` and `manifest.json` before working on source bundles. Preserve the ZIP byte for byte; never rebuild or overwrite this historical record with current code.
- The local `source_archives/semester2_2026/extracted/` folder is an ignored copy of the historical bundle, not a source for future releases. Preserve it as well.
- Build future bundles from the current repository source, including the updated `python/virus_task.py`, `python/response_timing.py`, `python/check_response_timing.py`, and `0-setup_pygame.R`, alongside the normal launcher, helpers, instructions, fonts, and other required assets. Include the response-timing setup and validation instructions from `README.md`.
- Future bundle names and manifests must distinguish them from the Semester 2 2026 record and identify their source revision, any uncommitted changes, and timing convention (`ptb_queue_flip_v1` for the current implementation). Check the bundled files against the intended source and validate the timing dependency/imports before distribution.
