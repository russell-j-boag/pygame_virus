# Project Notes

- Use the `r-pygame` conda environment for Pygame checks and task runs. The local interpreter is `/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python`.
- The default `python3` on this machine may not have `pygame` installed, so use the `r-pygame` interpreter for import smoke checks and runtime validation.

# Current Design Snapshot

- The live task is a reliability-drop/recovery design: `CAL -> MAN/PRE_AUTOMATION -> REL_DROP -> MAN/POST_AUTOMATION`.
- Calibration assigns participants to `CAL65` or `CAL85`; the manual comparison trials are split into two 200-trial segments around the aided block.
- Counterbalancing uses a four-participant cycle crossing calibration target with key mapping: `CAL65`/standard, `CAL85`/standard, `CAL65`/flipped, `CAL85`/flipped.
- The aided reliability sequence is `95% -> 70% -> 95%`, with 400 trials per phase and output label `DROP95_70_95`.
- Preserve output metadata for `calibration_target_group`, `calibration_target_accuracy`, `main_block_order`, `manual_segment`, `reliability_phase_label`, and `automation_reliability_group` when changing task outputs or collation.
