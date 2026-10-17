# Semester 2 2026 collection source

This directory preserves the existing source bundle designated by the user as
the Semester 2 2026 source. It retains the historical response-timing
implementation: the response clock starts before the first response display,
and RT is measured when Pygame retrieves the key event. Preserve this record
without applying the timing fix to it.

The canonical artifact is
[pygame_virus_aid_onset_task_source_semester2_2026.zip](pygame_virus_aid_onset_task_source_semester2_2026.zip).
On 17 October 2026 it was moved from the repository root's
`pygame_virus_aid_onset_task_source.zip` without recompression or changes to its
bytes. The ZIP is 156,031 bytes and has SHA-256:

```text
cfff80c8e02f858257fbeb16c8b56b88291c1530d807ed60b37d039a61c8909f
```

[manifest.json](manifest.json) records the ZIP checksum, all 11 member-file
checksums, and the source revisions. The ZIP passed its CRC integrity check.
Every member matches the source at pre-fix HEAD `3d8a3e6467a7` and at
`ed0d0a8772e9`, the commit that last changed the ZIP on 28 August 2026. This
comparison establishes the archived source contents; it does not independently
verify which files were installed on every collection computer.

No separate extracted bundle existed when this archive was created. A fresh
checkout can recover the source by extracting the ZIP. Optional local extraction
under `extracted/` is ignored by Git because the ZIP is the canonical record.
Keep any extracted copy unchanged.

## Future bundles

Build future bundles from current repository source. The RT timing fix is
present in the working tree and remains uncommitted at archive creation. It
uses `rt_timing_version = ptb_queue_phase_flip_v1`, with independent native
keypress timestamps and post-flip origins for Decision 1 and Decision 2.

Future bundles must include `python/response_timing.py`,
`python/check_response_timing.py`, the updated `python/virus_task.py` and
`0-setup_pygame.R`, plus the normal launcher, helpers, RStudio project,
instructions, fonts and required assets. Include the
[current timing setup and validation instructions](../../README.md#response-timing).
Any Quick Start handout distributed with a future bundle must also reflect the
Psychtoolbox dependency and native validation steps; an older handout alone is
not sufficient setup guidance.

Use a distinct release name and source manifest. Record the source revision,
any working-tree changes, timing convention, and ZIP/member checksums. Validate
the bundled dependency and imports before distribution, and complete native
checks on each collection platform as described in the current README.

Do not rebuild this archive, modify its extracted copy, or use the historical
source as the basis for a future bundle. No new distribution bundle is created
by this archival step. Historical participant data and RT values are unchanged.
