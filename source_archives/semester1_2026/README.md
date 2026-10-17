# Semester 1 2026 pre-fix task source

This directory preserves the base Virus source designated by the user as
Semester 1 2026. The canonical artifact is
[pygame_virus_task_source_semester1_2026.zip](pygame_virus_task_source_semester1_2026.zip).
It retains the historical Pygame response timing, including the
response-before-onset ordering. Preserve this source without applying the
timing fix to it.

No existing base-Virus source ZIP was found in this repository, its Git history,
or the inspected local locations. On 17 October 2026 this archive was therefore
created from the exact Git blobs at the pre-fix revision
`a458ece4b93ac93740297642812d3f121ef40b63`. It is a reconstructed source snapshot,
not an original distributed ZIP. This establishes the archived source contents;
it does not independently verify which version was installed on every
Semester 1 collection computer.

The ZIP is 138,299 bytes and has SHA-256:

```text
803bdf8b90ff0c05cc251dda07367107fd40c49b4f188babc3cd5abeb0fd3e85
```

[manifest.json](manifest.json) records the ZIP checksum, all eight member-file
checksums and Git blob identifiers, the source revision, and the archive
construction convention. Every member matches its pre-fix Git blob exactly,
and the ZIP passed its CRC integrity check. Member timestamps use the source
commit's UTC timestamp at ZIP resolution; they are not original file mtimes.

The bundle contains the R setup, helpers and launcher, the RStudio project,
`python/virus_task.py`, `python/instructions.py`, and both fonts. It contains
no participant data, analysis outputs or updated native timing modules.

No separate extracted copy is required. Optional local extraction under
`extracted/` is ignored by Git because the ZIP is the canonical record. Keep
any extracted source unchanged.

## Future bundles

Build future bundles from the current repository source. The RT timing fix is
present in the working tree and remains uncommitted at archive creation. It
uses `rt_timing_version = ptb_queue_flip_v1`: native keypress timestamps and a
stimulus origin recorded immediately after the first eligible display flip.

Include `python/response_timing.py`, `python/check_response_timing.py`, the
updated `python/virus_task.py` and `0-setup_pygame.R`, plus the normal launcher,
helpers, RStudio project, instructions, fonts and required assets. Include the
[current timing setup and validation instructions](../../README.md#response-timing).
Any distributed setup handout must describe the Psychtoolbox dependency and
native checks. If distributing collation, use the current `collate_data.R`
that retains optional timing audit columns.

Use a distinct release name and source manifest. Record the source revision,
working-tree changes, timing convention, and ZIP/member checksums. Validate
bundled dependencies and imports, and complete native checks on each collection
platform as described in the current README.

Do not rebuild this Semester 1 archive, modify an extracted historical copy,
or use it as the basis for a future bundle. No new distribution bundle is
created by this archival step. Historical participant data and RTs are unchanged.
