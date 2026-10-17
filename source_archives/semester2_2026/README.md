# Semester 2 2026 collection source

This directory preserves the existing source bundle identified by the user as
the version used for Semester 2 2026. It contains the historical response-timing
implementation, including the response-before-onset ordering investigated after
collection. Preserve this record without applying the timing fix to it.

The canonical artifact is
[pygame_virus_dynamic_reliability_source_semester2_2026.zip](pygame_virus_dynamic_reliability_source_semester2_2026.zip).
On 17 October 2026 it was moved from the repository root's
`pygame_virus_dynamic_reliability_source.zip` without recompression or changes to
its bytes. The ZIP is 153,616 bytes and has SHA-256:

```text
c854b350d419fd79d5d45c36ba9bb4294906309b79f55ae26dd22143e7213bee
```

[manifest.json](manifest.json) records the ZIP checksum, all 10 member-file
checksums, and the source revisions. The ZIP passed its CRC integrity check.
Every member matches both the existing extracted copy and the corresponding
source at commit `20f2a6644621`, immediately before the timing fix. The ZIP itself
was last changed in commit `ce61f9c2f600` on 14 July 2026.

The existing extracted folder was moved to `extracted/` without changing any
files. It is ignored by Git because the ZIP is the canonical record. A fresh
checkout can recover those source files by extracting the ZIP; the original
local folder also contains a macOS `.DS_Store` file that is not part of the ZIP.

## Future bundles

Use current source from the repository root. The updated timing implementation
first appears in commit `0a9ea8b4f771` and uses `rt_timing_version =
ptb_queue_flip_v1`. Future bundles must include `python/response_timing.py`,
`python/check_response_timing.py`, the updated task and dependency setup, and
the normal launcher, helpers, instructions, fonts, and required assets. Include
the [current timing setup and validation instructions](../../README.md#response-timing-for-new-task-runs).

Use a distinct release name and record the source revision, any working-tree
changes, timing convention, and checksums. Validate the bundled dependency and
imports before distribution. Do not rebuild this archive, modify its extracted
copy, or reuse the historical source as the basis for a future bundle. The
archive designation does not modify historical participant data or RT values.
