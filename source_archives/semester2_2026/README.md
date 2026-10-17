# Semester 2 2026 collection source

This directory preserves the existing source bundle identified by the user as
the Semester 2 2026 collection record. It contains the historical response-timing
implementation, including the response-before-onset ordering investigated after
collection. Preserve this record without applying the timing fix to it.

The canonical artifact is
[pygame_virus_time_pressure_task_source_semester2_2026.zip](pygame_virus_time_pressure_task_source_semester2_2026.zip).
On 17 October 2026 it was moved from the repository root's
`pygame_virus_time_pressure_task_source.zip` without recompression or changes to
its bytes. The ZIP is 154,551 bytes and has SHA-256:

```text
5e07f5868f96620e02281deb23c8698a035cef1e48124d4cdede0d573602efe7
```

[manifest.json](manifest.json) records the ZIP checksum, all 11 member-file
checksums and stored timestamps, and the source revisions. The ZIP passed its
CRC integrity check. Every member matches the corresponding source at commit
`26427220e3cf`, immediately before the timing fix, and the last committed ZIP's
member contents.

The ZIP already had an uncommitted change when the timing work began. Its member
timestamps differ from the committed ZIP, while the member contents are identical.
The working ZIP is preserved exactly. The last committed ZIP was changed in
`a47bb64059e4` on 4 August 2026 and remains recoverable from Git history; its
SHA-256 is also recorded in the manifest. This archive does not replace either
version with a newly generated bundle.

An inspection copy was extracted to `extracted/` and verified against every ZIP
member. It is ignored by Git because the ZIP is the canonical record. A fresh
checkout can recover the source files by extracting the ZIP. Do not edit the
historical extracted copy.

## Future bundles

Use current source from the repository root. The updated timing implementation
first appears in commit `670743264ca1` and uses `rt_timing_version =
ptb_queue_flip_v1`. Future bundles must include `python/response_timing.py`,
`python/check_response_timing.py`, the updated task and dependency setup, and
the normal launcher, helpers, instructions, fonts, project file, and required
assets. Include the [current timing setup and validation instructions](../../README.md#response-timing).

Use a distinct release name and record the source revision, any working-tree
changes, timing convention, and checksums. Validate the bundled dependency and
imports before distribution. Do not rebuild this archive, modify its extracted
copy, or reuse the historical source as the basis for a future bundle. The
archive designation does not modify historical participant data or RT values.
