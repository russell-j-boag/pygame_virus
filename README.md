# pygame_virus

Experimental task built in Pygame for studying decision-making and automation use. 
Based on Bartlett & McCarley RDC task.

## Structure
- Python experiment code in `/python`
- Output data saved to `/output`
- Plots saved to `/plots`
- Analysis conducted in R

## Getting started
- Run `0-setup_pygame.R` to install Python, Pygame, and `psychtoolbox==3.0.22.2` in `r-pygame`
- Use `1-run_virus_task.R` to launch task and pre-task instructions using the helper functions

## Semester 1 2026 source archive

The pre-fix task source is preserved in
[source_archives/semester1_2026](source_archives/semester1_2026/README.md).
No existing base-Virus source ZIP was found locally or in Git history, so this
archive was reconstructed from the exact source at pre-fix commit `a458ece4b93a`.
Its manifest records the provenance and ZIP/member checksums. It retains the
historical response-timing implementation; it is not an original distributed
ZIP or independent evidence of the installation on every collection computer.

Future bundles must be built from current repository source and use
`rt_timing_version = ptb_queue_flip_v1`. Include `python/response_timing.py`,
`python/check_response_timing.py`, the updated task and dependency setup, and
the normal launcher, helpers, project file, instructions and fonts. Include the
current timing setup and validation guidance below. Use a distinct release name
and source manifest; preserve the Semester 1 archive unchanged.

## Response timing

New trials use the native Psychtoolbox keyboard queue on macOS and Windows.
RT is the original key-down timestamp minus `GetSecs()` immediately after the
first stimulus flip, in floating-point seconds. Rendering and the 120 FPS frame
cap do not supply response timestamps. The first eligible D/J press is final;
pre-onset presses and repeated downs are ignored, and held keys require release.
The 10-second window accepts `onset <= key_time < onset + 10`, including keys
retrieved after the deadline. New timeouts have missing RT. Backend failure is
reported explicitly; there is no frame-timing fallback.

The 300-trial calibration at 80%, three 500-trial main blocks, 95%/65% advice,
transparency options, six block orders and 12-participant key/order cycle are
unchanged. Advanced advice retains a negative realised `aid_onset_ms_rel`,
simultaneous advice has zero, and delayed advice is timed after its first visible
flip. If the trial ends before advice is displayed, realised aid onset is missing.
Configured `aid_onset_ms` remains separate. RT always starts with the stimulus.

Raw block and combined files retain `rt_s` and unrounded `rt_ms = 1000 * rt_s`,
plus these audit fields:

- `rt_timing_version`: `ptb_queue_flip_v1`
- `rt_backend_version`: native package version
- `stimulus_onset_s`: timestamp immediately after the first stimulus flip
- `response_timestamp_s`: accepted native key-down timestamp, missing on timeout
- `n_ignored_pre_onset_keydowns`: response downs between the initial held-state snapshot and onset
- `response_readout_delay_s`: retrieval timestamp minus key timestamp, missing on timeout

The timestamps are monotonic clock readings, not dates. Readout delay is not
added to RT. Both collated trial outputs retain optional audit fields; old rows
are not assigned the new convention and historical RTs are not corrected.
Collation still selects the latest file by modification time for each participant
and file family. Optional directories allow validation without study outputs:

```sh
Rscript collate_data.R /path/to/fixture-input /path/to/temporary-output
```

Omitting the arguments retains the `output/` and `data/` defaults. Collation uses
`dplyr`, `readr`, `stringr`, `purrr`, and `tibble`.

## Timing validation

Use the `r-pygame` interpreter. On the reference Mac:

```sh
PYTHON=/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python
"$PYTHON" -B -m unittest discover -s tests -v
"$PYTHON" -B python/check_response_timing.py --preflight
"$PYTHON" -B python/check_response_timing.py
```

On other collection computers, use their own `r-pygame` Python path. Run the
native checks outside sandboxes that deny system input APIs. On macOS, grant
Input Monitoring to the application launching Python (Terminal or R/RStudio)
and restart it if required.

The interactive diagnostic asks for D/J presses with 0/8/20/50 ms readout stalls,
and saves timestamps and readout delays separately under
`analysis_outputs/response_timing/`. Use `--output /path/to/new-report.json` to
choose another location; existing reports are never overwritten. Release each
key between presses. Escape cancels the diagnostic; the experiment retains its
Option/Alt+Q hard quit.

The software tests cover onset/deadline boundaries, held keys and repeats,
preparation delays, first-response selection, both mappings, all blocks and
transparency settings, signed advice timing, cleanup, and actual R collation of
synthetic historical/new exports. They do not read or regenerate participant
data. The macOS native preflight and a headless rendering smoke check are
recorded in `validation/response_timing/`. Physical-keypress interactive checks
and native Windows checks remain to be completed on collection machines.
Preflight and simulated tests do not establish physical screen-to-keypress
accuracy within ±1 ms; that requires hardware measurement.

## Author
Russell J. Boag
