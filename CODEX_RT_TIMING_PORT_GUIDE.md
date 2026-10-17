# Codex instructions for porting high precision response timing

Implement the asynchronous response-timing fix from `pygame_virus_dynamic_reliability` in the requested target repository. Preserve its experimental design, response semantics, and historical data. Use source keypress timestamps rather than the time a rendering frame retrieves a keypress.

This handoff covers `pygame_virus`, `pygame_atclab`, `pygame_virus_time_pressure`, and `pygame_virus_aid_onset`. It was checked against their local source on 17 October 2026. Recheck the live checkout before editing; the four tasks have different timing and export contracts.

## Invocation and scope

The user can start a Codex session in any target repository with:

```text
Read /Users/rjb779/Documents/pygame_virus_dynamic_reliability/CODEX_RT_TIMING_PORT_GUIDE.md
and implement its common requirements and the section for this repository.
Complete the applicable tests and report native validation separately.
Preserve unrelated work and historical data. Do not commit or push.
```

Work only in the repository named by the user or established by the current workspace. A request to use this guide in one repository does not authorise edits in the other three. Do not replace an entire task file with the dynamic-reliability version. Do not regenerate participant data, QC reports, analyses, figures, or source ZIPs as part of this port. Commit and push require a separate instruction.

Use the `r-pygame` environment for Python checks and task runs. On the reference Mac its interpreter is:

```text
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python
```

On another computer, resolve that computer's `r-pygame` interpreter rather than copying this absolute path. Read applicable `AGENTS.md` files and inspect `git status --short` before starting. Preserve existing changes, including untracked scripts and outputs.

## Reference implementation

Reference root: `/Users/rjb779/Documents/pygame_virus_dynamic_reliability`.

| Reference file | What to reuse |
| --- | --- |
| `python/response_timing.py` | Native Psychtoolbox queue adapter, timestamped events, held-key handling, response-window selection, audit fields, and cleanup. |
| `python/virus_task.py` | `present_trial`, `run_single_trial`, `main`, and `run_experiment` illustrate acquisition before preparation, first-flip onset, and exception-safe ownership. Adapt this structure to the target task. |
| `python/check_response_timing.py` | Native preflight and interactive diagnostic with 0/8/20/50 ms readout stalls. |
| `tests/test_response_timing.py` | Timestamp invariants, native API shapes, mocked task integration, and real R collation of synthetic exports. Adapt task-specific fixtures and assertions. |
| `0-setup_pygame.R` and `collate_data.R` | Pinned dependency setup and optional audit-column retention. Preserve the target's own setup and run-selection logic. |

At handoff creation the source repository HEAD was `20f2a6644621`, branch `dynamic_reliability`, **but the timing implementation was still uncommitted**. That commit alone does not contain the fix. Use the actual working files. These SHA-256 values identify the reference snapshot:

```text
1121a271421c5c3fd1c9caabf74914ad71c1cc17c53f583f785fa66114edc0c6  python/response_timing.py
0b7cbe044ce4521024f984b3ba8ada06554dba37fadf222770b37e92e484d7a2  python/check_response_timing.py
c378c9fdfd672fa46f8bd55067592e2db8aa94bfb65fd42cb377f719bbf845e2  tests/test_response_timing.py
7230aaca6966129c7bfcf055e16bf3effa4e34e8c17756260b63d2108e104f19  python/virus_task.py
```

If these hashes differ, inspect the newer source and record which version you ported. Keep reusable code local to each target repository; do not create runtime imports from a sibling checkout.

The reference passed 39 Python tests, including its existing QC tests, plus a native macOS queue preflight and a headless Pygame smoke test across its four blocks. The native smoke trials exercised timeouts, not physical keypress accuracy. Windows native validation and interactive keypress validation were not completed by those checks.

## Common timing contract

1. Install and require `psychtoolbox==3.0.22.2`. It has Windows x64 and macOS Intel/Apple Silicon wheels. Retain Pygame for presentation and interface controls. Keep native initialisation lazy so imports, `--help`, and screenshot tools work without starting keyboard capture.
2. Use Psychtoolbox `GetSecs()` and native keyboard-event `Time` in the same clock domain. Never subtract a PTB timestamp from `perf_counter()` or `pygame.time.get_ticks()`. Do not replace source timestamps with event-readout timestamps.
3. Start the native queue before preparing the relevant response display. Capture initial held-key state in that backend. Present the first eligible response frame, flip it, and immediately record onset with `GetSecs()`. Start any response deadline at this onset.
4. Keep input acquisition independent of the drawing loop. Animation can retain its current frame rate; calling `clock.tick()` must not determine the timestamp of a response. Calling `event.get()` more frequently or adding a Python polling thread is not a substitute for native timestamped acquisition.
5. Process eligible events by timestamp, retaining native order for equal timestamps. Ignore pre-onset presses and continue without a penalty, restart, or delay to presentation. A held key becomes eligible only after release; another response key remains available. Disable keyboard auto-repeat and suppress repeated downs without an intervening up.
6. Do not flush the native queue after recording onset. A valid post-onset event may already be queued. Filter on its timestamp instead. Ignore events represented by the initial held-state snapshot, as the reference does.
7. Freeze the first valid main response. Apply deadlines to event timestamps, accepting `onset <= key_time < deadline`. Drain available events before expiring the window, so a pre-deadline key retrieved after expiry still counts. Preserve genuinely unlimited windows as unlimited.
8. Calculate RT in floating-point seconds without rounding, integer-millisecond conversion, flooring, clamping, or absolute values. Preserve responses below 200 ms. Where `rt_ms` is exported, derive it as `1000 * rt_s` without rounding. Keep the target's missing-value conventions for timeouts and unavailable phases.
9. Preserve hard quit and ordinary interface behaviour. Stop acquisition before feedback and other non-response waits; stop/release on normal completion, cancellation, exceptions, and the target's own quit-return convention. Fail visibly on a missing/wrong backend or unusable queue; never silently revert to frame-based timing.
10. Keep original exports and historical RTs intact, including negative or zero values. A runtime fix does not repair already recorded trials or authorise new exclusion rules.

The reference adapter deliberately accepts only D/J and labels responses BLACK/WHITE. ATCLAB must generalise this mapping; aid-onset must instantiate separate windows for its two decisions. These are required adaptations, not optional refactors.

### Audit fields and collation

For a single main response, retain the reference schema:

| Field | Meaning |
| --- | --- |
| `rt_timing_version` | `ptb_queue_flip_v1` for the single-response convention. |
| `rt_backend_version` | Native package version. |
| `stimulus_onset_s` | Timestamp immediately after the first response-eligible stimulus flip. |
| `response_timestamp_s` | Original accepted key-down timestamp; missing on timeout. |
| `n_ignored_pre_onset_keydowns` | Main-response downs after the initial held-state snapshot and before onset. It does not count an entire preceding fixation period. |
| `response_readout_delay_s` | Retrieval time minus original key-down timestamp; never added to RT. Missing on timeout. |

Store full floating-point precision. These are monotonic clock readings with a backend-defined origin, not dates or time since the task started. Preserve audit fields in raw block files, combined participant files, and both full and reduced collated trial CSVs. Treat them as optional when reading historical exports; never fill old rows with the new timing version. Preserve all existing required-column checks and identity/run-selection rules.

For `pygame_virus` and `pygame_atclab`, the inspected collation scripts hardcode `output/` and `data/`. Add optional input/output directory arguments with those same defaults, and create the requested output directory. This enables safe fixture tests in temporary directories. Do not change which participant runs these scripts select as part of this timing port.

## Repository adaptations

### pygame_virus

Inspected branch: `transparency`, HEAD `a458ece4b93a`. Entry point: `python/virus_task.py`.

`run_single_trial` has the same response-before-onset ordering as the original dynamic-reliability task. Port the single-response structure and use the common audit schema.

- Preserve 300 calibration trials targeting 80%, followed by the existing 500-trial MANUAL, AUTOMATION1, and AUTOMATION2 blocks. Preserve the six post-calibration orders and the 12-participant order/key-mapping cycle.
- Preserve the current **10-second** deadline, 95%/65% advice reliability, and transparency settings. Do not import CAL65/CAL90 groups, a five-second deadline, split manual blocks, or the 95–70–95 sequence.
- Preserve `rt_s`, `rt_ms`, `keymap_flip`, `auto_on`, `aid_accuracy_setting`, and `aid_transparency_level`.
- Keep configured `aid_onset_ms` distinct from realised `aid_onset_ms_rel`. Timestamp actual aid presentation after the flip which first displays it, using the same PTB clock as stimulus onset. Simultaneous aid has realised relative onset zero; advanced aid can legitimately have a negative relative onset. Negative aid onset is not a negative RT.
- Preserve advanced, simultaneous, and delayed aid support. For advanced aid, RT starts with the stimulus, not the aid preview. Test all three modes, including a response before delayed advice appears.
- Extend `collate_data.R` so the reduced `data_virus.csv` also retains optional timing fields; the full file currently binds raw columns.

### pygame_virus_time_pressure

Inspected branch: `time_pressure`, HEAD `26427220e3cf`. Entry point: `python/virus_task.py`.

The trial loop also has the original response-before-onset ordering. Apply the single-response timing contract, retaining the target's deadline function and metadata.

- Preserve **HP = 1 second** and **LP = 3 seconds**. There is exactly one calibration block, `CAL_LP`, at the LP deadline. Reuse its single calibrated difficulty in all subsequent conditions; do not reintroduce HP calibration.
- Preserve the four post-calibration conditions `M_HP`, `A_HP`, `M_LP`, and `A_LP`, and the 96-participant cycle crossing all 24 orders, reliability allocation, and key mapping. Use the current allocation functions and `AGENTS.md` as authoritative.
- Preserve `condition_deadline_code`, `time_pressure_condition`, `trial_deadline_s`, `automation_reliability_pattern`, `automation_reliability_group`, and aid-onset/transparency fields. Apply the same signed aid-onset treatment as in the base Virus port.
- `collate_data.R` uses `TRIAL_COLUMNS` and `TRIAL_SUMMARY_COLUMNS` with `all_of()`. Append timing fields through a separate optional-column list or explicitly typed missing columns; simply adding them as required columns would reject historical exports.
- Preserve `R/participant_identity.R`, identity corrections, `source_participant_id`, same-run matching, input manifests, and source checksums. Do not replace this collation code with the simpler reference script.
- Test just-before/exactly-at/just-after deadlines independently at 1 and 3 seconds, including a pre-deadline response retrieved after expiry. Confirm timeout rows have missing RT in new data while historical timeout values remain unchanged.

### pygame_virus_aid_onset

Inspected branch: `aid_onset`, HEAD `3d8a3e6467a7`. Entry point: `python/virus_task.py`.

This implementation starts timing before the first response frame in `collect_key_response`. It can accept a queued press before showing that frame, even though its arithmetic need not produce a negative number. Fix the presentation/input boundary, not just the sign of saved RTs.

- Refactor `collect_key_response` to own a native response window, first-frame presentation, and timestamp filtering. Propagate its timing record through `collect_stimulus_response`, `collect_final_decision_response`, other response wrappers, `make_decision_record`, and `build_trial_row`.
- Create **separate origins and held-key snapshots for Decision 1 and Decision 2**. Decision 1 starts at its first stimulus response frame; Decision 2 starts at the first blank final-decision response frame. Do not measure Decision 2 from the original stimulus, Decision 1 response, or the intervening preview.
- Keep preview/fixation periods non-responsive and keep their scheduled durations. Presses or held keys from Decision 1 or a preview must not become Decision 2 responses. Do not add a deadline to either currently unlimited decision window.
- Preserve the current three scheduled conditions, MANUAL/AIDFIRST/STIMFIRST, with 260 trials each; the 60-trial two-decision practice at a 75% target; 85% advice reliability; and the existing difficulty prior and practice-derived adjustment. Preserve the hidden simultaneous condition without adding it to the public schedule.
- Preserve which information is visible in each preview and decision, the six main-block orders, key counterbalancing, feedback/scoring, and `changed_response`. Preserve separate `decision1_rt_s` and `decision2_rt_s`; do not collapse them into a single response outcome.
- Use `rt_timing_version = ptb_queue_phase_flip_v1` and one `rt_backend_version`. For each prefix `decision1` and `decision2`, export `{prefix}_onset_s`, `{prefix}_response_timestamp_s`, `{prefix}_n_ignored_pre_onset_keydowns`, and `{prefix}_response_readout_delay_s`. The corresponding RT is response timestamp minus that phase's onset. Absent phases remain missing. Cover any retained single-decision helper paths without falsely relabelling their response as Decision 2.
- Extend both `current_trial_all_cols` and `current_trial_summary_cols`. Keep `select_current_cols` compatibility and existing legacy-field normalisation. Preserve timestamp-based run selection, matching trial/questionnaire/slider exports, and `collation_manifest.csv`.
- Preserve the p59 replacement rule: retain run `20260924_110831`; exclude only the earlier run `20260903_120309`. Do not turn this into participant-wide exclusion or change cohort membership during timing work.

### pygame_atclab

Inspected branch: `dev`, HEAD `758904db0c1c`. Entry point: **`python/atclab.py`**, especially `run_trial` and `ALL_RESULT_FIELDS`.

ATCLAB currently timestamps onset before the first aircraft frame and uses one frame's `elapsed` value for keypresses. It needs onset and input-acquisition correction even if RTs stay positive. Preserve aircraft motion, geometry, advice, and scoring while restructuring the response loop.

- Preserve the current 300-trial calibration and three 500-trial main blocks, 80% calibration target, 95%/65% advice reliability, and key/order counterbalancing. Use **`TrialSpec.deadline`**, including externally supplied trial deadlines; the current default is 10 seconds. Do not substitute a global five-second window.
- Generalise the local adapter to accept a logical key-to-response mapping. D/J map to `conflict`/`nonconflict` according to the current counterbalancing, not BLACK/WHITE. When prospective memory is enabled, both `9` and keypad `9` map to `pm` and compete for the first main response. Derive/verify PTB codes for these keys on both platforms; Pygame constants are not PTB codes.
- Extract first-frame drawing without changing aircraft geometry or overlays. Show initial aircraft positions before advancing motion; measure subsequent movement from stimulus onset, excluding fixation or advanced-aid time. Preserve velocity, TTMS/DOMS calculations, route geometry, and the task's current trial termination rule.
- Preserve advanced, simultaneous, and delayed advice. Main RT always starts at the first aircraft frame. Keep configured aid-delay fields distinct from any realised-onset measurement.
- Preserve optional DRT behaviour even though the inspected blocks set `DRT_ON=False`. Extend the native queue to include Space when DRT is enabled. Drain it once per iteration and route main responses and DRT events to separate consumers: Space must not complete the main response window, and a PM response must not be interpreted as a flash response.
- Timestamp each DRT flash after its first visible flip. Evaluate Space timestamps against the correct flash's response interval. The existing interval lasts until the next flash onset, not merely until the flash turns off; preserve that rule and one accepted response per flash. A queued key from before a newly displayed flash must not be assigned to that flash. Preserve flash scheduling distributions, visual durations, and missing-response conventions.
- Preserve `flash_onsets` and `flash_rts` as semicolon-separated fields, but use round-trip float formatting for new values rather than the existing three-decimal formatting. Keep `NA` for unresponded flashes. All flash timestamps/RTs must use the same PTB clock and retain sub-millisecond numerical information.
- Add the common main-response audit fields to `ALL_RESULT_FIELDS` as well as the result dictionary. The fixed-schema normaliser otherwise discards new values. Keep `rt_s` full precision and remove `round(rt * 1000, 2)` from new `rt_ms` exports.
- Preserve `TIMEOUT`, `too_slow`, missing correctness/RT conventions, PM correctness rules, staircase updates, optional questions, and the `{"quit": True}` return path. Queue cleanup must work with this return path, not only with `SystemExit`.
- Update `collate_data.R` for optional timing fields in `data_atc_all.csv` and `data_atc.csv`. Do not remove existing PM, DRT, or geometry fields from the full export.

## Implementation sequence

1. Record the target's initial status, active branch, task entry point, response phases, clocks, deadlines, key mappings, exports, and collation selection rules. Confirm the snapshot-specific facts above against live source.
2. Port the dependency setup, local adapter, and diagnostic. Generalise only where the target requires it. Preserve the source's handling of native `(1, 256)` held-state arrays and empty-array event reads, which were checked against the installed Mac backend.
3. Refactor the target's response collection using its own display callbacks and scoring. Keep tests independent of real participant files. Include a regression reproducing an early queued key at the first frame.
4. Add audit fields through every writer/normaliser and both collated trial outputs. Exercise the actual R collation in a temporary directory with historical and new synthetic exports.
5. Add target-specific integration tests and native diagnostic instructions to its README. Test import/`--help` paths and screenshot utilities that import the task.
6. Run the checks below, inspect the diff, and report software tests, native queue checks, interactive checks, and any unavailable platform separately. Preserve unrelated changes and leave work uncommitted unless instructed otherwise.

## Required validation

### Shared acceptance cases

- A key queued during preparation or the first flip is ignored if timestamped before onset; a valid press immediately after onset is retained. Never repair a negative RT by changing its sign or setting it to zero.
- Test held keys, release/repress, repeats, both mappings, two keys in one batch, stable equal-timestamp ties, and hard quit before/after onset.
- Inject 8, 20, and 50 ms rendering/readout delays. The same source keypress timestamp must produce the same RT. Only readout-delay diagnostics should change.
- Preserve a 0.5 ms difference through Python calculation, raw CSV export, and actual R collation. Compare against independent expected timestamp differences, not a second call to the implementation.
- Test events just before, exactly at, and just after each applicable onset/deadline, including delayed retrieval. Test unlimited windows and slow first-frame preparation separately.
- Exercise normal completion, timeouts, backend failure, wrong package version, rendering exceptions, and hard quit. Confirm native queue stop/release and absence of a silent timing fallback.
- Synthetic mixed-export tests retain historical negative/zero RTs, missing audit fields, existing metadata, participant/run identities, and same-run questionnaire/slider matching.
- Check native key-code mappings for Windows and macOS in automated adapter tests. These tests do not substitute for native execution on each operating system.

### Additional target cases

| Target | Mandatory integration coverage |
| --- | --- |
| Base Virus | All four blocks, both mappings, transparency preserved, advanced/simultaneous/delayed advice, signed realised aid onset, 10-second deadline. |
| Time pressure | CAL_LP plus all four main conditions; 1/3-second boundaries; the complete 96-participant allocation invariant; existing identity/collation regression tests. |
| Aid onset | Practice plus all three scheduled conditions and hidden simultaneous support; independent Decision 1/2 origins; held/queued keys crossing previews; unlimited windows; change-of-mind and p59 selection unchanged. |
| ATCLAB | D/J and PM keys, PM on/off, DRT on/off and combined PM/DRT, flash-window boundaries, first-frame aircraft positions, motion after onset, signed advice delays, custom trial deadlines, all fixed-schema writers, quit-return cleanup. |

Run the new `tests/test_response_timing.py` and existing relevant tests using `r-pygame`. Where available, include the existing participant-QC tests and the time-pressure identity/collation tests. Do not regenerate saved analysis fits merely to validate a runtime change. Run R collation tests against temporary fixtures or a temporary output directory, never canonical study outputs.

For native validation, use the target environment's Python:

```sh
python python/check_response_timing.py --preflight
python python/check_response_timing.py
```

Adapt the interactive diagnostic for ATCLAB's logical outcomes and optional keys, and for aid-onset's two distinct response windows. Record original timestamps and readout delay in a separate validation artifact. A preflight checks APIs and resource lifecycle; an interactive check exercises key input. Report missing physical responses or timeouts as missing validation evidence, not as successful keypress validation. On macOS, follow OS Input Monitoring requirements for the application launching Python; run native checks outside sandboxes that deny system input APIs.

The acceptance claim is **full numerical precision and keyboard timestamps independent of rendering**. Software tests and native queue preflights do not establish physical screen-to-keypress accuracy within ±1 ms. That would require separate hardware measurements and is outside this port.

## Completion report

Report the target repository and changed files, the clock/onset convention, preserved design and schema invariants, tests actually run, and remaining native/interactive platform checks. Confirm that original data and unrelated changes were preserved. Distinguish implementation, validation, commit, and push status explicitly.
