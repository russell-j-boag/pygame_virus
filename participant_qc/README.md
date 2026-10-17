# Semester 1 2026 participant audit

This is an **audit-only**, retrospective adaptation of the participant QC reports
in `pygame_virus_time_pressure` and `pygame_virus_dynamic_reliability`. It retains
every source row and participant. It does not apply exclusions, collate new data,
fit models, or change existing analyses or the historical source ZIP.

Run from the repository root with the project's environment:

```sh
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python -m participant_qc.run_qc
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python tests/validate_participant_qc.py --expect-current-cohort --receipt analysis_outputs/semester1_2026_qc_validation/validation.json
/Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python -m unittest discover -s tests -p 'test_*.py'
```

The generator also supports direct execution (`python participant_qc/run_qc.py`).
Its options are `--input`, `--config`, `--raw-dir`, and `--output-dir`. Defaults are
`data/data_virus_all.csv`, `participant_qc/config.json`, `output/`, and
`analysis_outputs/semester1_2026_qc/`. Open that output directory's `index.html`.
The independent validator accepts `--output-dir`, an optional `--receipt` outside
the report, and an optional `--preservation-snapshot` mapping paths to SHA-256
hashes. Omit `--expect-current-cohort` when auditing another cohort or synthetic
data; only that option asserts this particular 48-person snapshot.

## Study contract and rules

The historical contract in `study.py` is independent of native keyboard capture.
Tests compare it with the current task: calibration has 300 trials, followed by
all six permutations of MANUAL, AUTOMATION1 (95% advice), and AUTOMATION2 (65%
advice), each with 500 trials. Key mapping flips in six-participant chunks. The
deadline is 10 seconds throughout. A future change to the study design requires
an explicit contract update, rather than silently changing this historical audit.
Historical transparency and native timing fields are optional.

| Screen | Scope | Default threshold | Role |
| --- | --- | --- | --- |
| Calibration target | Trials 151–300 | Absolute deviation from 80% greater than 10 percentage points | Candidate |
| Manual target | Whole MANUAL block | Absolute deviation from 80% greater than 10 percentage points | Diagnostic only |
| Near-chance or worse | Each whole scope | All-trial accuracy at most 55% | Candidate |
| Anticipatory responding | Each whole scope | RT below 200 ms on at least 20% of all trials, at least 20 fast responses, and fast-only accuracy at most 55%, jointly within that scope | Candidate |

Whole scopes are CALIBRATION, MANUAL, AUTOMATION1, AUTOMATION2, pooled aided
trials (1,000), and all experimental trials (1,500). Pooled aided trials need not
be contiguous. Calibration-tail metrics support only the target screen.
Disjoint 50-trial bins support plots only; they cannot trigger exclusions.
All settings in `config.json` are recorded in the output configuration snapshot.

Timeouts count as incorrect and never count as fast, even when they carry a
numeric RT. Finite zero/negative answered RTs are retained and count as fast;
exactly 200 ms is not fast. Fast fractions use all scope trials. Empty fast
subsets have missing accuracy. Answered accuracy, timeout counts, nonpositive
RTs, timeouts with recorded RTs, and answered RTs beyond the deadline are separate
diagnostics without additional exclusion rules. Missing or invalid scopes are
`not_assessable`, not clear. Rule comparisons use a 1e-12 tolerance for floating
point proportions; the strict RT cutoff uses no tolerance.

The candidate total is the union across the three candidate criteria and all
scopes; manual target flags never contribute. These descriptive screens are not
preregistered decisions or evidence of equivalence to chance. Wilson 95%
binomial intervals describe uncertainty without accounting for serial dependence
or adaptive calibration and do not determine flags.

## Sources, figures and outputs

Canonical participant/run identities are authoritative. The generator reconciles
all raw columns with the exact `results_p*_TIMESTAMP_b00_ALL.csv` selected in the
input, normalising missing tokens, boolean case and numeric formatting (1e-12
tolerance). Missing sources, mismatches, duplicate trial identities, ambiguous
selected runs and malformed schemas stop generation. Alternative ALL files are
hashed and listed without reselecting runs. In this snapshot, participant 35's
`20260422_133042` run is selected; `20260422_112735` remains an alternative.

The report includes searchable participant summaries, scope metrics, all checks,
flagged checks, integrity findings, candidate and overlap summaries, bins,
configuration, source-run ledger, plot sources, peer pools, comparator selection,
and a manifest with source/input/output hashes and software versions. Four cohort
figures and every individual diagnostic are exported as PNG and PDF, plus a
combined individual PDF. The diagnostic style follows the sibling reports:
phase shading, blue answered RTs, orange fast RTs, crimson nonpositive RTs, a
symlog RT axis linear through 200 ms, timeouts at 10,000 ms, fast fractions and
accuracy traces, sized open markers for fast-only accuracy, and grey peer bands.

Anticipatory comparison pages contain up to two flagged participants and a
comparator with the same block order and key mapping. Eligible comparators have
complete scopes, no anticipatory flags, and a positive median answered RT;
other flags do not disqualify them. Selection minimises the sum of absolute
MAD-standardised distances from the eligible pool's medians of experimental
accuracy and positive RT. Zero MAD contributes zero; ties use participant ID.
An empty pool is reported without relaxing the match. If there are no
anticipatory flags, no comparison PDF is generated and the report says so.

Peer bands pool all other complete participants without anticipatory flags,
across orders and mappings, aligned by condition and within-block trial. They
show median and interquartile range, not confidence intervals. Every pool ID and
band value is exported. Comparator pools and peer pools serve different purposes.

Generation uses a staging directory. Only a complete build with unchanged
input, configuration, implementation and raw-source hashes is published.
Existing owned reports are preserved as `.previous_TIMESTAMP` siblings. A failed
build remains unpublished. Protected input/source/analysis paths and unowned
output directories cannot be replaced. Generated reports, backups and validation
artifacts are ignored by Git.

## Validation

Synthetic tests cover all 12 order/key cells, exact rule boundaries, timeouts,
nonpositive RTs, missing/invalid scopes, duplicate or ambiguous runs, source
reconciliation, candidate deduplication, comparator selection and empty pools.
`tests/validate_participant_qc.py` imports neither production QC calculations nor
task allocation code. It independently reconstructs allocation, scope metrics,
Wilson intervals, flags, bins, peer quantiles, comparator rankings, raw-source
agreement, output hashes, local HTML links, PNG integrity and PDF page counts.
Validation receipts are separate from the report inventory. Figures also require
visual inspection; render representative PDFs with `pdftoppm` to check layout.
`python tests/smoke_participant_qc.py --work-dir /path/to/empty/directory` builds
and independently validates a synthetic report with flagged comparisons, an
unavailable comparator and an incomplete scope. It also verifies that rebuilding
preserves the previous report and that a source mismatch cannot replace it.

This cohort's expected result is 48 participants / 86,400 trials, zero candidates,
and manual-only diagnostic flags for 28, 29 and 40. It contains 10 nonpositive
answered RTs and 36 timeouts. These are optional validator assertions, never
production selection rules.
