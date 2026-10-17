"""Reconcile already-selected runs without choosing or changing identities."""
import csv
import hashlib
import math
import re
from pathlib import Path


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def equivalent(a, b):
    """R collation changes numeric formatting, booleans and missing tokens."""
    def normalize(x):
        value = str(x).strip()
        if value.lower() in ("", "na", "nan"):
            return None
        if value.lower() in ("true", "false"):
            return value.lower() == "true"
        try:
            return float(value)
        except ValueError:
            return value
    a, b = normalize(a), normalize(b)
    if isinstance(a, (float, bool)) and isinstance(b, (float, bool)):
        return math.isclose(a, b, rel_tol=1e-12, abs_tol=1e-12)
    return a == b


def reconcile_sources(source, raw_dir):
    with Path(source).open(newline="") as handle:
        reader = csv.DictReader(handle)
        fields = reader.fieldnames
        canonical = list(reader)
    selected = {}
    for row in canonical:
        key = (int(row["participant_id"]), row["run_timestamp"])
        selected.setdefault(key, []).append(row)
    ledger, matched = [], set()
    for path in sorted(Path(raw_dir).glob("results_*_b00_ALL.csv")):
        match = re.fullmatch(r"results_p(\d+)_(\d{8}_\d{6})_b00_ALL\.csv", path.name)
        if not match:
            raise ValueError(f"Unrecognized raw ALL-file identity: {path}")
        key = (int(match[1]), match[2])
        chosen = key in selected
        ledger.append(dict(participant_id=key[0], run_timestamp=key[1], path=str(path.resolve()),
                           sha256=sha256(path), selected=chosen,
                           status="selected_in_input" if chosen else "not_selected_in_input"))
        if not chosen:
            continue
        if key in matched:
            raise ValueError(f"Ambiguous raw files for selected run {key}")
        matched.add(key)
        with path.open(newline="") as handle:
            reader = csv.DictReader(handle)
            raw_fields = reader.fieldnames or []
            raw_rows = list(reader)
        if len(raw_fields) != len(set(raw_fields)) or not {"participant_id", "run_timestamp", "global_trial"}.issubset(raw_fields):
            raise ValueError(f"Invalid raw source schema: {path}")
        if set(raw_fields) - set(fields):
            raise ValueError(f"Raw source fields absent from canonical input: {path}")
        canonical_by_trial = {int(r["global_trial"]): r for r in selected[key]}
        seen = set()
        for raw in raw_rows:
            if None in raw or any(v is None for v in raw.values()):
                raise ValueError(f"Malformed raw CSV row: {path}")
            trial = int(raw["global_trial"])
            if trial in seen or trial not in canonical_by_trial:
                raise ValueError(f"Raw/canonical trial mismatch or duplicate: {path}, trial {trial}")
            seen.add(trial)
            for field in raw_fields:
                if not equivalent(raw[field], canonical_by_trial[trial][field]):
                    raise ValueError(f"Raw/canonical source mismatch: {path}, trial {trial}, {field}")
        if seen != set(canonical_by_trial):
            raise ValueError(f"Raw/canonical row count mismatch: {path}")
        ledger[-1].update(trial_n=len(raw_rows), compared_columns=raw_fields)
    if matched != set(selected):
        raise ValueError(f"Missing raw sources for selected runs: {sorted(set(selected) - matched)}")
    return ledger
