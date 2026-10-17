"""Data integrity, transparent descriptive screens, and comparator selection.

No inclusion decisions, trial removal, or hypothesis-model fitting occur here.
"""
import csv
import json
import math
import os
import statistics
from collections import Counter, defaultdict
from pathlib import Path

os.environ.setdefault("PYGAME_HIDE_SUPPORT_PROMPT", "1")
from python import virus_task as task

ROOT = Path(__file__).resolve().parents[1]
IDENTITY_FILE = ROOT / "config/participant_identity_corrections.csv"
EPS = 1e-12
CONDITIONS = ["CAL_LP", "M_HP", "A_HP", "M_LP", "A_LP"]
LABELS = {"CAL_LP": "Calibration LP", "M_HP": "Manual HP", "A_HP": "Aided HP",
          "M_LP": "Manual LP", "A_LP": "Aided LP"}
# Fixed display order only; chronological bounds always come from task allocation.
SEGMENTS = [(c, LABELS[c]) for c in CONDITIONS]
SCOPES = SEGMENTS + [("aided_overall", "Aided overall"),
                     ("experimental_overall", "Experimental overall")]
META = ("participant_id", "source_participant_id", "run_timestamp",
        "automation_reliability_pattern", "key_black", "key_white", "main_block_order")


def identity_corrections(path=IDENTITY_FILE):
    with Path(path).open(newline="") as handle:
        records = list(csv.DictReader(handle))
    mapping, targets = {}, set()
    for r in records:
        key = (int(r["source_participant_id"]), r["run_timestamp"])
        target = int(r["participant_id"])
        if key in mapping or target in targets or min(key[0], target) < 1 or not key[1] or not r["reason"]:
            raise ValueError("Invalid or conflicting identity corrections")
        mapping[key] = target
        targets.add(target)
    return mapping


def segments_for_person(rows):
    blocks = task.build_blocks_for_participant(rows[0]["source_participant_id"], task.BLOCKS)
    segments, lo = [], 1
    for idx, block in enumerate(blocks, 1):
        code = block["CONDITION_DEADLINE_CODE"]
        hi = lo + block["N_TRIALS"] - 1
        segments.append((code, LABELS[code], lo, hi, idx, block))
        lo = hi + 1
    return segments


def scopes_for_person(rows, cfg):
    segments = segments_for_person(rows)
    scopes = [(code, LABELS[code], set(range(lo, hi+1)))
              for code in CONDITIONS for c, _, lo, hi, *_ in segments if c == code]
    scopes.extend([
        ("aided_overall", "Aided overall", {t for c, _, lo, hi, *_ in segments
                                            if c.startswith("A_") for t in range(lo, hi+1)}),
        ("experimental_overall", "Experimental overall", set(range(301, 1901))),
        ("calibration_tail", "Calibration final trials", set(range(301-cfg["calibration_tail_n"], 301))),
    ])
    return scopes


CATEGORIES = ["calibration_target", "manual_target",
              "near_chance_or_worse", "anticipatory"]
EXCLUSION_CATEGORIES = ["calibration_target", "near_chance_or_worse", "anticipatory"]
DIAGNOSTIC_CATEGORIES = ["manual_target"]
REQUIRED = {
    "participant_id", "source_participant_id", "run_timestamp", "block", "block_idx", "trial", "global_trial",
    "condition_deadline_code", "time_pressure_condition", "automation_reliability_pattern",
    "automation_reliability_group", "keymap_flip", "key_black", "key_white", "trial_deadline_s",
    "stimulus", "response", "correct", "feedback", "rt_s", "aid_label", "aid_correct",
    "aid_accuracy_setting", "auto_on",
}


def number(value):
    try:
        result = float(value)
        return result if math.isfinite(result) else None
    except (TypeError, ValueError):
        return None


def empty(value):
    return value is None or str(value).strip() in ("", "NA", "NaN")


def binary(value):
    return {"true": 1, "false": 0, "1": 1, "0": 0}.get(str(value).strip().lower())


def load_config(path):
    cfg = json.loads(Path(path).read_text())
    default = json.loads(Path(__file__).with_name("config.json").read_text())
    if set(cfg) != set(default):
        raise ValueError("Configuration keys must match the documented config.json schema")
    if cfg["schema_version"] != 1:
        raise ValueError("Unsupported configuration version")
    for key in ("calibration_tail_n", "fast_count_min", "bin_size"):
        if type(cfg[key]) is not int or cfg[key] <= 0:
            raise ValueError(f"{key} must be a positive integer")
    if cfg["calibration_tail_n"] > 250 or any(n % cfg["bin_size"] for n in (300, 400)):
        raise ValueError("Calibration tail must be post-burn-in; bins must divide all segments")
    for key in ("chance_accuracy_max", "fast_fraction_min", "fast_accuracy_max", "interval_level"):
        if not isinstance(cfg[key], (int, float)) or not 0 < cfg[key] < 1:
            raise ValueError(f"{key} must lie strictly between zero and one")
    if not 0 < cfg["fast_rt_seconds"] < 5:
        raise ValueError("Invalid RT threshold")
    for key in ("calibration_target_band_pp", "manual_target_band_pp"):
        if not 0 < cfg[key] < 100:
            raise ValueError(f"Invalid {key}")
    thresholds = cfg["sensitivity_thresholds_ms"]
    if not isinstance(thresholds, list) or not thresholds or any(type(x) is not int or x <= 0 for x in thresholds):
        raise ValueError("Sensitivity thresholds must be positive integer milliseconds")
    if thresholds != sorted(set(thresholds)):
        raise ValueError("Sensitivity thresholds must be unique and sorted")
    return cfg


def wilson(k, n, level=.95):
    if not n:
        return None, None
    z = statistics.NormalDist().inv_cdf((1+level)/2)
    p = k/n
    scale = 1 + z*z/n
    center = (p + z*z/(2*n))/scale
    half = z*math.sqrt(p*(1-p)/n + z*z/(4*n*n))/scale
    return max(0, center-half), min(1, center+half)


def load_trials(path):
    people, keys, globals_seen = defaultdict(list), set(), set()
    corrections = identity_corrections()
    with Path(path).open(newline="") as handle:
        reader = csv.DictReader(handle)
        missing = REQUIRED - set(reader.fieldnames or [])
        if missing:
            raise ValueError("Missing required trial columns: " + ", ".join(sorted(missing)))
        for line, raw in enumerate(reader, 2):
            row = dict(raw)
            for key in ("participant_id", "source_participant_id", "block_idx", "trial", "global_trial"):
                value = number(row[key])
                if value is None or value <= 0 or value % 1:
                    raise ValueError(f"Invalid {key} at CSV line {line}")
                row[key] = int(value)
            if empty(row["run_timestamp"]):
                raise ValueError(f"Missing run identity at CSV line {line}")
            source_identity = (row["source_participant_id"], row["run_timestamp"])
            if row["participant_id"] != corrections.get(source_identity, row["source_participant_id"]):
                raise ValueError(f"Inconsistent recorded and analysis participant identities at CSV line {line}")
            allocation = task.counterbalance_allocation_for_participant(row["source_participant_id"])
            row["main_block_order"] = "|".join(
                f"{name[0]}_{pressure}" for pressure, name in allocation["post_calibration_block_order"])
            identity = (row["participant_id"], row["run_timestamp"])
            key = (*identity, row["block_idx"], row["trial"])
            global_key = (*identity, row["global_trial"])
            if key in keys or global_key in globals_seen:
                raise ValueError(f"Duplicate trial identity at CSV line {line}")
            keys.add(key); globals_seen.add(global_key)
            row["rt"] = number(row["rt_s"])
            row["answered"] = row["response"] in ("BLACK", "WHITE")
            row["timeout"] = row["response"] == "TIMEOUT"
            row["is_correct"] = int(row["answered"] and row["response"] == row["stimulus"])
            people[row["participant_id"]].append(row)
    if not people:
        raise ValueError("No trial records")
    for pid, rows in people.items():
        if len({(r["source_participant_id"], r["run_timestamp"]) for r in rows}) != 1:
            raise ValueError(f"Ambiguous multiple runs for participant {pid}; select one run explicitly")
        rows.sort(key=lambda r: r["global_trial"])
    return dict(sorted(people.items()))


def validate_person(rows):
    source_id = rows[0]["source_participant_id"]
    allocation = task.counterbalance_allocation_for_participant(source_id)
    mapping = task.key_mapping_for_participant(source_id)
    segments = segments_for_person(rows)
    errors, global_errors = defaultdict(list), []
    for r in rows:
        t = r["global_trial"]
        segment = next((s for s in segments if s[2] <= t <= s[3]), None)
        if segment is None:
            global_errors.append("Trial outside expected 1-1900 session")
            continue
        code, _, lo, hi, idx, block = segment
        if (r["block_idx"] != idx or r["block"] != block["name"] or r["trial"] != t-lo+1
                or r["condition_deadline_code"] != code):
            errors[t].append("Block/condition/trial sequence mismatch")
        if (r["automation_reliability_pattern"] != allocation["reliability_pattern"]
                or binary(r["keymap_flip"]) != int(allocation["key_flip"])
                or r["key_black"] != mapping["key_black_name"] or r["key_white"] != mapping["key_white_name"]):
            errors[t].append("Allocation or key mapping mismatch")
        if (r["time_pressure_condition"] != block["TIME_PRESSURE_CONDITION"]
                or number(r["trial_deadline_s"]) != block["TRIAL_DEADLINE_MS"]/1000
                or binary(r["auto_on"]) != int(block["AUTOMATION_ON"])):
            errors[t].append("Pressure, deadline, or automation state mismatch")
        if r["automation_reliability_group"] != block["AUTOMATION_RELIABILITY_GROUP"]:
            errors[t].append("Reliability group mismatch")
        if block["AUTOMATION_ON"]:
            if number(r["aid_accuracy_setting"]) != block["AID_ACCURACY"]:
                errors[t].append("Reliability setting mismatch")
            if r["aid_label"] not in ("BLACK", "WHITE") or binary(r["aid_correct"]) != int(r["aid_label"] == r["stimulus"]):
                errors[t].append("Invalid advice identity/correctness")
        elif any(not empty(r[k]) for k in ("aid_accuracy_setting", "aid_label", "aid_correct")):
            errors[t].append("Unexpected advice outside automation")
        if r["stimulus"] not in ("BLACK", "WHITE") or not (r["answered"] or r["timeout"]):
            errors[t].append("Invalid stimulus or response")
        if r["answered"] and (binary(r["correct"]) != r["is_correct"] or r["feedback"] != ("CORRECT" if r["is_correct"] else "INCORRECT")):
            errors[t].append("Response correctness/feedback mismatch")
        if r["timeout"] and ((not empty(r["correct"]) and binary(r["correct"]) != 0)
                             or r["feedback"] != "TOO SLOW"):
            errors[t].append("Timeout correctness/feedback mismatch")
        if r["answered"] and r["rt"] is None:
            errors[t].append("Answered trial has missing/nonfinite RT")
        # A keypress can be timestamped in the frame that the deadline takes
        # precedence. The task exports its RT but labels the trial TIMEOUT.
        if r["timeout"] and not empty(r["rt_s"]) and r["rt"] is None:
            errors[t].append("Timeout has an invalid recorded RT")
    return errors, sorted(set(global_errors))


def scope_metrics(rows, scope, label, lo, hi, cfg, errors, global_errors, expected_trials=None):
    first = rows[0]
    expected_trials = set(range(lo, hi+1)) if expected_trials is None else expected_trials
    selected = [r for r in rows if r["global_trial"] in expected_trials]
    issues = set(global_errors)
    if {r["global_trial"] for r in selected} != expected_trials:
        issues.add(f"Incomplete scope: expected {len(expected_trials)} trials, found {len(selected)}")
    for r in selected:
        issues.update(errors.get(r["global_trial"], []))
    assessable = not issues
    fast = [r for r in selected if r["answered"] and r["rt"] is not None and r["rt"] < cfg["fast_rt_seconds"]]
    k, fk = sum(r["is_correct"] for r in selected), sum(r["is_correct"] for r in fast)
    n, nf = len(selected), len(fast)
    lower, upper = wilson(k, n, cfg["interval_level"]) if assessable else (None, None)
    fl, fu = wilson(fk, nf, cfg["interval_level"]) if assessable else (None, None)
    positive = [r["rt"] for r in selected if r["answered"] and r["rt"] is not None and r["rt"] > 0]
    m = {k: first[k] for k in META}
    m.update(calibration_target_accuracy=task.CALIBRATION_TARGET_ACCURACY,
        scope=scope, scope_label=label, trial_start=lo, trial_end=hi,
        assessable=assessable, integrity_issues="; ".join(sorted(issues)), n_expected=len(expected_trials),
        n_trials=n, n_answered=sum(r["answered"] for r in selected), n_timeouts=sum(r["timeout"] for r in selected),
        n_correct=k if assessable else None, accuracy=k/n if assessable else None,
        accuracy_lower=lower, accuracy_upper=upper, n_fast=nf,
        fast_fraction=nf/n if assessable else None, n_fast_correct=fk if assessable else None,
        fast_accuracy=fk/nf if assessable and nf else None, fast_accuracy_lower=fl, fast_accuracy_upper=fu,
        n_nonpositive_rt=sum(r["answered"] and r["rt"] is not None and r["rt"] <= 0 for r in selected),
        n_positive_fast=sum(r["rt"] > 0 for r in fast),
        median_positive_rt_ms=1000*statistics.median(positive) if positive else None)
    answered_n = sum(r["answered"] for r in selected)
    m["answered_accuracy"] = k/answered_n if assessable and answered_n else None
    m["timeout_fraction"] = m["n_timeouts"]/n if assessable else None
    m["n_timeouts_with_recorded_rt"] = sum(r["timeout"] and r["rt"] is not None for r in selected)
    m["n_beyond_deadline"] = sum(r["answered"] and r["rt"] is not None
        and number(r["trial_deadline_s"]) is not None and r["rt"] > number(r["trial_deadline_s"]) for r in selected)
    for key in ("condition_deadline_code", "time_pressure_condition", "automation_reliability_group"):
        m[key] = "|".join(sorted({str(r[key]) for r in selected if not empty(r[key])}))
    for ms in cfg["sensitivity_thresholds_ms"]:
        m[f"fraction_rt_lt_{ms}ms"] = sum(r["answered"] and r["rt"] is not None and r["rt"] < ms/1000 for r in selected)/n if assessable else None
    m["target_difference_pp"] = 100*(m["accuracy"]-m["calibration_target_accuracy"]) if assessable else None
    return m


def screen(m, category, cfg):
    if category not in CATEGORIES:
        raise ValueError(f"Unknown screening category: {category}")
    a, f, fa = m["accuracy"], m["fast_fraction"], m["fast_accuracy"]
    if category in ("calibration_target", "manual_target"):
        band = cfg[f"{category}_band_pp"]
        rule = f"Absolute target difference > {band:g} pp"
        flagged = a is not None and abs(m["target_difference_pp"]) > band+EPS
    elif category == "near_chance_or_worse":
        rule = f"All-trial accuracy <= {100*cfg['chance_accuracy_max']:g}%"
        flagged = a is not None and a <= cfg["chance_accuracy_max"]+EPS
    else:
        rule = (f"RT < {1000*cfg['fast_rt_seconds']:g} ms; fraction >= {cfg['fast_fraction_min']:g}; "
                f"n >= {cfg['fast_count_min']}; fast accuracy <= {cfg['fast_accuracy_max']:g}")
        flagged = (f is not None and fa is not None and f >= cfg["fast_fraction_min"]-EPS
                   and m["n_fast"] >= cfg["fast_count_min"] and fa <= cfg["fast_accuracy_max"]+EPS)
    return {**{k: m[k] for k in (*META, "scope", "scope_label",
        "n_trials", "accuracy", "target_difference_pp", "n_fast", "fast_fraction", "fast_accuracy", "integrity_issues")},
        "category": category, "use_for_exclusion": category in EXCLUSION_CATEGORIES, "rule": rule,
        "status": "not_assessable" if not m["assessable"] else "flag" if flagged else "clear"}


def analyse(people, cfg):
    metrics, checks, summaries, bins, issues = [], [], [], [], []
    for pid, rows in people.items():
        errors, global_errors = validate_person(rows)
        person_metrics, person_checks = [], []
        scopes = scopes_for_person(rows, cfg)
        for scope, label, expected in scopes:
            m = scope_metrics(rows, scope, label, min(expected), max(expected), cfg, errors, global_errors, expected)
            person_metrics.append(m)
            categories = ["calibration_target"] if scope == "calibration_tail" else ["near_chance_or_worse", "anticipatory"]
            if scope == "M_LP":
                categories += ["manual_target"]
            person_checks.extend(screen(m, category, cfg) for category in categories)
            if not m["assessable"]:
                issues.append({k: m[k] for k in (*META, "scope", "n_trials", "n_expected", "integrity_issues")})
        for s in segments_for_person(rows):
            for lo in range(s[2], s[3]+1, cfg["bin_size"]):
                b = scope_metrics(rows, s[0], s[1], lo, lo+cfg["bin_size"]-1, cfg, errors, global_errors)
                b["trial_midpoint"] = lo+(cfg["bin_size"]-1)/2
                b["condition_trial_start"] = lo-s[2]+1
                b["condition_trial_midpoint"] = b["trial_midpoint"]-s[2]+1
                bins.append(b)
        summary = {k: rows[0][k] for k in META}
        for category in CATEGORIES:
            statuses = [c["status"] for c in person_checks if c["category"] == category]
            summary[category] = "flag" if "flag" in statuses else "not_assessable" if "not_assessable" in statuses else "clear"
        flagged = [c for c in person_checks if c["status"] == "flag"]
        summary.update(n_flags=len(flagged), n_not_assessable=sum(c["status"] == "not_assessable" for c in person_checks),
                       n_flagged_rules=sum(summary[c] == "flag" for c in CATEGORIES),
                       flagged_any_rule=bool(flagged),
                       n_exclusion_rules=sum(summary[c] == "flag" for c in EXCLUSION_CATEGORIES),
                       exclude_by_rules=any(summary[c] == "flag" for c in EXCLUSION_CATEGORIES),
                       exclusion_reasons="; ".join(f"{c['category']}:{c['scope']}" for c in flagged if c["use_for_exclusion"]),
                       review_required=bool(flagged) or any(not m["assessable"] for m in person_metrics),
                       reasons="; ".join(f"{c['category']}:{c['scope']}" for c in flagged))
        summaries.append(summary); metrics.extend(person_metrics); checks.extend(person_checks)
    return dict(metrics=metrics, checks=checks, summaries=summaries, bins=bins, issues=issues)


def exclusion_summary(result):
    """Count each participant once; integrity findings alone are not exclusions."""
    summaries = result["summaries"]
    candidates = [s for s in summaries if s["exclude_by_rules"]]
    diagnostic_only = [s["participant_id"] for s in summaries if s["flagged_any_rule"] and not s["exclude_by_rules"]]
    incomplete = [s["participant_id"] for s in summaries if s["n_not_assessable"]]
    total, n = len(summaries), len(candidates)
    application = result.get("primary_analysis", {})
    recorded = defaultdict(list)
    for s in summaries:
        recorded[s["source_participant_id"]].append(s)
    identity_cases = []
    for source_id, runs in sorted(recorded.items()):
        if len(runs) < 2:
            continue
        identity_cases.append(dict(source_participant_id=source_id,
            analysis_participant_ids=sorted(s["participant_id"] for s in runs),
            runs=[dict(participant_id=s["participant_id"], run_timestamp=s["run_timestamp"],
                       allocation_source_participant_id=source_id,
                       included_in_primary=(s["participant_id"] not in application.get("excluded_participant_ids", [])) if application else None)
                  for s in sorted(runs,key=lambda s:s["run_timestamp"])],
            resolution="Recorded ID reused across separate participant runs. The later run has a distinct analysis ID; "
                       "both runs retain the allocation actually administered under the recorded ID. "
                       "Identity correction is not an exclusion criterion."))
    return dict(
        participant_n=total,
        unique_flagged_n=n,
        unique_flagged_percent=100*n/total if total else 0,
        remaining_if_applied_n=total-n,
        participant_ids=sorted(s["participant_id"] for s in candidates),
        exclusion_categories=EXCLUSION_CATEGORIES,
        diagnostic_categories=DIAGNOSTIC_CATEGORIES,
        rule_counts={c:sum(s[c] == "flag" for s in summaries) for c in EXCLUSION_CATEGORIES},
        diagnostic_rule_counts={c:sum(s[c] == "flag" for s in summaries) for c in DIAGNOSTIC_CATEGORIES},
        rule_participant_ids={c:sorted(s["participant_id"] for s in summaries if s[c] == "flag") for c in CATEGORIES},
        diagnostic_only_participant_ids=sorted(diagnostic_only),
        diagnostic_only_n=len(diagnostic_only),
        overlap=[dict(n_rules=k, participant_n=sum(s["n_exclusion_rules"] == k for s in candidates),
                      participant_ids=sorted(s["participant_id"] for s in candidates if s["n_exclusion_rules"] == k))
                 for k in range(1,len(EXCLUSION_CATEGORIES)+1)],
        incomplete_assessment_ids=sorted(incomplete),
        exclusions_applied=application.get("exclusions_applied", False),
        qc_data_filtered=False,
        primary_analysis=application,
        identity_reuse_cases=identity_cases,
        note="Union of participants meeting calibration, near-chance, or anticipatory exclusion criteria; repeated scopes count once. "
             "Manual target deviation is diagnostic only and never contributes to the exclusion total. "
             "Unassessable checks are separate and are not treated as exclusions or evidence of eligibility.")


def choose_comparators(result):
    summaries = result["summaries"]
    overall = {m["participant_id"]: m for m in result["metrics"] if m["scope"] == "experimental_overall"}
    flagged = {s["participant_id"] for s in summaries if s["anticipatory"] == "flag"}
    matches = defaultdict(list)
    for s in summaries:
        if s["participant_id"] in flagged:
            matches[s["automation_reliability_pattern"], s["key_black"], s["key_white"]].append(s["participant_id"])
    output = []
    for match, targets in sorted(matches.items()):
        pool = [s["participant_id"] for s in summaries if (s["automation_reliability_pattern"], s["key_black"], s["key_white"]) == match
                and s["participant_id"] not in flagged and s["n_not_assessable"] == 0
                and overall[s["participant_id"]]["median_positive_rt_ms"] is not None]
        centers, scales = {}, {}
        for key in ("median_positive_rt_ms", "accuracy"):
            if pool:
                centers[key] = statistics.median(overall[p][key] for p in pool)
                scales[key] = statistics.median(abs(overall[p][key]-centers[key]) for p in pool)
        ranked = sorted([{"participant_id": p, "score": sum(abs(overall[p][k]-centers[k])/scales[k] for k in centers if scales[k] > EPS)} for p in pool], key=lambda r: (r["score"], r["participant_id"]))
        output.append(dict(group=match[0], key_black=match[1], key_white=match[2], targets=sorted(targets),
            comparator=ranked[0]["participant_id"] if ranked else None, ranked_candidates=ranked,
            centers=centers, median_absolute_deviations=scales,
            note="Zero-MAD metrics contribute zero distance; ties use participant ID." if ranked else "No complete, non-anticipatory-flagged matching comparator available."))
    return output
