"""Data integrity, transparent descriptive screens, and comparator selection.

No inclusion decisions, trial removal, or hypothesis-model fitting occur here.
"""
import csv
import json
import math
import statistics
from collections import Counter, defaultdict
from pathlib import Path

EPS = 1e-12
SEGMENTS = [
    ("calibration_full", "Calibration", 1, 300, 1, "CALIBRATION", "CAL", ""),
    ("manual_pre", "Manual pre", 301, 500, 2, "MANUAL", "MAN", "PRE_AUTOMATION"),
    ("P1_95", "Aid 95%", 501, 900, 3, "AUTOMATION", "REL_DROP", "P1_95"),
    ("P2_70", "Aid 70%", 901, 1300, 3, "AUTOMATION", "REL_DROP", "P2_70"),
    ("P3_95", "Aid 95% again", 1301, 1700, 3, "AUTOMATION", "REL_DROP", "P3_95"),
    ("manual_post", "Manual post", 1701, 1900, 4, "MANUAL", "MAN", "POST_AUTOMATION"),
]
SCOPES = [(s[0], s[1], s[2], s[3]) for s in SEGMENTS] + [
    ("aided_overall", "Aided block overall", 501, 1700),
    ("experimental_overall", "Experimental overall", 301, 1900),
]
CATEGORIES = ["calibration_target", "manual_target",
              "near_chance_or_worse", "anticipatory"]
EXCLUSION_CATEGORIES = ["calibration_target", "near_chance_or_worse", "anticipatory"]
DIAGNOSTIC_CATEGORIES = ["manual_target"]
REQUIRED = {
    "participant_id", "run_timestamp", "block", "block_idx", "trial", "global_trial",
    "calibration_target_group", "calibration_target_accuracy", "main_block_order",
    "manual_segment", "reliability_phase_label", "reliability_phase_idx",
    "trial_in_reliability_phase", "aid_reliability_level", "automation_reliability_group",
    "condition_code", "key_black", "key_white", "trial_deadline_s", "stimulus",
    "response", "correct", "feedback", "rt_s", "aid_label", "aid_correct",
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
    if cfg["calibration_tail_n"] > 250 or any((s[3]-s[2]+1) % cfg["bin_size"] for s in SEGMENTS):
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
    with Path(path).open(newline="") as handle:
        reader = csv.DictReader(handle)
        missing = REQUIRED - set(reader.fieldnames or [])
        if missing:
            raise ValueError("Missing required trial columns: " + ", ".join(sorted(missing)))
        for line, raw in enumerate(reader, 2):
            row = dict(raw)
            for key in ("participant_id", "block_idx", "trial", "global_trial"):
                value = number(row[key])
                if value is None or value <= 0 or value % 1:
                    raise ValueError(f"Invalid {key} at CSV line {line}")
                row[key] = int(value)
            if empty(row["run_timestamp"]):
                raise ValueError(f"Missing run identity at CSV line {line}")
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
        if len({r["run_timestamp"] for r in rows}) != 1:
            raise ValueError(f"Ambiguous multiple runs for participant {pid}; select one run explicitly")
        rows.sort(key=lambda r: r["global_trial"])
    return dict(sorted(people.items()))


def validate_person(rows):
    first = rows[0]; pid = first["participant_id"]
    errors, global_errors = defaultdict(list), []
    group = "CAL65" if pid % 2 else "CAL90"
    target = .65 if pid % 2 else .9
    flipped = ((pid-1)//2) % 2 == 1
    expected_black, expected_white = ("J", "D") if flipped else ("D", "J")
    for r in rows:
        t = r["global_trial"]
        if (r["calibration_target_group"] != group or number(r["calibration_target_accuracy"]) != target
                or r["key_black"] != expected_black or r["key_white"] != expected_white
                or r["main_block_order"] != "SPLIT_MANUAL" or number(r["trial_deadline_s"]) != 5):
            global_errors.append("Allocation, target, mapping, block order, or deadline mismatch")
        segment = next((s for s in SEGMENTS if s[2] <= t <= s[3]), None)
        if segment is None:
            global_errors.append("Trial outside the expected 1-1900 session")
            continue
        code, _, lo, hi, block_idx, block, condition, phase = segment
        trial_offset = {1: 0, 2: 300, 3: 500, 4: 1700}[block_idx]
        if (r["block_idx"] != block_idx or r["block"] != block or r["condition_code"] != condition
                or r["trial"] != t-trial_offset):
            errors[t].append("Block/condition/trial sequence mismatch")
        if block == "MANUAL":
            if r["manual_segment"] != phase:
                errors[t].append("Manual segment mismatch")
        elif not empty(r["manual_segment"]):
            errors[t].append("Unexpected manual segment")
        if block == "AUTOMATION":
            phase_idx = (t-501)//400+1
            reliability = .7 if phase_idx == 2 else .95
            if (r["reliability_phase_label"] != phase or number(r["reliability_phase_idx"]) != phase_idx
                    or number(r["trial_in_reliability_phase"]) != t-lo+1
                    or number(r["aid_reliability_level"]) != reliability
                    or r["automation_reliability_group"] != ("low" if phase_idx == 2 else "high")):
                errors[t].append("Reliability phase metadata mismatch")
            if r["aid_label"] not in ("BLACK", "WHITE") or binary(r["aid_correct"]) != int(r["aid_label"] == r["stimulus"]):
                errors[t].append("Invalid advice identity/correctness")
        elif any(not empty(r[k]) for k in ("reliability_phase_label", "reliability_phase_idx", "trial_in_reliability_phase", "aid_reliability_level", "aid_label", "aid_correct")):
            errors[t].append("Unexpected advice/phase metadata outside automation")
        if r["stimulus"] not in ("BLACK", "WHITE") or not (r["answered"] or r["timeout"]):
            errors[t].append("Invalid stimulus or response")
        if r["answered"] and (binary(r["correct"]) != r["is_correct"] or (r["feedback"] == "CORRECT") != bool(r["is_correct"])):
            errors[t].append("Response correctness/feedback mismatch")
        if r["timeout"] and (binary(r["correct"]) == 1 or r["feedback"] == "CORRECT"):
            errors[t].append("Timeout marked correct")
        if r["answered"] and r["rt"] is None:
            errors[t].append("Answered trial has missing/nonfinite RT")
        if r["timeout"] and not empty(r["rt_s"]):
            errors[t].append("Timeout has a recorded RT")
    return errors, sorted(set(global_errors))


def scope_metrics(rows, scope, label, lo, hi, cfg, errors, global_errors):
    first = rows[0]
    selected = [r for r in rows if lo <= r["global_trial"] <= hi]
    issues = set(global_errors)
    if {r["global_trial"] for r in selected} != set(range(lo, hi+1)):
        issues.add(f"Incomplete scope: expected {hi-lo+1} trials, found {len(selected)}")
    for r in selected:
        issues.update(errors.get(r["global_trial"], []))
    assessable = not issues
    fast = [r for r in selected if r["answered"] and r["rt"] is not None and r["rt"] < cfg["fast_rt_seconds"]]
    k, fk = sum(r["is_correct"] for r in selected), sum(r["is_correct"] for r in fast)
    n, nf = len(selected), len(fast)
    lower, upper = wilson(k, n, cfg["interval_level"]) if assessable else (None, None)
    fl, fu = wilson(fk, nf, cfg["interval_level"]) if assessable else (None, None)
    positive = [r["rt"] for r in selected if r["answered"] and r["rt"] is not None and r["rt"] > 0]
    m = {k: first[k] for k in ("participant_id", "run_timestamp", "calibration_target_group",
                               "key_black", "key_white", "main_block_order")}
    m.update(calibration_target_accuracy=number(first["calibration_target_accuracy"]),
        scope=scope, scope_label=label, trial_start=lo, trial_end=hi,
        assessable=assessable, integrity_issues="; ".join(sorted(issues)), n_expected=hi-lo+1,
        n_trials=n, n_answered=sum(r["answered"] for r in selected), n_timeouts=sum(r["timeout"] for r in selected),
        n_correct=k if assessable else None, accuracy=k/n if assessable else None,
        accuracy_lower=lower, accuracy_upper=upper, n_fast=nf,
        fast_fraction=nf/n if assessable else None, n_fast_correct=fk if assessable else None,
        fast_accuracy=fk/nf if assessable and nf else None, fast_accuracy_lower=fl, fast_accuracy_upper=fu,
        n_nonpositive_rt=sum(r["answered"] and r["rt"] is not None and r["rt"] <= 0 for r in selected),
        n_positive_fast=sum(r["rt"] > 0 for r in fast),
        median_positive_rt_ms=1000*statistics.median(positive) if positive else None)
    for key in ("manual_segment", "reliability_phase_label", "automation_reliability_group"):
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
    return {**{k: m[k] for k in ("participant_id", "run_timestamp", "calibration_target_group", "scope", "scope_label",
        "n_trials", "accuracy", "target_difference_pp", "n_fast", "fast_fraction", "fast_accuracy", "integrity_issues")},
        "category": category, "use_for_exclusion": category in EXCLUSION_CATEGORIES, "rule": rule,
        "status": "not_assessable" if not m["assessable"] else "flag" if flagged else "clear"}


def analyse(people, cfg):
    metrics, checks, summaries, bins, issues = [], [], [], [], []
    for pid, rows in people.items():
        errors, global_errors = validate_person(rows)
        person_metrics, person_checks = [], []
        scopes = SCOPES + [("calibration_tail", "Calibration final trials", 301-cfg["calibration_tail_n"], 300)]
        for scope, label, lo, hi in scopes:
            m = scope_metrics(rows, scope, label, lo, hi, cfg, errors, global_errors)
            person_metrics.append(m)
            categories = ["calibration_target"] if scope == "calibration_tail" else ["near_chance_or_worse", "anticipatory"]
            if scope in ("manual_pre", "manual_post"):
                categories += ["manual_target"]
            person_checks.extend(screen(m, category, cfg) for category in categories)
            if not m["assessable"]:
                issues.append({k: m[k] for k in ("participant_id", "run_timestamp", "scope", "n_trials", "n_expected", "integrity_issues")})
        for s in SEGMENTS:
            for lo in range(s[2], s[3]+1, cfg["bin_size"]):
                b = scope_metrics(rows, s[0], s[1], lo, lo+cfg["bin_size"]-1, cfg, errors, global_errors)
                b["trial_midpoint"] = lo+(cfg["bin_size"]-1)/2
                bins.append(b)
        summary = {k: rows[0][k] for k in ("participant_id", "run_timestamp", "calibration_target_group", "calibration_target_accuracy", "key_black", "key_white", "main_block_order")}
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
        exclusions_applied=False,
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
            matches[s["calibration_target_group"], s["key_black"], s["key_white"]].append(s["participant_id"])
    output = []
    for match, targets in sorted(matches.items()):
        pool = [s["participant_id"] for s in summaries if (s["calibration_target_group"], s["key_black"], s["key_white"]) == match
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
