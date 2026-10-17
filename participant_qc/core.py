"""Run-matched sources, decision-specific integrity, descriptive rules and peers.

Practice and Manual target checks are diagnostic. Only near-chance and
anticipatory checks on main-task scopes can create exclusion candidates.
"""
import csv
import hashlib
import json
import math
import os
import statistics
from collections import Counter, defaultdict
from pathlib import Path

os.environ.setdefault("PYGAME_HIDE_SUPPORT_PROMPT", "1")
from python import virus_task as task

ROOT = Path(__file__).resolve().parents[1]
MAIN = ("MANUAL", "AIDFIRST", "STIMFIRST")
CONDITIONS = ("PRACTICE", *MAIN)
SCOPES = (*CONDITIONS, "aided_overall", "experimental_overall", "calibration_tail")
CATEGORIES = ("calibration_target", "manual_target", "near_chance_or_worse", "anticipatory")
EXCLUSION_CATEGORIES = ("near_chance_or_worse", "anticipatory")
META = ("participant_id", "run_timestamp", "calibration_target_accuracy", "key_black", "key_white", "main_block_order")
REQUIRED = {
    "participant_id", "run_timestamp", "block_idx", "trial", "global_trial",
    "condition_code", "aid_condition", "key_black", "key_white", "keymap_flip",
    "trial_deadline_s", "difficulty_mode", "staircase_target_accuracy",
    "stimulus", "auto_on", "aid_label", "aid_correct", "aid_accuracy_setting",
    "preview_display", "preview_label", "decision2_preview_display", "decision2_preview_label",
    "decision1_display", "decision2_display", "changed_response", "feedback",
    *(f"decision{d}_{field}" for d in (1, 2) for field in ("response", "correct", "rt_s", "matches_aid")),
}


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def number(value):
    try:
        value = float(value)
        return value if math.isfinite(value) else None
    except (ValueError, TypeError):
        return None


def empty(value):
    return value is None or str(value).strip().lower() in ("", "na", "nan", "none")


def binary(value):
    return {"true": 1, "false": 0, "1": 1, "0": 0}.get(str(value).strip().lower())


def read_csv(path, required=()):
    with Path(path).open(newline="") as handle:
        reader = csv.DictReader(handle)
        if len(reader.fieldnames or []) != len(set(reader.fieldnames or [])):
            raise ValueError(f"Duplicate columns: {path}")
        if set(required) - set(reader.fieldnames or []):
            raise ValueError(f"Missing required columns in {path}: {sorted(set(required)-set(reader.fieldnames or []))}")
        rows = list(reader)
        if any(None in r or None in r.values() for r in rows):
            raise ValueError(f"Malformed CSV rows: {path}")
        return rows


def load_config(path):
    cfg = json.loads(Path(path).read_text())
    defaults = json.loads(Path(__file__).with_name("config.json").read_text())
    if set(cfg) != set(defaults) or cfg["schema_version"] != 1:
        raise ValueError("Configuration must match the version 1 schema")
    for key in ("calibration_tail_n", "fast_count_min", "practice_bin_size", "main_bin_size"):
        if type(cfg[key]) is not int or cfg[key] <= 0:
            raise ValueError(f"{key} must be a positive integer")
    if cfg["calibration_tail_n"] != 40:
        raise ValueError("Aid-onset target audit uses the recorded final 40 practice trials")
    for key in ("chance_accuracy_max", "fast_fraction_min", "fast_accuracy_max", "interval_level"):
        if type(cfg[key]) not in (float, int) or not 0 < cfg[key] < 1:
            raise ValueError(f"Invalid {key}")
    for key, maximum in (("fast_rt_seconds", 5), ("calibration_target_band_pp", 100), ("manual_target_band_pp", 100)):
        if type(cfg[key]) not in (float, int) or not 0 < cfg[key] < maximum:
            raise ValueError(f"Invalid {key}")
    thresholds = cfg["sensitivity_thresholds_ms"]
    if not isinstance(thresholds, list) or not thresholds or any(type(x) is not int or x <= 0 for x in thresholds) or thresholds != sorted(set(thresholds)):
        raise ValueError("Sensitivity thresholds must be sorted unique positive milliseconds")
    return cfg


def equivalent(a, b):
    if empty(a) or empty(b):
        return empty(a) and empty(b)
    if str(a).lower() in ("true", "false") or str(b).lower() in ("true", "false"):
        return binary(a) == binary(b) and binary(a) is not None
    if number(a) is not None and number(b) is not None:
        return math.isclose(number(a), number(b), rel_tol=1e-12, abs_tol=1e-12)
    return a == b


def parse_rows(rows):
    seen, global_seen, people = set(), set(), defaultdict(list)
    for raw in rows:
        r = dict(raw)
        for key in ("participant_id", "block_idx", "trial", "global_trial"):
            value = number(r[key])
            if value is None or value % 1 or value < (0 if key == "block_idx" else 1):
                raise ValueError(f"Invalid {key}: {r[key]}")
            r[key] = int(value)
        if empty(r["run_timestamp"]):
            raise ValueError("Missing run timestamp")
        identity = (r["participant_id"], r["run_timestamp"], r["block_idx"], r["trial"])
        # Main global_trial restarts at 1 after practice in these exports.
        global_key = (r["participant_id"], r["run_timestamp"], r["block_idx"] == 0, r["global_trial"])
        if identity in seen or global_key in global_seen:
            raise ValueError(f"Duplicate trial identity: {identity}")
        seen.add(identity)
        global_seen.add(global_key)
        r["session_trial"] = r["trial"] if r["block_idx"] == 0 else 60 + r["global_trial"]
        people[r["participant_id"]].append(r)
    for pid, rr in people.items():
        if len({r["run_timestamp"] for r in rr}) != 1:
            raise ValueError(f"Ambiguous multiple runs for participant {pid}")
        rr.sort(key=lambda r: (r["block_idx"], r["trial"]))
    return dict(sorted(people.items()))


def load_dataset(input_path, manifest_path):
    """Verify canonical/main provenance; add only exact-run practice evidence."""
    raw = read_csv(input_path, REQUIRED)
    canonical = parse_rows(raw)
    if not canonical or any(r["condition_code"] not in MAIN for r in raw):
        raise ValueError("Canonical input must contain main-task trials only")
    records = read_csv(manifest_path, ("participant_id", "run_timestamp", "export_type", "source_file", "source_md5"))
    selected, sources, practice_issues = {}, [], {}
    for rec in records:
        pid = int(rec["participant_id"])
        key = (pid, rec["export_type"])
        if key in selected or pid not in canonical or rec["run_timestamp"] != canonical[pid][0]["run_timestamp"]:
            raise ValueError("Manifest has duplicate, conflicting or unexpected participant/run identities")
        source = Path(rec["source_file"])
        if not source.is_absolute():
            source = Path(manifest_path).resolve().parent / source
        if not source.is_file() or hashlib.md5(source.read_bytes()).hexdigest() != rec["source_md5"]:
            raise ValueError(f"Manifest source missing or MD5 mismatch: {source}")
        rec = {**rec, "source_file": str(source.resolve()), "sha256": sha256(source), "status": "present"}
        selected[key] = rec
        sources.append(rec)
    if set(selected) != {(p, k) for p in canonical for k in ("trials", "questionnaire", "sliders")}:
        raise ValueError("Manifest must cover all three export families for every selected participant")
    all_rows = list(raw)
    for pid, main in canonical.items():
        timestamp = main[0]["run_timestamp"]
        source = Path(selected[pid, "trials"]["source_file"])
        expected_name = f"results_p{pid:03d}_{timestamp}_b00_ALL.csv"
        if source.name != expected_name:
            raise ValueError(f"Trial filename/run mismatch: {source}")
        source_rows = read_csv(source, REQUIRED)
        source_people = parse_rows(source_rows)
        if set(source_people) != {pid} or source_people[pid][0]["run_timestamp"] != timestamp:
            raise ValueError(f"Raw trial identity mismatch: {source}")
        a, b = main, source_people[pid]
        if len(a) != len(b):
            raise ValueError(f"Canonical/raw row count mismatch: {pid}")
        for left, right in zip(a, b):
            for key in raw[0]:
                if key not in right or not equivalent(left[key], right[key]):
                    raise ValueError(f"Canonical/raw mismatch: participant {pid}, trial {left['global_trial']}, {key}")
        issues = []
        practice = source.with_name(expected_name.replace("_ALL.csv", "_PRACTICE.csv"))
        delta = practice.with_name(practice.name.replace("results_", "delta_", 1))
        for kind, path in (("practice", practice), ("calibration_summary", delta)):
            sources.append(dict(participant_id=pid, run_timestamp=timestamp, export_type=kind,
                                source_file=str(path), source_md5=None,
                                sha256=sha256(path) if path.is_file() else None,
                                status="present" if path.is_file() else "missing"))
        if practice.is_file():
            rr = read_csv(practice, REQUIRED)
            if any(int(r["participant_id"]) != pid or r["run_timestamp"] != timestamp or r["condition_code"] != "PRACTICE" for r in rr):
                raise ValueError(f"Practice identity/run mismatch: {practice}")
            all_rows.extend(rr)
        else:
            issues.append("Missing exact-run practice export; no older-run fallback")
        if delta.is_file():
            dd = read_csv(delta, ("participant_id", "run_timestamp", "staircase_target_accuracy", "burnin_trials_excluded", "n_trials_summarised", "n_trials_total"))
            if len(dd) != 1 or int(dd[0]["participant_id"]) != pid or dd[0]["run_timestamp"] != timestamp:
                raise ValueError(f"Calibration-summary identity/run mismatch: {delta}")
            summary = dd[0]
            if any(number(summary[k]) != n for k, n in (("burnin_trials_excluded", 20), ("n_trials_summarised", 40), ("n_trials_total", 60))):
                issues.append("Calibration-summary window differs from 60 trials / 20 burn-in / final 40")
            practice_targets = {number(r["staircase_target_accuracy"]) for r in all_rows if int(r["participant_id"]) == pid and r["condition_code"] == "PRACTICE"}
            if practice_targets != {number(summary["staircase_target_accuracy"])} or number(summary["staircase_target_accuracy"]) is None:
                issues.append("Calibration target missing or inconsistent with summary")
        else:
            issues.append("Missing exact-run calibration summary")
        practice_issues[pid] = issues
    return parse_rows(all_rows), sources, practice_issues


def metadata(pid, rows):
    targets = {number(r["staircase_target_accuracy"]) for r in rows if r["condition_code"] == "PRACTICE"}
    target = next(iter(targets)) if len(targets) == 1 and None not in targets else None
    if target is not None and not 0 < target < 1:
        target = None
    blocks = task.build_blocks_for_participant(pid, task.BLOCKS)
    return dict(participant_id=pid, run_timestamp=rows[0]["run_timestamp"],
                calibration_target_accuracy=target, key_black=rows[0]["key_black"], key_white=rows[0]["key_white"],
                main_block_order="|".join(task.block_condition_code(b) for b in blocks))


def segments(meta):
    return [("PRACTICE", 0, 1, 60)] + [(code, i, 61+(i-1)*260, 60+i*260)
            for i, code in enumerate(meta["main_block_order"].split("|"), 1)]


def validate_person(pid, rows):
    meta = metadata(pid, rows)
    mapping = task.key_mapping_for_participant(pid)
    order = meta["main_block_order"].split("|")
    errors = defaultdict(set)
    for r in rows:
        t = (r["block_idx"], r["trial"])
        common = []
        code = r["condition_code"]
        practice = code == "PRACTICE"
        expected_block = 0 if practice else order.index(code)+1 if code in order else -1
        if r["block_idx"] != expected_block or not 1 <= r["trial"] <= (60 if practice else 260) or r["global_trial"] != (r["trial"] if practice else (expected_block-1)*260+r["trial"]):
            common.append("Block/condition/trial sequence mismatch")
        if any(r[k] != mapping[f"{k}_name"] for k in ("key_black", "key_white")) or binary(r["keymap_flip"]) != int(mapping["flip"]):
            common.append("Key mapping mismatch")
        if not empty(r["trial_deadline_s"]) or r["difficulty_mode"] != ("staircase" if practice else "fixed_delta"):
            common.append("Unexpected deadline or difficulty mode")
        aided = code in ("AIDFIRST", "STIMFIRST")
        if binary(r["auto_on"]) != int(aided) or r["aid_condition"] != {"PRACTICE":"manual", "MANUAL":"manual", "AIDFIRST":"aid_first", "STIMFIRST":"stimulus_first"}.get(code):
            common.append("Aid condition mismatch")
        if r["stimulus"] not in ("BLACK", "WHITE"):
            common.append("Invalid stimulus")
        if aided:
            if number(r["aid_accuracy_setting"]) != .85 or r["aid_label"] not in ("BLACK", "WHITE") or binary(r["aid_correct"]) != int(r["aid_label"] == r["stimulus"]):
                common.append("Advice identity, correctness or reliability mismatch")
        elif any(not empty(r[k]) for k in ("aid_label", "aid_correct", "aid_accuracy_setting")):
            common.append("Unexpected advice in unaided condition")
        for d, prefix in ((1, "preview"), (2, "decision2_preview")):
            real = code == ("AIDFIRST" if d == 1 else "STIMFIRST")
            if r[f"{prefix}_display"] != ("aid_only" if real else "masked_placeholder") or r[f"{prefix}_label"] != (r["aid_label"] if real else "#####"):
                common.append("Preview identity/timing mismatch")
        if binary(r["changed_response"]) != int(r["decision1_response"] != r["decision2_response"]):
            common.append("Decision-transition mismatch")
        for d in (1, 2):
            issues = list(common)
            response = r[f"decision{d}_response"]
            ok = int(response in ("BLACK", "WHITE") and response == r["stimulus"])
            if response not in ("BLACK", "WHITE", "TIMEOUT"):
                issues.append("Invalid response")
            if binary(r[f"decision{d}_correct"]) != ok and not (response == "TIMEOUT" and empty(r[f"decision{d}_correct"])):
                issues.append("Response correctness mismatch")
            if response != "TIMEOUT" and number(r[f"decision{d}_rt_s"]) is None:
                issues.append("Answered trial has missing/nonfinite RT")
            if response == "TIMEOUT" and not empty(r[f"decision{d}_rt_s"]) and number(r[f"decision{d}_rt_s"]) is None:
                issues.append("Invalid timeout RT")
            if r[f"decision{d}_display"] != ("stimulus_only" if d == 1 else "blank_response"):
                issues.append("Decision display mismatch")
            match = r[f"decision{d}_matches_aid"]
            if (aided and response != "TIMEOUT" and binary(match) != int(response == r["aid_label"])) or (not aided and not empty(match)):
                issues.append("Advice agreement mismatch")
            if d == 2 and r["feedback"] != ("TOO SLOW" if response == "TIMEOUT" else "CORRECT" if ok else "INCORRECT"):
                issues.append("Final feedback mismatch")
            errors[t, d].update(issues)
    return errors


def wilson(k, n, level=.95):
    if not n:
        return None, None
    z = statistics.NormalDist().inv_cdf((1+level)/2)
    p, scale = k/n, 1+z*z/n
    center = (p+z*z/(2*n))/scale
    half = z*math.sqrt(p*(1-p)/n+z*z/(4*n*n))/scale
    return max(0, center-half), min(1, center+half)


def metric(meta, selected, scope, decision, expected, cfg, errors, extra=()):
    issues = set(extra)
    actual = {(r["block_idx"], r["trial"]) for r in selected}
    if actual != expected:
        issues.add(f"Incomplete/invalid scope: expected {len(expected)} trials, found {len(selected)}")
    for r in selected:
        issues.update(errors[(r["block_idx"], r["trial"]), decision])
    answered = [r for r in selected if r[f"decision{decision}_response"] in ("BLACK", "WHITE")]
    timeout = [r for r in selected if r[f"decision{decision}_response"] == "TIMEOUT"]
    fast = [r for r in answered if number(r[f"decision{decision}_rt_s"]) is not None and number(r[f"decision{decision}_rt_s"]) < cfg["fast_rt_seconds"]]
    positive = [number(r[f"decision{decision}_rt_s"])*1000 for r in answered if number(r[f"decision{decision}_rt_s"]) is not None and number(r[f"decision{decision}_rt_s"]) > 0]
    k = sum(r[f"decision{decision}_response"] == r["stimulus"] for r in answered)
    fk = sum(r[f"decision{decision}_response"] == r["stimulus"] for r in fast)
    n, nf, valid = len(selected), len(fast), not issues
    lower, upper = wilson(k, n, cfg["interval_level"]) if valid else (None, None)
    fl, fu = wilson(fk, nf, cfg["interval_level"]) if valid else (None, None)
    m = dict(**meta, scope=scope, decision=decision, assessable=valid, integrity_issues="; ".join(sorted(issues)),
             n_trials=n, n_expected=len(expected), n_answered=len(answered), n_timeouts=len(timeout),
             n_correct=k if valid else None, accuracy=k/n if valid and n else None,
             accuracy_lower=lower, accuracy_upper=upper, answered_accuracy=k/len(answered) if valid and answered else None,
             timeout_fraction=len(timeout)/n if valid and n else None,
             n_fast=nf, n_fast_correct=fk if valid else None, fast_fraction=nf/n if valid and n else None,
             fast_accuracy=fk/nf if valid and nf else None, fast_accuracy_lower=fl, fast_accuracy_upper=fu,
             n_nonpositive_rt=sum(number(r[f"decision{decision}_rt_s"]) is not None and number(r[f"decision{decision}_rt_s"]) <= 0 for r in answered),
             n_timeouts_with_recorded_rt=sum(number(r[f"decision{decision}_rt_s"]) is not None for r in timeout),
             median_positive_rt_ms=statistics.median(positive) if positive and valid else None)
    for ms in cfg["sensitivity_thresholds_ms"]:
        m[f"fraction_rt_lt_{ms}ms"] = sum(number(r[f"decision{decision}_rt_s"]) is not None and number(r[f"decision{decision}_rt_s"]) < ms/1000 for r in answered)/n if valid and n else None
    return m


def screen(m, category, cfg):
    candidate = category in EXCLUSION_CATEGORIES and m["scope"] not in ("PRACTICE", "calibration_tail")
    valid = m["assessable"]
    difference = None
    if category in ("calibration_target", "manual_target"):
        valid = valid and m["calibration_target_accuracy"] is not None
        difference = 100*(m["accuracy"]-m["calibration_target_accuracy"]) if valid else None
        flag = valid and abs(difference) > cfg[f"{category}_band_pp"] + 1e-10
    elif category == "near_chance_or_worse":
        flag = valid and m["accuracy"] <= cfg["chance_accuracy_max"] + 1e-12
    elif category == "anticipatory":
        flag = valid and m["n_fast"] >= cfg["fast_count_min"] and m["fast_fraction"] >= cfg["fast_fraction_min"]-1e-12 and m["fast_accuracy"] is not None and m["fast_accuracy"] <= cfg["fast_accuracy_max"]+1e-12
    else:
        raise ValueError(f"Unknown category: {category}")
    return {**{k:m[k] for k in (*META, "scope", "decision", "n_trials", "accuracy", "n_fast", "fast_fraction", "fast_accuracy")},
            "category":category, "status":"not_assessable" if not valid else "flag" if flag else "clear",
            "use_for_exclusion":candidate, "target_difference_pp":difference,
            "integrity_issues":m["integrity_issues"] or ("Missing recorded calibration target" if not valid else "")}


def analyse(people, cfg, practice_issues=None):
    result = dict(summaries=[], metrics=[], checks=[], bins=[], issues=[])
    for pid, rows in people.items():
        meta, errors = metadata(pid, rows), validate_person(pid, rows)
        segment_map = {c:(idx, lo, hi) for c, idx, lo, hi in segments(meta)}
        checks = []
        for scope in SCOPES:
            codes = ("AIDFIRST", "STIMFIRST") if scope == "aided_overall" else MAIN if scope == "experimental_overall" else ("PRACTICE",) if scope == "calibration_tail" else (scope,)
            expected = {(segment_map[c][0], t) for c in codes for t in range(21 if scope == "calibration_tail" else 1, (60 if c == "PRACTICE" else 260)+1)}
            selected = [r for r in rows if r["condition_code"] in codes and (scope != "calibration_tail" or r["trial"] >= 21)]
            for d in ((2,) if scope == "calibration_tail" else (1, 2)):
                extra = (practice_issues or {}).get(pid, []) if scope in ("PRACTICE", "calibration_tail") else []
                m = metric(meta, selected, scope, d, expected, cfg, errors, extra)
                result["metrics"].append(m)
                if not m["assessable"]:
                    result["issues"].append(m)
                cats = ("calibration_target",) if scope == "calibration_tail" else EXCLUSION_CATEGORIES
                if scope == "MANUAL" and d == 2:
                    cats = (*cats, "manual_target")
                checks.extend(screen(m, c, cfg) for c in cats)
        for code, idx, lo, hi in segments(meta):
            size = cfg["practice_bin_size" if code == "PRACTICE" else "main_bin_size"]
            count = hi-lo+1
            for start in range(1, count+1, size):
                end = min(count, start+size-1)
                expected = {(idx,t) for t in range(start,end+1)}
                selected = [r for r in rows if r["condition_code"] == code and start <= r["trial"] <= end]
                for d in (1, 2):
                    b = metric(meta, selected, code, d, expected, cfg, errors, (practice_issues or {}).get(pid, []) if code == "PRACTICE" else [])
                    b.update(trial_start=start, trial_end=end, condition_trial_midpoint=(start+end)/2, trial_midpoint=lo-1+(start+end)/2)
                    result["bins"].append(b)
        flagged = [c for c in checks if c["status"] == "flag"]
        candidate = [c for c in flagged if c["use_for_exclusion"]]
        diagnostic = [c for c in flagged if not c["use_for_exclusion"]]
        summary = dict(**meta, n_trials=len(rows), n_main_trials=sum(r["condition_code"] in MAIN for r in rows),
                       n_not_assessable=sum(c["status"] == "not_assessable" for c in checks),
                       exclude_by_rules=bool(candidate), n_exclusion_rules=len({c["category"] for c in candidate}),
                       n_flagged_rules=len({c["category"] for c in flagged}), diagnostic_only=bool(diagnostic) and not candidate,
                       review_required=bool(flagged) or any(c["status"] == "not_assessable" for c in checks),
                       exclusion_reasons="; ".join(f"{c['category']}:{c['scope']}:D{c['decision']}" for c in candidate),
                       diagnostic_reasons="; ".join(f"{c['category']}:{c['scope']}:D{c['decision']}" for c in diagnostic))
        for c in CATEGORIES:
            cc = [x for x in checks if x["category"] == c and (x["use_for_exclusion"] if c in EXCLUSION_CATEGORIES else True)]
            summary[c] = "flag" if any(x["status"] == "flag" for x in cc) else "not_assessable" if any(x["status"] == "not_assessable" for x in cc) else "clear"
        result["summaries"].append(summary)
        result["checks"].extend(checks)
    return result


def exclusion_summary(result):
    ss = result["summaries"]
    ids = [s["participant_id"] for s in ss if s["exclude_by_rules"]]
    return dict(participant_n=len(ss), unique_flagged_n=len(ids), percentage=100*len(ids)/len(ss),
                participant_ids=ids, remaining_if_applied_n=len(ss)-len(ids), exclusions_applied=False,
                rule_counts={c:sum(s[c] == "flag" for s in ss) for c in EXCLUSION_CATEGORIES},
                rule_participant_ids={c:[s["participant_id"] for s in ss if s[c] == "flag"] for c in EXCLUSION_CATEGORIES},
                overlap_counts=dict(sorted(Counter(s["n_exclusion_rules"] for s in ss if s["exclude_by_rules"]).items())),
                diagnostic_only_participant_ids=[s["participant_id"] for s in ss if s["diagnostic_only"]],
                incomplete_assessment_ids=[s["participant_id"] for s in ss if s["n_not_assessable"]])


def choose_comparators(result):
    ss = result["summaries"]
    overall = {(m["participant_id"], m["decision"]):m for m in result["metrics"] if m["scope"] == "experimental_overall"}
    groups = defaultdict(list)
    for s in ss:
        if s["anticipatory"] == "flag":
            groups[(s["calibration_target_accuracy"], s["key_black"], s["key_white"])].append(s["participant_id"])
    output = []
    for (target, black, white), ids in sorted(groups.items(), key=lambda x:str(x[0])):
        pool = [s["participant_id"] for s in ss if target is not None and s["calibration_target_accuracy"] == target and s["key_black"] == black and s["key_white"] == white and s["anticipatory"] == "clear" and not s["n_not_assessable"] and all(overall[s["participant_id"],d]["median_positive_rt_ms"] is not None for d in (1,2))]
        values = {p:{f"D{d}_{k}":overall[p,d][k] for d in (1,2) for k in ("accuracy", "median_positive_rt_ms")} for p in pool}
        centers = {k:statistics.median(values[p][k] for p in pool) for k in next(iter(values.values()), {})}
        scales = {k:statistics.median(abs(values[p][k]-centers[k]) for p in pool) for k in centers}
        ranking = sorted([dict(participant_id=p, score=sum(abs(values[p][k]-centers[k])/scales[k] for k in centers if scales[k] > 1e-12)) for p in pool], key=lambda r:(r["score"],r["participant_id"]))
        output.append(dict(calibration_target_accuracy=target, key_black=black, key_white=white, targets=ids,
                           comparator=ranking[0]["participant_id"] if ranking else None, ranked_candidates=ranking,
                           centers=centers, median_absolute_deviations=scales,
                           note="Zero-MAD terms contribute zero; ties use participant ID." if pool else "No complete matching comparator available."))
    return output
