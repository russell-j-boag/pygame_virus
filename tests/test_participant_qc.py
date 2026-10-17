"""Boundary and integrity tests; synthetic trials never modify study data."""
import copy
import csv
import json
import random
import tempfile
import unittest
from pathlib import Path

from participant_qc.core import (CATEGORIES, REQUIRED, SEGMENTS, analyse,
    choose_comparators, exclusion_summary, load_config, load_trials, scope_metrics, screen,
    validate_person, wilson)
from participant_qc.run_qc import check_destination

ROOT = Path(__file__).resolve().parents[1]


def fixture(pid=1):
    group, target = ("CAL65", .65) if pid % 2 else ("CAL90", .9)
    flipped = ((pid-1)//2) % 2 == 1
    rows = []
    for code, label, lo, hi, b, block, condition, phase in SEGMENTS:
        for t in range(lo, hi+1):
            stimulus = "BLACK" if t % 2 else "WHITE"
            other = "WHITE" if stimulus == "BLACK" else "BLACK"
            correct = (t-1) % 20 < round(target*20)
            aided = b == 3
            p = (t-501)//400+1 if aided else None
            reliable = .7 if p == 2 else .95
            advice_correct = (t-lo) % 20 < round(20*reliable)
            row = dict.fromkeys(REQUIRED, "")
            row.update(participant_id=pid, run_timestamp="20260101_120000", block=block, block_idx=b,
                trial=t-{1:0,2:300,3:500,4:1700}[b], global_trial=t,
                calibration_target_group=group, calibration_target_accuracy=target,
                main_block_order="SPLIT_MANUAL", manual_segment=phase if b in (2,4) else "",
                reliability_phase_label=phase if aided else "", reliability_phase_idx=p if aided else "",
                trial_in_reliability_phase=t-lo+1 if aided else "", aid_reliability_level=reliable if aided else "",
                automation_reliability_group=("low" if p == 2 else "high") if aided else "",
                condition_code=condition, key_black="J" if flipped else "D", key_white="D" if flipped else "J",
                trial_deadline_s=5, stimulus=stimulus, response=stimulus if correct else other,
                correct="TRUE" if correct else "FALSE", feedback="CORRECT" if correct else "INCORRECT", rt_s=.6,
                aid_label=(stimulus if advice_correct else other) if aided else "", aid_correct=str(advice_correct) if aided else "")
            rows.append(row)
    return rows


def parsed(rows):
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp)/"input.csv"
        with path.open("w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=sorted(REQUIRED)); w.writeheader(); w.writerows(rows)
        return load_trials(path)


def set_correct(row, correct):
    row["response"] = row["stimulus"] if correct else ("WHITE" if row["stimulus"] == "BLACK" else "BLACK")
    row["correct"] = "TRUE" if correct else "FALSE"
    row["feedback"] = "CORRECT" if correct else "INCORRECT"


class ParticipantQCTests(unittest.TestCase):
    def setUp(self):
        self.cfg = load_config(ROOT/"participant_qc/config.json")

    def metric(self, raw, scope="manual_pre", lo=301, hi=500):
        rows = next(iter(parsed(raw).values()))
        errors, global_errors = validate_person(rows)
        return scope_metrics(rows, scope, scope, lo, hi, self.cfg, errors, global_errors)

    def test_complete_both_groups_and_schema(self):
        result = analyse(parsed(fixture(1)+fixture(2)), self.cfg)
        self.assertEqual(len(result["metrics"]), 18)
        self.assertEqual(len(result["checks"]), 38)
        self.assertEqual(len(result["bins"]), 76)
        self.assertFalse(result["issues"])
        self.assertTrue(all(c["status"] == "clear" for c in result["checks"]))

    def test_target_band_strict_and_chance_inclusive(self):
        raw = fixture()
        for i, row in enumerate(raw[300:500]): set_correct(row, i < 150)
        m = self.metric(raw)
        self.assertEqual(screen(m, "manual_target", self.cfg)["status"], "clear")
        set_correct(raw[450], True)
        self.assertEqual(screen(self.metric(raw), "manual_target", self.cfg)["status"], "flag")
        for i, row in enumerate(raw[300:500]): set_correct(row, i < 110)
        self.assertEqual(screen(self.metric(raw), "near_chance_or_worse", self.cfg)["status"], "flag")
        set_correct(raw[410], True)
        self.assertEqual(screen(self.metric(raw), "near_chance_or_worse", self.cfg)["status"], "clear")

    def test_calibration_uses_final_window(self):
        raw = fixture()
        for row in raw[:150]: set_correct(row, False)
        result = analyse(parsed(raw), self.cfg)
        check = next(c for c in result["checks"] if c["category"] == "calibration_target")
        self.assertEqual(check["status"], "clear")
        for row in raw[150:300]: set_correct(row, False)
        result = analyse(parsed(raw), self.cfg)
        self.assertEqual(next(c for c in result["checks"] if c["category"] == "calibration_target")["status"], "flag")

    def test_aid_ordering_does_not_trigger_flags(self):
        for pid, count in [(1,140),(2,190)]:
            raw = fixture(pid)
            for i,row in enumerate(raw[300:500]): set_correct(row, i < count)
            result = analyse(parsed(raw),self.cfg)
            self.assertTrue(all(c["status"] == "clear" for c in result["checks"]))
            self.assertNotIn("manual_aid_ordering",result["summaries"][0])
            with self.assertRaisesRegex(ValueError,"Unknown screening category"):
                screen(self.metric(raw),"manual_aid_ordering",self.cfg)

    def test_unique_exclusions_deduplicate_rules_and_scopes(self):
        first = fixture(1)
        for lo,hi in [(300,500),(1700,1900)]:
            for i,row in enumerate(first[lo:hi]): set_correct(row,i % 2 == 0)
        for row in first[300:340]: row["rt_s"] = .1
        second = fixture(5)
        for i,row in enumerate(second[300:340]):
            row["rt_s"] = .1
            set_correct(row,i % 2 == 0)
        incomplete = fixture(13); incomplete.pop(599)
        result = analyse(parsed(first+second+fixture(9)+incomplete),self.cfg)
        summary = exclusion_summary(result)
        self.assertEqual(summary["participant_ids"],[1,5])
        self.assertEqual(summary["unique_flagged_n"],2)
        self.assertEqual(summary["unique_flagged_percent"],50)
        self.assertEqual(summary["remaining_if_applied_n"],2)
        self.assertEqual(summary["incomplete_assessment_ids"],[13])
        self.assertEqual([r["participant_n"] for r in summary["overlap"]],[1,1,0])
        self.assertEqual(result["summaries"][0]["n_flagged_rules"],3)
        self.assertEqual(result["summaries"][0]["n_exclusion_rules"],2)
        self.assertGreater(result["summaries"][0]["n_flags"],3)
        self.assertFalse(summary["exclusions_applied"])
        clean = exclusion_summary(analyse(parsed(fixture()),self.cfg))
        self.assertEqual(clean["participant_ids"],[])
        self.assertEqual(clean["remaining_if_applied_n"],1)

    def test_manual_target_flags_are_diagnostic_only(self):
        raw = fixture()
        for lo,hi in [(300,500),(1700,1900)]:
            for i,row in enumerate(raw[lo:hi]): set_correct(row,i < 160)
        result = analyse(parsed(raw),self.cfg)
        person = result["summaries"][0]
        self.assertEqual(person["manual_target"],"flag")
        self.assertTrue(person["flagged_any_rule"])
        self.assertTrue(person["review_required"])
        self.assertEqual(person["n_flags"],2)
        self.assertEqual(person["n_flagged_rules"],1)
        self.assertEqual(person["n_exclusion_rules"],0)
        self.assertFalse(person["exclude_by_rules"])
        self.assertEqual(person["exclusion_reasons"],"")
        self.assertTrue(all(not c["use_for_exclusion"] for c in result["checks"] if c["status"]=="flag"))
        summary = exclusion_summary(result)
        self.assertEqual(summary["participant_ids"],[])
        self.assertEqual(summary["remaining_if_applied_n"],1)
        self.assertEqual(summary["diagnostic_rule_counts"],{"manual_target":1})
        self.assertEqual(summary["diagnostic_only_participant_ids"],[1])

    def test_anticipatory_joint_rule_and_rt_boundary(self):
        raw = fixture()
        for i,row in enumerate(raw[300:340]):
            row["rt_s"] = .1; set_correct(row, i < 22)
        raw[300]["rt_s"] = -.001; raw[301]["rt_s"] = 0
        raw[340]["rt_s"] = .2
        m = self.metric(raw)
        self.assertEqual((m["n_fast"], m["n_nonpositive_rt"], m["n_positive_fast"]), (40,2,38))
        self.assertAlmostEqual(m["fast_fraction"], .2)
        self.assertEqual(screen(m,"anticipatory",self.cfg)["status"],"flag")
        set_correct(raw[322],True)
        self.assertEqual(screen(self.metric(raw),"anticipatory",self.cfg)["status"],"clear")
        set_correct(raw[322],False); raw[339]["rt_s"] = .2
        self.assertEqual(screen(self.metric(raw),"anticipatory",self.cfg)["status"],"clear")
        cfg = {**self.cfg,"fast_fraction_min":.05}
        m = {**m,"n_fast":19,"fast_fraction":.2,"fast_accuracy":.5}
        self.assertEqual(screen(m,"anticipatory",cfg)["status"],"clear")

    def test_timeouts_empty_fast_and_missing_rt(self):
        raw = fixture()
        raw[300].update(response="TIMEOUT",correct="FALSE",feedback="TOO SLOW",rt_s="NA")
        m = self.metric(raw)
        self.assertTrue(m["assessable"]); self.assertEqual(m["n_timeouts"],1)
        self.assertIsNone(m["fast_accuracy"]); self.assertIsNone(m["fast_accuracy_lower"])
        self.assertEqual(screen(m,"anticipatory",self.cfg)["status"],"clear")
        raw[301]["rt_s"] = "NA"
        m = self.metric(raw)
        self.assertFalse(m["assessable"])
        self.assertEqual(screen(m,"anticipatory",self.cfg)["status"],"not_assessable")

    def test_missing_trial_and_metadata_errors_are_local(self):
        raw = fixture(); raw.pop(599)
        result = analyse(parsed(raw),self.cfg)
        scopes = {m["scope"]:m for m in result["metrics"]}
        self.assertFalse(scopes["P1_95"]["assessable"])
        self.assertFalse(scopes["experimental_overall"]["assessable"])
        self.assertTrue(scopes["manual_pre"]["assessable"])
        raw = fixture(); raw[501]["reliability_phase_label"] = "P2_70"
        self.assertTrue(any("Reliability" in x["integrity_issues"] for x in analyse(parsed(raw),self.cfg)["issues"]))

    def test_duplicate_and_multiple_run_rejected(self):
        raw=fixture()
        with self.assertRaisesRegex(ValueError,"Duplicate"): parsed(raw+[raw[0]])
        raw[-1]["run_timestamp"] = "20260102_120000"
        with self.assertRaisesRegex(ValueError,"multiple runs"): parsed(raw)

    def test_missing_schema_and_empty_input_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp)/"input.csv"
            path.write_text("participant_id,global_trial\n1,1\n")
            with self.assertRaisesRegex(ValueError,"Missing required trial columns"):
                load_trials(path)
            path.write_text(",".join(sorted(REQUIRED))+"\n")
            with self.assertRaisesRegex(ValueError,"No trial records"):
                load_trials(path)

    def test_destinations_protect_existing_work(self):
        source = ROOT/"data/data_virus_all.csv"
        for destination in (ROOT, ROOT/"data", ROOT/"plots/qc", ROOT/"participant_qc",
                            ROOT/"analysis_outputs/semester2_2026_behavioural/qc"):
            with self.assertRaises(ValueError): check_destination(source,destination)
        with tempfile.TemporaryDirectory() as tmp:
            destination = Path(tmp)/"qc"
            destination.mkdir()
            check_destination(source,destination)
            (destination/"existing.txt").write_text("Preserve this file")
            with self.assertRaisesRegex(ValueError,"not owned"):
                check_destination(source,destination)
            self.assertEqual((destination/"existing.txt").read_text(),"Preserve this file")
            (destination/"manifest.json").write_text(json.dumps({"generator":"participant_qc"}))
            check_destination(source,destination)

    def test_results_are_repeatable_independent_of_csv_order(self):
        raw = fixture(1)+fixture(5)
        for i,row in enumerate(raw[300:340]):
            row["rt_s"] = .1
            set_correct(row,i < 20)
        baseline = analyse(parsed(raw),self.cfg)
        shuffled = copy.deepcopy(raw)
        random.Random(42).shuffle(shuffled)
        repeated = analyse(parsed(shuffled),self.cfg)
        self.assertEqual(baseline,repeated)
        self.assertEqual(choose_comparators(baseline),choose_comparators(repeated))

    def test_wilson_reference_and_empty(self):
        self.assertEqual(wilson(0,0),(None,None))
        low,high=wilson(10,20)
        self.assertAlmostEqual(low,.2992980081982123,places=12)
        self.assertAlmostEqual(high,.7007019918017877,places=12)
        self.assertAlmostEqual(wilson(0,20)[1],.16112515805281938,places=12)

    def test_comparator_ties_empty_and_flagged_exclusion(self):
        result=analyse(parsed(fixture(1)+fixture(5)+fixture(9)),self.cfg)
        result["summaries"][0]["anticipatory"]="flag"
        matches=choose_comparators(result)
        self.assertEqual(matches[0]["comparator"],5)
        self.assertEqual(matches[0]["ranked_candidates"][0]["score"],0)
        for s in result["summaries"]: s["anticipatory"]="flag"
        self.assertIsNone(choose_comparators(result)[0]["comparator"])
        for s in result["summaries"]: s["anticipatory"]="clear"
        self.assertEqual(choose_comparators(result),[])


if __name__ == "__main__":
    unittest.main()
