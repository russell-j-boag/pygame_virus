"""Timestamp invariants and task integration; no native input or study writes."""

from contextlib import contextmanager, ExitStack
import csv
from collections import Counter
import hashlib
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import types
import unittest
from unittest.mock import patch

os.environ.setdefault("PYGAME_HIDE_SUPPORT_PROMPT", "1")
ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "python"))
import numpy as np
import pygame
import response_timing as rt
import virus_task as task

KEYMAP = {"key_black_name": "D", "key_white_name": "J"}


def event(key, time, pressed=True, lag=.05):
    return rt.KeyEvent(key, pressed, time, time + lag)


def window(held=(), duration=5.0):
    w = rt.ResponseWindow(99, held, KEYMAP, rt.PTB_VERSION)
    w.mark_onset(100, duration)
    return w


class TimestampTests(unittest.TestCase):
    def test_original_failure_early_press_is_not_relabelled_as_positive_rt(self):
        w = window()
        w.consume([event("D", 99.990), event("D", 99.995, False), event("J", 100.0005)])
        self.assertEqual(w.response, "WHITE")
        self.assertAlmostEqual(w.result()["rt_s"], .0005, places=12)
        self.assertEqual(w.result()["n_ignored_pre_onset_keydowns"], 1)

    def test_held_key_must_be_released_other_key_is_available(self):
        w = window(held={"D"})
        w.consume([event("D", 100.1), event("J", 100.2)])
        self.assertEqual(w.response, "WHITE")
        w = window(held={"D"})
        w.consume([event("D", 100.1), event("D", 100.2, False), event("D", 100.3)])
        self.assertAlmostEqual(w.result()["rt_s"], .3)

    def test_early_press_held_across_onset_requires_release(self):
        w = window()
        w.consume([event("D", 99.9), event("D", 100.1)])
        self.assertIsNone(w.response)
        w.consume([event("D", 100.2, False), event("D", 100.25)])
        self.assertAlmostEqual(w.result()["rt_s"], .25)

    def test_first_press_by_time_and_stable_equal_time_tie(self):
        w = window()
        w.consume([event("J", 100.2), event("D", 100.1)])
        self.assertEqual(w.response, "BLACK")
        w = window()
        w.consume([event("J", 100.1), event("D", 100.1)])
        self.assertEqual(w.response, "WHITE")
        w.expire(110)
        self.assertEqual(w.response, "WHITE")

    def test_no_floor_and_half_millisecond_difference_survives_csv(self):
        results = []
        for delta in (.000001, .150123456789, .150623456789):
            w = window()
            w.consume([event("D", 100 + delta)])
            results.append(w.result())
        with tempfile.TemporaryDirectory() as td:
            path = Path(td) / "trial.csv"
            task.write_csv_rows(path, results)
            with path.open() as f:
                restored = list(csv.DictReader(f))
        for before, after in zip(results, restored):
            self.assertEqual(before["rt_s"], float(after["rt_s"]))
            self.assertEqual(before["response_timestamp_s"], float(after["response_timestamp_s"]))
        self.assertAlmostEqual(float(restored[2]["rt_s"]) - float(restored[1]["rt_s"]), .0005, places=12)

    def test_rendering_delays_do_not_change_rt(self):
        for lag in (.008, .020, .050):
            with self.subTest(lag=lag):
                w = window()
                w.consume([event("D", 100.1234567, lag=lag)])
                self.assertAlmostEqual(w.result()["rt_s"], .1234567, places=12)
                self.assertAlmostEqual(w.result()["response_readout_delay_s"], lag, places=12)

    def test_deadline_uses_event_time_not_processing_time(self):
        for delta, expected in ((4.999999, "BLACK"), (5., "TIMEOUT"), (5.000001, "TIMEOUT")):
            w = window()
            w.consume([event("D", 100 + delta, lag=.05)])
            w.expire(105.1)
            self.assertEqual(w.response, expected)
            if expected == "TIMEOUT":
                self.assertIsNone(w.result()["rt_s"])
                self.assertIsNone(w.result()["response_timestamp_s"])
                self.assertIsNone(w.result()["response_readout_delay_s"])

    def test_onset_exactly_is_eligible_and_no_deadline_works(self):
        w = window(duration=None)
        w.expire(10000)
        self.assertIsNone(w.response)
        w.consume([event("D", 100)])
        self.assertEqual(w.result()["rt_s"], 0)

    def test_invalid_clock_or_event_order_fails(self):
        for e in (event("D", float("nan")), rt.KeyEvent("D", True, 101, 100)):
            with self.assertRaises(rt.TimingError):
                window().consume([e])
        w = window()
        w.consume([event("D", 99.9)])
        with self.assertRaises(rt.TimingError):
            w.consume([event("D", 99.8)])

    def test_snapshot_events_and_repeated_downs_are_ignored(self):
        w = window(held={"D"})
        w.consume([event("D", 98.9), event("D", 99), event("D", 100.1)])
        self.assertIsNone(w.response)
        self.assertEqual(w.ignored_early, 0)
        w.consume([event("D", 100.2, False), event("D", 100.3), event("D", 100.4)])
        self.assertAlmostEqual(w.result()["rt_s"], .3)

    def test_hp_lp_deadline_boundaries(self):
        for duration in (1., 3.):
            for offset in (-.000001, 0., .000001):
                with self.subTest(duration=duration, offset=offset):
                    w = window(duration=duration)
                    w.consume([event("D", 100 + duration + offset, lag=.05)])
                    w.expire(100 + duration + .1)
                    self.assertEqual(w.response, "BLACK" if offset < 0 else "TIMEOUT")

    def test_onset_boundaries(self):
        for offset in (-.000001, 0., .000001):
            w = window()
            w.consume([event("D", 100 + offset)])
            if offset < 0:
                self.assertIsNone(w.response)
                self.assertEqual(w.ignored_early, 1)
            else:
                self.assertAlmostEqual(w.result()["rt_s"], offset, places=12)


class FakeTiming:
    """Events happen on an independent clock while drawing/tick advances time."""
    backend_version = rt.PTB_VERSION

    def __init__(self, events=(), stall=0, held=()):
        self.t = 1000.
        self.events = list(events)
        self.stall = stall
        self.held = held
        self.active = False
        self.closed = False
        self.frames = []
        self.flips = []

    def now(self):
        return self.t

    @contextmanager
    def trial(self, keymap):
        self.active = True
        try:
            yield rt.ResponseWindow(self.t, self.held, keymap, self.backend_version)
        finally:
            self.active = False
            self.closed = True

    def drain(self):
        ready = [e for e in self.events if e.timestamp_s <= self.t]
        self.events = [e for e in self.events if e.timestamp_s > self.t]
        return [rt.KeyEvent(e.key, e.pressed, e.timestamp_s, self.t) for e in ready]

    def tick(self, fps):
        self.t += 1 / fps + self.stall

    def draw(self, screen, layer, dots, center, payload, ui, ms_left=None):
        self.frames.append((dict(payload), ms_left, self.t))
        self.t += .004

    def flip(self):
        self.t += .003
        self.flips.append(self.t)


class TaskIntegrationTests(unittest.TestCase):
    def run_trial(self, timing, pid=1, block_idx=2, trial_number=1, cfg_changes=None, pygame_events=None, aid_onset_ms=0, transparency="none",
                  run_timestamp="20260101_120000"):
        cfg = task.build_blocks_for_participant(pid, task.BLOCKS)[block_idx-1]
        cfg.update(participant_id=pid, block_idx=block_idx)
        cfg.update(cfg_changes or {})
        state = dict(difficulty_mode="staircase" if block_idx == 1 else "fixed_delta",
                     fixed_delta_mean=.1, fixed_delta_sd=.01, aid_transparency=transparency, delta_mean=.2)
        def feedback(*args):
            self.assertFalse(timing.active, "Stop acquisition before waiting for feedback.")
            return "TOO SLOW" if args[3] == "TIMEOUT" else "CORRECT"
        with ExitStack() as stack:
            stack.enter_context(patch.object(task, "DISH_RADIUS", 200, create=True))
            stack.enter_context(patch.object(task, "AID_ONSET_MS", aid_onset_ms))
            for name, value in (
                ("pick_trial_vblack_prop", lambda *a: (.6, .1)),
                ("make_trial_dots", lambda *a: ([], 60, 40)),
                ("update_dots", lambda *a: None),
                ("draw_trial_frame", timing.draw),
                ("make_aid_recommendation", lambda *a, **k: ("BLACK", True)),
                ("maybe_show_feedback", feedback),
            ):
                stack.enter_context(patch.object(task, name, value))
            stack.enter_context(patch.object(pygame.display, "flip", timing.flip))
            stack.enter_context(patch.object(pygame.event, "get", side_effect=pygame_events or (lambda: [])))
            stack.enter_context(patch.object(task, "quit_clean", side_effect=SystemExit))
            row = task.run_single_trial(None, timing, None, (0, 0), {}, task.key_mapping_for_participant(pid),
                                       cfg, state, 10, trial_number, 301, run_timestamp, timing)
        return row, state

    def test_bug_reproduction_and_post_flip_press_is_preserved(self):
        timing = FakeTiming([event("D", 1000.001), event("D", 1000.002, False), event("J", 1000.0075)])
        row, _ = self.run_trial(timing)
        self.assertAlmostEqual(row["stimulus_onset_s"], 1000.007, places=12)
        self.assertAlmostEqual(row["rt_s"], .0005, places=12)
        self.assertEqual(row["n_ignored_pre_onset_keydowns"], 1)
        self.assertEqual(row["response"], "WHITE")

    def test_real_trial_loop_does_not_use_pygame_key_or_frame_timestamp(self):
        for stall in (.008, .02, .05):
            with self.subTest(stall=stall):
                timing = FakeTiming([event("D", 1000.157123456)], stall=stall)
                row, _ = self.run_trial(timing, pygame_events=lambda: [pygame.event.Event(pygame.KEYDOWN, key=pygame.K_j, mod=0)])
                self.assertEqual(row["response"], "BLACK")
                self.assertAlmostEqual(row["rt_s"], .150123456, places=11)
                self.assertGreaterEqual(row["response_readout_delay_s"], 0)

    def test_both_mappings_patterns_all_five_conditions_and_transparency(self):
        for pid in (1, 2, 3, 4):
            for block_idx in range(1, 6):
                with self.subTest(pid=pid, block=block_idx):
                    cfg = task.build_blocks_for_participant(pid, task.BLOCKS)[block_idx-1]
                    key = task.key_mapping_for_participant(pid)["key_black_name"]
                    timing = FakeTiming([event(key, 1000.1)])
                    row, state = self.run_trial(timing, pid, block_idx, transparency="high")
                    self.assertTrue(row["correct"])
                    self.assertEqual(row["condition_deadline_code"], cfg["CONDITION_DEADLINE_CODE"])
                    self.assertEqual(row["time_pressure_condition"], cfg["TIME_PRESSURE_CONDITION"])
                    self.assertEqual(row["trial_deadline_s"], 1 if cfg["TIME_PRESSURE_CONDITION"] == "HP" else 3)
                    self.assertEqual(row["automation_reliability_pattern"], "HP95_LP65" if pid <= 2 else "HP65_LP95")
                    self.assertEqual(row["automation_reliability_group"], cfg["AUTOMATION_RELIABILITY_GROUP"])
                    self.assertEqual(row["keymap_flip"], pid % 2 == 0)
                    self.assertEqual(row["auto_on"], int(cfg["AUTOMATION_ON"]))
                    if cfg["AUTOMATION_ON"]:
                        self.assertEqual(row["aid_accuracy_setting"], cfg["AID_ACCURACY"])
                        self.assertEqual(row["aid_transparency_level"], "high")
                        self.assertEqual(timing.frames[0][0]["transparency_level"], "high")
                        self.assertTrue(timing.frames[0][0]["visible"])
                        self.assertEqual(row["aid_onset_ms_rel"], 0)
                    else:
                        self.assertIsNone(row["aid_onset_ms_rel"])
                    if block_idx == 1:
                        self.assertLess(state["delta_mean"], .2)

    def test_timeout_and_pre_deadline_response_retrieved_after_deadline(self):
        row, _ = self.run_trial(FakeTiming(stall=.05), cfg_changes={"TRIAL_DEADLINE_MS": 25})
        self.assertEqual(row["response"], "TIMEOUT")
        self.assertIsNone(row["correct"])
        self.assertIsNone(row["rt_s"])
        timing = FakeTiming([event("D", 1000.0319)], stall=.05)
        row, _ = self.run_trial(timing, cfg_changes={"TRIAL_DEADLINE_MS": 25})
        self.assertEqual(row["response"], "BLACK")
        self.assertAlmostEqual(row["rt_s"], .0249, places=11)

    def test_initial_render_time_does_not_consume_deadline(self):
        timing = FakeTiming([event("D", 1000.040)])
        original_draw = timing.draw
        def slow_first(*a, **k):
            if not timing.frames:
                timing.t += .030
            return original_draw(*a, **k)
        timing.draw = slow_first
        row, _ = self.run_trial(timing, cfg_changes={"TRIAL_DEADLINE_MS": 25})
        self.assertEqual(row["response"], "BLACK")
        self.assertAlmostEqual(row["rt_s"], .003, places=11)

    def test_hp_lp_deadlines_in_real_trial_loop(self):
        for duration, block_idx in ((1., 2), (3., 5)):
            for offset in (-.000001, 0., .000001):
                with self.subTest(duration=duration, offset=offset):
                    # A binary-exact first flip makes equality at the deadline
                    # independent of accumulated fake draw/flip float additions.
                    onset = 1000.125
                    timing = FakeTiming([event("D", onset + duration + offset)], stall=.05)
                    original_flip = timing.flip
                    def flip():
                        original_flip()
                        if len(timing.flips) == 1:
                            timing.t = onset
                            timing.flips[-1] = onset
                    timing.flip = flip
                    row, _ = self.run_trial(timing, block_idx=block_idx)
                    self.assertEqual(row["stimulus_onset_s"], onset)
                    self.assertEqual(row["trial_deadline_s"], duration)
                    self.assertEqual(row["response"], "BLACK" if offset < 0 else "TIMEOUT")
                    self.assertGreater(timing.now(), row["stimulus_onset_s"] + duration)
                    if offset < 0:
                        self.assertAlmostEqual(row["rt_s"], duration + offset, places=11)
                    else:
                        for field in ("rt_s", "response_timestamp_s", "response_readout_delay_s", "correct"):
                            self.assertIsNone(row[field])

    def test_queue_is_active_during_preparation_and_first_flip(self):
        timing = FakeTiming([event("D", 1000.15)])
        original_draw = timing.draw
        def slow_first(*args, **kwargs):
            self.assertTrue(timing.active)
            if not timing.frames:
                timing.t += .1
                timing.events[:0] = [event("J", 1000.05), event("J", 1000.06, False)]
            return original_draw(*args, **kwargs)
        timing.draw = slow_first
        row, _ = self.run_trial(timing)
        self.assertAlmostEqual(row["stimulus_onset_s"], 1000.107, places=11)
        self.assertAlmostEqual(row["rt_s"], .043, places=11)
        self.assertEqual(row["n_ignored_pre_onset_keydowns"], 1)

    def test_held_keys_and_first_response_in_actual_trial(self):
        cases = [
            ([event("D", 1000.1), event("J", 1000.101)], {"D"}, "WHITE", .094),
            ([event("D", 1000.1), event("D", 1000.11, False), event("D", 1000.12)], {"D"}, "BLACK", .113),
            ([event("D", 1000.1), event("J", 1000.101)], set(), "BLACK", .093),
            ([event("J", 1000.1), event("D", 1000.1)], set(), "WHITE", .093),
        ]
        for events, held, expected, expected_rt in cases:
            with self.subTest(expected=expected, held=held):
                row, _ = self.run_trial(FakeTiming(events, stall=.2, held=held))
                self.assertEqual(row["response"], expected)
                self.assertAlmostEqual(row["rt_s"], expected_rt, places=11)

    def test_delayed_advice_and_no_deadline(self):
        timing = FakeTiming([event("D", 1000.2)])
        row, _ = self.run_trial(timing, block_idx=3, aid_onset_ms=50, cfg_changes={"TRIAL_DEADLINE_MS": None})
        self.assertFalse(timing.frames[0][0]["visible"])
        self.assertTrue(any(p["visible"] for p, _, _ in timing.frames))
        self.assertTrue(all(left is None for _, left, _ in timing.frames))
        self.assertEqual(row["response"], "BLACK")
        first_aid_index = next(i for i, (p, _, _) in enumerate(timing.frames) if p["visible"])
        expected = (timing.flips[first_aid_index] - 1000.007) * 1000
        self.assertAlmostEqual(row["aid_onset_ms_rel"], expected, places=9)
        self.assertGreaterEqual(row["aid_onset_ms_rel"], 50)

    def test_response_before_delayed_advice_leaves_realised_onset_missing(self):
        timing = FakeTiming([event("D", 1000.008)])
        row, _ = self.run_trial(timing, block_idx=3, aid_onset_ms=500)
        self.assertTrue(all(not p["visible"] for p, _, _ in timing.frames))
        self.assertEqual(row["aid_onset_ms"], 500)
        self.assertIsNone(row["aid_onset_ms_rel"])

    def test_advanced_advice_has_signed_onset_and_ignores_preview_keys(self):
        timing = FakeTiming([event("J", 1000.02), event("J", 1000.03, False), event("D", 1000.2)])
        def preview(screen, clock, aid, ui, delay, now):
            self.assertTrue(timing.active)
            self.assertEqual(delay, -50)
            timing.t += .05
            return True, 1000.01
        with patch.object(task, "maybe_run_advanced_aid_phase", preview):
            row, _ = self.run_trial(timing, block_idx=3, aid_onset_ms=-50)
        self.assertAlmostEqual(row["stimulus_onset_s"], 1000.057, places=11)
        self.assertAlmostEqual(row["aid_onset_ms_rel"], -47., places=8)
        self.assertAlmostEqual(row["rt_s"], .143, places=11)
        self.assertEqual(row["n_ignored_pre_onset_keydowns"], 1)
        self.assertTrue(timing.frames[0][0]["visible"])

    def test_hard_quit_before_and_after_onset_always_stops_queue(self):
        quit_event = pygame.event.Event(pygame.KEYDOWN, key=pygame.K_q, mod=pygame.KMOD_ALT)
        for batches in ([[quit_event]], [[], [quit_event]]):
            timing = FakeTiming()
            with self.assertRaises(SystemExit):
                self.run_trial(timing, pygame_events=batches)
            self.assertTrue(timing.closed)
            self.assertFalse(timing.active)

    def test_render_error_stops_acquisition(self):
        timing = FakeTiming()
        def fail(*a, **k):
            raise ValueError("render failed")
        timing.draw = fail
        with self.assertRaisesRegex(ValueError, "render failed"):
            self.run_trial(timing)
        self.assertTrue(timing.closed)

    def test_advanced_advice_timestamp_is_after_first_flip(self):
        timing = FakeTiming()
        screen = types.SimpleNamespace(fill=lambda *a: None)
        ui = {"fonts": {"small": None, "aid_label": None, "aid": None}, "trials_left": 10, "n_trials": 10}
        aid = dict(label="BLACK", transparency_level="none", evidence_black_pct=60, evidence_white_pct=40)
        with ExitStack() as stack:
            for name in ("draw_progress_bar", "draw_samples_left_label", "draw_aid_recommendation_top_center"):
                stack.enter_context(patch.object(task, name))
            stack.enter_context(patch.object(pygame.event, "get", return_value=[]))
            stack.enter_context(patch.object(pygame.display, "flip", timing.flip))
            visible, onset = task.maybe_run_advanced_aid_phase(screen, timing, aid, ui, -20, timing.now)
        self.assertTrue(visible)
        self.assertAlmostEqual(onset, 1000 + 1/task.FPS + .003, places=10)
        self.assertGreaterEqual(timing.t - onset, .020)

    @unittest.skipUnless(shutil.which("Rscript"), "Rscript required for real collation check")
    def test_real_collation_mixes_old_new_and_preserves_precision(self):
        old, _ = self.run_trial(FakeTiming([event("D", 1000.1)]))
        for field in rt.AUDIT_FIELDS:
            old.pop(field)
        old_rows = [dict(old, trial=i, rt_s=value) for i, value in
                    enumerate((-.012345678901234, 0., .999), 1)]
        old_rows[2].update(response="TIMEOUT", correct=None, feedback="TOO SLOW")
        new_rows = []
        for index, timestamp in enumerate((1000.157123456789, 1000.157623456789), 1):
            row, _ = self.run_trial(FakeTiming([event("D", timestamp)]), pid=83, trial_number=index,
                                    run_timestamp="20261015_130522")
            new_rows.append(row)
        timeout, _ = self.run_trial(FakeTiming(stall=.05), pid=83, trial_number=3,
                                    run_timestamp="20261015_130522", cfg_changes={"TRIAL_DEADLINE_MS": 25})
        new_rows.append(timeout)
        with tempfile.TemporaryDirectory() as td:
            source, dest = Path(td) / "source", Path(td) / "collated"
            for rows in (old_rows, new_rows):
                self.write_fixture_run(source, rows)
            run = self.collate(source, dest)
            self.assertEqual(run.returncode, 0, run.stderr[-4000:])
            for filename in ("data_virus_all.csv", "data_virus.csv"):
                with (dest / filename).open() as f:
                    combined = list(csv.DictReader(f))
                self.assertEqual(len(combined), 6)
                historical = [r for r in combined if r["participant_id"] == "1"]
                self.assertEqual([float(r["rt_s"]) for r in historical], [r["rt_s"] for r in old_rows])
                self.assertTrue(all(r[k] in ("", "NA") for r in historical for k in rt.AUDIT_FIELDS))
                corrected = sorted((r for r in combined if r["participant_id"] == "84"), key=lambda r: int(r["trial"]))
                self.assertEqual(len(corrected), 3)
                for index, actual in enumerate(corrected[:2]):
                    self.assertEqual(actual["source_participant_id"], "83")
                    self.assertEqual(actual["run_timestamp"], "20261015_130522")
                    expected_rt = (1000.157123456789, 1000.157623456789)[index] - 1000.007
                    self.assertAlmostEqual(float(actual["rt_s"]), expected_rt, places=11)
                    for field in ("stimulus_onset_s", "response_timestamp_s", "response_readout_delay_s"):
                        self.assertAlmostEqual(float(actual[field]), new_rows[index][field], places=11)
                    self.assertEqual(actual["rt_timing_version"], rt.TIMING_VERSION)
                    self.assertEqual(actual["rt_backend_version"], rt.PTB_VERSION)
                    for field in ("condition_deadline_code", "time_pressure_condition", "automation_reliability_pattern",
                                  "automation_reliability_group"):
                        self.assertEqual(actual[field], new_rows[index][field])
                self.assertAlmostEqual(float(corrected[1]["rt_s"]) - float(corrected[0]["rt_s"]), .0005, places=11)
                self.assertEqual(corrected[2]["response"], "TIMEOUT")
                for field in ("rt_s", "response_timestamp_s", "response_readout_delay_s", "correct"):
                    self.assertIn(corrected[2][field], ("", "NA"))
            for filename in ("data_virus_postblock_all.csv", "data_virus_sliders_all.csv"):
                with (dest / filename).open() as f:
                    matched = list(csv.DictReader(f))
                self.assertEqual({r["participant_id"] for r in matched}, {"1", "84"})
                corrected = next(r for r in matched if r["participant_id"] == "84")
                self.assertEqual(corrected["source_participant_id"], "83")
                self.assertEqual(corrected["run_timestamp"], "20261015_130522")
            for filename in ("input_manifest.csv", "source_checksums.csv"):
                with (dest / filename).open() as f:
                    records = list(csv.DictReader(f))
                self.assertEqual(len(records), 6 if filename == "input_manifest.csv" else 3)
                for record in records:
                    self.assertEqual(record["md5"], hashlib.md5(Path(record["path"]).read_bytes()).hexdigest())

    def write_fixture_run(self, source, rows):
        row = rows[0]
        fields = ("participant_id", "run_timestamp", "block", "block_idx", "condition_deadline_code",
                  "time_pressure_condition", "trial_deadline_s", "automation_reliability_pattern",
                  "automation_reliability_group")
        common = {field: row[field] for field in fields}
        trust = dict(common, question_idx=1, question="Fixture trust", left_anchor="Low", right_anchor="High",
                     response=4, scale_min=1, scale_max=7)
        slider = dict(common, question_idx=1, question_key="self_accuracy", question="Fixture slider", response_percent=70.)
        task.save_combined_outputs(source, row["participant_id"], row["run_timestamp"], rows, [slider], [trust])
        task.write_csv_rows(source / "block_fixture.csv", rows)
        with (source / "block_fixture.csv").open() as f:
            restored = list(csv.DictReader(f))
        for before, after in zip(rows, restored):
            if before["rt_s"] is not None:
                self.assertEqual(before["rt_s"], float(after["rt_s"]))

    def collate(self, source, dest):
        return subprocess.run([shutil.which("Rscript"), str(ROOT / "collate_data.R"), str(source), str(dest)],
                              capture_output=True, text=True, timeout=60)

    @unittest.skipUnless(shutil.which("Rscript"), "Rscript required")
    def test_historical_only_collation_and_existing_schema_guards(self):
        old, _ = self.run_trial(FakeTiming([event("D", 1000.1)]))
        for field in rt.AUDIT_FIELDS:
            old.pop(field)
        with tempfile.TemporaryDirectory() as td:
            source, dest = Path(td) / "source", Path(td) / "collated"
            self.write_fixture_run(source, [old])
            run = self.collate(source, dest)
            self.assertEqual(run.returncode, 0, run.stderr[-4000:])
            with (dest / "data_virus_all.csv").open() as f:
                self.assertFalse(set(rt.AUDIT_FIELDS) & set(next(csv.reader(f))))
            path = next(source.glob("*_POSTBLOCK_ALL.csv"))
            contents = path.read_text()
            path.write_text(contents.replace("20260101_120000", "20260102_120000"))
            run = self.collate(source, dest)
            self.assertNotEqual(run.returncode, 0)
            self.assertIn("Mismatched run", run.stderr)
            path.write_text(contents)
            bad = dict(old)
            bad.pop("trial_deadline_s")
            task.write_csv_rows(next(source.glob("*_b00_ALL.csv")), [bad])
            run = self.collate(source, dest)
            self.assertNotEqual(run.returncode, 0)
            self.assertIn("trial_deadline_s", run.stderr)


class NativeAdapterContractTests(unittest.TestCase):
    def fake_ptb(self, fail=None, events=()):
        calls = []
        pending = list(events)
        def hid(name, *args):
            calls.append((name, args))
            if name == fail:
                raise RuntimeError("native failure")
            if name == "KbCheck":
                return 0, 100., np.zeros((1, 256))
            if name == "KbQueueGetEvent":
                return (pending.pop(0), len(pending)) if pending else (np.empty((0, 0)), 0)
        return types.SimpleNamespace(GetSecs=lambda: 100.1, PsychHID=hid, pending=pending), calls

    def context(self, ptb, platform="darwin", backend_version=rt.PTB_VERSION):
        stack = ExitStack()
        stack.enter_context(patch.dict(sys.modules, psychtoolbox=ptb))
        stack.enter_context(patch.object(rt, "version", return_value=backend_version))
        stack.enter_context(patch.object(sys, "platform", platform))
        return stack

    def test_native_shapes_masks_and_cleanup_both_platforms(self):
        for platform in ("darwin", "win32"):
            ptb, calls = self.fake_ptb()
            with self.context(ptb, platform), rt.PsychtoolboxKeyboard() as backend:
                with backend.trial(KEYMAP) as w:
                    self.assertEqual(w.held_keys, set())
                    self.assertEqual(backend.drain(), [])
            mask = next(args[1] for name, args in calls if name == "KbQueueCreate")
            self.assertEqual(set(np.flatnonzero(mask) + 1), set(rt.KEY_CODES[platform].values()))
            self.assertEqual(calls[-1][0], "KbQueueRelease")
            self.assertEqual(sum(n == "KbQueueStart" for n, _ in calls), 2)

    def test_failed_startup_releases_queue_and_does_not_fallback(self):
        ptb, calls = self.fake_ptb(fail="KbCheck")
        with self.context(ptb), self.assertRaisesRegex(rt.TimingError, "No frame-polling fallback"):
            with rt.PsychtoolboxKeyboard():
                self.fail("Must not enter task with failed native input")
        self.assertEqual([n for n, _ in calls][-2:], ["KbQueueStop", "KbQueueRelease"])

    def test_adapter_preserves_native_timestamp_and_records_readout_separately(self):
        ptb, _ = self.fake_ptb()
        with self.context(ptb), rt.PsychtoolboxKeyboard() as backend:
            with backend.trial(KEYMAP) as w:
                w.mark_onset(100.05, 5)
                ptb.pending.extend([
                    dict(Keycode=7, Pressed=1, Time=100.075123456),
                    dict(Keycode=13, Pressed=1, Time=100.075623456),
                ])
                w.consume(backend.drain())
                self.assertEqual(w.response, "BLACK")
                self.assertAlmostEqual(w.result()["rt_s"], .025123456, places=12)
                self.assertAlmostEqual(w.result()["response_readout_delay_s"], .024876544, places=12)

    def test_exception_inside_trial_stops_and_releases_native_queue(self):
        ptb, calls = self.fake_ptb()
        with self.context(ptb), self.assertRaises(SystemExit):
            with rt.PsychtoolboxKeyboard() as backend:
                with backend.trial(KEYMAP):
                    raise SystemExit()
        self.assertEqual([n for n, _ in calls][-2:], ["KbQueueStop", "KbQueueRelease"])

    def test_unvalidated_version_is_rejected(self):
        ptb, calls = self.fake_ptb()
        with self.context(ptb, backend_version="0.0"), self.assertRaises(rt.TimingError):
            with rt.PsychtoolboxKeyboard():
                self.fail()
        self.assertEqual(calls, [])

    def test_missing_backend_fails_without_entering_task(self):
        from importlib.metadata import PackageNotFoundError
        with patch.object(rt, "version", side_effect=PackageNotFoundError("psychtoolbox")):
            with self.assertRaisesRegex(rt.TimingError, "No frame-polling fallback"):
                with rt.PsychtoolboxKeyboard():
                    self.fail("Missing backend must prevent task entry")

    def test_native_held_snapshot_uses_platform_key_codes(self):
        for platform in ("darwin", "win32"):
            ptb, calls = self.fake_ptb()
            original = ptb.PsychHID
            def hid(name, *args):
                if name == "KbCheck":
                    state = np.zeros((1, 256))
                    state[0, rt.KEY_CODES[platform]["D"] - 1] = 1
                    return 1, 100., state
                return original(name, *args)
            ptb.PsychHID = hid
            with self.context(ptb, platform), rt.PsychtoolboxKeyboard() as backend:
                with backend.trial(KEYMAP) as w:
                    self.assertEqual(w.held_keys, {"D"})

    def test_main_releases_backend_and_pygame_on_completion_or_exception(self):
        for failure in (None, SystemExit(), ValueError("render failed"), rt.TimingError("queue failed")):
            ptb, calls = self.fake_ptb()
            with self.subTest(failure=failure), self.context(ptb), ExitStack() as stack:
                stack.enter_context(patch.object(task, "parse_cli_args"))
                stack.enter_context(patch.object(pygame, "init"))
                repeat = stack.enter_context(patch.object(pygame.key, "set_repeat"))
                quit_mock = stack.enter_context(patch.object(pygame, "quit"))
                stack.enter_context(patch.object(task, "run_experiment", side_effect=failure))
                if failure is None:
                    task.main()
                else:
                    expected = SystemExit if isinstance(failure, rt.TimingError) else type(failure)
                    with self.assertRaises(expected):
                        task.main()
                repeat.assert_called_once_with()
                quit_mock.assert_called_once_with()
                self.assertEqual(calls[-1][0], "KbQueueRelease")


class DesignAndImportTests(unittest.TestCase):
    def test_complete_predictable_allocation_cycle_and_single_lp_calibration(self):
        allocations = [task.counterbalance_allocation_for_participant(p) for p in range(1, 97)]
        cells = [(a["post_calibration_block_order_idx"], a["reliability_pattern"], a["key_flip"]) for a in allocations]
        expected = [(order, pattern, flip) for order in range(24)
                    for pattern in ("HP95_LP65", "HP65_LP95") for flip in (False, True)]
        self.assertEqual(cells, expected)
        self.assertEqual(set(Counter(c[0] for c in cells).values()), {4})
        self.assertEqual(set(Counter(c[1] for c in cells).values()), {48})
        self.assertEqual(set(Counter(c[2] for c in cells).values()), {48})
        for pid in range(1, 97):
            blocks = task.build_blocks_for_participant(pid, task.BLOCKS)
            self.assertEqual(len(blocks), 5)
            self.assertEqual(blocks[0]["CONDITION_DEADLINE_CODE"], "CAL_LP")
            self.assertEqual(blocks[0]["TRIAL_DEADLINE_MS"], 3000)
            self.assertEqual(blocks[0]["N_TRIALS"], 300)
            self.assertEqual(blocks[0]["TARGET_ACC"], .77)
            self.assertEqual({b["CONDITION_DEADLINE_CODE"] for b in blocks[1:]}, {"M_HP", "A_HP", "M_LP", "A_LP"})
            for block in blocks[1:]:
                self.assertEqual(block["N_TRIALS"], 400)
                self.assertEqual(block["TRIAL_DEADLINE_MS"], 1000 if block["TIME_PRESSURE_CONDITION"] == "HP" else 3000)
        with self.assertRaises(ValueError):
            task.select_single_block("CALIBRATION", task.BLOCKS, 1, deadline_s=1)
        self.assertEqual(task.select_single_block("CALIBRATION", task.BLOCKS, 1, deadline_s=3)[0]["CONDITION_DEADLINE_CODE"], "CAL_LP")

    def test_all_main_conditions_reuse_one_calibration_delta(self):
        for block in task.build_blocks_for_participant(1, task.BLOCKS)[1:]:
            state = task.prepare_block_state(block, 1, current_calibration=(.041, .014),
                                             previous_calibration=(None, None, None))
            self.assertEqual(state["difficulty_mode"], "fixed_delta")
            self.assertEqual((state["fixed_delta_mean"], state["fixed_delta_sd"]), (.041, .014))

    def test_import_and_help_paths_do_not_load_native_backend(self):
        code = "import sys; sys.modules['psychtoolbox'] = None; from python import virus_task; assert 'psychtoolbox.PsychHID' not in sys.modules"
        run = subprocess.run([sys.executable, "-c", code], cwd=ROOT, capture_output=True, text=True, timeout=15)
        self.assertEqual(run.returncode, 0, run.stderr)
        for script in ("virus_task.py", "check_response_timing.py", "capture_virus_task_screenshots.py",
                       "capture_instruction_screenshots.py"):
            code = ("import runpy, sys; sys.modules['psychtoolbox'] = None; "
                    "sys.path.insert(0, 'python'); "
                    f"sys.argv = ['{script}', '--help']; runpy.run_path('python/{script}', run_name='__main__')")
            run = subprocess.run([sys.executable, "-c", code], cwd=ROOT, capture_output=True, text=True, timeout=15)
            self.assertEqual(run.returncode, 0, run.stderr)
            self.assertIn("usage:", run.stdout)


if __name__ == "__main__":
    unittest.main()
