"""Timestamp invariants and task integration; no native input or study writes."""

from contextlib import contextmanager, ExitStack
import csv
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


def window(held=(), duration=10.0):
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
        for delta, expected in ((9.999999, "BLACK"), (10., "TIMEOUT"), (10.000001, "TIMEOUT")):
            w = window()
            w.consume([event("D", 100 + delta, lag=.05)])
            w.expire(110.1)
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

    def test_snapshot_events_are_not_counted_or_used_to_change_held_state(self):
        w = window(held={"D"})
        w.consume([event("D", 98.9, False), event("J", 99), event("D", 100.1)])
        self.assertIsNone(w.response)
        self.assertEqual(w.ignored_early, 0)
        w.consume([event("D", 100.2, False), event("D", 100.3)])
        self.assertEqual(w.response, "BLACK")

    def test_repeated_downs_cannot_overwrite_first_valid_response(self):
        w = window()
        w.consume([event("D", 100.1), event("D", 100.2), event("J", 100.3)])
        self.assertAlmostEqual(w.result()["rt_s"], .1)
        self.assertEqual(w.response, "BLACK")


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
    def run_trial(self, timing, pid=1, block_idx=2, trial_number=1, cfg_changes=None, pygame_events=None,
                  deadline_ms=10000, aid_onset_ms=0, preparation_s=0):
        cfg = task.build_blocks_for_participant(pid, task.BLOCKS)[block_idx-1]
        cfg.update(participant_id=pid, block_idx=block_idx)
        cfg.update(cfg_changes or {})
        state = dict(difficulty_mode="staircase" if block_idx == 1 else "fixed_delta",
                     fixed_delta_mean=.1, fixed_delta_sd=.01,
                     aid_transparency=cfg["AID_TRANSPARENCY"], delta_mean=.2)
        def prepare(*args):
            self.assertTrue(timing.active, "Acquisition must begin before stimulus preparation.")
            timing.t += preparation_s
            return [], 2160, 1440
        def feedback(*args):
            self.assertFalse(timing.active, "Stop acquisition before waiting for feedback.")
            return "TOO SLOW" if args[3] == "TIMEOUT" else "CORRECT"
        with ExitStack() as stack:
            stack.enter_context(patch.object(task, "DISH_RADIUS", 200, create=True))
            stack.enter_context(patch.object(task, "TRIAL_DEADLINE_MS", deadline_ms))
            stack.enter_context(patch.object(task, "AID_ONSET_MS", aid_onset_ms))
            for name, value in (
                ("pick_trial_vblack_prop", lambda *a: (.6, .1)),
                ("make_trial_dots", prepare),
                ("update_dots", lambda *a: None),
                ("draw_trial_frame", timing.draw),
                ("make_aid_recommendation", lambda *a, **k: ("BLACK", True)),
                ("maybe_show_feedback", feedback),
            ):
                stack.enter_context(patch.object(task, name, value))
            for name in ("draw_progress_bar", "draw_samples_left_label", "draw_aid_recommendation_top_center"):
                stack.enter_context(patch.object(task, name))
            stack.enter_context(patch.object(pygame.display, "flip", timing.flip))
            stack.enter_context(patch.object(pygame.event, "get", side_effect=pygame_events or (lambda: [])))
            stack.enter_context(patch.object(task, "quit_clean", side_effect=SystemExit))
            screen = types.SimpleNamespace(fill=lambda *a: None)
            fonts = {"small": None, "aid_label": None, "aid": None}
            row = task.run_single_trial(screen, timing, None, (0, 0), fonts, task.key_mapping_for_participant(pid),
                                       cfg, state, 10, trial_number, 301, "test", timing)
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

    def test_both_mappings_all_blocks_and_transparency(self):
        for pid in (1, 7):
            for block_idx in range(1, 5):
                for transparency in ("none", "low", "high"):
                    with self.subTest(pid=pid, block=block_idx, transparency=transparency):
                        key = task.key_mapping_for_participant(pid)["key_black_name"]
                        timing = FakeTiming([event(key, 1000.1)])
                        row, state = self.run_trial(timing, pid, block_idx,
                                                    cfg_changes={"AID_TRANSPARENCY": transparency})
                        self.assertTrue(row["correct"])
                        self.assertEqual(row["keymap_flip"], pid == 7)
                        self.assertEqual(row["rt_ms"], 1000 * row["rt_s"])
                        if block_idx >= 3:
                            self.assertEqual(row["auto_on"], 1)
                            self.assertEqual(row["aid_accuracy_setting"], .95 if block_idx == 3 else .65)
                            self.assertEqual(row["aid_transparency_level"], transparency)
                            self.assertEqual(timing.frames[0][0]["transparency_level"], transparency)
                            self.assertTrue(timing.frames[0][0]["visible"])
                            self.assertEqual(row["aid_onset_ms_rel"], 0)
                        else:
                            self.assertEqual(row["auto_on"], 0)
                            self.assertIsNone(row["aid_onset_ms_rel"])
                            self.assertEqual(timing.frames[0][0]["mode"], "masked" if block_idx == 2 else "none")
                        if block_idx == 1:
                            self.assertLess(state["delta_mean"], .2)
                        self.assertTrue(timing.closed)

    def test_design_and_complete_twelve_participant_cycle(self):
        self.assertEqual(task.TRIAL_DEADLINE_MS, 10000)
        self.assertEqual([(b["name"], b["N_TRIALS"]) for b in task.BLOCKS],
                         [("CALIBRATION", 300), ("MANUAL", 500), ("AUTOMATION1", 500), ("AUTOMATION2", 500)])
        self.assertEqual(task.BLOCKS[0]["TARGET_ACC"], .80)
        assignments = []
        for pid in range(1, 25):
            order = tuple(b["name"] for b in task.build_blocks_for_participant(pid, task.BLOCKS))
            self.assertEqual(order[0], "CALIBRATION")
            mapping = task.key_mapping_for_participant(pid)
            self.assertEqual(mapping["flip"], ((pid-1)//6) % 2 == 1)
            assignments.append((order, mapping["key_black_name"]))
        self.assertEqual(len(set(assignments[:12])), 12)
        self.assertEqual(assignments[:12], assignments[12:])

    def test_timeout_and_pre_deadline_response_retrieved_after_deadline(self):
        row, _ = self.run_trial(FakeTiming(stall=.05))
        self.assertEqual(row["response"], "TIMEOUT")
        self.assertIsNone(row["correct"])
        self.assertIsNone(row["rt_s"])
        timing = FakeTiming([event("D", 1010.0069)], stall=.05)
        row, _ = self.run_trial(timing)
        self.assertEqual(row["response"], "BLACK")
        self.assertAlmostEqual(row["rt_s"], 9.9999, places=11)
        self.assertGreater(timing.t, row["stimulus_onset_s"] + 10)

    def test_exact_and_late_deadline_keys_time_out_without_rt(self):
        for offset in (10, 10.000001):
            with self.subTest(offset=offset):
                timing = FakeTiming(stall=.05)
                original_flip = timing.flip
                def flip_and_schedule():
                    original_flip()
                    if len(timing.flips) == 1:
                        # Use the simulated flip's exact binary float so the
                        # boundary is not shifted by decimal literal rounding.
                        timing.events.append(event("D", timing.t + offset))
                timing.flip = flip_and_schedule
                row, _ = self.run_trial(timing)
                self.assertEqual(row["response"], "TIMEOUT")
                for field in ("rt_s", "rt_ms", "correct", "response_timestamp_s", "response_readout_delay_s"):
                    self.assertIsNone(row[field])

    def test_initial_render_time_does_not_consume_deadline(self):
        timing = FakeTiming([event("D", 1012.010)])
        original_draw = timing.draw
        def slow_first(*a, **k):
            if not timing.frames:
                timing.t += 12
            return original_draw(*a, **k)
        timing.draw = slow_first
        row, _ = self.run_trial(timing)
        self.assertEqual(row["response"], "BLACK")
        self.assertAlmostEqual(row["rt_s"], .003, places=11)

    def test_preparation_and_flip_events_filtered_without_losing_post_flip_press(self):
        timing = FakeTiming([event("D", 1001), event("D", 1002, False),
                             event("J", 1012.006), event("J", 1012.0065, False), event("D", 1012.0075)])
        row, _ = self.run_trial(timing, preparation_s=12)
        self.assertEqual(row["n_ignored_pre_onset_keydowns"], 2)
        self.assertAlmostEqual(row["stimulus_onset_s"], 1012.007)
        self.assertAlmostEqual(row["rt_s"], .0005, places=11)

    def test_delayed_advice_and_no_deadline(self):
        timing = FakeTiming([event("D", 1000.2)])
        row, _ = self.run_trial(timing, block_idx=3, aid_onset_ms=50, deadline_ms=None)
        self.assertFalse(timing.frames[0][0]["visible"])
        self.assertTrue(any(p["visible"] for p, _, _ in timing.frames))
        self.assertTrue(all(left is None for _, left, _ in timing.frames))
        self.assertEqual(row["response"], "BLACK")
        first_aid_flip = next(timing.flips[i] for i, (p, _, _) in enumerate(timing.frames) if p["visible"])
        self.assertAlmostEqual(row["aid_onset_ms_rel"], 1000 * (first_aid_flip - row["stimulus_onset_s"]))
        self.assertGreaterEqual(row["aid_onset_ms_rel"], 50)
        self.assertEqual(row["aid_onset_ms"], 50)

    def test_response_before_delayed_advice_leaves_realised_onset_missing(self):
        timing = FakeTiming([event("D", 1000.02)])
        row, _ = self.run_trial(timing, block_idx=3, aid_onset_ms=100)
        self.assertEqual(row["response"], "BLACK")
        self.assertIsNone(row["aid_onset_ms_rel"])
        self.assertEqual(row["aid_onset_ms"], 100)
        self.assertFalse(any(p["visible"] for p, _, _ in timing.frames))

    def test_advanced_advice_has_signed_realised_onset_and_stimulus_rt_origin(self):
        timing = FakeTiming([event("D", 1000.002), event("D", 1000.01, False), event("J", 1000.2)])
        row, _ = self.run_trial(timing, block_idx=3, aid_onset_ms=-50)
        self.assertEqual(row["response"], "WHITE")
        self.assertLessEqual(row["aid_onset_ms_rel"], -50)
        self.assertAlmostEqual(row["aid_onset_ms_rel"], 1000 * (timing.flips[0] - row["stimulus_onset_s"]))
        self.assertAlmostEqual(row["rt_s"], 1000.2 - row["stimulus_onset_s"])
        self.assertEqual(row["n_ignored_pre_onset_keydowns"], 1)
        self.assertTrue(timing.frames[0][0]["visible"])

    def test_unlimited_window_allows_response_after_ten_seconds(self):
        row, _ = self.run_trial(FakeTiming([event("D", 1015)], stall=.05), deadline_ms=None)
        self.assertEqual(row["response"], "BLACK")
        self.assertAlmostEqual(row["rt_s"], 14.993)

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
        old["rt_s"] = -.012345678901234  # Historical values must never be repaired.
        old["rt_ms"] = 1000 * old["rt_s"]
        old_zero = dict(old, trial=2, rt_s=0, rt_ms=0)
        old_timeout = dict(old, trial=3, response="TIMEOUT", rt_s=10.02, rt_ms=10020)
        new_rows = []
        for index, timestamp in enumerate((1000.157123456789, 1000.157623456789), 1):
            row, _ = self.run_trial(FakeTiming([event("J", timestamp)]), pid=7, block_idx=3,
                                    trial_number=index, cfg_changes={"AID_TRANSPARENCY": "high"})
            new_rows.append(row)
        timeout, _ = self.run_trial(FakeTiming(stall=.05), pid=7, block_idx=3, trial_number=3,
                                    cfg_changes={"AID_TRANSPARENCY": "high"})
        new_rows.append(timeout)
        with tempfile.TemporaryDirectory() as td:
            source = Path(td) / "source"
            dest = Path(td) / "collated"
            source.mkdir()
            for pid, rows in ((1, [old, old_zero, old_timeout]), (7, new_rows)):
                for row in rows:
                    row["run_timestamp"] = "20260101_120000"
                prefix = source / f"results_p{pid:03d}_20260101_120000_b00"
                # Exercise the actual block and combined writers.
                task.write_csv_rows(source / f"results_p{pid:03d}_20260101_120000_b02_MANUAL.csv", rows)
                task.save_combined_outputs(source, pid, "20260101_120000", rows,
                    [dict(participant_id=pid, run_timestamp="20260101_120000", block="MANUAL", question_key="self_accuracy", response_percent=70.)],
                    [dict(participant_id=pid, run_timestamp="20260101_120000", block="MANUAL", question_idx=1, response=4)])
                # Existing selection is by mtime, separately for each file family.
                # A later filename with an older mtime must not replace this run.
                for suffix in ("_ALL.csv", "_POSTBLOCK_ALL.csv", "_POSTBLOCK_SLIDERS_ALL.csv"):
                    selected = Path(str(prefix) + suffix)
                    superseded = source / f"results_p{pid:03d}_20260102_120000_b00{suffix}"
                    superseded.write_text(selected.read_text().replace("20260101_120000", "20260102_120000"))
                    os.utime(superseded, (1000000000, 1000000000))
                    os.utime(selected, (1000000100, 1000000100))
                if pid == 7:
                    with Path(str(prefix) + "_ALL.csv").open() as f:
                        raw = list(csv.DictReader(f))
                    for expected, actual in zip(new_rows, raw):
                        self.assertEqual(actual["rt_timing_version"], rt.TIMING_VERSION)
                        if expected["rt_s"] is not None:
                            self.assertEqual(float(actual["rt_s"]), expected["rt_s"])
                            self.assertEqual(float(actual["rt_ms"]), 1000 * expected["rt_s"])
            run = subprocess.run([shutil.which("Rscript"), str(ROOT / "collate_data.R"), str(source), str(dest)],
                                 capture_output=True, text=True, timeout=60)
            self.assertEqual(run.returncode, 0, run.stderr[-4000:])
            for filename in ("data_virus_all.csv", "data_virus.csv"):
                with (dest / filename).open() as f:
                    combined = list(csv.DictReader(f))
                self.assertEqual(len(combined), 6)
                historical = sorted((r for r in combined if r["participant_id"] == "1"), key=lambda r: int(r["trial"]))
                for expected, actual in zip((old, old_zero, old_timeout), historical):
                    self.assertEqual(float(actual["rt_s"]), expected["rt_s"])
                    self.assertTrue(all(actual[k] in ("", "NA") for k in rt.AUDIT_FIELDS))
                corrected = sorted((r for r in combined if r["participant_id"] == "7"), key=lambda r: int(r["trial"]))
                for expected, actual in zip(new_rows[:2], corrected[:2]):
                    for field in ("rt_s", "stimulus_onset_s", "response_timestamp_s", "response_readout_delay_s"):
                        self.assertAlmostEqual(float(actual[field]), expected[field], places=11)
                    self.assertEqual(actual["rt_timing_version"], rt.TIMING_VERSION)
                    self.assertEqual(actual["rt_backend_version"], rt.PTB_VERSION)
                    # Independent expectations, not a second call to the timing code.
                    timestamp = (1000.157123456789, 1000.157623456789)[int(actual["trial"])-1]
                    self.assertAlmostEqual(float(actual["rt_s"]), timestamp - 1000.007, places=11)
                    self.assertEqual(actual["auto_on"], "1")
                    self.assertEqual(float(actual["aid_accuracy_setting"]), .95)
                self.assertAlmostEqual(float(corrected[1]["rt_s"]) - float(corrected[0]["rt_s"]), .0005, places=11)
                self.assertEqual(corrected[2]["response"], "TIMEOUT")
                self.assertIn(corrected[2]["rt_s"], ("", "NA"))
                if filename == "data_virus_all.csv":
                    self.assertTrue(all(r["run_timestamp"] == "20260101_120000" for r in combined))
                    self.assertEqual(corrected[0]["keymap_flip"].lower(), "true")
                    self.assertEqual(corrected[0]["aid_transparency_level"], "high")
                    self.assertEqual(float(corrected[0]["aid_onset_ms_rel"]), 0)
                    self.assertEqual(float(corrected[0]["rt_ms"]), new_rows[0]["rt_ms"])
            for filename in ("data_virus_postblock_all.csv", "data_virus_sliders_all.csv"):
                with (dest / filename).open() as f:
                    measures = list(csv.DictReader(f))
                self.assertEqual({r["participant_id"] for r in measures}, {"1", "7"})
                self.assertTrue(all(r["run_timestamp"] == "20260101_120000" for r in measures))
            # Historical-only exports remain readable without any audit columns.
            for path in source.glob("results_p007_*"):
                path.unlink()
            legacy_dest = Path(td) / "historical_only"
            run = subprocess.run([shutil.which("Rscript"), str(ROOT / "collate_data.R"), str(source), str(legacy_dest)],
                                 capture_output=True, text=True, timeout=60)
            self.assertEqual(run.returncode, 0, run.stderr[-4000:])
            for filename in ("data_virus_all.csv", "data_virus.csv"):
                with (legacy_dest / filename).open() as f:
                    legacy = list(csv.DictReader(f))
                self.assertEqual(len(legacy), 3)
                self.assertTrue(set(rt.AUDIT_FIELDS).isdisjoint(legacy[0]))


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

    def test_missing_backend_is_reported_without_fallback(self):
        from importlib.metadata import PackageNotFoundError
        with patch.object(rt, "version", side_effect=PackageNotFoundError("psychtoolbox")):
            with self.assertRaisesRegex(rt.TimingError, "No frame-polling fallback"):
                with rt.PsychtoolboxKeyboard():
                    self.fail()

    def test_platform_key_codes_against_explicit_expected_values(self):
        self.assertEqual(rt.KEY_CODES, {"darwin": {"D": 7, "J": 13}, "win32": {"D": 68, "J": 74}})

    def test_read_failure_stops_and_releases_queue(self):
        ptb, calls = self.fake_ptb()
        original = ptb.PsychHID
        with self.context(ptb), self.assertRaisesRegex(RuntimeError, "read failure"):
            with rt.PsychtoolboxKeyboard() as backend:
                with backend.trial(KEYMAP):
                    def fail_read(name, *args):
                        if name == "KbQueueGetEvent":
                            raise RuntimeError("read failure")
                        return original(name, *args)
                    ptb.PsychHID = fail_read
                    backend.drain()
        self.assertEqual([n for n, _ in calls][-2:], ["KbQueueStop", "KbQueueRelease"])

    def test_no_flush_after_onset_and_initial_native_held_state(self):
        ptb, calls = self.fake_ptb()
        original = ptb.PsychHID
        def held(name, *args):
            result = original(name, *args)
            if name == "KbCheck":
                result[2][0, 6] = 1
            return result
        ptb.PsychHID = held
        with self.context(ptb), rt.PsychtoolboxKeyboard() as backend:
            with backend.trial(KEYMAP) as w:
                self.assertEqual(w.held_keys, {"D"})
                w.mark_onset(100.05, 10)
                calls.clear()
                ptb.pending.append(dict(Keycode=13, Pressed=1, Time=100.0505))
                w.consume(backend.drain())
                self.assertEqual(w.response, "WHITE")
                self.assertNotIn("KbQueueFlush", [n for n, _ in calls])

    def test_cancelled_native_initialisation_releases_queue(self):
        ptb, calls = self.fake_ptb()
        original = ptb.PsychHID
        def cancel(name, *args):
            if name == "KbCheck":
                raise KeyboardInterrupt()
            return original(name, *args)
        ptb.PsychHID = cancel
        with self.context(ptb), self.assertRaises(KeyboardInterrupt):
            with rt.PsychtoolboxKeyboard():
                self.fail()
        self.assertEqual([n for n, _ in calls][-2:], ["KbQueueStop", "KbQueueRelease"])


class EntryPointTests(unittest.TestCase):
    def test_imports_and_help_do_not_initialise_native_capture(self):
        code = "import sys; sys.path.insert(0, 'python'); import virus_task, instructions, check_response_timing; assert 'psychtoolbox' not in sys.modules"
        commands = [[sys.executable, "-B", "-c", code]]
        commands += [[sys.executable, "-B", str(ROOT / "python" / name), "--help"]
                     for name in ("virus_task.py", "check_response_timing.py")]
        for cmd in commands:
            result = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True, timeout=30)
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_main_owns_native_cleanup_and_pygame_shutdown(self):
        for failure in (None, SystemExit(), ValueError("render failed"), rt.TimingError("queue failed")):
            with self.subTest(failure=failure), ExitStack() as stack:
                stack.enter_context(patch.object(task, "parse_cli_args", return_value=object()))
                stack.enter_context(patch.object(pygame, "init"))
                stack.enter_context(patch.object(pygame.key, "set_repeat"))
                quit_mock = stack.enter_context(patch.object(pygame, "quit"))
                native = stack.enter_context(patch.object(task, "PsychtoolboxKeyboard"))
                stack.enter_context(patch.object(task, "run_experiment", side_effect=failure))
                if failure is None:
                    task.main()
                else:
                    expected = SystemExit if isinstance(failure, rt.TimingError) else type(failure)
                    with self.assertRaises(expected):
                        task.main()
                native.return_value.__exit__.assert_called_once()
                quit_mock.assert_called_once()


if __name__ == "__main__":
    unittest.main()
