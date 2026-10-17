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


class TaskIntegrationTests(unittest.TestCase):
    def run_trial(self, timing, pid=1, block_idx=2, trial_number=1, cfg_changes=None, pygame_events=None):
        cfg = task.build_blocks_for_participant(pid, task.BLOCKS)[block_idx-1]
        cfg.update(participant_id=pid, block_idx=block_idx)
        cfg.update(cfg_changes or {})
        state = dict(difficulty_mode="staircase" if block_idx == 1 else "fixed_delta",
                     fixed_delta_mean=.1, fixed_delta_sd=.01, aid_transparency="none", delta_mean=.2)
        def feedback(*args):
            self.assertFalse(timing.active, "Stop acquisition before waiting for feedback.")
            return "TOO SLOW" if args[3] == "TIMEOUT" else "CORRECT"
        with ExitStack() as stack:
            stack.enter_context(patch.object(task, "DISH_RADIUS", 200, create=True))
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

    def test_both_mappings_groups_blocks_and_phase_metadata(self):
        for pid in (1, 2, 3, 4):
            for block_idx, trial_n in ((1, 1), (2, 1), (3, 1), (3, 401), (3, 801), (4, 1)):
                with self.subTest(pid=pid, block=block_idx, trial=trial_n):
                    key = task.key_mapping_for_participant(pid)["key_black_name"]
                    timing = FakeTiming([event(key, 1000.1)])
                    row, state = self.run_trial(timing, pid, block_idx, trial_n)
                    self.assertTrue(row["correct"])
                    self.assertEqual(row["calibration_target_group"], "CAL65" if pid % 2 else "CAL90")
                    self.assertEqual(row["main_block_order"], "SPLIT_MANUAL")
                    if block_idx == 3:
                        phase = (trial_n-1)//400+1
                        self.assertEqual(row["reliability_phase_idx"], phase)
                        self.assertEqual(row["aid_reliability_level"], .7 if phase == 2 else .95)
                        self.assertTrue(timing.frames[0][0]["visible"])
                    elif block_idx in (2, 4):
                        self.assertEqual(row["manual_segment"], "PRE_AUTOMATION" if block_idx == 2 else "POST_AUTOMATION")
                    else:
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

    def test_delayed_advice_and_no_deadline(self):
        timing = FakeTiming([event("D", 1000.2)])
        row, _ = self.run_trial(timing, block_idx=3, cfg_changes={"AID_ONSET_MS": 50, "TRIAL_DEADLINE_MS": None})
        self.assertFalse(timing.frames[0][0]["visible"])
        self.assertTrue(any(p["visible"] for p, _, _ in timing.frames))
        self.assertTrue(all(left is None for _, left, _ in timing.frames))
        self.assertEqual(row["response"], "BLACK")

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
        new_rows = []
        for index, timestamp in enumerate((1000.157123456789, 1000.157623456789), 1):
            row, _ = self.run_trial(FakeTiming([event("D", timestamp)]), pid=2, trial_number=index)
            new_rows.append(row)
        timeout, _ = self.run_trial(FakeTiming(stall=.05), pid=2, trial_number=3,
                                    cfg_changes={"TRIAL_DEADLINE_MS": 25})
        new_rows.append(timeout)
        with tempfile.TemporaryDirectory() as td:
            source = Path(td) / "source"
            dest = Path(td) / "collated"
            source.mkdir()
            for pid, rows in ((1, [old]), (2, new_rows)):
                prefix = source / f"results_p{pid:03d}_20260101_120000_b00"
                task.write_csv_rows(str(prefix) + "_ALL.csv", rows)
                task.write_csv_rows(str(prefix) + "_POSTBLOCK_ALL.csv",
                                    [dict(participant_id=pid, block="MANUAL", question_idx=1, response=4)])
                task.write_csv_rows(str(prefix) + "_POSTBLOCK_SLIDERS_ALL.csv",
                                    [dict(participant_id=pid, block="MANUAL", question_key="self_accuracy", response_percent=70.)])
            run = subprocess.run([shutil.which("Rscript"), str(ROOT / "collate_data.R"), str(source), str(dest)],
                                 capture_output=True, text=True, timeout=60)
            self.assertEqual(run.returncode, 0, run.stderr[-4000:])
            for filename in ("data_virus_all.csv", "data_virus.csv"):
                with (dest / filename).open() as f:
                    combined = list(csv.DictReader(f))
                self.assertEqual(len(combined), 4)
                historical = next(r for r in combined if r["participant_id"] == "1")
                self.assertEqual(float(historical["rt_s"]), old["rt_s"])
                self.assertTrue(all(historical[k] in ("", "NA") for k in rt.AUDIT_FIELDS))
                corrected = sorted((r for r in combined if r["participant_id"] == "2"), key=lambda r: int(r["trial"]))
                for expected, actual in zip(new_rows[:2], corrected[:2]):
                    for field in ("rt_s", "stimulus_onset_s", "response_timestamp_s", "response_readout_delay_s"):
                        self.assertAlmostEqual(float(actual[field]), expected[field], places=11)
                    self.assertEqual(actual["rt_timing_version"], rt.TIMING_VERSION)
                    self.assertEqual(actual["rt_backend_version"], rt.PTB_VERSION)
                self.assertAlmostEqual(float(corrected[1]["rt_s"]) - float(corrected[0]["rt_s"]), .0005, places=11)
                self.assertEqual(corrected[2]["response"], "TIMEOUT")
                self.assertIn(corrected[2]["rt_s"], ("", "NA"))


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


if __name__ == "__main__":
    unittest.main()
