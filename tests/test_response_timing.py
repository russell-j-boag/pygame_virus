"""Timestamp invariants and task integration; no native input or study writes."""

from contextlib import contextmanager, ExitStack
import csv
import io
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
            self.assertEqual(set(np.flatnonzero(mask) + 1), {7, 13} if platform == 'darwin' else {68, 74})
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

    def test_missing_backend_fails_visibly(self):
        from importlib.metadata import PackageNotFoundError
        with patch.object(rt, 'version', side_effect=PackageNotFoundError('psychtoolbox')):
            with self.assertRaisesRegex(rt.TimingError, 'Install psychtoolbox==3.0.22.2'):
                with rt.PsychtoolboxKeyboard():
                    self.fail('Missing backend must stop acquisition')

    def test_interrupted_startup_releases_queue(self):
        ptb, calls = self.fake_ptb()
        hid = ptb.PsychHID
        def interrupted(name, *args):
            if name == 'KbCheck':
                raise KeyboardInterrupt()
            return hid(name, *args)
        ptb.PsychHID = interrupted
        with self.context(ptb), self.assertRaises(KeyboardInterrupt):
            with rt.PsychtoolboxKeyboard():
                self.fail()
        self.assertEqual([n for n, _ in calls][-2:], ['KbQueueStop', 'KbQueueRelease'])

    def test_initial_snapshot_is_authoritative_for_held_keys(self):
        ptb, _ = self.fake_ptb()
        hid = ptb.PsychHID
        def held(name, *args):
            if name == 'KbCheck':
                state = np.zeros((1, 256))
                state[0, 6] = 1  # D uses PTB code 7 on macOS.
                return 1, 100., state
            return hid(name, *args)
        ptb.PsychHID = held
        with self.context(ptb), rt.PsychtoolboxKeyboard() as backend:
            with backend.trial(KEYMAP) as w:
                self.assertEqual(w.held_keys, {'D'})
                w.mark_onset(100.01, None)
                # A release already represented by the held-state snapshot
                # must not undo that snapshot and admit a repeated key-down.
                w.consume([event('D', 100., False), event('D', 100.02), event('J', 100.03)])
                self.assertEqual(w.response, 'WHITE')



class FakeTiming:
    """Independent source times; only presentation/readout advances virtual time."""
    backend_version = rt.PTB_VERSION

    def __init__(self, plans=None, stall=0, first_draw_cost=.004):
        self.t = 1000.
        self.plans = plans or [{}, {}]
        self.stall = stall
        self.first_draw_cost = first_draw_cost
        self.active = False
        self.closed = False
        self.windows = []
        self.stops = 0
        self.frames = []
        self.waits = []
        self.pending = []
        self.draw_count = 0

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        self.closed = True
        self.active = False

    def now(self):
        return self.t

    @contextmanager
    def trial(self, keymap):
        assert not self.active
        plan = self.plans[len(self.windows)]
        self.active = True
        self.draw_count = 0
        self.pending = [event(key, self.t + offset, pressed)
                        for key, offset, pressed in plan.get('events', [('D', .157123456789, True)])]
        w = rt.ResponseWindow(self.t, plan.get('held', ()), keymap, self.backend_version)
        self.windows.append(w)
        try:
            yield w
        finally:
            self.active = False
            self.stops += 1

    def drain(self):
        assert self.active
        ready = [e for e in self.pending if e.timestamp_s <= self.t]
        self.pending = [e for e in self.pending if e.timestamp_s > self.t]
        return [rt.KeyEvent(e.key, e.pressed, e.timestamp_s, self.t) for e in ready]

    def tick(self, fps):
        self.t += 1 / fps + self.stall
        if self.t > 1100:
            raise AssertionError('Synthetic trial failed to complete')

    def draw(self, kind, payload=None):
        assert self.active, 'Start acquisition before preparing the first frame'
        self.frames.append((len(self.windows), kind, payload))
        self.t += self.first_draw_cost if self.draw_count == 0 else .004
        self.draw_count += 1

    def flip(self):
        self.t += .003

    def wait(self, kind, ms):
        assert not self.active, 'No acquisition during previews, fixation or feedback'
        self.waits.append((kind, ms))
        self.t += ms / 1000.


class TaskHarness:
    def patches(self, timing, pygame_events=None):
        stack = ExitStack()
        for name, value in (
            ('DISH_RADIUS', 200),
            ('pick_trial_vblack_prop', lambda *a: (.6, .04)),
            ('make_trial_dots', lambda *a: ([], 60, 40)),
            ('update_dots', lambda *a: None),
            ('draw_trial_frame', lambda *a, **k: timing.draw('stimulus', a[4])),
            ('draw_final_decision_frame', lambda *a, **k: timing.draw('blank_response')),
            ('draw_aid_only_frame', lambda *a, **k: timing.draw('aid_only')),
            ('draw_masked_placeholder_frame', lambda *a, **k: timing.draw('masked_placeholder')),
            ('make_aid_recommendation', lambda *a, **k: ('BLACK', True)),
            ('fixation_cross_screen', lambda screen, clock, ms: timing.wait('fixation', ms)),
            ('run_masked_preview_phase', lambda screen, clock, ui, duration_ms: timing.wait('masked', duration_ms)),
            ('run_aid_preview_phase', lambda screen, clock, aid, ui, duration_ms: timing.wait('aid', duration_ms)),
            ('maybe_show_feedback', lambda *a: timing.wait('feedback', 0) or ('CORRECT' if a[4] else 'INCORRECT')),
        ):
            stack.enter_context(patch.object(task, name, value, create=True))
        stack.enter_context(patch.object(pygame.display, 'flip', timing.flip))
        stack.enter_context(patch.object(pygame.key, 'set_repeat'))
        stack.enter_context(patch.object(pygame.event, 'get', side_effect=pygame_events or (lambda: [])))
        stack.enter_context(patch.object(task, 'quit_clean', side_effect=SystemExit))
        return stack

    def run_trial(self, timing, cfg=None, pid=1, trial=1, timestamp='20260101_120000'):
        cfg = dict(task.BLOCKS[0] if cfg is None else cfg)
        cfg.update(participant_id=pid, block_idx=0 if cfg['name'] == 'PRACTICE' else 1)
        with patch('builtins.print'):
            state = task.prepare_block_state(cfg, pid)
        with self.patches(timing):
            row = task.run_single_trial(None, timing, None, (0, 0), {}, task.key_mapping_for_participant(pid),
                                        cfg, state, 10, trial, trial, timestamp, timing=timing)
        return row, state


class TaskIntegrationTests(TaskHarness, unittest.TestCase):
    def test_raw_block_writer_preserves_both_phase_records(self):
        for original in (task.PRACTICE_BLOCK, *task.BLOCKS, task.HIDDEN_SIM_BLOCK):
            cfg = dict(original, N_TRIALS=1, participant_id=1, block_idx=1)
            timing = FakeTiming()
            with tempfile.TemporaryDirectory() as td, self.patches(timing), patch('builtins.print'):
                rows, _, _ = task.run_trial_block(None, timing, None, (0, 0), {},
                    task.key_mapping_for_participant(1), cfg, '20260101_120000', td, 0, timing=timing)
                path = next(Path(td).glob('results_*.csv'))
                restored = list(csv.DictReader(io.StringIO(path.read_text())))[0]
                for d in (1, 2):
                    for suffix in ('rt_s', *rt.PHASE_FIELDS):
                        field = f'decision{d}_{suffix}'
                        self.assertEqual(float(restored[field]), rows[0][field])
                self.assertEqual(restored['rt_timing_version'], rt.TIMING_VERSION)

    def test_queued_early_key_and_immediate_post_flip_key_each_phase(self):
        plan = dict(events=[('D', .001, True), ('D', .002, False), ('J', .0075, True)])
        timing = FakeTiming([plan, plan])
        row, _ = self.run_trial(timing)
        for d in (1, 2):
            self.assertEqual(row[f'decision{d}_response'], 'WHITE')
            self.assertAlmostEqual(row[f'decision{d}_rt_s'], .0005, places=11)
            self.assertEqual(row[f'decision{d}_n_ignored_pre_onset_keydowns'], 1)
            self.assertAlmostEqual(row[f'decision{d}_onset_s'], timing.windows[d-1].start_s + .007)
        self.assertEqual(timing.stops, 2)
        self.assertFalse(timing.active)
        self.assertGreater(row['decision2_onset_s'] - row['decision1_response_timestamp_s'], 2.0)

    def test_same_source_times_survive_render_stalls_both_phases(self):
        for stall in (0, .008, .020, .050):
            with self.subTest(stall=stall):
                timing = FakeTiming(stall=stall)
                row, _ = self.run_trial(timing)
                for d in (1, 2):
                    self.assertAlmostEqual(row[f'decision{d}_rt_s'], .150123456789, places=11)
                    self.assertGreaterEqual(row[f'decision{d}_response_readout_delay_s'], 0)
                self.assertEqual(row['rt_timing_version'], 'ptb_queue_phase_flip_v1')

    def test_pygame_downs_cannot_choose_response(self):
        timing = FakeTiming([{}])
        with self.patches(timing, lambda: [pygame.event.Event(pygame.KEYDOWN, key=pygame.K_j, mod=0)]):
            result = task.collect_key_response(None, timing, task.key_mapping_for_participant(1),
                                               lambda ms: timing.draw('test'), timing=timing)
        self.assertEqual(result['response'], 'BLACK')
        self.assertAlmostEqual(result['rt_s'], .150123456789, places=11)

    def test_held_and_queued_keys_crossing_preview_require_new_down(self):
        plans = [{}, dict(held={'D'}, events=[
            ('J', -.01, True), ('D', .001, True), ('D', .05, True),
            ('D', .1, False), ('D', .2, True)])]
        row, _ = self.run_trial(FakeTiming(plans))
        self.assertEqual(row['decision2_response'], 'BLACK')
        self.assertAlmostEqual(row['decision2_rt_s'], .193, places=11)
        # A held Decision 1 key never blocks the other response key.
        plans[1] = dict(held={'D'}, events=[('D', .01, True), ('J', .02, True)])
        row, _ = self.run_trial(FakeTiming(plans))
        self.assertEqual(row['decision2_response'], 'WHITE')
        self.assertTrue(row['changed_response'])

    def test_slow_first_frame_starts_clock_after_flip(self):
        plans = [dict(events=[('D', .01, True), ('D', .02, False), ('J', .06, True)])] * 2
        timing = FakeTiming(plans, first_draw_cost=.050)
        row, _ = self.run_trial(timing)
        for d in (1, 2):
            self.assertAlmostEqual(row[f'decision{d}_rt_s'], .007, places=11)
            self.assertEqual(row[f'decision{d}_n_ignored_pre_onset_keydowns'], 1)

    def test_all_conditions_mappings_design_and_feedback(self):
        configs = [task.PRACTICE_BLOCK, *task.BLOCKS, task.HIDDEN_SIM_BLOCK]
        self.assertEqual([b['CONDITION_CODE'] for b in task.BLOCKS], ['MANUAL', 'AIDFIRST', 'STIMFIRST'])
        self.assertEqual([b['N_TRIALS'] for b in configs], [60, 260, 260, 260, 260])
        self.assertEqual(task.GLOBAL_AID_ACCURACY, .85)
        self.assertEqual(task.CALIBRATION_TARGET_ACCURACY, .75)
        self.assertEqual(task.GLOBAL_FIXED_DELTA, .040324718919)
        self.assertEqual(task.GLOBAL_FIXED_DELTA_SD, .014615991726)
        cells = set()
        for pid in range(1, 13):
            mapping = task.key_mapping_for_participant(pid)
            order = tuple(b['CONDITION_CODE'] for b in task.build_blocks_for_participant(pid, task.BLOCKS))
            cells.add((order, mapping['key_black_name']))
            for cfg in configs:
                with self.subTest(pid=pid, condition=cfg['CONDITION_CODE']):
                    plans = [dict(events=[(mapping['key_black_name'], .1, True)]),
                             dict(events=[(mapping['key_white_name'], .2, True)])]
                    timing = FakeTiming(plans)
                    row, state = self.run_trial(timing, cfg, pid)
                    self.assertTrue(row['decision1_correct'])
                    self.assertFalse(row['decision2_correct'])
                    self.assertTrue(row['changed_response'])
                    self.assertEqual(row['feedback'], 'INCORRECT')
                    self.assertEqual(row['decision2_display'], 'blank_response')
                    self.assertIsNone(row['trial_deadline_s'])
                    self.assertTrue(all(w.deadline_s is None for w in timing.windows))
                    expected_aid = cfg['AID_CONDITION'] in ('aid_first', 'stimulus_first', 'simultaneous')
                    self.assertEqual(row['aid_accuracy_setting'], .85 if expected_aid else None)
                    self.assertEqual(row['preview_display'], 'aid_only' if cfg['AID_CONDITION'] == 'aid_first' else 'masked_placeholder')
                    self.assertEqual(row['decision2_preview_display'], 'aid_only' if cfg['AID_CONDITION'] == 'stimulus_first' else 'masked_placeholder')
                    stimulus_frames = [f for f in timing.frames if f[1] == 'stimulus']
                    self.assertEqual(stimulus_frames[0][2]['visible'], cfg['AID_CONDITION'] == 'simultaneous')
                    self.assertEqual([ms for kind, ms in timing.waits if kind == 'fixation'], [500]*4)
                    self.assertEqual([ms for kind, ms in timing.waits if kind in ('aid', 'masked')], [1000]*2)
                    if cfg['name'] == 'PRACTICE':
                        self.assertEqual(state['staircase_target_accuracy'], .75)
                        self.assertGreater(state['delta_mean'], task.DELTA_INIT)
        self.assertEqual(len(cells), 12)

    def test_unlimited_phases_accept_slow_responses(self):
        timing = FakeTiming([dict(events=[('D', 20., True)])]*2, stall=.05)
        row, _ = self.run_trial(timing)
        self.assertAlmostEqual(row['decision1_rt_s'], 19.993, places=10)
        self.assertAlmostEqual(row['decision2_rt_s'], 19.993, places=10)

    def test_retained_single_decision_and_deadline_boundaries(self):
        cfg = dict(task.BLOCKS[0], AUTOMATION_ON=False, TRIAL_DEADLINE_MS=25)
        for offset, expected in ((.031999, 'BLACK'), (.032, 'TIMEOUT'), (.032001, 'TIMEOUT')):
            timing = FakeTiming([dict(events=[('D', offset, True)])], stall=.05)
            row, _ = self.run_trial(timing, cfg)
            self.assertEqual(row['decision1_response'], expected)
            self.assertIsNone(row['decision2_rt_s'])
            self.assertTrue(all(row[f'decision2_{field}'] is None for field in rt.PHASE_FIELDS))
            if expected == 'TIMEOUT':
                self.assertIsNone(row['decision1_rt_s'])
                self.assertIsNone(row['decision1_response_timestamp_s'])
            else:
                self.assertAlmostEqual(row['decision1_rt_s'], .024999, places=11)
                self.assertGreater(row['decision1_response_readout_delay_s'], .025)

    def test_other_response_helpers_keep_audit(self):
        mapping = task.key_mapping_for_participant(1)
        for helper in ('aid', 'masked'):
            timing = FakeTiming([{}])
            with self.patches(timing):
                if helper == 'aid':
                    result = task.collect_aid_only_response(None, timing, mapping, {}, {}, timing=timing)
                else:
                    result = task.collect_masked_response(None, timing, None, mapping, {}, timing=timing)
            self.assertTrue(set(rt.AUDIT_FIELDS) <= result.keys())
            self.assertAlmostEqual(result['rt_ms'], 1000 * result['rt_s'])
            self.assertEqual(timing.stops, 1)

    def test_hard_quit_before_and_after_onset_stops_and_releases(self):
        quit_event = pygame.event.Event(pygame.KEYDOWN, key=pygame.K_q, mod=pygame.KMOD_ALT)
        for batches in ([[quit_event]], [[], [quit_event]]):
            timing = FakeTiming([{}])
            with self.patches(timing, iter(batches).__next__), patch.object(task, 'PsychtoolboxKeyboard', return_value=timing):
                with self.assertRaises(SystemExit):
                    task.collect_key_response(None, timing, task.key_mapping_for_participant(1), lambda ms: timing.draw('test'))
            self.assertFalse(timing.active)
            self.assertTrue(timing.closed)
            self.assertEqual(timing.stops, 1)

    def test_render_and_readout_errors_release_owned_backend(self):
        for error_at in ('draw', 'drain'):
            timing = FakeTiming([{}])
            with self.patches(timing), patch.object(task, 'PsychtoolboxKeyboard', return_value=timing), patch.object(timing, error_at, side_effect=rt.TimingError('injected failure')):
                with self.assertRaisesRegex(rt.TimingError, 'injected failure'):
                    task.collect_key_response(None, timing, task.key_mapping_for_participant(1), lambda ms: timing.draw('test'))
            self.assertFalse(timing.active)
            self.assertTrue(timing.closed)
            self.assertEqual(timing.stops, 1)

    def test_main_owns_backend_on_completion_quit_and_error(self):
        for failure in (None, SystemExit(), ValueError('render failed')):
            timing = FakeTiming()
            with patch.object(task, 'parse_cli_args'), patch.object(pygame, 'init'), patch.object(pygame, 'quit') as quit_mock, patch.object(pygame.key, 'set_repeat'), patch.object(task, 'PsychtoolboxKeyboard', return_value=timing), patch.object(task, 'run_experiment', side_effect=failure):
                if failure is None:
                    task.main()
                else:
                    with self.assertRaises(type(failure)):
                        task.main()
                self.assertTrue(timing.closed)
                quit_mock.assert_called_once()


@unittest.skipUnless(shutil.which('Rscript'), 'Rscript required for real collation checks')
class CollationTests(TaskHarness, unittest.TestCase):
    def write_run(self, source, pid, stamp, rows):
        rows = [dict(r, participant_id=pid, run_timestamp=stamp) for r in rows]
        rating = dict(participant_id=pid, run_timestamp=stamp, condition_code='MANUAL', response=4, question_idx=1)
        slider = dict(participant_id=pid, run_timestamp=stamp, condition_code='MANUAL', response_percent=70, question_key='self_accuracy')
        with patch('builtins.print'):
            task.save_combined_outputs(source, pid, stamp, rows, [slider], [rating])

    def collate(self, source, dest, expected_error=None):
        run = subprocess.run([shutil.which('Rscript'), str(ROOT / 'collate_data.R'), str(source), str(dest)],
                             capture_output=True, text=True, timeout=60)
        if expected_error:
            self.assertNotEqual(run.returncode, 0)
            self.assertIn(expected_error, run.stderr)
            return
        self.assertEqual(run.returncode, 0, run.stderr[-4000:])
        return {p.name: list(csv.DictReader(io.StringIO(p.read_text()))) for p in dest.glob('*.csv')}

    def test_mixed_exports_precision_audit_and_p59_selection(self):
        old, _ = self.run_trial(FakeTiming())
        for field in rt.PHASE_AUDIT_FIELDS:
            old.pop(field)
        old.update(decision1_rt_s=-.012345678901234, decision2_rt_s=0.)
        new = []
        for trial, offset in enumerate((.157123456789, .157623456789), 1):
            row, _ = self.run_trial(FakeTiming([dict(events=[('D', offset, True)])]*2), pid=2, trial=trial)
            new.append(row)
        single_cfg = dict(task.BLOCKS[0], AUTOMATION_ON=False, TRIAL_DEADLINE_MS=25)
        timeout, _ = self.run_trial(FakeTiming([dict(events=[])], stall=.05), single_cfg, pid=2, trial=3)
        new.append(timeout)
        with tempfile.TemporaryDirectory() as td:
            source, dest = Path(td) / 'source', Path(td) / 'collated'
            source.mkdir()
            self.write_run(source, 1, '20260101_120000', [old])
            self.write_run(source, 2, '20260101_120000', new)
            self.write_run(source, 59, '20260903_120309', [old])
            self.write_run(source, 59, '20260924_110831', [new[0]])
            # Raw combined exports retain all numerical digits before R runs.
            raw = list(csv.DictReader(io.StringIO((source / 'results_p002_20260101_120000_b00_ALL.csv').read_text())))
            for d in (1, 2):
                for idx in (0, 1):
                    self.assertEqual(float(raw[idx][f'decision{d}_rt_s']), new[idx][f'decision{d}_rt_s'])
                self.assertAlmostEqual(float(raw[1][f'decision{d}_rt_s']) - float(raw[0][f'decision{d}_rt_s']), .0005, places=11)
            files = self.collate(source, dest)
            for name in ('data_virus_all.csv', 'data_virus.csv'):
                rows = files[name]
                self.assertEqual(len(rows), 5)
                historical = next(r for r in rows if r['participant_id'] == '1')
                self.assertEqual(float(historical['decision1_rt_s']), -.012345678901234)
                self.assertEqual(float(historical['decision2_rt_s']), 0.)
                self.assertTrue(all(historical[k] in ('', 'NA') for k in rt.PHASE_AUDIT_FIELDS))
                corrected = [r for r in rows if r['participant_id'] == '2']
                for idx, expected_rt in enumerate((.150123456789, .150623456789)):
                    for d in (1, 2):
                        actual = corrected[idx]
                        self.assertAlmostEqual(float(actual[f'decision{d}_rt_s']), expected_rt, places=11)
                        for suffix in rt.PHASE_FIELDS:
                            field = f'decision{d}_{suffix}'
                            self.assertAlmostEqual(float(actual[field]), new[idx][field], places=10)
                    self.assertEqual(actual['rt_timing_version'], rt.TIMING_VERSION)
                    self.assertEqual(actual['rt_backend_version'], rt.PTB_VERSION)
                for d in (1, 2):
                    self.assertAlmostEqual(float(corrected[1][f'decision{d}_rt_s']) - float(corrected[0][f'decision{d}_rt_s']), .0005, places=11)
                self.assertEqual(corrected[2]['decision1_response'], 'TIMEOUT')
                self.assertIn(corrected[2]['decision1_rt_s'], ('', 'NA'))
                self.assertIn(corrected[2]['decision2_onset_s'], ('', 'NA'))
            manifest = files['collation_manifest.csv']
            p59 = [r for r in manifest if r['participant_id'] == '59']
            self.assertEqual(len(p59), 3)
            self.assertTrue(all(r['run_timestamp'] == '20260924_110831' for r in p59))
            for name in ('data_virus_postblock_all.csv', 'data_virus_sliders_all.csv'):
                p59 = [r for r in files[name] if r['participant_id'] == '59']
                self.assertEqual(p59[0]['run_timestamp'], '20260924_110831')

    def test_p59_old_run_and_missing_matching_family_are_still_rejected(self):
        row, _ = self.run_trial(FakeTiming())
        with tempfile.TemporaryDirectory() as td:
            source, dest = Path(td) / 'source', Path(td) / 'dest'
            self.write_run(source, 59, '20260903_120309', [row])
            self.collate(source, dest, 'earlier p59 run was excluded')
            self.write_run(source, 59, '20260924_110831', [row])
            (source / 'results_p059_20260924_110831_b00_POSTBLOCK_SLIDERS_ALL.csv').unlink()
            self.collate(source, dest, 'no fallback to older runs')

    def test_legacy_field_normalisation_is_retained(self):
        row, _ = self.run_trial(FakeTiming())
        for field in rt.PHASE_AUDIT_FIELDS:
            row.pop(field)
        for current, legacy in (
            ('condition_code', 'condition_deadline_code'), ('trial', 'trial_idx'),
            ('decision1_response', 'initial_response'), ('decision1_correct', 'initial_correct'),
            ('decision1_rt_s', 'initial_rt_s'), ('decision2_response', 'final_response'),
            ('decision2_correct', 'final_correct'), ('decision2_rt_s', 'final_rt_s'),
        ):
            row[legacy] = row.pop(current)
        with tempfile.TemporaryDirectory() as td:
            source, dest = Path(td) / 'source', Path(td) / 'dest'
            self.write_run(source, 1, '20260101_120000', [row])
            files = self.collate(source, dest)
            for name in ('data_virus_all.csv', 'data_virus.csv'):
                result = files[name][0]
                self.assertEqual(result['condition_code'], 'MANUAL')
                self.assertAlmostEqual(float(result['decision1_rt_s']), .150123456789, places=11)
                self.assertIn(result['rt_timing_version'], ('', 'NA'))


if __name__ == '__main__':
    unittest.main()
