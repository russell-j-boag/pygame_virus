"""Native input preflight and interactive timestamp/readout diagnostic.

Run separately on every collection computer. This measures queue readout delay,
not physical keyboard or display accuracy. Only response keys D/J are recorded.
"""

import argparse
from datetime import datetime, timezone
import json
from pathlib import Path
import platform
import sys
import time

from response_timing import PsychtoolboxKeyboard, TimingError, TIMING_VERSION


KEYMAP = {"key_black_name": "D", "key_white_name": "J"}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--preflight", action="store_true", help="Check native queue APIs without opening a window.")
    parser.add_argument("--output", type=Path, help="New JSON diagnostic path; existing files are never overwritten.")
    args = parser.parse_args()
    if args.output and args.output.exists():
        parser.error(f"Output already exists: {args.output}")
    report = dict(platform=platform.platform(), architecture=platform.machine(),
                  python=sys.version, timing_version=TIMING_VERSION,
                  timestamp_utc=datetime.now(timezone.utc).isoformat(),
                  validation="native_preflight" if args.preflight else "native_interactive",
                  physical_accuracy_validated=False)
    with PsychtoolboxKeyboard() as timing:
        report.update(backend_version=timing.backend_version, native_queue_preflight="passed")
        if not args.preflight:
            report["trials"] = interactive_check(timing)
            report["answered_trials"] = sum(r["response"] != "TIMEOUT" for r in report["trials"])
            report["all_phases_and_stalls_answered"] = all(r["response"] != "TIMEOUT" for r in report["trials"])
    if args.output is None and not args.preflight:
        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S%fZ")
        args.output = Path("analysis_outputs/response_timing") / f"native_{sys.platform}_{stamp}.json"
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        with args.output.open("x") as handle:
            json.dump(report, handle, indent=2, allow_nan=False)
            handle.write("\n")
        print(f"Saved {args.output}")
    print(json.dumps(report, indent=2, allow_nan=False))


def interactive_check(timing):
    import pygame
    pygame.init()
    try:
        screen = pygame.display.set_mode((800, 360))
        pygame.display.set_caption("Aid-onset: two-decision native timing check")
        pygame.key.set_repeat()
        font = pygame.font.Font(None, 30)
        rows = []
        def controls():
            for event in pygame.event.get():
                if event.type == pygame.QUIT or (event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE):
                    raise TimingError("Interactive timing check cancelled; no completed report written.")

        def preview():
            # No native acquisition during the intervening preview. The next
            # phase takes a new held-state snapshot, just as the task does.
            screen.fill((245, 245, 245))
            screen.blit(font.render("Preview: ##### (no response)", True, (25, 25, 25)), (25, 100))
            pygame.display.flip()
            end = time.perf_counter() + 1.0
            while time.perf_counter() < end:
                controls()
                time.sleep(.005)

        for index, stall_ms in enumerate((0, 8, 20, 50), 1):
            for decision in (1, 2):
                preview()
                with timing.trial(KEYMAP) as responses:
                    screen.fill((245, 245, 245))
                    for y, msg in enumerate((f"Check {index}/4, Decision {decision}: press D or J once",
                                             f"Added readout stall: {stall_ms} ms; release between presses",
                                             "Esc cancels. Diagnostic timeout: 15 s (task is unlimited).")):
                        screen.blit(font.render(msg, True, (25, 25, 25)), (25, 30 + 45*y))
                    if decision == 1:
                        pygame.draw.circle(screen, (25, 25, 25), (400, 260), 45)
                    pygame.display.flip()
                    responses.mark_onset(timing.now(), 15.0)
                    while responses.response is None:
                        controls()
                        # The native queue timestamps while the main thread stalls.
                        time.sleep(max(stall_ms / 1000.0, .001))
                        fence_s = timing.now()
                        responses.consume(timing.drain())
                        responses.expire(fence_s)
                    rows.append(dict(trial=index, decision=decision,
                                     requested_stall_ms=stall_ms, **responses.result()))
        return rows
    finally:
        pygame.quit()


if __name__ == "__main__":
    try:
        main()
    except TimingError as exc:
        print(f"[TIMING ERROR] {exc}", file=sys.stderr)
        raise SystemExit(1) from exc
