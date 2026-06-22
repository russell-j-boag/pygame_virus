"""
Render key Virus Detection Task screens to PNG without running full blocks.

Use the r-pygame interpreter from AGENTS.md, for example:

    /Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python \
        python/capture_virus_task_screenshots.py --overwrite
"""

from __future__ import annotations

import argparse
import os
import random
import re
import shutil
import sys
import warnings
from pathlib import Path

os.environ.setdefault("PYGAME_HIDE_SUPPORT_PROMPT", "1")
warnings.filterwarnings(
    "ignore",
    message="pkg_resources is deprecated as an API.*",
    category=UserWarning,
)

import pygame

import virus_task as task


DEFAULT_OUTPUT_DIR = Path("virus_task_screenshots")
DEFAULT_RESOLUTION = "1512x982"
DEFAULT_PARTICIPANT = 1
FILENAME_SAFE_RE = re.compile(r"[^a-z0-9]+")

FEEDBACK_EXAMPLES = {
    "CAL": ("CORRECT", task.FEEDBACK_CORRECT_COLOR),
    "MAN": ("INCORRECT", task.FEEDBACK_ERROR_COLOR),
    "REL_DROP": ("TOO SLOW", task.FEEDBACK_SLOW_COLOR),
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Render key screens from python/virus_task.py to PNG without "
            "running full calibration/manual/automation blocks."
        )
    )
    parser.add_argument(
        "--participant",
        type=int,
        default=DEFAULT_PARTICIPANT,
        help=f"Representative participant path to render. Default: {DEFAULT_PARTICIPANT}",
    )
    parser.add_argument(
        "--output-dir",
        default=str(DEFAULT_OUTPUT_DIR),
        help=f"Directory for PNG files. Default: {DEFAULT_OUTPUT_DIR}",
    )
    parser.add_argument(
        "--resolution",
        default=DEFAULT_RESOLUTION,
        help=(
            f"Capture size. Default: {DEFAULT_RESOLUTION}. Use 'current' to detect "
            "the current display size, or pass another explicit WIDTHxHEIGHT value."
        ),
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Replace an existing output directory before writing PNG files.",
    )
    return parser.parse_args()


def parse_explicit_resolution(value: str) -> tuple[int, int] | None:
    match = re.fullmatch(r"\s*(\d+)\s*x\s*(\d+)\s*", value.lower())
    if match is None:
        return None

    width = int(match.group(1))
    height = int(match.group(2))
    if width <= 0 or height <= 0:
        raise ValueError("resolution dimensions must be positive")
    return width, height


def detect_current_display_size() -> tuple[int, int]:
    try:
        pygame.display.init()
        if hasattr(pygame.display, "get_desktop_sizes"):
            sizes = pygame.display.get_desktop_sizes()
            sizes = [size for size in sizes if size[0] > 0 and size[1] > 0]
            if sizes:
                return sizes[0]

        info = pygame.display.Info()
        if info.current_w > 0 and info.current_h > 0:
            return info.current_w, info.current_h
    except pygame.error as exc:
        raise RuntimeError(str(exc)) from exc

    raise RuntimeError("Pygame could not determine the current display size")


def resolve_capture_size(value: str) -> tuple[int, int]:
    explicit = parse_explicit_resolution(value)
    if explicit is not None:
        return explicit

    if value.lower() == "current":
        return detect_current_display_size()

    raise ValueError(
        f"invalid --resolution {value!r}; use 'current' or WIDTHxHEIGHT"
    )


def initialize_task_metrics(width: int, height: int) -> None:
    task.WIDTH = width
    task.HEIGHT = height
    task.UI_SCALE = task.compute_ui_scale(width, height)

    task.DISH_RADIUS = task.S(task.DISH_RADIUS_BASE)
    task.DOT_RADIUS = max(1, task.S(task.DOT_RADIUS_BASE))

    task.VEL_WANDER_SD = task.SF(task.VEL_WANDER_SD_BASE)
    task.VEL_MAX = task.SF(task.VEL_MAX_BASE)
    task.VEL_INIT_RANGE = task.SF(task.VEL_INIT_RANGE_BASE)

    task.FIX_SIZE = task.S(task.FIX_SIZE_BASE)
    task.FIX_THICKNESS = max(1, task.S(task.FIX_THICKNESS_BASE))

    task.PB_W = task.S(task.PB_W_BASE)
    task.PB_H = task.S(task.PB_H_BASE)
    task.PB_PAD = task.S(task.PB_PAD_BASE)


def prepare_output_dir(output_dir: Path, overwrite: bool) -> None:
    if output_dir.exists():
        if not output_dir.is_dir():
            raise RuntimeError(f"output path exists and is not a directory: {output_dir}")
        if overwrite:
            shutil.rmtree(output_dir)
        elif any(output_dir.glob("*.png")):
            raise RuntimeError(
                f"{output_dir} already contains PNG files; pass --overwrite to replace them"
            )

    output_dir.mkdir(parents=True, exist_ok=True)


def slugify(label: str) -> str:
    slug = FILENAME_SAFE_RE.sub("_", label.lower()).strip("_")
    return slug or "screen"


class ScreenshotWriter:
    def __init__(self, output_dir: Path):
        self.output_dir = output_dir
        self.index = 0
        self.paths: list[Path] = []

    def save(self, surface: pygame.Surface, label: str) -> Path:
        self.index += 1
        path = self.output_dir / f"{self.index:03d}_{slugify(label)}.png"
        pygame.image.save(surface, str(path))
        self.paths.append(path)
        return path


def condition_code(block_cfg: dict) -> str:
    return task.block_condition_code(block_cfg)


def block_slug(block_cfg: dict) -> str:
    return f"b{block_cfg['block_idx']:02d}_{condition_code(block_cfg)}"


def block_slider_items(block_name: str) -> list[dict]:
    if block_name in ("CALIBRATION", "MANUAL"):
        return task.SLIDER_ITEMS_MANUAL
    if block_name == "AUTOMATION":
        return task.SLIDER_ITEMS_AUTOMATION
    return []


def build_participant_blocks(participant_id: int) -> list[dict]:
    blocks = []
    for block_idx, block in enumerate(
        task.build_blocks_for_participant(participant_id, task.BLOCKS),
        start=1,
    ):
        cfg = task.copy_block_config(block)
        cfg["block_idx"] = block_idx
        cfg["participant_id"] = participant_id
        blocks.append(cfg)
    return blocks


def draw_example_trial(surface, fonts, keymap, block_cfg, trial_number: int = 1):
    trial_number = max(1, min(block_cfg["N_TRIALS"], trial_number))
    trials_left = block_cfg["N_TRIALS"] - trial_number + 1
    center = (task.WIDTH // 2, task.HEIGHT // 2 + task.S(20))
    dot_layer = pygame.Surface((task.WIDTH, task.HEIGHT), pygame.SRCALPHA)

    vblack_prop = 0.56
    dots, n_vblack, n_vwhite = task.make_trial_dots(
        task.N_DOTS,
        vblack_prop,
        center,
        task.DISH_RADIUS,
    )
    stimulus = "BLACK" if n_vblack > n_vwhite else "WHITE"

    aid_label = None
    if block_cfg["AUTOMATION_ON"]:
        aid_label = stimulus

    evidence_black_pct = (n_vblack / task.N_DOTS) * 100.0
    evidence_white_pct = (n_vwhite / task.N_DOTS) * 100.0

    ui_payload = {
        "fonts": fonts,
        "key_names": {
            "black": keymap["key_black_name"],
            "white": keymap["key_white_name"],
        },
        "trials_left": trials_left,
        "n_trials": block_cfg["N_TRIALS"],
    }

    if block_cfg["AUTOMATION_ON"]:
        aid_mode = "automation"
    elif block_cfg.get("SHOW_AID_MASKED", False):
        aid_mode = "masked"
    else:
        aid_mode = "none"

    task.draw_trial_frame(
        surface,
        dot_layer,
        dots,
        center,
        {
            "mode": aid_mode,
            "label": aid_label,
            "visible": True,
            "transparency_level": block_cfg.get("AID_TRANSPARENCY", "none"),
            "evidence_black_pct": evidence_black_pct,
            "evidence_white_pct": evidence_white_pct,
        },
        ui_payload,
        ms_left=task.trial_deadline_ms_for_block(block_cfg),
    )


def render_trial_sequence(writer, surface, fonts, keymap, block_cfg, prefix: str, trial_number: int = 1):
    task.draw_fixation_screen_state(surface)
    writer.save(surface, f"{prefix}_trial_fixation")

    draw_example_trial(surface, fonts, keymap, block_cfg, trial_number=trial_number)
    writer.save(surface, f"{prefix}_trial_stimulus")

    feedback_msg, feedback_color = FEEDBACK_EXAMPLES[condition_code(block_cfg)]
    task.draw_feedback_screen_state(
        surface,
        fonts["title"],
        feedback_msg,
        bg_color=task.BG,
        text_color=feedback_color,
        prompt_text="Press any key to continue",
        prompt_font=fonts["body"],
        prompt_color=task.WHITE,
    )
    writer.save(surface, f"{prefix}_trial_feedback_{feedback_msg.lower().replace(' ', '_')}")


def render_block(writer, surface, fonts, keymap, block_cfg):
    prefix = block_slug(block_cfg)

    task.draw_main_instruction_screen_state(
        surface,
        fonts["title"],
        fonts["body"],
        fonts["body_bold"],
        key_black_name=keymap["key_black_name"],
        key_white_name=keymap["key_white_name"],
        button_enabled=True,
    )
    writer.save(surface, f"{prefix}_main_instructions")

    payload = task.get_block_instruction_payload(block_cfg["name"], block_cfg=block_cfg)
    for slide_idx in range(len(payload["slides"])):
        task.draw_block_instruction_slide_state(
            surface,
            fonts["title"],
            fonts["body"],
            block_name=block_cfg["name"],
            block_cfg=block_cfg,
            slide_idx=slide_idx,
            button_enabled=True,
        )
        writer.save(surface, f"{prefix}_block_instruction_{slide_idx + 1:02d}")

    task.draw_begin_block_screen_state(surface, fonts["body"], block_cfg)
    writer.save(surface, f"{prefix}_begin_block")

    if block_cfg["name"] == "AUTOMATION":
        reliability_block_size = block_cfg.get(
            "DYNAMIC_RELIABILITY_BLOCK_SIZE",
            task.RELIABILITY_BLOCK_SIZE,
        )
        for phase_idx, _reliability in enumerate(task.dynamic_reliability_schedule_for_participant(
            block_cfg["participant_id"]
        ), start=1):
            trial_number = ((phase_idx - 1) * reliability_block_size) + 1
            reliability_metadata = task.dynamic_reliability_metadata_for_trial(
                block_cfg,
                trial_number,
            )
            phase_prefix = f"{prefix}_{reliability_metadata['reliability_phase_label'].lower()}"

            render_trial_sequence(
                writer,
                surface,
                fonts,
                keymap,
                block_cfg,
                phase_prefix,
                trial_number=trial_number,
            )
    else:
        render_trial_sequence(writer, surface, fonts, keymap, block_cfg, prefix)

    task.draw_block_complete_screen_state(
        surface,
        fonts["body"],
        block_cfg["name"],
        block_cfg=block_cfg,
    )
    writer.save(surface, f"{prefix}_block_complete")

    for item_idx, item in enumerate(block_slider_items(block_cfg["name"]), start=1):
        task.draw_slider_question_screen_state(
            surface,
            fonts["title"],
            fonts["body"],
            question=item["question"],
            initial_value=50,
            slider_moved=False,
            button_enabled=False,
        )
        writer.save(surface, f"{prefix}_postblock_slider_{item_idx:02d}_{item['key']}")

    if block_cfg["name"] == "AUTOMATION":
        task.draw_questionnaire_intro_screen_state(surface, fonts["title"], fonts["body"])
        writer.save(surface, f"{prefix}_questionnaire_intro")

        for item_idx, item in enumerate(task.QUESTION_ITEMS, start=1):
            task.draw_likert_question_screen_state(
                surface,
                fonts["body"],
                item,
                slider_moved=False,
                button_enabled=False,
            )
            writer.save(surface, f"{prefix}_questionnaire_item_{item_idx:02d}")


def render_screens(output_dir: Path, width: int, height: int, participant_id: int) -> list[Path]:
    random.seed(1234)
    initialize_task_metrics(width, height)
    fonts = task.load_ui_fonts()
    surface = pygame.Surface((width, height))
    writer = ScreenshotWriter(output_dir)

    keymap = task.key_mapping_for_participant(participant_id)
    blocks = build_participant_blocks(participant_id)

    task.draw_participant_number_screen_state(
        surface,
        fonts["body"],
        entry="",
        active=True,
        cursor_on=True,
    )
    writer.save(surface, "participant_number")

    for block_cfg in blocks:
        render_block(writer, surface, fonts, keymap, block_cfg)

    task.draw_final_complete_screen_state(surface, fonts["body"], perf_score=80.0)
    writer.save(surface, "experiment_complete")

    return writer.paths


def main() -> int:
    args = parse_args()
    output_dir = Path(args.output_dir)

    if args.participant < 1:
        print("capture_virus_task_screenshots: error: --participant must be >= 1", file=sys.stderr)
        return 1

    try:
        pygame.font.init()
        width, height = resolve_capture_size(args.resolution)
        prepare_output_dir(output_dir, args.overwrite)
        written = render_screens(output_dir, width, height, args.participant)
    except (RuntimeError, ValueError, pygame.error, OSError) as exc:
        print(f"capture_virus_task_screenshots: error: {exc}", file=sys.stderr)
        if args.resolution.lower() == "current":
            print(
                "Tip: if this session cannot access the display, retry with an "
                "explicit display size such as --resolution 1440x900.",
                file=sys.stderr,
            )
        return 1
    finally:
        pygame.quit()

    print(f"Wrote {len(written)} virus task screenshots to {output_dir}")
    print(f"Participant: {args.participant}")
    print(f"Resolution: {width}x{height}")
    for path in written:
        print(path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
