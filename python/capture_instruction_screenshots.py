"""
Render every standalone instruction slide to PNG.

Use the r-pygame interpreter from AGENTS.md, for example:

    /Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python \
        python/capture_instruction_screenshots.py --resolution 1280x720 --overwrite
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

import instructions


DEFAULT_OUTPUT_DIR = Path("instruction_screenshots")
DEFAULT_RESOLUTION = "1280x720"
FILENAME_SAFE_RE = re.compile(r"[^a-z0-9]+")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Render each slide in python/instructions.py to a PNG using the "
            "same drawing code seen by participants."
        )
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
            "Capture size. Use 'current' for the current display size, or "
            "an explicit WIDTHxHEIGHT value such as 1280x720."
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


def initialize_instruction_metrics(width: int, height: int) -> None:
    instructions.WIDTH = width
    instructions.HEIGHT = height
    instructions.UI_SCALE = instructions.compute_ui_scale(width, height)

    instructions.DISH_RADIUS = instructions.S(instructions.DISH_RADIUS_BASE)
    instructions.DOT_RADIUS = max(1, instructions.S(instructions.DOT_RADIUS_BASE))
    instructions.PB_W = instructions.S(instructions.PB_W_BASE)
    instructions.PB_H = instructions.S(instructions.PB_H_BASE)
    instructions.PB_PAD = instructions.S(instructions.PB_PAD_BASE)

    instructions.VEL_WANDER_SD = instructions.SF(instructions.VEL_WANDER_SD_BASE)
    instructions.VEL_MAX = instructions.SF(instructions.VEL_MAX_BASE)
    instructions.VEL_INIT_RANGE = instructions.SF(instructions.VEL_INIT_RANGE_BASE)
    instructions.EXAMPLE_DOTS_CACHE = None


def load_instruction_fonts() -> tuple[pygame.font.Font, ...]:
    return (
        instructions.load_font(
            instructions.FONT_LIGHT,
            max(12, instructions.S(instructions.FONT_TITLE_BASE)),
        ),
        instructions.load_font(
            instructions.FONT_LIGHT,
            max(10, instructions.S(instructions.FONT_BODY_BASE)),
        ),
        instructions.load_font(
            instructions.FONT_LIGHT,
            max(10, instructions.S(instructions.FONT_BUTTON_BASE)),
        ),
        instructions.load_font(
            instructions.FONT_LIGHT,
            max(9, instructions.S(instructions.FONT_SMALL_BASE)),
        ),
        instructions.load_font(
            instructions.FONT_LIGHT,
            max(9, instructions.S(instructions.FONT_AID_LABEL_BASE)),
        ),
        instructions.load_font(
            instructions.FONT_BOLD,
            max(10, instructions.S(instructions.FONT_AID_BASE)),
        ),
    )


def slide_filename(slide_idx: int, slide: dict) -> str:
    title = slide.get("title")
    if title:
        label = str(title)
    else:
        label = str(slide.get("kind", "slide"))
        callout = slide.get("callout")
        if callout:
            label = f"{label}_{callout}"
        aid_label = slide.get("aid_label")
        if aid_label:
            label = f"{label}_{aid_label}"

    slug = FILENAME_SAFE_RE.sub("_", label.lower()).strip("_")
    if not slug:
        slug = "slide"
    return f"{slide_idx + 1:03d}_{slug}.png"


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


def render_slides(output_dir: Path, width: int, height: int) -> list[Path]:
    random.seed(1234)
    initialize_instruction_metrics(width, height)
    fonts = load_instruction_fonts()
    surface = pygame.Surface((width, height))

    written = []
    for slide_idx, slide in enumerate(instructions.SLIDES):
        instructions.draw_slide(surface, *fonts, slide_idx)
        out_path = output_dir / slide_filename(slide_idx, slide)
        pygame.image.save(surface, str(out_path))
        written.append(out_path)

    return written


def main() -> int:
    args = parse_args()
    output_dir = Path(args.output_dir)

    try:
        pygame.font.init()
        width, height = resolve_capture_size(args.resolution)
        prepare_output_dir(output_dir, args.overwrite)
        written = render_slides(output_dir, width, height)
    except (RuntimeError, ValueError, pygame.error, OSError) as exc:
        print(f"capture_instruction_screenshots: error: {exc}", file=sys.stderr)
        if args.resolution.lower() == "current":
            print(
                "Tip: if this session cannot access the display, retry with "
                "--resolution 1280x720.",
                file=sys.stderr,
            )
        return 1
    finally:
        pygame.quit()

    print(f"Wrote {len(written)} instruction screenshots to {output_dir}")
    print(f"Resolution: {width}x{height}")
    for path in written:
        print(path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
