#!/usr/bin/env python3
"""Render one enlarged Automated Decision Aid instruction screenshot.

This is intentionally independent of the live task modules.  It recreates the
viewer-facing automation-intro screen at 1512x982 with every visible font set
to exactly twice the size used by the current screenshot renderer.
"""

from __future__ import annotations

import argparse
import math
import os
import random
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


WIDTH, HEIGHT = 1512, 982
BACKGROUND = (128, 128, 128)
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
LIGHT_GREY = (170, 170, 170)
CALLOUT_FILL = (30, 30, 30)

SCRIPT_DIR = Path(__file__).resolve().parent
FONT_DIR = SCRIPT_DIR / "fonts"
DEFAULT_OUTPUT = SCRIPT_DIR / "automated_decision_aid_intro_2x.png"

# At 1512x982, the live renderer produces 28/21/26/24/38 px fonts.  These
# values are exactly doubled; they are not fit-to-window fallback sizes.
FONT_SIZES = {
    "body": 56,
    "small": 42,
    "button": 52,
    "aid_label": 48,
    "aid_value": 76,
}

CALLOUT_TEXT = (
    "In some parts of this study you will be assisted "
    "by an Automated Decision Aid"
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Render the one-off 2x Automated Decision Aid instruction PNG."
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=DEFAULT_OUTPUT,
        help=f"PNG destination (default: {DEFAULT_OUTPUT.name} beside this script).",
    )
    return parser.parse_args()


def load_fonts() -> dict[str, pygame.font.Font]:
    light_path = FONT_DIR / "Roboto-Light.ttf"
    bold_path = FONT_DIR / "Roboto-Bold.ttf"
    missing = [str(path) for path in (light_path, bold_path) if not path.is_file()]
    if missing:
        raise RuntimeError("missing bundled font file(s): " + ", ".join(missing))

    return {
        "body": pygame.font.Font(str(light_path), FONT_SIZES["body"]),
        "small": pygame.font.Font(str(light_path), FONT_SIZES["small"]),
        "button": pygame.font.Font(str(light_path), FONT_SIZES["button"]),
        "aid_label": pygame.font.Font(str(light_path), FONT_SIZES["aid_label"]),
        "aid_value": pygame.font.Font(str(bold_path), FONT_SIZES["aid_value"]),
    }


def blit_text(
    surface: pygame.Surface,
    font: pygame.font.Font,
    text: str,
    color: tuple[int, int, int],
    *,
    topleft: tuple[int, int] | None = None,
    center: tuple[int, int] | None = None,
    midtop: tuple[int, int] | None = None,
) -> pygame.Rect:
    image = font.render(text, True, color)
    rect = image.get_rect()
    anchors = [topleft is not None, center is not None, midtop is not None]
    if sum(anchors) != 1:
        raise ValueError("provide exactly one text anchor")
    if topleft is not None:
        rect.topleft = topleft
    elif center is not None:
        rect.center = center
    else:
        rect.midtop = midtop
    surface.blit(image, rect)
    return rect


def wrap_text(text: str, font: pygame.font.Font, max_width: int) -> list[str]:
    lines: list[str] = []
    current: list[str] = []
    for word in text.split():
        candidate = " ".join([*current, word])
        if current and font.size(candidate)[0] > max_width:
            lines.append(" ".join(current))
            current = [word]
        else:
            current.append(word)
    if current:
        lines.append(" ".join(current))
    return lines


def draw_arrow(
    surface: pygame.Surface,
    start: tuple[int, int],
    end: tuple[int, int],
    *,
    width: int = 3,
    head_length: int = 18,
) -> None:
    pygame.draw.line(surface, WHITE, start, end, width)
    angle = math.atan2(end[1] - start[1], end[0] - start[0])
    for offset in (math.radians(158), -math.radians(158)):
        endpoint = (
            end[0] + head_length * math.cos(angle + offset),
            end[1] + head_length * math.sin(angle + offset),
        )
        pygame.draw.line(surface, WHITE, end, endpoint, width)


def draw_button(
    surface: pygame.Surface,
    rect: pygame.Rect,
    label: str,
    font: pygame.font.Font,
) -> pygame.Rect:
    pygame.draw.rect(surface, (50, 50, 50), rect, border_radius=12)
    pygame.draw.rect(surface, WHITE, rect, 4, border_radius=12)
    return blit_text(surface, font, label, WHITE, center=rect.center)


def make_dots(
    count: int,
    black_proportion: float,
    center: tuple[int, int],
    radius: int,
    dot_radius: int,
) -> list[tuple[int, int, tuple[int, int, int]]]:
    rng = random.Random(1234)
    points: list[tuple[int, int]] = []
    usable_radius = radius - dot_radius - 2
    for _ in range(count):
        angle = rng.random() * 2 * math.pi
        distance = usable_radius * math.sqrt(rng.random())
        points.append(
            (
                round(center[0] + distance * math.cos(angle)),
                round(center[1] + distance * math.sin(angle)),
            )
        )
    rng.shuffle(points)

    black_count = round(count * black_proportion)
    colors = [BLACK] * black_count + [WHITE] * (count - black_count)
    rng.shuffle(colors)
    return [(x, y, color) for (x, y), color in zip(points, colors)]


def assert_inside(label: str, rect: pygame.Rect, bounds: pygame.Rect) -> None:
    if not bounds.contains(rect):
        raise RuntimeError(f"{label} is outside the {WIDTH}x{HEIGHT} canvas: {rect}")


def assert_disjoint(
    first_label: str,
    first: pygame.Rect,
    second_label: str,
    second: pygame.Rect,
) -> None:
    if first.colliderect(second):
        raise RuntimeError(
            f"unintended overlap between {first_label} {first} and "
            f"{second_label} {second}"
        )


def render(output_path: Path) -> None:
    pygame.font.init()
    fonts = load_fonts()
    surface = pygame.Surface((WIDTH, HEIGHT))
    surface.fill(BACKGROUND)
    canvas = surface.get_rect()

    text_rects: dict[str, pygame.Rect] = {}

    # Top status text and progress display.
    text_rects["timer"] = blit_text(
        surface, fonts["body"], "0.8s", WHITE, topleft=(28, 18)
    )
    progress_bar = pygame.Rect(1180, 24, 284, 20)
    pygame.draw.rect(surface, LIGHT_GREY, progress_bar, 3, border_radius=8)
    pygame.draw.rect(
        surface,
        WHITE,
        pygame.Rect(progress_bar.x, progress_bar.y, 122, progress_bar.height),
        border_radius=8,
    )
    text_rects["progress"] = blit_text(
        surface,
        fonts["small"],
        "Samples left: 24",
        WHITE,
        topleft=(1168, 52),
    )

    # The aid, stimulus, and response group all share the screen's horizontal
    # center line.  This remains true even though the callout sits to the left.
    content_center_x = WIDTH // 2
    text_rects["aid_label"] = blit_text(
        surface,
        fonts["aid_label"],
        "AID JUDGES:",
        WHITE,
        midtop=(content_center_x, 8),
    )
    text_rects["aid_value"] = blit_text(
        surface,
        fonts["aid_value"],
        "BLACK",
        BLACK,
        midtop=(content_center_x, text_rects["aid_label"].bottom + 2),
    )

    # Draw the centered stimulus before the callout so the callout can sit on
    # top where the two intentionally overlap slightly.
    dish_center = (content_center_x, 506)
    dish_radius = 230
    dish_bounds = pygame.Rect(
        dish_center[0] - dish_radius - 4,
        dish_center[1] - dish_radius - 4,
        2 * dish_radius + 8,
        2 * dish_radius + 8,
    )
    pygame.draw.circle(surface, BACKGROUND, dish_center, dish_radius)
    pygame.draw.circle(surface, BLACK, dish_center, dish_radius + 3, 3)
    pygame.draw.circle(surface, BLACK, dish_center, dish_radius + 1, 1)

    dot_layer = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
    for x, y, color in make_dots(3600, 0.56, dish_center, dish_radius, 3):
        pygame.draw.circle(dot_layer, (*color, 255), (x, y), 3)
    surface.blit(dot_layer, (0, 0))

    # The narrower callout wraps its title to two lines and retains the
    # planned three-line body at the doubled font sizes.
    callout_padding_x = 24
    callout_padding_y = 20
    title_line_gap = 4
    title_body_gap = 14
    body_line_gap = 10
    callout_x, callout_y, callout_w = 20, 132, 542
    callout_content_w = callout_w - 2 * callout_padding_x
    title_lines = wrap_text(
        "Automated decision aid",
        fonts["body"],
        callout_content_w,
    )
    body_lines = wrap_text(
        CALLOUT_TEXT,
        fonts["small"],
        callout_content_w,
    )
    if title_lines != ["Automated decision", "aid"]:
        raise RuntimeError(f"unexpected callout title wrapping: {title_lines}")
    if body_lines != [
        "In some parts of this study",
        "you will be assisted by an",
        "Automated Decision Aid",
    ]:
        raise RuntimeError(f"unexpected callout wrapping: {body_lines}")

    callout_h = (
        2 * callout_padding_y
        + len(title_lines) * fonts["body"].get_height()
        + (len(title_lines) - 1) * title_line_gap
        + title_body_gap
        + len(body_lines) * fonts["small"].get_height()
        + (len(body_lines) - 1) * body_line_gap
    )
    callout = pygame.Rect(callout_x, callout_y, callout_w, callout_h)
    pygame.draw.rect(surface, CALLOUT_FILL, callout, border_radius=12)
    pygame.draw.rect(surface, WHITE, callout, 3, border_radius=12)

    line_y = callout.y + callout_padding_y
    for index, line in enumerate(title_lines, start=1):
        text_rects[f"callout_title_{index}"] = blit_text(
            surface,
            fonts["body"],
            line,
            WHITE,
            topleft=(callout.x + callout_padding_x, line_y),
        )
        line_y += fonts["body"].get_height() + title_line_gap
    line_y += title_body_gap - title_line_gap
    for index, line in enumerate(body_lines, start=1):
        text_rects[f"callout_body_{index}"] = blit_text(
            surface,
            fonts["small"],
            line,
            WHITE,
            topleft=(callout.x + callout_padding_x, line_y),
        )
        line_y += fonts["small"].get_height() + body_line_gap

    arrow_start = (callout.right, callout.y + 38)
    arrow_end = (text_rects["aid_value"].left - 18, text_rects["aid_value"].centery)
    draw_arrow(surface, arrow_start, arrow_end)

    # The response columns are symmetric around the same center line.
    response_y = 752
    response_column_offset = 115
    left_center_x = content_center_x - response_column_offset
    right_center_x = content_center_x + response_column_offset
    text_rects["v_black"] = blit_text(
        surface, fonts["small"], "V-BLACK", BLACK, midtop=(left_center_x, response_y)
    )
    text_rects["press_d"] = blit_text(
        surface,
        fonts["small"],
        "Press D",
        BLACK,
        midtop=(left_center_x, response_y + fonts["small"].get_height() + 4),
    )
    text_rects["v_white"] = blit_text(
        surface, fonts["small"], "V-WHITE", WHITE, midtop=(right_center_x, response_y)
    )
    text_rects["press_j"] = blit_text(
        surface,
        fonts["small"],
        "Press J",
        WHITE,
        midtop=(right_center_x, response_y + fonts["small"].get_height() + 4),
    )
    response_bounds = (
        text_rects["v_black"]
        .union(text_rects["press_d"])
        .union(text_rects["v_white"])
        .union(text_rects["press_j"])
    )

    back_button = pygame.Rect(40, 878, 260, 78)
    next_button = pygame.Rect(WIDTH - 40 - 260, 878, 260, 78)
    text_rects["back"] = draw_button(surface, back_button, "BACK", fonts["button"])
    text_rects["next"] = draw_button(surface, next_button, "NEXT", fonts["button"])

    # Fail loudly if a later edit breaks the containment/reflow guarantees.
    structural_rects = {
        "progress bar": progress_bar,
        "callout": callout,
        "dish": dish_bounds,
        "back button": back_button,
        "next button": next_button,
    }
    for label, rect in {**structural_rects, **text_rects}.items():
        assert_inside(label, rect, canvas)

    for title_label, title_rect in text_rects.items():
        for other_label, other_rect in text_rects.items():
            if title_label >= other_label:
                continue
            assert_disjoint(title_label, title_rect, other_label, other_rect)

    assert_disjoint("dish", dish_bounds, "response text", response_bounds)
    assert_disjoint("response text", response_bounds, "back button", back_button)
    assert_disjoint("response text", response_bounds, "next button", next_button)

    if text_rects["aid_label"].centerx != content_center_x:
        raise RuntimeError("aid label is not horizontally centered")
    if text_rects["aid_value"].centerx != content_center_x:
        raise RuntimeError("aid value is not horizontally centered")
    if dish_center[0] != content_center_x:
        raise RuntimeError("stimulus disc is not horizontally centered")
    if left_center_x + right_center_x != 2 * content_center_x:
        raise RuntimeError("response columns are not symmetric around the canvas center")

    for label, point in (("arrow start", arrow_start), ("arrow end", arrow_end)):
        if not canvas.collidepoint(point):
            raise RuntimeError(f"{label} is outside the canvas: {point}")

    output_path = output_path.expanduser().resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)
    pygame.image.save(surface, str(output_path))
    print(f"Wrote {output_path}")
    print(f"Resolution: {WIDTH}x{HEIGHT}")
    print("Font sizes: " + ", ".join(f"{name}={size}px" for name, size in FONT_SIZES.items()))
    print(f"Callout lines: {len(body_lines)}")


def main() -> int:
    args = parse_args()
    try:
        render(args.output)
    except (OSError, RuntimeError, ValueError, pygame.error) as exc:
        print(f"render_aid_instruction_2x: error: {exc}", file=sys.stderr)
        return 1
    finally:
        pygame.quit()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
