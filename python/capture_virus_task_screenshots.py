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
    "PRACTICE": ("CORRECT", task.FEEDBACK_CORRECT_COLOR),
    "MANUAL": ("CORRECT", task.FEEDBACK_CORRECT_COLOR),
    "AIDFIRST": ("INCORRECT", task.FEEDBACK_ERROR_COLOR),
    "STIMFIRST": ("CORRECT", task.FEEDBACK_CORRECT_COLOR),
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Render key screens from python/virus_task.py to PNG without "
            "running full experiment blocks."
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


def block_slider_items(block_cfg: dict) -> list[dict]:
    if not task.ENABLE_POSTBLOCK_SLIDERS:
        return []
    if block_cfg["name"] != "AUTOMATION":
        return []
    if task.block_has_real_aid(block_cfg):
        return task.SLIDER_ITEMS_AUTOMATION
    return task.SLIDER_ITEMS_MANUAL


def build_participant_blocks(participant_id: int) -> list[dict]:
    blocks = []
    practice_cfg = task.copy_block_config(task.PRACTICE_BLOCK)
    practice_cfg["block_idx"] = 0
    practice_cfg["participant_id"] = participant_id
    blocks.append(practice_cfg)

    for block_idx, block in enumerate(
        task.build_blocks_for_participant(participant_id, task.BLOCKS),
        start=1,
    ):
        cfg = task.copy_block_config(block)
        cfg["block_idx"] = block_idx
        cfg["participant_id"] = participant_id
        blocks.append(cfg)
    return blocks


def draw_participant_number_screen_state(
    surface: pygame.Surface,
    font: pygame.font.Font,
    entry: str = "",
    active: bool = True,
    cursor_on: bool = True,
) -> None:
    prompt = "Enter participant number:"

    base_h = font.get_linesize()
    gap = int(base_h * 1.2)
    input_w = task.S(260)
    input_h = int(base_h * 1.6)
    btn_w = task.S(180)
    btn_h = int(base_h * 1.8)

    cx = task.WIDTH // 2
    cy = task.HEIGHT // 2
    prompt_y = cy - 2 * gap
    input_rect = pygame.Rect(cx - input_w // 2, cy - input_h // 2, input_w, input_h)
    btn_rect = pygame.Rect(cx - btn_w // 2, input_rect.bottom + gap, btn_w, btn_h)

    surface.fill(task.BG_INSTRUCTIONS)
    task.draw_text(surface, font, prompt, task.WHITE, (cx, prompt_y))

    border_col = task.WHITE if active else (140, 140, 140)
    pygame.draw.rect(surface, border_col, input_rect, max(1, task.S(2)))

    show = entry + "|" if active and cursor_on else entry
    text_surf = font.render(show, True, task.WHITE)
    text_pos = (
        input_rect.left + task.S(10),
        input_rect.centery - text_surf.get_height() // 2,
    )
    surface.blit(text_surf, text_pos)

    enabled = entry.isdigit() and len(entry) > 0
    task.draw_button(surface, btn_rect, "Continue", font, enabled=enabled)


def draw_main_instruction_screen_state(
    surface: pygame.Surface,
    fonts: dict[str, pygame.font.Font],
    button_enabled: bool = True,
) -> None:
    title = "VIRUS DETECTION TASK"
    intro = (
        "As a reminder, we have identified two dangerous viruses. Unfortunately, the two strains are "
        "difficult to tell apart. Both are speckled BLACK and WHITE. The only difference "
        "visually is that one strain tends to have a little more BLACK, and the other "
        "tends to have a little more WHITE. For simplicity, we will call them V-BLACK and V-WHITE. "
        "You'll be shown a similar number of V-BLACK and V-WHITE samples. \n"
        "Your job is to evaluate the following samples to determine which virus is present.\n"
    )
    press1 = "Click V-BLACK if the sample looks more BLACK overall"
    press2 = "Click V-WHITE if the sample looks more WHITE overall"
    speed = "Try to respond as quickly and accurately as possible\n"

    font_title = fonts["title"]
    font_body = fonts["body"]
    font_body_bold = fonts["body_bold"]

    content_x = task.S(120)
    content_w = task.WIDTH - task.S(240)
    line_spacing = task.S(10)
    blank_spacing = task.S(30)
    gap_title_to_body = task.S(30)
    gap_between_blocks = task.S(12)
    gap_before_speed = task.S(30)
    gap_body_to_button = task.S(40)

    btn_w = task.S(220)
    btn_h = task.S(64)
    btn_rect = pygame.Rect(task.WIDTH // 2 - btn_w // 2, 0, btn_w, btn_h)

    title_h = font_title.get_height()
    intro_h = task._measure_wrapped_height(intro, font_body, content_w, line_spacing, blank_spacing)
    press1_h = task._measure_wrapped_height(press1, font_body_bold, content_w, line_spacing, blank_spacing)
    press2_h = task._measure_wrapped_height(press2, font_body_bold, content_w, line_spacing, blank_spacing)
    speed_h = task._measure_wrapped_height(speed, font_body, content_w, line_spacing, blank_spacing)

    total_h = (
        title_h
        + gap_title_to_body
        + intro_h
        + gap_between_blocks
        + press1_h
        + gap_between_blocks
        + press2_h
        + gap_before_speed
        + speed_h
        + gap_body_to_button
        + btn_h
    )
    start_y = task.HEIGHT // 2 - total_h // 2
    btn_rect.y = start_y + total_h - btn_h

    surface.fill(task.BG_INSTRUCTIONS)
    y = start_y
    title_img = font_title.render(title, True, task.WHITE)
    surface.blit(title_img, (task.WIDTH // 2 - title_img.get_width() // 2, y))

    body_rect = (content_x, 0, content_w, task.HEIGHT)
    y += title_h + gap_title_to_body
    y = task._draw_wrapped_at(surface, intro, font_body, task.WHITE, body_rect, y, line_spacing, blank_spacing)
    y += gap_between_blocks
    y = task._draw_wrapped_at(surface, press1, font_body_bold, task.WHITE, body_rect, y, line_spacing, blank_spacing)
    y += gap_between_blocks
    y = task._draw_wrapped_at(surface, press2, font_body_bold, task.WHITE, body_rect, y, line_spacing, blank_spacing)
    y += gap_before_speed
    task._draw_wrapped_at(surface, speed, font_body, task.WHITE, body_rect, y, line_spacing, blank_spacing)

    task.draw_button(surface, btn_rect, "Continue", font_body, enabled=button_enabled)


def draw_block_instruction_slide_state(
    surface: pygame.Surface,
    fonts: dict[str, pygame.font.Font],
    block_cfg: dict,
    slide_idx: int,
    button_enabled: bool = True,
) -> None:
    payload = task.get_block_instruction_payload(block_cfg["name"], block_cfg=block_cfg)
    title = payload["title"]
    body = payload["slides"][slide_idx]

    font_title = fonts["title"]
    font_body = fonts["body"]

    btn_w = task.S(220)
    btn_h = task.S(64)
    btn_rect = pygame.Rect(task.WIDTH // 2 - btn_w // 2, 0, btn_w, btn_h)

    content_x = task.S(120)
    content_w = task.WIDTH - task.S(240)
    line_spacing = task.S(10)
    blank_spacing = task.S(30)
    gap_title_to_body = task.S(26)
    gap_body_to_button = task.S(32)

    title_h = font_title.get_height() if slide_idx == 0 else 0
    body_h = task._measure_wrapped_height(body, font_body, content_w, line_spacing, blank_spacing)

    total_h = title_h + gap_title_to_body + body_h + gap_body_to_button + btn_h
    start_y = task.HEIGHT // 2 - total_h // 2

    y = start_y
    if slide_idx == 0:
        y += title_h + gap_title_to_body

    btn_rect.y = int(y + body_h + gap_body_to_button)

    surface.fill(task.BG_INSTRUCTIONS)
    y_draw = start_y
    if slide_idx == 0:
        title_img = font_title.render(title, True, task.WHITE)
        surface.blit(title_img, (task.WIDTH // 2 - title_img.get_width() // 2, y_draw))
        y_draw += title_h + gap_title_to_body

    body_rect = (content_x, 0, content_w, task.HEIGHT)
    task._draw_wrapped_at(
        surface,
        body,
        font_body,
        task.WHITE,
        body_rect,
        y_draw,
        line_spacing=line_spacing,
        blank_spacing=blank_spacing,
    )
    task.draw_button(surface, btn_rect, "Continue", font_body, enabled=button_enabled)


def draw_begin_block_screen_state(
    surface: pygame.Surface,
    font_body: pygame.font.Font,
    block_cfg: dict,
) -> None:
    surface.fill(task.BG_INSTRUCTIONS)
    task.draw_center_lines(
        surface,
        [task.block_title(block_cfg["name"], block_cfg=block_cfg), "Press any key to begin"],
        font_body,
        task.WHITE,
        rect=(0, 0, task.WIDTH, task.HEIGHT),
        line_spacing=task.S(14),
        vert_center=True,
    )


def draw_fixation_screen_state(surface: pygame.Surface) -> None:
    surface.fill(task.BG)
    task.draw_fixation_cross(
        surface,
        center=(task.WIDTH // 2, task.HEIGHT // 2),
        size=task.FIX_SIZE,
        color=task.FIX_COLOR,
        thickness=task.FIX_THICKNESS,
    )


def draw_feedback_screen_state(
    surface: pygame.Surface,
    font: pygame.font.Font,
    msg: str,
    bg_color=task.BG,
    text_color=task.WHITE,
    prompt_text="Press any key to continue",
    prompt_font=None,
    prompt_color=task.WHITE,
) -> None:
    if prompt_font is None:
        prompt_font = font

    surface.fill(bg_color)
    img = font.render(msg, True, text_color)
    surface.blit(
        img,
        (
            task.WIDTH // 2 - img.get_width() // 2,
            task.HEIGHT // 2 - img.get_height() // 2,
        ),
    )

    pimg = prompt_font.render(prompt_text, True, prompt_color)
    surface.blit(
        pimg,
        (
            task.WIDTH // 2 - pimg.get_width() // 2,
            task.HEIGHT // 2 + img.get_height() // 2 + task.S(16),
        ),
    )


def draw_block_complete_screen_state(
    surface: pygame.Surface,
    font_body: pygame.font.Font,
    block_cfg: dict,
) -> None:
    surface.fill(task.BG_INSTRUCTIONS)
    task.draw_center_lines(
        surface,
        [
            f"{task.block_title(block_cfg['name'], block_cfg=block_cfg)} COMPLETE",
            "Press any key to continue",
        ],
        font_body,
        task.WHITE,
        rect=(0, 0, task.WIDTH, task.HEIGHT),
        line_spacing=task.S(14),
        vert_center=True,
    )


def draw_final_complete_screen_state(
    surface: pygame.Surface,
    font_body: pygame.font.Font,
    perf_score: float = 80.0,
) -> None:
    surface.fill(task.BG_INSTRUCTIONS)
    task.draw_center_lines(
        surface,
        [
            "EXPERIMENT COMPLETE",
            f"Performance score: {perf_score:.1f}% correct",
            "Please alert the experimenter now",
        ],
        font_body,
        task.WHITE,
        rect=(0, 0, task.WIDTH, task.HEIGHT),
        line_spacing=task.S(14),
        vert_center=True,
    )


def draw_slider_question_screen_state(
    surface: pygame.Surface,
    font_title: pygame.font.Font,
    font_body: pygame.font.Font,
    question: str,
    initial_value: int = 50,
    slider_moved: bool = False,
    button_enabled: bool = False,
    anchors=None,
) -> None:
    if anchors is None:
        anchors = [
            (0, "All incorrect"),
            (50, "Half correct and half incorrect"),
            (100, "All correct"),
        ]

    value = task._clamp_int(int(initial_value), 0, 100)

    content_w = task.WIDTH - task.S(240)
    content_x = task.S(120)
    track_w = min(task.S(760), content_w)
    track_h = max(2, task.S(8))
    knob_r = max(6, task.S(12))

    cx = task.WIDTH // 2
    track_x = cx - track_w // 2
    track_y = task.HEIGHT // 2
    track_rect = pygame.Rect(track_x, track_y, track_w, track_h)

    btn_w = task.S(220)
    btn_h = task.S(64)
    btn_rect = pygame.Rect(cx - btn_w // 2, track_y + task.S(240), btn_w, btn_h)
    q_rect = (content_x, track_y - task.S(240), content_w, task.S(180))

    def value_to_x(v):
        return track_x + int(round((v / 100.0) * track_w))

    tick_positions = [value_to_x(v) for v, _ in anchors]

    surface.fill(task.BG_INSTRUCTIONS)
    task.draw_rich_text_centered(
        surface=surface,
        text=question,
        font=font_body,
        base_color=task.WHITE,
        rect=q_rect,
        line_spacing=task.S(10),
        blank_spacing=task.S(18),
        vert_center=True,
    )

    val_img = font_title.render(f"{value}%", True, task.WHITE)
    surface.blit(val_img, (cx - val_img.get_width() // 2, track_y - task.S(80)))

    pygame.draw.rect(surface, task.LIGHT_GREY, track_rect, border_radius=max(1, task.S(6)))
    fill_w = value_to_x(value) - track_x
    if fill_w > 0:
        fill_rect = pygame.Rect(track_x, track_y, fill_w, track_h)
        pygame.draw.rect(surface, task.WHITE, fill_rect, border_radius=max(1, task.S(6)))

    for tx in tick_positions:
        pygame.draw.line(
            surface,
            task.WHITE,
            (tx, track_y - task.S(10)),
            (tx, track_y + track_h + task.S(10)),
            max(1, task.S(2)),
        )

    knob_x = value_to_x(value)
    knob_y = track_y + track_h // 2
    pygame.draw.circle(surface, task.WHITE, (knob_x, knob_y), knob_r)

    label_font = task.load_font(task.FONT_LIGHT, max(8, task.S(task.FONT_SMALL_BASE)))
    label_y = track_y + task.S(34)
    for (_val, text), tx in zip(anchors, tick_positions):
        task.draw_rich_text_centered(
            surface=surface,
            text=text,
            font=label_font,
            base_color=task.WHITE,
            rect=(tx - task.S(140), label_y, task.S(280), task.S(110)),
            line_spacing=task.S(4),
            blank_spacing=task.S(8),
            vert_center=False,
        )

    hint_y = track_y + task.S(170)
    task.draw_text(surface, font_body, "Drag the slider to respond", task.WHITE, (cx, hint_y))
    task.draw_button(
        surface,
        btn_rect,
        "Continue",
        font_body,
        enabled=button_enabled and slider_moved,
    )


def draw_questionnaire_intro_screen_state(
    surface: pygame.Surface,
    font_title: pygame.font.Font,
    font_body: pygame.font.Font,
) -> None:
    title = "AUTOMATED DECISION AID"
    body = (
        "The following questionnaire relates to your trust in the Automated Decision Aid. "
        "For each item, the scale ranges from strongly disagree to strongly agree. "
        "Please indicate how much you agree or disagree with the following statements "
        "by choosing the appropriate response on the scale."
    )
    prompt = "Press any key to continue"

    surface.fill(task.BG_INSTRUCTIONS)

    content_width = task.WIDTH - task.S(240)
    content_x = task.S(120)
    line_spacing = task.S(10)
    blank_spacing = task.S(24)

    body_items = task.layout_rich_text_blocks(body, font_body, max_width=content_width)
    body_height = task.measure_rich_block_height(
        body_items,
        font_body,
        line_spacing=line_spacing,
        blank_spacing=blank_spacing,
    )
    title_height = font_title.get_height()
    prompt_height = font_body.get_height()
    block_gap = task.S(30)

    total_height = title_height + block_gap + body_height + block_gap + prompt_height
    start_y = task.HEIGHT // 2 - total_height // 2

    title_img = font_title.render(title, True, task.WHITE)
    surface.blit(title_img, (task.WIDTH // 2 - title_img.get_width() // 2, start_y))

    body_y = start_y + title_height + block_gap
    task.draw_rich_text_centered(
        surface=surface,
        text=body,
        font=font_body,
        base_color=task.WHITE,
        rect=(content_x, body_y, content_width, body_height),
        line_spacing=line_spacing,
        blank_spacing=blank_spacing,
        vert_center=False,
    )

    prompt_y = body_y + body_height + block_gap
    task.draw_text(
        surface,
        font_body,
        prompt,
        task.WHITE,
        (task.WIDTH // 2, prompt_y + prompt_height // 2),
    )


def draw_likert_question_screen_state(
    surface: pygame.Surface,
    font: pygame.font.Font,
    item: dict,
    slider_moved: bool = False,
    button_enabled: bool = False,
) -> None:
    question = item["question"]
    anchor_labels = [
        "Strongly disagree",
        "Disagree",
        "Neither agree nor disagree",
        "Agree",
        "Strongly agree",
    ]
    current_idx = len(anchor_labels) // 2

    content_w = task.WIDTH - task.S(240)
    content_x = task.S(120)
    cx = task.WIDTH // 2
    track_w = min(task.S(760), content_w)
    track_h = max(2, task.S(8))
    knob_r = max(6, task.S(12))
    track_x = cx - track_w // 2
    track_y = task.HEIGHT // 2
    tick_xs = [
        track_x + int(round(i * track_w / (len(anchor_labels) - 1)))
        for i in range(len(anchor_labels))
    ]
    btn_w = task.S(220)
    btn_h = task.S(64)
    btn_rect = pygame.Rect(cx - btn_w // 2, track_y + task.S(170), btn_w, btn_h)
    q_rect = (content_x, track_y - task.S(220), content_w, task.S(180))

    surface.fill(task.BG_INSTRUCTIONS)
    task.draw_rich_text_centered(
        surface=surface,
        text=question,
        font=font,
        base_color=task.WHITE,
        rect=q_rect,
        line_spacing=task.S(10),
        blank_spacing=task.S(18),
        vert_center=True,
    )

    label_font = task.load_font(task.FONT_LIGHT, max(10, task.S(task.FONT_BODY_BASE)))
    task.draw_rich_text_centered(
        surface=surface,
        text=anchor_labels[current_idx],
        font=label_font,
        base_color=task.WHITE,
        rect=(content_x, track_y - task.S(85), content_w, task.S(40)),
        line_spacing=task.S(4),
        blank_spacing=task.S(8),
        vert_center=True,
    )

    track_rect = pygame.Rect(track_x, track_y, track_w, track_h)
    pygame.draw.rect(surface, task.LIGHT_GREY, track_rect, border_radius=max(1, task.S(6)))

    knob_x = tick_xs[current_idx]
    fill_w = knob_x - track_x
    if fill_w > 0:
        fill_rect = pygame.Rect(track_x, track_y, fill_w, track_h)
        pygame.draw.rect(surface, task.WHITE, fill_rect, border_radius=max(1, task.S(6)))

    for tx in tick_xs:
        pygame.draw.line(
            surface,
            task.WHITE,
            (tx, track_y - task.S(10)),
            (tx, track_y + track_h + task.S(10)),
            max(1, task.S(2)),
        )

    knob_y = track_y + track_h // 2
    pygame.draw.circle(surface, task.WHITE, (knob_x, knob_y), knob_r)

    end_label_font = task.load_font(task.FONT_LIGHT, max(9, task.S(task.FONT_SMALL_BASE)))
    labels_y = track_y + task.S(26)
    left_img = end_label_font.render(anchor_labels[0], True, task.WHITE)
    right_img = end_label_font.render(anchor_labels[-1], True, task.WHITE)
    surface.blit(left_img, (track_x, labels_y))
    surface.blit(right_img, (track_x + track_w - right_img.get_width(), labels_y))

    hint_y = track_y + task.S(120)
    task.draw_text(surface, font, "Drag the slider to respond", task.WHITE, (cx, hint_y))
    task.draw_button(
        surface,
        btn_rect,
        "Continue",
        font,
        enabled=button_enabled and slider_moved,
    )


def example_trial_context(fonts, block_cfg: dict, trial_number: int = 1) -> dict:
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
    aid_label = stimulus if task.block_has_real_aid(block_cfg) else None

    evidence_black_pct = (n_vblack / task.N_DOTS) * 100.0
    evidence_white_pct = (n_vwhite / task.N_DOTS) * 100.0

    return {
        "center": center,
        "dot_layer": dot_layer,
        "dots": dots,
        "ui_payload": {
            "fonts": fonts,
            "trials_left": trials_left,
            "n_trials": block_cfg["N_TRIALS"],
        },
        "aid_payload": {
            "label": aid_label,
            "transparency_level": block_cfg.get("AID_TRANSPARENCY", "none"),
            "evidence_black_pct": evidence_black_pct,
            "evidence_white_pct": evidence_white_pct,
        },
        "initial_response": "BLACK",
    }


def draw_stimulus_decision(
    surface: pygame.Surface,
    context: dict,
    show_aid: bool = False,
    show_masked_aid: bool = False,
    initial_response: str | None = None,
) -> None:
    aid_mode = "masked" if show_masked_aid else ("automation" if show_aid else "none")
    task.draw_trial_frame(
        surface,
        context["dot_layer"],
        context["dots"],
        context["center"],
        {
            "mode": aid_mode,
            "label": context["aid_payload"]["label"],
            "visible": bool(show_aid),
            "transparency_level": context["aid_payload"]["transparency_level"],
            "evidence_black_pct": context["aid_payload"]["evidence_black_pct"],
            "evidence_white_pct": context["aid_payload"]["evidence_white_pct"],
        },
        context["ui_payload"],
        ms_left=None,
        initial_response=initial_response,
    )


def render_trial_sequence(
    writer: ScreenshotWriter,
    surface: pygame.Surface,
    fonts: dict[str, pygame.font.Font],
    block_cfg: dict,
    prefix: str,
    trial_number: int = 1,
) -> None:
    context = example_trial_context(fonts, block_cfg, trial_number=trial_number)
    aid_condition = task.aid_condition_for_block(block_cfg)

    draw_fixation_screen_state(surface)
    writer.save(surface, f"{prefix}_trial_01_fixation")

    if aid_condition == "manual":
        task.draw_masked_placeholder_frame(
            surface,
            context["ui_payload"],
            show_prompt=False,
            phase_label="Preview",
        )
        writer.save(surface, f"{prefix}_trial_02_masked_aid_preview")

        draw_fixation_screen_state(surface)
        writer.save(surface, f"{prefix}_trial_03_fixation")

        draw_stimulus_decision(surface, context)
        writer.save(surface, f"{prefix}_trial_04_decision1_stimulus_only")

        draw_fixation_screen_state(surface)
        writer.save(surface, f"{prefix}_trial_05_fixation")

        task.draw_masked_placeholder_frame(
            surface,
            context["ui_payload"],
            show_prompt=True,
            initial_response=context["initial_response"],
        )
        writer.save(surface, f"{prefix}_trial_06_decision2_masked_placeholder")

    elif aid_condition == "aid_first":
        task.draw_aid_only_frame(
            surface,
            context["aid_payload"],
            context["ui_payload"],
            show_prompt=False,
            phase_label="Preview",
        )
        writer.save(surface, f"{prefix}_trial_02_aid_preview")

        draw_fixation_screen_state(surface)
        writer.save(surface, f"{prefix}_trial_03_fixation")

        draw_stimulus_decision(surface, context)
        writer.save(surface, f"{prefix}_trial_04_decision1_stimulus_only")

        draw_fixation_screen_state(surface)
        writer.save(surface, f"{prefix}_trial_05_fixation")

        task.draw_masked_placeholder_frame(
            surface,
            context["ui_payload"],
            show_prompt=True,
            initial_response=context["initial_response"],
        )
        writer.save(surface, f"{prefix}_trial_06_decision2_masked_placeholder")

    elif aid_condition == "stimulus_first_change":
        task.draw_masked_placeholder_frame(
            surface,
            context["ui_payload"],
            show_prompt=False,
            phase_label="Preview",
        )
        writer.save(surface, f"{prefix}_trial_02_masked_aid_preview")

        draw_fixation_screen_state(surface)
        writer.save(surface, f"{prefix}_trial_03_fixation")

        draw_stimulus_decision(surface, context)
        writer.save(surface, f"{prefix}_trial_04_decision1_stimulus_only")

        draw_fixation_screen_state(surface)
        writer.save(surface, f"{prefix}_trial_05_fixation")

        task.draw_aid_only_frame(
            surface,
            context["aid_payload"],
            context["ui_payload"],
            show_prompt=True,
            initial_response=context["initial_response"],
        )
        writer.save(surface, f"{prefix}_trial_06_decision2_aid_only")

    else:
        raise RuntimeError(f"unsupported scheduled aid condition: {aid_condition}")

    feedback_msg, feedback_color = FEEDBACK_EXAMPLES[condition_code(block_cfg)]
    draw_feedback_screen_state(
        surface,
        fonts["title"],
        feedback_msg,
        bg_color=task.BG,
        text_color=feedback_color,
        prompt_text="Press any key to continue",
        prompt_font=fonts["body"],
        prompt_color=task.WHITE,
    )
    writer.save(surface, f"{prefix}_trial_07_feedback_{slugify(feedback_msg)}")


def render_block(
    writer: ScreenshotWriter,
    surface: pygame.Surface,
    fonts: dict[str, pygame.font.Font],
    block_cfg: dict,
) -> None:
    prefix = block_slug(block_cfg)

    draw_main_instruction_screen_state(surface, fonts, button_enabled=True)
    writer.save(surface, f"{prefix}_main_instructions")

    payload = task.get_block_instruction_payload(block_cfg["name"], block_cfg=block_cfg)
    for slide_idx in range(len(payload["slides"])):
        draw_block_instruction_slide_state(
            surface,
            fonts,
            block_cfg=block_cfg,
            slide_idx=slide_idx,
            button_enabled=True,
        )
        writer.save(surface, f"{prefix}_block_instruction_{slide_idx + 1:02d}")

    draw_begin_block_screen_state(surface, fonts["body"], block_cfg)
    writer.save(surface, f"{prefix}_begin_block")

    render_trial_sequence(writer, surface, fonts, block_cfg, prefix)

    draw_block_complete_screen_state(surface, fonts["body"], block_cfg)
    writer.save(surface, f"{prefix}_block_complete")

    for item_idx, item in enumerate(block_slider_items(block_cfg), start=1):
        draw_slider_question_screen_state(
            surface,
            fonts["title"],
            fonts["body"],
            question=item["question"],
            initial_value=50,
            slider_moved=False,
            button_enabled=False,
        )
        writer.save(surface, f"{prefix}_postblock_slider_{item_idx:02d}_{item['key']}")

    if task.block_has_real_aid(block_cfg) and task.ENABLE_POSTBLOCK_QUESTIONS:
        draw_questionnaire_intro_screen_state(surface, fonts["title"], fonts["body"])
        writer.save(surface, f"{prefix}_questionnaire_intro")

        for item_idx, item in enumerate(task.QUESTION_ITEMS, start=1):
            draw_likert_question_screen_state(
                surface,
                fonts["body"],
                item,
                slider_moved=False,
                button_enabled=False,
            )
            writer.save(surface, f"{prefix}_questionnaire_item_{item_idx:02d}")


def render_screens(
    output_dir: Path,
    width: int,
    height: int,
    participant_id: int,
) -> list[Path]:
    random.seed(1234)
    initialize_task_metrics(width, height)
    fonts = task.load_ui_fonts()
    surface = pygame.Surface((width, height))
    writer = ScreenshotWriter(output_dir)

    blocks = build_participant_blocks(participant_id)

    draw_participant_number_screen_state(
        surface,
        fonts["body"],
        entry="",
        active=True,
        cursor_on=True,
    )
    writer.save(surface, "participant_number")

    for block_cfg in blocks:
        render_block(writer, surface, fonts, block_cfg)

    draw_final_complete_screen_state(surface, fonts["body"], perf_score=80.0)
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
