"""
Random Dot Classification (Bartlett & McCarley–style)

Responses:
- D/J = classify as V-BLACK or V-WHITE, counterbalanced by participant
- Option/Alt + Q = hard quit
- ESC = quit from final completion screen
"""

import argparse
import csv
import sys
import random
import math
import os
import pygame
import time
from datetime import datetime
from typing import Any, Dict

# -----------------------------
# Run start time
# -----------------------------
run_ts = int(time.time())   # unix format
run_ts = datetime.fromtimestamp(run_ts).strftime("%Y%m%d_%H%M%S")

# -----------------------------
# Block definitions
# -----------------------------
MAIN_BLOCK_N_TRIALS = 260
PRACTICE_N_TRIALS = 60
# Prior across-participant difficulty distribution. Practice starts here, and
# single-block runs use these values directly when no practice calibration runs.
# See derive_prior_calibration_delta.R for the reproducible prior-data summary.
GLOBAL_FIXED_DELTA = 0.040324718919
GLOBAL_FIXED_DELTA_SD = 0.014615991726
AUTOMATION_PRE_PHASE_MS = 1000
CALIBRATION_TARGET_ACCURACY = 0.75
GLOBAL_AID_ACCURACY = 0.85
SCHEDULED_AUTOMATION_AID_CONDITIONS = {
    "manual": "Manual",
    "aid_first": "Aid first",
    "stimulus_first": "Stimulus first",
}
HIDDEN_AUTOMATION_AID_CONDITIONS = {
    "simultaneous": "Aid + stimulus",
}
AUTOMATION_AID_CONDITIONS = {
    **SCHEDULED_AUTOMATION_AID_CONDITIONS,
    **HIDDEN_AUTOMATION_AID_CONDITIONS,
}
REAL_AID_CONDITIONS = {"simultaneous", "aid_first", "stimulus_first"}
SCHEDULED_MAIN_BLOCK_ORDERS = (
    ("MANUAL", "AIDFIRST", "STIMFIRST"),
    ("STIMFIRST", "AIDFIRST", "MANUAL"),
    ("MANUAL", "STIMFIRST", "AIDFIRST"),
    ("AIDFIRST", "STIMFIRST", "MANUAL"),
    ("AIDFIRST", "MANUAL", "STIMFIRST"),
    ("STIMFIRST", "MANUAL", "AIDFIRST"),
)

BLOCKS = [
    dict(
        name="AUTOMATION",
        N_TRIALS=MAIN_BLOCK_N_TRIALS,
        AUTOMATION_ON=True,       # two-decision main-block flow
        AID_ACCURACY=None,        # no real aid in the manual condition
        AID_TRANSPARENCY="none",
        AID_CONDITION="manual",
        STAIRCASE_ON=False,       # staircase off
        FIXED_DELTA_ON=True,
        FIXED_DELTA_VALUE=GLOBAL_FIXED_DELTA,
        FIXED_DELTA_SD=GLOBAL_FIXED_DELTA_SD,
        TRIAL_FEEDBACK_ON=True,
        TRIAL_DEADLINE_MS=None,
        CONDITION_CODE="MANUAL",
    ),
    dict(
        name="AUTOMATION",
        N_TRIALS=MAIN_BLOCK_N_TRIALS,
        AUTOMATION_ON=True,       # automation on
        AID_ACCURACY=GLOBAL_AID_ACCURACY,
        AID_TRANSPARENCY="none",
        AID_CONDITION="aid_first",
        STAIRCASE_ON=False,       # staircase off
        FIXED_DELTA_ON=True,
        FIXED_DELTA_VALUE=GLOBAL_FIXED_DELTA,
        FIXED_DELTA_SD=GLOBAL_FIXED_DELTA_SD,
        TRIAL_FEEDBACK_ON=True,
        TRIAL_DEADLINE_MS=None,
        CONDITION_CODE="AIDFIRST",
    ),
    dict(
        name="AUTOMATION",
        N_TRIALS=MAIN_BLOCK_N_TRIALS,
        AUTOMATION_ON=True,       # automation on
        AID_ACCURACY=GLOBAL_AID_ACCURACY,
        AID_TRANSPARENCY="none",
        AID_CONDITION="stimulus_first",
        STAIRCASE_ON=False,       # staircase off
        FIXED_DELTA_ON=True,
        FIXED_DELTA_VALUE=GLOBAL_FIXED_DELTA,
        FIXED_DELTA_SD=GLOBAL_FIXED_DELTA_SD,
        TRIAL_FEEDBACK_ON=True,
        TRIAL_DEADLINE_MS=None,
        CONDITION_CODE="STIMFIRST",
    ),
]

PRACTICE_BLOCK = dict(
    name="PRACTICE",
    N_TRIALS=PRACTICE_N_TRIALS,
    AUTOMATION_ON=True,
    AID_ACCURACY=None,
    AID_TRANSPARENCY="none",
    AID_CONDITION="manual",
    STAIRCASE_ON=True,
    TARGET_ACC=CALIBRATION_TARGET_ACCURACY,
    FIXED_DELTA_ON=False,
    FIXED_DELTA_VALUE=GLOBAL_FIXED_DELTA,
    FIXED_DELTA_SD=GLOBAL_FIXED_DELTA_SD,
    TRIAL_FEEDBACK_ON=True,
    TRIAL_DEADLINE_MS=None,
    CONDITION_CODE="PRACTICE",
)

# Hidden developer-only condition. It is intentionally excluded from BLOCKS,
# R helpers, and public examples; append a copy of this config when a SIM block
# is needed for testing or a custom run.
HIDDEN_SIM_BLOCK = dict(
    name="AUTOMATION",
    N_TRIALS=MAIN_BLOCK_N_TRIALS,
    AUTOMATION_ON=True,
    AID_ACCURACY=GLOBAL_AID_ACCURACY,
    AID_TRANSPARENCY="none",
    AID_CONDITION="simultaneous",
    STAIRCASE_ON=False,
    FIXED_DELTA_ON=True,
    FIXED_DELTA_VALUE=GLOBAL_FIXED_DELTA,
    FIXED_DELTA_SD=GLOBAL_FIXED_DELTA_SD,
    TRIAL_FEEDBACK_ON=True,
    TRIAL_DEADLINE_MS=None,
    CONDITION_CODE="SIM",
)

BLOCK_DEFAULTS = {
    "SHOW_AID_MASKED": False,
    "AID_TRANSPARENCY": "none",
    "TRIAL_DEADLINE_MS": None,
    "CONDITION_CODE": None,
    "CONDITION_DEADLINE_CODE": None,
    "AID_CONDITION": None,
    "AUTOMATION_RELIABILITY_GROUP": "none",
}

BLOCK_INSTRUCTIONS = {
    "PRACTICE": {
        "title": "PRACTICE BLOCK",
        "slides": [
            (
                "You will now complete 60 practice trials.\n\n"
                "These trials are to familiarise you with the 2-decision format.\n\n"
                "No advice will be shown in the centre of the display.\n"
                "There is simply a string '#####', which you should ignore."
            ),
        ],
    },
    "AUTOMATION": {
        "title": "AUTOMATION BLOCK",
        "slides": [

            # Slide 1
            (
            "You will be provided with an automated decision aid to assist you with this task. "
            "The automation will recommend a classification (either BLACK or WHITE) for each sample. "
            "The recommended classification will be presented in the centre of the display. "
            "If the aid shows 'BLACK', this means that the automation recommends you classify "
            "that sample as V-BLACK. If it shows 'WHITE', this means that the automation "
            "recommends you classify the sample as V-WHITE."
            ),

            # Slide 2
            (
            "In the event that the automation makes an incorrect recommendation, "
            "it is essential that you perform the correct action. "
            "Remember that deciding whether a sample is V-BLACK or V-WHITE "
            "is your responsibility."
            ),
        ],
    },
}


def copy_block_config(block_cfg):
    cfg = dict(BLOCK_DEFAULTS)
    cfg.update(block_cfg)
    return cfg


def block_condition_code(block_cfg) -> str:
    code = block_cfg.get("CONDITION_CODE") or block_cfg.get("CONDITION_DEADLINE_CODE")
    if code:
        return code
    return block_cfg["name"]


def trial_deadline_ms_for_block(block_cfg):
    return block_cfg.get("TRIAL_DEADLINE_MS", None)


def trial_deadline_s_for_block(block_cfg):
    deadline_ms = trial_deadline_ms_for_block(block_cfg)
    if deadline_ms is None:
        return None
    return deadline_ms / 1000.0


def aid_condition_for_block(block_cfg):
    return block_cfg.get("AID_CONDITION")


def block_has_real_aid(block_cfg) -> bool:
    return bool(
        block_cfg
        and block_cfg.get("AUTOMATION_ON")
        and aid_condition_for_block(block_cfg) in REAL_AID_CONDITIONS
    )


def aid_condition_instruction_slide(block_cfg) -> str:
    aid_condition = aid_condition_for_block(block_cfg)
    if aid_condition is None:
        return ""

    if aid_condition == "simultaneous":
        return (
            "In the next block, the aid's recommendation and the virus sample will appear together."
        )

    if aid_condition == "aid_first":
        return (
            "In the next block, the aid's recommendation will appear before the virus sample."
        )

    if aid_condition == "stimulus_first":
        return (
            "In the next block, the virus sample will appear before the aid's recommendation."
        )

    raise ValueError(
        f"Unsupported aid condition '{aid_condition}'. "
        f"Valid values: {sorted(AUTOMATION_AID_CONDITIONS)}"
    )


def automation_accuracy_instruction_slide() -> str:
    return (
        "In the next block, although the automation reliability is very good, it is not perfect, "
        "and automation advice errors may occur."
    )


def manual_condition_instruction_slide() -> str:
    return (
        "In the next block, no advice will be shown in the centre of the display.\n"
        "There is simply a string '#####', which you should ignore."
    )


def block_order_index_for_participant(participant_id: int) -> int:
    return ((participant_id - 1) // 2) % len(SCHEDULED_MAIN_BLOCK_ORDERS)


def keymap_flip_for_participant(participant_id: int) -> bool:
    return ((participant_id - 1) % 2) == 1


def transparency_instruction_slide(transparency_level: str) -> str:
    if transparency_level == "none":
        return (
            "In the next block, the automated decision aid will display only its recommendation. "
            "No additional explanation will be shown."
        )

    if transparency_level == "low":
        return (
            "In the next block, the automated decision aid will display its recommendation and a brief reason. "
            "The reason line will state which category the available evidence favors."
        )

    if transparency_level == "high":
        return (
            "In the next block, the automated decision aid will display its recommendation, a brief reason, "
            "a summary of the estimated BLACK and WHITE evidence, and the decision rule used to make the recommendation."
        )

    raise ValueError(
        f"Unsupported transparency level '{transparency_level}'. "
        f"Valid values: {sorted(AID_TRANSPARENCY_LEVELS)}"
    )


# -----------------------------
# Config
# -----------------------------
FPS = 120
FULLSCREEN = True          # set False if you ever want windowed testing
USE_DESKTOP_RES = True     # True = use current display resolution

# -----------------------------
# UI scaling
# -----------------------------
BASE_W, BASE_H = 1280, 720
# BASE_W, BASE_H = 1512, 982 # my mac
# BASE_W, BASE_H = 1920, 1080

def compute_ui_scale(actual_w, actual_h):
    # uniform scale preserves aspect / proportions
    return min(actual_w / BASE_W, actual_h / BASE_H)

def S(x):  # scale a length (px)
    return int(round(x * UI_SCALE))

def SF(x):  # scale a float (useful for speed, etc.)
    return float(x * UI_SCALE)

def SP(x):  # scale padding/spacing; keeps ints
    return int(round(x * UI_SCALE))
  
# Base geometry
DISH_RADIUS_BASE = 260
DOT_RADIUS_BASE  = 3
N_DOTS = 3600
DOT_ALPHA = 255  # 0–255 (lower = more transparent)

# Fixation cross
FIX_SIZE_BASE      = 21
FIX_THICKNESS_BASE = 2

# Progress bar
PB_W_BASE, PB_H_BASE = 224, 16
PB_PAD_BASE          = 18

# Base font sizes
FONT_TITLE_BASE = 36
FONT_BODY_BASE  = 24
FONT_SMALL_BASE = 18
FONT_AID_LABEL_BASE = 20
FONT_AID_BASE       = 32

# Backgrounds and common colours
BG = (128, 128, 128)              # main task background
BG_INSTRUCTIONS = (40, 40, 40)    # darker instruction screen
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
LIGHT_GREY = (170, 170, 170)
DARK_GREY = (60, 60, 60)
MASKED_AID_COLOR = (35, 85, 125)
DECISION_PHASE_COLOR = MASKED_AID_COLOR
FIX_COLOR = LIGHT_GREY      # fixation cross colour

# V-BLACK cell colour
VBLACK = BLACK         # black

# V-WHITE cell colour
VWHITE = WHITE    # white

# Dish colour
DISH_FILL = (128, 128, 128)   # neutral mid-grey (halfway between black/white)
DISH_RING = BLACK     # outer ring
DISH_EDGE = BLACK     # thin edge

# V-BLACK cell proportions
VBLACK_PROPORTION_LEVELS = [0.40, 0.42, 0.44, 0.46, 0.48, 0.52, 0.54, 0.56, 0.58, 0.60]

# Aid timing is configured as a behavior condition per main block.
AID_TRANSPARENCY_LEVELS = {"none", "low", "high"}

# -----------------------------
# Adaptive staircase (practice calibration)
# -----------------------------
DELTA_INIT = GLOBAL_FIXED_DELTA      # start at prior across-participant mean
DELTA_SD   = GLOBAL_FIXED_DELTA_SD   # trial-wise sampling SD around staircase mean
DELTA_MIN  = 1/N_DOTS        # hardest allowed (closest to 0.5)
DELTA_MAX  = 0.25            # easiest allowed
DELTA_STEP_DOWN = 0.01       # starting (large) down-step
DELTA_STEP_DOWN_MIN = 0.001  # target min down-step after burn-in
# Burn-in period with annealing
BURNIN_TRIALS = 20           # first 20 practice trials use the larger annealed step
# Calibration summary window
# None = use all eligible trials after burn-in exclusion
# int  = use only the most recent N eligible trials after burn-in exclusion
CALIB_SUMMARY_LAST_N = 40

# -----------------------------
# Fixed-delta mode (works for automation and manual blocks)
# -----------------------------
# FIXED_DELTA_ON = False        # True = ignore staircase + fixed props, use FIXED_DELTA_VALUE
# FIXED_DELTA_VALUE = 0.040324718919
# FIXED_DELTA_SD = 0.014615991726

# -----------------------------
# Feedback screen (post-trial)
# -----------------------------
FEEDBACK_MS = 1000   # duration of feedback screen in ms
FEEDBACK_CORRECT_COLOR = (0, 200, 0)
FEEDBACK_ERROR_COLOR   = (220, 50, 50)
FEEDBACK_SLOW_COLOR    = (200, 200, 0)

# Trial timing
FIXATION_DURATION_MS = 500
TRIAL_DEADLINE_MS = None

# Brownian motion for dots (per-frame random walk)
BROWNIAN_STEP_MEAN = 0.001   # pixels per frame (typical step)
BROWNIAN_STEP_SD   = 0.60   # variability in step length

# Floaty motion params (BASE units; will be scaled after UI_SCALE is known)
VEL_WANDER_SD_BASE  = 0.008   # velocity jitter per frame (px/frame)
VEL_DAMPING         = 0.99    # unitless (do NOT scale)
VEL_MAX_BASE        = 0.16    # max speed (px/frame)
VEL_INIT_RANGE_BASE = 0.5    # px/frame (base)
COLLISION_DAMPING   = 0.9     # unitless (do NOT scale)

# Fonts
FONT_LIGHT = "Roboto-Light.ttf"
FONT_BOLD = "Roboto-Bold.ttf"
FONT_DIR_CANDIDATES = [
    os.path.join(os.sep, "python", "fonts"),  # /python/fonts
    os.path.join("python", "fonts"),          # python/fonts
    os.path.join(".", "python", "fonts"),     # ./python/fonts
    os.path.join("fonts"),                    # fonts (fallback)
]

# -----------------------------
# Text coloring toggles
# -----------------------------
TEXT_COLORING_UI_ON = False      # participant-facing response-key prompt
TEXT_COLORING_AID_ON = True      # automated recommendation stays coloured

RAW_COLOR_TOKENS = {"BLACK": VBLACK, "WHITE": VWHITE}

COLOR_TOKENS_UI  = RAW_COLOR_TOKENS if TEXT_COLORING_UI_ON  else {}
COLOR_TOKENS_AID = RAW_COLOR_TOKENS if TEXT_COLORING_AID_ON else {}

# ---------------- Post-block slider questions -----------------------------

ENABLE_POSTBLOCK_SLIDERS = True

SLIDER_ITEMS_MANUAL = [
    {
        "key": "perc_self_correct",
        "question": "For your responses, what percentage do you think were correct in the preceding block of trials?",
    },
]

SLIDER_ITEMS_AUTOMATION = [
    {
        "key": "perc_auto_correct",
        "question": "For the automation's recommendations, what percentage do you think were correct in the preceding block of trials?",
    },
]

# ---------------- Questionnaire settings ---------------------------------

ENABLE_POSTBLOCK_QUESTIONS = True

QUESTION_SCALE_MIN = 1
QUESTION_SCALE_MAX = 5

QUESTION_ITEMS = [
    {
        "question": "I believe the automated decision aid is a competent performer.",
        "left_anchor": "Strongly disagree",
        "right_anchor": "Strongly agree",
    },
    {
        "question": "I trust the automated decision aid.",
        "left_anchor": "Strongly disagree",
        "right_anchor": "Strongly agree",
    },
    {
        "question": "I have confidence in the advice given by the automated decision aid. ",
        "left_anchor": "Strongly disagree",
        "right_anchor": "Strongly agree",
    },
    {
        "question": "I can depend on the automated decision aid.",
        "left_anchor": "Strongly disagree",
        "right_anchor": "Strongly agree",
    },
    {
        "question": "I can rely on the automated decision aid to behave in consistent ways.",
        "left_anchor": "Strongly disagree",
        "right_anchor": "Strongly agree",
    },
    {
        "question": "I can rely on the automated decision aid to do its best every time I take its advice.",
        "left_anchor": "Strongly disagree",
        "right_anchor": "Strongly agree",
    },
]

def run_likert_question(
    screen,
    clock,
    font,
    item,
    scale_min=QUESTION_SCALE_MIN,
    scale_max=QUESTION_SCALE_MAX,
    min_show_ms=250,
):
    """
    Slider-style Likert question screen.

    - Uses the same general style as run_slider_question_screen()
    - Slider snaps to 5 ordered anchor statements
    - Continue button is disabled until slider moved at least once
    - Returns chosen numeric value (1..5) or {"quit": True}
    """
    t0 = pygame.time.get_ticks()

    question = item["question"]

    valid_values = list(range(scale_min, scale_max + 1))
    anchor_labels = [
        "Strongly disagree",
        "Disagree",
        "Neither agree nor disagree",
        "Agree",
        "Strongly agree",
    ]

    if len(valid_values) != len(anchor_labels):
        raise ValueError("anchor_labels length must match number of scale values")

    # Start in the middle, but do NOT count that as a response until moved
    current_idx = len(valid_values) // 2
    dragging = False
    slider_moved = False

    # --- Layout ---
    content_w = WIDTH - S(240)
    content_x = S(120)

    cx = WIDTH // 2

    # Slider geometry
    track_w = min(S(760), content_w)
    track_h = max(2, S(8))
    knob_r  = max(6, S(12))

    track_x = cx - track_w // 2
    track_y = HEIGHT // 2

    # Snap points for the 5 response options
    if len(valid_values) == 1:
        tick_xs = [track_x + track_w // 2]
    else:
        tick_xs = [
            track_x + int(round(i * track_w / (len(valid_values) - 1)))
            for i in range(len(valid_values))
        ]

    # Continue button
    btn_w = S(220)
    btn_h = S(64)
    btn_rect = pygame.Rect(cx - btn_w // 2, track_y + S(170), btn_w, btn_h)

    # Question text position
    q_rect = (content_x, track_y - S(220), content_w, S(180))

    def idx_to_value(idx):
        idx = _clamp_int(idx, 0, len(valid_values) - 1)
        return valid_values[idx]

    def idx_to_x(idx):
        idx = _clamp_int(idx, 0, len(tick_xs) - 1)
        return tick_xs[idx]

    def x_to_idx(mx):
        # snap to nearest anchor position
        return min(range(len(tick_xs)), key=lambda i: abs(mx - tick_xs[i]))

    while True:
        clock.tick(FPS)

        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                pass

            if ev.type == pygame.KEYDOWN:
                if is_hard_quit_event(ev):
                    return {"quit": True}

            if ev.type == pygame.MOUSEBUTTONDOWN and ev.button == 1:
                mx, my = ev.pos
                elapsed = pygame.time.get_ticks() - t0

                # Continue button
                if btn_rect.collidepoint(mx, my) and slider_moved and elapsed >= min_show_ms:
                    return int(idx_to_value(current_idx))

                # Track / knob hitbox
                track_hit = pygame.Rect(track_x, track_y - S(20), track_w, track_h + S(40))
                knob_hit = pygame.Rect(
                    idx_to_x(current_idx) - knob_r - S(8),
                    track_y + track_h // 2 - knob_r - S(8),
                    2 * (knob_r + S(8)),
                    2 * (knob_r + S(8)),
                )

                if track_hit.collidepoint(mx, my) or knob_hit.collidepoint(mx, my):
                    current_idx = x_to_idx(mx)
                    dragging = True
                    slider_moved = True

            if ev.type == pygame.MOUSEBUTTONUP and ev.button == 1:
                dragging = False

            if ev.type == pygame.MOUSEMOTION and dragging:
                mx, my = ev.pos
                current_idx = x_to_idx(mx)
                slider_moved = True

        # --- Draw ---
        screen.fill(BG_INSTRUCTIONS)

        # Question
        draw_rich_text_centered(
            surface=screen,
            text=question,
            font=font,
            base_color=WHITE,
            rect=q_rect,
            line_spacing=S(10),
            blank_spacing=S(18),
            vert_center=True,
        )

        # Current selected anchor label above slider
        current_label = anchor_labels[current_idx]
        label_font = load_font(FONT_LIGHT, max(10, S(FONT_BODY_BASE)))
        draw_rich_text_centered(
            surface=screen,
            text=current_label,
            font=label_font,
            base_color=WHITE,
            rect=(content_x, track_y - S(85), content_w, S(40)),
            line_spacing=S(4),
            blank_spacing=S(8),
            vert_center=True,
        )

        # Track
        track_rect = pygame.Rect(track_x, track_y, track_w, track_h)
        pygame.draw.rect(screen, LIGHT_GREY, track_rect, border_radius=max(1, S(6)))

        # Fill to current point
        knob_x = idx_to_x(current_idx)
        fill_w = knob_x - track_x
        if fill_w > 0:
            fill_rect = pygame.Rect(track_x, track_y, fill_w, track_h)
            pygame.draw.rect(screen, WHITE, fill_rect, border_radius=max(1, S(6)))

        # Tick marks
        for tx in tick_xs:
            pygame.draw.line(
                screen,
                WHITE,
                (tx, track_y - S(10)),
                (tx, track_y + track_h + S(10)),
                max(1, S(2))
            )

        # Knob
        knob_y = track_y + track_h // 2
        pygame.draw.circle(screen, WHITE, (knob_x, knob_y), knob_r)

        # Endpoint labels under the slider
        end_label_font = load_font(FONT_LIGHT, max(9, S(FONT_SMALL_BASE)))
        left_lab = anchor_labels[0]
        right_lab = anchor_labels[-1]

        left_img = end_label_font.render(left_lab, True, WHITE)
        right_img = end_label_font.render(right_lab, True, WHITE)

        labels_y = track_y + S(26)
        screen.blit(left_img, (track_x, labels_y))
        screen.blit(right_img, (track_x + track_w - right_img.get_width(), labels_y))

        # # All anchor labels under each tick
        # label_font = load_font(FONT_LIGHT, max(9, S(FONT_SMALL_BASE)))
        # labels_y = track_y + S(26)
        # 
        # for lab, tx in zip(anchor_labels, tick_xs):
        #     draw_rich_text_centered(
        #         surface=screen,
        #         text=lab,
        #         font=label_font,
        #         base_color=WHITE,
        #         rect=(tx - S(90), labels_y, S(180), S(80)),
        #         line_spacing=S(4),
        #         blank_spacing=S(6),
        #         vert_center=False,
        #     )
    
        # Hint between slider and button
        hint = "Drag the slider to respond"
        hint_y = track_y + S(120)
        draw_text(screen, font, hint, WHITE, (cx, hint_y))

        # Continue button
        elapsed = pygame.time.get_ticks() - t0
        enabled = slider_moved and elapsed >= min_show_ms
        draw_button(screen, btn_rect, "Continue", font, enabled=enabled)

        pygame.display.flip()
        
def run_questionnaire_intro_screen(screen, clock, font_title, font_body, min_show_ms=250):
    """
    Vertically centered intro screen before post-block questionnaire.
    """
    title = "AUTOMATED DECISION AID"
    body = (
        "The following questionnaire relates to your trust in the Automated Decision Aid. "
        "For each item, the scale ranges from strongly disagree to strongly agree. "
        "Please indicate how much you agree or disagree with the following statements "
        "by choosing the appropriate response on the scale."
    )
    prompt = "Press any key to continue"

    screen.fill(BG_INSTRUCTIONS)

    # --- Layout calculations ---
    content_width = WIDTH - S(240)
    content_x = S(120)

    line_spacing = S(10)
    blank_spacing = S(24)

    # Measure wrapped body height
    body_items = layout_rich_text_blocks(body, font_body, max_width=content_width)
    body_height = measure_rich_block_height(
        body_items,
        font_body,
        line_spacing=line_spacing,
        blank_spacing=blank_spacing,
    )

    title_height = font_title.get_height()
    prompt_height = font_body.get_height()

    block_gap = S(30)  # space between title/body/prompt

    total_height = (
        title_height
        + block_gap
        + body_height
        + block_gap
        + prompt_height
    )

    start_y = HEIGHT // 2 - total_height // 2

    # --- Draw Title ---
    title_img = font_title.render(title, True, WHITE)
    screen.blit(title_img, (WIDTH // 2 - title_img.get_width() // 2, start_y))

    # --- Draw Body ---
    body_y = start_y + title_height + block_gap

    draw_rich_text_centered(
        surface=screen,
        text=body,
        font=font_body,
        base_color=WHITE,
        rect=(content_x, body_y, content_width, body_height),
        line_spacing=line_spacing,
        blank_spacing=blank_spacing,
        vert_center=False,
    )

    # --- Draw Prompt ---
    prompt_y = body_y + body_height + block_gap

    draw_text(
        screen,
        font_body,
        prompt,
        WHITE,
        (WIDTH // 2, prompt_y + prompt_height // 2)
    )

    pygame.display.flip()

    wait_for_keypress(clock, min_show_ms=min_show_ms)
    
    
def run_postblock_questionnaire(
    screen,
    clock,
    font,
    participant_id=None,
    run_timestamp=None,
    block_name=None,
    block_idx=None,
    block_cfg=None,
):
    """
    Present all post-block QUESTION_ITEMS if enabled.
    Returns: list of response dicts, or {"quit": True}
    """
    if not ENABLE_POSTBLOCK_QUESTIONS or not QUESTION_ITEMS:
        return []

    responses = []

    for idx, item in enumerate(QUESTION_ITEMS, start=1):
        resp = run_likert_question(
            screen,
            clock,
            font,
            item,
            scale_min=QUESTION_SCALE_MIN,
            scale_max=QUESTION_SCALE_MAX,
        )

        if isinstance(resp, dict) and resp.get("quit"):
            return {"quit": True}

        responses.append({
            "participant_id": participant_id,
            "run_timestamp": run_timestamp,
            "block_idx": block_idx,
            "condition_code": block_condition_code(block_cfg) if block_cfg else None,
            "aid_condition": aid_condition_for_block(block_cfg) if block_cfg else None,
            "trial_deadline_s": trial_deadline_s_for_block(block_cfg) if block_cfg else None,
            "question_idx": idx,
            "question": item["question"],
            "left_anchor": item["left_anchor"],
            "right_anchor": item["right_anchor"],
            "response": int(resp),
            "scale_min": QUESTION_SCALE_MIN,
            "scale_max": QUESTION_SCALE_MAX,
        })

    return responses

# -----------------------------
# Helpers
# -----------------------------
def quit_clean():
    pygame.quit()
    sys.exit()

def _find_font_path(font_filename):
    for d in FONT_DIR_CANDIDATES:
        p = os.path.join(d, font_filename)
        if os.path.exists(p):
            return p
    return None

def load_font(font_filename, size):
    path = _find_font_path(font_filename)
    if path is not None:
        return pygame.font.Font(path, size)
    return pygame.font.SysFont("arial", size)

def _wrap_words_to_width(words, font, max_width):
    space_w = font.size(" ")[0]
    lines = []
    current = []
    current_w = 0

    for word in words:
        w = font.size(word)[0]
        if not current:
            current = [word]
            current_w = w
        else:
            if current_w + space_w + w <= max_width:
                current.append(word)
                current_w += space_w + w
            else:
                lines.append(current)
                current = [word]
                current_w = w

    if current:
        lines.append(current)
    return lines

def layout_rich_text_blocks(text, font, max_width):
    raw_lines = text.split("\n")
    laid_out = []

    for raw in raw_lines:
        if raw.strip() == "":
            laid_out.append(None)
            continue

        tokens = raw.split(" ")
        wrapped = _wrap_words_to_width(tokens, font, max_width=max_width)
        laid_out.extend(wrapped)

    return laid_out

def measure_rich_block_height(items, font, line_spacing=8, blank_spacing=16):
    line_h = font.get_height()
    total = 0
    for it in items:
        if it is None:
            total += blank_spacing
        else:
            total += line_h + line_spacing

    if items and items[-1] is not None:
        total -= line_spacing
    return max(0, total)

def _measure_wrapped_height(text, font, max_width, line_spacing, blank_spacing):
    items = layout_rich_text_blocks(text, font, max_width=max_width)
    return measure_rich_block_height(
        items, font, line_spacing=line_spacing, blank_spacing=blank_spacing
    )

def _draw_wrapped_at(surface, text, font, color, rect, y_start, line_spacing, blank_spacing):
    # Uses your existing wrapper (top-aligned), returns y after drawing.
    return draw_wrapped_block_centered(
        surface, text, font, color, rect, y_start=y_start,
        line_spacing=line_spacing, blank_spacing=blank_spacing
    )
    
def split_token_word_punct(token):
    i = len(token)
    while i > 0 and token[i - 1] in ".,;:!?)]}\"'":
        i -= 1
    return token[:i], token[i:]

def wait_for_keypress(clock, min_show_ms=250, require_key=None):
    """
    Block until a keypress occurs, but ignore keypresses that happen before min_show_ms.
    - require_key: pygame key constant (e.g., pygame.K_SPACE) to require a specific key.
    Hard-quit (Option/Alt+Q) still works.
    Window close is ignored.
    """
    t0 = pygame.time.get_ticks()

    while True:
        clock.tick(FPS)
        elapsed = pygame.time.get_ticks() - t0

        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                pass  # ignore window close button

            if ev.type == pygame.KEYDOWN:
                if is_hard_quit_event(ev):
                    quit_clean()

                if elapsed < min_show_ms:
                    continue

                if require_key is None:
                    return
                else:
                    if ev.key == require_key:
                        return
                      
def draw_text(screen, font, text, color, center, antialias=True):
    surf = font.render(text, antialias, color)
    rect = surf.get_rect(center=center)
    screen.blit(surf, rect)
    
def draw_rich_text_centered(
    surface,
    text,
    font,
    base_color,
    rect,
    line_spacing=8,
    blank_spacing=16,
    color_tokens=None,
    vert_center=True,
):
    if color_tokens is None:
        color_tokens = {}

    x, y, w, h = rect
    cx = x + w // 2
    line_h = font.get_height()

    items = layout_rich_text_blocks(text, font, max_width=w)
    total_h = measure_rich_block_height(items, font, line_spacing=line_spacing, blank_spacing=blank_spacing)
    start_y = y + (h - total_h) // 2 if vert_center else y

    space_w = font.size(" ")[0]
    yy = start_y

    for it in items:
        if it is None:
            yy += blank_spacing
            continue

        token_widths = []
        for tok in it:
            word, punct = split_token_word_punct(tok)
            w_word = font.size(word)[0] if word else 0
            w_punct = font.size(punct)[0] if punct else 0
            token_widths.append(w_word + w_punct)

        line_w = sum(token_widths) + space_w * (len(it) - 1)
        xx = cx - line_w // 2

        for idx, tok in enumerate(it):
            word, punct = split_token_word_punct(tok)

            if word:
                col_word = color_tokens.get(word, base_color)
                img_word = font.render(word, True, col_word)
                surface.blit(img_word, (xx, yy))
                xx += img_word.get_width()

            if punct:
                img_p = font.render(punct, True, base_color)
                surface.blit(img_p, (xx, yy))
                xx += img_p.get_width()

            if idx < len(it) - 1:
                xx += space_w

        yy += line_h + line_spacing

def draw_wrapped_block_centered(surface, text, font, color, rect, y_start,
                                line_spacing=8, blank_spacing=22):
    """
    Draw wrapped text starting at y_start (top-aligned), centered horizontally.
    Returns the y position after drawing.
    """
    x, y, w, h = rect
    cx = x + w // 2
    items = layout_rich_text_blocks(text, font, max_width=w)

    space_w = font.size(" ")[0]
    yy = y_start

    for it in items:
        if it is None:
            yy += blank_spacing
            continue

        token_widths = []
        for tok in it:
            word, punct = split_token_word_punct(tok)
            w_word = font.size(word)[0] if word else 0
            w_punct = font.size(punct)[0] if punct else 0
            token_widths.append(w_word + w_punct)

        line_w = sum(token_widths) + space_w * (len(it) - 1)
        xx = cx - line_w // 2

        for idx, tok in enumerate(it):
            word, punct = split_token_word_punct(tok)

            if word:
                img_word = font.render(word, True, color)
                surface.blit(img_word, (xx, yy))
                xx += img_word.get_width()

            if punct:
                img_p = font.render(punct, True, color)
                surface.blit(img_p, (xx, yy))
                xx += img_p.get_width()

            if idx < len(it) - 1:
                xx += space_w

        yy += font.get_height() + line_spacing

    return yy
  
def is_hard_quit_event(event) -> bool:
    """
    Hard quit shortcut:
      - Option + Q (macOS)
      - Alt + Q (Windows/Linux)

    ESC is ignored entirely.
    """
    if event.type != pygame.KEYDOWN:
        return False

    # must press Q
    if event.key != pygame.K_q:
        return False

    mods = event.mod

    # Option on Mac == Alt in pygame
    return bool(mods & (pygame.KMOD_ALT | pygame.KMOD_LALT | pygame.KMOD_RALT))
  
def build_blocks_for_participant(participant_id: int, blocks_template):
    """
    The three main condition cells are assigned with the full 3! order counterbalance.
    """
    main_blocks = [
        copy_block_config(b)
        for b in blocks_template
        if b["name"] == "AUTOMATION"
    ]
    main_blocks_by_code = {}
    for block in main_blocks:
        code = block_condition_code(block)
        if code in main_blocks_by_code:
            raise ValueError(f"Duplicate scheduled block condition code: {code}")
        main_blocks_by_code[code] = block

    expected_codes = set().union(*SCHEDULED_MAIN_BLOCK_ORDERS)
    actual_codes = set(main_blocks_by_code)
    if actual_codes != expected_codes:
        raise ValueError(
            "The scheduled design expects main block condition codes "
            f"{sorted(expected_codes)}, got {sorted(actual_codes)}."
        )

    order_idx = block_order_index_for_participant(participant_id)
    return [
        copy_block_config(main_blocks_by_code[code])
        for code in SCHEDULED_MAIN_BLOCK_ORDERS[order_idx]
    ]


def key_mapping_for_participant(participant_id: int):
    """
    Counterbalance D/J response mapping within the 12-participant cycle:
      - standard  (D->BLACK, J->WHITE)
      - flipped   (J->BLACK, D->WHITE)
    """
    flip = keymap_flip_for_participant(participant_id)

    if not flip:
        key_black = pygame.K_d
        key_white = pygame.K_j
        key_black_name = "D"
        key_white_name = "J"
    else:
        key_black = pygame.K_j
        key_white = pygame.K_d
        key_black_name = "J"
        key_white_name = "D"

    return {
        "flip": flip,
        "key_black": key_black,
        "key_white": key_white,
        "key_black_name": key_black_name,
        "key_white_name": key_white_name,
    }

def draw_countdown_timer(surface, font, ms_left, x, y, color=WHITE):
    """
    Draws a countdown timer (seconds remaining) at (x,y) top-left anchored.
    """
    sec_left = max(0.0, ms_left / 1000.0)
    txt = f"{sec_left:4.1f}s"
    img = font.render(txt, True, color)
    surface.blit(img, (x, y))
    
def draw_fixation_cross(surface, center, size, color, thickness=2):
    cx, cy = center
    pygame.draw.line(
        surface, color,
        (cx - size, cy),
        (cx + size, cy),
        thickness
    )
    pygame.draw.line(
        surface, color,
        (cx, cy - size),
        (cx, cy + size),
        thickness
    )

def draw_center_lines(surface, lines, font, color, rect, line_spacing=12, vert_center=True):
    x, y, w, h = rect
    cx = x + w // 2
    line_h = font.get_height()
    total_h = len(lines) * line_h + max(0, len(lines) - 1) * line_spacing
    start_y = y + (h - total_h) // 2 if vert_center else y

    yy = start_y
    for line in lines:
        img = font.render(line, True, color)
        surface.blit(img, (cx - img.get_width() // 2, yy))
        yy += line_h + line_spacing

def _clamp_int(x, lo, hi):
    return lo if x < lo else hi if x > hi else x

def draw_button(surface, rect, label, font, enabled=True,
                fill_enabled=(50, 50, 50), fill_disabled=(35, 35, 35),
                border_enabled=WHITE, border_disabled=(110, 110, 110),
                text_enabled=WHITE, text_disabled=(140, 140, 140)):
    """
    Simple centered label button. Returns nothing (draw-only).
    """
    if enabled:
        pygame.draw.rect(surface, fill_enabled, rect, 0, border_radius=max(1, S(10)))
        pygame.draw.rect(surface, border_enabled, rect, max(1, S(3)), border_radius=max(1, S(10)))
        col = text_enabled
    else:
        pygame.draw.rect(surface, fill_disabled, rect, 0, border_radius=max(1, S(10)))
        pygame.draw.rect(surface, border_disabled, rect, max(1, S(2)), border_radius=max(1, S(10)))
        col = text_disabled

    img = font.render(label, True, col)
    surface.blit(img, (rect.centerx - img.get_width() // 2, rect.centery - img.get_height() // 2))


def draw_samples_left_label(surface, font, trials_left):
    label = f"Samples left: {trials_left}"
    image = font.render(label, True, WHITE)
    surface.blit(image, (WIDTH - PB_PAD - PB_W, PB_PAD + PB_H + 6))


def write_csv_rows(path, rows):
    if not rows:
        return

    with open(path, "w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=rows[0].keys())
        writer.writeheader()
        writer.writerows(rows)


def run_slider_question_screen(
    screen,
    clock,
    font_title,
    font_body,
    question: str,
    initial_value: int = 50,
    min_show_ms: int = 250,
    anchors=None,
):
    """
    One-screen slider question:
      - slider ranges 0..100
      - shows current % above slider
      - shows 0/50/100 anchor ticks + labels below slider
      - requires clicking 'Continue' to proceed
    Returns: int (0..100) OR {"quit": True}
    """
    t0 = pygame.time.get_ticks()

    if anchors is None:
        anchors = [
            (0, "All incorrect"),
            (50, "Half correct and half incorrect\n(guessing at random)"),
            (100, "All correct"),
        ]

    value = _clamp_int(int(initial_value), 0, 100)
    dragging = False
    slider_moved = False

    # --- Layout ---
    content_w = WIDTH - S(240)
    content_x = S(120)

    # Slider geometry
    track_w = min(S(760), content_w)
    track_h = max(2, S(8))
    knob_r  = max(6, S(12))

    cx = WIDTH // 2
    track_x = cx - track_w // 2
    track_y = HEIGHT // 2

    track_rect = pygame.Rect(track_x, track_y, track_w, track_h)

    # Continue button
    btn_w = S(220)
    btn_h = S(64)
    btn_rect = pygame.Rect(cx - btn_w // 2, track_y + S(240), btn_w, btn_h)

    # Question text position
    q_rect = (content_x, track_y - S(240), content_w, S(180))

    def value_to_x(v):
        return track_x + int(round((v / 100.0) * track_w))

    def x_to_value(mx):
        frac = (mx - track_x) / float(track_w)
        return _clamp_int(int(round(frac * 100.0)), 0, 100)

    tick_positions = [value_to_x(v) for v, _ in anchors]

    while True:
        clock.tick(FPS)

        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                pass

            if ev.type == pygame.KEYDOWN:
                if is_hard_quit_event(ev):
                    return {"quit": True}

            if ev.type == pygame.MOUSEBUTTONDOWN and ev.button == 1:
                mx, my = ev.pos

                # Button click
                elapsed = pygame.time.get_ticks() - t0
                if btn_rect.collidepoint(mx, my) and slider_moved and elapsed >= min_show_ms:
                    return int(value)

                # Click on/near track starts dragging and sets value
                hit = track_rect.inflate(0, S(40))
                if hit.collidepoint(mx, my):
                    value = x_to_value(mx)
                    dragging = True
                    slider_moved = True

            if ev.type == pygame.MOUSEBUTTONUP and ev.button == 1:
                dragging = False

            if ev.type == pygame.MOUSEMOTION and dragging:
                mx, my = ev.pos
                value = x_to_value(mx)
                slider_moved = True

        # --- Draw ---
        screen.fill(BG_INSTRUCTIONS)

        # Question
        draw_rich_text_centered(
            surface=screen,
            text=question,
            font=font_body,
            base_color=WHITE,
            rect=q_rect,
            line_spacing=S(10),
            blank_spacing=S(18),
            vert_center=True,
        )

        # Current value label
        val_txt = f"{value}%"
        val_img = font_title.render(val_txt, True, WHITE)
        screen.blit(val_img, (cx - val_img.get_width() // 2, track_y - S(80)))

        # Track
        pygame.draw.rect(screen, LIGHT_GREY, track_rect, border_radius=max(1, S(6)))

        # Fill up to knob
        fill_w = value_to_x(value) - track_x
        if fill_w > 0:
            fill_rect = pygame.Rect(track_x, track_y, fill_w, track_h)
            pygame.draw.rect(screen, WHITE, fill_rect, border_radius=max(1, S(6)))

        # Tick marks
        for tx in tick_positions:
            pygame.draw.line(
                screen,
                WHITE,
                (tx, track_y - S(10)),
                (tx, track_y + track_h + S(10)),
                max(1, S(2))
            )

        # Knob
        knob_x = value_to_x(value)
        knob_y = track_y + track_h // 2
        pygame.draw.circle(screen, WHITE, (knob_x, knob_y), knob_r)

        # Anchor labels under slider
        label_font = load_font(FONT_LIGHT, max(8, S(FONT_SMALL_BASE)))
        label_y = track_y + S(34)

        for (val, text), tx in zip(anchors, tick_positions):
            draw_rich_text_centered(
                surface=screen,
                text=text,
                font=label_font,
                base_color=WHITE,
                rect=(tx - S(140), label_y, S(280), S(110)),
                line_spacing=S(4),
                blank_spacing=S(8),
                vert_center=False,
            )

        # Instruction line between labels and button
        hint = "Drag the slider to respond"
        hint_y = track_y + S(170)
        draw_text(screen, font_body, hint, WHITE, (cx, hint_y))

        # Continue button
        elapsed = pygame.time.get_ticks() - t0
        enabled = slider_moved and elapsed >= min_show_ms
        draw_button(screen, btn_rect, "Continue", font_body, enabled=enabled)

        pygame.display.flip()
        

def run_postblock_slider_questions(
    screen,
    clock,
    font_title,
    font_body,
    participant_id,
    run_ts,
    block_name,
    block_idx,
    block_cfg=None,
    output_dir="output",
):
    """
    Runs the appropriate set of slider questions for a block.
    Returns: list of dict rows OR {"quit": True} OR [] (if disabled)
    """
    if not ENABLE_POSTBLOCK_SLIDERS:
        return []

    if block_name == "AUTOMATION":
        items = SLIDER_ITEMS_AUTOMATION if block_has_real_aid(block_cfg) else SLIDER_ITEMS_MANUAL
    else:
        return []  # no sliders for other blocks

    rows = []
    for i, it in enumerate(items, start=1):
        if it["key"] == "perc_self_correct":
            anchors = [
                (0, "All incorrect"),
                (50, "Half correct and half incorrect\n(guessing at random)"),
                (100, "All correct"),
            ]
        else:
            anchors = [
                (0, "All incorrect"),
                (50, "Half correct and half incorrect\n(guessing at random)"),
                (100, "All correct"),
            ]

        resp = run_slider_question_screen(
            screen=screen,
            clock=clock,
            font_title=font_title,
            font_body=font_body,
            question=it["question"],
            initial_value=50,
            min_show_ms=250,
            anchors=anchors,
        )

        if isinstance(resp, dict) and resp.get("quit"):
            return {"quit": True}

        rows.append({
            "participant_id": participant_id,
            "run_timestamp": run_ts,
            "block_idx": block_idx,
            "condition_code": block_condition_code(block_cfg) if block_cfg else None,
            "aid_condition": aid_condition_for_block(block_cfg) if block_cfg else None,
            "trial_deadline_s": trial_deadline_s_for_block(block_cfg) if block_cfg else None,
            "question_idx": i,
            "question_key": it["key"],
            "question": it["question"],
            "response_percent": int(resp),
        })

    # Save per-block sliders CSV
    if rows:
        os.makedirs(output_dir, exist_ok=True)
        path = os.path.join(
            output_dir,
            f"results_p{participant_id:03d}_{run_ts}_b{block_idx:02d}_{block_name}_POSTBLOCK_SLIDERS.csv"
        )
        write_csv_rows(path, rows)
        print(f"[{block_name}] Post-block slider responses saved to: {path}")

    return rows

def sample_points_in_circle(n, center, radius):
    cx, cy = center
    pts = []
    for _ in range(n):
        ang = random.random() * 2.0 * math.pi
        rr = radius * math.sqrt(random.random())
        x = cx + rr * math.cos(ang)
        y = cy + rr * math.sin(ang)
        pts.append((x, y))
    return pts

def make_trial_dots(n_dots, vblack_prop, center, radius):
    n_vblack = int(round(n_dots * vblack_prop))
    n_vwhite = n_dots - n_vblack

    pts = sample_points_in_circle(n_dots, center, radius)
    random.shuffle(pts)

    dots = []
    for i in range(n_dots):
        col = VBLACK if i < n_vblack else VWHITE
        vx = random.uniform(-VEL_INIT_RANGE, VEL_INIT_RANGE)
        vy = random.uniform(-VEL_INIT_RANGE, VEL_INIT_RANGE)
        dots.append({"x": pts[i][0], "y": pts[i][1], "vx": vx, "vy": vy, "col": col})

    random.shuffle(dots)
    return dots, n_vblack, n_vwhite

def _latest_delta_csv(output_dir="output", participant_id=None, block_name=None):
    """
    Returns path to most recent delta_*.csv in output_dir.

    Optional filters:
      - participant_id: only files starting with f"delta_p{participant_id:03d}_"
      - block_name: only files containing _{block_name}.csv (e.g., "_CALIBRATION.csv")
    """
    if not os.path.isdir(output_dir):
        return None

    files = [
        f for f in os.listdir(output_dir)
        if f.startswith("delta_") and f.endswith(".csv")
    ]

    if participant_id is not None:
        prefix = f"delta_p{int(participant_id):03d}_"
        files = [f for f in files if f.startswith(prefix)]

    if block_name is not None:
        files = [f for f in files if f"_{block_name}.csv" in f]

    if not files:
        return None

    # Timestamp is YYYYMMDD_HHMMSS so lexicographic sort == chronological
    files.sort()
    return os.path.join(output_dir, files[-1])

def load_delta_distribution_from_csv(csv_path):
    """
    Reads delta_block_mean and delta_block_sd from a delta_*.csv file.
    Returns (mean, sd) as floats, or (None, None) if not available.
    """
    try:
        with open(csv_path, "r", newline="") as f:
            reader = csv.DictReader(f)
            row = next(reader, None)
            if not row:
                return None, None

            m = row.get("delta_block_mean", None) # observed mean over trials in calibration block
            s = row.get("delta_block_sd", None) # observed sd over trials in calibration block
            # s = row.get("delta_sd_setting", None) # use fixed sd (true value used in calibration blocks)

            m = float(m) if m not in (None, "", "None") else None
            s = float(s) if s not in (None, "", "None") else None

            return m, s
    except Exception:
        return None, None


def get_latest_calibration_delta_for_participant(participant_id, output_dir="output"):
    """
    Load the most recent CALIBRATION delta summary for this participant,
    regardless of timestamp.

    Returns:
        (delta_mean, delta_sd, path)
    where mean/sd may be None if no usable file exists.
    """
    latest_path = _latest_delta_csv(
        output_dir=output_dir,
        participant_id=participant_id,
        block_name="CALIBRATION",
    )

    if latest_path is None:
        return None, None, None

    delta_mean, delta_sd = load_delta_distribution_from_csv(latest_path)
    return delta_mean, delta_sd, latest_path
  
  
def clamp(x, lo, hi):
    return lo if x < lo else hi if x > hi else x

def sample_delta_from_mean(delta_mean, delta_sd):
    d = random.gauss(delta_mean, delta_sd)
    return clamp(d, DELTA_MIN, DELTA_MAX)

def pick_vblack_prop_from_delta(delta):
    """
    Returns V-BLACK proportion (BLACK) as 0.5 +/- delta, with random sign to balance labels.
    """
    sign = 1 if random.random() < 0.5 else -1
    p = 0.5 + sign * delta
    # keep safely away from 0/1 boundaries
    return clamp(p, 0.001, 0.999)

def burnin_step_down(trial_in_block_1based, step_start, step_min, burnin_trials):
    """
    Linearly decreases step_start -> step_min across the burn-in trials.
    From the first post-burn-in trial onward, returns step_min.
    """
    if burnin_trials <= 1:
        return step_min

    t = max(1, int(trial_in_block_1based))
    if t > burnin_trials:
        return step_min

    frac = (t - 1) / burnin_trials
    return step_start + frac * (step_min - step_start)

# Smooth Brownian motion update for dots
def update_dots(dots, center, radius):
    cx, cy = center
    r_inner = radius - DOT_RADIUS

    for d in dots:
        # 1) gently perturb velocity (adds "Brownian" wandering but smooth)
        d["vx"] += random.gauss(0.0, VEL_WANDER_SD)
        d["vy"] += random.gauss(0.0, VEL_WANDER_SD)

        # 2) apply damping (inertia / low-pass filter)
        d["vx"] *= VEL_DAMPING
        d["vy"] *= VEL_DAMPING

        # 3) clamp speed to keep things stable
        speed = math.hypot(d["vx"], d["vy"])
        if speed > VEL_MAX:
            s = VEL_MAX / speed
            d["vx"] *= s
            d["vy"] *= s

        # 4) move
        d["x"] += d["vx"]
        d["y"] += d["vy"]

        # 5) keep inside circle (reflect velocity off boundary)
        dx = d["x"] - cx
        dy = d["y"] - cy
        dist = math.hypot(dx, dy)

        if dist > r_inner:
            # outward normal
            nx = dx / dist
            ny = dy / dist

            # put on boundary
            d["x"] = cx + nx * r_inner
            d["y"] = cy + ny * r_inner

            # reflect velocity: v' = v - 2*(v·n)*n
            vdotn = d["vx"] * nx + d["vy"] * ny
            d["vx"] -= 2.0 * vdotn * nx
            d["vy"] -= 2.0 * vdotn * ny

            # optional extra damping on collision to prevent "rattling"
            d["vx"] *= COLLISION_DAMPING
            d["vy"] *= COLLISION_DAMPING

def draw_petri_dish(surface, center, radius):
    # Fill: neutral mid-grey
    pygame.draw.circle(surface, DISH_FILL, center, radius, width=0)

    # Rings/edges: 
    pygame.draw.circle(surface, DISH_RING, center, radius + 3, width=3)
    pygame.draw.circle(surface, DISH_EDGE, center, radius + 1, width=1)

def draw_progress_bar(surface, trials_left, total_trials):
    x = WIDTH - PB_PAD - PB_W
    y = PB_PAD
    pygame.draw.rect(
        surface,
        LIGHT_GREY,
        (x, y, PB_W, PB_H),
        width=max(1, S(2)),
        border_radius=max(1, S(6)),
    )
    frac_done = (total_trials - trials_left) / float(total_trials)
    fill_w = int(PB_W * frac_done)
    if fill_w > 0:
        pygame.draw.rect(surface, WHITE, (x, y, fill_w, PB_H), border_radius=max(1, S(6)))

def run_participant_number_screen(screen, clock, font) -> Dict[str, Any]:
    """
    Initial screen before instructions.
    Requires numeric participant number input before continuing.
    Clickable "Continue" button is disabled until a number is entered.

    Returns:
      {"quit": True}                          on hard quit
      {"quit": False, "participant": <int>}   on success
    """
    prompt = "Enter participant number:"
    entry = ""
    active = True  # input box focused by default

    base_h = font.get_linesize()
    gap = int(base_h * 1.2)

    # Layout
    input_w = S(260)
    input_h = int(base_h * 1.6)          # already follows font size, keep
    btn_w   = S(180)
    btn_h   = int(base_h * 1.8)          # already follows font size, keep

    cx = WIDTH // 2
    cy = HEIGHT // 2

    prompt_y = cy - 2 * gap
    input_rect = pygame.Rect(cx - input_w // 2, cy - input_h // 2, input_w, input_h)
    btn_rect = pygame.Rect(cx - btn_w // 2, input_rect.bottom + gap, btn_w, btn_h)

    # Simple blink cursor
    blink_period = 0.55
    last_blink = time.perf_counter()
    cursor_on = True

    while True:
        clock.tick(FPS)

        now = time.perf_counter()
        if (now - last_blink) >= blink_period:
            cursor_on = not cursor_on
            last_blink = now

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pass  # ignore window close button

            if event.type == pygame.KEYDOWN:
                if is_hard_quit_event(event):
                    return {"quit": True}

                # Allow Enter as a convenience *only if* valid
                if event.key in (pygame.K_RETURN, pygame.K_KP_ENTER):
                    if entry.isdigit() and len(entry) > 0:
                        return {"quit": False, "participant": int(entry)}
                    continue

                if active:
                    if event.key == pygame.K_BACKSPACE:
                        entry = entry[:-1]
                    else:
                        # digits only
                        ch = event.unicode
                        if ch.isdigit():
                            entry += ch

            if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                mx, my = event.pos

                # click input box toggles focus
                if input_rect.collidepoint(mx, my):
                    active = True
                else:
                    active = False

                # click Continue if enabled
                if btn_rect.collidepoint(mx, my):
                    if entry.isdigit() and len(entry) > 0:
                        return {"quit": False, "participant": int(entry)}

        # --- Draw ---
        screen.fill(BG_INSTRUCTIONS)

        # Prompt
        draw_text(screen, font, prompt, WHITE, (cx, prompt_y))

        # Input box
        border_col = WHITE if active else (140, 140, 140)
        pygame.draw.rect(screen, border_col, input_rect, max(1, S(2)))

        # Render entry + cursor
        show = entry
        if active and cursor_on:
            show = entry + "|"

        # keep text inside the box (simple left padding)
        pad_x   = S(10)
        text_surf = font.render(show, True, WHITE)
        text_pos = (input_rect.left + pad_x, input_rect.centery - text_surf.get_height() // 2)
        screen.blit(text_surf, text_pos)

        # Continue button (disabled until valid)
        enabled = entry.isdigit() and len(entry) > 0

        if enabled:
            pygame.draw.rect(screen, (50, 50, 50), btn_rect, 0)
            pygame.draw.rect(screen, WHITE, btn_rect, max(1, S(3)))
            label_col = WHITE
        else:
            pygame.draw.rect(screen, (35, 35, 35), btn_rect, 0)
            pygame.draw.rect(screen, (110, 110, 110), btn_rect, max(1, S(2)))
            label_col = (140, 140, 140)

        draw_text(screen, font, "Continue", label_col, btn_rect.center)

        pygame.display.flip()


def run_instructions(screen, font_title, font_body, font_body_bold, clock,
                     key_black_name, key_white_name, min_show_ms=250):

    title = "VIRUS DETECTION TASK"

    intro = (
        "As a reminder, we have identified two dangerous viruses. Unfortunately, the two strains are "
        "difficult to tell apart. Both are speckled BLACK and WHITE. The only difference "
        "visually is that one strain tends to have a little more BLACK, and the other "
        "tends to have a little more WHITE. For simplicity, we will call them V-BLACK and V-WHITE. "
        "You'll be shown a similar number of V-BLACK and V-WHITE samples. \n"
        "Your job is to evaluate the following samples to determine which virus is present.\n"
    )

    press1 = f"Press {key_black_name} if the sample looks more BLACK overall (V-BLACK)"
    press2 = f"Press {key_white_name} if the sample looks more WHITE overall (V-WHITE)"
    speed  = "Try to respond as quickly and accurately as possible\n"

    # ---- Layout constants ----
    content_x = S(120)
    content_w = WIDTH - S(240)

    line_spacing  = S(10)
    blank_spacing = S(30)

    gap_title_to_body = S(30)
    gap_between_blocks = S(12)
    gap_before_speed = S(30)
    gap_body_to_button = S(40)

    # Continue button
    btn_w = S(220)
    btn_h = S(64)
    btn_rect = pygame.Rect(WIDTH // 2 - btn_w // 2, 0, btn_w, btn_h)

    # ---- Measure heights ----
    title_h = font_title.get_height()

    intro_h  = _measure_wrapped_height(intro,  font_body,      content_w, line_spacing, blank_spacing)
    press1_h = _measure_wrapped_height(press1, font_body_bold, content_w, line_spacing, blank_spacing)
    press2_h = _measure_wrapped_height(press2, font_body_bold, content_w, line_spacing, blank_spacing)
    speed_h  = _measure_wrapped_height(speed,  font_body,      content_w, line_spacing, blank_spacing)

    total_h = (
        title_h
        + gap_title_to_body
        + intro_h
        + gap_between_blocks + press1_h
        + gap_between_blocks + press2_h
        + gap_before_speed + speed_h
        + gap_body_to_button
        + btn_h
    )

    start_y = HEIGHT // 2 - total_h // 2

    # Button position
    btn_rect.y = start_y + total_h - btn_h

    t0 = pygame.time.get_ticks()

    while True:
        clock.tick(FPS)
        elapsed = pygame.time.get_ticks() - t0
        enabled = elapsed >= min_show_ms

        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                pass

            if ev.type == pygame.KEYDOWN:
                if is_hard_quit_event(ev):
                    quit_clean()

            if ev.type == pygame.MOUSEBUTTONDOWN and ev.button == 1:
                if enabled and btn_rect.collidepoint(ev.pos):
                    return

        screen.fill(BG_INSTRUCTIONS)

        # ---- Draw title ----
        y = start_y
        title_img = font_title.render(title, True, WHITE)
        screen.blit(title_img, (WIDTH // 2 - title_img.get_width() // 2, y))

        body_rect = (content_x, 0, content_w, HEIGHT)

        y += title_h + gap_title_to_body
        y = _draw_wrapped_at(screen, intro,  font_body,      WHITE, body_rect, y, line_spacing, blank_spacing)
        y += gap_between_blocks
        y = _draw_wrapped_at(screen, press1, font_body_bold, WHITE, body_rect, y, line_spacing, blank_spacing)
        y += gap_between_blocks
        y = _draw_wrapped_at(screen, press2, font_body_bold, WHITE, body_rect, y, line_spacing, blank_spacing)
        y += gap_before_speed
        _draw_wrapped_at(screen, speed, font_body, WHITE, body_rect, y, line_spacing, blank_spacing)

        draw_button(screen, btn_rect, "Continue", font_body, enabled=enabled)

        pygame.display.flip()
  
  
def get_block_instruction_payload(block_name: str, block_cfg=None) -> dict:
    """
    Returns {"title": str, "slides": list[str]}
    """
    if block_name in BLOCK_INSTRUCTIONS:
        payload = dict(BLOCK_INSTRUCTIONS[block_name])
        slides = list(payload.get("slides", [payload.get("body", "")]))

        if block_cfg is not None and block_name == "AUTOMATION":
            if block_has_real_aid(block_cfg):
                slides = (
                    slides[:1]
                    + [
                        aid_condition_instruction_slide(block_cfg),
                        automation_accuracy_instruction_slide(),
                    ]
                    + slides[1:]
                )
            else:
                slides = [
                    manual_condition_instruction_slide(),
                ]
                payload["title"] = "MANUAL BLOCK"

        payload["slides"] = slides

        return payload

    return {"title": f"{block_name} BLOCK", "slides": ["Instructions"]}
  
  
def block_title(block_name: str, block_cfg=None) -> str:
    return get_block_instruction_payload(block_name, block_cfg=block_cfg)["title"]
  

def run_block_instructions(
    screen,
    font_title,
    font_body,
    clock,
    block_name: str,
    block_cfg=None,
    min_show_ms=250
):
    payload = get_block_instruction_payload(block_name, block_cfg=block_cfg)
    title = payload["title"]
    slides = payload["slides"]

    for slide_idx, body in enumerate(slides):
        btn_w = S(220)
        btn_h = S(64)
        btn_rect = pygame.Rect(
            WIDTH // 2 - btn_w // 2,
            0,   # set after layout
            btn_w,
            btn_h
        )

        content_x = S(120)
        content_w = WIDTH - S(240)

        line_spacing  = S(10)
        blank_spacing = S(30)

        gap_title_to_body   = S(26)
        gap_body_to_button  = S(32)

        title_h = font_title.get_height() if slide_idx == 0 else 0
        body_h  = _measure_wrapped_height(body, font_body, content_w, line_spacing, blank_spacing)

        total_h = title_h + gap_title_to_body + body_h + gap_body_to_button + btn_h
        start_y = HEIGHT // 2 - total_h // 2

        # Position button after layout is known
        y = start_y
        if slide_idx == 0:
            y += title_h + gap_title_to_body

        button_y = y + body_h + gap_body_to_button
        btn_rect.y = int(button_y)

        t0 = pygame.time.get_ticks()

        while True:
            clock.tick(FPS)
            elapsed = pygame.time.get_ticks() - t0
            enabled = elapsed >= min_show_ms

            for ev in pygame.event.get():
                if ev.type == pygame.QUIT:
                    pass

                if ev.type == pygame.KEYDOWN:
                    if is_hard_quit_event(ev):
                        quit_clean()

                if ev.type == pygame.MOUSEBUTTONDOWN and ev.button == 1:
                    if enabled and btn_rect.collidepoint(ev.pos):
                        break
            else:
                screen.fill(BG_INSTRUCTIONS)

                y_draw = start_y

                # Draw title only on first slide
                if slide_idx == 0:
                    title_img = font_title.render(title, True, WHITE)
                    screen.blit(title_img, (WIDTH // 2 - title_img.get_width() // 2, y_draw))
                    y_draw += title_h + gap_title_to_body

                body_rect = (content_x, 0, content_w, HEIGHT)

                _draw_wrapped_at(
                    screen,
                    body,
                    font_body,
                    WHITE,
                    body_rect,
                    y_draw,
                    line_spacing=line_spacing,
                    blank_spacing=blank_spacing
                )

                draw_button(screen, btn_rect, "Continue", font_body, enabled=enabled)

                pygame.display.flip()
                continue

            break
        
    
def show_feedback_screen(
    screen,
    clock,
    font,
    msg,
    bg_color=BG,
    text_color=(255, 255, 255),
    prompt_text="Press any key to continue",
    prompt_font=None,
    prompt_color=WHITE,
    min_show_ms=250,   # prevents accidental carry-through from the response key
):
    """
    Shows feedback text + prompt, and waits until ANY keypress.
    Window close ignored; hard-quit (Option/Alt+Q) still works.
    """
    if prompt_font is None:
        prompt_font = font

    t0 = pygame.time.get_ticks()

    while True:
        clock.tick(FPS)
        elapsed = pygame.time.get_ticks() - t0

        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                pass  # ignore window close button
            if ev.type == pygame.KEYDOWN:
                if is_hard_quit_event(ev):
                    quit_clean()
                if elapsed >= min_show_ms:
                    return  # any key continues

        screen.fill(bg_color)

        # Main message (center)
        img = font.render(msg, True, text_color)
        screen.blit(
            img,
            (WIDTH // 2 - img.get_width() // 2,
             HEIGHT // 2 - img.get_height() // 2)
        )

        # Prompt (below)
        pimg = prompt_font.render(prompt_text, True, prompt_color)
        screen.blit(
            pimg,
            (WIDTH // 2 - pimg.get_width() // 2,
             HEIGHT // 2 + img.get_height() // 2 + S(16))
        )

        pygame.display.flip()


def press_any_key_screen(
    screen,
    clock,
    font,
    msg="Press any key to continue",
    bg_color=BG_INSTRUCTIONS,
    text_color=WHITE,
    min_show_ms=250,
):
    """
    Blocking inter-trial screen: shows centered text and waits for any keypress.
    Window close button ignored; hard-quit (Option/Alt+Q) still works.
    """
    screen.fill(bg_color)
    draw_text(screen, font, msg, text_color, (WIDTH // 2, HEIGHT // 2))
    pygame.display.flip()

    wait_for_keypress(clock, min_show_ms=min_show_ms)


def fixation_cross_screen(screen, clock, ms):
    t0 = pygame.time.get_ticks()
    center = (WIDTH // 2, HEIGHT // 2)

    while pygame.time.get_ticks() - t0 < ms:
        clock.tick(FPS)
        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                pass
            if ev.type == pygame.KEYDOWN:
                if is_hard_quit_event(ev):
                    quit_clean()

        screen.fill(BG)

        # Draw central fixation cross
        draw_fixation_cross(
            screen,
            center=center,
            size=FIX_SIZE,
            color=FIX_COLOR,
            thickness=FIX_THICKNESS
        )

        pygame.display.flip()


def display_label_for_response(response):
    if response == "BLACK":
        return "V-BLACK"
    if response == "WHITE":
        return "V-WHITE"
    return str(response)


def display_label_for_aid_recommendation(response):
    if response in ("BLACK", "WHITE", "#####"):
        return response
    return str(response)


def render_text_fit_width(font, text, color, max_width):
    img = font.render(text, True, color)
    if img.get_width() <= max_width:
        return img
    scale = max_width / float(img.get_width())
    new_size = (max(1, int(round(img.get_width() * scale))), max(1, int(round(img.get_height() * scale))))
    return pygame.transform.smoothscale(img, new_size)


def decision_phase_label(initial_response=None) -> str:
    return "Final decision?" if initial_response in ("BLACK", "WHITE") else "Initial decision"


def response_key_prompt_rect(font, y_pos=None, key_black_name="D", key_white_name="J"):
    if y_pos is None:
        y_pos = HEIGHT - S(80)

    meaning_by_key = {
        key_black_name: "BLACK",
        key_white_name: "WHITE",
    }

    def label_for_key(key_name: str):
        return "V-BLACK" if meaning_by_key.get(key_name) == "BLACK" else "V-WHITE"

    left_top = label_for_key("D")
    right_top = label_for_key("J")
    left_bottom = "Press D"
    right_bottom = "Press J"

    col_gap = 80
    line_gap = 4
    left_w = max(font.size(left_top)[0], font.size(left_bottom)[0])
    right_w = max(font.size(right_top)[0], font.size(right_bottom)[0])
    total_w = left_w + col_gap + right_w
    total_h = font.get_height() * 2 + line_gap
    return pygame.Rect(
        WIDTH // 2 - total_w // 2,
        y_pos,
        total_w,
        total_h,
    )


def default_stimulus_disc_bottom_y():
    return HEIGHT // 2 + S(20) + DISH_RADIUS


def phase_label_center_y(phase_font, prompt_rect, stimulus_bottom_y=None):
    if stimulus_bottom_y is None:
        stimulus_bottom_y = default_stimulus_disc_bottom_y()

    label_half_h = phase_font.get_height() // 2
    clearance = max(2, S(4))
    target_y = int(round((stimulus_bottom_y + prompt_rect.top) / 2.0))
    min_y = int(stimulus_bottom_y + label_half_h + clearance)
    max_y = int(prompt_rect.top - label_half_h - clearance)

    if min_y <= max_y:
        return max(min_y, min(target_y, max_y))
    return min(target_y, max_y)


def draw_bottom_phase_label(
    screen,
    font,
    label,
    y_pos=None,
    key_black_name="D",
    key_white_name="J",
    phase_font=None,
    stimulus_bottom_y=None,
    prompt_rect=None,
):
    if phase_font is None:
        phase_font = load_font(FONT_BOLD, max(9, S(FONT_SMALL_BASE)))
    if prompt_rect is None:
        prompt_rect = response_key_prompt_rect(font, y_pos, key_black_name, key_white_name)
    label_img = phase_font.render(label, True, DECISION_PHASE_COLOR)
    label_rect = label_img.get_rect(
        center=(WIDTH // 2, phase_label_center_y(phase_font, prompt_rect, stimulus_bottom_y))
    )
    screen.blit(label_img, label_rect)
    return label_rect


def draw_trial_prompt_stacked(screen, font_small, y_pos=None, key_black_name="D",
                              key_white_name="J"):
    """
    Bottom response prompt: D is fixed left and J is fixed right, while
    V-BLACK/V-WHITE meaning follows the participant-specific key mapping.
    """
    if y_pos is None:
        y_pos = HEIGHT - S(80)

    meaning_by_key = {
        key_black_name: "BLACK",
        key_white_name: "WHITE",
    }

    def label_and_color_for_key(key_name: str):
        if meaning_by_key.get(key_name) == "BLACK":
            return "V-BLACK", BLACK
        return "V-WHITE", WHITE

    left_top, left_col = label_and_color_for_key("D")
    right_top, right_col = label_and_color_for_key("J")
    left_bottom = "Press D"
    right_bottom = "Press J"

    lt_img = font_small.render(left_top, True, left_col)
    lb_img = font_small.render(left_bottom, True, left_col)
    rt_img = font_small.render(right_top, True, right_col)
    rb_img = font_small.render(right_bottom, True, right_col)

    col_gap = 80
    line_gap = 4
    left_w = max(lt_img.get_width(), lb_img.get_width())
    right_w = max(rt_img.get_width(), rb_img.get_width())
    total_w = left_w + col_gap + right_w
    start_x = WIDTH // 2 - total_w // 2

    y_top = y_pos
    y_bottom = y_pos + font_small.get_height() + line_gap
    screen.blit(lt_img, (start_x + (left_w - lt_img.get_width()) // 2, y_top))
    screen.blit(lb_img, (start_x + (left_w - lb_img.get_width()) // 2, y_bottom))

    right_x = start_x + left_w + col_gap
    screen.blit(rt_img, (right_x + (right_w - rt_img.get_width()) // 2, y_top))
    screen.blit(rb_img, (right_x + (right_w - rb_img.get_width()) // 2, y_bottom))

    prompt_rect = pygame.Rect(
        start_x,
        y_top,
        total_w,
        y_bottom + font_small.get_height() - y_top,
    )
    return prompt_rect


def make_aid_recommendation(stimulus, accuracy):
    if random.random() < accuracy:
        return stimulus, True
    other = "WHITE" if stimulus == "BLACK" else "BLACK"
    return other, False


def _build_aid_recommendation_layout(
    font_label,
    font_main,
    rec_label,
    show_value=True,
    transparency_level="none",
    evidence_black_pct=None,
    evidence_white_pct=None,
):
    line_gap = max(1, S(4))
    detail_font = load_font(FONT_LIGHT, max(10, S(FONT_SMALL_BASE - 1)))

    img_label = font_label.render("AID JUDGES:", True, WHITE)
    img_main = None
    detail_imgs = []

    if show_value:
        phrase = display_label_for_aid_recommendation(rec_label)
        col = MASKED_AID_COLOR if rec_label == "#####" else COLOR_TOKENS_AID.get(rec_label, WHITE)
        img_main = font_main.render(phrase, True, col)

        detail_lines = []
        if transparency_level in ("low", "high"):
            detail_lines.append(f"Reason: The available evidence favors {phrase}")

        if transparency_level == "high":
            detail_lines.append(
                f"Basis: Stimulus scan estimates {evidence_black_pct:.1f}% BLACK and {evidence_white_pct:.1f}% WHITE"
            )
            detail_lines.append("Decision rule: Choose the higher-evidence category")

        detail_imgs = [detail_font.render(line, True, WHITE) for line in detail_lines]

    total_height = img_label.get_height()
    if img_main is not None:
        total_height += line_gap + img_main.get_height()
    if detail_imgs:
        total_height += len(detail_imgs) * line_gap + sum(img.get_height() for img in detail_imgs)

    return {
        "label_img": img_label,
        "value_img": img_main,
        "detail_imgs": detail_imgs,
        "line_gap": line_gap,
        "total_height": total_height,
    }


def _draw_aid_recommendation_layout(screen, layout, cx, y0):
    img_label = layout["label_img"]
    img_main = layout["value_img"]
    detail_imgs = layout["detail_imgs"]
    line_gap = layout["line_gap"]

    rect_label = img_label.get_rect(midtop=(cx, y0))
    screen.blit(img_label, rect_label)

    value_rect = None
    detail_rects = []
    if img_main is not None:
        current_y = rect_label.bottom + line_gap
        value_rect = img_main.get_rect(midtop=(cx, current_y))
        screen.blit(img_main, value_rect)
        current_y = value_rect.bottom + line_gap

        for detail_img in detail_imgs:
            detail_rect = detail_img.get_rect(midtop=(cx, current_y))
            screen.blit(detail_img, detail_rect)
            detail_rects.append(detail_rect)
            current_y = detail_rect.bottom + line_gap

    return {
        "label_rect": rect_label,
        "value_rect": value_rect,
        "detail_rects": detail_rects,
    }


def draw_aid_recommendation_top_center(
    screen,
    font_label,   # small font for "RECOMMENDATION:"
    font_main,    # large font for recommendation
    rec_label,
    show_value=True,
    transparency_level="none",
    evidence_black_pct=None,
    evidence_white_pct=None,
    dish_top_limit=None,
):
    cx = WIDTH // 2
    top_padding = S(8)
    layout = _build_aid_recommendation_layout(
        font_label,
        font_main,
        rec_label,
        show_value=show_value,
        transparency_level=transparency_level,
        evidence_black_pct=evidence_black_pct,
        evidence_white_pct=evidence_white_pct,
    )

    y0 = top_padding
    if dish_top_limit is not None:
        max_bottom = dish_top_limit - S(18)
        y0 = min(y0, max_bottom - layout["total_height"])
        y0 = max(S(4), y0)

    return _draw_aid_recommendation_layout(screen, layout, cx, y0)


def draw_aid_recommendation_centered(
    screen,
    font_label,
    font_main,
    rec_label,
    show_value=True,
    transparency_level="none",
    evidence_black_pct=None,
    evidence_white_pct=None,
):
    layout = _build_aid_recommendation_layout(
        font_label,
        font_main,
        rec_label,
        show_value=show_value,
        transparency_level=transparency_level,
        evidence_black_pct=evidence_black_pct,
        evidence_white_pct=evidence_white_pct,
    )
    cx = WIDTH // 2
    y0 = HEIGHT // 2 - layout["total_height"] // 2
    return _draw_aid_recommendation_layout(screen, layout, cx, y0)


def parse_cli_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--block",
        type=str,
        default=None,
        help="Run only a selected block. Valid value: AUTOMATION",
    )
    parser.add_argument(
        "--aid-condition",
        type=str,
        default=None,
        help="Select an AUTOMATION block by aid condition.",
    )
    args = parser.parse_args()

    if args.block is not None:
        args.block = args.block.upper()
        if args.block != "AUTOMATION":
            parser.error("--block must be AUTOMATION for scheduled main-block runs")

    if args.aid_condition is not None and args.block is None:
        parser.error("--aid-condition requires --block")
    if args.aid_condition is not None and args.block != "AUTOMATION":
        parser.error("--aid-condition can only be used with --block AUTOMATION")
    if args.aid_condition is not None:
        args.aid_condition = args.aid_condition.lower()
        if args.aid_condition not in AUTOMATION_AID_CONDITIONS:
            parser.error(
                "--aid-condition must be one of: "
                + ", ".join(sorted(AUTOMATION_AID_CONDITIONS))
            )

    return args

def select_single_block(block_name: str, blocks_template, participant_id: int, aid_condition=None):
    """
    Return block configs matching block_name and, when needed, aid_condition.
    Raises a clear error if the block is not available in BLOCKS.
    """
    matches = [copy_block_config(b) for b in blocks_template if b["name"] == block_name]

    if not matches:
        available = sorted(set(b["name"] for b in blocks_template))
        raise ValueError(
            f"Unknown block '{block_name}'. Available blocks in this script: {available}"
        )

    if aid_condition is not None:
        matches = [
            b for b in matches
            if aid_condition_for_block(b) == aid_condition
        ]
        if not matches:
            available = sorted(
                set(
                    aid_condition_for_block(b)
                    for b in blocks_template
                    if b["name"] == block_name
                )
            )
            raise ValueError(
                f"No {block_name} block has aid condition '{aid_condition}'. "
                f"Available aid conditions for this block: {available}"
            )

    if len(matches) > 1:
        available = sorted(
            set(aid_condition_for_block(b) for b in matches)
        )
        raise ValueError(
            f"Block '{block_name}' has multiple aid-condition variants. "
            f"Pass --aid-condition with one of: {available}"
        )

    return [copy_block_config(matches[0])]


def create_display_surface():
    flags = pygame.FULLSCREEN if FULLSCREEN else 0
    if FULLSCREEN and USE_DESKTOP_RES:
        return pygame.display.set_mode((0, 0), flags)
    return pygame.display.set_mode((BASE_W, BASE_H))


def initialize_ui_metrics(screen):
    global WIDTH, HEIGHT
    WIDTH, HEIGHT = screen.get_size()

    global UI_SCALE
    UI_SCALE = compute_ui_scale(WIDTH, HEIGHT)
    print("[UI_SCALE]", UI_SCALE, "for", WIDTH, "x", HEIGHT)

    global DISH_RADIUS, DOT_RADIUS, VEL_WANDER_SD, VEL_MAX, VEL_INIT_RANGE
    DISH_RADIUS = S(DISH_RADIUS_BASE)
    DOT_RADIUS = max(1, S(DOT_RADIUS_BASE))
    VEL_WANDER_SD = SF(VEL_WANDER_SD_BASE)
    VEL_MAX = SF(VEL_MAX_BASE)
    VEL_INIT_RANGE = SF(VEL_INIT_RANGE_BASE)

    global FIX_SIZE, FIX_THICKNESS, PB_W, PB_H, PB_PAD
    FIX_SIZE = S(FIX_SIZE_BASE)
    FIX_THICKNESS = max(1, S(FIX_THICKNESS_BASE))
    PB_W = S(PB_W_BASE)
    PB_H = S(PB_H_BASE)
    PB_PAD = S(PB_PAD_BASE)


def load_ui_fonts():
    return {
        "title": load_font(FONT_LIGHT, max(12, S(FONT_TITLE_BASE))),
        "body": load_font(FONT_LIGHT, max(10, S(FONT_BODY_BASE))),
        "body_bold": load_font(FONT_BOLD, max(10, S(FONT_BODY_BASE))),
        "small": load_font(FONT_LIGHT, max(9, S(FONT_SMALL_BASE))),
        "phase_label": load_font(FONT_BOLD, max(9, S(FONT_SMALL_BASE))),
        "aid_label": load_font(FONT_LIGHT, max(9, S(FONT_AID_LABEL_BASE))),
        "aid": load_font(FONT_BOLD, max(10, S(FONT_AID_BASE))),
    }


def choose_blocks_to_run(args, participant_id):
    if args.block is not None:
        blocks_to_run = select_single_block(
            args.block,
            BLOCKS,
            participant_id=participant_id,
            aid_condition=args.aid_condition,
        )
        print(
            "[SINGLE BLOCK MODE]",
            participant_id,
            "->",
            [block_condition_code(b) for b in blocks_to_run],
        )
        return blocks_to_run

    blocks_to_run = build_blocks_for_participant(participant_id, BLOCKS)
    print(
        "[BLOCK ORDER]",
        participant_id,
        "->",
        [block_condition_code(b) for b in blocks_to_run],
    )
    return blocks_to_run


def resolve_difficulty_mode(block_cfg):
    if block_cfg["FIXED_DELTA_ON"]:
        return "fixed_delta"
    if block_cfg["STAIRCASE_ON"]:
        return "staircase"
    return "fixed_props"


def resolve_fixed_delta_source(block_name, participant_id, fixed_delta_value, fixed_delta_sd):
    print(f"[{block_name}] Using fixed delta mean {fixed_delta_value}, sd={fixed_delta_sd}")
    return fixed_delta_value, fixed_delta_sd


def apply_practice_delta_to_block(block_cfg, delta_mean, delta_sd, source_path=None):
    try:
        mean_value = float(delta_mean)
    except (TypeError, ValueError):
        mean_value = None

    if mean_value is None or not math.isfinite(mean_value):
        print(
            f"[{block_condition_code(block_cfg)}] Practice delta mean unavailable; "
            "using default fixed delta."
        )
        return block_cfg

    try:
        sd_value = float(delta_sd)
    except (TypeError, ValueError):
        sd_value = 0.0

    if not math.isfinite(sd_value):
        sd_value = 0.0

    block_cfg["FIXED_DELTA_ON"] = True
    block_cfg["STAIRCASE_ON"] = False
    block_cfg["FIXED_DELTA_VALUE"] = mean_value
    block_cfg["FIXED_DELTA_SD"] = sd_value

    source_detail = f" from {source_path}" if source_path else ""
    print(
        f"[{block_condition_code(block_cfg)}] Using practice-calibrated delta "
        f"mean {mean_value}, sd={sd_value}{source_detail}"
    )
    return block_cfg


def prepare_block_state(block_cfg, participant_id):
    difficulty_mode = resolve_difficulty_mode(block_cfg)
    transparency = block_cfg.get("AID_TRANSPARENCY", "none")
    if transparency not in AID_TRANSPARENCY_LEVELS:
        raise ValueError(
            f"Unsupported AID_TRANSPARENCY '{transparency}' for block '{block_cfg['name']}'. "
            f"Valid values: {sorted(AID_TRANSPARENCY_LEVELS)}"
        )
    fixed_delta_mean = None
    fixed_delta_sd = None

    if difficulty_mode == "fixed_delta":
        fixed_delta_mean, fixed_delta_sd = resolve_fixed_delta_source(
            block_name=block_cfg["name"],
            participant_id=participant_id,
            fixed_delta_value=block_cfg["FIXED_DELTA_VALUE"],
            fixed_delta_sd=block_cfg.get("FIXED_DELTA_SD", 0.0),
        )

    if difficulty_mode == "fixed_props":
        vblack_props = [random.choice(VBLACK_PROPORTION_LEVELS) for _ in range(block_cfg["N_TRIALS"])]
        random.shuffle(vblack_props)
    else:
        vblack_props = None

    staircase_target_accuracy = None
    delta_step_up = None
    if difficulty_mode == "staircase":
        staircase_target_accuracy = float(
            block_cfg.get("TARGET_ACC", CALIBRATION_TARGET_ACCURACY)
        )
        if not 0.0 < staircase_target_accuracy < 1.0:
            raise ValueError(
                f"TARGET_ACC must be between 0 and 1 for block '{block_cfg['name']}'"
            )
        delta_step_up = DELTA_STEP_DOWN * (
            staircase_target_accuracy / (1.0 - staircase_target_accuracy)
        )

    return {
        "difficulty_mode": difficulty_mode,
        "fixed_delta_mean": fixed_delta_mean,
        "fixed_delta_sd": fixed_delta_sd,
        "aid_transparency": transparency,
        "vblack_props": vblack_props,
        "staircase_target_accuracy": staircase_target_accuracy,
        "delta_mean": DELTA_INIT,
        "deltas_realised": [],
        "delta_step_up_setting": delta_step_up,
    }


def show_block_intro(screen, clock, fonts, block_cfg, keymap):
    run_instructions(
        screen,
        fonts["title"],
        fonts["body"],
        fonts["body_bold"],
        clock,
        keymap["key_black_name"],
        keymap["key_white_name"],
        min_show_ms=250,
    )

    run_block_instructions(
        screen=screen,
        font_title=fonts["title"],
        font_body=fonts["body"],
        clock=clock,
        block_name=block_cfg["name"],
        block_cfg=block_cfg,
        min_show_ms=250,
    )

    screen.fill(BG_INSTRUCTIONS)
    draw_center_lines(
        screen,
        [block_title(block_cfg["name"], block_cfg=block_cfg), "Press any key to begin"],
        fonts["body"],
        WHITE,
        rect=(0, 0, WIDTH, HEIGHT),
        line_spacing=S(14),
        vert_center=True,
    )
    pygame.display.flip()
    wait_for_keypress(clock, min_show_ms=250)
    if not block_cfg["AUTOMATION_ON"]:
        fixation_cross_screen(screen, clock, FIXATION_DURATION_MS)


def pick_trial_vblack_prop(block_state, trial_index):
    difficulty_mode = block_state["difficulty_mode"]
    if difficulty_mode == "fixed_props":
        return block_state["vblack_props"][trial_index], None

    if difficulty_mode == "fixed_delta":
        delta_realised = sample_delta_from_mean(
            block_state["fixed_delta_mean"],
            block_state["fixed_delta_sd"],
        )
        return pick_vblack_prop_from_delta(delta_realised), delta_realised

    delta_realised = sample_delta_from_mean(block_state["delta_mean"], DELTA_SD)
    block_state["deltas_realised"].append(delta_realised)
    return pick_vblack_prop_from_delta(delta_realised), delta_realised


def draw_trial_frame(screen, dot_layer, dots, center, aid_payload, ui_payload, ms_left=None, initial_response=None):
    fonts = ui_payload["fonts"]
    key_names = ui_payload.get("key_names", {"black": "D", "white": "J"})

    screen.fill(BG)
    if ms_left is not None:
        draw_countdown_timer(
            surface=screen,
            font=fonts["body"],
            ms_left=ms_left,
            x=PB_PAD,
            y=PB_PAD,
            color=WHITE,
        )

    draw_progress_bar(screen, trials_left=ui_payload["trials_left"], total_trials=ui_payload["n_trials"])
    draw_samples_left_label(screen, fonts["small"], ui_payload["trials_left"])
    draw_petri_dish(screen, center, DISH_RADIUS)

    dot_layer.fill((0, 0, 0, 0))
    for dot in dots:
        x = int(dot["x"])
        y = int(dot["y"])
        r, g, b = dot["col"]
        pygame.draw.circle(dot_layer, (r, g, b, DOT_ALPHA), (x, y), DOT_RADIUS)
    screen.blit(dot_layer, (0, 0))

    prompt_rect = draw_trial_prompt_stacked(
        screen,
        fonts["small"],
        key_black_name=key_names["black"],
        key_white_name=key_names["white"],
    )
    draw_bottom_phase_label(
        screen,
        fonts["small"],
        decision_phase_label(initial_response),
        key_black_name=key_names["black"],
        key_white_name=key_names["white"],
        phase_font=fonts["phase_label"],
        stimulus_bottom_y=center[1] + DISH_RADIUS,
        prompt_rect=prompt_rect,
    )

    if aid_payload["mode"] == "automation":
        draw_aid_recommendation_top_center(
            screen,
            fonts["aid_label"],
            fonts["aid"],
            aid_payload["label"],
            show_value=aid_payload["visible"],
            transparency_level=aid_payload["transparency_level"],
            evidence_black_pct=aid_payload["evidence_black_pct"],
            evidence_white_pct=aid_payload["evidence_white_pct"],
            dish_top_limit=center[1] - DISH_RADIUS,
        )
    elif aid_payload["mode"] == "masked":
        draw_aid_recommendation_top_center(
            screen,
            fonts["aid_label"],
            fonts["aid"],
            rec_label="#####",
            show_value=True,
            transparency_level="none",
            dish_top_limit=center[1] - DISH_RADIUS,
        )


def run_blank_phase(screen, clock, duration_ms):
    t0 = pygame.time.get_ticks()

    while pygame.time.get_ticks() - t0 < duration_ms:
        clock.tick(FPS)
        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                pass
            if ev.type == pygame.KEYDOWN and is_hard_quit_event(ev):
                quit_clean()

        screen.fill(BG)
        pygame.display.flip()


def draw_aid_only_frame(
    screen,
    aid_payload,
    ui_payload,
    show_prompt=False,
    initial_response=None,
    phase_label=None,
):
    fonts = ui_payload["fonts"]
    key_names = ui_payload.get("key_names", {"black": "D", "white": "J"})

    screen.fill(BG)
    draw_progress_bar(screen, trials_left=ui_payload["trials_left"], total_trials=ui_payload["n_trials"])
    draw_samples_left_label(screen, fonts["small"], ui_payload["trials_left"])
    draw_aid_recommendation_centered(
        screen,
        fonts["aid_label"],
        fonts["aid"],
        aid_payload["label"],
        show_value=True,
        transparency_level=aid_payload["transparency_level"],
        evidence_black_pct=aid_payload["evidence_black_pct"],
        evidence_white_pct=aid_payload["evidence_white_pct"],
    )

    if show_prompt:
        prompt_rect = draw_trial_prompt_stacked(
            screen,
            fonts["small"],
            key_black_name=key_names["black"],
            key_white_name=key_names["white"],
        )
        draw_bottom_phase_label(
            screen,
            fonts["small"],
            decision_phase_label(initial_response),
            key_black_name=key_names["black"],
            key_white_name=key_names["white"],
            phase_font=fonts["phase_label"],
            prompt_rect=prompt_rect,
        )
    elif phase_label:
        draw_bottom_phase_label(
            screen,
            fonts["small"],
            phase_label,
            key_black_name=key_names["black"],
            key_white_name=key_names["white"],
            phase_font=fonts["phase_label"],
        )


def draw_masked_placeholder_frame(screen, ui_payload, show_prompt=False, initial_response=None, phase_label=None):
    fonts = ui_payload["fonts"]
    key_names = ui_payload.get("key_names", {"black": "D", "white": "J"})

    screen.fill(BG)
    draw_progress_bar(screen, trials_left=ui_payload["trials_left"], total_trials=ui_payload["n_trials"])
    draw_samples_left_label(screen, fonts["small"], ui_payload["trials_left"])

    draw_aid_recommendation_centered(
        screen,
        fonts["aid_label"],
        fonts["aid"],
        rec_label="#####",
        show_value=True,
        transparency_level="none",
    )

    if show_prompt:
        prompt_rect = draw_trial_prompt_stacked(
            screen,
            fonts["small"],
            key_black_name=key_names["black"],
            key_white_name=key_names["white"],
        )
        draw_bottom_phase_label(
            screen,
            fonts["small"],
            decision_phase_label(initial_response),
            key_black_name=key_names["black"],
            key_white_name=key_names["white"],
            phase_font=fonts["phase_label"],
            prompt_rect=prompt_rect,
        )
    elif phase_label:
        draw_bottom_phase_label(
            screen,
            fonts["small"],
            phase_label,
            key_black_name=key_names["black"],
            key_white_name=key_names["white"],
            phase_font=fonts["phase_label"],
        )


def draw_final_decision_frame(screen, ui_payload, initial_response=None, ms_left=None):
    fonts = ui_payload["fonts"]
    key_names = ui_payload.get("key_names", {"black": "D", "white": "J"})

    screen.fill(BG)
    if ms_left is not None:
        draw_countdown_timer(
            surface=screen,
            font=fonts["body"],
            ms_left=ms_left,
            x=PB_PAD,
            y=PB_PAD,
            color=WHITE,
        )

    draw_progress_bar(screen, trials_left=ui_payload["trials_left"], total_trials=ui_payload["n_trials"])
    draw_samples_left_label(screen, fonts["small"], ui_payload["trials_left"])
    prompt_rect = draw_trial_prompt_stacked(
        screen,
        fonts["small"],
        key_black_name=key_names["black"],
        key_white_name=key_names["white"],
    )
    draw_bottom_phase_label(
        screen,
        fonts["small"],
        decision_phase_label(initial_response),
        key_black_name=key_names["black"],
        key_white_name=key_names["white"],
        phase_font=fonts["phase_label"],
        prompt_rect=prompt_rect,
    )


def run_aid_preview_phase(screen, clock, aid_payload, ui_payload, duration_ms):
    t0 = pygame.time.get_ticks()

    while pygame.time.get_ticks() - t0 < duration_ms:
        clock.tick(FPS)
        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                pass
            if ev.type == pygame.KEYDOWN and is_hard_quit_event(ev):
                quit_clean()

        draw_aid_only_frame(screen, aid_payload, ui_payload, show_prompt=False, phase_label="Preview")
        pygame.display.flip()


def run_masked_preview_phase(screen, clock, ui_payload, duration_ms):
    t0 = pygame.time.get_ticks()

    while pygame.time.get_ticks() - t0 < duration_ms:
        clock.tick(FPS)
        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                pass
            if ev.type == pygame.KEYDOWN and is_hard_quit_event(ev):
                quit_clean()

        draw_masked_placeholder_frame(screen, ui_payload, show_prompt=False, phase_label="Preview")
        pygame.display.flip()


def decision2_preview_display_for_condition(aid_condition, aid_label):
    if aid_condition == "stimulus_first":
        return "aid_only", aid_label
    if aid_condition in {"manual", "aid_first", "simultaneous"}:
        return "masked_placeholder", "#####"
    raise ValueError(
        f"Unsupported aid condition '{aid_condition}'. "
        f"Valid values: {sorted(AUTOMATION_AID_CONDITIONS)}"
    )


def run_decision2_preview_phase(screen, clock, aid_condition, aid_payload, ui_payload, duration_ms):
    preview_display, preview_label = decision2_preview_display_for_condition(
        aid_condition,
        aid_payload["label"],
    )
    if preview_display == "aid_only":
        run_aid_preview_phase(screen, clock, aid_payload, ui_payload, duration_ms)
    else:
        run_masked_preview_phase(screen, clock, ui_payload, duration_ms)
    return preview_display, preview_label


def collect_key_response(screen, clock, keymap, draw_frame_fn, deadline_ms=None, update_fn=None):
    phase_start_ticks = pygame.time.get_ticks()
    phase_start_perf = time.perf_counter()

    while True:
        clock.tick(FPS)
        now = pygame.time.get_ticks()

        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                pass
            if ev.type == pygame.KEYDOWN:
                if is_hard_quit_event(ev):
                    quit_clean()
                if ev.key == keymap["key_black"]:
                    return {
                        "response": "BLACK",
                        "rt_ms": (time.perf_counter() - phase_start_perf) * 1000.0,
                    }
                if ev.key == keymap["key_white"]:
                    return {
                        "response": "WHITE",
                        "rt_ms": (time.perf_counter() - phase_start_perf) * 1000.0,
                    }

        if deadline_ms is not None and (now - phase_start_ticks) >= deadline_ms:
            return {"response": "TIMEOUT", "rt_ms": None}

        if update_fn is not None:
            update_fn()

        ms_left = None
        if deadline_ms is not None:
            ms_left = deadline_ms - (now - phase_start_ticks)

        draw_frame_fn(ms_left)
        pygame.display.flip()


def collect_stimulus_response(screen, clock, dot_layer, dots, center, keymap, aid_payload,
                              ui_payload, show_aid=False, show_masked_aid=False,
                              deadline_ms=None, initial_response=None):
    def update_fn():
        update_dots(dots, center, DISH_RADIUS)

    def draw_frame(ms_left):
        draw_trial_frame(
            screen,
            dot_layer,
            dots,
            center,
            {
                "mode": "masked" if show_masked_aid else ("automation" if show_aid else "none"),
                "label": aid_payload["label"],
                "visible": bool(show_aid),
                "transparency_level": aid_payload["transparency_level"],
                "evidence_black_pct": aid_payload["evidence_black_pct"],
                "evidence_white_pct": aid_payload["evidence_white_pct"],
            },
            ui_payload,
            ms_left=ms_left,
            initial_response=initial_response,
        )

    return collect_key_response(
        screen,
        clock,
        keymap=keymap,
        draw_frame_fn=draw_frame,
        deadline_ms=deadline_ms,
        update_fn=update_fn,
    )


def collect_aid_only_response(screen, clock, keymap, aid_payload, ui_payload, initial_response=None):
    def draw_frame(ms_left):
        draw_aid_only_frame(screen, aid_payload, ui_payload, show_prompt=True, initial_response=initial_response)

    return collect_key_response(
        screen,
        clock,
        keymap=keymap,
        draw_frame_fn=draw_frame,
        deadline_ms=None,
        update_fn=None,
    )


def collect_masked_response(
    screen,
    clock,
    center,
    keymap,
    ui_payload,
    initial_response=None,
):
    def draw_frame(ms_left):
        draw_masked_placeholder_frame(
            screen,
            ui_payload,
            show_prompt=True,
            initial_response=initial_response,
        )

    return collect_key_response(
        screen,
        clock,
        keymap=keymap,
        draw_frame_fn=draw_frame,
        deadline_ms=None,
        update_fn=None,
    )


def collect_final_decision_response(screen, clock, keymap, ui_payload, initial_response=None):
    def draw_frame(ms_left):
        draw_final_decision_frame(
            screen,
            ui_payload,
            initial_response=initial_response,
            ms_left=ms_left,
        )

    return collect_key_response(
        screen,
        clock,
        keymap=keymap,
        draw_frame_fn=draw_frame,
        deadline_ms=None,
        update_fn=None,
    )


def response_correct(response, stimulus):
    if response in ("BLACK", "WHITE"):
        return response == stimulus
    return None


def response_matches_aid(response, aid_label):
    if response in ("BLACK", "WHITE") and aid_label in ("BLACK", "WHITE"):
        return response == aid_label
    return None


def make_decision_record(result, stimulus, aid_label, display_type):
    response = result["response"]
    return {
        "response": response,
        "correct": response_correct(response, stimulus),
        "rt_ms": result["rt_ms"],
        "display": display_type,
        "matches_aid": response_matches_aid(response, aid_label),
    }


def update_staircase_state(block_state, is_correct, trial_in_block, target_acc):
    if block_state["difficulty_mode"] != "staircase":
        return None, None

    step_down_now = burnin_step_down(
        trial_in_block_1based=trial_in_block,
        step_start=DELTA_STEP_DOWN,
        step_min=DELTA_STEP_DOWN_MIN,
        burnin_trials=BURNIN_TRIALS,
    )
    step_up_now = step_down_now * (target_acc / (1.0 - target_acc))

    if is_correct:
        block_state["delta_mean"] = max(DELTA_MIN, block_state["delta_mean"] - step_down_now)
    else:
        block_state["delta_mean"] = min(DELTA_MAX, block_state["delta_mean"] + step_up_now)

    return step_down_now, step_up_now


def build_trial_row(participant_id, run_timestamp, keymap, block_name, block_idx, trial_number,
                    global_trial_index, block_cfg, block_state, trial_data, feedback_msg,
                    delta_realised, step_down_now, step_up_now):
    difficulty_mode = block_state["difficulty_mode"]
    has_real_aid = block_has_real_aid(block_cfg)

    return {
        "participant_id": participant_id,
        "run_timestamp": run_timestamp,
        "key_black": keymap["key_black_name"],
        "key_white": keymap["key_white_name"],
        "keymap_flip": keymap["flip"],
        "block_idx": block_idx,
        "condition_code": block_condition_code(block_cfg),
        "aid_condition": aid_condition_for_block(block_cfg),
        "trial_deadline_s": trial_deadline_s_for_block(block_cfg),
        "trial": trial_number,
        "global_trial": global_trial_index,
        "difficulty_mode": difficulty_mode,
        "staircase_target_accuracy": block_state["staircase_target_accuracy"],
        "delta_fixed_mean": block_state["fixed_delta_mean"] if difficulty_mode == "fixed_delta" else None,
        "delta_fixed_sd": block_state["fixed_delta_sd"] if difficulty_mode == "fixed_delta" else None,
        "delta_stair_realised": delta_realised if difficulty_mode == "staircase" else None,
        "delta_stair_mean": block_state["delta_mean"] if difficulty_mode == "staircase" else None,
        "delta_step_down_used": step_down_now if difficulty_mode == "staircase" else None,
        "delta_step_up_used": step_up_now if difficulty_mode == "staircase" else None,
        "vblack_prop": trial_data["vblack_prop"],
        "n_vblack": trial_data["n_vblack"],
        "n_vwhite": trial_data["n_vwhite"],
        "auto_on": 1 if has_real_aid else 0,
        "aid_accuracy_setting": block_cfg["AID_ACCURACY"] if has_real_aid else None,
        "stimulus": trial_data["stimulus"],
        "aid_label": trial_data["aid_label"],
        "aid_correct": trial_data["aid_correct"],
        "preview_display": trial_data["preview_display"],
        "preview_label": trial_data["preview_label"],
        "decision1_display": trial_data["decision1_display"],
        "decision1_response": trial_data["decision1_response"],
        "decision1_correct": trial_data["decision1_correct"],
        "decision1_rt_s": (trial_data["decision1_rt_ms"] / 1000.0) if trial_data["decision1_rt_ms"] is not None else None,
        "decision1_matches_aid": trial_data["decision1_matches_aid"],
        "decision2_preview_display": trial_data["decision2_preview_display"],
        "decision2_preview_label": trial_data["decision2_preview_label"],
        "decision2_display": trial_data["decision2_display"],
        "decision2_label": trial_data["decision2_label"],
        "decision2_response": trial_data["decision2_response"],
        "decision2_correct": trial_data["decision2_correct"],
        "decision2_rt_s": (trial_data["decision2_rt_ms"] / 1000.0) if trial_data["decision2_rt_ms"] is not None else None,
        "decision2_matches_aid": trial_data["decision2_matches_aid"],
        "changed_response": trial_data["changed_response"],
        "feedback": feedback_msg if block_cfg["TRIAL_FEEDBACK_ON"] else None,
    }


def maybe_show_feedback(screen, clock, fonts, response, correct, feedback_on):
    if not feedback_on:
        return None

    if response == "TIMEOUT":
        feedback_msg = "TOO SLOW"
        feedback_color = FEEDBACK_SLOW_COLOR
    elif correct:
        feedback_msg = "CORRECT"
        feedback_color = FEEDBACK_CORRECT_COLOR
    else:
        feedback_msg = "INCORRECT"
        feedback_color = FEEDBACK_ERROR_COLOR

    show_feedback_screen(
        screen=screen,
        clock=clock,
        font=fonts["title"],
        msg=feedback_msg,
        bg_color=BG,
        text_color=feedback_color,
        prompt_text="Press any key to continue",
        prompt_font=fonts["body"],
        prompt_color=WHITE,
        min_show_ms=250,
    )
    return feedback_msg


def run_single_trial(screen, clock, dot_layer, center, fonts, keymap, block_cfg, block_state,
                     trials_left, trial_number, global_trial_index, run_timestamp):
    vblack_prop, delta_realised = pick_trial_vblack_prop(block_state, trial_number - 1)
    dots, n_vblack, n_vwhite = make_trial_dots(N_DOTS, vblack_prop, center, DISH_RADIUS)
    stimulus = "BLACK" if n_vblack > n_vwhite else "WHITE"

    if block_has_real_aid(block_cfg):
        aid_label, aid_correct = make_aid_recommendation(stimulus, accuracy=block_cfg["AID_ACCURACY"])
    else:
        aid_label, aid_correct = None, None

    evidence_black_pct = (n_vblack / N_DOTS) * 100.0
    evidence_white_pct = (n_vwhite / N_DOTS) * 100.0

    ui_payload = {
        "fonts": fonts,
        "trials_left": trials_left,
        "n_trials": block_cfg["N_TRIALS"],
        "key_names": {"black": keymap["key_black_name"], "white": keymap["key_white_name"]},
    }
    aid_render_payload = {
        "label": aid_label,
        "transparency_level": block_state["aid_transparency"],
        "evidence_black_pct": evidence_black_pct,
        "evidence_white_pct": evidence_white_pct,
    }

    decision1 = {
        "response": None,
        "correct": None,
        "rt_ms": None,
        "display": None,
        "matches_aid": None,
    }
    decision2 = dict(decision1)
    preview_display = None
    preview_label = None
    decision2_preview_display = None
    decision2_preview_label = None
    decision2_label = None

    if block_cfg["AUTOMATION_ON"]:
        fixation_cross_screen(screen, clock, FIXATION_DURATION_MS)
        aid_condition = aid_condition_for_block(block_cfg)

        if aid_condition == "manual":
            preview_display = "masked_placeholder"
            preview_label = "#####"
            run_masked_preview_phase(screen, clock, ui_payload, AUTOMATION_PRE_PHASE_MS)
            fixation_cross_screen(screen, clock, FIXATION_DURATION_MS)
            decision1_result = collect_stimulus_response(
                screen, clock, dot_layer, dots, center, keymap,
                aid_render_payload, ui_payload, deadline_ms=None,
            )
            decision1 = make_decision_record(
                decision1_result, stimulus, aid_label, display_type="stimulus_only"
            )

        elif aid_condition == "simultaneous":
            preview_display = "masked_placeholder"
            preview_label = "#####"
            run_masked_preview_phase(screen, clock, ui_payload, AUTOMATION_PRE_PHASE_MS)
            fixation_cross_screen(screen, clock, FIXATION_DURATION_MS)
            decision1_result = collect_stimulus_response(
                screen, clock, dot_layer, dots, center, keymap,
                aid_render_payload, ui_payload, show_aid=True, deadline_ms=None,
            )
            decision1 = make_decision_record(
                decision1_result, stimulus, aid_label, display_type="aid_stimulus"
            )

        elif aid_condition == "aid_first":
            preview_display = "aid_only"
            preview_label = aid_label
            run_aid_preview_phase(
                screen, clock, aid_render_payload, ui_payload,
                duration_ms=AUTOMATION_PRE_PHASE_MS,
            )
            fixation_cross_screen(screen, clock, FIXATION_DURATION_MS)
            decision1_result = collect_stimulus_response(
                screen, clock, dot_layer, dots, center, keymap,
                aid_render_payload, ui_payload, show_aid=False, deadline_ms=None,
            )
            decision1 = make_decision_record(
                decision1_result, stimulus, aid_label, display_type="stimulus_only"
            )

        elif aid_condition == "stimulus_first":
            preview_display = "masked_placeholder"
            preview_label = "#####"
            run_masked_preview_phase(screen, clock, ui_payload, AUTOMATION_PRE_PHASE_MS)
            fixation_cross_screen(screen, clock, FIXATION_DURATION_MS)
            decision1_result = collect_stimulus_response(
                screen, clock, dot_layer, dots, center, keymap,
                aid_render_payload, ui_payload, show_aid=False, deadline_ms=None,
            )
            decision1 = make_decision_record(
                decision1_result, stimulus, aid_label, display_type="stimulus_only"
            )

        else:
            raise ValueError(
                f"Unsupported aid condition '{aid_condition}'. "
                f"Valid values: {sorted(AUTOMATION_AID_CONDITIONS)}"
            )

        fixation_cross_screen(screen, clock, FIXATION_DURATION_MS)
        decision2_preview_display, decision2_preview_label = run_decision2_preview_phase(
            screen,
            clock,
            aid_condition,
            aid_render_payload,
            ui_payload,
            duration_ms=AUTOMATION_PRE_PHASE_MS,
        )
        fixation_cross_screen(screen, clock, FIXATION_DURATION_MS)
        decision2_result = collect_final_decision_response(
            screen,
            clock,
            keymap,
            ui_payload,
            initial_response=decision1["response"],
        )
        decision2 = make_decision_record(
            decision2_result, stimulus, aid_label, display_type="blank_response"
        )

        response = decision2["response"]
        rt_ms = decision2["rt_ms"]
        correct = response_correct(response, stimulus)
        correct_for_feedback = bool(correct)
        final_response = response
        initial_response = decision1["response"]

    else:
        final = collect_stimulus_response(
            screen,
            clock,
            dot_layer,
            dots,
            center,
            keymap,
            aid_render_payload,
            ui_payload,
            show_aid=False,
            deadline_ms=trial_deadline_ms_for_block(block_cfg),
        )
        response = final["response"]
        rt_ms = final["rt_ms"]
        correct = response_correct(response, stimulus)
        correct_for_feedback = bool(correct)
        final_response = response
        initial_response = None

    if initial_response in ("BLACK", "WHITE") and final_response in ("BLACK", "WHITE"):
        changed_response = initial_response != final_response
    else:
        changed_response = None

    feedback_msg = maybe_show_feedback(screen, clock, fonts, response, correct_for_feedback, block_cfg["TRIAL_FEEDBACK_ON"])
    step_down_now, step_up_now = update_staircase_state(
        block_state,
        correct_for_feedback,
        trial_number,
        block_state["staircase_target_accuracy"],
    )

    row = build_trial_row(
        participant_id=block_cfg["participant_id"],
        run_timestamp=run_timestamp,
        keymap=keymap,
        block_name=block_cfg["name"],
        block_idx=block_cfg["block_idx"],
        trial_number=trial_number,
        global_trial_index=global_trial_index,
        block_cfg=block_cfg,
        block_state=block_state,
        trial_data={
            "vblack_prop": vblack_prop,
            "n_vblack": n_vblack,
            "n_vwhite": n_vwhite,
            "stimulus": stimulus,
            "aid_label": aid_label,
            "aid_correct": aid_correct,
            "preview_display": preview_display,
            "preview_label": preview_label,
            "decision1_display": decision1["display"],
            "decision1_response": decision1["response"],
            "decision1_correct": decision1["correct"],
            "decision1_rt_ms": decision1["rt_ms"],
            "decision1_matches_aid": decision1["matches_aid"],
            "decision2_preview_display": decision2_preview_display,
            "decision2_preview_label": decision2_preview_label,
            "decision2_display": decision2["display"],
            "decision2_label": decision2_label,
            "decision2_response": decision2["response"],
            "decision2_correct": decision2["correct"],
            "decision2_rt_ms": decision2["rt_ms"],
            "decision2_matches_aid": decision2["matches_aid"],
            "changed_response": changed_response,
        },
        feedback_msg=feedback_msg,
        delta_realised=delta_realised,
        step_down_now=step_down_now,
        step_up_now=step_up_now,
    )

    return row


def write_delta_summary(output_dir, participant_id, run_timestamp, block_name, block_idx, deltas_realised,
                        delta_mean, delta_step_up, staircase_target_accuracy):
    delta_out_path = os.path.join(
        output_dir,
        f"delta_p{participant_id:03d}_{run_timestamp}_b{block_idx:02d}_{block_name}.csv"
    )

    burn = int(BURNIN_TRIALS) if BURNIN_TRIALS is not None else 0
    deltas_post_burnin = deltas_realised[burn:] if burn > 0 else deltas_realised[:]

    if CALIB_SUMMARY_LAST_N is None:
        deltas_summary = deltas_post_burnin
        summary_last_n_used = None
    else:
        n_last = max(1, int(CALIB_SUMMARY_LAST_N))
        deltas_summary = deltas_post_burnin[-n_last:]
        summary_last_n_used = n_last

    if deltas_summary:
        mean_delta = sum(deltas_summary) / len(deltas_summary)
        if len(deltas_summary) > 1:
            var = sum((x - mean_delta) ** 2 for x in deltas_summary) / (len(deltas_summary) - 1)
            sd_delta = math.sqrt(var)
        else:
            sd_delta = 0.0
    else:
        mean_delta, sd_delta = None, None

    row = {
        "participant_id": participant_id,
        "run_timestamp": run_timestamp,
        "block_idx": block_idx,
        "n_trials_total": len(deltas_realised),
        "burnin_trials_excluded": burn,
        "n_trials_post_burnin": len(deltas_post_burnin),
        "summary_last_n_setting": summary_last_n_used,
        "n_trials_summarised": len(deltas_summary),
        "staircase_target_accuracy": staircase_target_accuracy,
        "delta_init": DELTA_INIT,
        "delta_sd_setting": DELTA_SD,
        "delta_step_down": DELTA_STEP_DOWN,
        "delta_step_up": delta_step_up,
        "delta_min": DELTA_MIN,
        "delta_max": DELTA_MAX,
        "delta_block_mean": mean_delta,
        "delta_block_sd": sd_delta,
        "delta_mean_final": delta_mean,
    }

    with open(delta_out_path, "w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=row.keys())
        writer.writeheader()
        writer.writerow(row)

    print(
        f"[{block_name}] Staircase delta summary saved to: {delta_out_path} "
        f"(post-burnin n={len(deltas_post_burnin)}, summarised n={len(deltas_summary)})"
    )

    return mean_delta, sd_delta, delta_out_path


def show_block_complete_screen(screen, clock, font_body, block_name, block_cfg=None):
    end_lines = [
        f"{block_title(block_name, block_cfg=block_cfg)} COMPLETE",
        "Press any key to continue",
    ]

    screen.fill(BG_INSTRUCTIONS)
    draw_center_lines(
        screen,
        end_lines,
        font_body,
        WHITE,
        rect=(0, 0, WIDTH, HEIGHT),
        line_spacing=S(14),
        vert_center=True,
    )
    pygame.display.flip()
    wait_for_keypress(clock, min_show_ms=250)


def run_post_block_measures(screen, clock, fonts, participant_id, run_timestamp, block_cfg, output_dir,
                            all_postblock_slider_rows, all_questionnaire_rows):
    block_name = block_cfg["name"]
    block_idx = block_cfg["block_idx"]

    if ENABLE_POSTBLOCK_SLIDERS and block_name == "AUTOMATION":
        slider_rows = run_postblock_slider_questions(
            screen=screen,
            clock=clock,
            font_title=fonts["title"],
            font_body=fonts["body"],
            participant_id=participant_id,
            run_ts=run_timestamp,
            block_name=block_name,
            block_idx=block_idx,
            block_cfg=block_cfg,
            output_dir=output_dir,
        )
        if isinstance(slider_rows, dict) and slider_rows.get("quit"):
            quit_clean()
        if slider_rows:
            all_postblock_slider_rows.extend(slider_rows)

    if block_name == "AUTOMATION" and block_has_real_aid(block_cfg) and ENABLE_POSTBLOCK_QUESTIONS:
        run_questionnaire_intro_screen(
            screen=screen,
            clock=clock,
            font_title=fonts["title"],
            font_body=fonts["body"],
            min_show_ms=250,
        )

        questionnaire_rows = run_postblock_questionnaire(
            screen,
            clock,
            fonts["body"],
            participant_id=participant_id,
            run_timestamp=run_timestamp,
            block_name=block_name,
            block_idx=block_idx,
            block_cfg=block_cfg,
        )
        if isinstance(questionnaire_rows, dict) and questionnaire_rows.get("quit"):
            quit_clean()
        if questionnaire_rows:
            q_path = os.path.join(
                output_dir,
                f"results_p{participant_id:03d}_{run_timestamp}_b{block_idx:02d}_{block_name}_POSTBLOCK.csv"
            )
            write_csv_rows(q_path, questionnaire_rows)
            print(f"[{block_name}] Questionnaire saved to: {q_path}")
            all_questionnaire_rows.extend(questionnaire_rows)


def save_combined_outputs(output_dir, participant_id, run_timestamp, all_results,
                          all_postblock_slider_rows, all_questionnaire_rows):
    os.makedirs(output_dir, exist_ok=True)

    if all_postblock_slider_rows:
        sliders_all_path = os.path.join(
            output_dir,
            f"results_p{participant_id:03d}_{run_timestamp}_b00_POSTBLOCK_SLIDERS_ALL.csv"
        )
        write_csv_rows(sliders_all_path, all_postblock_slider_rows)
        print(f"[ALL] Post-block sliders saved to: {sliders_all_path}")

    all_csv_path = os.path.join(output_dir, f"results_p{participant_id:03d}_{run_timestamp}_b00_ALL.csv")
    write_csv_rows(all_csv_path, all_results)
    print(f"[ALL] Results saved to: {all_csv_path}")

    if all_questionnaire_rows:
        q_all_path = os.path.join(output_dir, f"results_p{participant_id:03d}_{run_timestamp}_b00_POSTBLOCK_ALL.csv")
        write_csv_rows(q_all_path, all_questionnaire_rows)
        print(f"[ALL] Questionnaire saved to: {q_all_path}")


def compute_performance_score(all_results):
    scored_trials = [row for row in all_results if row.get("decision2_correct") is not None]
    if not scored_trials:
        return 0.0

    n_correct = sum(1 for row in scored_trials if row["decision2_correct"] is True)
    return (n_correct / len(scored_trials)) * 100.0


def run_trial_block(screen, clock, dot_layer, center, fonts, keymap, block_cfg,
                    run_timestamp, output_dir, global_trial_index):
    block_state = prepare_block_state(
        block_cfg,
        block_cfg["participant_id"],
    )
    block_results = []

    for t in range(block_cfg["N_TRIALS"]):
        trial_number = t + 1
        trials_left = block_cfg["N_TRIALS"] - t
        global_trial_index += 1
        row = run_single_trial(
            screen,
            clock,
            dot_layer,
            center,
            fonts,
            keymap,
            block_cfg,
            block_state,
            trials_left,
            trial_number,
            global_trial_index,
            run_timestamp,
        )
        block_results.append(row)

        if t != block_cfg["N_TRIALS"] - 1:
            if not block_cfg["TRIAL_FEEDBACK_ON"]:
                press_any_key_screen(
                    screen=screen,
                    clock=clock,
                    font=fonts["body"],
                    msg="Press any key to continue",
                    bg_color=BG_INSTRUCTIONS,
                    text_color=WHITE,
                )
            if not block_cfg["AUTOMATION_ON"]:
                fixation_cross_screen(screen, clock, FIXATION_DURATION_MS)

    os.makedirs(output_dir, exist_ok=True)
    block_csv_path = os.path.join(
        output_dir,
        f"results_p{block_cfg['participant_id']:03d}_{run_timestamp}_"
        f"b{block_cfg['block_idx']:02d}_{block_cfg['name']}.csv"
    )
    write_csv_rows(block_csv_path, block_results)
    print(f"[{block_cfg['name']}] Results saved to: {block_csv_path}")

    delta_summary = None
    if block_state["difficulty_mode"] == "staircase":
        delta_summary = write_delta_summary(
            output_dir=output_dir,
            participant_id=block_cfg["participant_id"],
            run_timestamp=run_timestamp,
            block_name=block_cfg["name"],
            block_idx=block_cfg["block_idx"],
            deltas_realised=block_state["deltas_realised"],
            delta_mean=block_state["delta_mean"],
            delta_step_up=block_state["delta_step_up_setting"],
            staircase_target_accuracy=block_state["staircase_target_accuracy"],
        )

    return block_results, global_trial_index, delta_summary
  
  
# -----------------------------
# Main experiment
# -----------------------------
def main():
    args = parse_cli_args()
    pygame.init()
    pygame.display.set_caption("Virus Detection Task")
    screen = create_display_surface()

    # Hide mouse cursor
    pygame.event.set_grab(True)
    pygame.mouse.set_visible(True)  # Set 'False' to hide cursor
    initialize_ui_metrics(screen)
    fonts = load_ui_fonts()

    # Alpha dot layer (draw dots here, then blit to screen)
    dot_layer = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)

    clock = pygame.time.Clock()

    # ---- Participant ID screen (BEFORE instructions) ----
    res = run_participant_number_screen(screen, clock, fonts["body"])
    if res.get("quit", False):
        quit_clean()

    participant_id = res["participant"]
    keymap = key_mapping_for_participant(participant_id)
    print(
        "[KEY MAP]",
        participant_id,
        f"-> V-BLACK={keymap['key_black_name']}, V-WHITE={keymap['key_white_name']}",
        "(flipped)" if keymap["flip"] else "(standard)",
    )

    center = (WIDTH // 2, HEIGHT // 2 + S(20))
    all_results = []
    all_postblock_slider_rows = []
    all_questionnaire_rows = []
    global_trial_index = 0
    output_dir = "output"

    blocks_to_run = choose_blocks_to_run(args, participant_id)
    practice_delta_summary = None

    if args.block is None:
        practice_cfg = copy_block_config(PRACTICE_BLOCK)
        practice_cfg["block_idx"] = 0
        practice_cfg["participant_id"] = participant_id
        show_block_intro(screen, clock, fonts, practice_cfg, keymap)
        _, _, practice_delta_summary = run_trial_block(
            screen=screen,
            clock=clock,
            dot_layer=dot_layer,
            center=center,
            fonts=fonts,
            keymap=keymap,
            block_cfg=practice_cfg,
            run_timestamp=run_ts,
            output_dir=output_dir,
            global_trial_index=0,
        )
        show_block_complete_screen(
            screen,
            clock,
            fonts["body"],
            practice_cfg["name"],
            block_cfg=practice_cfg,
        )

    for b_idx, blk in enumerate(blocks_to_run, start=1):
        block_cfg = copy_block_config(blk)
        block_cfg["block_idx"] = b_idx
        block_cfg["participant_id"] = participant_id
        if practice_delta_summary is not None:
            practice_delta_mean, practice_delta_sd, practice_delta_path = practice_delta_summary
            apply_practice_delta_to_block(
                block_cfg,
                practice_delta_mean,
                practice_delta_sd,
                source_path=practice_delta_path,
            )
        show_block_intro(screen, clock, fonts, block_cfg, keymap)
        block_results, global_trial_index, _ = run_trial_block(
            screen=screen,
            clock=clock,
            dot_layer=dot_layer,
            center=center,
            fonts=fonts,
            keymap=keymap,
            block_cfg=block_cfg,
            run_timestamp=run_ts,
            output_dir=output_dir,
            global_trial_index=global_trial_index,
        )
        all_results.extend(block_results)

        show_block_complete_screen(screen, clock, fonts["body"], block_cfg["name"], block_cfg=block_cfg)
        run_post_block_measures(
            screen,
            clock,
            fonts,
            participant_id,
            run_ts,
            block_cfg,
            output_dir,
            all_postblock_slider_rows,
            all_questionnaire_rows,
        )

    save_combined_outputs(
        output_dir,
        participant_id,
        run_ts,
        all_results,
        all_postblock_slider_rows,
        all_questionnaire_rows,
    )

    perf_score = compute_performance_score(all_results)
    
    # Final end screen (ESC allowed ONLY here)
    screen.fill(BG_INSTRUCTIONS)
    draw_center_lines(
        screen,
        [
            "EXPERIMENT COMPLETE",
            f"Performance score: {perf_score:.1f}% correct",
            "Please alert the experimenter now"
        ],
        fonts["body"],
        WHITE,
        rect=(0, 0, WIDTH, HEIGHT),
        line_spacing=S(14),
        vert_center=True,
    )
    pygame.display.flip()

    # gate ESC only (hard quit still works inside wait_for_keypress)
    wait_for_keypress(clock, min_show_ms=250, require_key=pygame.K_ESCAPE)
    quit_clean()


if __name__ == "__main__":
    main()
