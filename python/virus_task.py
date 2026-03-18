"""
Random Dot Classification (Bartlett & McCarley–style)

Keys:
- D = Black dominant
- J = White dominant
- Any key = advance from instructions
- ESC = quit (from end-of-block screen only)
"""

import argparse
import sys
import random
import math
import os
import pygame
import time
import csv
from datetime import datetime

# -----------------------------
# Run start time
# -----------------------------
run_ts = int(time.time())   # unix format
run_ts = datetime.fromtimestamp(run_ts).strftime("%Y%m%d_%H%M%S")


# -----------------------------
# Block definitions
# -----------------------------
BLOCKS = [
    # dict(
    #     name="TRAINING",
    #     N_TRIALS=2,
    #     AUTOMATION_ON=False,
    #     AID_ACCURACY=0.90,
    #     STAIRCASE_ON=True,
    #     TARGET_ACC=0.80,
    #     FIXED_DELTA_ON=False,
    #     FIXED_DELTA_VALUE=0.10,   # not used here, but harmless
    #     TRIAL_FEEDBACK_ON=True,
    # ),
    dict(
        name="CALIBRATION",
        N_TRIALS=200,
        AUTOMATION_ON=False,
        AID_ACCURACY=0.90,
        STAIRCASE_ON=True,
        TARGET_ACC=0.80,
        FIXED_DELTA_ON=False,
        FIXED_DELTA_VALUE=0.10,   # not used here, but harmless
        TRIAL_FEEDBACK_ON=True,
    ),
    dict(
        name="MANUAL",
        N_TRIALS=200,
        AUTOMATION_ON=False,
        AID_ACCURACY=0.90,        # not used (automation off), but harmless
        STAIRCASE_ON=False,
        TARGET_ACC=0.80,          # not used (staircase off), but harmless
        FIXED_DELTA_ON=True,
        FIXED_DELTA_VALUE=0.10,   # not used here, but harmless
        TRIAL_FEEDBACK_ON=True,
        SHOW_AID_MASKED=True,
    ),
    dict(
        name="AUTOMATION1",
        N_TRIALS=2,
        AUTOMATION_ON=True,
        AID_ACCURACY=0.95,        # high reliability block
        STAIRCASE_ON=False,
        TARGET_ACC=0.80,          # not used (staircase off), but harmless
        FIXED_DELTA_ON=True,
        FIXED_DELTA_VALUE=0.10,   # fallback if no delta file found
        TRIAL_FEEDBACK_ON=True,
    ),
    dict(
        name="AUTOMATION2",
        N_TRIALS=2,
        AUTOMATION_ON=True,
        AID_ACCURACY=0.65,        # low reliability block
        STAIRCASE_ON=False,
        TARGET_ACC=0.80,          # not used (staircase off), but harmless
        FIXED_DELTA_ON=True,
        FIXED_DELTA_VALUE=0.10,   # fallback if no delta file found
        TRIAL_FEEDBACK_ON=True,
    ),
]

BLOCK_INSTRUCTIONS = {

    "CALIBRATION": {
        "title": "MANUAL BLOCK",
        "slides": [
            "You will now begin your first block of trials."
        ],
    },

    "MANUAL": {
        "title": "MANUAL BLOCK",
        "slides": [
            "In this block, there is no special information shown at the top of the display.\nThere is simply a string '#####', which you should ignore."
        ],
    },

    "AUTOMATION1": {
        "title": "AUTOMATION BLOCK",
        "slides": [

            # Slide 1
            (
            "You will be provided with an automated decision aid to assist you with this task. "
            "The automation will recommend a classification (either BLACK or WHITE) for each sample. "
            "The recommended classification will be presented at the top of the display. "
            "If the aid shows 'BLACK', this means that the automation recommends you classify "
            "that sample as V-BLACK. If it shows 'WHITE', this means that the automation "
            "recommends you classify the sample as V-WHITE."
            ),

            # Slide 2
            (
            "In the next block, although the automation is highly reliable, it is not perfect, "
            "and automation advice errors are unlikely but still possible."
            ),

            # Slide 3
            (
            "In the event that the automation makes an incorrect recommendation, "
            "it is essential that you perform the correct action. "
            "Remember that deciding whether a sample is V-BLACK or V-WHITE "
            "is your responsibility."
            ),
        ],
    },

    "AUTOMATION2": {
        "title": "AUTOMATION BLOCK",
        "slides": [

            # Slide 1 (same as above)
            (
            "You will be provided with an automated decision aid to assist you with this task. "
            "The automation will recommend a classification (either BLACK or WHITE) for each sample. "
            "The recommended classification will be presented at the top of the display. "
            "If the aid shows 'BLACK', this means that the automation recommends you classify "
            "that sample as V-BLACK. If it shows 'WHITE', this means that the automation "
            "recommends you classify the sample as V-WHITE."
            ),

            # Slide 2 (different reliability text)
            (
            "In the next block, although the automation is reasonably reliable, it is not perfect, "
            "and automation advice errors may be relatively common."
            ),

            # Slide 3
            (
            "In the event that the automation makes an incorrect recommendation, "
            "it is essential that you perform the correct action. "
            "Remember that deciding whether a sample is V-BLACK or V-WHITE "
            "is your responsibility."
            ),
        ],
    },
}


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

# Automated aid onset (relative to dot stimulus onset)
# 0   = aid appears simultaneously with dot stimulus
# >0  = aid delayed (ms after dot onset)
# <0  = aid advanced (ms before dot onset)
AID_ONSET_MS = 0

# -----------------------------
# Adaptive staircase (manual-only)
# -----------------------------
DELTA_INIT = 0.20            # starting distance from 0.5 (e.g., 0.20 -> 0.30/0.70)
DELTA_SD   = 0.01            # sampling SD for trial-wise delta in staircase mode
DELTA_MIN  = 1/N_DOTS        # hardest allowed (closest to 0.5)
DELTA_MAX  = 0.25            # easiest allowed
DELTA_STEP_DOWN = 0.02       # starting (large) down-step
DELTA_STEP_DOWN_MIN = 0.001  # target min down-step by trial 50
BURNIN_TRIALS = 50           # reach DELTA_STEP_DOWN_MIN at trial 50 (inclusive)

# -----------------------------
# Fixed-delta mode (works for automation and manual blocks)
# -----------------------------
# FIXED_DELTA_ON = False        # True = ignore staircase + fixed props, use FIXED_DELTA_VALUE
# FIXED_DELTA_VALUE = 0.10      # e.g., carry over from previous manual staircase block

# -----------------------------
# Feedback screen (post-trial)
# -----------------------------
FEEDBACK_MS = 1000   # duration of feedback screen in ms
FEEDBACK_CORRECT_COLOR = (0, 200, 0)
FEEDBACK_ERROR_COLOR   = (220, 50, 50)
FEEDBACK_SLOW_COLOR    = (200, 200, 0)

# Trial timing
FIXATION_DURATION_MS = 750
TRIAL_DEADLINE_MS = 10000   # 10 seconds

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
TEXT_COLORING_UI_ON = False      # instructions + bottom prompt ("D = BLACK", "J = WHITE")
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
    {
        "key": "perc_self_correct",
        "question": "For your responses, what percentage do you think were correct in the preceding block of trials?",
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
        "by ticking the appropriate box."
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
    base_name=None,
    participant_id=None,
    block_name=None,
    block_idx=None,
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
            "block": block_name,
            "block_idx": block_idx,
            "question_index": idx,
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
  
import itertools

def build_blocks_for_participant(participant_id: int, blocks_template):
    """
    TRAINING + CALIBRATION stay fixed.

    MANUAL/AUTOMATION1/AUTOMATION2 are counterbalanced across 6 possible orders.
    Participant assignment:
        idx = (participant_id - 1) % 6
        order = permutations([MANUAL, AUTOMATION1, AUTOMATION2])[idx]
    """
    by_name = {b["name"]: dict(b) for b in blocks_template}  # copy dicts
    # fixed = [by_name["TRAINING"], by_name["CALIBRATION"]]
    fixed = [by_name["CALIBRATION"]]

    tail_names = ["MANUAL", "AUTOMATION1", "AUTOMATION2"]
    all_orders = list(itertools.permutations(tail_names, 3))  # 6 permutations

    idx = (participant_id - 1) % len(all_orders)
    chosen_order = all_orders[idx]

    tail = [by_name[name] for name in chosen_order]
    return fixed + tail

def key_mapping_for_participant(participant_id: int):
    """
    Flip key mapping in 6-participant chunks:
      - p 1-6:   standard  (D->BLACK, J->WHITE)
      - p 7-12:  flipped   (J->BLACK, D->WHITE)
      - p 13-18: standard
      - p 19-24: flipped
      ... etc
    """
    chunk = (participant_id - 1) // 6
    flip = (chunk % 2) == 1

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
            (50, "Half correct and half incorrect"),
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
    automation_on: bool,
    output_dir="output",
):
    """
    Runs the appropriate set of slider questions for a block.
    Returns: list of dict rows OR {"quit": True} OR [] (if disabled)
    """
    if not ENABLE_POSTBLOCK_SLIDERS:
        return []

    if block_name in ("CALIBRATION", "MANUAL"):
        items = SLIDER_ITEMS_MANUAL
    elif block_name in ("AUTOMATION1", "AUTOMATION2"):
        items = SLIDER_ITEMS_AUTOMATION
    else:
        return []  # no sliders for other blocks

    rows = []
    for i, it in enumerate(items, start=1):
        if it["key"] == "perc_self_correct":
            anchors = [
                (0, "All incorrect"),
                (50, "Half correct and half incorrect"),
                (100, "All correct"),
            ]
        else:
            anchors = [
                (0, "All incorrect"),
                (50, "Half correct and half incorrect"),
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
            "block": block_name,
            "block_idx": block_idx,
            "question_index": i,
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
        with open(path, "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=rows[0].keys())
            w.writeheader()
            w.writerows(rows)
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
    Linearly decreases step_start -> step_min over trials 1..burnin_trials.
    From trial burnin_trials onward, returns step_min.
    """
    if burnin_trials <= 1:
        return step_min

    t = max(1, int(trial_in_block_1based))
    if t >= burnin_trials:
        return step_min

    frac = (t - 1) / (burnin_trials - 1)   # 0 at trial 1, 1 at trial burnin_trials
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

from typing import Dict, Any

def run_participant_number_screen(screen, clock, font) -> Dict[str, Any]:
    """
    Initial screen before instructions.
    Requires numeric participant number input before continuing.
    Clickable "Continue" button is disabled until a number is entered.

    Returns:
      {"quit": True}                          on quit/escape
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
                     key_black_name: str, key_white_name: str,
                     min_show_ms=250):

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
  
  
def get_block_instruction_payload(block_name: str) -> dict:
    """
    Returns {"title": str, "slides": list[str]}
    """
    if block_name in BLOCK_INSTRUCTIONS:
        payload = BLOCK_INSTRUCTIONS[block_name]

        if "slides" not in payload:
            payload["slides"] = [payload.get("body", "")]

        return payload

    return {"title": f"{block_name} BLOCK", "slides": ["Instructions"]}
  
  
def block_title(block_name: str) -> str:
    return get_block_instruction_payload(block_name)["title"]
  

def wait_for_continue_button(clock, button_rect, min_show_ms=250):
    """
    Wait until the user clicks the Continue button.
    Window close is ignored.
    Hard-quit (Option/Alt+Q) still works.
    """
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
                if enabled and button_rect.collidepoint(ev.pos):
                    return


def run_block_instructions(
    screen,
    font_title,
    font_body,
    clock,
    block_name: str,
    min_show_ms=250
):
    payload = get_block_instruction_payload(block_name)
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


def draw_trial_prompt_colored(screen, font_small, y_pos):
    # tokens = ["C", "=", "BLACK", "dominant", "|SPACER|", "N", "=", "WHITE", "dominant"]
    tokens = ["D", "=", "V-BLACK", " ", "|SPACER|", "J", "=", "V-WHITE", " "]
    space_w = font_small.size(" ")[0]
    spacer_w = space_w * 5

    token_widths = []
    for t in tokens:
        token_widths.append(spacer_w if t == "|SPACER|" else font_small.size(t)[0])

    total_w = 0
    for i, t in enumerate(tokens):
        total_w += token_widths[i]
        if i < len(tokens) - 1 and t != "|SPACER|" and tokens[i + 1] != "|SPACER|":
            total_w += space_w

    x = WIDTH // 2 - total_w // 2

    for i, t in enumerate(tokens):
        if t == "|SPACER|":
            x += spacer_w
            continue

        col = COLOR_TOKENS_UI.get(t, WHITE)
        img = font_small.render(t, True, col)
        screen.blit(img, (x, y_pos))
        x += img.get_width()

        if i < len(tokens) - 1 and tokens[i + 1] != "|SPACER|":
            x += space_w


def draw_trial_prompt_stacked(screen, font_small, y_pos, key_black_name: str, key_white_name: str):
    """
    Bottom prompt, always laid out as:

        (LEFT COLUMN)                 (RIGHT COLUMN)
        key D meaning label           key J meaning label
        Press D                       Press J

    But the meaning (V-BLACK vs V-WHITE) + text colour (black vs white)
    follows the participant-specific key mapping:
      - whichever key maps to BLACK is rendered in BLACK
      - whichever key maps to WHITE is rendered in WHITE
    """

    # Mapping from key-name -> category
    # (key_black_name is either "D" or "J"; key_white_name is the other)
    meaning_by_key = {
        key_black_name: "BLACK",
        key_white_name: "WHITE",
    }

    def label_and_color_for_key(key_name: str):
        cat = meaning_by_key.get(key_name, "WHITE")
        if cat == "BLACK":
            return "V-BLACK", BLACK
        else:
            return "V-WHITE", WHITE

    # Fixed spatial layout: D always left, J always right
    left_key = "D"
    right_key = "J"

    left_top, left_col = label_and_color_for_key(left_key)
    right_top, right_col = label_and_color_for_key(right_key)

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

    # Left column (C)
    screen.blit(lt_img, (start_x + (left_w - lt_img.get_width()) // 2, y_top))
    screen.blit(lb_img, (start_x + (left_w - lb_img.get_width()) // 2, y_bottom))

    # Right column (N)
    right_x = start_x + left_w + col_gap
    screen.blit(rt_img, (right_x + (right_w - rt_img.get_width()) // 2, y_top))
    screen.blit(rb_img, (right_x + (right_w - rb_img.get_width()) // 2, y_bottom))


def make_aid_recommendation(true_label, accuracy):
    if random.random() < accuracy:
        return true_label, True
    other = "WHITE" if true_label == "BLACK" else "BLACK"
    return other, False


def draw_aid_recommendation_top_center(
    screen,
    font_label,   # small font for "RECOMMENDATION:"
    font_main,    # large font for recommendation
    rec_label,
    show_value=True
):
    cx = WIDTH // 2
    y0 = PB_PAD + PB_H + S(22)
    line_gap = 4

    # Line 1: always visible
    img_label = font_label.render("AID JUDGES:", True, WHITE)
    screen.blit(img_label, (cx - img_label.get_width() // 2, y0))

    # Line 2: only if allowed
    if show_value:
        phrase = f"{rec_label}"
        col = COLOR_TOKENS_AID.get(rec_label, WHITE)
        img_main = font_main.render(phrase, True, col)
        screen.blit(
            img_main,
            (cx - img_main.get_width() // 2, y0 + img_label.get_height() + line_gap)
        )


def parse_cli_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--block",
        type=str,
        default=None,
        help="Run only a single block. Valid values: CALIBRATION, MANUAL, AUTOMATION1, AUTOMATION2",
    )
    args = parser.parse_args()

    if args.block is not None:
        args.block = args.block.upper()

    return args
  

def select_single_block(block_name: str, blocks_template):
    """
    Return a single-block list matching block_name.
    Raises a clear error if the block is not available in BLOCKS.
    """
    matches = [dict(b) for b in blocks_template if b["name"] == block_name]

    if not matches:
        available = [b["name"] for b in blocks_template]
        raise ValueError(
            f"Unknown block '{block_name}'. Available blocks in this script: {available}"
        )

    return matches
  
  
# -----------------------------
# Main experiment
# -----------------------------
def main():
    args = parse_cli_args()
    pygame.init()
    pygame.display.set_caption("Virus Detection Task")

    flags = 0
    if FULLSCREEN:
        flags |= pygame.FULLSCREEN

    # Option A (recommended): let pygame pick the current display mode in fullscreen
    if FULLSCREEN and USE_DESKTOP_RES:
        screen = pygame.display.set_mode((0, 0), flags)
    else:
        # fallback: windowed / fixed
        screen = pygame.display.set_mode((BASE_W, BASE_H))

    # Hide mouse cursor
    pygame.event.set_grab(True)
    pygame.mouse.set_visible(True)  # Set 'False' to hide cursor
    
    # --- Auto-detect actual size from the created display surface ---
    global WIDTH, HEIGHT
    WIDTH, HEIGHT = screen.get_size()
    
    global UI_SCALE
    UI_SCALE = compute_ui_scale(WIDTH, HEIGHT)
    print("[UI_SCALE]", UI_SCALE, "for", WIDTH, "x", HEIGHT)
    
    global DISH_RADIUS, DOT_RADIUS, VEL_WANDER_SD, VEL_MAX, VEL_INIT_RANGE
    DISH_RADIUS    = S(DISH_RADIUS_BASE)
    DOT_RADIUS     = max(1, S(DOT_RADIUS_BASE))
    VEL_WANDER_SD  = SF(VEL_WANDER_SD_BASE)  # px/frame scaled
    VEL_MAX        = SF(VEL_MAX_BASE)        # px/frame scaled
    VEL_INIT_RANGE = SF(VEL_INIT_RANGE_BASE)
    
    global FIX_SIZE, FIX_THICKNESS, PB_W, PB_H, PB_PAD
    FIX_SIZE      = S(FIX_SIZE_BASE)
    FIX_THICKNESS = max(1, S(FIX_THICKNESS_BASE))
    PB_W  = S(PB_W_BASE)
    PB_H  = S(PB_H_BASE)
    PB_PAD = S(PB_PAD_BASE)
    
    # Fonts
    font_title = load_font(FONT_LIGHT, max(12, S(FONT_TITLE_BASE)))
    font_body  = load_font(FONT_LIGHT, max(10, S(FONT_BODY_BASE)))
    font_body_bold  = load_font(FONT_BOLD, max(10, S(FONT_BODY_BASE)))
    font_small = load_font(FONT_LIGHT, max(9,  S(FONT_SMALL_BASE)))
    
    # Automated aid recommendation fonts
    font_aid_label = load_font(FONT_LIGHT, max(9,  S(FONT_AID_LABEL_BASE)))
    font_aid       = load_font(FONT_BOLD, max(10, S(FONT_AID_BASE)))

    # Alpha dot layer (draw dots here, then blit to screen)
    dot_layer = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)

    clock = pygame.time.Clock()

    # ---- Participant ID screen (BEFORE instructions) ----
    res = run_participant_number_screen(screen, clock, font_body)  # or whichever font you use for UI screens
    if res.get("quit", False):
        quit_clean()

    participant_id = res["participant"]

    # Preload most recent CALIBRATION delta summary for this participant
    # so single-block MANUAL/AUTOMATION runs can inherit it from a prior run.
    prev_calib_delta_mean, prev_calib_delta_sd, prev_calib_delta_path = (
        get_latest_calibration_delta_for_participant(participant_id, output_dir="output")
    )

    if prev_calib_delta_path is not None:
        print(
            f"[PREV CALIB] Loaded participant {participant_id} calibration delta "
            f"from: {prev_calib_delta_path} "
            f"(mean={prev_calib_delta_mean}, sd={prev_calib_delta_sd})"
        )
    else:
        print(f"[PREV CALIB] No prior CALIBRATION delta file found for participant {participant_id}")
        
    # ---- key counterbalancing (every 2 participants) ----
    km = key_mapping_for_participant(participant_id)
    KEY_BLACK = km["key_black"]
    KEY_WHITE = km["key_white"]
    KEY_BLACK_NAME = km["key_black_name"]
    KEY_WHITE_NAME = km["key_white_name"]

    print("[KEYMAP]", participant_id, "->",
          f"{KEY_BLACK_NAME}=BLACK, {KEY_WHITE_NAME}=WHITE",
          "(reversed)" if km["flip"] else "(standard)")

    center = (WIDTH // 2, HEIGHT // 2 + S(20))

    # run_instructions(screen, font_title, font_body, font_body_bold, clock,
    #                       KEY_BLACK_NAME, KEY_WHITE_NAME, min_show_ms=250)
    
    # ---------------------------------------------------------
    # Run all blocks
    # ---------------------------------------------------------
    
    # Cache calibration delta distribution for later blocks
    calib_delta_mean = None
    calib_delta_sd   = None
    calib_delta_path = None

    all_results = []
    all_postblock_slider_rows = []
    all_questionnaire_rows = []

    global_trial_index = 0

    # ---------------------------------------------------------
    # Decide which blocks to run
    # ---------------------------------------------------------
    if args.block is not None:
        blocks_to_run = select_single_block(args.block, BLOCKS)
        print("[SINGLE BLOCK MODE]", participant_id, "->", [b["name"] for b in blocks_to_run])
    else:
        blocks_to_run = build_blocks_for_participant(participant_id, BLOCKS)
        print("[BLOCK ORDER]", participant_id, "->", [b["name"] for b in blocks_to_run])

    for b_idx, blk in enumerate(blocks_to_run, start=1):
        block_name   = blk["name"]
        N_TRIALS     = blk["N_TRIALS"]
        AUTOMATION_ON = blk["AUTOMATION_ON"]
        AID_ACCURACY  = blk["AID_ACCURACY"]
        STAIRCASE_ON  = blk["STAIRCASE_ON"]
        TARGET_ACC    = blk["TARGET_ACC"]
        FIXED_DELTA_ON = blk["FIXED_DELTA_ON"]
        FIXED_DELTA_VALUE = blk["FIXED_DELTA_VALUE"]
        TRIAL_FEEDBACK_ON   = blk["TRIAL_FEEDBACK_ON"]
        SHOW_AID_MASKED = bool(blk.get("SHOW_AID_MASKED", False))

        # Show the general task instructions before every block
        run_instructions(
            screen,
            font_title,
            font_body,
            font_body_bold,
            clock,
            KEY_BLACK_NAME,
            KEY_WHITE_NAME,
            min_show_ms=250,
        )

        # IMPORTANT: step sizes depend on TARGET_ACC
        DELTA_STEP_UP = DELTA_STEP_DOWN * (TARGET_ACC / (1.0 - TARGET_ACC))
        
        run_block_instructions(
            screen=screen,
            font_title=font_title,
            font_body=font_body,
            clock=clock,
            block_name=block_name,
            min_show_ms=250
        )

        # Block start screen
        screen.fill(BG_INSTRUCTIONS)
        draw_center_lines(
            screen,
            [block_title(block_name), "Press any key to begin"],
            font_body,
            WHITE,
            rect=(0, 0, WIDTH, HEIGHT),
            line_spacing=S(14),
            vert_center=True,
        )
        pygame.display.flip()

        wait_for_keypress(clock, min_show_ms=250)

        # Inter-trial interval before block starts
        fixation_cross_screen(screen, clock, FIXATION_DURATION_MS)

        # -----------------------------
        # Decide difficulty source for THIS block
        # -----------------------------
        if FIXED_DELTA_ON:
            difficulty_mode = "fixed_delta"
        elif (not AUTOMATION_ON) and STAIRCASE_ON:
            difficulty_mode = "staircase"
        else:
            difficulty_mode = "fixed_props"

        # If fixed-delta is on, pull mean/sd from most recent delta_*.csv (likely from previous manual block)
        fixed_delta_mean = None
        fixed_delta_sd   = None

        if difficulty_mode == "fixed_delta":
            # Preference order:
            # 1) CALIBRATION completed earlier in THIS run
            # 2) most recent prior CALIBRATION for this participant (timestamp does not need to match)
            # 3) block fallback value in BLOCKS

            if calib_delta_mean is not None and calib_delta_sd is not None:
                fixed_delta_mean, fixed_delta_sd = calib_delta_mean, calib_delta_sd
                print(
                    f"[{block_name}] Using CALIBRATION delta from current run "
                    f"(mean={fixed_delta_mean}, sd={fixed_delta_sd})"
                )

            elif prev_calib_delta_mean is not None and prev_calib_delta_sd is not None:
                fixed_delta_mean, fixed_delta_sd = prev_calib_delta_mean, prev_calib_delta_sd
                print(
                    f"[{block_name}] Using most recent prior CALIBRATION delta for participant "
                    f"{participant_id} from {prev_calib_delta_path} "
                    f"(mean={fixed_delta_mean}, sd={fixed_delta_sd})"
                )

            else:
                fixed_delta_mean = FIXED_DELTA_VALUE
                fixed_delta_sd = 0.0
                print(
                    f"[{block_name}] No CALIBRATION delta found for participant {participant_id}; "
                    f"falling back to FIXED_DELTA_VALUE={fixed_delta_mean}, sd={fixed_delta_sd}"
                )


        # Pre-generate proportions only for fixed-props mode
        if difficulty_mode == "fixed_props":
            vblack_props = [random.choice(VBLACK_PROPORTION_LEVELS) for _ in range(N_TRIALS)]
            random.shuffle(vblack_props)
        else:
            vblack_props = None

        # Staircase state only meaningful in staircase mode
        delta_mean = DELTA_INIT
        deltas_realised = []

        block_results = []

        for t in range(N_TRIALS):
            trials_left = N_TRIALS - t
            global_trial_index += 1

            # --- pick vblack_prop depending on difficulty_mode ---
            if difficulty_mode == "fixed_props":
                vblack_prop = vblack_props[t]
                delta_realised_this_trial = None

            elif difficulty_mode == "fixed_delta":
                delta_realised_this_trial = sample_delta_from_mean(fixed_delta_mean, fixed_delta_sd)
                vblack_prop = pick_vblack_prop_from_delta(delta_realised_this_trial)

            else:  # staircase
                delta_realised_this_trial = sample_delta_from_mean(delta_mean, DELTA_SD)
                deltas_realised.append(delta_realised_this_trial)
                vblack_prop = pick_vblack_prop_from_delta(delta_realised_this_trial)

            dots, n_vblack, n_vwhite = make_trial_dots(N_DOTS, vblack_prop, center, DISH_RADIUS)
            true_label = "BLACK" if n_vblack > n_vwhite else "WHITE"

            if AUTOMATION_ON:
                aid_label, aid_correct = make_aid_recommendation(true_label, accuracy=AID_ACCURACY)
            else:
                aid_label = None
                aid_correct = None

            responded = False
            resp_label = None
            rt_ms = None

            stim_onset_perf = None
            resp_perf = None

            aid_onset_perf = None
            aid_visible = False
            pre_aid_onset_perf = None

            if not AUTOMATION_ON:
                aid_visible = False

            # (A) advanced aid (<0)
            if AUTOMATION_ON and AID_ONSET_MS < 0:
                pre_ms = abs(AID_ONSET_MS)
                pre_start_perf = time.perf_counter()

                while (time.perf_counter() - pre_start_perf) * 1000.0 < pre_ms:
                    clock.tick(FPS)
                    for ev in pygame.event.get():
                        if ev.type == pygame.QUIT:
                            pass
                        if ev.type == pygame.KEYDOWN:
                            if is_hard_quit_event(ev):
                                quit_clean()

                    screen.fill(BG)
                    draw_progress_bar(screen, trials_left=trials_left, total_trials=N_TRIALS)

                    pb_text = f"Samples left: {trials_left}"
                    pb_img = font_small.render(pb_text, True, WHITE)
                    screen.blit(pb_img, (WIDTH - PB_PAD - PB_W, PB_PAD + PB_H + 6))

                    draw_aid_recommendation_top_center(
                        screen, font_aid_label, font_aid, aid_label, show_value=True
                    )

                    if pre_aid_onset_perf is None:
                        pre_aid_onset_perf = time.perf_counter()

                    pygame.display.flip()

                aid_visible = True

            # (B) main trial loop
            trial_start_ticks = pygame.time.get_ticks()

            while not responded:
                clock.tick(FPS)
                now = pygame.time.get_ticks()

                for ev in pygame.event.get():
                    if ev.type == pygame.QUIT:
                        pass  # ignore window close button
                    if ev.type == pygame.KEYDOWN:
                        if is_hard_quit_event(ev):
                            quit_clean()
                        if ev.key == KEY_BLACK:
                            responded = True
                            resp_label = "BLACK"
                            resp_perf = time.perf_counter()
                        elif ev.key == KEY_WHITE:
                            responded = True
                            resp_label = "WHITE"
                            resp_perf = time.perf_counter()


                if TRIAL_DEADLINE_MS is not None and (now - trial_start_ticks) >= TRIAL_DEADLINE_MS:
                    responded = True
                    resp_label = "TIMEOUT"
                    rt_ms = None

                update_dots(dots, center, DISH_RADIUS)

                screen.fill(BG)
                # Countdown timer (top-left)
                if TRIAL_DEADLINE_MS is not None:
                    ms_left = TRIAL_DEADLINE_MS - (now - trial_start_ticks)
                    draw_countdown_timer(
                        surface=screen,
                        font=font_body,
                        ms_left=ms_left,
                        x=PB_PAD,
                        y=PB_PAD,
                        color=WHITE
                    )
                    
                # Progress bar (top-right)
                draw_progress_bar(screen, trials_left=trials_left, total_trials=N_TRIALS)

                pb_text = f"Samples left: {trials_left}"
                pb_img = font_small.render(pb_text, True, WHITE)
                screen.blit(pb_img, (WIDTH - PB_PAD - PB_W, PB_PAD + PB_H + 6))

                draw_petri_dish(screen, center, DISH_RADIUS)

                dot_layer.fill((0, 0, 0, 0))
                for d in dots:
                    x = int(d["x"])
                    y = int(d["y"])
                    r, g, b = d["col"]
                    pygame.draw.circle(dot_layer, (r, g, b, DOT_ALPHA), (x, y), DOT_RADIUS)
                screen.blit(dot_layer, (0, 0))

                draw_trial_prompt_stacked(screen, font_small, HEIGHT - S(80), KEY_BLACK_NAME, KEY_WHITE_NAME)

                if stim_onset_perf is None:
                    stim_onset_perf = time.perf_counter()
                    if pre_aid_onset_perf is not None:
                        aid_onset_perf = pre_aid_onset_perf

                if AUTOMATION_ON and AID_ONSET_MS >= 0 and stim_onset_perf is not None:
                    elapsed_ms = (time.perf_counter() - stim_onset_perf) * 1000.0
                    if (not aid_visible) and (elapsed_ms >= AID_ONSET_MS):
                        aid_visible = True
                        aid_onset_perf = time.perf_counter()

                if AUTOMATION_ON:
                    draw_aid_recommendation_top_center(
                        screen, font_aid_label, font_aid, aid_label, show_value=aid_visible
                    )
                    
                # Show masked "aid" in MANUAL (uninformative)
                elif SHOW_AID_MASKED:
                    draw_aid_recommendation_top_center(
                        screen,
                        font_aid_label,
                        font_aid,
                        rec_label="#####",
                        show_value=True,   # always show the mask
                    )

                pygame.display.flip()

            # RT from dot onset
            if stim_onset_perf is not None and resp_perf is not None:
                rt_ms = (resp_perf - stim_onset_perf) * 1000.0
            else:
                rt_ms = None

            if stim_onset_perf is not None and aid_onset_perf is not None:
                aid_onset_ms_rel = (aid_onset_perf - stim_onset_perf) * 1000.0
            else:
                aid_onset_ms_rel = None

            correct = (resp_label == true_label)

            # Feedback (per-block)
            fb_msg = None
            if TRIAL_FEEDBACK_ON:
                if resp_label == "TIMEOUT":
                    fb_msg = "TOO SLOW"
                    fb_col = FEEDBACK_SLOW_COLOR
                elif correct:
                    fb_msg = "CORRECT"
                    fb_col = FEEDBACK_CORRECT_COLOR
                else:
                    fb_msg = "INCORRECT"
                    fb_col = FEEDBACK_ERROR_COLOR

                show_feedback_screen(
                    screen=screen,
                    clock=clock,
                    font=font_title,
                    msg=fb_msg,
                    bg_color=BG,
                    text_color=fb_col,
                    prompt_text="Press any key to continue",
                    prompt_font=font_body,   # nicer for the small prompt
                    prompt_color=WHITE,
                    min_show_ms=250
                )

            # Staircase update (only if staircase this block)
            if difficulty_mode == "staircase":
                trial_in_block = t + 1  # 1-based

                step_down_now = burnin_step_down(
                    trial_in_block_1based=trial_in_block,
                    step_start=DELTA_STEP_DOWN,
                    step_min=DELTA_STEP_DOWN_MIN,
                    burnin_trials=BURNIN_TRIALS
                )
                step_up_now = step_down_now * (TARGET_ACC / (1.0 - TARGET_ACC))

                is_correct = (resp_label == true_label)
                if is_correct:
                    delta_mean = max(DELTA_MIN, delta_mean - step_down_now)
                else:
                    delta_mean = min(DELTA_MAX, delta_mean + step_up_now)

            row = {
                "participant_id": participant_id,
                "run_timestamp": run_ts,
                
                "key_black": KEY_BLACK_NAME,   # "D" or "J"
                "key_white": KEY_WHITE_NAME,   # "J" or "D"
                "keymap_flip": km["flip"],     # True/False
                
                "block": block_name,
                "block_idx": b_idx,
                "trial": t + 1,
                "global_trial": global_trial_index,

                "difficulty_mode": difficulty_mode,
                "delta_fixed_mean": fixed_delta_mean if difficulty_mode == "fixed_delta" else None,
                "delta_fixed_sd": fixed_delta_sd if difficulty_mode == "fixed_delta" else None,
                "delta_stair_realised": delta_realised_this_trial if difficulty_mode == "staircase" else None,
                "delta_stair_mean": delta_mean if difficulty_mode == "staircase" else None,
                
                "delta_step_down_used": step_down_now if difficulty_mode == "staircase" else None,
                "delta_step_up_used": step_up_now if difficulty_mode == "staircase" else None,

                "vblack_prop": vblack_prop,
                "n_vblack": n_vblack,
                "n_vwhite": n_vwhite,

                "auto_on": 1 if AUTOMATION_ON else 0,
                "aid_accuracy_setting": AID_ACCURACY if AUTOMATION_ON else None,

                "true_label": true_label,
                "aid_label": aid_label,
                "aid_correct": aid_correct,
                "aid_onset_ms": AID_ONSET_MS,
                "aid_onset_ms_rel": aid_onset_ms_rel,

                "resp_label": resp_label,
                "correct": correct if resp_label in ("BLACK", "WHITE") else None,
                "feedback": fb_msg if TRIAL_FEEDBACK_ON else None,
                "rt_ms": rt_ms,
            }

            block_results.append(row)
            all_results.append(row)
            
            # Inter-trial participant-controlled continue screen
            is_last_trial_in_block = (t == N_TRIALS - 1)

            if not is_last_trial_in_block:
                # If no feedback screen was shown, still gate progression between trials
                if not TRIAL_FEEDBACK_ON:
                    press_any_key_screen(
                        screen=screen,
                        clock=clock,
                        font=font_body,
                        msg="Press any key to continue",
                        bg_color=BG_INSTRUCTIONS,
                        text_color=WHITE,
                    )

                # Fixation cross before the next trial only
                fixation_cross_screen(screen, clock, FIXATION_DURATION_MS)

        # -----------------------------
        # Save per-block results CSV
        # -----------------------------
        output_dir = "output"
        os.makedirs(output_dir, exist_ok=True)

        block_csv_path = os.path.join(
            output_dir,
            f"results_p{participant_id:03d}_{run_ts}_b{b_idx:02d}_{block_name}.csv"
        )

        with open(block_csv_path, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=block_results[0].keys())
            writer.writeheader()
            writer.writerows(block_results)

        print(f"[{block_name}] Results saved to: {block_csv_path}")

        # -----------------------------
        # Save per-block staircase delta summary (if staircase)
        # -----------------------------
        if difficulty_mode == "staircase":
            delta_out_path = os.path.join(
                output_dir,
                f"delta_p{participant_id:03d}_{run_ts}_b{b_idx:02d}_{block_name}.csv"
            )

            # --- EXCLUDE burn-in trials from summary stats ---
            burn = int(BURNIN_TRIALS) if BURNIN_TRIALS is not None else 0
            deltas_summary = deltas_realised[burn:] if burn > 0 else deltas_realised

            if deltas_summary:
                m = sum(deltas_summary) / len(deltas_summary)
                if len(deltas_summary) > 1:
                    var = sum((x - m) ** 2 for x in deltas_summary) / (len(deltas_summary) - 1)
                    sd = math.sqrt(var)
                else:
                    sd = 0.0
            else:
                m, sd = None, None

            row = {
                "participant_id": participant_id,
                "run_timestamp": run_ts,
                "block": block_name,
                "block_idx": b_idx,

                "n_trials_total": len(deltas_realised),
                "burnin_trials_excluded": burn,
                "n_trials_summarised": len(deltas_summary),

                "delta_init": DELTA_INIT,
                "delta_sd_setting": DELTA_SD,
                "delta_step_down": DELTA_STEP_DOWN,
                "delta_step_up": DELTA_STEP_UP,
                "delta_min": DELTA_MIN,
                "delta_max": DELTA_MAX,

                # summary stats (post burn-in only)
                "delta_block_mean": m,
                "delta_block_sd": sd,

                "delta_mean_final": delta_mean,
            }

            with open(delta_out_path, "w", newline="") as f:
                writer = csv.DictWriter(f, fieldnames=row.keys())
                writer.writeheader()
                writer.writerow(row)

            print(f"[{block_name}] Staircase delta summary saved to: {delta_out_path}")

            # If this was the CALIBRATION block, cache its delta distribution for later blocks
            if block_name == "CALIBRATION":
                calib_delta_mean = m
                calib_delta_sd   = sd
                calib_delta_path = delta_out_path

        # End-of-block feedback screen
        # Treat TIMEOUT as incorrect (a miss)
        n_trials_total = len(block_results)
        n_correct = sum(1 for r in block_results if r["resp_label"] in ("BLACK", "WHITE") and r["correct"] is True)
        acc = (n_correct / n_trials_total) if n_trials_total > 0 else 0.0

        aid_vals = [r["aid_correct"] for r in block_results if r.get("aid_correct") is not None]
        aid_acc = sum(1 for v in aid_vals if v) / len(aid_vals) if aid_vals else 0.0

        end_lines = [
            f"{block_title(block_name)} COMPLETE",
            # f"Samples evaluated: {len(block_results)}",
            # f"Your accuracy: {acc*100:.1f}%",
        ]
        # if AUTOMATION_ON:
            # end_lines.append(f"Automation accuracy: {aid_acc*100:.1f}%")
        end_lines.append("Press any key to continue")

        screen.fill(BG_INSTRUCTIONS)
        draw_center_lines(
            screen, end_lines, font_body, WHITE,
            rect=(0, 0, WIDTH, HEIGHT),
            line_spacing=S(14),
            vert_center=True,
        )
        pygame.display.flip()

        wait_for_keypress(clock, min_show_ms=250)

        # -----------------------------
        # Post-block slider questions
        # -----------------------------
        if ENABLE_POSTBLOCK_SLIDERS and block_name in ("CALIBRATION", "MANUAL", "AUTOMATION1", "AUTOMATION2"):
            slider_rows = run_postblock_slider_questions(
                screen=screen,
                clock=clock,
                font_title=font_title,
                font_body=font_body,
                participant_id=participant_id,
                run_ts=run_ts,
                block_name=block_name,
                block_idx=b_idx,
                automation_on=AUTOMATION_ON,
                output_dir=output_dir,
            )
            if isinstance(slider_rows, dict) and slider_rows.get("quit"):
                quit_clean()

            # keep for ALL file
            if slider_rows:
                # create the list once near your other accumulators
                all_postblock_slider_rows.extend(slider_rows)
                
        # -----------------------------
        # Post-block questionnaire (AUTOMATION blocks only)
        # Shown after end-of-block feedback screen
        # -----------------------------
        if block_name in ("AUTOMATION1", "AUTOMATION2") and ENABLE_POSTBLOCK_QUESTIONS:
            
            # Questionnaire intro screen
            run_questionnaire_intro_screen(
                screen=screen,
                clock=clock,
                font_title=font_title,
                font_body=font_body,
                min_show_ms=250,
            )
            
            q_base = f"p{participant_id:03d}_b{b_idx:02d}_{block_name}"
            
            q_resps = run_postblock_questionnaire(
                screen,
                clock,
                font_body,
                base_name=q_base,
                participant_id=participant_id,
                block_name=block_name,
                block_idx=b_idx,
            )

            if isinstance(q_resps, dict) and q_resps.get("quit"):
                quit_clean()

            # Save per-block questionnaire CSV
            if q_resps:
                q_path = os.path.join(
                    output_dir,
                    f"results_p{participant_id:03d}_{run_ts}_b{b_idx:02d}_{block_name}_POSTBLOCK.csv"
                )
                with open(q_path, "w", newline="") as f:
                    writer = csv.DictWriter(f, fieldnames=q_resps[0].keys())
                    writer.writeheader()
                    writer.writerows(q_resps)

                print(f"[{block_name}] Questionnaire saved to: {q_path}")

                all_questionnaire_rows.extend(q_resps)

    # ---------------------------------------------------------
    # Save combined post-block slider CSV (all blocks that had it)
    # ---------------------------------------------------------
    if all_postblock_slider_rows:
        sliders_all_path = os.path.join(
            output_dir,
            f"results_p{participant_id:03d}_{run_ts}_b00_POSTBLOCK_SLIDERS_ALL.csv"
        )
        with open(sliders_all_path, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=all_postblock_slider_rows[0].keys())
            writer.writeheader()
            writer.writerows(all_postblock_slider_rows)

        print(f"[ALL] Post-block sliders saved to: {sliders_all_path}")
        
    # ---------------------------------------------------------
    # Save combined results CSV (all blocks)
    # ---------------------------------------------------------
    output_dir = "output"
    os.makedirs(output_dir, exist_ok=True)

    all_csv_path = os.path.join(output_dir, f"results_p{participant_id:03d}_{run_ts}_b00_ALL.csv")

    with open(all_csv_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=all_results[0].keys())
        writer.writeheader()
        writer.writerows(all_results)

    print(f"[ALL] Results saved to: {all_csv_path}")

    # ---------------------------------------------------------
    # Save combined questionnaire CSV (all blocks that had it)
    # ---------------------------------------------------------
    if all_questionnaire_rows:
        q_all_path = os.path.join(output_dir, f"results_p{participant_id:03d}_{run_ts}_b00_POSTBLOCK_ALL.csv")
        with open(q_all_path, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=all_questionnaire_rows[0].keys())
            writer.writeheader()
            writer.writerows(all_questionnaire_rows)

        print(f"[ALL] Questionnaire saved to: {q_all_path}")

    # ---------------------------------------------------------
    # Compute overall performance score (selected blocks only)
    # ---------------------------------------------------------
    score_blocks = {"MANUAL", "AUTOMATION1", "AUTOMATION2"}

    scored_trials = [
        r for r in all_results
        if r["block"] in score_blocks
    ]

    if scored_trials:
        n_correct = sum(1 for r in scored_trials if r["correct"] is True)
        perf_score = (n_correct / len(scored_trials)) * 100.0
    else:
        perf_score = 0.0
    
    # Final end screen (ESC allowed ONLY here)
    screen.fill(BG_INSTRUCTIONS)
    draw_center_lines(
        screen,
        [
            "EXPERIMENT COMPLETE",
            f"Performance score: {perf_score:.1f}% correct",
            "Please alert the experimenter now"
        ],
        font_body,
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
