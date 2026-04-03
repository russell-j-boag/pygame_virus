"""
Standalone instruction slides for the Virus Detection Task.

- BACK button in bottom-left
- NEXT button in bottom-right
- Option/Alt + Q hard quits
"""

import sys
import random
import math
import os
import pygame


# -----------------------------
# Config
# -----------------------------
FPS = 120
FULLSCREEN = True
USE_DESKTOP_RES = True

BASE_W, BASE_H = 1280, 720

# Backgrounds and common colours
BG = (128, 128, 128)              # main task background
BG_INSTRUCTIONS = (40, 40, 40)
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
LIGHT_GREY = (170, 170, 170)
VBLACK = BLACK
VWHITE = WHITE
DISH_FILL = (128, 128, 128)   # neutral mid-grey (halfway between black/white)
DISH_RING = BLACK     # outer ring
DISH_EDGE = BLACK     # thin edge

# Base geometry
DISH_RADIUS_BASE = 260
DOT_RADIUS_BASE  = 3
N_DOTS = 3600
DOT_ALPHA = 255 
EXAMPLE_DOTS_CACHE = None

# Progress bar
PB_W_BASE, PB_H_BASE = 224, 16
PB_PAD_BASE          = 18

FONT_TITLE_BASE = 36
FONT_BODY_BASE = 24
FONT_BUTTON_BASE = 22
FONT_SMALL_BASE = 18
FONT_AID_LABEL_BASE = 20
FONT_AID_BASE       = 32

FONT_LIGHT = "Roboto-Light.ttf"
FONT_BOLD = "Roboto-Bold.ttf"
FONT_DIR_CANDIDATES = [
    os.path.join(os.sep, "python", "fonts"),  # /python/fonts
    os.path.join("python", "fonts"),          # python/fonts
    os.path.join(".", "python", "fonts"),     # ./python/fonts
    os.path.join("fonts"),                    # fonts
]

# Floaty motion params (BASE units)
VEL_WANDER_SD_BASE  = 0.008
VEL_DAMPING         = 0.99
VEL_MAX_BASE        = 0.16
VEL_INIT_RANGE_BASE = 0.5
COLLISION_DAMPING   = 0.9

COLOR_TOKENS_AID = {"BLACK": VBLACK, "WHITE": VWHITE}

# -----------------------------
# Placeholder slides
# -----------------------------
SLIDES = [
    {
        "kind": "text",
        "title": "VIRUS DETECTION TASK", 
        "body": (
            "We have identified two dangerous viruses. "
            "Unfortunately, the two strains are difficult to tell apart. Both are speckled BLACK and WHITE. "
            "The only difference visually is that one strain tends to have a little more BLACK, and the other tends to have a little more WHITE. "
            "For simplicity, we will call them V-BLACK and V-WHITE. "
            "You'll be shown a similar number of V-BLACK and V-WHITE samples. \n"
            "Your job is to evaluate the following samples to determine which virus is present.\n\n"
            "The following screens will help you get familiar with the task"
        ),
    },
    {
        "kind": "text",
        "title": "TASK DISPLAY",
        "body": "",
    },
    {
        "kind": "example_task_display",
        "callout": "stimulus",
    },
    {
        "kind": "example_task_display",
        "callout": "keys",
    },
    {
        "kind": "example_task_display",
        "callout": "timer",
    },
    {
        "kind": "example_task_display",
        "callout": "aid",
    },
    {
        "kind": "example_task_display",
        "callout": "progress",
    },
    {
        "kind": "text",
        "title": "AUTOMATED DECISION AID",
        "body": "",
    },
    {
        "kind": "automation_example",
        "aid_label": "BLACK",
        "callout": "intro",
    },
    {
        "kind": "automation_example",
        "aid_label": "BLACK",
        "callout": "classification",
    },
    {
        "kind": "automation_example",
        "aid_label": "BLACK",
        "callout": "black_example",
    },
    {
        "kind": "automation_example",
        "aid_label": "WHITE",
        "callout": "white_example",
    },
    {
        "kind": "text",
        "title": "TRANSPARENCY LEVELS",
        "body": (
            "Across automation blocks, the aid may present different levels of explanatory transparency. "
            "Sometimes it will show only a recommendation. In other blocks it may also show a brief reason, "
            "or more detailed information about the evidence and decision rule."
        ),
    },
    {
        "kind": "automation_example",
        "aid_label": "BLACK",
        "transparency_level": "none",
        "callout": "transparency_none",
    },
    {
        "kind": "automation_example",
        "aid_label": "BLACK",
        "transparency_level": "low",
        "callout": "transparency_low",
    },
    {
        "kind": "automation_example",
        "aid_label": "BLACK",
        "transparency_level": "high",
        "callout": "transparency_high",
    },
    {
        "kind": "text",
        "title": "PERFORMANCE", 
        "body": (
            "We will keep an ongoing tally of your performance. "
            "At the end of the experiment you will receive a point-based bonus, up to $25, based on your performance score.\n\n"
            "You will have up to 10 seconds to respond per trial. "
            "Incorrect responses and responses not made before the deadline will reduce your performance score, so try "
            "to respond as quickly and accurately as possible.\n\n"
            "You may take short breaks at any time between trials\n\n"
        ),
    },
    {
        "kind": "text",
        "title": "INSTRUCTIONS COMPLETE", 
        "body": (
            "You will now begin your first block of trials\n\n"
            "Please let the experimenter know if you have any questions"
        ),
    },
]


# -----------------------------
# UI scaling
# -----------------------------
def compute_ui_scale(actual_w, actual_h):
    # uniform scale preserves aspect / proportions
    return min(actual_w / BASE_W, actual_h / BASE_H)

def S(x):  # scale a length (px)
    return int(round(x * UI_SCALE))

def SF(x):  # scale a float (useful for speed, etc.)
    return float(x * UI_SCALE)

def SP(x):  # scale padding/spacing; keeps ints
    return int(round(x * UI_SCALE))

# -----------------------------
# Font helpers
# -----------------------------
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


# -----------------------------
# Rich text / wrapping helpers
# -----------------------------
def split_token_word_punct(token):
    i = len(token)
    while i > 0 and token[i - 1] in ".,;:!?)]}\"'":
        i -= 1
    return token[:i], token[i:]


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
        items,
        font,
        line_spacing=line_spacing,
        blank_spacing=blank_spacing,
    )


def draw_wrapped_block_centered(
    surface,
    text,
    font,
    color,
    rect,
    y_start,
    line_spacing=8,
    blank_spacing=22,
):
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


def draw_wrapped_block_left(
    surface,
    text,
    font,
    color,
    rect,
    y_start,
    line_spacing=8,
    blank_spacing=22,
):
    x, y, w, h = rect
    items = layout_rich_text_blocks(text, font, max_width=w)

    space_w = font.size(" ")[0]
    yy = y_start

    for it in items:
        if it is None:
            yy += blank_spacing
            continue

        xx = x

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
  
  
# -----------------------------
# General helpers
# -----------------------------
def is_hard_quit_event(event) -> bool:
    if event.type != pygame.KEYDOWN:
        return False

    if event.key != pygame.K_q:
        return False

    mods = event.mod
    return bool(mods & (pygame.KMOD_ALT | pygame.KMOD_LALT | pygame.KMOD_RALT))


def quit_clean():
    pygame.quit()
    sys.exit()


def draw_text(screen, font, text, color, center, antialias=True):
    surf = font.render(text, antialias, color)
    rect = surf.get_rect(center=center)
    screen.blit(surf, rect)


def draw_button(
    surface,
    rect,
    label,
    font,
    enabled=True,
    fill_enabled=(50, 50, 50),
    fill_disabled=(35, 35, 35),
    border_enabled=WHITE,
    border_disabled=(110, 110, 110),
    text_enabled=WHITE,
    text_disabled=(140, 140, 140),
):
    radius = max(1, S(10))

    if enabled:
        pygame.draw.rect(surface, fill_enabled, rect, 0, border_radius=radius)
        pygame.draw.rect(surface, border_enabled, rect, max(1, S(3)), border_radius=radius)
        col = text_enabled
    else:
        pygame.draw.rect(surface, fill_disabled, rect, 0, border_radius=radius)
        pygame.draw.rect(surface, border_disabled, rect, max(1, S(2)), border_radius=radius)
        col = text_disabled

    img = font.render(label, True, col)
    surface.blit(
        img,
        (rect.centerx - img.get_width() // 2, rect.centery - img.get_height() // 2),
    )


def measure_callout_box(title, body, font_title, font_body):
    pad_x = S(16)
    pad_y = S(14)
    line_gap = S(8)
    line_spacing = S(6)
    blank_spacing = S(14)

    # wrap body to a reasonable max width
    max_body_w = S(280)

    body_items = layout_rich_text_blocks(body, font_body, max_width=max_body_w)
    body_h = measure_rich_block_height(
        body_items,
        font_body,
        line_spacing=line_spacing,
        blank_spacing=blank_spacing,
    )

    body_w = 0
    space_w = font_body.size(" ")[0]
    for it in body_items:
        if it is None:
            continue
        line_w = sum(font_body.size(tok)[0] for tok in it) + space_w * (len(it) - 1)
        body_w = max(body_w, line_w)

    title_w = font_title.size(title)[0]
    title_h = font_title.get_height()

    content_w = max(title_w, body_w)
    box_w = content_w + 2 * pad_x
    box_h = pad_y + title_h + line_gap + body_h + pad_y

    return box_w, box_h
  
  
def draw_callout_box(surface, title, body, rect, font_title, font_body):
    """
    Simple dark instruction-style overlay callout box.
    rect = pygame.Rect(...)
    """
    box_fill = (30, 30, 30)
    box_border = WHITE
    pad_x = S(16)
    pad_y = S(14)
    line_gap = S(8)

    pygame.draw.rect(surface, box_fill, rect, border_radius=max(1, S(10)))
    pygame.draw.rect(surface, box_border, rect, max(1, S(2)), border_radius=max(1, S(10)))

    tx = rect.x + pad_x
    ty = rect.y + pad_y
    tw = rect.width - 2 * pad_x

    title_img = font_title.render(title, True, WHITE)
    surface.blit(title_img, (tx, ty))

    body_y = ty + title_img.get_height() + line_gap
    draw_wrapped_block_left(
        surface,
        body,
        font_body,
        WHITE,
        (tx, body_y, tw, rect.height - (body_y - rect.y) - pad_y),
        y_start=body_y,
        line_spacing=S(6),
        blank_spacing=S(14),
    )
 
    
def draw_arrow(surface, start, end, color=WHITE, width=2, head_len=None, head_angle_deg=19):
    if head_len is None:
        head_len = S(12)

    pygame.draw.line(surface, color, start, end, max(1, width))

    dx = end[0] - start[0]
    dy = end[1] - start[1]
    ang = math.atan2(dy, dx)

    a1 = ang + math.radians(180 - head_angle_deg)
    a2 = ang - math.radians(180 - head_angle_deg)

    p1 = (
        end[0] + head_len * math.cos(a1),
        end[1] + head_len * math.sin(a1),
    )
    p2 = (
        end[0] + head_len * math.cos(a2),
        end[1] + head_len * math.sin(a2),
    )

    pygame.draw.line(surface, color, end, p1, max(1, width))
    pygame.draw.line(surface, color, end, p2, max(1, width))
    
    
def get_example_dots(center, radius):
    global EXAMPLE_DOTS_CACHE

    if EXAMPLE_DOTS_CACHE is None:
        example_prop = 0.56
        dots, _, _ = make_trial_dots(N_DOTS, example_prop, center, radius)
        EXAMPLE_DOTS_CACHE = {
            "dots": dots,
            "center": center,
            "radius": radius,
        }

    return EXAMPLE_DOTS_CACHE
  
  
def draw_example_task_display(
    screen,
    font_title,
    font_body,
    font_small,
    font_aid_label,
    font_aid,
    aid_label="#####",
    transparency_level="none",
):
    """
    Frozen example of a task trial for instruction slides.
    """
    center = (WIDTH // 2, HEIGHT // 2 + S(20))

    # fixed example stimulus (generated once only)
    cached = get_example_dots(center, DISH_RADIUS)
    dots = cached["dots"]

    # background
    screen.fill(BG)

    # countdown timer (top-left)
    timer_rect = draw_countdown_timer(
        surface=screen,
        font=font_body,
        ms_left=4200,
        x=PB_PAD,
        y=PB_PAD,
        color=WHITE,
    )

    # progress bar (top-right)
    draw_progress_bar(screen, trials_left=24, total_trials=40)

    pb_text = "Samples left: 24"
    pb_img = font_small.render(pb_text, True, WHITE)
    screen.blit(pb_img, (WIDTH - PB_PAD - PB_W, PB_PAD + PB_H + 6))

    # central dish
    draw_petri_dish(screen, center, DISH_RADIUS)

    # dots
    dot_layer = pygame.Surface((WIDTH, HEIGHT), pygame.SRCALPHA)
    dot_layer.fill((0, 0, 0, 0))
    for d in dots:
        x = int(d["x"])
        y = int(d["y"])
        r, g, b = d["col"]
        pygame.draw.circle(dot_layer, (r, g, b, DOT_ALPHA), (x, y), DOT_RADIUS)
    screen.blit(dot_layer, (0, 0))

    # bottom prompt
    prompt_rect = draw_trial_prompt_stacked(
        screen,
        font_small,
        HEIGHT - S(80),
        key_black_name="D",
        key_white_name="J",
    )

    aid_layout = draw_aid_recommendation_top_center(
        screen,
        font_aid_label,
        font_aid,
        rec_label=aid_label,
        show_value=True,
        transparency_level=transparency_level,
        evidence_black_pct=56.0,
        evidence_white_pct=44.0,
        dish_top_limit=center[1] - DISH_RADIUS,
    )

    return {
        "dish_center": center,
        "dish_radius": DISH_RADIUS,
        "timer_rect": timer_rect,
        "progress_anchor": (
            WIDTH - PB_PAD - PB_W // 2,
            PB_PAD + PB_H + font_small.get_height() + S(8),
        ),
        "aid_rect": aid_layout["value_rect"],
        "aid_anchor": aid_layout["value_rect"].center if aid_layout["value_rect"] is not None else (WIDTH // 2, PB_PAD + PB_H + S(50)),
        "aid_detail_rects": aid_layout["detail_rects"],
        "prompt_rect": prompt_rect,
    }
    

def draw_example_task_slide(
    screen,
    font_title,
    font_body,
    font_small,
    font_aid_label,
    font_aid,
    callout=None,
):
    meta = draw_example_task_display(
        screen,
        font_title=font_title,
        font_body=font_body,
        font_small=font_small,
        font_aid_label=font_aid_label,
        font_aid=font_aid,
    )

    callout_title_font = font_body
    callout_body_font = font_small
    AID_ARROW_PAD = S(12)
    
    if callout == "timer":
        timer_title = "Timer"
        timer_body = "The countdown timer shows how many seconds remain in the trial"
        tw, th = measure_callout_box(timer_title, timer_body, callout_title_font, callout_body_font)
        rect_timer = pygame.Rect(S(80), S(95), tw, th)
        draw_callout_box(
            screen,
            timer_title,
            timer_body,
            rect_timer,
            callout_title_font,
            callout_body_font,
        )
        target = (
            meta["timer_rect"].midright[0] + AID_ARROW_PAD,
            meta["timer_rect"].midright[1],
        )
        draw_arrow(
            screen,
            rect_timer.midtop,
            target,
            color=WHITE,
            width=max(1, S(2)),
        )

    elif callout == "aid":
        aid_title = "Automated decision aid"
        aid_body = "In automation blocks, the aid's recommendation appears here"
        aw, ah = measure_callout_box(aid_title, aid_body, callout_title_font, callout_body_font)

        rect_aid = pygame.Rect(S(80), S(95), aw, ah)

        draw_callout_box(
            screen,
            aid_title,
            aid_body,
            rect_aid,
            callout_title_font,
            callout_body_font,
        )

        target = (
            meta["aid_rect"].midleft[0] - AID_ARROW_PAD,
            meta["aid_rect"].midleft[1],
        )

        draw_arrow(
            screen,
            rect_aid.midright,
            target,
            color=WHITE,
            width=max(1, S(2)),
        )

    elif callout == "stimulus":
        stim_title = "Virus sample"
        stim_body = "Your task is to judge whether the sample looks more BLACK or more WHITE overall"
        sw, sh = measure_callout_box(stim_title, stim_body, callout_title_font, callout_body_font)
        
        rect_stim = pygame.Rect(S(40), HEIGHT // 2 - sh // 2, sw, sh)

        draw_callout_box(
            screen,
            stim_title,
            stim_body,
            rect_stim,
            callout_title_font,
            callout_body_font,
        )

        # horizontal arrow into left side of dish, with arrow tip shifted a bit left
        stim_target = (
            meta["dish_center"][0] - meta["dish_radius"] + S(12),
            rect_stim.midright[1],
        )

        draw_arrow(
            screen,
            rect_stim.midright,
            stim_target,
            color=WHITE,
            width=max(1, S(2)),
        )
        
    elif callout == "progress":
        prog_title = "Progress bar"
        prog_body = "The progress bar indicates how far through the current block you are"
        pw, ph = measure_callout_box(prog_title, prog_body, callout_title_font, callout_body_font)
        rect_prog = pygame.Rect(WIDTH - S(80) - pw, S(95), pw, ph)
        draw_callout_box(
            screen,
            prog_title,
            prog_body,
            rect_prog,
            callout_title_font,
            callout_body_font,
        )
        draw_arrow(
            screen,
            rect_prog.midtop,
            meta["progress_anchor"],
            color=WHITE,
            width=max(1, S(2)),
        )

    elif callout == "keys":
        keys_title = "Response keys"
        keys_body = "The response keys to use will be shown here"
        kw, kh = measure_callout_box(keys_title, keys_body, callout_title_font, callout_body_font)

        # moved slightly lower
        rect_keys = pygame.Rect(
            WIDTH // 2 - kw // 2 - S(120),
            HEIGHT - S(260),
            kw,
            kh,
        )

        draw_callout_box(
            screen,
            keys_title,
            keys_body,
            rect_keys,
            callout_title_font,
            callout_body_font,
        )
        target = (
            meta["prompt_rect"].midtop[0],
            meta["prompt_rect"].midtop[1] - S(12),
        )

        draw_arrow(
            screen,
            rect_keys.midbottom,
            target,
            color=WHITE,
            width=max(1, S(2)),
        )
    

def draw_automation_example_slide(
    screen,
    font_title,
    font_body,
    font_small,
    font_aid_label,
    font_aid,
    aid_label,
    transparency_level="none",
    callout=None,
):
    meta = draw_example_task_display(
        screen,
        font_title=font_title,
        font_body=font_body,
        font_small=font_small,
        font_aid_label=font_aid_label,
        font_aid=font_aid,
        aid_label=aid_label,
        transparency_level=transparency_level,
    )

    callout_title_font = font_body
    callout_body_font = font_small
    AID_ARROW_PAD = S(12)

    if callout == "intro":
        title = "Automated decision aid"
        body = (
            "In some parts of this study you will be assisted "
            "by an Automated Decision Aid"
        )
        bw, bh = measure_callout_box(title, body, callout_title_font, callout_body_font)
        rect = pygame.Rect(S(80), S(95), bw, bh)
        draw_callout_box(
            screen,
            title,
            body,
            rect,
            callout_title_font,
            callout_body_font,
        )
        target = (
            meta["aid_rect"].midleft[0] - AID_ARROW_PAD,
            meta["aid_rect"].midleft[1],
        )
        draw_arrow(
            screen,
            rect.midright,
            target,
            color=WHITE,
            width=max(1, S(2)),
        )

    elif callout == "classification":
        title = "Automated decision aid"
        body = (
            "The automation will recommend a classification "
            "(either BLACK or WHITE) for each sample"
        )
        bw, bh = measure_callout_box(title, body, callout_title_font, callout_body_font)
        rect = pygame.Rect(WIDTH - S(80) - bw, S(95), bw, bh)
        draw_callout_box(
            screen,
            title,
            body,
            rect,
            callout_title_font,
            callout_body_font,
        )
        target = (
            meta["aid_rect"].midright[0] + AID_ARROW_PAD,
            meta["aid_rect"].midright[1],
        )
        draw_arrow(
            screen,
            rect.midleft,
            target,
            color=WHITE,
            width=max(1, S(2)),
        )

    elif callout == "black_example":
        title = "Recommendation"
        body = (
            "In this example, the automated decision aid is "
            "recommending that you classify the sample as V-BLACK"
        )
        bw, bh = measure_callout_box(title, body, callout_title_font, callout_body_font)
        
        rect = pygame.Rect(S(80), S(95), bw, bh)

        draw_callout_box(
            screen,
            title,
            body,
            rect,
            callout_title_font,
            callout_body_font,
        )
        target = (
            meta["aid_rect"].midleft[0] - AID_ARROW_PAD,
            meta["aid_rect"].midleft[1],
        )
        draw_arrow(
            screen,
            rect.midright,
            target,
            color=WHITE,
            width=max(1, S(2)),
        )

    elif callout == "white_example":
        title = "Recommendation"
        body = (
            "In this example, the automated decision aid is "
            "recommending that you classify the sample as V-WHITE"
        )
        bw, bh = measure_callout_box(title, body, callout_title_font, callout_body_font)

        rect = pygame.Rect(WIDTH - S(80) - bw, S(95), bw, bh)

        draw_callout_box(
            screen,
            title,
            body,
            rect,
            callout_title_font,
            callout_body_font,
        )
        target = (
            meta["aid_rect"].midright[0] + AID_ARROW_PAD,
            meta["aid_rect"].midright[1],
        )
        draw_arrow(
            screen,
            rect.midleft,
            target,
            color=WHITE,
            width=max(1, S(2)),
        )
    elif callout == "transparency_none":
        title = "No transparency"
        body = (
            "In some automation blocks, the aid will display only its recommendation"
        )
        bw, bh = measure_callout_box(title, body, callout_title_font, callout_body_font)
        rect = pygame.Rect(S(80), S(95), bw, bh)
        draw_callout_box(screen, title, body, rect, callout_title_font, callout_body_font)
        target = (
            meta["aid_rect"].midleft[0] - AID_ARROW_PAD,
            meta["aid_rect"].midleft[1],
        )
        draw_arrow(screen, rect.midright, target, color=WHITE, width=max(1, S(2)))
    elif callout == "transparency_low":
        title = "Low transparency"
        body = (
            "In some automation blocks, the aid will display its recommendation and a brief reason"
        )
        bw, bh = measure_callout_box(title, body, callout_title_font, callout_body_font)
        rect = pygame.Rect(WIDTH - S(80) - bw, S(95), bw, bh)
        draw_callout_box(screen, title, body, rect, callout_title_font, callout_body_font)
        detail_rect = meta["aid_detail_rects"][0]
        target = (
            detail_rect.right + AID_ARROW_PAD,
            detail_rect.centery,
        )
        draw_arrow(screen, rect.midleft, target, color=WHITE, width=max(1, S(2)))
    elif callout == "transparency_high":
        title = "High transparency"
        body = (
            "In some automation blocks, the aid will display its recommendation, a brief reason, "
            "the evidence summary, and the decision rule"
        )
        bw, bh = measure_callout_box(title, body, callout_title_font, callout_body_font)
        rect = pygame.Rect(S(80), S(95), bw, bh)
        draw_callout_box(screen, title, body, rect, callout_title_font, callout_body_font)
        detail_rect = meta["aid_detail_rects"][2]
        target = (
            detail_rect.left - AID_ARROW_PAD,
            detail_rect.centery,
        )
        draw_arrow(screen, rect.midright, target, color=WHITE, width=max(1, S(2)))
        
    
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

    # keep full dot circles inside the dish border
    effective_radius = max(0, radius - DOT_RADIUS)

    pts = sample_points_in_circle(n_dots, center, effective_radius)
    random.shuffle(pts)

    dots = []
    for i in range(n_dots):
        col = VBLACK if i < n_vblack else VWHITE
        vx = random.uniform(-VEL_INIT_RANGE, VEL_INIT_RANGE)
        vy = random.uniform(-VEL_INIT_RANGE, VEL_INIT_RANGE)
        dots.append({"x": pts[i][0], "y": pts[i][1], "vx": vx, "vy": vy, "col": col})

    random.shuffle(dots)
    return dots, n_vblack, n_vwhite
  

def draw_countdown_timer(surface, font, ms_left, x, y, color=WHITE):
    """
    Draws a countdown timer (seconds remaining) at (x,y) top-left anchored.
    Returns the text rect.
    """
    sec_left = max(0.0, ms_left / 1000.0)
    txt = f"{sec_left:4.1f}s"
    img = font.render(txt, True, color)

    rect = img.get_rect(topleft=(x, y))
    surface.blit(img, rect)

    return rect
    
    
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
    
    # compute bounding rect for the two-column prompt
    top_y = y_top
    bottom_y = y_bottom + font_small.get_height()

    rect = pygame.Rect(
        start_x,
        top_y,
        total_w,
        bottom_y - top_y
    )

    return rect


def draw_aid_recommendation_top_center(
    screen,
    font_label,
    font_main,
    rec_label,
    show_value=True,
    transparency_level="none",
    evidence_black_pct=None,
    evidence_white_pct=None,
    dish_top_limit=None,
):
    cx = WIDTH // 2
    top_padding = S(8)
    line_gap = max(1, S(4))
    detail_font = load_font(FONT_LIGHT, max(10, S(FONT_SMALL_BASE - 1)))

    img_label = font_label.render("AID JUDGES:", True, WHITE)
    img_main = None
    detail_imgs = []

    if show_value:
        phrase = f"{rec_label}"
        col = COLOR_TOKENS_AID.get(rec_label, WHITE)
        img_main = font_main.render(phrase, True, col)

        detail_lines = []
        if transparency_level in ("low", "high"):
            detail_lines.append(f"Reason: The available evidence favors {rec_label}")

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

    y0 = top_padding
    if dish_top_limit is not None:
        max_bottom = dish_top_limit - S(18)
        y0 = min(y0, max_bottom - total_height)
        y0 = max(S(4), y0)

    rect_label = img_label.get_rect(midtop=(cx, y0))
    screen.blit(img_label, rect_label)

    value_rect = None
    detail_rects = []
    if img_main is not None:
        value_rect = img_main.get_rect(midtop=(cx, rect_label.bottom + line_gap))
        screen.blit(img_main, value_rect)

        current_y = value_rect.bottom + line_gap
        for detail_img in detail_imgs:
            if transparency_level == "low":
                rect_detail = detail_img.get_rect(midtop=(cx, current_y))
            else:
                rect_detail = detail_img.get_rect(midtop=(cx, current_y))
            screen.blit(detail_img, rect_detail)
            detail_rects.append(rect_detail)
            current_y = rect_detail.bottom + line_gap

    return {
        "label_rect": rect_label,
        "value_rect": value_rect,
        "detail_rects": detail_rects,
    }


# -----------------------------
# Slide drawing
# -----------------------------
def draw_slide(screen, font_title, font_body, font_button, font_small, font_aid_label, font_aid, slide_idx):
    slide = SLIDES[slide_idx]
    kind = slide.get("kind", "text")

    if kind == "text":
        title = slide["title"]
        body = slide["body"]

        screen.fill(BG_INSTRUCTIONS)

        content_x = S(120)
        content_w = WIDTH - S(240)

        line_spacing = S(10)
        blank_spacing = S(30)
        gap_title_to_body = S(26)

        title_h = font_title.get_height()
        body_h = _measure_wrapped_height(body, font_body, content_w, line_spacing, blank_spacing)

        total_h = title_h + gap_title_to_body + body_h
        start_y = HEIGHT // 2 - total_h // 2

        title_img = font_title.render(title, True, WHITE)
        screen.blit(title_img, (WIDTH // 2 - title_img.get_width() // 2, start_y))

        y = start_y + title_h + gap_title_to_body
        body_rect = (content_x, 0, content_w, HEIGHT)
        draw_wrapped_block_centered(
            screen,
            body,
            font_body,
            WHITE,
            body_rect,
            y_start=y,
            line_spacing=line_spacing,
            blank_spacing=blank_spacing,
        )

    elif kind == "example_task_display":
        draw_example_task_slide(
            screen,
            font_title=font_title,
            font_body=font_body,
            font_small=font_small,
            font_aid_label=font_aid_label,
            font_aid=font_aid,
            callout=slide.get("callout"),
        )
        
    elif kind == "automation_example":
        draw_automation_example_slide(
            screen,
            font_title=font_title,
            font_body=font_body,
            font_small=font_small,
            font_aid_label=font_aid_label,
            font_aid=font_aid,
            aid_label=slide.get("aid_label", "BLACK"),
            transparency_level=slide.get("transparency_level", "none"),
            callout=slide.get("callout"),
        )
        
    # buttons always on top
    btn_w = S(170)
    btn_h = S(58)
    pad_x = S(40)
    pad_y = S(34)

    back_rect = pygame.Rect(pad_x, HEIGHT - pad_y - btn_h, btn_w, btn_h)
    next_rect = pygame.Rect(WIDTH - pad_x - btn_w, HEIGHT - pad_y - btn_h, btn_w, btn_h)

    back_enabled = slide_idx > 0
    next_label = "NEXT" if slide_idx < len(SLIDES) - 1 else "DONE"

    draw_button(screen, back_rect, "BACK", font_button, enabled=back_enabled)
    draw_button(screen, next_rect, next_label, font_button, enabled=True)

    return back_rect, next_rect, back_enabled


# -----------------------------
# Main
# -----------------------------
def main():
    random.seed(1234)
    pygame.init()
    pygame.display.set_caption("Instruction Slides")

    flags = 0
    if FULLSCREEN:
        flags |= pygame.FULLSCREEN

    if FULLSCREEN and USE_DESKTOP_RES:
        screen = pygame.display.set_mode((0, 0), flags)
    else:
        screen = pygame.display.set_mode((BASE_W, BASE_H))

    global WIDTH, HEIGHT
    WIDTH, HEIGHT = screen.get_size()

    global UI_SCALE
    UI_SCALE = compute_ui_scale(WIDTH, HEIGHT)

    global DISH_RADIUS, DOT_RADIUS, PB_W, PB_H, PB_PAD
    DISH_RADIUS = S(DISH_RADIUS_BASE)
    DOT_RADIUS = max(1, S(DOT_RADIUS_BASE))
    PB_W = S(PB_W_BASE)
    PB_H = S(PB_H_BASE)
    PB_PAD = S(PB_PAD_BASE)
    
    global VEL_WANDER_SD, VEL_MAX, VEL_INIT_RANGE

    VEL_WANDER_SD  = SF(VEL_WANDER_SD_BASE)
    VEL_MAX        = SF(VEL_MAX_BASE)
    VEL_INIT_RANGE = SF(VEL_INIT_RANGE_BASE)

    font_title = load_font(FONT_LIGHT, max(12, S(FONT_TITLE_BASE)))
    font_body = load_font(FONT_LIGHT, max(10, S(FONT_BODY_BASE)))
    font_button = load_font(FONT_LIGHT, max(10, S(FONT_BUTTON_BASE)))
    font_small = load_font(FONT_LIGHT, max(9, S(18)))
    font_aid_label = load_font(FONT_LIGHT, max(9, S(20)))
    font_aid = load_font(FONT_BOLD, max(10, S(32)))

    clock = pygame.time.Clock()
    current_slide = 0

    while True:
        clock.tick(FPS)

        back_rect, next_rect, back_enabled = draw_slide(
            screen,
            font_title,
            font_body,
            font_button,
            font_small,
            font_aid_label,
            font_aid,
            current_slide,
        )
        pygame.display.flip()

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pass

            elif event.type == pygame.KEYDOWN:
                if is_hard_quit_event(event):
                    quit_clean()

                elif event.key == pygame.K_LEFT:
                    if current_slide > 0:
                        current_slide -= 1
                        break

                elif event.key == pygame.K_RIGHT:
                    if current_slide < len(SLIDES) - 1:
                        current_slide += 1
                    else:
                        quit_clean()
                    break

            elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                mx, my = event.pos

                if back_enabled and back_rect.collidepoint(mx, my):
                    current_slide -= 1
                    break

                if next_rect.collidepoint(mx, my):
                    if current_slide < len(SLIDES) - 1:
                        current_slide += 1
                    else:
                        quit_clean()
                    break


if __name__ == "__main__":
    main()
    
