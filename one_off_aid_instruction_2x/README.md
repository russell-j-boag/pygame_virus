# One-off 2x Automated Decision Aid screenshot

This directory is self-contained and does not import or modify the live task.
It renders the viewer-facing instruction screen whose callout begins “In some
parts of this study…”, with every visible font exactly twice its current size.

From the repository root, run:

```sh
SDL_VIDEODRIVER=dummy \
  /Users/rjb779/Library/r-miniconda-arm64/envs/r-pygame/bin/python \
  one_off_aid_instruction_2x/render_aid_instruction_2x.py
```

The default output is `automated_decision_aid_intro_2x.png` in this directory.
To write another copy elsewhere, pass `--output PATH`.

The renderer uses a fixed 1512×982 canvas, deterministic stimulus dots, and
internal checks for text containment and unintended text overlap. Its bundled
Roboto font files are resolved relative to the script, so it can be moved and
run as a standalone directory. The aid heading/value, stimulus disc, and pair
of response-key columns share the display's horizontal center line; the
narrower callout overlaps the left edge of the stimulus slightly.
