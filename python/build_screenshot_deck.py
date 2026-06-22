"""
Build a PowerPoint deck from generated screenshot PNGs.

The deck is ordered as:
  1. instruction_screenshots/*.png
  2. virus_task_screenshots/*.png

Pandoc writes the Office package, then this script patches the slide canvas to
the default screenshot resolution and places each image full slide height.
"""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
import tempfile
import zipfile
from pathlib import Path
from xml.etree import ElementTree as ET

from PIL import Image


DEFAULT_INSTRUCTION_DIR = Path("instruction_screenshots")
DEFAULT_VIRUS_DIR = Path("virus_task_screenshots")
DEFAULT_OUTPUT = Path("screenshots_review.pptx")
DEFAULT_SLIDE_RESOLUTION = "1512x982"
EMU_PER_PIXEL = 9525

P_NS = "http://schemas.openxmlformats.org/presentationml/2006/main"
A_NS = "http://schemas.openxmlformats.org/drawingml/2006/main"
R_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
NS = {"p": P_NS, "a": A_NS, "r": R_NS}

ET.register_namespace("p", P_NS)
ET.register_namespace("a", A_NS)
ET.register_namespace("r", R_NS)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Create a PowerPoint review deck from instruction_screenshots "
            "followed by virus_task_screenshots."
        )
    )
    parser.add_argument(
        "--instruction-dir",
        default=str(DEFAULT_INSTRUCTION_DIR),
        help=f"Instruction screenshot directory. Default: {DEFAULT_INSTRUCTION_DIR}",
    )
    parser.add_argument(
        "--virus-dir",
        default=str(DEFAULT_VIRUS_DIR),
        help=f"Virus task screenshot directory. Default: {DEFAULT_VIRUS_DIR}",
    )
    parser.add_argument(
        "--output",
        default=str(DEFAULT_OUTPUT),
        help=f"Output .pptx path. Default: {DEFAULT_OUTPUT}",
    )
    parser.add_argument(
        "--slide-resolution",
        default=DEFAULT_SLIDE_RESOLUTION,
        help=(
            f"Slide canvas size. Default: {DEFAULT_SLIDE_RESOLUTION}. Use "
            "'screenshots' to preserve the screenshot aspect ratio with "
            "Pandoc's default slide height."
        ),
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Replace an existing output deck.",
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


def parse_slide_resolution(value: str) -> tuple[int, int] | None:
    if value.lower() == "screenshots":
        return None

    explicit = parse_explicit_resolution(value)
    if explicit is None:
        raise ValueError(
            f"invalid --slide-resolution {value!r}; use 'screenshots' or WIDTHxHEIGHT"
        )
    return explicit


def pngs_in(directory: Path) -> list[Path]:
    if not directory.is_dir():
        raise RuntimeError(f"input directory does not exist: {directory}")

    paths = sorted(directory.glob("*.png"))
    if not paths:
        raise RuntimeError(f"no PNG files found in {directory}")
    return paths


def validate_output_path(path: Path, overwrite: bool) -> None:
    if path.exists():
        if not overwrite:
            raise RuntimeError(f"{path} already exists; pass --overwrite to replace it")
        if path.is_dir():
            raise RuntimeError(f"output path is a directory: {path}")
        path.unlink()

    path.parent.mkdir(parents=True, exist_ok=True)


def markdown_image_target(path: Path) -> str:
    # Angle-bracket links keep paths with spaces parseable in Pandoc Markdown.
    return f"<{path.resolve().as_posix()}>"


def write_markdown_deck(path: Path, image_paths: list[Path]) -> None:
    lines = []

    for idx, image_path in enumerate(image_paths):
        if idx > 0:
            lines.extend(["", "---", ""])
        lines.append(f"![]({markdown_image_target(image_path)})")

    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def run_pandoc(markdown_path: Path, output_path: Path) -> None:
    pandoc = shutil.which("pandoc")
    if pandoc is None:
        raise RuntimeError("pandoc is required to build the PowerPoint deck")

    result = subprocess.run(
        [pandoc, str(markdown_path), "-o", str(output_path)],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        detail = result.stderr.strip() or result.stdout.strip() or "no details"
        raise RuntimeError(f"pandoc failed: {detail}")


def slide_size(pptx_path: Path) -> tuple[int, int]:
    with zipfile.ZipFile(pptx_path) as deck:
        root = ET.fromstring(deck.read("ppt/presentation.xml"))

    slide_size_el = root.find("p:sldSz", NS)
    if slide_size_el is None:
        raise RuntimeError("generated PPTX is missing ppt/presentation.xml slide size")

    return int(slide_size_el.attrib["cx"]), int(slide_size_el.attrib["cy"])


def image_pixel_size(path: Path) -> tuple[int, int]:
    with Image.open(path) as img:
        pixel_w, pixel_h = img.size

    if pixel_w <= 0 or pixel_h <= 0:
        raise RuntimeError(f"invalid image size for {path}: {pixel_w}x{pixel_h}")

    return pixel_w, pixel_h


def common_image_aspect(image_paths: list[Path]) -> tuple[int, int]:
    ref_w, ref_h = image_pixel_size(image_paths[0])
    for path in image_paths[1:]:
        pixel_w, pixel_h = image_pixel_size(path)
        if pixel_w * ref_h != ref_w * pixel_h:
            raise RuntimeError(
                "screenshots do not share one aspect ratio; regenerate both "
                "screenshot folders at the same resolution before building the deck "
                f"({image_paths[0]} is {ref_w}x{ref_h}, {path} is {pixel_w}x{pixel_h})"
            )

    return ref_w, ref_h


def target_slide_size(
    pptx_path: Path,
    image_paths: list[Path],
    slide_resolution: tuple[int, int] | None,
) -> tuple[int, int]:
    pixel_w, pixel_h = common_image_aspect(image_paths)

    if slide_resolution is not None:
        slide_pixel_w, slide_pixel_h = slide_resolution
        if pixel_w * slide_pixel_h != slide_pixel_w * pixel_h:
            raise RuntimeError(
                "screenshots do not match the requested slide resolution; "
                f"screenshots are {pixel_w}x{pixel_h}, requested slide "
                f"resolution is {slide_pixel_w}x{slide_pixel_h}. Regenerate "
                "both screenshot folders at that resolution or pass "
                "--slide-resolution screenshots."
            )
        return slide_pixel_w * EMU_PER_PIXEL, slide_pixel_h * EMU_PER_PIXEL

    _, source_slide_h = slide_size(pptx_path)
    slide_w = int(round(source_slide_h * (pixel_w / pixel_h)))
    if slide_w <= 0:
        raise RuntimeError(f"invalid target slide width computed from {pixel_w}x{pixel_h}")

    return slide_w, source_slide_h


def image_placement(path: Path, slide_w: int, slide_h: int) -> tuple[int, int, int, int]:
    pixel_w, pixel_h = image_pixel_size(path)

    height = slide_h
    width = int(round(height * (pixel_w / pixel_h)))
    x = int(round((slide_w - width) / 2))
    y = 0
    return x, y, width, height


def patch_slide_xml(xml_bytes: bytes, image_path: Path, slide_w: int, slide_h: int) -> bytes:
    root = ET.fromstring(xml_bytes)
    picture = root.find(".//p:pic", NS)
    if picture is None:
        raise RuntimeError(f"generated slide has no picture for {image_path}")

    xfrm = picture.find("p:spPr/a:xfrm", NS)
    if xfrm is None:
        raise RuntimeError(f"generated slide picture has no transform for {image_path}")

    off = xfrm.find("a:off", NS)
    ext = xfrm.find("a:ext", NS)
    if off is None or ext is None:
        raise RuntimeError(f"generated slide picture has incomplete transform for {image_path}")

    x, y, cx, cy = image_placement(image_path, slide_w, slide_h)
    off.attrib.update({"x": str(x), "y": str(y)})
    ext.attrib.update({"cx": str(cx), "cy": str(cy)})

    return ET.tostring(root, encoding="utf-8", xml_declaration=True)


def patch_presentation_xml(xml_bytes: bytes, slide_w: int, slide_h: int) -> bytes:
    root = ET.fromstring(xml_bytes)
    slide_size_el = root.find("p:sldSz", NS)
    if slide_size_el is None:
        raise RuntimeError("generated PPTX is missing ppt/presentation.xml slide size")

    slide_size_el.attrib.update(
        {
            "cx": str(slide_w),
            "cy": str(slide_h),
            "type": "custom",
        }
    )

    return ET.tostring(root, encoding="utf-8", xml_declaration=True)


def patch_full_height_images(
    input_pptx: Path,
    output_pptx: Path,
    image_paths: list[Path],
    slide_resolution: tuple[int, int] | None,
) -> None:
    slide_w, slide_h = target_slide_size(input_pptx, image_paths, slide_resolution)
    slide_name_re = re.compile(r"^ppt/slides/slide(\d+)\.xml$")

    with zipfile.ZipFile(input_pptx) as src, zipfile.ZipFile(
        output_pptx,
        "w",
        compression=zipfile.ZIP_DEFLATED,
    ) as dst:
        for info in src.infolist():
            data = src.read(info.filename)
            if info.filename == "ppt/presentation.xml":
                data = patch_presentation_xml(data, slide_w, slide_h)
            elif match := slide_name_re.match(info.filename):
                slide_idx = int(match.group(1))
                if 1 <= slide_idx <= len(image_paths):
                    data = patch_slide_xml(data, image_paths[slide_idx - 1], slide_w, slide_h)
            dst.writestr(info, data)


def write_pptx(
    output: Path,
    image_paths: list[Path],
    slide_resolution: tuple[int, int] | None,
) -> None:
    with tempfile.TemporaryDirectory(prefix="screenshot-deck-") as tmp_dir:
        tmp_dir_path = Path(tmp_dir)
        markdown_path = tmp_dir_path / "screenshots.md"
        pandoc_pptx = tmp_dir_path / "pandoc_deck.pptx"

        write_markdown_deck(markdown_path, image_paths)
        run_pandoc(markdown_path, pandoc_pptx)
        patch_full_height_images(pandoc_pptx, output, image_paths, slide_resolution)


def main() -> int:
    args = parse_args()
    instruction_dir = Path(args.instruction_dir)
    virus_dir = Path(args.virus_dir)
    output = Path(args.output)

    try:
        instruction_paths = pngs_in(instruction_dir)
        virus_paths = pngs_in(virus_dir)
        image_paths = instruction_paths + virus_paths
        slide_resolution = parse_slide_resolution(args.slide_resolution)
        validate_output_path(output, args.overwrite)
        write_pptx(output, image_paths, slide_resolution)
    except (RuntimeError, ValueError, OSError, zipfile.BadZipFile, ET.ParseError) as exc:
        print(f"build_screenshot_deck: error: {exc}", file=sys.stderr)
        return 1

    print(f"Instruction screenshots: {len(instruction_paths)}")
    print(f"Virus task screenshots: {len(virus_paths)}")
    print(f"Total slides: {len(image_paths)}")
    print(f"Slide resolution: {args.slide_resolution}")
    print(f"Output: {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
