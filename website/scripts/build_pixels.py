#!/usr/bin/env python3
"""Build the landing-page pixel art.

The sprites are defined here as spans on a character grid — this file is the
source of truth, not the SVG output. Every colour is a design-system token
(docs/design.md), so the emitted SVG inherits the app palette instead of
carrying hexes of its own.

Outputs, all relative to `website/`:

  assets/img/favicon.svg      standalone, literal hexes (favicons cannot
                              inherit CSS custom properties)
  index.qmd                   the buffer diagram is injected inline between the
                              `pixel:<name>` markers, which is what lets its
                              pixels use var() tokens

Run from the repository root:

    python3 website/scripts/build_pixels.py
"""
from __future__ import annotations

import pathlib
import re

HERE = pathlib.Path(__file__).resolve().parent
SITE = HERE.parent
IMG = SITE / "assets" / "img"

# char -> (literal hex, CSS custom property). The hexes mirror the app tokens
# and are only used where var() cannot reach (the favicon).
PALETTE: dict[str, tuple[str, str]] = {
    "K": ("#222826", "--pel-ink"),      # --text-primary  · orelhas, focinho
    "R": ("#C86446", "--pel-coat"),     # --secondary     · pelagem, meio-tom
    "D": ("#A84A30", "--pel-shade"),    # --secondary-ink · sombra da pelagem
    "W": ("#F7F3E9", "--pel-light"),    # --bg-main       · queixo
    "A": ("#E5A93C", "--pel-eye"),      # --warning       · olho âmbar
    "G": ("#1E4620", "--pel-area"),     # --primary       · área de operação
    "S": ("#C9AE7C", "--pel-buffer"),   # --sand          · buffer de 10 km
}


class Grid:
    def __init__(self, w: int, h: int) -> None:
        self.w, self.h = w, h
        self.cells = [["."] * w for _ in range(h)]

    def span(self, y: int, x0: int, x1: int, ch: str) -> None:
        if 0 <= y < self.h:
            for x in range(max(0, x0), min(self.w - 1, x1) + 1):
                self.cells[y][x] = ch

    def paint(self, spans, ch: str) -> None:
        for y, x0, x1 in spans:
            self.span(y, x0, x1, ch)


    def to_svg(self, css_class: str, label: str = "", literal: bool = False) -> str:
        """One <g> per colour, horizontal runs merged into single rects."""
        groups = []
        for ch, (hexval, var) in PALETTE.items():
            runs = []
            for y in range(self.h):
                x = 0
                while x < self.w:
                    if self.cells[y][x] != ch:
                        x += 1
                        continue
                    end = x
                    while end + 1 < self.w and self.cells[y][end + 1] == ch:
                        end += 1
                    runs.append(
                        f'<rect x="{x}" y="{y}" width="{end - x + 1}" height="1"/>'
                    )
                    x = end + 1
            if runs:
                fill = hexval if literal else f"var({var})"
                groups.append(f'<g fill="{fill}">{"".join(runs)}</g>')

        a11y = (f' role="img" aria-label="{label}"' if label
                else ' role="presentation" aria-hidden="true"')
        return (
            f'<svg xmlns="http://www.w3.org/2000/svg" '
            f'viewBox="0 0 {self.w} {self.h}" shape-rendering="crispEdges" '
            f'class="{css_class}"{a11y}>{"".join(groups)}</svg>'
        )


# --------------------------------------------------------------------------
# The geographic check, drawn the way the app draws it (docs/design.md §Mapa):
# the área de operação in --primary, the 10 km buffer in --sand, GBIF
# occurrences in --secondary. A rasterised circle is how pixel art draws a
# circle and how a buffer gets computed — same gesture, two centuries apart.
# --------------------------------------------------------------------------
def buffer_diagram() -> Grid:
    g = Grid(40, 40)
    cx = cy = 20

    # midpoint circle, drawn dashed so it reads as a limit rather than a ring
    x, y, d = 0, 17, 1 - 17
    points = set()
    while x <= y:
        for sx, sy in ((x, y), (y, x), (-x, y), (-y, x),
                       (x, -y), (y, -x), (-x, -y), (-y, -x)):
            points.add((cx + sx, cy + sy))
        x += 1
        if d < 0:
            d += 2 * x + 1
        else:
            y -= 1
            d += 2 * (x - y) + 1
    for px, py in sorted(points):
        if (px + py) % 3:
            g.span(py, px, px, "S")

    g.paint([(17, 17, 22), (18, 16, 23), (19, 16, 24),
             (20, 15, 23), (21, 16, 22), (22, 17, 21)], "G")

    # Uma ocorrência é um ponto: um pixel só. Em 2×2 os registros viravam
    # blocos e competiam com a área de estudo em vez de pontuá-la.
    for px, py in [(11, 12), (27, 15), (24, 27), (13, 24), (35, 33)]:
        g.span(py, px, px, "R")

    return g


def favicon() -> Grid:
    """Frontal wolf face. Literal hexes: a favicon has no page to inherit from."""
    g = Grid(16, 16)
    g.paint([
        (1, 2, 3), (1, 12, 13), (2, 2, 4), (2, 11, 13), (3, 2, 4), (3, 11, 13),
    ], "K")
    g.paint([
        (4, 2, 13), (5, 2, 13), (6, 2, 13), (7, 2, 13), (8, 3, 12),
        (9, 3, 12), (10, 4, 11), (11, 5, 10), (12, 6, 9),
    ], "R")
    g.paint([(3, 3, 3), (3, 12, 12)], "D")
    g.paint([(6, 4, 5), (6, 10, 11)], "A")
    g.paint([(9, 6, 9), (10, 6, 9)], "W")
    g.paint([(11, 7, 8)], "K")
    return g


def inject(path: pathlib.Path, name: str, svg: str) -> bool:
    """Replace whatever sits between the markers for `name`."""
    text = path.read_text(encoding="utf-8")
    pattern = re.compile(
        rf"(<!-- pixel:{re.escape(name)}:start -->).*?(<!-- pixel:{re.escape(name)}:end -->)",
        re.DOTALL,
    )
    if not pattern.search(text):
        raise SystemExit(f"marcador 'pixel:{name}' não encontrado em {path}")
    updated = pattern.sub(lambda m: m.group(1) + "\n" + svg + "\n" + m.group(2), text)
    if updated == text:
        return False
    path.write_text(updated, encoding="utf-8")
    return True


def main() -> None:
    IMG.mkdir(parents=True, exist_ok=True)

    (IMG / "favicon.svg").write_text(
        favicon().to_svg("favicon", literal=True), encoding="utf-8"
    )

    index = SITE / "index.qmd"
    changed = inject(index, "buffer", buffer_diagram().to_svg("sprite sprite-buffer"))
    print(f"favicon.svg escrito em {IMG.relative_to(SITE.parent)}")
    print("index.qmd:", "atualizado" if changed else "já estava atual")


if __name__ == "__main__":
    main()
