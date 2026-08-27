#!/usr/bin/env python3
# Title: Build the Observatorio wordmark assets
# Crops the supplied logo to its ink box, scales it down, repaints the black ink
# ivory, and writes the result to the two trees that need it:
#
#   inst/app/www/img/observatorio-claro.png       -> the app's dark left rail
#   website/assets/img/observatorio-claro.png     -> the landing page's dark footer
#
# Both surfaces sit on the same forest green (--primary), so one variant serves
# them. The terracotta accent survives: only the achromatic ink is repainted.
#
# The source ships on a 2526x1786 canvas whose ink occupies only 2504x620: 72% of
# the height is empty, so an untrimmed logo renders small inside a box three and a
# half times taller than the wordmark.
#
# Standard library only. This machine has no PIL, no ImageMagick and no cwebp, and
# the deploy platform must not gain a build dependency for one asset.
#
#   python3 data-raw/prep_logo.py <source.png>
#
# Re-run it whenever the client supplies new artwork. Nothing here runs at app
# runtime.

import os
import struct
import sys
import zlib

TARGET_WIDTH = 800          # ~4x the 200px rail slot, enough for a HiDPI screen
IVORY = (0xF7, 0xF3, 0xE9)  # --bg-main, the app's ivory
ALPHA_FLOOR = 8             # below this a pixel is background, not ink


def read_png(path):
    """Decode an 8-bit RGBA PNG into (width, height, bytearray)."""
    data = open(path, "rb").read()
    if data[:8] != b"\x89PNG\r\n\x1a\n":
        raise SystemExit("%s is not a PNG." % path)

    idat, width, height, depth, color = b"", None, None, None, None
    pos = 8
    while pos < len(data):
        length = struct.unpack(">I", data[pos:pos + 4])[0]
        kind = data[pos + 4:pos + 8]
        chunk = data[pos + 8:pos + 8 + length]
        if kind == b"IHDR":
            width, height, depth, color = struct.unpack(">IIBB", chunk[:10])
        elif kind == b"IDAT":
            idat += chunk
        pos += 12 + length

    if (depth, color) != (8, 6):
        raise SystemExit("Expected an 8-bit RGBA PNG, got depth=%s colortype=%s."
                         % (depth, color))

    raw = zlib.decompress(idat)
    stride = width * 4
    out = bytearray(width * height * 4)
    prev = bytearray(stride)
    pos = 0
    for y in range(height):
        ftype = raw[pos]
        pos += 1
        line = bytearray(raw[pos:pos + stride])
        pos += stride
        if ftype:
            for x in range(stride):
                a = line[x - 4] if x >= 4 else 0
                b = prev[x]
                c = prev[x - 4] if x >= 4 else 0
                if ftype == 1:
                    line[x] = (line[x] + a) & 0xFF
                elif ftype == 2:
                    line[x] = (line[x] + b) & 0xFF
                elif ftype == 3:
                    line[x] = (line[x] + (a + b) // 2) & 0xFF
                elif ftype == 4:
                    guess = a + b - c
                    da, db, dc = abs(guess - a), abs(guess - b), abs(guess - c)
                    pick = a if da <= db and da <= dc else (b if db <= dc else c)
                    line[x] = (line[x] + pick) & 0xFF
                else:
                    raise SystemExit("Unknown PNG filter %d." % ftype)
        out[y * stride:(y + 1) * stride] = line
        prev = line
    return width, height, out


def write_png(path, width, height, pixels):
    stride = width * 4
    raw = bytearray()
    for y in range(height):
        raw.append(0)                                   # filter type 0 (None)
        raw += pixels[y * stride:(y + 1) * stride]

    def chunk(kind, payload):
        body = kind + payload
        return (struct.pack(">I", len(payload)) + body
                + struct.pack(">I", zlib.crc32(body) & 0xFFFFFFFF))

    png = (b"\x89PNG\r\n\x1a\n"
           + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0))
           + chunk(b"IDAT", zlib.compress(bytes(raw), 9))
           + chunk(b"IEND", b""))
    open(path, "wb").write(png)
    return len(png)


def ink_box(width, height, pixels):
    """Bounding box of every pixel that carries ink."""
    x0, y0, x1, y1 = width, height, -1, -1
    for y in range(height):
        row = y * width * 4
        for x in range(width):
            if pixels[row + x * 4 + 3] > ALPHA_FLOOR:
                if x < x0:
                    x0 = x
                if x > x1:
                    x1 = x
                if y < y0:
                    y0 = y
                if y > y1:
                    y1 = y
    if x1 < 0:
        raise SystemExit("The source image is empty.")
    return x0, y0, x1, y1


def crop(width, height, pixels, box):
    x0, y0, x1, y1 = box
    w, h = x1 - x0 + 1, y1 - y0 + 1
    out = bytearray(w * h * 4)
    for y in range(h):
        src = ((y + y0) * width + x0) * 4
        out[y * w * 4:(y + 1) * w * 4] = pixels[src:src + w * 4]
    return w, h, out


def resize(width, height, pixels, new_width):
    """Box filter on premultiplied alpha, so edges do not pick up a dark halo."""
    new_height = max(1, round(height * new_width / width))
    out = bytearray(new_width * new_height * 4)
    for ny in range(new_height):
        sy0, sy1 = ny * height // new_height, max(ny * height // new_height + 1,
                                                  (ny + 1) * height // new_height)
        for nx in range(new_width):
            sx0, sx1 = nx * width // new_width, max(nx * width // new_width + 1,
                                                    (nx + 1) * width // new_width)
            r = g = b = a = n = 0
            for sy in range(sy0, sy1):
                base = sy * width * 4
                for sx in range(sx0, sx1):
                    i = base + sx * 4
                    pa = pixels[i + 3]
                    r += pixels[i] * pa
                    g += pixels[i + 1] * pa
                    b += pixels[i + 2] * pa
                    a += pa
                    n += 1
            o = (ny * new_width + nx) * 4
            if a:
                out[o] = min(255, r // a)
                out[o + 1] = min(255, g // a)
                out[o + 2] = min(255, b // a)
            out[o + 3] = a // n
    return new_width, new_height, out


def recolor_dark_ink(pixels, to=IVORY):
    """Repaint the achromatic ink, leave the terracotta alone.

    The wordmark is black type plus a terracotta accent. On the dark rail the
    black disappears, so it becomes ivory — but flattening the whole mark (what
    `filter: brightness(0) invert(1)` did) would take the accent with it. Ink is
    classified by chroma, not by luminance, so the antialiased edges follow the
    letter they belong to.
    """
    out = bytearray(pixels)
    swapped = 0
    for i in range(0, len(out), 4):
        if out[i + 3] <= ALPHA_FLOOR:
            continue
        r, g, b = out[i], out[i + 1], out[i + 2]
        if max(r, g, b) - min(r, g, b) <= 24:
            out[i], out[i + 1], out[i + 2] = to
            swapped += 1
    return out, swapped


def main():
    if len(sys.argv) != 2:
        raise SystemExit("usage: python3 data-raw/prep_logo.py <source.png>")
    src = sys.argv[1]

    width, height, pixels = read_png(src)
    print("source        %dx%d" % (width, height))

    box = ink_box(width, height, pixels)
    width, height, pixels = crop(width, height, pixels, box)
    print("ink box       x %d..%d  y %d..%d  -> %dx%d"
          % (box[0], box[2], box[1], box[3], width, height))

    width, height, pixels = resize(width, height, pixels, TARGET_WIDTH)
    print("resized       %dx%d" % (width, height))

    light, swapped = recolor_dark_ink(pixels)
    print("ivory variant %d pixels repainted" % swapped)

    for path in ("inst/app/www/img/observatorio-claro.png",
                 "website/assets/img/observatorio-claro.png"):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        print("wrote %-45s %d bytes" % (path, write_png(path, width, height, light)))


if __name__ == "__main__":
    main()
