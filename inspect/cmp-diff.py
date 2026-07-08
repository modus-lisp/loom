#!/usr/bin/env python3
"""cmp-diff.py — diff two element-geometry TSV dumps (Chromium vs weft) keyed by the
structural DOM path.  Reports the systematic offset (e.g. a uniform centering shift),
then per-element divergences that survive it — sorted by width difference, which is
the strongest structural signal (a collapsed grid column, an un-hidden menu, a box
that failed to fill its parent).  Font-metric drift on text runs is expected and
filtered out.  Usage: cmp-diff.py <chrome.tsv> <weft.tsv> [width]"""
import sys, statistics

def load(path):
    d = {}
    for line in open(path, encoding="utf-8", errors="replace"):
        f = line.rstrip("\n").split("\t")
        if len(f) < 6:
            continue
        p, x, y, w, h, tx = f[0], *map(lambda s: int(s) if s.lstrip("-").isdigit() else 0, f[1:5]), f[5]
        d[p] = {"x": x, "y": y, "w": w, "h": h, "tx": tx}
    return d

chrome = load(sys.argv[1])
weft = load(sys.argv[2])
common = sorted(set(chrome) & set(weft))
c_only = set(chrome) - set(weft)
w_only = set(weft) - set(chrome)

if not common:
    print("no elements matched between the two dumps")
    sys.exit(0)

mdx = int(statistics.median(weft[p]["x"] - chrome[p]["x"] for p in common))
mdy = int(statistics.median(weft[p]["y"] - chrome[p]["y"] for p in common))

TOL = 8
width_bad, pos_bad = [], 0
for p in common:
    c, w = chrome[p], weft[p]
    rdx = w["x"] - c["x"] - mdx
    rdy = w["y"] - c["y"] - mdy
    dw = w["w"] - c["w"]
    dh = w["h"] - c["h"]
    if abs(dw) > max(TOL, 0.12 * max(c["w"], 1)):
        width_bad.append((abs(dw), p, c, w, rdx, dw, dh))
    # a correctly-sized box that is still mis-placed after the systematic shift.  (dy
    # accumulates down the page as spacing errors compound, so weight x more.)
    elif abs(rdx) > 3 * TOL or abs(rdy) > 8 * TOL:
        pos_bad += 1
width_bad.sort(reverse=True)

n = len(common)
print(f"matched {n} block elements")
print(f"chrome-only paths: {len(c_only)} (mostly inline — weft dumps block boxes)   weft-only: {len(w_only)}")
print(f"systematic offset: dx={mdx} dy={mdy}   (a large dy => a tall region weft sizes differently)")
print(f"width divergences: {len(width_bad)}   position-only divergences: {pos_bad}")
if width_bad:
    print(f"\n--- {min(len(width_bad), 25)} widest divergences (structural signal) ---")
    for _, p, c, w, rdx, dw, dh in width_bad[:25]:
        short = "/".join(p.split("/")[-3:])
        print(f"  w={w['w']:>5} vs c={c['w']:>5} (dw={dw:+5})  dx={rdx:+5} dh={dh:+6}  {short}  '{c['tx']}'")
