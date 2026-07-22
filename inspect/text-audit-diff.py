#!/usr/bin/env python3
"""text-audit-diff.py — diff weft's text-layout geometry against Chromium's, keyed
by element id, with a font-metric tolerance calibrated so a correct-but-font-different
render PASSES while a real layout-structure divergence FAILS.

weft substitutes its own font, so absolute glyph advances differ.  The signals we
trust are STRUCTURAL and (mostly) font-independent:

  * LINE COUNT per inline element — the dominant line-breaking / white-space signal.
      Forced structure (br, pre newlines, nowrap) => EXACT.  Natural wrapping can
      legitimately move by one line under a different font, so a +/-1 line delta on a
      naturally-wrapping element is reported as FONT-SOFT, not a failure; >=2 is a bug.
  * ELEMENT BOX x/y after the page's systematic offset — flow position.  Tolerant.
  * ELEMENT HEIGHT — scales with line count; compared with a per-line slack.
  * FIRST-LINE START X relative to the element left — text-indent / center / right / rtl.
      A fixed-px shift (indent) or a directional edge (rtl/right/center) is font-robust.
  * JUSTIFY FILL — non-last lines of a justified block fill ~the content width.
  * LINE ADVANCE (y delta) — line-height; font-independent when line-height is explicit.

Usage: text-audit-diff.py <dir-with .chrome.json/.weft.json> [--verbose]
Exit 0 always; prints a ranked bug map.
"""
import sys, os, json, glob, statistics

DIR = sys.argv[1]
VERBOSE = "--verbose" in sys.argv[2:]

# tolerances
POS_TOL = 6        # px slack on element x/y after systematic offset
H_PER_LINE = 6     # px slack on element height, per line of content
FIRST_X_TOL = 8    # px slack on first-line start-x comparisons
FILL_TOL = 12      # px: a justified non-last line must reach within this of content right

# elements whose wrapping is "natural" (no forced breaks) — a +/-1 line delta is
# font tolerance, not a bug.  We detect forced structure from the recorded text.
def is_forced(chrome_lines):
    # heuristic unused; forced-ness is judged per-test below via test name hints
    return False

def load(path):
    try:
        return json.load(open(path))
    except Exception:
        return None

def line_count(rec):
    L = rec.get("lines")
    return len(L) if L else (1 if rec.get("h", 0) > 0 else 0)

def systematic_offset(chrome, weft, ids):
    dxs, dys = [], []
    for i in ids:
        dxs.append(weft[i]["x"] - chrome[i]["x"])
        dys.append(weft[i]["y"] - chrome[i]["y"])
    return (int(statistics.median(dxs)) if dxs else 0,
            int(statistics.median(dys)) if dys else 0)

# Forced-structure tests: line count is fixed by markup (newlines, <br>, nowrap,
# single word), so a delta of even 1 is a real bug — NOT font tolerance.  Natural
# wrapping tests can legitimately move by one line under a different font, so their
# +/-1 delta is FONT-SOFT.  Enumerated explicitly (substring hints collided, e.g.
# "br" inside "break", "pre" inside "pre-wrap").
FORCED = {
    "10-single-word-fits", "12-ws-nowrap", "13-ws-pre", "16-pre-tab",
    "30-center-short", "31-right-short", "37-pre-block-size", "43-rtl-short",
    "44-empty-p", "45-br-forced", "46-multiple-br",
}

def main():
    tests = sorted({os.path.basename(p).rsplit(".chrome.json", 1)[0]
                    for p in glob.glob(os.path.join(DIR, "*.chrome.json"))})
    findings = []   # (severity, test, id, msg)
    n_tests = 0
    n_pass = 0
    for t in tests:
        cj = load(os.path.join(DIR, t + ".chrome.json"))
        wj = load(os.path.join(DIR, t + ".weft.json"))
        if cj is None or wj is None:
            findings.append((100, t, "-", "MISSING dump (weft error or chrome error)"))
            n_tests += 1
            continue
        n_tests += 1
        forced = t in FORCED
        # directional signals are only meaningful for the alignment they test; on a
        # left-aligned ragged block the line's right gap / start is pure font metric.
        wants_right = ("right" in t) or ("rtl" in t)
        wants_start = ("indent" in t) or ("center" in t) or ("rtl" in t)
        ids = [i for i in cj if i in wj]
        if not ids:
            findings.append((100, t, "-", "no ids matched"))
            continue
        mdx, mdy = systematic_offset(cj, wj, ids)
        test_bad = 0
        for i in ids:
            c, w = cj[i], wj[i]
            # -- line count --
            cc, wc = line_count(c), line_count(w)
            if c.get("lines") is not None:
                d = abs(wc - cc)
                if forced and d >= 1:
                    findings.append((90, t, i, f"LINE COUNT {wc} vs {cc} (forced-structure => exact)"))
                    test_bad += 1
                elif not forced and d >= 2:
                    findings.append((80, t, i, f"LINE COUNT {wc} vs {cc} (delta {d}, beyond font tol)"))
                    test_bad += 1
                elif not forced and d == 1:
                    findings.append((10, t, i, f"line count {wc} vs {cc} (delta 1, FONT-SOFT)"))
            # -- element position (systematic-corrected) --
            rdx = (w["x"] - c["x"]) - mdx
            rdy = (w["y"] - c["y"]) - mdy
            if abs(rdx) > POS_TOL or abs(rdy) > POS_TOL:
                sev = 60 if abs(rdy) > 3 * H_PER_LINE else 40
                findings.append((sev, t, i, f"POSITION dx={rdx} dy={rdy} (corrected; box@{w['x']},{w['y']} vs {c['x']},{c['y']})"))
                test_bad += 1
            # -- element height --
            # A block CONTAINER (no line rects of its own) derives its height from
            # inner wrapping, where a +/-1 line font delta (~one line-height) is
            # tolerable; a leaf inline element is held tighter.
            dh = w["h"] - c["h"]
            if c.get("lines") is None:
                htol = 24
            else:
                htol = H_PER_LINE * max(1, cc) + H_PER_LINE
            if abs(dh) > htol:
                findings.append((55, t, i, f"HEIGHT {w['h']} vs {c['h']} (dh={dh}, lines={cc})"))
                test_bad += 1
            # -- per-line structure (first-line start, fill, advance) --
            cl, wl = c.get("lines"), w.get("lines")
            if cl and wl and len(cl) == len(wl) and len(cl) >= 1:
                # first-line start-x relative to element left (indent / center / rtl)
                if wants_start:
                    c0 = cl[0]["x"] - c["x"]
                    w0 = wl[0]["x"] - w["x"]
                    if abs(w0 - c0) > FIRST_X_TOL + 2:
                        findings.append((50, t, i, f"FIRST-LINE START x rel {w0} vs {c0} (indent/align/rtl)"))
                        test_bad += 1
                # right-edge of first line relative to element right (right-align / rtl)
                if wants_right:
                    cR = (c["x"] + c["w"]) - (cl[0]["x"] + cl[0]["w"])
                    wR = (w["x"] + w["w"]) - (wl[0]["x"] + wl[0]["w"])
                    if abs(wR - cR) > FIRST_X_TOL + 4:
                        findings.append((45, t, i, f"FIRST-LINE RIGHT gap {wR} vs {cR} (right/rtl edge)"))
                        test_bad += 1
                # line advance (2nd - 1st line y): line-height
                if len(cl) >= 2:
                    ca = cl[1]["y"] - cl[0]["y"]
                    wa = wl[1]["y"] - wl[0]["y"]
                    if abs(wa - ca) > 4:
                        findings.append((48, t, i, f"LINE ADVANCE {wa} vs {ca} (line-height)"))
                        test_bad += 1
                # justify fill: for a 'justify' test, non-last lines fill content width
                if "justify" in t and len(cl) >= 2:
                    contentR = c["x"] + c["w"]
                    for k in range(len(wl) - 1):
                        wfill = (wl[k]["x"] + wl[k]["w"])
                        cfill = (cl[k]["x"] + cl[k]["w"])
                        c_fills = (contentR - cfill) <= FILL_TOL
                        w_fills = (contentR + mdx - wfill) <= FILL_TOL + 6
                        if c_fills and not w_fills:
                            findings.append((70, t, i, f"JUSTIFY line {k} not filled: right {wfill} vs content {contentR+mdx}"))
                            test_bad += 1
                            break
        if test_bad == 0:
            n_pass += 1

    # ---- report ----
    findings.sort(key=lambda f: -f[0])
    print(f"\n=== text-layout geometry audit: {n_pass}/{n_tests} tests clean ===\n")
    real = [f for f in findings if f[0] >= 40]
    soft = [f for f in findings if f[0] < 40]
    print(f"REAL divergences ({len(real)}):")
    for sev, t, i, msg in real:
        print(f"  [{sev:3d}] {t:28s} #{i:10s} {msg}")
    if VERBOSE and soft:
        print(f"\nFONT-SOFT / tolerated ({len(soft)}):")
        for sev, t, i, msg in soft:
            print(f"  [{sev:3d}] {t:28s} #{i:10s} {msg}")
    print()

main()
