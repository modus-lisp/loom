#!/usr/bin/env python3
"""hn-audit.py — deterministic render-quality audit of the Hacker News front page.

For every external story link on the HN front page it renders the page in weft (the
way loom's service does, JS on) and in Chromium (the reference, JS off), then diffs
the two element-geometry dumps keyed by structural DOM path.  Each page gets a
badness score from the structural (width) and positional divergences that survive a
systematic offset; a render that crashes/times out scores the maximum.  The worst
offenders are logged to loom's error sqlite db (kind='render-audit').

No LLM, no interactivity — safe to run from cron:
    python3 inspect/hn-audit.py
Options (env): HN_WIDTH (default 1024), HN_DB (default ../loom-errors.db),
    HN_LOG_TOP (default 15), HN_MIN_SCORE (default 5).
"""
import os, re, sys, json, sqlite3, subprocess, shutil, time, urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
LOOM = os.path.dirname(HERE)
WIDTH = int(os.environ.get("HN_WIDTH", "1024"))
DB = os.environ.get("HN_DB", os.path.join(LOOM, "loom-errors.db"))
LOG_TOP = int(os.environ.get("HN_LOG_TOP", "15"))
MIN_SCORE = float(os.environ.get("HN_MIN_SCORE", "8"))
WORK = os.environ.get("HN_WORK", "/tmp/hn-audit")
FAIL_SCORE = 1000.0
TOL = 8  # px tolerance, matching cmp-diff.py

def log(*a): print(*a, file=sys.stderr, flush=True)

def hn_links():
    """External story links on the HN front page, in order, deduped."""
    req = urllib.request.Request("https://news.ycombinator.com/",
                                 headers={"User-Agent": "Mozilla/5.0 (hn-audit)"})
    html = urllib.request.urlopen(req, timeout=30).read().decode("utf-8", "replace")
    out = []
    for u in re.findall(r'class="titleline"><a href="(http[^"]+)"', html):
        if "news.ycombinator.com" in u:
            continue
        if u not in out:
            out.append(u)
    return out

def load_tsv(path):
    d = {}
    ch = None
    for line in open(path, encoding="utf-8", errors="replace"):
        f = line.rstrip("\n").split("\t")
        if len(f) < 6:
            continue
        if f[0] == "#":                       # trailing metric comment (content-height)
            if f[1] == "content-height":
                try: ch = int(f[2])
                except ValueError: pass
            continue
        try:
            x, y, w, h = (int(s) if s.lstrip("-").isdigit() else 0 for s in f[1:5])
        except ValueError:
            continue
        d[f[0]] = {"x": x, "y": y, "w": w, "h": h, "tx": f[5]}
    return d, ch

def score_page(chrome_path, weft_path):
    """Return (score, components-dict).  Higher score = worse divergence from Chrome."""
    chrome, _ = load_tsv(chrome_path)
    weft, wch = load_tsv(weft_path)
    if not weft:
        return FAIL_SCORE, {"note": "weft produced no boxes", "matched": 0}
    common = sorted(set(chrome) & set(weft))
    if not common:
        return FAIL_SCORE, {"note": "no DOM elements matched Chrome", "matched": 0,
                            "weft_boxes": len(weft), "chrome_boxes": len(chrome)}
    import statistics
    mdx = int(statistics.median(weft[p]["x"] - chrome[p]["x"] for p in common))
    mdy = int(statistics.median(weft[p]["y"] - chrome[p]["y"] for p in common))
    width_bad = pos_bad = 0
    worst = []  # (abs_dw, path, dw)
    for p in common:
        c, w = chrome[p], weft[p]
        dw = w["w"] - c["w"]
        rdx = w["x"] - c["x"] - mdx
        rdy = w["y"] - c["y"] - mdy
        if abs(dw) > max(TOL, 0.12 * max(c["w"], 1)):
            width_bad += 1
            worst.append((abs(dw), "/".join(p.split("/")[-2:]), dw))
        elif abs(rdx) > 3 * TOL or abs(rdy) > 8 * TOL:
            pos_bad += 1
    matched = len(common)
    wr = width_bad / matched
    pr = pos_bad / matched
    # width divergences (collapsed columns, boxes that failed to fill their parent)
    # are the strong structural signal; position drift accumulates down long pages
    # (spacing errors compound) so it is weighted lightly.
    score = round(100 * wr + 25 * pr, 1)
    # a page weft renders far shorter than Chrome likely dropped content
    worst.sort(reverse=True)
    comp = {"score": score, "matched": matched, "width_bad": width_bad,
            "pos_bad": pos_bad, "sys_dx": mdx, "sys_dy": mdy,
            "worst": [{"path": p, "dw": dw} for _, p, dw in worst[:5]]}
    return score, comp

def run_renderers(links, workdir):
    with open(os.path.join(workdir, "links.txt"), "w") as f:
        f.write("\n".join(links) + "\n")
    lf = os.path.join(workdir, "links.txt")
    log(f"[audit] rendering {len(links)} pages in Chromium (reference)…")
    subprocess.run(["node", os.path.join(HERE, "hn-audit-chrome.js"), lf, workdir, str(WIDTH)],
                   timeout=1800, check=False)
    log(f"[audit] rendering {len(links)} pages in weft…")
    subprocess.run(["sbcl", "--dynamic-space-size", "4096", "--script",
                    os.path.join(HERE, "hn-audit-weft.lisp"), lf, workdir, str(WIDTH)],
                   timeout=1800, check=False)

def db_log(rows):
    con = sqlite3.connect(DB, timeout=30)
    con.execute("PRAGMA busy_timeout=30000")
    con.execute("""CREATE TABLE IF NOT EXISTS errors (id INTEGER PRIMARY KEY,
                   ts TEXT DEFAULT CURRENT_TIMESTAMP, kind TEXT, url TEXT, detail TEXT)""")
    con.executemany("INSERT INTO errors (kind,url,detail) VALUES ('render-audit',?,?)",
                    rows)
    con.commit()
    con.close()

def main():
    if os.path.isdir(WORK):
        shutil.rmtree(WORK, ignore_errors=True)
    os.makedirs(WORK, exist_ok=True)
    try:
        links = hn_links()
    except Exception as e:
        log(f"[audit] could not fetch HN front page: {e}")
        return 1
    if not links:
        log("[audit] no links extracted from HN front page")
        return 1
    run_renderers(links, WORK)

    results = []   # (score, url, comp, status)
    for i, url in enumerate(links):
        cp = os.path.join(WORK, f"{i}.chrome.tsv")
        wp = os.path.join(WORK, f"{i}.weft.tsv")
        werr = os.path.join(WORK, f"{i}.weft.err")
        if os.path.exists(werr):
            detail = open(werr, encoding="utf-8", errors="replace").read().strip()[:200]
            results.append((FAIL_SCORE, url, {"note": "weft render failed: " + detail}, "fail"))
            continue
        if not os.path.exists(cp):                 # no reference — can't grade our render
            results.append((-1.0, url, {"note": "chrome reference unavailable"}, "no-ref"))
            continue
        if not os.path.exists(wp):
            results.append((FAIL_SCORE, url, {"note": "weft produced no dump"}, "fail"))
            continue
        score, comp = score_page(cp, wp)
        results.append((score, url, comp, "ok"))

    graded = [r for r in results if r[0] >= 0]
    graded.sort(key=lambda r: r[0], reverse=True)

    # console summary (the cron log)
    log(f"\n[audit] {len(links)} links, {sum(1 for r in results if r[3]=='ok')} graded, "
        f"{sum(1 for r in results if r[3]=='fail')} failed, "
        f"{sum(1 for r in results if r[3]=='no-ref')} no-ref")
    log("[audit] worst offenders:")
    for score, url, comp, status in graded[:LOG_TOP]:
        log(f"  {score:6.1f}  {status:5}  {url[:70]}  {comp.get('note','')}")

    # log worst offenders to the error db
    rows = []
    for rank, (score, url, comp, status) in enumerate(graded[:LOG_TOP], 1):
        if score < MIN_SCORE:
            break
        comp = dict(comp); comp["rank"] = rank; comp["status"] = status; comp["width"] = WIDTH
        rows.append((url, json.dumps(comp, separators=(",", ":"))))
    if rows:
        try:
            db_log(rows)
            log(f"[audit] logged {len(rows)} offenders to {DB}")
        except Exception as e:
            log(f"[audit] db log failed: {e}")
    else:
        log("[audit] nothing above the score threshold to log")
    return 0

if __name__ == "__main__":
    sys.exit(main())
