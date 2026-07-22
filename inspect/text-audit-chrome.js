// text-audit-chrome.js — reference geometry dumper for the text-layout audit.
// For each local test HTML file it dumps, keyed by element id: the element box
// (getBoundingClientRect) AND the per-LINE rects of its inline content
// (Range.selectNodeContents(el).getClientRects(), merged by top) — the per-line
// signal that reveals line-breaking / white-space / justify / rtl behaviour.
// Output: <outdir>/<basename>.chrome.json.  Args: <files-file> <outdir> <width>.
const { chromium } = require('/home/claude/pw/node_modules/playwright');
const fs = require('fs');
const path = require('path');

const filesFile = process.argv[2];
const outdir = process.argv[3];
const W = parseInt(process.argv[4] || '400', 10);
const files = fs.readFileSync(filesFile, 'utf-8').split('\n').map(s => s.trim()).filter(Boolean);

const DUMP = () => {
  // an element is a "line container" if it has NO block-level element children
  // (only inline content / text) — then selectNodeContents yields one rect per line.
  function hasBlockChild(el) {
    for (const c of el.children) {
      const d = getComputedStyle(c).display;
      if (d === 'block' || d === 'list-item' || d === 'flex' || d === 'grid' ||
          d === 'table' || c.tagName === 'BR') return true;
    }
    return false;
  }
  function lineRects(el) {
    const r = document.createRange();
    r.selectNodeContents(el);
    const rects = [...r.getClientRects()].filter(q => q.width > 0 || q.height > 0);
    // merge rects sharing (approximately) the same top into one logical line
    const lines = [];
    for (const q of rects) {
      let hit = null;
      for (const L of lines) if (Math.abs(L.top - q.top) <= 3) { hit = L; break; }
      if (hit) {
        hit.left = Math.min(hit.left, q.left);
        hit.right = Math.max(hit.right, q.right);
        hit.top = Math.min(hit.top, q.top);
        hit.bottom = Math.max(hit.bottom, q.bottom);
      } else {
        lines.push({ left: q.left, right: q.right, top: q.top, bottom: q.bottom });
      }
    }
    lines.sort((a, b) => a.top - b.top);
    return lines.map(L => ({
      x: Math.round(L.left), y: Math.round(L.top + window.scrollY),
      w: Math.round(L.right - L.left), h: Math.round(L.bottom - L.top)
    }));
  }
  const out = {};
  for (const el of document.querySelectorAll('[id]')) {
    const cs = getComputedStyle(el);
    if (cs.display === 'none') continue;
    const r = el.getBoundingClientRect();
    const rec = {
      x: Math.round(r.x), y: Math.round(r.y + window.scrollY),
      w: Math.round(r.width), h: Math.round(r.height),
      lines: null
    };
    if (!hasBlockChild(el)) {
      const L = lineRects(el);
      if (L.length) rec.lines = L;
    }
    out[el.id] = rec;
  }
  return JSON.stringify(out);
};

(async () => {
  const browser = await chromium.launch({
    executablePath: '/home/claude/.cache/ms-playwright/chromium-1148/chrome-linux/chrome',
  });
  for (const file of files) {
    const base = path.basename(file).replace(/\.html?$/, '');
    let ctx, page;
    try {
      ctx = await browser.newContext({ viewport: { width: W, height: 900 }, javaScriptEnabled: false });
      page = await ctx.newPage();
      await page.goto('file://' + path.resolve(file), { waitUntil: 'load', timeout: 30000 });
      const json = await page.evaluate(DUMP);
      fs.writeFileSync(`${outdir}/${base}.chrome.json`, json);
      process.stderr.write(`[chrome] ${base} ok\n`);
    } catch (e) {
      fs.writeFileSync(`${outdir}/${base}.chrome.err`, String(e && e.message || e) + '\n');
      process.stderr.write(`[chrome] ${base} FAIL ${e && e.message || e}\n`);
    } finally {
      try { if (ctx) await ctx.close(); } catch (_) {}
    }
  }
  await browser.close();
})();
