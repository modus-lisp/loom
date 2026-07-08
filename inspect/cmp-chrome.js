// cmp-chrome.js — dump every visible element's box geometry from Chromium (the
// reference engine) as TSV: path<TAB>x<TAB>y<TAB>w<TAB>h<TAB>text.  The path is a
// structural tag:nth-of-type chain from the root, so it matches the weft dump for
// the same HTML.  JS is disabled so the SSR markup is laid out with real CSS — the
// same input weft renders.  Usage: node cmp-chrome.js <url> [width]
const { chromium } = require('/home/claude/pw/node_modules/playwright');
(async () => {
  const url = process.argv[2];
  const W = parseInt(process.argv[3] || '1024', 10);
  const browser = await chromium.launch({
    executablePath: '/home/claude/.cache/ms-playwright/chromium-1148/chrome-linux/chrome',
  });
  const ctx = await browser.newContext({ viewport: { width: W, height: 900 }, javaScriptEnabled: false });
  const page = await ctx.newPage();
  await page.goto(url, { waitUntil: 'load', timeout: 60000 });
  const rows = await page.evaluate(() => {
    function pathOf(el) {
      const parts = [];
      while (el && el.nodeType === 1 && el.tagName !== 'HTML') {
        const p = el.parentElement;
        if (!p) break;
        const same = [...p.children].filter(c => c.tagName === el.tagName);
        parts.unshift(el.tagName.toLowerCase() + ':' + (same.indexOf(el) + 1));
        el = p;
      }
      return parts.join('/');
    }
    const out = [];
    for (const el of document.querySelectorAll('*')) {
      const cs = getComputedStyle(el);
      if (cs.display === 'none' || cs.visibility === 'hidden') continue;
      const r = el.getBoundingClientRect();
      if (r.width === 0 && r.height === 0) continue;
      const tx = (el.textContent || '').trim().replace(/\s+/g, ' ').slice(0, 25);
      out.push([pathOf(el), Math.round(r.x), Math.round(r.y + window.scrollY),
                Math.round(r.width), Math.round(r.height), tx].join('\t'));
    }
    return out;
  });
  process.stdout.write(rows.join('\n') + '\n');
  await browser.close();
})();
