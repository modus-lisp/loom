// hn-audit-chrome.js — batch reference geometry dumper for the HN render audit.
// Launches Chromium once and, for each URL in the links file, dumps every visible
// element's box geometry (JS off, so the SSR markup is laid out with real CSS) as
// TSV to <outdir>/<index>.chrome.tsv.  A page that fails writes <index>.chrome.err.
// Args: <links-file> <outdir> <width>.  No LLM, cron-safe.
const { chromium } = require('/home/claude/pw/node_modules/playwright');
const fs = require('fs');

const linksFile = process.argv[2];
const outdir = process.argv[3];
const W = parseInt(process.argv[4] || '1024', 10);
const urls = fs.readFileSync(linksFile, 'utf-8').split('\n').map(s => s.trim()).filter(Boolean);

const DUMP = () => {
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
  return out.join('\n') + '\n';
};

(async () => {
  const browser = await chromium.launch({
    executablePath: '/home/claude/.cache/ms-playwright/chromium-1148/chrome-linux/chrome',
  });
  for (let i = 0; i < urls.length; i++) {
    let ctx, page;
    try {
      ctx = await browser.newContext({ viewport: { width: W, height: 900 }, javaScriptEnabled: false });
      page = await ctx.newPage();
      await page.goto(urls[i], { waitUntil: 'load', timeout: 45000 });
      const tsv = await page.evaluate(DUMP);
      fs.writeFileSync(`${outdir}/${i}.chrome.tsv`, tsv);
      process.stderr.write(`[chrome] ${i} ok\n`);
    } catch (e) {
      fs.writeFileSync(`${outdir}/${i}.chrome.err`, String(e && e.message || e) + '\n');
      process.stderr.write(`[chrome] ${i} FAIL ${e && e.message || e}\n`);
    } finally {
      try { if (ctx) await ctx.close(); } catch (_) {}
    }
  }
  await browser.close();
})();
