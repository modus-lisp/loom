// wpt-chrome-shot.js — batch reference screenshotter for the Chrome-referenced
// reftest runner.  Launches Chromium ONCE and, for each entry in the manifest,
// loads a WPT reference file:// URL and writes an 800x600 PNG screenshot (the
// ground-truth pixels the weft test is graded against).
//
// Manifest: one entry per line, TAB-separated:  <js:0|1>\t<abs-ref-path>\t<out-png>
// A ref that fails to load writes no PNG and logs to stderr (the Lisp side then
// buckets that test as :error rather than crediting a false pass).
//
// Args: <manifest-file> [width] [height].  No LLM; cron-safe.
const { chromium } = require('/home/claude/pw/node_modules/playwright');
const fs = require('fs');

const manifest = process.argv[2];
const W = parseInt(process.argv[3] || '800', 10);
const H = parseInt(process.argv[4] || '600', 10);

const entries = fs.readFileSync(manifest, 'utf-8')
  .split('\n').map(s => s.trim()).filter(Boolean)
  .map(line => { const [js, path, out] = line.split('\t'); return { js: js === '1', path, out }; });

(async () => {
  const browser = await chromium.launch({
    executablePath: '/home/claude/.cache/ms-playwright/chromium-1148/chrome-linux/chrome',
    args: ['--force-color-profile=srgb', '--disable-lcd-text'],
  });
  let ok = 0, fail = 0;
  for (let i = 0; i < entries.length; i++) {
    const { js, path, out } = entries[i];
    let ctx, page;
    try {
      ctx = await browser.newContext({
        viewport: { width: W, height: H },
        deviceScaleFactor: 1,
        javaScriptEnabled: js,
      });
      page = await ctx.newPage();
      const url = 'file://' + path;
      await page.goto(url, { waitUntil: 'load', timeout: 30000 });
      // WPT reftest-wait: the ref may build itself and drop the class when ready.
      if (js) {
        try {
          await page.waitForFunction(
            () => !document.documentElement.classList.contains('reftest-wait'),
            { timeout: 3000 });
        } catch (_) { /* not a reftest-wait page, or it never cleared — screenshot as-is */ }
      }
      await page.screenshot({ path: out, clip: { x: 0, y: 0, width: W, height: H } });
      ok++;
      process.stderr.write(`[shot] ${i} ok ${path}\n`);
    } catch (e) {
      fail++;
      process.stderr.write(`[shot] ${i} FAIL ${path} :: ${(e && e.message) || e}\n`);
    } finally {
      try { if (ctx) await ctx.close(); } catch (_) {}
    }
  }
  await browser.close();
  process.stderr.write(`[shot] done: ${ok} ok, ${fail} fail\n`);
})();
