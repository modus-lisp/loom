# loom

**loom** is the interactive browser shell for [**weft**](../weft), a web engine
written in pure Common Lisp. weft does the whole pipeline — fetch, HTML parse,
CSS cascade, JavaScript, layout, paint to an RGB8 canvas. loom is the *shell*: it
holds a persistent page model (load → render → dispatch DOM events → re-render)
and drives it onto a screen through [**glass**](https://github.com/modus-lisp/glass),
a pure-Common-Lisp framebuffer + VNC (RFB) server.

**The whole stack is FFI-free.** weft paints a row-major RGB8 canvas; glass ships
`0x00RRGGBB` framebuffers over RFB; loom's glass driver packs one into the other
and turns RFB pointer/key events into DOM events weft already dispatches. No SDL,
no X, no C library — the entire weaving stack (scribe / shuttle / gesso / stencil
/ weft / loom) is pure Common Lisp end to end. Point any VNC client at it.

```
   ┌────────── loom (this repo) ──────────┐        ┌──── weft (pure CL) ────┐
   │  page model · input · glass driver    │  <-->  │  fetch → HTML → CSS →   │
   │  (pure Common Lisp — no FFI)          │        │  JS → layout → paint    │
   └───────────────────────────────────────┘        └─────────────────────────┘
              │  glass framebuffer + RFB (VNC)
              ▼
        any VNC client
```

## What it does

- **Live display over VNC.** weft renders to a tall RGB8 canvas; loom packs the
  visible slice (at the current scroll offset) into a glass framebuffer, and
  glass ships only the dirty tiles to any connected VNC client.
- **Browse.** Mouse-wheel scrolling pans the page canvas. Clicking an `<a href>`
  resolves the URL against the page base, fetches it through weft, and renders a
  fresh scripted page in place.
- **Live JavaScript.** Hit-testing maps a pixel to the deepest DOM node (inline
  links/buttons included); RFB pointer/key events become trusted DOM events
  routed through weft's `dispatchEvent` (`mousedown` / `mouseup` / `click`,
  `mousemove` / `mouseover` / `mouseout` with a hover cursor, `keydown`). weft's
  timer/macrotask clock is pumped each frame, so `setTimeout` animations run. A
  page with an `onclick` or hover handler visibly reacts.

## Running it

```sh
# a Common Lisp with Quicklisp (SBCL recommended), plus weft + its engine
# siblings + glass visible to ASDF/Quicklisp, e.g.
#   ln -s /path/to/loom  ~/quicklisp/local-projects/loom
#   ln -s /path/to/weft  ~/quicklisp/local-projects/weft   (and scribe/shuttle/gesso/stencil)
#   ln -s /path/to/glass ~/quicklisp/local-projects/glass

sbcl --control-stack-size 256 --dynamic-space-size 4096 \
     --eval '(ql:quickload :loom/glass)' \
     --eval '(loom.glass:run-glass :port 5900)'            # the bundled home page
# or a URL / local file:
sbcl --control-stack-size 256 --dynamic-space-size 4096 \
     --eval '(ql:quickload :loom/glass)' \
     --eval '(loom.glass:run-glass :start "https://example.com" :port 5900)'
```

Then point a VNC viewer at `localhost:5900`. `:background t` returns immediately
(the RFB server and repaint pump run in their own threads).

Or with the helper script:

```sh
./run.sh                              # bundled home page, VNC on :5900
./run.sh https://example.com          # a URL
./run.sh path/to/page.html            # a local file
./run.sh https://example.com 5901     # a URL on a chosen VNC port
```

## How it's built

| file | role |
|------|------|
| `src/page.lisp`  | the persistent page model: load, render, hit-test, dispatch, scroll, navigate — display-agnostic |
| `src/input.lisp` | pure pointer/wheel → DOM translation + scroll / URL math |
| `src/main.lisp`  | start-page + `file://` → path helpers a driver needs to open its first page |
| `src/glass-shell.lisp` | the glass driver: pack weft's canvas into a glass framebuffer, wire RFB input to the page model, run the repaint/timer pump (`loom/glass` system) |

The correctness-critical logic lives in `input.lisp` + `page.lisp` and is fully
unit-tested headlessly (`inspect/tests.lisp`): hit-testing, event translation,
scroll clamping, URL resolution, and a full load → click → observe-the-DOM-react
cycle. `inspect/smoke.lisp` exercises the glass display path end-to-end without a
VNC client (attach a page to a framebuffer, pump the loop, write the render PNG).

weft's JS context is single-threaded, so the glass driver serialises the RFB
client thread against the timer/repaint pump with one mutex.

## Tests

```sh
# headless unit tests (pure logic + page model; no display needed)
sbcl --eval '(ql:quickload :loom/test)' --eval '(uiop:quit (if (loom.test:run) 0 1))'

# glass smoke test (load a real render, attach to a framebuffer, pump, write a PNG)
sbcl --script inspect/smoke.lisp
```

`inspect/glass-demo.lisp` closes the loop headlessly: it serves the bundled home
page over VNC, sends a real RFB click on a link with an in-process RFB client, and
confirms the page navigates and re-renders — no display required.

## Status / not yet

- Editable text, form controls, focus and a text caret are a later round —
  `keydown` is dispatched to the body as a thin start.
- CSS `:hover` restyling (recascading on hover) is deferred; JS hover handlers
  (`mouseover` / `mouseout`) and the pointer cursor already work.

MIT licensed. The engine it drives (weft and siblings) is pure Common Lisp.
