# loom

**loom** is the interactive browser shell for [**weft**](../weft), a web engine
written in pure Common Lisp. weft does the whole pipeline — fetch, HTML parse,
CSS cascade, JavaScript, layout, paint to an RGB8 canvas. loom is the *platform
shell*: it opens a native window with [cl-sdl2](https://github.com/lispgames/cl-sdl2)
(SDL2 via CFFI), blits weft's painted canvas into it, and translates window input
into the DOM events weft already dispatches.

**loom is the one place FFI is allowed.** The engine stack
(scribe / shuttle / gesso / stencil / weft) stays pure Common Lisp; only the
shell talks to SDL.

```
   ┌────────── loom (this repo) ──────────┐        ┌──── weft (pure CL) ────┐
   │  SDL2 window · input · texture blit   │  <-->  │  fetch → HTML → CSS →   │
   │  (the ONLY FFI in the whole stack)    │        │  JS → layout → paint    │
   └───────────────────────────────────────┘        └─────────────────────────┘
```

## What it does

- **v0 — window + live blit.** Opens an SDL window (default 1024×768) with a
  renderer and a streaming RGB24 texture matching weft's canvas. Each frame it
  copies weft's row-major RGB8 pixels straight into the texture (zero-copy: SDL
  reads the canvas at the scroll offset), then `render-copy` / `render-present`.
  Resizing the window re-lays out weft at the new width.
- **v1 — browse.** Mouse-wheel scrolling (blit a translated slice of the tall
  page canvas). Clicking an `<a href>` resolves the URL against the page base,
  fetches it through weft, and renders a fresh scripted page.
- **v2 — live JavaScript.** Hit-testing maps a pixel to the deepest DOM node
  (including inline links/buttons); SDL mouse/keyboard events become trusted DOM
  events routed through weft's existing `dispatchEvent` (`mousedown` / `mouseup`
  / `click`, `mousemove` / `mouseover` / `mouseout` with hover cursor,
  `keydown` / `keypress`). weft's timer/macrotask clock is pumped one frame per
  loop iteration, so `setTimeout` animations run. A page with an `onclick` or
  hover handler visibly reacts.

## Running it (macOS)

```sh
brew install sdl2                     # the SDL2 native library
# a Common Lisp with Quicklisp (SBCL recommended):
#   brew install sbcl
#   then install Quicklisp: https://www.quicklisp.org/beta/

# make weft + its engine siblings and loom visible to ASDF/Quicklisp, e.g.
#   ln -s /path/to/loom ~/quicklisp/local-projects/loom
#   ln -s /path/to/weft ~/quicklisp/local-projects/weft   (and scribe/shuttle/gesso/stencil)

sbcl --control-stack-size 256 --dynamic-space-size 4096 --eval '(ql:quickload :loom)' \
     --eval '(loom:run)'                                   # the bundled home page
# or a URL / local file:
sbcl --control-stack-size 256 --dynamic-space-size 4096 --eval '(ql:quickload :loom)' \
     --eval '(loom:run :start "https://example.com")'
```

Or with the helper script:

```sh
./run.sh                          # bundled home page
./run.sh https://example.com      # a URL
./run.sh path/to/page.html        # a local file
```

### macOS main-thread note (the #1 gotcha)

Cocoa requires the window and event loop to run on the **process main thread**.
`loom:run` handles this: it enters SDL through `sdl2:make-this-thread-main`, which
designates the calling thread as SDL's main thread and runs the window loop there.
As long as you call `(loom:run)` from the REPL's initial thread (or a
`sbcl --eval` invocation, which runs on the main thread), the window comes up
correctly. If you spawn loom from a *different* thread you will get a silent
failure or a Cocoa abort — always drive `loom:run` from the main thread.

On Linux/Windows the same entry point works (SDL runs its loop on the calling
thread); the main-thread requirement is macOS-specific.

## How it's built

| file | role |
|------|------|
| `src/input.lisp` | pure SDL→DOM translation + scroll/URL math (no SDL) |
| `src/page.lisp`  | the persistent page model: load, render, hit-test, dispatch, scroll, navigate (no SDL) |
| `src/shell.lisp` | the SDL glue: window, renderer, streaming texture, blit, event loop (**the only FFI**) |
| `src/main.lisp`  | `loom:run` / `loom:main` — the macOS-safe entry point + CLI |

The correctness-critical logic lives in `input.lisp` + `page.lisp` and is fully
unit-tested headlessly (`inspect/tests.lisp`): hit-testing, event translation,
scroll clamping, URL resolution, and a full load → click → observe-the-DOM-react
cycle. `inspect/smoke.lisp` exercises the SDL FFI path end-to-end under the dummy
video driver.

## Tests

```sh
# headless unit tests (pure logic + page model; no display needed)
sbcl --eval '(ql:quickload :loom/test)' --eval '(uiop:quit (if (loom.test:run) 0 1))'

# SDL smoke test under the dummy driver (init window+texture, blit a real render)
SDL_VIDEODRIVER=dummy sbcl --script inspect/smoke.lisp
```

## Status / not yet

- Editable text, form controls, focus and a text caret are a later round —
  `keydown` / `keypress` are dispatched to the body as a thin start.
- CSS `:hover` restyling (recascading on hover) is deferred; JS hover handlers
  (`mouseover` / `mouseout`) and the pointer cursor already work.

MIT licensed. The engine it drives (weft and siblings) is pure Common Lisp.
