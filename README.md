# loom

**loom** is the interactive browser shell for [**weft**](../weft), a web engine
written in pure Common Lisp. weft does the whole pipeline — fetch, HTML parse,
CSS cascade, JavaScript, layout, paint to an RGB8 canvas. loom is the *platform
shell*: it opens a native window over SDL2, blits weft's painted canvas into it,
and translates window input into the DOM events weft already dispatches.

The SDL2 binding is a small **hand-written CFFI layer** (`src/sdl-ffi.lisp`) over
the dozen SDL functions loom actually calls — *not* cl-sdl2. cl-sdl2 uses
cl-autowrap, which shells out to [c2ffi](https://github.com/rpav/c2ffi) (an LLVM
tool) at build time to regenerate its bindings from the SDL headers; c2ffi is
painful to install, especially on macOS. So loom's only dependencies are `cffi`
(pure Quicklisp) plus the SDL2 shared library — no c2ffi, no autowrap.

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
brew install sdl2                     # the SDL2 native library (the ONLY C dep)
# a Common Lisp with Quicklisp (SBCL recommended):
#   brew install sbcl
#   then install Quicklisp: https://www.quicklisp.org/beta/
# Quicklisp pulls in cffi automatically — no c2ffi / cl-autowrap / cl-sdl2 needed.
#
# Homebrew installs libSDL2 under /opt/homebrew/lib (Apple Silicon) or
# /usr/local/lib (Intel); loom adds both to the CFFI search path, so no
# DYLD_LIBRARY_PATH juggling is required.

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
`loom:run` runs the window loop on the thread it is called from, so you must call
it on the initial/main thread: an `sbcl --eval` invocation (or `./run.sh`) runs
on the process main thread, as does the REPL's initial thread. If you spawn loom
from a *different* thread — a `bordeaux-threads` worker, or a SLIME/Swank
evaluation thread — you will get a silent failure or a Cocoa abort. Always drive
`loom:run` from the main thread (the `sbcl --eval` / `run.sh` path).

On Linux/Windows the same entry point works (SDL runs its loop on the calling
thread); the main-thread requirement is macOS-specific.

## How it's built

| file | role |
|------|------|
| `src/sdl-ffi.lisp` | hand-written CFFI bindings to libSDL2 (**the only FFI**): window, renderer, streaming texture, event union decoding |
| `src/input.lisp` | pure SDL→DOM translation + scroll/URL math (no SDL) |
| `src/page.lisp`  | the persistent page model: load, render, hit-test, dispatch, scroll, navigate (no SDL) |
| `src/shell.lisp` | the SDL glue: window, renderer, streaming texture, blit, event loop |
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

## A second backend: glass (VNC, no SDL, no X)

The page model (`page.lisp`) is display-agnostic — `shell.lisp` is just the SDL
driver over it. The optional **`loom/glass`** system is a *second* driver that
serves the same page model over VNC through
[**glass**](https://github.com/modus-lisp/glass), a pure-Common-Lisp framebuffer
+ RFB server. weft paints to a row-major RGB8 canvas; glass ships `0x00RRGGBB`
framebuffers; the glass shell packs one into the other and turns RFB pointer/key
events into the very same `mouse-press` / `mouse-wheel` / `key-down` page-model
calls. **This backend has no FFI at all** — with it, the whole weaving stack
(scribe / shuttle / gesso / stencil / weft / loom) is FFI-free end to end; point
any VNC client at it.

```lisp
(ql:quickload :loom/glass)
(loom.glass:run-glass :start "https://example.com" :port 5900)   ; then open a VNC viewer on :5900
;; :background t returns immediately (server + pump run in threads)
```

`inspect/glass-demo.lisp` closes the loop headlessly: it serves the bundled home
page over VNC, sends a real RFB click on a link, and confirms the page navigates
and re-renders — all with an in-process RFB client, no display required. (weft's
JS context is single-threaded, so the glass shell serialises the RFB client
thread against the timer/repaint pump with one mutex.)

## Status / not yet

- Editable text, form controls, focus and a text caret are a later round —
  `keydown` / `keypress` are dispatched to the body as a thin start.
- CSS `:hover` restyling (recascading on hover) is deferred; JS hover handlers
  (`mouseover` / `mouseout`) and the pointer cursor already work.

MIT licensed. The engine it drives (weft and siblings) is pure Common Lisp.
