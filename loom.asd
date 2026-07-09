;;;; loom.asd — the interactive browser shell for weft.
;;;;
;;;; loom is the platform shell: it opens a native window over SDL2, blits weft's
;;;; painted canvas into it, and translates SDL input into the DOM events weft
;;;; already dispatches.  This is the ONE place FFI lives — the engine stack
;;;; (scribe/shuttle/gesso/stencil/weft) stays pure Common Lisp.
;;;;
;;;; SDL2 is bound with hand-written CFFI (src/sdl-ffi.lisp), NOT cl-sdl2: that
;;;; library uses cl-autowrap, which shells out to c2ffi (an LLVM tool) at build
;;;; time — hard to install, especially on macOS.  So loom depends only on cffi
;;;; (pure Quicklisp) plus the SDL2 shared library (brew install sdl2).
(defsystem "loom"
  :description "An interactive browser shell that turns the weft web engine into a
                real, browsable native window (SDL2 window + input -> weft render
                + DOM events).  FFI lives here only; the engine stays pure CL."
  :version "0.0.1" :author "ynniv" :license "MIT"
  :depends-on ("weft/render" "weft/script" "weft/fetch" "seal" "cffi")
  :serial t
  :components ((:module "src" :serial t
                :components ((:file "packages")
                             (:file "sdl-ffi")   ; hand-written CFFI bindings to libSDL2 (the ONLY FFI)
                             (:file "input")     ; pure SDL->DOM + scroll/url math
                             (:file "page")      ; persistent page model (no SDL)
                             (:file "shell")     ; SDL glue: window/renderer/texture/loop
                             (:file "main"))))    ; run entrypoint + CLI
  :in-order-to ((test-op (test-op "loom/test"))))

(defsystem "loom/test"
  :description "Headless tests for loom: the pure logic (hit-testing, SDL->DOM
                translation, scroll math, URL resolution) and the page model
                (load -> render -> dispatch a click -> observe the DOM react)."
  :depends-on ("loom")
  :components ((:module "inspect" :components ((:file "tests"))))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :loom.test :run)
               (error "loom: test failures"))))
