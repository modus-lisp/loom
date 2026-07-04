;;;; loom.asd — the interactive browser shell for weft.
;;;;
;;;; loom is the platform shell: it opens a native window with cl-sdl2 (SDL2 via
;;;; CFFI), blits weft's painted canvas into it, and translates SDL input into the
;;;; DOM events weft already dispatches.  This is the ONE place FFI lives — the
;;;; engine stack (scribe/shuttle/gesso/stencil/weft) stays pure Common Lisp.
(defsystem "loom"
  :description "An interactive browser shell that turns the weft web engine into a
                real, browsable native window (SDL2 window + input -> weft render
                + DOM events).  FFI lives here only; the engine stays pure CL."
  :version "0.0.1" :author "ynniv" :license "MIT"
  :depends-on ("weft/render" "weft/script" "weft/fetch" "sdl2")
  :serial t
  :components ((:module "src" :serial t
                :components ((:file "packages")
                             (:file "input")     ; pure SDL->DOM + scroll/url math
                             (:file "page")      ; persistent page model (no SDL)
                             (:file "shell")     ; SDL glue: window/renderer/texture/loop (FFI)
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
