;;;; loom.asd — the interactive browser shell for weft.
;;;;
;;;; loom turns the weft web engine into a real, browsable session: it holds a
;;;; persistent page model (load -> render -> dispatch DOM events -> re-render)
;;;; and drives it through a display backend.  The core (this "loom" system) is
;;;; display-agnostic and FFI-free pure Common Lisp; the backend that puts it on
;;;; a screen is loom/glass — a glass framebuffer served over VNC (RFB input, no
;;;; SDL and no X), keeping the whole stack (scribe/shuttle/gesso/stencil/weft)
;;;; free of any C FFI.
(defsystem "loom"
  :description "The display-agnostic browser core for weft: a persistent page
                model with input translation and start-page helpers, driven by a
                display backend (loom/glass).  Pure Common Lisp, no FFI."
  :version "0.0.1" :author "ynniv" :license "MIT"
  :depends-on ("weft/render" "weft/script" "weft/fetch" "seal" "folio")
  :serial t
  :components ((:module "src" :serial t
                :components ((:file "packages")
                             (:file "input")     ; pure pointer/wheel -> DOM + scroll/url math
                             (:file "page")      ; persistent page model
                             (:file "main"))))    ; start-page + file-URL helpers
  :in-order-to ((test-op (test-op "loom/test"))))

;;; The display backend: drive the page model over VNC via glass (pure-CL
;;; framebuffer + RFB), with no SDL and no X.  This is loom's everyday driver.
(defsystem "loom/glass"
  :description "loom's display backend: serve weft over VNC (pure-CL framebuffer
                + RFB input) with no SDL/X — the FFI-free display path.  This is
                the everyday-driver UI (loom.glass:run-glass)."
  :version "0.0.1" :author "ynniv" :license "MIT"
  :depends-on ("loom" "glass" "glass/text")
  :components ((:module "src" :serial t
                :components ((:file "ui")            ; immediate-mode widget kit over glass
                             (:file "nav")           ; navigation tree (nodes / cursor / branching)
                             (:file "glass-shell"))))) ; the browser shell (chrome + input + pump)

(defsystem "loom/test"
  :description "Headless tests for loom: the pure logic (hit-testing, pointer->DOM
                translation, scroll math, URL resolution), the page model
                (load -> render -> dispatch a click -> observe the DOM react),
                and the form-interaction gate — click/type at pixel coordinates
                and assert on both the DOM and what got painted."
  :depends-on ("loom")
  :components ((:module "inspect" :components ((:file "tests") (:file "forms-interact"))))
  :perform (test-op (o c)
             ;; run BOTH, then decide: a failure in one should not hide the
             ;; other's result, which is the whole point of having two gates.
             (let* ((core  (uiop:symbol-call :loom.test :run))
                    (forms (uiop:symbol-call :loom.forms-interact :run)))
               (unless (and core forms) (error "loom: test failures")))))
