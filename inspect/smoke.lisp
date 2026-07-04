;;;; inspect/smoke.lisp — headless SDL smoke test (SDL_VIDEODRIVER=dummy).
;;;;
;;;; Proves the shell's FFI path end to end without a display: initialize SDL,
;;;; create the window + renderer + streaming texture, load a real weft render,
;;;; upload it into the texture (the zero-copy blit), and run several event-loop
;;;; iterations without crashing.  Also writes the blitted canvas to a PNG so the
;;;; render that feeds the texture can be eyeballed.
;;;;
;;;;   SDL_VIDEODRIVER=dummy sbcl --script inspect/smoke.lisp
(require "asdf")
;; --script skips ~/.sbclrc, so bootstrap quicklisp explicitly for the cffi dep.
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(funcall (read-from-string "ql:quickload") :loom :silent t)

(in-package #:loom)

(defun smoke (&key (out "/home/claude/loom/inspect/smoke.png"))
  (sdl:init)
  (unwind-protect
   (progn
    (format t "~&SDL initialized (driver: ~a)~%"
            (or (uiop:getenv "SDL_VIDEODRIVER") "default"))
    (let* ((home (namestring (default-home)))
           (page (load-file home :width 1024 :viewport-height 768)))
      (format t "loaded ~a: canvas ~dx~d, title ~s~%"
              home (r:canvas-width (page-canvas page)) (r:canvas-height (page-canvas page))
              (page-title page))
      ;; write the exact pixels that get blitted, as proof of the render/blit source
      (r:write-png (page-canvas page) out)
      (format t "wrote blit source PNG -> ~a~%" out)
      ;; exercise the input translation through the page model (as the loop does)
      (mouse-move page 40 60)
      (mouse-wheel page -6)             ; scroll down a few notches
      (format t "input applied: scroll-y ~d, cursor ~a~%"
              (page-scroll-y page) (page-cursor page))
      ;; run the real shell loop, bounded, under the dummy driver: it creates the
      ;; window + renderer + streaming texture and blits the (scrolled) frame N
      ;; times, then tears the window down — all through the SDL FFI path.
      (let ((app (run-shell page :width 1024 :height 768 :max-iterations 8)))
        (format t "shell ran: texture ~dx~d, 8 iterations, final scroll-y ~d~%"
                (app-tex-w app) (app-tex-h app) (page-scroll-y (app-page app))))
      (format t "SMOKE-OK~%")))
   (sdl:quit)))

(handler-case (smoke)
  (error (e) (format t "~&SMOKE-FAIL: ~a~%" e) (uiop:quit 1)))
(uiop:quit 0)
