;;;; inspect/smoke.lisp — headless glass smoke test.
;;;;
;;;; Proves loom's glass display path end to end without a VNC client: load a
;;;; real weft render into the page model, attach it to a glass framebuffer,
;;;; pump the loop several frames (paint into the framebuffer + advance timers),
;;;; and write the page canvas to a PNG so the render can be eyeballed.
;;;;
;;;;   sbcl --script inspect/smoke.lisp
(require "asdf")
;; --script skips ~/.sbclrc, so bootstrap quicklisp explicitly for the deps.
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
;; SIBLING REPOS BY TREE, NOT BY LIST.  This used to name loom's dependencies one
;; directory at a time, which meant every new dependency had to be remembered here
;; as well as in loom.asd — and when `folio' (the PDF engine) was added, it was not.
;; ASDF then resolved "folio" to the UNRELATED 2013 library of the same name in the
;; quicklisp dist, which loads perfectly and defines no FOLIO package, so this gate
;; died in a read error about a package rather than a missing system.  A :tree finds
;; whatever the workspace actually contains, and cannot fall out of date.
(asdf:initialize-source-registry
 (let ((here (make-pathname :name nil :type nil :defaults *load-truename*)))
   `(:source-registry (:tree ,(merge-pathnames "../../" here))
                      (:exclude "vendor") (:exclude "deps") :inherit-configuration)))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "loom/glass"))

(in-package #:loom.glass)

(defparameter *here* (make-pathname :name nil :type nil :defaults *load-truename*))
(defun smoke (&key (out (merge-pathnames "smoke.png" *here*)))
  (let* ((home (namestring (loom::default-home)))
         (page (loom:load-file home :width 1024 :viewport-height 768)))
    (loom:render-page page)
    (format t "~&loaded ~a: canvas ~dx~d, title ~s~%"
            home (weft.render:canvas-width (loom:page-canvas page))
            (weft.render:canvas-height (loom:page-canvas page))
            (loom:page-title page))
    ;; write the exact pixels glass packs into the framebuffer, as proof of source
    (weft.render:write-png (loom:page-canvas page) out)
    (format t "wrote render PNG -> ~a~%" out)
    ;; exercise the input translation through the page model (as the RFB callbacks do)
    (loom:mouse-move page 40 60)
    (loom:mouse-wheel page -6)           ; scroll down a few notches
    (format t "input applied: scroll-y ~d, cursor ~a~%"
            (loom:page-scroll-y page) (loom:page-cursor page))
    ;; attach the page to a glass framebuffer and pump the loop headlessly: this
    ;; paints the (scrolled) frame into the framebuffer and advances timers N
    ;; times — the whole glass display path, minus the RFB socket.
    (let* ((fb (glass:make-framebuffer 1024 768 (glass:rgb 255 255 255)))
           (app (attach page fb)))
      (pump-loop app :max-iterations 8)
      (format t "glass path ran: framebuffer ~dx~d, 8 iterations, final scroll-y ~d~%"
              (glass:fb-width fb) (glass:fb-height fb) (loom:page-scroll-y page))
      (format t "SMOKE-OK~%"))))

(handler-case (smoke)
  (error (e) (format t "~&SMOKE-FAIL: ~a~%" e) (uiop:quit 1)))
(uiop:quit 0)
