;;;; src/packages.lisp — loom's packages.

;;; loom — the display-agnostic browser core: the page model, input translation,
;;; and start-page helpers.  A display backend (loom.glass, over VNC — see
;;; src/glass-shell.lisp) drives this model; the core itself is FFI-free pure CL.
(defpackage #:loom
  (:use #:cl)
  (:local-nicknames (#:h #:weft.html) (#:dom #:weft.dom) (#:css #:weft.css)
                    (#:r #:weft.render) (#:ws #:weft.script)
                    (#:fetch #:weft.fetch) (#:url #:weft.url))
  (:export
   ;; page model
   #:page #:page-p #:load-page #:load-url #:load-file
   #:*progress* #:report-progress
   ;; navigation instrumentation (for the inspector)
   #:*net-log* #:net-log-reset #:nav-elapsed-ms #:dom-node-counts
   #:render-page #:relayout
   #:inview-lazy-pending-urls #:warm-image-urls #:warm-lazy-for-scroll
   #:page-doc
   #:page-canvas #:page-root #:page-styles #:page-width #:page-viewport-height
   #:page-scroll-y #:page-content-height #:page-title #:page-cursor
   #:page-hover-node #:page-url #:page-on-navigate #:page-js-error
   ;; input -> DOM
   #:node-at-page #:mouse-press #:mouse-release #:mouse-move #:mouse-wheel
   #:key-down #:key-text #:link-at #:anchor-href
   ;; pure helpers (headless-testable)
   #:pointer-button->dom #:wheel->scroll-delta #:clamp-scroll #:resolve-url
   #:*wheel-step*))
