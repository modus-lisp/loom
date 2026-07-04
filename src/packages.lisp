;;;; src/packages.lisp — loom's single package.
(defpackage #:loom
  (:use #:cl)
  (:local-nicknames (#:h #:weft.html) (#:dom #:weft.dom) (#:css #:weft.css)
                    (#:r #:weft.render) (#:ws #:weft.script)
                    (#:fetch #:weft.fetch) (#:url #:weft.url))
  (:export
   ;; entry points
   #:run #:main #:run-shell
   ;; page model
   #:page #:page-p #:load-page #:load-url #:load-file
   #:render-page #:relayout
   #:page-canvas #:page-root #:page-styles #:page-width #:page-viewport-height
   #:page-scroll-y #:page-content-height #:page-title #:page-cursor
   #:page-hover-node #:page-url #:page-on-navigate
   ;; input -> DOM
   #:node-at-page #:mouse-press #:mouse-release #:mouse-move #:mouse-wheel
   #:key-down #:key-text #:link-at #:anchor-href
   ;; pure helpers (headless-testable)
   #:sdl-button->dom #:wheel->scroll-delta #:clamp-scroll #:resolve-url
   #:*wheel-step*))
