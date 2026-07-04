;;;; src/packages.lisp — loom's packages.

;;; loom.sdl — the hand-written CFFI layer over libSDL2 (the ONLY FFI in the
;;; stack).  Replaces cl-sdl2/cl-autowrap so loom needs no c2ffi at build time:
;;; just cffi (pure Quicklisp) + the SDL2 shared library.  Implemented in
;;; src/sdl-ffi.lisp; the wrappers here are the whole surface shell.lisp uses.
(defpackage #:loom.sdl
  (:use #:cl)
  (:export
   #:init #:quit #:sdl-error
   #:create-window #:destroy-window #:set-window-title #:window-size
   #:create-renderer #:destroy-renderer
   #:create-texture #:destroy-texture #:update-texture
   #:set-render-draw-color #:render-clear #:render-copy #:render-present
   #:poll-event #:delay))

;;; loom — the shell proper (page model, input translation, event loop).
(defpackage #:loom
  (:use #:cl)
  (:local-nicknames (#:h #:weft.html) (#:dom #:weft.dom) (#:css #:weft.css)
                    (#:r #:weft.render) (#:ws #:weft.script)
                    (#:fetch #:weft.fetch) (#:url #:weft.url)
                    (#:sdl #:loom.sdl))
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
