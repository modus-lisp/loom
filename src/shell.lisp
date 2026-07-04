;;;; src/shell.lisp — the SDL2 window, texture upload and event loop.
;;;;
;;;; The driver over the page model (page.lisp): it owns the window + renderer +
;;;; a streaming RGB24 texture matching weft's canvas, blits the current page's
;;;; painted pixels into it each frame, and feeds SDL input into the page model,
;;;; which turns it into the DOM events weft dispatches.  All SDL access goes
;;;; through the LOOM.SDL CFFI layer (sdl-ffi.lisp) — no raw FFI here.
;;;;
;;;; macOS note: Cocoa requires the window and event pump to run on the process
;;;; main thread.  RUN (main.lisp) is called on the initial thread (the
;;;; sbcl --eval / run.sh path), so this loop runs there — see the README.
(in-package #:loom)

(defstruct app
  window renderer texture
  (tex-w 0) (tex-h 0)
  page
  (running t)
  (dirty t))            ; a blit is needed (input happened / page changed)

;;; ---------------------------------------------------------------------------
;;; Texture management — a streaming RGB24 texture sized to the window viewport
;;; ---------------------------------------------------------------------------
(defun ensure-texture (app w h)
  "Create (or recreate) the streaming texture at W x H, matching the window."
  (when (and (app-texture app)
             (or (/= w (app-tex-w app)) (/= h (app-tex-h app))))
    (sdl:destroy-texture (app-texture app))
    (setf (app-texture app) nil))
  (unless (app-texture app)
    (setf (app-texture app) (sdl:create-texture (app-renderer app) w h)
          (app-tex-w app) w (app-tex-h app) h)))

(defun blit (app)
  "Upload the visible slice of the page canvas into the texture and present it.
   Zero-copy: SDL reads straight from weft's row-major RGB8 canvas at the scroll
   offset (pitch = width*3), so no intermediate buffer is allocated."
  (let* ((pg (app-page app))
         (cv (page-canvas pg))
         (w (r:canvas-width cv))
         (ch (r:canvas-height cv))
         (vh (page-viewport-height pg))
         (sy (min (page-scroll-y pg) (max 0 (- ch vh))))
         (vis (max 0 (min vh (- ch sy))))
         (px (r:canvas-pixels cv))
         (rr (app-renderer app)))
    (ensure-texture app w vh)
    (when (plusp vis)
      (cffi:with-pointer-to-vector-data (base px)
        (let ((src (cffi:inc-pointer base (* sy w 3))))
          (sdl:update-texture (app-texture app) 0 0 w vis src (* w 3)))))
    (sdl:set-render-draw-color rr 255 255 255 255)
    (sdl:render-clear rr)
    (when (plusp vis)
      (sdl:render-copy rr (app-texture app)
                       :src (list 0 0 w vis)
                       :dst (list 0 0 w vis)))
    (sdl:render-present rr)))

;;; ---------------------------------------------------------------------------
;;; SDL event -> page model
;;; ---------------------------------------------------------------------------
(defun sync-cursor (app)
  "Reflect the page's cursor hint as the system cursor (pointer over links)."
  (declare (ignorable app)))     ; cursor image swap deferred; page-cursor is tracked

(defun navigate-to (app target)
  "Load TARGET into a fresh page, preserving window size, and mark for repaint."
  (handler-case
      (let ((w (app-tex-w app)) (h (app-tex-h app)))
        (setf (app-page app)
              (if (or (url-prefix-p "http:" target) (url-prefix-p "https:" target))
                  (load-url target :width w :viewport-height h)
                  (load-file (namestring (url->path target)) :width w :viewport-height h)))
        (wire-navigation app)
        (when (app-window app) (sdl:set-window-title (app-window app) (page-title (app-page app))))
        (setf (app-dirty app) t))
    (error (e) (format *error-output* "~&loom: navigation to ~a failed: ~a~%" target e))))

(defun url->path (file-url)
  "The filesystem path of a file:// URL (or the string unchanged)."
  (if (url-prefix-p "file://" file-url) (subseq file-url 7) file-url))

(defun wire-navigation (app)
  "Install the page's on-navigate callback so a followed link loads in this app."
  (setf (page-on-navigate (app-page app))
        (lambda (pg target) (declare (ignore pg)) (navigate-to app target))))

(defun handle-event (app ev)
  "Translate one SDL event (a plist from SDL:POLL-EVENT) into page-model calls."
  (let ((pg (app-page app)))
    (case (getf ev :type)
      (:quit (setf (app-running app) nil))
      (:mousebuttondown
       (mouse-press pg (getf ev :x) (getf ev :y) (sdl-button->dom (getf ev :button)))
       (setf (app-dirty app) t))
      (:mousebuttonup
       (mouse-release pg (getf ev :x) (getf ev :y) (sdl-button->dom (getf ev :button)))
       (setf (app-dirty app) t))
      (:mousemotion
       (mouse-move pg (getf ev :x) (getf ev :y))
       (setf (app-dirty app) t))
      (:mousewheel
       (mouse-wheel pg (getf ev :y))
       (setf (app-dirty app) t))
      (:keydown
       (let ((sym (getf ev :sym)))
         (key-down pg (key-name sym) :key-code sym)
         (setf (app-dirty app) t)))
      (:textinput
       (key-text pg (getf ev :text))
       (setf (app-dirty app) t))
      (:windowevent
       ;; any window event: reconcile the layout width with the current size
       (multiple-value-bind (w h) (sdl:window-size (app-window app))
         (when (or (/= w (page-width pg)) (/= h (page-viewport-height pg)))
           (relayout pg w h)
           (setf (app-dirty app) t)))))))

(defun key-name (sym)
  "A DOM key string for the printable SDL keycode SYM (thin — enough for keydown
   to fire; a full key map is a later round)."
  (if (and (>= sym 32) (< sym 127)) (string (code-char sym)) "Unidentified"))

;;; ---------------------------------------------------------------------------
;;; The loop
;;; ---------------------------------------------------------------------------
(defun run-loop (app &key max-iterations)
  "Poll SDL events, pump the page's timer loop, and blit.  Runs until a quit
   event (or, headlessly, until MAX-ITERATIONS frames)."
  (loop with i = 0
        while (and (app-running app)
                   (or (null max-iterations) (< i max-iterations)))
        do (loop for ev = (sdl:poll-event) while ev
                 do (handle-event app ev))
           ;; advance timers/animations one frame every iteration; repaint if a
           ;; timer fired and mutated the DOM
           (when (pump (app-page app))
             (setf (app-dirty app) t))
           (when (app-dirty app)
             (blit app)
             (setf (app-dirty app) nil))
           (incf i)
           (unless max-iterations (sdl:delay 8))))

(defun make-renderer (win)
  "A renderer for WIN: accelerated+vsync if available (the laptop path), else a
   software renderer (the headless/dummy-driver path)."
  (or (ignore-errors (sdl:create-renderer win '(:accelerated :presentvsync)))
      (ignore-errors (sdl:create-renderer win '(:software)))
      (sdl:create-renderer win nil)))

(defun run-shell (page &key (width 1024) (height 768) max-iterations)
  "Open a window + renderer + texture for PAGE and run the event loop.  Assumes
   SDL video is initialized and this is the main thread (see RUN).  Returns the
   final APP (useful for headless inspection)."
  (let ((win (sdl:create-window (page-title page) width height
                                :shown t :resizable t)))
    (unwind-protect
         (let* ((rr (make-renderer win))
                (app (make-app :window win :renderer rr :page page)))
           (handler-case (sdl:set-window-title win (page-title page)) (error () nil))
           (wire-navigation app)
           (blit app)
           (run-loop app :max-iterations max-iterations)
           (sdl:destroy-renderer rr)
           app)
      (sdl:destroy-window win))))
