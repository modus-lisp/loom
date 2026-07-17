;;;; src/glass-shell.lisp — a glass (pure-CL VNC) driver over the page model.
;;;;
;;;; loom's other driver, shell.lisp, opens an SDL2 window and blits weft's canvas
;;;; into a GPU texture.  This one is the SAME driver over the SAME page model
;;;; (page.lisp) — but the "screen" is a glass framebuffer served over VNC, and
;;;; input arrives as RFB pointer/key events instead of SDL ones.  It needs NO
;;;; SDL and NO X: weft paints to a row-major RGB8 canvas, we pack that into a
;;;; glass 0x00RRGGBB framebuffer, and glass ships the dirty tiles to any VNC
;;;; client.  This is the pure-Lisp display path — the shell that finally makes
;;;; the whole weaving stack (scribe/shuttle/gesso/stencil/weft/loom) FFI-free.
;;;;
;;;; The RFB server runs its client loop in glass's own thread; its on-key/
;;;; on-pointer callbacks mutate the live page (dispatching DOM events through
;;;; weft), and a pump loop advances timers and repaints.  weft's JS context is
;;;; not reentrant, so a single mutex serialises input handling against the pump.

(defpackage #:loom.glass
  (:use #:cl)
  (:local-nicknames (#:r #:weft.render))
  (:export #:serve #:run-glass #:attach #:pump-loop #:on-key #:on-pointer
           #:glass-app #:glass-app-page #:glass-app-fb))
(in-package #:loom.glass)

(defstruct glass-app
  page fb
  (vw 0) (vh 0)
  (lock (sb-thread:make-mutex :name "loom-glass-page"))
  (dirty t)
  (buttons 0))                          ; last RFB button mask (low 3 bits)

;;; ---------------------------------------------------------------------------
;;; Paint — weft's RGB8 canvas slice -> the glass framebuffer
;;; ---------------------------------------------------------------------------
(defun paint (app)
  "Copy the visible slice of the page canvas (at the current scroll offset) into
   the glass framebuffer, packing RGB8 -> 0x00RRGGBB.  Rows past the content end
   are painted white.  Assumes the caller holds the page lock."
  (let* ((pg (glass-app-page app))
         (cv (loom:page-canvas pg))
         (cw (r:canvas-width cv))
         (ch (r:canvas-height cv))
         (px (r:canvas-pixels cv))
         (fb (glass-app-fb app))
         (fbpx (glass:fb-pixels fb))
         (fbw (glass:fb-width fb))
         (fbh (glass:fb-height fb))
         (sy (min (loom:page-scroll-y pg) (max 0 (- ch fbh))))
         (cols (min cw fbw)))
    (glass:with-fb-locked (fb)
      (dotimes (y fbh)
        (let ((cy (+ sy y))
              (drow (* y fbw)))
          (cond
            ((< cy ch)
             (let ((srow (* cy cw 3)))
               (dotimes (x cols)
                 (let ((o (+ srow (* x 3))))
                   (setf (aref fbpx (+ drow x))
                         (logior (ash (aref px o) 16)
                                 (ash (aref px (+ o 1)) 8)
                                 (aref px (+ o 2))))))
               (loop for x from cols below fbw do (setf (aref fbpx (+ drow x)) #xffffff))))
            (t (loop for x from 0 below fbw do (setf (aref fbpx (+ drow x)) #xffffff)))))))))

;;; ---------------------------------------------------------------------------
;;; Navigation — a followed link loads a fresh page at the same viewport
;;; ---------------------------------------------------------------------------
(defun load-target (target vw vh)
  "Load TARGET (an http(s) URL or a file path/URL) into a fresh, rendered page
   sized to the VW x VH viewport."
  (let ((pg (if (or (loom::url-prefix-p "http:" target) (loom::url-prefix-p "https:" target))
                (loom:load-url target :width vw :viewport-height vh)
                (loom:load-file (namestring (loom::url->path target)) :width vw :viewport-height vh))))
    (loom:render-page pg)
    pg))

(defun wire-navigation (app)
  "Install the page's on-navigate callback so a clicked link loads in this app."
  (setf (loom:page-on-navigate (glass-app-page app))
        (lambda (pg target)
          (declare (ignore pg))
          (handler-case
              (let ((new (load-target target (glass-app-vw app) (glass-app-vh app))))
                (setf (glass-app-page app) new (glass-app-dirty app) t)
                (wire-navigation app))
            (error (e) (format *error-output* "~&loom.glass: navigation to ~a failed: ~a~%" target e))))))

;;; ---------------------------------------------------------------------------
;;; RFB input -> page-model calls (the SDL shell's handle-event, over RFB)
;;; ---------------------------------------------------------------------------
;;; RFB button mask: bit0 left, bit1 middle, bit2 right; bits 3/4 = wheel up/down
;;; (transient).  DOM button numbers: left 0, middle 1, right 2.
(defun on-pointer (app mask x y)
  (sb-thread:with-mutex ((glass-app-lock app))
    (let* ((pg (glass-app-page app))
           (real (logand mask 7))
           (changed (logxor real (glass-app-buttons app))))
      (when (logtest mask 8)  (loom:mouse-wheel pg 1))     ; wheel up
      (when (logtest mask 16) (loom:mouse-wheel pg -1))    ; wheel down
      (loom:mouse-move pg x y)
      (dotimes (b 3)
        (when (logbitp b changed)
          (if (logbitp b real)
              (loom:mouse-press pg x y b)
              (loom:mouse-release pg x y b))))
      (setf (glass-app-buttons app) real
            (glass-app-dirty app) t))))

(defun keysym-name (keysym)
  "A DOM key string for a non-printable X keysym (thin — enough for keydown to
   fire; matches the SDL shell's key-name coverage)."
  (case keysym
    (#xff0d "Enter") (#xff08 "Backspace") (#xff09 "Tab") (#xff1b "Escape")
    (#xff51 "ArrowLeft") (#xff52 "ArrowUp") (#xff53 "ArrowRight") (#xff54 "ArrowDown")
    (#xffff "Delete") (#xff50 "Home") (#xff57 "End")
    (t "Unidentified")))

(defun on-key (app down keysym)
  (when down
    (sb-thread:with-mutex ((glass-app-lock app))
      (let ((pg (glass-app-page app)))
        (cond
          ((<= 32 keysym 126)                              ; printable: keydown + textinput
           (let ((s (string (code-char keysym))))
             (loom:key-down pg s :key-code keysym)
             (loom:key-text pg s)))
          (t (loom:key-down pg (keysym-name keysym) :key-code keysym)))
        (setf (glass-app-dirty app) t)))))

;;; ---------------------------------------------------------------------------
;;; The loop
;;; ---------------------------------------------------------------------------
(defun pump-loop (app &key max-iterations)
  "Advance the page's timer loop and repaint when the DOM changed or input dirtied
   the view; glass's dirty-tile diff ships only what actually changed.  Runs until
   MAX-ITERATIONS frames (headless) or forever (a live session)."
  (loop with i = 0
        while (or (null max-iterations) (< i max-iterations))
        do (sb-thread:with-mutex ((glass-app-lock app))
             (when (loom::pump (glass-app-page app)) (setf (glass-app-dirty app) t))
             (when (glass-app-dirty app)
               (paint app)
               (setf (glass-app-dirty app) nil)))
           (incf i)
           (unless max-iterations (sleep 1/60))))

(defun attach (page fb)
  "Build an app driving PAGE into the EXISTING framebuffer FB (viewport = FB
   size), wire link navigation, paint once, and return the app — WITHOUT owning a
   server.  For embedding a live page as someone else's surface (e.g. a window in
   a compositor / window manager): the host forwards RFB input to ON-KEY /
   ON-POINTER and runs PUMP-LOOP to advance timers and repaint into FB."
  (let ((app (make-glass-app :page page :fb fb
                             :vw (glass:fb-width fb) :vh (glass:fb-height fb))))
    (wire-navigation app)
    (sb-thread:with-mutex ((glass-app-lock app)) (paint app))
    app))

(defun latin1 (string)
  "RFB desktop names are a byte string; fold any char > 255 (e.g. a title's
   em-dash) to ASCII so glass can write the ServerInit name cleanly."
  (map 'string (lambda (c) (if (< (char-code c) 256) c #\-)) (or string "loom")))

(defun serve (page &key (port 5900) max-iterations (background nil))
  "Serve PAGE over VNC on PORT: a glass framebuffer sized to the page viewport,
   RFB input wired into the page model.  Runs the pump loop on the calling thread
   (or, with BACKGROUND, in a new thread, returning the APP immediately)."
  (let* ((vw (loom:page-width page))
         (vh (loom:page-viewport-height page))
         (fb (glass:make-framebuffer vw vh (glass:rgb 255 255 255)))
         (app (make-glass-app :page page :fb fb :vw vw :vh vh)))
    (wire-navigation app)
    (sb-thread:with-mutex ((glass-app-lock app)) (paint app))
    (sb-thread:make-thread
     (lambda () (glass:serve fb port
                             :on-key     (lambda (d k)   (on-key app d k))
                             :on-pointer (lambda (m x y) (on-pointer app m x y))
                             :name (latin1 (loom:page-title page))))
     :name "loom-glass-rfb")
    (if background
        (progn (sb-thread:make-thread (lambda () (pump-loop app)) :name "loom-glass-pump") app)
        (progn (pump-loop app :max-iterations max-iterations) app))))

(defun run-glass (&key start (port 5900) (width 1024) (height 768) max-iterations background)
  "Open START (a URL or file; default = the bundled home page) and serve it over
   VNC on PORT.  The glass counterpart of LOOM:RUN — no SDL, no X."
  (let ((page (load-target (or start (namestring (loom::default-home))) width height)))
    (serve page :port port :max-iterations max-iterations :background background)))
