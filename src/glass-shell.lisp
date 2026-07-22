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
  (:export #:serve #:run-glass #:attach #:attach-browser #:pump-loop #:on-key #:on-pointer #:stop
           #:glass-app #:glass-app-page #:glass-app-fb))
(in-package #:loom.glass)

(defstruct glass-app
  page fb
  (vw 0) (vh 0)
  ;; --- browser chrome (address bar + back/fwd/reload); CHROME-H 0 = bare page ---
  (chrome-h 0)                          ; toolbar height in px (0 = no chrome)
  (url "")                              ; current location shown in the address bar
  (editing nil)                         ; is the address bar being edited?
  (edit-buf "")                         ; the address-bar text while editing
  (edit-sel nil)                        ; text is "selected" (click-to-edit) — next key replaces it
  (back '())                            ; history: visited locations behind us
  (fwd '())                             ; locations ahead (after going Back)
  (lock (sb-thread:make-mutex :name "loom-glass-page"))
  (dirty t)
  (running t)                           ; pump-loop keeps going while true
  (buttons 0)                           ; last RFB button mask (low 3 bits)
  ;; scroll-triggered lazy image loading (see MAYBE-WARM-LAZY):
  (warmed (make-hash-table :test 'equal)) ; lazy img URLs already warm-attempted (no re-warm)
  (warming nil)                         ; T while a background warm+re-render is in flight
  (last-scroll -1)                      ; page-scroll-y at the last lazy check (skip if unmoved)
  (last-lazy-check 0))                  ; internal-real-time of the last check (throttle to a few/sec)

(defun stop (app)
  "Stop APP's pump loop (e.g. when its host window is closed) so weft stops
   re-rendering into an orphaned framebuffer."
  (setf (glass-app-running app) nil))

;;; ---------------------------------------------------------------------------
;;; Paint — weft's RGB8 canvas slice -> the glass framebuffer
;;; ---------------------------------------------------------------------------
(defun paint (app)
  "Copy the visible slice of the page canvas (at the current scroll offset) into
   the glass framebuffer, packing RGB8 -> 0x00RRGGBB.  The page occupies the rows
   BELOW the chrome bar (CHROME-H, 0 for a bare page); rows past the content end are
   white and the chrome (if any) is drawn on top.  Assumes the caller holds the lock."
  (let* ((pg (glass-app-page app))
         (cv (loom:page-canvas pg))
         (cw (r:canvas-width cv))
         (ch (r:canvas-height cv))
         (px (r:canvas-pixels cv))
         (fb (glass-app-fb app))
         (fbpx (glass:fb-pixels fb))
         (fbw (glass:fb-width fb))
         (fbh (glass:fb-height fb))
         (ch-h (glass-app-chrome-h app))
         (page-h (- fbh ch-h))
         (sy (min (loom:page-scroll-y pg) (max 0 (- ch page-h))))
         (cols (min cw fbw)))
    (glass:with-fb-locked (fb)
      (dotimes (y page-h)
        (let ((cy (+ sy y))
              (drow (* (+ y ch-h) fbw)))            ; page starts CH-H rows down
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
            (t (loop for x from 0 below fbw do (setf (aref fbpx (+ drow x)) #xffffff))))))
      (when (plusp ch-h) (draw-chrome app)))))

;;; ---------------------------------------------------------------------------
;;; Browser chrome — a toolbar: [<] [>] [reload]  [ address bar ]
;;; ---------------------------------------------------------------------------
(defparameter +chrome-h+ 34)                 ; toolbar height
(defparameter +btn-w+ 28)                    ; nav-button width/height box
(defun %btn-x (i) (+ 4 (* i (+ +btn-w+ 2)))) ; left edge of nav button I (0=back,1=fwd,2=reload)
(defun %addr-x () (+ (%btn-x 3) 4))          ; address bar left edge
(defun %grey (n) (glass:rgb n n n))

(defun %arrow (fb bx dir enabled)
  "A small left/right filled triangle centred in the nav button at column BX."
  (let ((color (if enabled (%grey 60) (%grey 170))) (h 6) (cy (floor +chrome-h+ 2))
        (lx (+ bx 10)) (rx (+ bx 19)))
    (loop for dy from (- h) to h
          for frac = (/ (abs dy) h) do
      (if (eq dir :left)
          (let ((e (round (+ lx (* frac (- rx lx)))))) (glass:fb-hline fb e (+ cy dy) (max 0 (- rx e)) color))
          (let ((e (round (- rx (* frac (- rx lx)))))) (glass:fb-hline fb lx (+ cy dy) (max 0 (- e lx)) color))))))

(defun %reload-icon (fb bx enabled)
  "A reload glyph: a ~3/4 ring with a little arrowhead."
  (let ((color (if enabled (%grey 60) (%grey 170))) (cx (+ bx 14)) (cy (floor +chrome-h+ 2)) (r 6))
    (loop for deg from 20 to 300 by 6
          for a = (* deg (/ pi 180.0d0))
          for x = (round (+ cx (* r (cos a)))) for y = (round (+ cy (* r (sin a))))
          do (glass:fb-rect fb x y 2 2 color))
    ;; arrowhead at the arc's end (~300 deg, upper right)
    (let ((ax (round (+ cx (* r (cos (* 300 (/ pi 180.0d0))))))) (ay (round (+ cy (* r (sin (* 300 (/ pi 180.0d0))))))))
      (glass:fb-rect fb (- ax 1) (- ay 3) 4 2 color)
      (glass:fb-rect fb (+ ax 1) (- ay 3) 2 5 color))))

(defun draw-chrome (app)
  "Draw the toolbar into the top +CHROME-H+ rows of the fb.  Caller holds the lock."
  (let* ((fb (glass-app-fb app)) (fbw (glass:fb-width fb))
         (back-on (consp (glass-app-back app))) (fwd-on (consp (glass-app-fwd app))))
    (glass:fb-rect fb 0 0 fbw +chrome-h+ (%grey 224))               ; toolbar background
    (glass:fb-hline fb 0 (1- +chrome-h+) fbw (%grey 150))           ; bottom divider
    ;; nav buttons
    (dotimes (i 3)
      (let ((bx (%btn-x i)))
        (glass:fb-rect fb bx 3 +btn-w+ (- +chrome-h+ 6) (%grey 236))
        (glass:fb-frame fb bx 3 +btn-w+ (- +chrome-h+ 6) (%grey 160) 1)))
    (%arrow fb (%btn-x 0) :left back-on)
    (%arrow fb (%btn-x 1) :right fwd-on)
    (%reload-icon fb (%btn-x 2) t)
    ;; address bar
    (let* ((ax (%addr-x)) (aw (- fbw ax 5)) (ay 5) (ah (- +chrome-h+ 10))
           (editing (glass-app-editing app))
           (text (if editing (glass-app-edit-buf app) (glass-app-url app))))
      (glass:fb-rect fb ax ay aw ah (glass:rgb 255 255 255))
      (glass:fb-frame fb ax ay aw ah (%grey (if editing 90 160)) 1)
      (when (and editing (glass-app-edit-sel app) (plusp (length text)))  ; selection highlight
        (glass:fb-rect fb (+ ax 5) (+ ay 2) (min (+ 2 (glass:text-width text :size 13)) (- aw 8)) (- ah 4)
                       (glass:rgb 180 210 250)))
      (glass:fb-text fb (+ ax 6) (+ ay 4) text :size 13 :color (%grey 30))
      (when (and editing (not (glass-app-edit-sel app)))            ; caret at end of text
        (let ((cx (+ ax 6 (glass:text-width text :size 13) 1)))
          (glass:fb-vline fb (min cx (- (+ ax aw) 3)) (+ ay 3) (- ah 6) (%grey 30)))))))

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

(defun load-start (start vw vh)
  "Load START into a fresh rendered page sized VW x VH.  \"about:blank\" -> an empty
   document (instant, no network — the browser window appears immediately); an
   http(s) URL -> the network; anything else -> a local file."
  (let ((pg (cond
              ((string-equal start "about:blank")
               (loom:load-page "" :url "about:blank" :width vw :viewport-height vh))
              ((or (loom::url-prefix-p "http:" start) (loom::url-prefix-p "https:" start))
               (loom:load-url start :width vw :viewport-height vh))
              (t (loom:load-file (namestring (loom::url->path start)) :width vw :viewport-height vh)))))
    (loom:render-page pg)
    pg))

(defun navigate (app dest &key (record t))
  "Load DEST in APP, updating the address bar + (with RECORD) the Back history.
   Back/Forward pass RECORD nil and manage the stacks themselves."
  (handler-case
      (let ((new (load-start dest (glass-app-vw app) (glass-app-vh app))))
        (when (and record (plusp (length (glass-app-url app))))
          (push (glass-app-url app) (glass-app-back app))
          (setf (glass-app-fwd app) '()))
        (setf (glass-app-page app) new
              (glass-app-url app) (or (ignore-errors (loom:page-url new)) dest)
              (glass-app-editing app) nil
              (glass-app-dirty app) t)
        (wire-navigation app))
    (error (e) (format *error-output* "~&loom.glass: navigate to ~a failed: ~a~%" dest e)
      (setf (glass-app-editing app) nil (glass-app-dirty app) t))))

(defun go-back (app)
  (when (consp (glass-app-back app))
    (push (glass-app-url app) (glass-app-fwd app))
    (navigate app (pop (glass-app-back app)) :record nil)))
(defun go-forward (app)
  (when (consp (glass-app-fwd app))
    (push (glass-app-url app) (glass-app-back app))
    (navigate app (pop (glass-app-fwd app)) :record nil)))
(defun reload-page (app)
  (when (plusp (length (glass-app-url app))) (navigate app (glass-app-url app) :record nil)))

(defun wire-navigation (app)
  "Install the page's on-navigate callback so a clicked link loads in this app
   (through NAVIGATE, so the address bar + Back history follow along)."
  (setf (loom:page-on-navigate (glass-app-page app))
        (lambda (pg target) (declare (ignore pg)) (navigate app target))))

;;; ---------------------------------------------------------------------------
;;; RFB input -> page-model calls (the SDL shell's handle-event, over RFB)
;;; ---------------------------------------------------------------------------
;;; RFB button mask: bit0 left, bit1 middle, bit2 right; bits 3/4 = wheel up/down
;;; (transient).  DOM button numbers: left 0, middle 1, right 2.
(defun %in-btn (x i) (<= (%btn-x i) x (+ (%btn-x i) +btn-w+)))

(defun chrome-pointer (app mask x)
  "Handle a pointer event in the toolbar strip (only left-press acts)."
  (let ((press-edge (and (logtest mask 1) (not (logtest (glass-app-buttons app) 1)))))
    (setf (glass-app-buttons app) (logand mask 7))
    (when press-edge
      (cond
        ((%in-btn x 0) (go-back app))
        ((%in-btn x 1) (go-forward app))
        ((%in-btn x 2) (reload-page app))
        ((>= x (%addr-x))                                   ; click the address bar -> edit (all selected)
         (setf (glass-app-editing app) t
               (glass-app-edit-buf app) (glass-app-url app)
               (glass-app-edit-sel app) t                   ; whole URL selected: next key replaces it
               (glass-app-dirty app) t))))))

(defun on-pointer (app mask x y)
  (sb-thread:with-mutex ((glass-app-lock app))
    (let ((ch-h (glass-app-chrome-h app)))
      (cond
        ((and (plusp ch-h) (< y ch-h))                     ; in the toolbar
         (chrome-pointer app mask x))
        (t                                                 ; in the page (offset past the chrome)
         (when (glass-app-editing app)                     ; clicking the page ends address editing
           (setf (glass-app-editing app) nil (glass-app-dirty app) t))
         (let* ((pg (glass-app-page app))
                (py (- y ch-h))
                (real (logand mask 7))
                (changed (logxor real (glass-app-buttons app))))
           (when (logtest mask 8)  (loom:mouse-wheel pg 1))
           (when (logtest mask 16) (loom:mouse-wheel pg -1))
           (loom:mouse-move pg x py)
           (dotimes (b 3)
             (when (logbitp b changed)
               (if (logbitp b real)
                   (loom:mouse-press pg x py b)
                   (loom:mouse-release pg x py b))))
           (setf (glass-app-buttons app) real
                 (glass-app-dirty app) t)))))))

(defun keysym-name (keysym)
  "A DOM key string for a non-printable X keysym (thin — enough for keydown to
   fire; matches the SDL shell's key-name coverage)."
  (case keysym
    (#xff0d "Enter") (#xff08 "Backspace") (#xff09 "Tab") (#xff1b "Escape")
    (#xff51 "ArrowLeft") (#xff52 "ArrowUp") (#xff53 "ArrowRight") (#xff54 "ArrowDown")
    (#xffff "Delete") (#xff50 "Home") (#xff57 "End")
    (t "Unidentified")))

(defun normalize-input (s)
  "Turn address-bar text into a loadable location: keep a scheme/about: as-is,
   otherwise assume https://.  Empty -> about:blank."
  (let ((s (string-trim " " s)))
    (cond
      ((zerop (length s)) "about:blank")
      ((string-equal s "about:blank") s)
      ((or (loom::url-prefix-p "http:" s) (loom::url-prefix-p "https:" s)
           (loom::url-prefix-p "file:" s)) s)
      (t (concatenate 'string "https://" s)))))

(defun edit-key (app keysym)
  "Feed a keystroke to the address bar while it's being edited.  When the text is
   SELECTED (just clicked), the next edit replaces it whole."
  (cond
    ((= keysym #xff0d)                                     ; Enter -> go
     (setf (glass-app-editing app) nil (glass-app-edit-sel app) nil)
     (navigate app (normalize-input (glass-app-edit-buf app))))
    ((= keysym #xff1b)                                     ; Escape -> cancel
     (setf (glass-app-editing app) nil (glass-app-edit-sel app) nil (glass-app-dirty app) t))
    ((= keysym #xff08)                                     ; Backspace (clears all if selected)
     (let ((b (glass-app-edit-buf app)))
       (setf (glass-app-edit-buf app)
             (if (glass-app-edit-sel app) "" (if (plusp (length b)) (subseq b 0 (1- (length b))) b))))
     (setf (glass-app-edit-sel app) nil (glass-app-dirty app) t))
    ((<= 32 keysym 126)                                    ; printable (replaces selection, else appends)
     (setf (glass-app-edit-buf app)
           (concatenate 'string (if (glass-app-edit-sel app) "" (glass-app-edit-buf app))
                        (string (code-char keysym)))
           (glass-app-edit-sel app) nil
           (glass-app-dirty app) t))))

(defun on-key (app down keysym)
  (when down
    (sb-thread:with-mutex ((glass-app-lock app))
      (if (glass-app-editing app)
          (edit-key app keysym)
          (let ((pg (glass-app-page app)))
            (cond
              ((<= 32 keysym 126)                          ; printable: keydown + textinput
               (let ((s (string (code-char keysym))))
                 (loom:key-down pg s :key-code keysym)
                 (loom:key-text pg s)))
              (t (loom:key-down pg (keysym-name keysym) :key-code keysym)))
            (setf (glass-app-dirty app) t))))))

;;; ---------------------------------------------------------------------------
;;; Scroll-triggered lazy image loading
;;; ---------------------------------------------------------------------------
;;; The glass driver scrolls by blitting a slice of the PRE-rendered full-page canvas
;;; (PAINT), so it never re-lays-out — a below-fold loading=lazy <img> would show its
;;; gray placeholder forever.  As the view scrolls, pull those images in like a real
;;; browser: notice when new deferred lazy images enter the scroll-relative in-view band,
;;; warm them (network) OFF the page lock so scrolling stays smooth, then re-render under
;;; the lock and mark the frame dirty so they "pop in" on the next paint.
(defparameter *lazy-check-interval* 1/5
  "Minimum seconds between scroll-driven lazy-image checks — throttles the box-tree walk
   to a few times a second so it never competes with the paint loop.")

(defun maybe-warm-lazy (app)
  "If scrolling has brought new deferred loading=lazy images into the in-view band, kick
   a background warm + re-render (non-blocking) so they pop in on a later frame.  Debounced
   three ways: skipped while a warm is already in flight, when the scroll hasn't moved since
   the last check, and throttled to *LAZY-CHECK-INTERVAL*; each URL is warmed at most once
   (a failed/offline image never re-triggers).  The slow network fetch runs off the page
   lock; only the final re-render (canvas swap) takes the lock, so the paint loop is smooth."
  (when (glass-app-warming app)
    (return-from maybe-warm-lazy nil))
  (let ((now (get-internal-real-time)))
    (when (< (/ (- now (glass-app-last-lazy-check app)) internal-time-units-per-second)
             *lazy-check-interval*)
      (return-from maybe-warm-lazy nil))
    (setf (glass-app-last-lazy-check app) now))
  (let ((pg (glass-app-page app)) (new '()))
    ;; read the pending in-view lazy set under the lock (it walks the live box tree)
    (sb-thread:with-mutex ((glass-app-lock app))
      (let ((sy (loom:page-scroll-y pg)))
        (when (/= sy (glass-app-last-scroll app))
          (setf (glass-app-last-scroll app) sy)
          (dolist (u (loom:inview-lazy-pending-urls pg))
            (unless (gethash u (glass-app-warmed app)) (push u new))))))
    (when new
      (dolist (u new) (setf (gethash u (glass-app-warmed app)) t))  ; don't re-warm these
      (setf (glass-app-warming app) t)
      (sb-thread:make-thread
       (lambda ()
         (unwind-protect
             (handler-case
                 (progn
                   (loom:warm-image-urls pg new)              ; network — OFF the page lock
                   (sb-thread:with-mutex ((glass-app-lock app))
                     (loom:render-page pg)                    ; cache-hit fill + repaint, under lock
                     (setf (glass-app-dirty app) t)))         ; next paint shows the pop-in
               (error (e) (format *error-output* "~&loom.glass: lazy warm failed: ~a~%" e)))
           (setf (glass-app-warming app) nil)))
       :name "loom-glass-lazy")
      t)))

;;; ---------------------------------------------------------------------------
;;; The loop
;;; ---------------------------------------------------------------------------
(defun pump-loop (app &key max-iterations)
  "Advance the page's timer loop and repaint when the DOM changed or input dirtied
   the view; glass's dirty-tile diff ships only what actually changed.  Runs until
   MAX-ITERATIONS frames (headless) or forever (a live session)."
  (loop with i = 0
        while (and (glass-app-running app) (or (null max-iterations) (< i max-iterations)))
        do (sb-thread:with-mutex ((glass-app-lock app))
             (when (loom::pump (glass-app-page app)) (setf (glass-app-dirty app) t))
             (when (glass-app-dirty app)
               (paint app)
               (setf (glass-app-dirty app) nil)))
           ;; off the lock: notice new lazy images the scroll brought into view and
           ;; warm+re-render them in the background (non-blocking — see MAYBE-WARM-LAZY).
           (maybe-warm-lazy app)
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

(defun attach-browser (start fb)
  "Like ATTACH, but with browser CHROME (a toolbar: back / forward / reload + an
   address bar) in the top strip; the page renders below it and its viewport is
   sized accordingly.  START is a URL, a file, or \"about:blank\" (instant).  The
   host forwards RFB input to ON-KEY / ON-POINTER and runs PUMP-LOOP as for ATTACH."
  (let* ((ch-h +chrome-h+)
         (vw (glass:fb-width fb)) (vh (max 1 (- (glass:fb-height fb) ch-h)))
         (pg (load-start start vw vh))
         (app (make-glass-app :page pg :fb fb :vw vw :vh vh
                              :chrome-h ch-h
                              :url (or (ignore-errors (loom:page-url pg)) start))))
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
