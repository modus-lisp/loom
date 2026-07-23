;;;; ui.lisp — a tiny immediate-mode UI kit for loom's browser chrome, drawn on
;;;; glass framebuffer primitives (no McCLIM, no retained widget tree).
;;;;
;;;; Model: each paint pass REBUILDS the UI from current state.  A widget draws
;;;; itself at the layout cursor, reads the (single) pointer position to render a
;;;; hover/active look, and — the one retained thing — appends its clickable
;;;; rectangle to the frame's HIT LIST tagged with an action id.  After painting,
;;;; the input handler calls HIT-AT to turn a click into that action id.  So the
;;;; draw is immediate (state -> pixels every frame) while hit-testing stays
;;;; compatible with loom's existing paint / on-pointer split (no threading change).
;;;;
;;;; Everything is state-driven, which is exactly what browser chrome wants: the
;;;; breadcrumb spine, the sibling/children rail, the loading spinner and the
;;;; address field are just re-emitted from the nav tree each frame.
(defpackage #:loom.ui
  (:use #:cl)
  (:local-nicknames (#:g #:glass))
  (:export #:ui #:begin-frame #:ui-fb #:ui-cx #:ui-cy #:hit-at #:frame-hits
           #:row #:advance #:gap #:remaining
           #:theme #:*theme* #:make-theme
           #:icon-button #:text-field #:breadcrumb-crumb #:crumb-sep #:chip
           #:label #:spinner #:progress-bar #:divider #:fill-bg
           #:measure))
(in-package #:loom.ui)

;;; ---- theme ----------------------------------------------------------------
(defstruct theme
  (bg        (g:rgb 244 245 247))      ; chrome background
  (bg-hover  (g:rgb 228 231 236))      ; widget hover fill
  (bg-active (g:rgb 216 221 228))      ; pressed / selected fill
  (surface   (g:rgb 255 255 255))      ; input field / white surfaces
  (border    (g:rgb 208 211 216))
  (ink       (g:rgb  45  49  56))      ; primary text
  (ink-dim   (g:rgb 120 128 138))      ; secondary text (separators, labels)
  (ink-mute  (g:rgb 176 182 190))      ; disabled
  (accent    (g:rgb  47 111 235))      ; current node / focus / selection
  (accent-bg (g:rgb 224 234 255)))     ; accent-tinted fill (current crumb, active chip)
(defvar *theme* (make-theme))

;;; ---- frame context --------------------------------------------------------
(defstruct ui
  fb (w 0) (h 0)
  (cx 0) (cy 0) (row-h 0)               ; layout cursor + current row height
  (px -1) (py -1)                       ; pointer (for hover); -1 = off-widget
  (gap 6)                               ; default inter-widget gap
  (theme *theme*)
  (hits '()))                           ; (x y w h id) pushed as widgets draw

(defun begin-frame (fb &key (px -1) (py -1) (theme *theme*))
  "Start a UI frame over FB with the current pointer at (PX,PY)."
  (make-ui :fb fb :w (g:fb-width fb) :h (g:fb-height fb) :px px :py py :theme theme))

(defun frame-hits (ui) (ui-hits ui))

(defun hit-at (ui x y)
  "The action id of the topmost widget whose rect contains (X,Y), or NIL.  Hits
   were pushed front-to-back as drawn, so the last-drawn (topmost) wins first."
  (dolist (r (ui-hits ui))
    (destructuring-bind (hx hy hw hh id) r
      (when (and (<= hx x (+ hx hw)) (<= hy y (+ hy hh))) (return id)))))

(defun push-hit (ui x y w h id) (when id (push (list x y w h id) (ui-hits ui))))
(defun %hot (ui x y w h) (and (<= x (ui-px ui) (+ x w)) (<= y (ui-py ui) (+ y h))))

;;; ---- layout ---------------------------------------------------------------
(defun row (ui x y h) "Begin a horizontal row at (X,Y) with height H." (setf (ui-cx ui) x (ui-cy ui) y (ui-row-h ui) h) ui)
(defun advance (ui dx) (incf (ui-cx ui) dx) ui)
(defun gap (ui &optional (n (ui-gap ui))) (incf (ui-cx ui) n) ui)
(defun remaining (ui &optional (right-margin 6)) (max 0 (- (ui-w ui) (ui-cx ui) right-margin)))
(defun measure (text &optional (size 13)) (g:text-width text :size size))

;;; ---- small drawing helpers ------------------------------------------------
(defun round-rect (fb x y w h r color)
  "Filled rect with R-pixel rounded corners (corner pixels simply omitted — the
   chrome bg shows through, so pass small R)."
  (g:fb-rect fb (+ x r) y (- w (* 2 r)) h color)
  (g:fb-rect fb x (+ y r) w (- h (* 2 r)) color))

(defun disc (fb cx cy r color)
  (loop for dy from (- r) to r do
    (let ((dx (isqrt (max 0 (- (* r r) (* dy dy))))))
      (g:fb-hline fb (- cx dx) (+ cy dy) (1+ (* 2 dx)) color))))

;;; ---- widgets --------------------------------------------------------------
;;; Each widget draws at the layout cursor, advances it, and (if clickable) pushes
;;; a hit rect tagged ID.  It returns the width it consumed.

(defun icon-button (ui id glyph &key (enabled t) (size 28))
  "A square nav button (:back :forward :reload) at the cursor.  ID = action tag."
  (let* ((th (ui-theme ui)) (fb (ui-fb ui)) (x (ui-cx ui)) (y (ui-cy ui))
         (h (min size (ui-row-h ui))) (hot (and enabled (%hot ui x y size h)))
         (ink (cond ((not enabled) (theme-ink-mute th)) (hot (theme-ink th)) (t (theme-ink-dim th)))))
    (when hot (round-rect fb x y size h 4 (theme-bg-hover th)))
    (let ((cx (+ x (floor size 2))) (cy (+ y (floor h 2))))
      (ecase glyph
        (:back    (glyph-chevron fb cx cy :left ink))
        (:forward (glyph-chevron fb cx cy :right ink))
        (:reload  (glyph-reload fb cx cy ink))))
    (when enabled (push-hit ui x y size h id))
    (advance ui size)
    size))

(defun glyph-chevron (fb cx cy dir color)
  (let ((s 4))
    (loop for i from 0 to s
          for dx = (if (eq dir :left) (- i (floor s 2)) (- (floor s 2) i)) do
      (g:fb-vline fb (+ cx dx) (- cy i) 1 color)
      (g:fb-vline fb (+ cx dx) (+ cy i) 1 color))))

(defun glyph-reload (fb cx cy color)
  (let ((r 6))
    (loop for deg from 30 to 300 by 8
          for a = (* deg (/ pi 180d0))
          do (g:fb-rect fb (round (+ cx (* r (cos a)))) (round (+ cy (* r (sin a)))) 2 2 color))
    (g:fb-rect fb (+ cx r -2) (- cy r -1) 4 2 color)))

(defun label (ui text &key (size 12) color)
  (let* ((th (ui-theme ui)) (w (measure text size)))
    (g:fb-text (ui-fb ui) (ui-cx ui) (+ (ui-cy ui) (floor (- (ui-row-h ui) size) 2))
               text :size size :color (or color (theme-ink-dim th)))
    (advance ui w) w))

(defun crumb-sep (ui &key (glyph "›"))
  (gap ui 3) (label ui glyph :size 13) (gap ui 3))

(defun breadcrumb-crumb (ui id text &key current (size 13))
  "One clickable breadcrumb.  CURRENT gets an accent chip; others are plain links."
  (let* ((th (ui-theme ui)) (fb (ui-fb ui)) (x (ui-cx ui)) (y (ui-cy ui))
         (tw (measure text size)) (padx 8) (h (- (ui-row-h ui) 4))
         (w (+ tw (* 2 padx))) (hot (%hot ui x y w (ui-row-h ui))))
    (cond (current (round-rect fb x (+ y 2) w h 4 (theme-accent-bg th)))
          (hot     (round-rect fb x (+ y 2) w h 4 (theme-bg-hover th))))
    (g:fb-text fb (+ x padx) (+ y (floor (- (ui-row-h ui) size) 2)) text
               :size size :color (if current (theme-accent th) (theme-ink th)))
    (push-hit ui x y w (ui-row-h ui) id)
    (advance ui w) w))

(defun chip (ui id text &key active (size 12))
  "A pill in the branch rail.  ACTIVE marks the current sibling."
  (let* ((th (ui-theme ui)) (fb (ui-fb ui)) (x (ui-cx ui)) (y (ui-cy ui))
         (tw (measure text size)) (padx 9) (h (- (ui-row-h ui) 6))
         (w (+ tw (* 2 padx))) (hot (%hot ui x y w (ui-row-h ui)))
         (bg (cond (active (theme-accent-bg th)) (hot (theme-bg-hover th)) (t (theme-surface th)))))
    (round-rect fb x (+ y 3) w h 5 bg)
    (unless active (g:fb-frame fb x (+ y 3) w h (theme-border th) 1))
    (when active (g:fb-frame fb x (+ y 3) w h (theme-accent th) 1))
    (g:fb-text fb (+ x padx) (+ y (floor (- (ui-row-h ui) size) 2)) text
               :size size :color (if active (theme-accent th) (theme-ink th)))
    (push-hit ui x y w (ui-row-h ui) id)
    (advance ui w) w))

(defun text-field (ui id text &key focus (selected nil) (right-margin 6) (size 13))
  "The address/URL field, spanning to RIGHT-MARGIN from the right edge."
  (let* ((th (ui-theme ui)) (fb (ui-fb ui)) (x (ui-cx ui)) (y (ui-cy ui))
         (w (remaining ui right-margin)) (h (- (ui-row-h ui) 6)) (yy (+ y 3)))
    (round-rect fb x yy w h 5 (theme-surface th))
    (g:fb-frame fb x yy w h (if focus (theme-accent th) (theme-border th)) 1)
    (when (and focus selected (plusp (length text)))
      (g:fb-rect fb (+ x 7) (+ yy 2) (min (+ 2 (measure text size)) (- w 12)) (- h 4) (theme-accent-bg th)))
    (g:fb-text fb (+ x 8) (+ y (floor (- (ui-row-h ui) size) 2)) text :size size :color (theme-ink th))
    (when (and focus (not selected))
      (g:fb-vline fb (min (+ x 8 (measure text size) 1) (- (+ x w) 4)) (+ yy 3) (- h 6) (theme-ink th)))
    (push-hit ui x y w (ui-row-h ui) id)
    (advance ui w) w))

(defun spinner (ui frac &key (size 16))
  "A determinate arc spinner: FRAC in [0,1] of a ring lit in accent; the rest dim.
   (Animate by advancing FRAC over time; a loading node keeps it moving.)"
  (let* ((th (ui-theme ui)) (fb (ui-fb ui)) (x (ui-cx ui)) (y (ui-cy ui))
         (cx (+ x (floor size 2))) (cy (+ y (floor (ui-row-h ui) 2))) (r (floor size 2))
         (lit (round (* frac 360))))
    (loop for deg from 0 below 360 by 12
          for a = (* deg (/ pi 180d0))
          do (g:fb-rect fb (round (+ cx (* r (cos a)))) (round (+ cy (* r (sin a)))) 2 2
                        (if (< deg lit) (theme-accent th) (theme-bg-active th))))
    (advance ui size) size))

(defun progress-bar (ui frac &key (right-margin 6) (h 2))
  "A thin top-loading bar (accent) filling FRAC of the width — the classic
   page-load progress line under the toolbar."
  (let* ((th (ui-theme ui)) (fb (ui-fb ui)) (x (ui-cx ui)) (y (ui-cy ui))
         (w (remaining ui right-margin)))
    (g:fb-rect fb x y w h (theme-bg-active th))
    (g:fb-rect fb x y (round (* frac w)) h (theme-accent th))))

(defun divider (ui y &key (right-margin 0))
  (g:fb-hline (ui-fb ui) 0 y (- (ui-w ui) right-margin) (theme-border (ui-theme ui))))

(defun fill-bg (ui y0 y1)
  "Paint the chrome background for rows [Y0,Y1)."
  (g:fb-rect (ui-fb ui) 0 y0 (ui-w ui) (- y1 y0) (theme-bg (ui-theme ui))))
