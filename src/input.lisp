;;;; src/input.lisp — pure input translation and viewport math.
;;;;
;;;; No SDL here: these are the small, correctness-critical functions that turn
;;;; raw pointer/wheel numbers into DOM values and clamp the scroll position.
;;;; They are unit-tested headlessly (inspect/tests.lisp).
(in-package #:loom)

(defun sdl-button->dom (sdl-button)
  "Map an SDL mouse button number (1 left, 2 middle, 3 right) to the DOM
   MouseEvent.button code (0 left, 1 middle, 2 right)."
  (case sdl-button (1 0) (2 1) (3 2) (t 0)))

(defparameter *wheel-step* 48
  "Pixels of scroll per mouse-wheel notch.")

(defun wheel->scroll-delta (wheel-y &optional (step *wheel-step*))
  "Pixels to add to scroll-y for an SDL wheel event whose Y is WHEEL-Y.  SDL
   reports +Y for scrolling up (away from the user); the page scrolls the
   opposite way, so a positive wheel-y yields a negative (upward) delta."
  (* (- wheel-y) step))

(defun clamp-scroll (y content-height viewport-height)
  "Clamp a desired scroll-y Y to [0, max(0, content-height - viewport-height)]."
  (max 0 (min (round y) (max 0 (- content-height viewport-height)))))

(defun url-prefix-p (prefix s)
  (and (stringp s) (>= (length s) (length prefix))
       (string-equal (subseq s 0 (length prefix)) prefix)))

(defun resolve-url (href base)
  "Resolve HREF against BASE (both strings) to an absolute URL string via the
   WHATWG URL parser, or NIL if it cannot be resolved.  An absolute HREF ignores
   BASE; a relative or fragment HREF resolves against it."
  (when (and (stringp href) (plusp (length href)))
    (let ((u (ignore-errors
              (if (and base (plusp (length base))) (url:parse href base) (url:parse href)))))
      (and u (url:href u)))))
