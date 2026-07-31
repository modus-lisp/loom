;;;; scroll-perf.lisp — per-frame counters for the glass shell's paint path.
;;;;
;;;; glass measures the half of a frame it owns (composite + diff/encode/send —
;;;; glass:perf-report).  It cannot see the half loom owns: the PAINT that copies
;;;; the visible slice of weft's tall page canvas into the framebuffer, and the
;;;; pump loop's cadence around it.  Scrolling never re-lays-out (the whole page
;;;; is already painted into one canvas), so a scroll frame is exactly that blit
;;;; plus whatever glass then has to ship — and until both halves are on the same
;;;; clock there is no way to say which one sets the frame rate.
;;;;
;;;; These counters are the loom half, in glass's style: a few INCFs behind
;;;; *SCROLL-PERF* (OFF by default, so the normal path is untouched), read as a
;;;; string alongside glass's:
;;;;
;;;;   (setf loom.glass:*scroll-perf* t) (loom.glass:scroll-perf-reset)
;;;;   ... scroll ...
;;;;   (loom.glass:scroll-perf-report)     ; and (glass:perf-report)
;;;;
;;;; Only whole-blit timing is taken (one GET-INTERNAL-REAL-TIME pair per paint);
;;;; nothing is timed per pixel.

(in-package #:loom.glass)

(defparameter *scroll-perf* nil
  "Whether the paint/pump counters accumulate.  OFF by default: with it NIL the
   hot path takes one special-variable read per paint and nothing else.")

(defstruct (spf (:conc-name spf-))
  (lock (sb-thread:make-mutex :name "loom-scroll-perf"))
  (t0 (get-internal-real-time))
  ;; --- pump loop ---
  (pumps 0)                             ; pump-loop iterations (painted or not)
  ;; --- paint ---
  (paints 0) (paint-ticks 0) (paint-max 0)
  (chrome-ticks 0)                      ; of PAINT-TICKS, the part spent drawing the chrome
  ;; --- what the paint was for ---
  (scroll-paints 0)                     ; paints where the scroll offset moved
  (scroll-px 0)                         ; total |delta| scrolled across those paints
  (scroll-max 0)                        ; largest single-paint |delta|
  (still-paints 0)                      ; paints with NO observable change (same offset, same canvas)
  (wheels 0)                            ; wheel notches arriving from RFB
  ;; previous-paint state, to classify the next one
  (last-scroll -1) (last-canvas nil))

(defvar *spf* (make-spf))

(defun scroll-perf-reset ()
  "Zero the scroll-perf window."
  (setf *spf* (make-spf))
  t)

(declaim (inline note-pump))
(defun note-pump ()
  "One pump-loop iteration (whether or not it painted)."
  (when *scroll-perf*
    (let ((p *spf*)) (sb-thread:with-mutex ((spf-lock p)) (incf (spf-pumps p)))))
  nil)

(defun note-wheel (n)
  "N wheel notches arrived on one RFB pointer event."
  (when *scroll-perf*
    (let ((p *spf*)) (sb-thread:with-mutex ((spf-lock p)) (incf (spf-wheels p) n))))
  nil)

(defun note-paint (ticks chrome-ticks scroll-y canvas)
  "One PAINT: TICKS for the whole blit (CHROME-TICKS of it drawing the chrome),
   landing at SCROLL-Y on CANVAS.  Comparing those two against the previous paint
   says what the paint was FOR — a scroll (and by how much), or nothing at all."
  (when *scroll-perf*
    (let ((p *spf*))
      (sb-thread:with-mutex ((spf-lock p))
        (incf (spf-paints p))
        (incf (spf-paint-ticks p) ticks)
        (incf (spf-chrome-ticks p) chrome-ticks)
        (when (> ticks (spf-paint-max p)) (setf (spf-paint-max p) ticks))
        (let ((moved (and (>= (spf-last-scroll p) 0) (/= scroll-y (spf-last-scroll p)))))
          (cond
            (moved
             (let ((d (abs (- scroll-y (spf-last-scroll p)))))
               (incf (spf-scroll-paints p))
               (incf (spf-scroll-px p) d)
               (when (> d (spf-scroll-max p)) (setf (spf-scroll-max p) d))))
            ;; same offset AND the same canvas object: whatever set DIRTY changed
            ;; nothing this paint could show (hover, a key, an idle timer tick).
            ((and (>= (spf-last-scroll p) 0) (eq canvas (spf-last-canvas p)))
             (incf (spf-still-paints p)))))
        (setf (spf-last-scroll p) scroll-y (spf-last-canvas p) canvas))))
  nil)

(defun %spf-ms (ticks) (/ (* 1000.0 ticks) internal-time-units-per-second))

(defun scroll-perf-report ()
  "A human-readable snapshot of the paint window since the last SCROLL-PERF-RESET,
   shaped to read next to GLASS:PERF-REPORT."
  (let ((p *spf*))
    (sb-thread:with-mutex ((spf-lock p))
      (let* ((el (max 0.001 (/ (- (get-internal-real-time) (spf-t0 p))
                               (float internal-time-units-per-second))))
             (n (spf-paints p)))
        (with-output-to-string (o)
          (format o "loom scroll perf — ~,1fs window~:[~; (scroll-perf OFF)~]~%" el (not *scroll-perf*))
          (format o "  PUMP       ~d iterations, ~,1f/s~%" (spf-pumps p) (/ (spf-pumps p) el))
          (format o "  PAINT      ~d, ~,1f/s (~,1f% of pumps)~%"
                  n (/ n el) (if (plusp (spf-pumps p)) (* 100.0 (/ n (spf-pumps p))) 0.0))
          (when (plusp n)
            (format o "    blit       ~,2f ms/paint (max ~,2f ms) | ~,1f% of wall~%"
                    (%spf-ms (/ (spf-paint-ticks p) n)) (%spf-ms (spf-paint-max p))
                    (* 100.0 (/ (%spf-ms (spf-paint-ticks p)) (* 1000.0 el))))
            (format o "    of which chrome ~,2f ms/paint~%" (%spf-ms (/ (spf-chrome-ticks p) n))))
          (format o "  SCROLL     ~d wheel notches -> ~d scroll paints (~,2f paints/notch)~%"
                  (spf-wheels p) (spf-scroll-paints p)
                  (if (plusp (spf-wheels p)) (/ (spf-scroll-paints p) (float (spf-wheels p))) 0.0))
          (when (plusp (spf-scroll-paints p))
            (format o "    delta      ~,1f px/paint (max ~d px) | ~d px total~%"
                    (/ (spf-scroll-px p) (float (spf-scroll-paints p)))
                    (spf-scroll-max p) (spf-scroll-px p)))
          (format o "    no-change  ~d paints repainted an identical view~%" (spf-still-paints p)))))))
