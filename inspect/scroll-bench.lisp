;;;; inspect/scroll-bench.lisp — where a scroll frame's milliseconds actually go.
;;;;
;;;; Scrolling loom over VNC is the one interaction that repaints the WHOLE screen
;;;; every frame, and until now nobody had measured it end to end.  The pieces each
;;;; had a story ("the blit is a million logiors", "ZRLE is fast", "CopyRect would
;;;; help") and none had a number.  This harness produces the numbers, repeatably:
;;;;
;;;;   1. build a deterministic tall, image-heavy page (generated here — no network,
;;;;      so the same bytes every run),
;;;;   2. serve it over glass on a spare port with BOTH perf windows armed
;;;;      (glass:*perf-on* and loom.glass:*scroll-perf*),
;;;;   3. attach a real RFB client (below) so the SEND half — diff, encode, socket —
;;;;      is genuinely exercised; without a client the sender never runs and the
;;;;      expensive half of the frame is invisible,
;;;;   4. drive a scripted scroll (N wheel notches at a fixed cadence) through real
;;;;      RFB PointerEvents, and
;;;;   5. print loom's paint window, glass's send window, and the client's own
;;;;      frames/bytes side by side.
;;;;
;;;; Then it answers the question the numbers are for: glass implements CopyRect but
;;;; wires it only to WM window moves, so a scroll re-encodes all 1280x800.  Phase 2
;;;; measures the alternative directly — take the client snapshot, apply the scroll
;;;; as a snapshot-move (exactly what a scroll-aware sender would do), re-diff, and
;;;; encode what is left — and reports the ratio in both bytes and milliseconds.
;;;;
;;;; Run:
;;;;   sbcl --dynamic-space-size 4096 --non-interactive \
;;;;        --eval '(ql:quickload :loom/glass)' \
;;;;        --load inspect/scroll-bench.lisp \
;;;;        --eval '(loom.scroll-bench:run)'
;;;; Knobs: :port :steps :cadence :width :height :chrome.

(defpackage #:loom.scroll-bench
  (:use #:cl)
  (:local-nicknames (#:r #:weft.render) (#:lg #:loom.glass))
  (:export #:run #:make-page-files))

(in-package #:loom.scroll-bench)

(defun ms (ticks) (/ (* 1000.0 ticks) internal-time-units-per-second))

;;; ---------------------------------------------------------------------------
;;; A deterministic tall page
;;; ---------------------------------------------------------------------------
;;; Generated rather than saved: a fixed corpus page would drift with the network
;;; and weigh down the repo, while the thing being measured (a slice blit + a
;;; whole-screen encode) only cares that the pixels are TALL and BUSY.  The images
;;; are smooth gradients plus low-amplitude noise — photographic in the way that
;;; matters here, i.e. most ZRLE tiles blow past the 16-colour palette and go raw,
;;; the same as a real photo-heavy article.

(defun %lcg (state) (logand (+ (* state 1103515245) 12345) #xffffffff))

(defun %write-noise-png (path w h seed)
  "A W x H image: a smooth two-axis gradient plus deterministic +/-16 noise."
  (let ((cv (r:make-canvas w h))
        (s (logand (+ 12345 (* seed 7919)) #xffffffff)))
    (let ((px (r:canvas-pixels cv)))
      (dotimes (y h)
        (dotimes (x w)
          (let* ((o (* 3 (+ (* y w) x)))
                 (gr (mod (+ (* x 5) (* y 3) (* seed 31)) 256))
                 (gg (mod (+ (* x 2) (* y 7) (* seed 53)) 256))
                 (gb (mod (+ (* x 7) (* y 2) (* seed 97)) 256)))
            (setf s (%lcg s))
            (let ((n (- (ldb (byte 5 18) s) 16)))
              (setf (aref px o)       (max 0 (min 255 (+ gr n)))
                    (aref px (+ o 1)) (max 0 (min 255 (+ gg n)))
                    (aref px (+ o 2)) (max 0 (min 255 (+ gb n)))))))))
    (r:write-png cv path))
  path)

(defparameter *lorem*
  "The frame arrives in two halves and only one of them is visible from either side.
   A page painted once into a tall canvas costs nothing to scroll in principle; what
   it costs in practice is the copy of the visible slice and the bytes that copy
   forces onto the wire. Between those two the milliseconds hide, and a scroll that
   feels slow feels slow for exactly one of them.")

(defun %section-html (i img)
  (with-output-to-string (o)
    (format o "<section><h2>Section ~d</h2>~%" i)
    (format o "<p>~a</p>~%" *lorem*)
    (format o "<img src=\"~a\" width=\"640\" height=\"360\" alt=\"figure ~d\">~%" img i)
    (format o "<p>~a</p><p>~a</p>~%" *lorem* *lorem*)
    (format o "<ul>~{<li>~a</li>~}</ul>~%"
            (loop for k below 5 collect (format nil "item ~d.~d — a short list row" i k)))
    (format o "</section>~%")))

(defun make-page-files (dir &key (sections 24) (images 8) (iw 640) (ih 360))
  "Write IMAGES PNGs and an index.html of SECTIONS into DIR; return the html path.
   Regenerates only what is missing, so repeat runs skip the (slow) PNG encode."
  (ensure-directories-exist dir)
  (let ((imgs (loop for i below images
                    for name = (format nil "fig~d.png" i)
                    for path = (merge-pathnames name dir)
                    do (unless (probe-file path) (%write-noise-png path iw ih i))
                    collect name))
        (html (merge-pathnames "index.html" dir)))
    (with-open-file (o html :direction :output :if-exists :supersede)
      (format o "<!doctype html><html><head><meta charset=\"utf-8\">~%")
      (format o "<title>loom scroll bench</title><style>~%")
      (format o "body{margin:0;font:16px/1.5 sans-serif;color:#1a1a1a;background:#fff}~%")
      (format o "section{padding:24px 40px;border-bottom:1px solid #ddd}~%")
      (format o "section:nth-child(even){background:#f4f6f8}~%")
      (format o "h2{font-size:26px;margin:0 0 12px}p{margin:0 0 14px;max-width:60em}~%")
      (format o "img{display:block;margin:16px 0}li{margin:2px 0}~%")
      (format o "</style></head><body>~%")
      (dotimes (i sections)
        (write-string (%section-html i (nth (mod i (length imgs)) imgs)) o))
      (format o "</body></html>~%"))
    html))

;;; ---------------------------------------------------------------------------
;;; A minimal RFB client
;;; ---------------------------------------------------------------------------
;;; Enough of RFC 6143 to be a real load on the server and no more: handshake,
;;; SetEncodings, incremental FramebufferUpdateRequests, PointerEvents, and a
;;; parser that knows each rect's length so it can consume updates and count
;;; bytes.  It advertises only encodings whose framing is self-describing (ZRLE
;;; carries a u32 length, CopyRect is 4 bytes, Raw is w*h*4), which is why there
;;; is no Hextile bit-picking here.

(defconstant +enc-raw+ 0)
(defconstant +enc-copyrect+ 1)
(defconstant +enc-zrle+ 16)

(defstruct (rfbc (:conc-name rfbc-))
  stream socket (in 0) (frames 0) (rects 0) (copyrects 0) (raw 0) (zrle 0)
  (wlock (sb-thread:make-mutex :name "bench-rfb-write"))
  (running t) (t0 0) (t1 0))

(defun %r-u8 (c) (incf (rfbc-in c)) (read-byte (rfbc-stream c)))
(defun %r-u16 (c) (logior (ash (%r-u8 c) 8) (%r-u8 c)))
(defun %r-u32 (c) (logior (ash (%r-u16 c) 16) (%r-u16 c)))
(defun %r-skip (c n)
  (let ((buf (make-array (min n 65536) :element-type '(unsigned-byte 8))))
    (loop with left = n while (plusp left)
          for want = (min left (length buf))
          do (read-sequence buf (rfbc-stream c) :end want)
             (incf (rfbc-in c) want)
             (decf left want))))

(defun %w-u8 (s v) (write-byte (logand v #xff) s))
(defun %w-u16 (s v) (%w-u8 s (ash v -8)) (%w-u8 s v))
(defun %w-u32 (s v) (%w-u16 s (ash v -16)) (%w-u16 s v))

(defun rfb-connect (host port)
  "Handshake to a live ServerInit; return (values client width height)."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address host) port)
    (setf (sb-bsd-sockets:sockopt-tcp-nodelay sock) t)
    (let* ((s (sb-bsd-sockets:socket-make-stream sock :input t :output t
                                                 :element-type '(unsigned-byte 8) :buffering :full))
           (c (make-rfbc :stream s :socket sock)))
      (%r-skip c 12)                                     ; server ProtocolVersion
      (write-sequence (map '(simple-array (unsigned-byte 8) (*)) #'char-code
                           (format nil "RFB 003.008~c" #\Newline)) s)
      (force-output s)
      (let ((ntypes (%r-u8 c)))                          ; security types offered
        (%r-skip c ntypes)
        (%w-u8 s 1) (force-output s)                     ; choose None
        (let ((res (%r-u32 c)))
          (unless (zerop res) (error "scroll-bench: RFB security result ~d" res))))
      (%w-u8 s 1) (force-output s)                       ; ClientInit: shared
      (let ((w (%r-u16 c)) (h (%r-u16 c)))
        (%r-skip c 16)                                   ; server pixel format (we keep it)
        (%r-skip c (%r-u32 c))                           ; desktop name
        (values c w h)))))

(defun rfb-set-encodings (c)
  "Advertise ZRLE / CopyRect / Raw — no Cursor pseudo-encoding, so every byte the
   client counts is frame pixels."
  (let ((s (rfbc-stream c)))
    (sb-thread:with-mutex ((rfbc-wlock c))
      (%w-u8 s 2) (%w-u8 s 0) (%w-u16 s 3)
      (dolist (e (list +enc-zrle+ +enc-copyrect+ +enc-raw+)) (%w-u32 s e))
      (force-output s))))

(defun rfb-request (c inc x y w h)
  (let ((s (rfbc-stream c)))
    (sb-thread:with-mutex ((rfbc-wlock c))
      (%w-u8 s 3) (%w-u8 s inc) (%w-u16 s x) (%w-u16 s y) (%w-u16 s w) (%w-u16 s h)
      (force-output s))))

(defun rfb-pointer (c mask x y)
  (let ((s (rfbc-stream c)))
    (sb-thread:with-mutex ((rfbc-wlock c))
      (%w-u8 s 5) (%w-u8 s mask) (%w-u16 s x) (%w-u16 s y)
      (force-output s))))

(defun rfb-read-update (c)
  "Consume one FramebufferUpdate, tallying rects by encoding.  Returns NIL on a
   message type we did not ask for (nothing else should arrive)."
  (let ((msg (%r-u8 c)))
    (unless (zerop msg) (return-from rfb-read-update nil))
    (%r-skip c 1)
    (let ((n (%r-u16 c)))
      (dotimes (i n)
        (let ((x (%r-u16 c)) (y (%r-u16 c)) (w (%r-u16 c)) (h (%r-u16 c)) (enc (%r-u32 c)))
          (declare (ignore x y))
          (incf (rfbc-rects c))
          (cond
            ((= enc +enc-zrle+)     (incf (rfbc-zrle c)) (%r-skip c (%r-u32 c)))
            ((= enc +enc-copyrect+) (incf (rfbc-copyrects c)) (%r-skip c 4))
            ((= enc +enc-raw+)      (incf (rfbc-raw c)) (%r-skip c (* w h 4)))
            (t (error "scroll-bench: client got encoding ~d it did not advertise" enc)))))
      (incf (rfbc-frames c))
      t)))

(defun rfb-reader-loop (c w h)
  "Request / consume / request again — a real client's update pump.  Frames and
   bytes are counted between the first update after T0 and the last."
  (handler-case
      (progn
        (rfb-request c 0 0 0 w h)                        ; prime with one full frame
        (loop while (rfbc-running c)
              do (unless (rfb-read-update c) (return))
                 (when (zerop (rfbc-t0 c)) (setf (rfbc-t0 c) (get-internal-real-time)))
                 (setf (rfbc-t1 c) (get-internal-real-time))
                 (rfb-request c 1 0 0 w h)))
    (error () nil))                                       ; socket closed under us = done
  (setf (rfbc-running c) nil))

;;; ---------------------------------------------------------------------------
;;; Phase 2 — what a scroll-aware (CopyRect) sender would have cost
;;; ---------------------------------------------------------------------------
;;; Both halves go through glass's OWN encoder on the SAME framebuffer, so the
;;; comparison is not a model of the send path, it IS the send path: the only
;;; difference is whether the client's snapshot was scrolled first.

(defun %timed (thunk &key (min-ms 25.0) (max-iters 500))
  "Milliseconds per call of THUNK, batched until at least MIN-MS has elapsed.
   SBCL's GET-INTERNAL-REAL-TIME advances in 1 ms steps here (whatever
   INTERNAL-TIME-UNITS-PER-SECOND claims), and the interesting half of this
   comparison — encoding a 48-row strip — lives well under that, so a single-shot
   clock pair would report it as a flat zero and make the ratio meaningless."
  (let ((t0 (get-internal-real-time)) (n 0))
    (loop (funcall thunk)
          (incf n)
          (let ((el (ms (- (get-internal-real-time) t0))))
            (when (or (>= el min-ms) (>= n max-iters))
              (return (/ el n)))))))

(defun %encode-cost (fb rects)
  "(values ms-per-encode bytes) to write RECTS through glass's real rect emitter.
   Bytes come from glass's own TX counter, so header and payload are both counted;
   the sink is /dev/null (a real fd-stream, so no gray-stream dispatch distorts the
   clock) and the zlib stream is created once and reused, as a live client's is."
  (let ((zs (cram:make-zstream)) (bytes 0)
        (banded (glass::band-rects rects)))
    (with-open-file (s "/dev/null" :direction :output :element-type '(unsigned-byte 8)
                                   :if-exists :append)
      (let ((per (%timed (lambda ()
                           (let ((tx (list 0)))
                             (let ((glass::*tx* tx))
                               (dolist (rr banded)
                                 (destructuring-bind (x y w h) rr
                                   (glass::emit-rect s fb x y w h glass::+enc-zrle+ zs nil nil)))
                               (force-output s))
                             (setf bytes (car tx)))))))
        (values per bytes)))))

(defun %diff-cost (fb snap)
  "(values ms-per-scan rects) for glass's whole-screen dirty-tile scan."
  (let ((rects nil))
    (let ((per (%timed (lambda () (setf rects (glass::dirty-rects fb snap nil))))))
      (values per rects))))

(defun blit-floor-ms (cv fb sy chrome-h)
  "Time the SAME slice copy PAINT does, but with the two arrays declared and the
   inner loop optimised — i.e. the memory-bandwidth floor for that copy on this
   machine.  PAINT's own measured time minus this is the part that is the untyped
   inner loop rather than the work; the gap is the whole reason to measure instead
   of assuming the blit is 'just a million logiors'.  Writes the same pixels PAINT
   would, so it leaves the framebuffer correct."
  (let ((px (r:canvas-pixels cv)) (cw (r:canvas-width cv)) (ch (r:canvas-height cv))
        (fbpx (glass:fb-pixels fb)) (fbw (glass:fb-width fb)) (fbh (glass:fb-height fb)))
    (%timed
     (lambda ()
       (let ((page-h (- fbh chrome-h)) (cols (min cw fbw)))
         (declare (type (simple-array (unsigned-byte 8) (*)) px)
                  (type (simple-array (unsigned-byte 32) (*)) fbpx)
                  (type fixnum cw ch fbw fbh page-h cols sy chrome-h)
                  (optimize (speed 3) (safety 0)))
         (dotimes (y page-h)
           (declare (fixnum y))
           (let ((cy (+ sy y)) (drow (* (+ y chrome-h) fbw)))
             (declare (fixnum cy drow))
             (if (< cy ch)
                 (let ((srow (* cy cw 3)))
                   (declare (fixnum srow))
                   (dotimes (x cols)
                     (declare (fixnum x))
                     (let ((o (+ srow (* x 3))))
                       (declare (fixnum o))
                       (setf (aref fbpx (+ drow x))
                             (logior (ash (aref px o) 16) (ash (aref px (+ o 1)) 8) (aref px (+ o 2))))))
                   (loop for x fixnum from cols below fbw do (setf (aref fbpx (+ drow x)) #xffffff)))
                 (loop for x fixnum from 0 below fbw do (setf (aref fbpx (+ drow x)) #xffffff)))))))
     :min-ms 50.0)))

(defun %rect-px (rects)
  (reduce #'+ rects :key (lambda (r) (* (third r) (fourth r))) :initial-value 0))

(defun copyrect-opportunity (fb snap delta chrome-h)
  "With SNAP the client's pixels BEFORE a DELTA-px scroll down and FB the pixels
   after, price the frame both ways and return a plist.  (a) is what happens today:
   diff the whole screen and encode what changed.  (b) is what a scroll-aware
   sender would do: translate the snapshot by DELTA (glass's own SNAPSHOT-MOVE, the
   routine that already backs WM window moves), diff again — now only the newly
   exposed strip differs — and encode that plus a 12-byte CopyRect header."
  (let* ((fw (glass:fb-width fb))
         (page-h (- (glass:fb-height fb) chrome-h))
         (mv (lambda (s) (glass::snapshot-move s fw 0 (+ chrome-h delta) 0 chrome-h
                                               fw (- page-h delta)))))
    ;; (a) today: whole-screen diff, encode everything it finds
    (multiple-value-bind (dms-a rects-a) (%diff-cost fb (copy-seq snap))
      (multiple-value-bind (ems-a bytes-a) (%encode-cost fb rects-a)
        ;; (b) scroll-aware: the page block moves UP by DELTA, exposing DELTA rows.
        ;; Time the move on a scratch buffer (repeated shifts cost the same memory
        ;; traffic), then build the real post-move snapshot with one clean pass.
        (let* ((mv-ms (let ((scratch (copy-seq snap))) (%timed (lambda () (funcall mv scratch)))))
               ;; SNAPSHOT-MOVE allocates a full-size temp because a WM window move is
               ;; a sub-rectangle whose rows overlap.  A SCROLL moves FULL rows, so the
               ;; source and destination are contiguous runs of the flat array and one
               ;; REPLACE (which is defined to handle overlap) does it with no temp and
               ;; no garbage.  Price that too, so the CopyRect column isn't charged for
               ;; an allocation a scroll-aware path would never make.
               (mv-fast-ms (let ((scratch (copy-seq snap))
                                 (dst (* chrome-h fw))
                                 (src (* (+ chrome-h delta) fw))
                                 (len (* (- page-h delta) fw)))
                             (%timed (lambda ()
                                       (replace scratch scratch :start1 dst :end1 (+ dst len)
                                                                :start2 src :end2 (+ src len))))))
               (snap-b (copy-seq snap)))
          (funcall mv snap-b)
          (multiple-value-bind (dms-b rects-b) (%diff-cost fb snap-b)
            (multiple-value-bind (ems-b bytes-b) (%encode-cost fb rects-b)
              (list :delta delta
                    :full-diff-ms dms-a :full-encode-ms ems-a :full-bytes bytes-a
                    :full-rects (length rects-a) :full-px (%rect-px rects-a)
                    :cr-move-ms mv-ms :cr-move-fast-ms mv-fast-ms
                    :cr-diff-ms dms-b :cr-encode-ms ems-b
                    :cr-bytes (+ bytes-b 12)      ; + the CopyRect rect header on the wire
                    :cr-rects (length rects-b) :cr-px (%rect-px rects-b)))))))))

(defun %plist-add (a b)
  (loop for (k v) on a by #'cddr append (list k (if (numberp v) (+ v (getf b k)) v))))
(defun %plist-scale (a n)
  (loop for (k v) on a by #'cddr append (list k (if (numberp v) (/ v n) v))))

(defun copyrect-opportunity-avg (app page fb delta chrome-h &key (samples 8))
  "COPYRECT-OPPORTUNITY at SAMPLES successive scroll positions, averaged.  One
   position is not a measurement: whether the newly exposed strip lands on dense
   text or on a band of page background swings its cost by an order of magnitude
   (a background band diffs to nearly nothing and flatters the CopyRect column).
   What a scroll costs is the average over a scroll."
  (let ((acc nil))
    (dotimes (i samples)
      (let ((snap (glass::copy-pixels fb)))
        (loom:mouse-wheel page -1)
        (loom.glass::paint app)
        (let ((op (copyrect-opportunity fb snap delta chrome-h)))
          (setf acc (if acc (%plist-add acc op) op)))))
    (%plist-scale acc samples)))

;;; ---------------------------------------------------------------------------
;;; The benchmark
;;; ---------------------------------------------------------------------------

(defun %page-dir ()
  (merge-pathnames "loom-scroll-bench/" (uiop:temporary-directory)))

(defun report (&key html width height chrome-h page steps cadence wall bytes frames client
                    loom glass op paint-ms floor-ms)
  (format t "~&~%==================== loom scroll-bench ====================~%")
  (format t "page      ~a~%" html)
  (format t "screen    ~dx~d (chrome ~d px), content ~d px~%"
          width height chrome-h (loom:page-content-height page))
  (format t "drive     ~d wheel notches @ ~,1f/s, ~d px/notch  (~,1fs wall)~%"
          steps (float (/ 1 cadence)) loom:*wheel-step* wall)
  (format t "~%-- loom (paint) --~%~a" loom)
  (format t "~%-- glass (composite/diff/encode/send) --~%~a" glass)
  (format t "~%-- client (what actually crossed the socket) --~%")
  (format t "  frames     ~d in ~,1fs = ~,1f fps~%" frames wall (/ frames (max 0.001 wall)))
  (format t "  bytes      ~,1f KB total | ~,1f KB/frame | ~,0f KB/s~%"
          (/ bytes 1024.0) (if (plusp frames) (/ bytes frames 1024.0) 0.0) (/ bytes (max 0.001 wall) 1024.0))
  (format t "  rects      ~d (~d ZRLE, ~d CopyRect, ~d Raw)~%"
          (rfbc-rects client) (rfbc-zrle client) (rfbc-copyrects client) (rfbc-raw client))
  (format t "~%-- blit (one isolated scroll step, nothing else running) --~%")
  (format t "  paint          ~,2f ms as written~%" paint-ms)
  (format t "  typed floor    ~,2f ms for the same copy with the arrays declared  => ~,1fx headroom~%"
          floor-ms (/ paint-ms (max 0.001 floor-ms)))
  (format t "~%-- CopyRect opportunity (per ~,0f px scroll step, measured through glass's encoder) --~%"
          (getf op :delta))
  (format t "  (a) today      diff ~,2f ms + encode ~,2f ms = ~,2f ms | ~,1f KB | ~,1f rect(s), ~,0f px~%"
          (getf op :full-diff-ms) (getf op :full-encode-ms)
          (+ (getf op :full-diff-ms) (getf op :full-encode-ms))
          (/ (getf op :full-bytes) 1024.0) (getf op :full-rects) (getf op :full-px))
  (format t "  (b) CopyRect   move ~,2f ms + diff ~,2f ms + encode ~,2f ms = ~,2f ms | ~,1f KB | ~,1f rect(s), ~,0f px~%"
          (getf op :cr-move-fast-ms) (getf op :cr-diff-ms) (getf op :cr-encode-ms)
          (+ (getf op :cr-move-fast-ms) (getf op :cr-diff-ms) (getf op :cr-encode-ms))
          (/ (getf op :cr-bytes) 1024.0) (getf op :cr-rects) (getf op :cr-px))
  (format t "      (glass's SNAPSHOT-MOVE, which allocates a temp, would make the move ~,2f ms)~%"
          (getf op :cr-move-ms))
  (let ((ta (+ (getf op :full-diff-ms) (getf op :full-encode-ms)))
        (tb (+ (getf op :cr-move-fast-ms) (getf op :cr-diff-ms) (getf op :cr-encode-ms))))
    (format t "  ratio          ~,1fx less time, ~,1fx fewer bytes~%"
            (/ ta (max 0.001 tb))
            (/ (getf op :full-bytes) (float (max 1 (getf op :cr-bytes))))))
  (format t "===========================================================~%")
  (force-output))

(defun run (&key (port 5915) (steps 120) (cadence 1/60) (width 1280) (height 800)
                 (chrome nil) (sections 24) (dir (%page-dir)) (settle 1.0) (client t))
  "Serve a generated tall page on PORT, drive STEPS wheel notches at CADENCE
   seconds apart through a real RFB client, and print the combined breakdown.
   CHROME serves the page under loom's browser toolbar (the WM's configuration);
   the default is a bare page, which is what SERVE / RUN-GLASS give you.
   :CLIENT NIL is the control run — nobody connects, so the sender never encodes
   and the wheel events go straight into ON-POINTER; whatever paint rate that
   reaches is the pump's ceiling with the SEND half removed, which is the only
   honest way to say whether the blit or the encode is holding the frame rate."
  (let* ((html (make-page-files dir :sections sections))
         (chrome-h (if chrome loom.glass::+chrome-h+ 0))
         (vh (- height chrome-h))
         (fb (glass:make-framebuffer width height (glass:rgb 255 255 255)))
         (app (progn (format t "~&scroll-bench: rendering ~a (~dx~d viewport)...~%" html width vh)
                     (if chrome
                         (lg:attach-browser (namestring html) fb)
                         (let ((p (loom:load-file (namestring html) :width width :viewport-height vh)))
                           (loom:render-page p)
                           (lg:attach p fb)))))
         (page (loom.glass::glass-app-page app)))
    (format t "~&scroll-bench: page ~dx~d, content-height ~d px (~,1f viewports)~%"
            width vh (loom:page-content-height page)
            (/ (loom:page-content-height page) (float vh)))
    ;; --- serve + pump ---
    (sb-thread:make-thread
     (lambda () (glass:serve fb port
                             :on-key (lambda (d k) (lg:on-key app d k))
                             :on-pointer (lambda (m x y) (lg:on-pointer app m x y))
                             :name "loom-scroll-bench"))
     :name "bench-rfb")
    (sb-thread:make-thread (lambda () (lg:pump-loop app)) :name "bench-pump")
    (sleep 0.4)
    ;; --- client (optional: NIL is the no-SEND control) ---
    (multiple-value-bind (c cw ch)
        (if client (rfb-connect "127.0.0.1" port) (values (make-rfbc) width height))
      (declare (ignorable cw ch))
      (when client (rfb-set-encodings c))
      (let ((reader (when client
                      (sb-thread:make-thread (lambda () (rfb-reader-loop c cw ch)) :name "bench-client"))))
        (sleep settle)                                     ; let the first full frame land
        ;; --- arm both perf windows and scroll ---
        (setf glass:*perf-on* t lg:*scroll-perf* t)
        (glass:perf-reset) (lg:scroll-perf-reset)
        (let* ((c0 (rfbc-in c)) (f0 (rfbc-frames c)) (w0 (get-internal-real-time))
               (px (floor width 2)) (py (+ chrome-h (floor (- height chrome-h) 2))))
          (dotimes (i steps)
            (cond (client (rfb-pointer c 16 px py)         ; wheel-down notch, over the wire
                          (rfb-pointer c 0 px py))         ; and its release
                  (t (lg:on-pointer app 16 px py)          ; control: straight into the shell
                     (lg:on-pointer app 0 px py)))
            (sleep cadence))
          (sleep 0.5)                                      ; drain the last frames
          (let* ((wall (/ (- (get-internal-real-time) w0) (float internal-time-units-per-second)))
                 (bytes (- (rfbc-in c) c0))
                 (frames (- (rfbc-frames c) f0))
                 (loom-report (lg:scroll-perf-report))
                 (glass-report (glass:perf-report)))
            ;; --- phase 2: price the same scroll step with and without CopyRect ---
            (lg:stop app)                                  ; pump off — we drive paint by hand now
            (sleep 0.1)
            (setf (rfbc-running c) nil)
            (when client
              (ignore-errors (close (rfbc-stream c)))
              (ignore-errors (sb-thread:join-thread reader :timeout 2)))
            (let ((delta loom:*wheel-step*)
                  ;; PAINT is idempotent at a fixed scroll offset, so it can be batched
                  ;; against the same 1 ms clock the typed floor is measured on.
                  (paint-ms (%timed (lambda () (loom.glass::paint app)) :min-ms 200.0)))
              (let* ((floor-ms (blit-floor-ms (loom:page-canvas page) fb
                                              (loom:page-scroll-y page) chrome-h))
                     (op (copyrect-opportunity-avg app page fb delta chrome-h)))
                (report :html html :width width :height height :chrome-h chrome-h
                        :page page :steps steps :cadence cadence :wall wall
                        :bytes bytes :frames frames :client c
                        :loom loom-report :glass glass-report :op op
                        :paint-ms paint-ms :floor-ms floor-ms)
                op))))))))
