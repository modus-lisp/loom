;;;; wpt-reftest-chrome.lisp — CHROME-REFERENCED WPT reftest runner.
;;;;
;;;; The sibling wpt-reftest.lisp renders BOTH the test and its reference in weft
;;;; and pixel-compares them.  That measures self-consistency, not correctness:
;;;; when weft does not implement a feature it draws the test and the reference
;;;; wrong-but-IDENTICALLY and FALSE-PASSES.  This runner fixes the methodology —
;;;; it renders the TEST in weft and the REFERENCE in Chrome (ground truth), then
;;;; fuzzy-pixel-compares.  weft passes a test only if it renders that test the way
;;;; Chrome renders the reference, so a pass is a REAL cross-engine correctness win.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --script inspect/wpt-reftest-chrome.lisp \
;;;;        <category> [limit] [wpt-root]
;;;;   e.g.  … css/css-backgrounds 40
;;;;
;;;; Pipeline:
;;;;   1. collect reftests in the category (rel=match / rel=mismatch links).
;;;;   2. resolve each test's direct match/mismatch reference paths.
;;;;   3. render every UNIQUE reference in Chrome once (batch Playwright helper,
;;;;      wpt-chrome-shot.js) -> 800x600 PNG, cached by ref path.
;;;;   4. render each TEST in weft -> 800x600 canvas.
;;;;   5. fuzzy compare: a pixel DIFFERS when its per-channel max delta exceeds a
;;;;      threshold; PASS iff the differing-pixel count is within a calibrated
;;;;      cross-engine baseline (+ the test's own <meta fuzzy> allowance).
;;;;      match => within tolerance for SOME match ref; mismatch => beyond it for
;;;;      ALL mismatch refs.
;;;;
;;;; Buckets pass/fail/error, prints chrome-ref PASS/TOTAL and failing tests with
;;;; differing-pixel counts (closest first).  DUMP=1 writes weft-test + chrome-ref
;;;; PNGs of the first few passes/fails to /tmp/wpt-reftest-chrome for eyeballing.

(require :asdf)
(let ((ql "/home/claude/quicklisp/setup.lisp")) (when (probe-file ql) (load ql)))
(push (truename "/home/claude/loom/") asdf:*central-registry*)
(push (truename "/home/claude/weft/") asdf:*central-registry*)
(push (truename "/home/claude/shuttle/") asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "loom"))

(defpackage #:wpt-ref-chrome
  (:use #:cl)
  (:local-nicknames (#:s #:weft.script) (#:r #:weft.render) (#:h #:weft.html) (#:css #:weft.css)))
(in-package #:wpt-ref-chrome)

(defparameter *args* (cdr sb-ext:*posix-argv*))
(defparameter *category* (or (first *args*) "css/css-backgrounds"))
(defparameter *limit* (and (second *args*) (parse-integer (second *args*) :junk-allowed t)))
(defparameter *wpt-root* (truename (or (third *args*) "/home/claude/wpt/")))
(defparameter *width* 800)
(defparameter *height* 600)                 ; weft canvas forced to Chrome's clip size
(defparameter *timeout* 30)
(defparameter *dump* (and (uiop:getenv "DUMP") t))
(defparameter *dump-dir* "/tmp/wpt-reftest-chrome/")
(defparameter *shot-helper* "/home/claude/loom/inspect/wpt-chrome-shot.js")
(defparameter *node* "node")
(defparameter *scratch* "/tmp/wpt-reftest-chrome-shots/")

;;; ---- calibrated cross-engine fuzzy tolerance ------------------------------
;;; A pixel counts as DIFFERING when max(|dR|,|dG|,|dB|) exceeds *CHANNEL-DELTA*.
;;; PASS when the differing-pixel count is <= *BASE-TOL-PX* (+ the test's own
;;; <meta fuzzy> totalPixels).  Calibrated EMPIRICALLY against Chrome on
;;; css-backgrounds (eyeballed weft-vs-chrome dumps for ground truth):
;;;
;;;   correct text-light / solid-color / Ahem renders  1763..5659 px
;;;     (css-border-radius-001 solid circle 1763; body-bg propagation 2021;
;;;      background-clip content/padding-box 2172..4865; image-fidelity 5659,
;;;      where the residual is weft nearest-neighbour upscaling vs Chrome's AA)
;;;   real feature failures                             6387..480000 px
;;;     (border-radius-012 unimplemented INLINE arcs render BLANK = 6387;
;;;      box-shadow missing = 28800; broken bg-propagation = 467k..480k)
;;;
;;; 6000 sits just above the clean-correct AA/scaling floor and below the
;;; missing-feature signal.  It is NOT a magic separator: border-radius's own
;;; pixel footprint on these ~100px WPT boxes (~2-4k px) overlaps cross-engine
;;; text/image AA noise, so two confusion classes are UNAVOIDABLE with a single
;;; global pixel-count threshold and are reported as such:
;;;   (a) tiny-footprint feature BUGS read as noise -> residual false-PASS risk
;;;       (pre-border-radius weft drew css-border-radius-001 as a green SQUARE,
;;;        corners wrong, but only 3895 differing px);
;;;   (b) text/image-heavy CORRECT renders inflated by cross-engine font/scaling
;;;       AA -> false-FAIL risk (border-radius-shorthand-002 draws the CORRECT
;;;       rounded box yet 7878 px, almost all bullet-list text AA).
;;; => text/image-heavy tests are LOWER-CONFIDENCE; Ahem/solid/large-region
;;;    visual features are reliable.  On an 800x600 (=480000 px) frame this is ~1.25%.
(defparameter *channel-delta* 20)
(defparameter *base-tol-px* 6000)

;;; ---- file IO --------------------------------------------------------------
(defun slurp-string (path)
  (ignore-errors
    (with-open-file (in path :external-format :utf-8 :if-does-not-exist nil)
      (and in (let ((str (make-string (file-length in)))) (subseq str 0 (read-sequence str in)))))))
(defun slurp-bytes (path)
  (ignore-errors
    (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (and in (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                (read-sequence v in) v)))))

;;; ---- subresource resolution (relative -> test dir, /abs -> WPT root) -------
(defun strip-query (url) (subseq url 0 (or (position #\? url) (position #\# url) (length url))))
(defun resolve-path (url dir)
  (let* ((c (strip-query url))
         (c (if (and (>= (length c) 7) (string-equal (subseq c 0 7) "file://")) (subseq c 7) c)))
    (cond ((zerop (length c)) nil)
          ((and (char= (char c 0) #\/) (probe-file c)) c)
          ((char= (char c 0) #\/) (merge-pathnames (subseq c 1) *wpt-root*))
          (t (merge-pathnames c dir)))))
(defun mime-for (path)
  (let ((p (string-downcase (namestring path))))
    (cond ((search ".png" p) "image/png") ((or (search ".jpg" p) (search ".jpeg" p)) "image/jpeg")
          ((search ".gif" p) "image/gif") ((search ".svg" p) "image/svg+xml")
          ((search ".webp" p) "image/webp") ((search ".bmp" p) "image/bmp")
          (t "application/octet-stream"))))
(defun file-loader (dir)
  (lambda (ctx url) (declare (ignore ctx))
    (handler-case
        (let* ((p (resolve-path url dir)) (content (and p (probe-file p) (slurp-string p))))
          (if content (values (if (search ".css" url :test #'char-equal) :css :text) content)
              (values nil nil)))
      (error () (values nil nil)))))
(defun image-loader (dir)
  (lambda (url)
    (handler-case
        (let* ((p (resolve-path url dir)) (bytes (and p (probe-file p) (slurp-bytes p))))
          (if bytes (values bytes (mime-for p)) (values nil nil)))
      (error () (values nil nil)))))

;;; ---- Ahem test font -------------------------------------------------------
(defun register-ahem ()
  (let ((path (merge-pathnames "fonts/Ahem.ttf" *wpt-root*)))
    (when (probe-file path)
      (let ((b (slurp-bytes path))) (when b (r:register-font "Ahem" b))))))

;;; ---- reftest metadata parse ----------------------------------------------
(defun attr (n name) (cdr (assoc name (h:dnode-attrs n) :test #'string-equal)))

(defun range-hi (s)
  (let* ((s (string-trim " " s))
         (dash (position #\- s :start (if (and (plusp (length s)) (char= (char s 0) #\-)) 1 0))))
    (or (parse-integer (if dash (subseq s (1+ dash)) s) :junk-allowed t) 0)))

(defun parse-fuzzy (content)
  "Parse a fuzzy meta CONTENT -> (values max-channel-delta max-pixels)."
  (when content
    (let* ((colon (position #\: content))
           (body (if (and colon (not (search "//" content)))
                     (let ((head (subseq content 0 colon)))
                       (if (or (search "difference" head :test #'char-equal)
                               (search "pixel" head :test #'char-equal))
                           content (subseq content (1+ colon))))
                     content))
           (maxd nil) (totp nil) (pos '()))
      (dolist (part (loop with st = 0 for i = (position #\; body :start st)
                          collect (subseq body st (or i (length body)))
                          while i do (setf st (1+ i))))
        (let* ((eq (position #\= part))
               (key (and eq (string-trim " " (subseq part 0 eq))))
               (val (if eq (subseq part (1+ eq)) part)))
          (cond ((and key (search "difference" key :test #'char-equal)) (setf maxd (range-hi val)))
                ((and key (search "pixel" key :test #'char-equal)) (setf totp (range-hi val)))
                (t (push (range-hi val) pos)))))
      (setf pos (nreverse pos))
      (values (or maxd (first pos) 0) (or totp (second pos) 0)))))

(defstruct meta refs fuzzy-d fuzzy-p) ; refs = ((:match|:mismatch . abspath)...)

(defun parse-meta (path)
  (let ((html (slurp-string path)))
    (when html
      (let* ((doc (h:parse-html html))
             (dir (directory-namestring (truename path)))
             (refs '()) (fd 0) (fp 0))
        (dolist (link (ignore-errors (css:query-select-all doc "link")))
          (let ((rel (attr link "rel")) (href (attr link "href")))
            (when (and rel href)
              (let ((rel (string-downcase (string-trim " " rel))))
                (cond ((string= rel "match")
                       (let ((p (resolve-path href dir))) (when p (push (cons :match p) refs))))
                      ((string= rel "mismatch")
                       (let ((p (resolve-path href dir))) (when p (push (cons :mismatch p) refs)))))))))
        (dolist (m (ignore-errors (css:query-select-all doc "meta")))
          (when (and (attr m "name") (string-equal (string-trim " " (attr m "name")) "fuzzy"))
            (multiple-value-bind (d p) (parse-fuzzy (attr m "content"))
              (setf fd (max fd d) fp (max fp p)))))
        (make-meta :refs (nreverse refs) :fuzzy-d fd :fuzzy-p fp)))))

;;; ---- weft test render -----------------------------------------------------
(defun render-test (path)
  "Render reftest PATH in weft -> canvas forced to *WIDTH*x*HEIGHT* (Chrome clip),
or :error / :timeout."
  (handler-case
      (sb-ext:with-timeout *timeout*
        (let* ((html (slurp-string path))
               (dir (directory-namestring (truename path)))
               (base (format nil "file://~a" (namestring (truename path))))
               (r:*image-loader* (image-loader dir)))
          (if html
              (values (s:render-scripted-to-canvas
                       html "" *width* :min-height *height* :max-height *height*
                       :viewport-height *height* :base base :loader (file-loader dir)))
              :error)))
    (sb-ext:timeout () :timeout)
    (error () :error)))

;;; ---- Chrome reference render (batch, cached by path) ----------------------
(defparameter *chrome-cache* (make-hash-table :test 'equal)) ; refpath -> canvas | :error

(defun ref-out-name (path)
  "A filesystem-safe PNG name for a ref PATH (its sxhash, collision-free enough)."
  (format nil "~aref-~36r.png" *scratch* (sxhash (namestring path))))

(defun batch-chrome (ref-paths)
  "Screenshot every ref in REF-PATHS in Chrome once, decode PNGs into *CHROME-CACHE*."
  (ensure-directories-exist *scratch*)
  (let ((manifest (format nil "~amanifest.txt" *scratch*))
        (todo '()))
    (dolist (p ref-paths)
      (let ((key (namestring p)))
        (unless (nth-value 1 (gethash key *chrome-cache*))
          (push (cons key (ref-out-name p)) todo))))
    (when todo
      (with-open-file (out manifest :direction :output :if-exists :supersede :external-format :utf-8)
        (dolist (e todo) (format out "1~c~a~c~a~%" #\Tab (car e) #\Tab (cdr e))))
      (format t "  [chrome] screenshotting ~d unique refs...~%" (length todo)) (finish-output)
      (let ((rc (sb-ext:process-exit-code
                 (sb-ext:run-program *node* (list *shot-helper* manifest
                                                  (princ-to-string *width*) (princ-to-string *height*))
                                     :search t :output *error-output* :error *error-output*))))
        (declare (ignore rc)))
      ;; decode each screenshot into an RGB canvas (composited over white)
      (dolist (e todo)
        (setf (gethash (car e) *chrome-cache*)
              (let ((bytes (and (probe-file (cdr e)) (slurp-bytes (cdr e)))))
                (or (and bytes (png->canvas bytes)) :error)))))))

(defun png->canvas (bytes)
  "Decode PNG BYTES (Chrome screenshot) into an RGB canvas of the runner size,
compositing straight-alpha over white.  NIL on decode failure."
  (let ((img (ignore-errors (pigment:png-decode bytes))))
    (when img
      (let* ((iw (r::img-w img)) (ih (r::img-h img)) (rgba (r::img-rgba img))
             (cv (r:make-canvas *width* *height* '(255 255 255)))
             (px (r:canvas-pixels cv)))
        (declare (type (simple-array (unsigned-byte 8) (*)) rgba px))
        (dotimes (y (min ih *height*))
          (dotimes (x (min iw *width*))
            (let* ((si (* 4 (+ (* y iw) x)))
                   (di (* 3 (+ (* y *width*) x)))
                   (a (aref rgba (+ si 3))) (ia (- 255 a)))
              (setf (aref px di)       (floor (+ (* (aref rgba si)       a) (* 255 ia)) 255)
                    (aref px (+ di 1)) (floor (+ (* (aref rgba (+ si 1)) a) (* 255 ia)) 255)
                    (aref px (+ di 2)) (floor (+ (* (aref rgba (+ si 2)) a) (* 255 ia)) 255)))))
        cv))))

;;; ---- fuzzy pixel compare --------------------------------------------------
(defun compare (a b chan-delta tol-px)
  "Compare canvases A (weft) and B (chrome).  A pixel DIFFERS when its per-channel
max delta > CHAN-DELTA.  Returns (values within-p ndiff) — WITHIN-P t iff the
differing-pixel count is <= TOL-PX.  Both canvases are the runner size."
  (if (or (not (typep a 'r:canvas)) (not (typep b 'r:canvas)))
      (values nil most-positive-fixnum)
      (let* ((w (min (r:canvas-width a) (r:canvas-width b)))
             (hh (min (r:canvas-height a) (r:canvas-height b)))
             (pa (r:canvas-pixels a)) (pb (r:canvas-pixels b))
             (wa (r:canvas-width a)) (wb (r:canvas-width b))
             (ndiff 0) (thr chan-delta))
        (declare (type (simple-array (unsigned-byte 8) (*)) pa pb)
                 (type fixnum w hh wa wb ndiff thr) (optimize (speed 3) (safety 0)))
        (dotimes (y hh)
          (let ((ra (* 3 (the fixnum (* y wa)))) (rb (* 3 (the fixnum (* y wb)))))
            (declare (type fixnum ra rb))
            (dotimes (x w)
              (let* ((ia (+ ra (the fixnum (* 3 x)))) (ib (+ rb (the fixnum (* 3 x))))
                     (dr (abs (- (aref pa ia) (aref pb ib))))
                     (dg (abs (- (aref pa (+ ia 1)) (aref pb (+ ib 1)))))
                     (db (abs (- (aref pa (+ ia 2)) (aref pb (+ ib 2)))))
                     (d (max dr dg db)))
                (declare (type fixnum ia ib dr dg db d))
                (when (> d thr) (incf ndiff))))))
        (values (<= ndiff tol-px) ndiff))))

;;; ---- evaluate one test vs its Chrome references ---------------------------
(defun eval-test (path m test-canvas)
  "Grade weft TEST-CANVAS for PATH (meta M) against its Chrome-rendered references.
Returns (values pass-p ndiff reason)."
  (when (keywordp test-canvas) (return-from eval-test (values nil most-positive-fixnum test-canvas)))
  (let* ((mrefs (remove :match (meta-refs m) :key #'car :test-not #'eq))
         (misrefs (remove :mismatch (meta-refs m) :key #'car :test-not #'eq))
         (chan (max *channel-delta* (meta-fuzzy-d m)))
         (tol (+ *base-tol-px* (meta-fuzzy-p m)))
         (best most-positive-fixnum) (match-ok (null mrefs)) (any-ref nil))
    ;; match refs: within tolerance for at least one
    (dolist (ref mrefs)
      (let ((rc (gethash (namestring (cdr ref)) *chrome-cache*)))
        (unless (keywordp rc)
          (setf any-ref t)
          (multiple-value-bind (ok nd) (compare test-canvas rc chan tol)
            (when (< nd best) (setf best nd))
            (when ok (setf match-ok t))))))
    ;; mismatch refs: beyond tolerance for ALL (use a strict 0 tolerance)
    (let ((mis-ok t))
      (dolist (ref misrefs)
        (let ((rc (gethash (namestring (cdr ref)) *chrome-cache*)))
          (if (keywordp rc)
              (setf mis-ok nil)         ; can't prove the required difference -> not a pass
              (progn (setf any-ref t)
                     (multiple-value-bind (ok nd) (compare test-canvas rc chan tol)
                       (declare (ignore nd))
                       (when ok (setf mis-ok nil)))))))
      (cond ((not any-ref) (values nil most-positive-fixnum :no-ref))
            (t (values (and match-ok mis-ok)
                       (if (= best most-positive-fixnum) 0 best) nil))))))

;;; ---- test selection -------------------------------------------------------
(defun reftest-file-p (path)
  (let ((name (string-downcase (file-namestring path))) (ns (namestring path)))
    (and (not (search "-ref." name)) (not (search "-manual." name))
         (not (search "/reference/" ns))
         (let ((head (with-open-file (in path :external-format :latin-1 :if-does-not-exist nil)
                       (and in (let ((b (make-string (min 6000 (file-length in)))))
                                 (subseq b 0 (read-sequence b in)))))))
           (and head
                (or (search "rel=\"match" head) (search "rel='match" head) (search "rel=match" head)
                    (search "rel=\"mismatch" head) (search "rel='mismatch" head) (search "rel=mismatch" head))
                t)))))

(defun collect-tests ()
  ;; TESTS env var: a newline/space-separated explicit list of test paths (relative
  ;; to the WPT root or absolute) — used for calibration and the before/after flip
  ;; without screenshotting a whole category's references.
  (let ((explicit (uiop:getenv "TESTS")))
    (when (and explicit (plusp (length explicit)))
      (return-from collect-tests
        (loop for tok in (uiop:split-string explicit :separator '(#\Newline #\Space #\Tab))
              for tok2 = (string-trim " " tok)
              when (plusp (length tok2))
              collect (let ((p (if (char= (char tok2 0) #\/) tok2
                                   (namestring (merge-pathnames tok2 *wpt-root*)))))
                        (truename p))))))
  (let* ((dir (merge-pathnames (concatenate 'string *category* "/") *wpt-root*))
         (files (append (directory (merge-pathnames "**/*.html" dir))
                        (directory (merge-pathnames "**/*.htm" dir))
                        (directory (merge-pathnames "**/*.xht" dir))
                        (directory (merge-pathnames "**/*.xhtml" dir))))
         (refs (sort (remove-if-not #'reftest-file-p files) #'string< :key #'namestring)))
    (if *limit* (subseq refs 0 (min *limit* (length refs))) refs)))

;;; ---- reporting ------------------------------------------------------------
(defun relname (path) (let ((r (namestring *wpt-root*)) (p (namestring path)))
                        (if (and (> (length p) (length r)) (string= r (subseq p 0 (length r))))
                            (subseq p (length r)) p)))

(defun dump-pair (path test-canvas m tag)
  (when (and *dump* (typep test-canvas 'r:canvas))
    (ignore-errors
      (ensure-directories-exist *dump-dir*)
      (let* ((base (substitute #\_ #\/ (relname path)))
             (ref (and m (cdr (first (meta-refs m))))))
        (r:write-png test-canvas (format nil "~a~a-~a-weft.png" *dump-dir* tag base))
        (when ref
          (let ((rc (gethash (namestring ref) *chrome-cache*)))
            (when (typep rc 'r:canvas)
              (r:write-png rc (format nil "~a~a-~a-chrome.png" *dump-dir* tag base)))))))))

;;; ---- main -----------------------------------------------------------------
(defun main ()
  (register-ahem)
  (let* ((files (collect-tests))
         (metas (make-hash-table :test 'equal))
         (buckets (make-hash-table)) (fails '())
         (ndumped-pass 0) (ndumped-fail 0))
    (format t "~&=== Chrome-referenced WPT reftest: ~a (~d reftests, ~dx~d) ===~%"
            *category* (length files) *width* *height*)
    (format t "    channel-delta=~d  base-tol=~d px (~,2f%% of frame)~%"
            *channel-delta* *base-tol-px* (* 100.0 (/ *base-tol-px* (* *width* *height*))))
    (finish-output)
    ;; pass 1: parse metas, gather unique reference paths
    (let ((refset (make-hash-table :test 'equal)))
      (dolist (f files)
        (let ((m (parse-meta (namestring f))))
          (setf (gethash (namestring f) metas) m)
          (when m (dolist (ref (meta-refs m))
                    (setf (gethash (namestring (cdr ref)) refset) (cdr ref))))))
      ;; pass 2: batch-render all refs in Chrome (cached)
      (let ((paths '())) (maphash (lambda (k v) (declare (ignore k)) (push v paths)) refset)
        (batch-chrome paths)))
    ;; pass 3: render each test in weft, compare
    (loop for f in files for i from 1 do
      (let* ((m (gethash (namestring f) metas))
             (tc (render-test (namestring f)))
             (pass nil) (nd most-positive-fixnum) (reason nil))
        (cond ((keywordp tc) (setf reason tc))
              ((null m) (setf reason :unparsable))
              (t (multiple-value-setq (pass nd reason) (eval-test (namestring f) m tc))))
        (let ((bucket (cond ((member reason '(:error :timeout :unparsable :no-ref)) :error)
                            (pass :pass) (t :fail))))
          (incf (gethash bucket buckets 0))
          (case bucket
            (:pass (when (< ndumped-pass 4) (incf ndumped-pass) (dump-pair (namestring f) tc m "PASS")))
            (:fail (push (list (relname f) nd) fails)
                   (when (< ndumped-fail 4) (incf ndumped-fail) (dump-pair (namestring f) tc m "FAIL"))))
          (format t "  [~4d/~4d] ~7a ~a~@[  diff=~d px~]~@[  (~a)~]~%"
                  i (length files) bucket (relname f)
                  (when (member bucket '(:pass :fail)) nd)
                  (when (and (eq bucket :error) reason) reason))
          (finish-output))))
    (let ((pass (gethash :pass buckets 0)) (total (length files)))
      (format t "~%=== ~a (Chrome-referenced) ===~%" *category*)
      (format t "chrome-ref match: ~d/~d~%" pass total)
      (dolist (k '(:pass :fail :error))
        (format t "  ~8a ~d~%" k (gethash k buckets 0)))
      (format t "~%--- failing tests (closest first: differing px vs Chrome ref) ---~%")
      (dolist (fl (sort fails #'< :key #'second))
        (format t "  ~9d px  ~a~%" (second fl) (first fl)))
      (when *dump*
        (format t "~%(dumped ~d pass + ~d fail weft/chrome PNG pairs to ~a)~%"
                ndumped-pass ndumped-fail *dump-dir*)))
    (finish-output)))

(main)
