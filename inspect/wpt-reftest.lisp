;;;; wpt-reftest.lisp — run WPT REFTESTS in weft and grade them by pixel-compare.
;;;;
;;;; A WPT reftest is an HTML file carrying <link rel="match" href="REF"> (must
;;;; render IDENTICALLY to REF) and/or <link rel="mismatch" href="REF"> (must
;;;; render DIFFERENTLY).  References may themselves be reftests, forming a chain:
;;;; a node passes iff (some match ref satisfied AND all mismatch refs satisfied),
;;;; recursively down to a leaf reference with no links of its own.  Optional
;;;;   <meta name="fuzzy" content="maxDifference=A-B;totalPixels=C-D">
;;;; relaxes the pixel comparison (up to totalPixels-hi pixels may each differ by
;;;; up to maxDifference-hi per channel).
;;;;
;;;; Unlike wpt-vs-chrome.lisp (which grades laid-out box GEOMETRY against Chrome),
;;;; this renders BOTH the test and its reference IN WEFT and pixel-compares them —
;;;; exactly what WPT / Ladybird do.  weft's rasteriser is deterministic, so a test
;;;; and a reference that lay out identically produce byte-identical canvases; any
;;;; difference is a real weft rendering difference, surfaced with its magnitude so
;;;; near-misses are visible.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --script inspect/wpt-reftest.lisp \
;;;;        <category> [limit] [wpt-root]
;;;;   e.g.  … css/css-flexbox 200
;;;;
;;;; Buckets: pass / fail / error / skip.  Prints reftest PASS/TOTAL, the buckets,
;;;; and every failing test with its differing-pixel count + max channel delta
;;;; (sorted closest-first) so the near-passes are obvious.  DUMP=1 writes the
;;;; test/ref PNGs of the first few passes+fails to /tmp/wpt-reftest for eyeballing.

(require :asdf)
(let ((ql "/home/claude/quicklisp/setup.lisp")) (when (probe-file ql) (load ql)))
(push (truename "/home/claude/loom/") asdf:*central-registry*)
(push (truename "/home/claude/weft/") asdf:*central-registry*)
(push (truename "/home/claude/shuttle/") asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "loom"))

(defpackage #:wpt-ref
  (:use #:cl)
  (:local-nicknames (#:s #:weft.script) (#:r #:weft.render) (#:h #:weft.html) (#:css #:weft.css)))
(in-package #:wpt-ref)

(defparameter *args* (cdr sb-ext:*posix-argv*))
(defparameter *category* (or (first *args*) "css/css-flexbox"))
(defparameter *limit* (and (second *args*) (parse-integer (second *args*) :junk-allowed t)))
(defparameter *wpt-root* (truename (or (third *args*) "/home/claude/wpt/")))
(defparameter *width* 800)
(defparameter *min-height* 600)
(defparameter *max-height* 3000)
(defparameter *timeout* 30)
(defparameter *dump* (and (uiop:getenv "DUMP") t))
(defparameter *dump-dir* "/tmp/wpt-reftest/")

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
  "Resolve a subresource/reference URL to a local file path."
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

;;; ---- Ahem test font (most CSS reftests measure with it) -------------------
(defun register-ahem ()
  (let ((path (merge-pathnames "fonts/Ahem.ttf" *wpt-root*)))
    (when (probe-file path)
      (let ((b (slurp-bytes path))) (when b (r:register-font "Ahem" b))))))

;;; ---- reftest metadata parse ----------------------------------------------
(defun attr (n name) (cdr (assoc name (h:dnode-attrs n) :test #'string-equal)))

(defun range-hi (s)
  "Upper bound of a WPT range token \"lo-hi\" (or a bare \"n\")."
  (let* ((s (string-trim " " s))
         (dash (position #\- s :start (if (and (plusp (length s)) (char= (char s 0) #\-)) 1 0))))
    (or (parse-integer (if dash (subseq s (1+ dash)) s) :junk-allowed t) 0)))

(defun parse-fuzzy (content)
  "Parse a fuzzy meta CONTENT string -> (values max-channel-delta max-pixels).
Handles maxDifference=/totalPixels= labels, bare positional \"A-B;C-D\", and a
leading scope prefix (\"ref.html:...\") which is stripped."
  (when content
    ;; strip a scope prefix like  some-ref.html:maxDifference=...
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
  "Parse reftest PATH: return a META with its match/mismatch refs (absolute paths)
and combined fuzzy tolerance, or NIL if the file cannot be read."
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

;;; ---- render (cached) ------------------------------------------------------
(defparameter *canvas-cache* (make-hash-table :test 'equal))

(defun render-path (path)
  "Render reftest node PATH -> canvas, or :error / :timeout.  Cached per path."
  (let ((key (namestring (truename path))))
    (multiple-value-bind (v hit) (gethash key *canvas-cache*)
      (if hit v
          (setf (gethash key *canvas-cache*)
                (handler-case
                    (sb-ext:with-timeout *timeout*
                      (let* ((html (slurp-string path))
                             (dir (directory-namestring (truename path)))
                             (base (format nil "file://~a" (namestring (truename path))))
                             (r:*image-loader* (image-loader dir)))
                        (if html
                            (values (s:render-scripted-to-canvas
                                     html "" *width* :min-height *min-height*
                                     :max-height *max-height* :base base :loader (file-loader dir)))
                            :error)))
                  (sb-ext:timeout () :timeout)
                  (error () :error)))))))

;;; ---- pixel compare --------------------------------------------------------
(defun compare-canvas (a b maxd totp)
  "Compare canvases A and B under fuzzy tolerance (MAXD channel delta, TOTP pixels).
Returns (values match-p ndiff worst) — MATCH-P t iff dimensions are equal AND the
render is equal within tolerance.  NDIFF is an honest differing-pixel count taken
over the UNION of the two canvases (pixels outside the shared overlap count as
differing), so a small size difference reports a small NDIFF (a near-miss) rather
than masquerading as 0.  WORST = largest per-channel delta in the overlap."
  (if (or (not (typep a (quote r:canvas))) (not (typep b (quote r:canvas))))
      (values nil most-positive-fixnum 255)
      (let* ((wa (r:canvas-width a)) (ha (r:canvas-height a))
             (wb (r:canvas-width b)) (hb (r:canvas-height b))
             (ow (min wa wb)) (oh (min ha hb))
             (pa (r:canvas-pixels a)) (pb (r:canvas-pixels b))
             (ndiff 0) (worst 0) (same-dims (and (= wa wb) (= ha hb))))
        (declare (type (simple-array (unsigned-byte 8) (*)) pa pb)
                 (type fixnum wa ha wb hb ow oh ndiff worst) (optimize (speed 3) (safety 0)))
        ;; differing pixels within the shared overlap
        (dotimes (y oh)
          (let ((ra (* 3 (the fixnum (* y wa)))) (rb (* 3 (the fixnum (* y wb)))))
            (declare (type fixnum ra rb))
            (dotimes (x ow)
              (let* ((ia (+ ra (the fixnum (* 3 x)))) (ib (+ rb (the fixnum (* 3 x))))
                     (dr (abs (- (aref pa ia) (aref pb ib))))
                     (dg (abs (- (aref pa (+ ia 1)) (aref pb (+ ib 1)))))
                     (db (abs (- (aref pa (+ ia 2)) (aref pb (+ ib 2)))))
                     (d (max dr dg db)))
                (declare (type fixnum ia ib dr dg db d))
                (when (plusp d) (incf ndiff) (when (> d worst) (setf worst d)))))))
        ;; pixels present in one canvas but not the other always differ
        (incf ndiff (- (+ (* wa ha) (* wb hb)) (* 2 (* ow oh))))
        (unless same-dims (setf worst (max worst 255)))
        (values (and same-dims (<= worst maxd) (<= ndiff totp)) ndiff worst))))

;;; ---- recursive reftest evaluation -----------------------------------------
(defun eval-node (path visited)
  "Evaluate reftest node PATH: does it render self-consistently with its reference
chain?  Returns (values pass-p ndiff worst reason) — NDIFF/WORST describe the
closest match ref (for reporting).  REASON is a keyword when it can't be graded."
  (let ((canvas (render-path path)))
    (when (keywordp canvas) (return-from eval-node (values nil most-positive-fixnum 255 canvas)))
    (let ((m (parse-meta path)))
      (unless m (return-from eval-node (values nil most-positive-fixnum 255 :unparsable)))
      (let ((mrefs (remove :match (meta-refs m) :key #'car :test-not #'eq))
            (misrefs (remove :mismatch (meta-refs m) :key #'car :test-not #'eq)))
        ;; leaf reference: no links of its own -> trivially self-consistent
        (when (and (null mrefs) (null misrefs)) (return-from eval-node (values t 0 0 nil)))
        (let ((match-ok (null mrefs)) (best-ndiff most-positive-fixnum) (best-worst 255))
          ;; match refs: at least one must pixel-match AND itself pass its chain
          (block match-loop
            (dolist (ref mrefs)
              (let ((rp (cdr ref)))
                (unless (member (namestring rp) visited :test #'equal)
                  (let ((rc (render-path rp)))
                    (unless (keywordp rc)
                      (let ((rm (parse-meta rp)))
                        (multiple-value-bind (eq nd wo)
                            (compare-canvas canvas rc
                                            (max (meta-fuzzy-d m) (if rm (meta-fuzzy-d rm) 0))
                                            (max (meta-fuzzy-p m) (if rm (meta-fuzzy-p rm) 0)))
                          (if eq
                              ;; matches this ref — now the ref must itself pass its chain
                              (multiple-value-bind (sub-pass sub-nd sub-wo)
                                  (eval-node rp (cons (namestring rp) visited))
                                (if sub-pass
                                    (progn (setf match-ok t best-ndiff nd best-worst wo)
                                           (return-from match-loop))
                                    ;; matched the immediate ref but the ref's own chain
                                    ;; broke deeper — surface that deeper diff as the distance
                                    (when (< sub-nd best-ndiff)
                                      (setf best-ndiff sub-nd best-worst sub-wo))))
                              (when (< nd best-ndiff) (setf best-ndiff nd best-worst wo)))))))))))
          ;; mismatch refs: ALL must differ
          (let ((mis-ok t))
            (dolist (ref misrefs)
              (let* ((rp (cdr ref)) (rc (render-path rp)))
                (if (keywordp rc)
                    (setf mis-ok nil)
                    (multiple-value-bind (eq nd wo) (compare-canvas canvas rc 0 0)
                      (declare (ignore nd wo))
                      (when eq (setf mis-ok nil))))))
            (values (and match-ok mis-ok)
                    (if (= best-ndiff most-positive-fixnum) 0 best-ndiff) best-worst nil)))))))

;;; ---- test selection -------------------------------------------------------
(defun reftest-file-p (path)
  "PATH is a reftest we grade: has a rel=match|mismatch <link>, is not a -ref /
reference / -manual file."
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
  (let* ((dir (merge-pathnames (concatenate 'string *category* "/") *wpt-root*))
         (files (append (directory (merge-pathnames "**/*.html" dir))
                        (directory (merge-pathnames "**/*.htm" dir))
                        (directory (merge-pathnames "**/*.xht" dir))
                        (directory (merge-pathnames "**/*.xhtml" dir))))
         (refs (sort (remove-if-not #'reftest-file-p files) #'string< :key #'namestring)))
    (if *limit* (subseq refs 0 (min *limit* (length refs))) refs)))

;;; ---- reporting helpers ----------------------------------------------------
(defun relname (path) (let ((r (namestring *wpt-root*)) (p (namestring path)))
                        (if (and (> (length p) (length r)) (string= r (subseq p 0 (length r))))
                            (subseq p (length r)) p)))

(defun dump-pair (test tag)
  (when *dump*
    (ignore-errors
      (ensure-directories-exist *dump-dir*)
      (let* ((base (substitute #\_ #\/ (relname test)))
             (tc (render-path test)) (m (parse-meta test))
             (ref (and m (cdr (first (meta-refs m))))))
        (when (typep tc (quote r:canvas)) (r:write-png tc (format nil "~a~a-~a-test.png" *dump-dir* tag base)))
        (when ref (let ((rc (render-path ref)))
                    (when (typep rc (quote r:canvas)) (r:write-png rc (format nil "~a~a-~a-ref.png" *dump-dir* tag base)))))))))

;;; ---- main -----------------------------------------------------------------
(defun main ()
  (register-ahem)
  (let ((files (collect-tests))
        (buckets (make-hash-table)) (fails '()) (passes '())
        (ndumped-pass 0) (ndumped-fail 0))
    (format t "~&=== WPT reftest: ~a (~d reftests, ~dx~d) ===~%"
            *category* (length files) *width* *min-height*)
    (finish-output)
    (loop for f in files for i from 1 do
      (clrhash *canvas-cache*)          ; keep memory flat across tests
      (multiple-value-bind (pass nd wo reason)
          (handler-case (eval-node (namestring f) (list (namestring (truename f))))
            (error () (values nil 0 0 :error)))
        (let* ((top (render-path (namestring f)))
               (bucket (cond ((eq reason :timeout) :error)
                             ((member reason '(:error :unparsable)) :error)
                             ((keywordp top) :error)
                             (pass :pass) (t :fail)))
               (ink (if (typep top (quote r:canvas)) (r:canvas-ink top) 0.0)))
          (incf (gethash bucket buckets 0))
          (case bucket
            (:pass (push (list (relname f) nd wo ink) passes)
                   (when (< ndumped-pass 4) (incf ndumped-pass) (dump-pair (namestring f) "PASS")))
            (:fail (push (list (relname f) nd wo ink) fails)
                   (when (< ndumped-fail 4) (incf ndumped-fail) (dump-pair (namestring f) "FAIL"))))
          (format t "  [~4d/~4d] ~7a ~a~@[  diff=~d px, dmax=~d~]~%"
                  i (length files) bucket (relname f)
                  (when (eq bucket :fail) nd) (when (eq bucket :fail) wo))
          (finish-output))))
    (let ((pass (gethash :pass buckets 0)) (total (length files)))
      (format t "~%=== ~a ===~%" *category*)
      (format t "reftest match: ~d/~d~%" pass total)
      (dolist (k '(:pass :fail :error))
        (format t "  ~8a ~d~%" k (gethash k buckets 0)))
      (format t "~%--- failing reftests (closest first: differing px / max channel delta) ---~%")
      (dolist (fl (sort fails #'< :key #'second))
        (destructuring-bind (name nd wo ink) fl
          (format t "  ~9d px  dmax ~3d  ~a~@[  [test ink ~,3f]~]~%"
                  nd wo name (when (< ink 0.001) ink))))
      (when *dump*
        (format t "~%(dumped ~d pass + ~d fail PNG pairs to ~a)~%" ndumped-pass ndumped-fail *dump-dir*)))
    (finish-output)))

(main)
