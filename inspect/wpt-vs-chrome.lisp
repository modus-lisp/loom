;;;; wpt-vs-chrome.lisp — Chrome-REFERENCED WPT structural audit.
;;;;
;;;; The self-referential reftest harness (weft renders both test and reference)
;;;; passes many tests as false matches: weft renders both sides wrong-but-identical.
;;;; This instead renders each WPT *test* in weft AND in Chromium and compares the
;;;; laid-out box GEOMETRY element-by-element, keyed by structural DOM path — Chrome
;;;; is the ground truth, so "same structure as Chrome" is measured directly and
;;;; there are no false passes.  A test structurally matches when (nearly) every
;;;; element weft and Chrome share is the same size and position.
;;;;
;;;; Chromium is driven by hn-audit-chrome.js (Playwright, JS off) — the one external
;;;; edge; weft rendering, the diff and the scoring are Lisp.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --script inspect/wpt-vs-chrome.lisp \
;;;;        <wpt-root> <category> [limit] [width]
;;;;   e.g.  … /home/claude/wpt css/css-sizing 200
(require :asdf)
(defparameter *loom-dir* (truename "/home/claude/loom/"))
(push *loom-dir* asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "loom"))

(defpackage #:wpt-cmp (:use #:cl))
(in-package #:wpt-cmp)

(defparameter *args* (cdr sb-ext:*posix-argv*))
(defparameter *wpt-root* (truename (or (first *args*) "/home/claude/wpt/")))
(defparameter *category* (or (second *args*) "css/css-sizing"))
(defparameter *limit* (and (third *args*) (parse-integer (third *args*) :junk-allowed t)))
(defparameter *width* (or (and (fourth *args*) (parse-integer (fourth *args*) :junk-allowed t)) 800))
(defparameter *vh* 900)                 ; viewport height — matches hn-audit-chrome.js
(defparameter *timeout* 40)
(defparameter *work* "/tmp/wpt-cmp")
(defparameter *chrome-js* (namestring (merge-pathnames "inspect/hn-audit-chrome.js" cl-user::*loom-dir*)))
(defparameter *tol* 8)                  ; px tolerance
(defparameter *pass-threshold* 4.0)     ; structural-score below this = "matches Chrome"

;;; ---- local file subresource loaders (CSS/text + images) -------------------
(defun read-file-string (path)
  (ignore-errors
    (with-open-file (in path :external-format :utf-8 :if-does-not-exist nil)
      (and in (let ((s (make-string (file-length in)))) (subseq s 0 (read-sequence s in)))))))
(defun read-file-bytes (path)
  (ignore-errors
    (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (and in (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8)))) (read-sequence v in) v)))))
(defun strip-query (url) (subseq url 0 (or (position #\? url) (position #\# url) (length url))))
(defun resolve-path (url test-dir)
  (let* ((clean (strip-query url))
         (clean (if (and (>= (length clean) 7) (string-equal (subseq clean 0 7) "file://")) (subseq clean 7) clean)))
    (cond ((and (plusp (length clean)) (char= (char clean 0) #\/) (probe-file clean)) clean)
          ((and (plusp (length clean)) (char= (char clean 0) #\/)) (merge-pathnames (subseq clean 1) *wpt-root*))
          (t (merge-pathnames clean test-dir)))))
(defun mime-for (path)
  (let ((p (string-downcase (namestring path))))
    (cond ((search ".png" p) "image/png") ((or (search ".jpg" p) (search ".jpeg" p)) "image/jpeg")
          ((search ".gif" p) "image/gif") ((search ".svg" p) "image/svg+xml") (t "application/octet-stream"))))
(defun file-loader (test-dir)
  (lambda (ctx url) (declare (ignore ctx))
    (let ((content (read-file-string (resolve-path url test-dir))))
      (if content (values (if (search ".css" url :test #'char-equal) :css :text) content) (values nil nil)))))
(defun file-image-loader (test-dir)
  (lambda (url)
    (let* ((path (resolve-path url test-dir)) (bytes (read-file-bytes path)))
      (if bytes (values bytes (mime-for path)) (values nil nil)))))

;;; ---- structural DOM path (matches hn-audit-chrome.js pathOf) ---------------
(defun path-of (n)
  (let ((parts '()))
    (loop for e = n then (weft.html:dnode-parent e)
          while (and e (eq (weft.html:dnode-kind e) :element)
                     (not (string-equal (weft.html:dnode-name e) "html")))
          do (let ((p (weft.html:dnode-parent e)) (tag (string-downcase (weft.html:dnode-name e))))
               (when p (let ((nth 0))
                         (loop for c across (weft.html:dnode-children p)
                               when (and (eq (weft.html:dnode-kind c) :element)
                                         (string-equal (string-downcase (weft.html:dnode-name c)) tag))
                                 do (incf nth) (when (eq c e) (return)))
                         (push (format nil "~a:~a" tag nth) parts)))))
    (format nil "~{~a~^/~}" parts)))

(defun weft-geom (test-path)
  "Render TEST-PATH in weft (as loom's service does) and return a hash path -> (x y w h),
or :error / :timeout."
  (handler-case
      (sb-ext:with-timeout *timeout*
        (let* ((html (read-file-string test-path))
               (dir (directory-namestring (truename test-path)))
               (base (format nil "file://~a" (namestring (truename test-path))))
               (pg (loom:load-page html :base base :url base :width *width* :viewport-height *vh*
                                   :loader (file-loader dir) :image-loader (file-image-loader dir)))
               (doc (loom::page-doc pg)) (boxes (make-hash-table)) (out (make-hash-table :test 'equal)))
          (labels ((collect (lb)
                     (when (typep lb 'weft.render::lbox)
                       (let ((n (weft.render::lbox-node lb)))
                         (when (and n (eq (weft.html:dnode-kind n) :element) (not (gethash n boxes)))
                           (setf (gethash n boxes) lb)))
                       (dolist (c (weft.render::lbox-children lb)) (when (typep c 'weft.render::lbox) (collect c))))))
            (collect (loom::page-root pg)))
          (labels ((walk (n)
                     (when (eq (weft.html:dnode-kind n) :element)
                       (let ((b (gethash n boxes)))
                         (when b (setf (gethash (path-of n) out)
                                       (list (round (weft.render::lbox-x b)) (round (weft.render::lbox-y b))
                                             (round (weft.render::lbox-w b)) (round (weft.render::lbox-h b)))))))
                     (loop for c across (weft.html:dnode-children n) do (walk c))))
            (walk doc))
          out))
    (sb-ext:timeout () :timeout)
    (error () :error)))

;;; ---- Chrome geometry (Playwright, JS off) ---------------------------------
(defun split-tab (s)
  (loop with start = 0 for i = (position #\Tab s :start start)
        collect (subseq s start (or i (length s))) do (if i (setf start (1+ i)) (loop-finish))))
(defun pint (s) (or (parse-integer (or s "") :junk-allowed t) 0))
(defun load-chrome-tsv (path)
  (let ((h (make-hash-table :test 'equal)))
    (with-open-file (s path :if-does-not-exist nil)
      (when s (loop for line = (read-line s nil) while line do
                (let ((f (split-tab line)))
                  (when (>= (length f) 6)
                    (setf (gethash (first f) h) (list (pint (nth 1 f)) (pint (nth 2 f)) (pint (nth 3 f)) (pint (nth 4 f)))))))))
    h))

;;; ---- structural diff ------------------------------------------------------
(defun median (nums) (let ((v (sort (copy-list nums) #'<)) (n (length nums))) (if (zerop n) 0 (nth (floor n 2) v))))
(defun round-1 (x) (/ (fround (* x 10)) 10.0))
(defun bx (b) (first b)) (defun by (b) (second b)) (defun bw (b) (third b)) (defun bh (b) (fourth b))

(defun structural-score (chrome weft)
  "Score weft's geometry against Chrome's (lower = closer).  (values score matched
width-bad pos-bad)."
  (let ((common '()))
    (maphash (lambda (p v) (declare (ignore v)) (when (gethash p chrome) (push p common))) weft)
    (if (null common)
        (values 1000.0 0 0 0)
        (let* ((mdx (median (mapcar (lambda (p) (- (bx (gethash p weft)) (bx (gethash p chrome)))) common)))
               (mdy (median (mapcar (lambda (p) (- (by (gethash p weft)) (by (gethash p chrome)))) common)))
               (matched (length common)) (wbad 0) (pbad 0))
          (dolist (p common)
            (let* ((c (gethash p chrome)) (w (gethash p weft))
                   (dw (- (bw w) (bw c))) (rdx (- (bx w) (bx c) mdx)) (rdy (- (by w) (by c) mdy)))
              (cond ((> (abs dw) (max *tol* (* 0.12 (max (bw c) 1)))) (incf wbad))
                    ((or (> (abs rdx) (* 3 *tol*)) (> (abs rdy) (* 8 *tol*))) (incf pbad)))))
          (values (round-1 (+ (* 100.0 (/ wbad matched)) (* 25.0 (/ pbad matched)))) matched wbad pbad)))))

;;; ---- test corpus ----------------------------------------------------------
(defun test-files ()
  "WPT test files under the category: *.html with a rel=match/mismatch link (the
tests, not references), which is a clean layout-bearing corpus."
  (let ((out '()))
    (dolist (p (directory (merge-pathnames (format nil "~a/**/*.html" *category*) *wpt-root*)))
      (let ((name (file-namestring p)))
        (unless (or (search "-ref.html" name) (search "/reference/" (namestring p)))
          (let ((head (with-open-file (s p :if-does-not-exist nil)
                        (and s (let ((b (make-string (min 4000 (file-length s))))) (subseq b 0 (read-sequence b s)))))))
            (when (and head (search "rel=" head) (search "match" head)
                       (or (search "rel=\"match" head) (search "rel=match" head)
                           (search "rel=\"mismatch" head) (search "rel='match" head)))
              (push (namestring p) out))))))
    (let ((tests (sort out #'string<)))
      (if *limit* (subseq tests 0 (min *limit* (length tests))) tests))))

;;; ---- run ------------------------------------------------------------------
(defun run-chrome (url-file workdir n)
  (format t "~&rendering ~a tests in Chromium (JS off, ground truth)…~%" n)
  (ignore-errors
    (sb-ext:run-program "node" (list *chrome-js* url-file workdir (princ-to-string *width*))
                        :search t :output *error-output* :error *error-output* :wait t)))

(defun ensure-dir (path) (sb-ext:run-program "/bin/rm" (list "-rf" path) :wait t)
  (sb-ext:run-program "/bin/mkdir" (list "-p" path) :wait t))

(defun main ()
  (let ((tests (test-files)))
    (unless tests (format t "~&no tests under ~a~%" *category*) (return-from main))
    (ensure-dir *work*)
    (let ((uf (format nil "~a/urls.txt" *work*)))
      (with-open-file (s uf :direction :output :if-exists :supersede :if-does-not-exist :create)
        (dolist (tp tests) (format s "file://~a~%" tp)))
      (format t "~&Chrome-referenced structural audit: ~a tests under ~a @ ~apx~%" (length tests) *category* *width*)
      (run-chrome uf *work* (length tests))
      (let ((npass 0) (ngraded 0) (nfail 0) (nnoref 0) (rows '()))
        (loop for tp in tests for i from 0 do
          (let ((wg (weft-geom tp)) (cp (format nil "~a/~a.chrome.tsv" *work* i)))
            (cond
              ((member wg '(:error :timeout)) (incf nfail) (push (list 1000.0 tp (string-downcase (string wg))) rows))
              ((not (probe-file cp)) (incf nnoref))          ; no Chrome ground truth: can't grade
              (t (multiple-value-bind (score matched wbad pbad) (structural-score (load-chrome-tsv cp) wg)
                   (declare (ignore matched wbad pbad))
                   (incf ngraded)
                   (when (< score *pass-threshold*) (incf npass))
                   (push (list score tp "ok") rows))))
            (sb-ext:gc :full t)))
        (setf rows (sort rows #'> :key #'first))
        (let ((total (+ ngraded nfail)))
          (format t "~&~%=== ~a ===~%" *category*)
          (format t "  structural match vs Chrome: ~a/~a  (~,1f%)   render-fail ~a   no-chrome-ref ~a~%"
                  npass total (if (plusp total) (* 100.0 (/ npass total)) 0.0) nfail nnoref)
          (format t "  worst structural divergences:~%")
          (loop for (score tp status) in rows for k from 1 while (<= k 20) do
            (format t "    ~7,1f  ~7a  ~a~%" score status
                    (let ((r (namestring *wpt-root*))) (if (and (> (length tp) (length r)))
                                                           (subseq tp (length r)) tp)))))))))
(main)
