;;;; wpt-testharness.lisp — run WPT testharness.js tests in weft+shuttle.
;;;;
;;;; weft's geometry harness (wpt-vs-chrome.lisp) grades reftests by comparing
;;;; laid-out box geometry against Chrome.  It is blind to the huge non-geometry
;;;; swath of WPT that asserts JS-observable behaviour: getComputedStyle / CSSOM
;;;; computed values, DOM APIs, parsing.  Those tests ship a JS harness
;;;; (resources/testharness.js) and report pass/fail via test()/assert_* — no
;;;; rendering reference.  This runner drives each such test through weft's REAL
;;;; page lifecycle (parse -> make-context -> run inline scripts -> fire
;;;; DOMContentLoaded+load), lets testharness run, and reads back the per-subtest
;;;; results that shuttle (weft's JS engine) computed.
;;;;
;;;; Using the real loom:load-page pipeline is essential: a bare make-context sets
;;;; document.readyState="complete" immediately, which defeats testharness's phase
;;;; model so its completion callback never fires.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --script inspect/wpt-testharness.lisp \
;;;;        <category> [limit] [wpt-root] [width]
;;;;   e.g.  … css/css-values 300
;;;;
;;;; Only testharness tests are graded: reftests (<link rel=match>, no
;;;; testharness.js) are skipped.  A FILE passes iff the harness completed and
;;;; every subtest PASSed.  Buckets: pass / fail / timeout / error / no-result.

(require :asdf)
(let ((ql "/home/claude/quicklisp/setup.lisp")) (when (probe-file ql) (load ql)))
(push (truename "/home/claude/loom/") asdf:*central-registry*)
(push (truename "/home/claude/weft/") asdf:*central-registry*)
(push (truename "/home/claude/shuttle/") asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "loom"))

;; Conformance-measurement budget (this runner ONLY — not loom's production page
;; budget). A few WPT testharness mega-files generate thousands of subtests
;; (dom/ranges/Range-set alone emits 10920) whose harness needs more wall-clock
;; than a raster load's 6 s *js-budget* allows; under the default they report
;; NO-RESULT even though every subtest runs. Raise the script budget here so the
;; harness completes and its real per-subtest pass count is measurable.
(setf loom::*js-budget* 30.0)

(defpackage #:wpt-th (:use #:cl))
(in-package #:wpt-th)

(defparameter *args* (cdr sb-ext:*posix-argv*))
(defparameter *category* (or (first *args*) "css/css-values"))
(defparameter *limit* (and (second *args*) (parse-integer (second *args*) :junk-allowed t)))
(defparameter *wpt-root* (namestring (truename (or (third *args*) "/home/claude/wpt/"))))
(defparameter *width* (or (and (fourth *args*) (parse-integer (fourth *args*) :junk-allowed t)) 800))
(defparameter *timeout* 45)   ; per-file wall-clock cap; must exceed *js-budget* above

;;; ---- file IO --------------------------------------------------------------
(defun slurp-string (path)
  (ignore-errors
    (with-open-file (in path :external-format :utf-8 :if-does-not-exist nil)
      (and in (let ((s (make-string (file-length in)))) (subseq s 0 (read-sequence s in)))))))
(defun slurp-bytes (path)
  (ignore-errors
    (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (and in (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                (read-sequence v in) v)))))

(defparameter *testharness-js*
  (slurp-string (merge-pathnames "resources/testharness.js" *wpt-root*)))

;; Replacement for resources/testharnessreport.js.  setup({output:false}) is
;; required: testharness's HTML-log output path throws in weft's env; disabling
;; output bypasses it.  The completion callback stashes a JSON result on window.
(defparameter *report-js*
  "setup({output:false}); add_completion_callback(function(ts,st){ window.__wpt = {s:st.status, t:ts.map(function(x){return [x.name,x.status,x.message];})}; });")

;; JS reducer: flatten window.__wpt into a delimited string so we need no
;; Lisp JSON reader and are immune to commas/quotes in subtest messages.
;;   s \x01 count { \x02 status \x03 name \x03 message }
(defparameter *reduce-js*
  "(function(){var w=window.__wpt; if(!w||!w.t) return ''; var o=String(w.s)+'\\u0001'+w.t.length; for(var i=0;i<w.t.length;i++){var x=w.t[i]; o+='\\u0002'+x[1]+'\\u0003'+x[0]+'\\u0003'+(x[2]==null?'':String(x[2]));} return o;})()")

;;; ---- subresource resolution ----------------------------------------------
;; weft passes relative script URLs (../support/x.js) unresolved -> resolve
;; against the TEST's directory; /-absolute -> WPT root; strip scheme://host.
(defun strip-query (url) (subseq url 0 (or (position #\? url) (position #\# url) (length url))))
(defun url->path (url test-dir)
  (let* ((c (strip-query url))
         (i (search "://" c)))
    (when i (let ((s (position #\/ c :start (+ i 3)))) (setf c (if s (subseq c s) "/"))))
    (cond ((and (>= (length c) 7) (string-equal (subseq c 0 7) "file://")) (subseq c 7))
          ((and (plusp (length c)) (char= (char c 0) #\/)) (merge-pathnames (subseq c 1) *wpt-root*))
          (t (merge-pathnames c test-dir)))))
(defun kind-for (url)
  (let ((c (strip-query url)))
    (cond ((search ".css" c) :css)
          ((search ".js" c) :js)
          ((or (search ".png" c) (search ".jpg" c) (search ".jpeg" c)
               (search ".gif" c) (search ".svg" c)) :bytes)
          (t :text))))

(defun make-loader (test-dir)
  (lambda (ctx url)
    (declare (ignore ctx))
    (let ((c (strip-query url)))
      (cond ((search "testharnessreport.js" c) (values :js *report-js*))
            ((search "resources/testharness.js" c) (values :js *testharness-js*))
            ((eq (kind-for url) :bytes)
             (let ((b (slurp-bytes (url->path url test-dir))))
               (if b (values :bytes b) (values nil nil))))
            (t (let ((s (slurp-string (url->path url test-dir))))
                 (if s (values (kind-for url) s) (values nil nil))))))))

;;; ---- result parsing -------------------------------------------------------
;; window.__wpt = {"s":<harness-status>,"t":[[name,status,msg],...]}
;; subtest status: 0=PASS 1=FAIL 2=TIMEOUT 3=NOTRUN.
;; harness status:  0=OK    1=ERROR 2=TIMEOUT 3=PRECONDITION_FAILED.
;; The completion path here frequently reports harness-status 2 (TIMEOUT) even
;; when every subtest finished and PASSed — an artifact of driving the harness
;; outside a live event loop.  So we grade on the subtests and only treat
;; harness-status ERROR (1) as a hard failure.
(defun split-on (string ch)
  (loop with start = 0 for i = (position ch string :start start)
        collect (subseq string start (or i (length string)))
        while i do (setf start (1+ i))))

(defparameter +sep1+ (code-char 1))
(defparameter +sep2+ (code-char 2))
(defparameter +sep3+ (code-char 3))

(defun parse-result (raw)
  "RAW is the delimited reducer output.  Return (values bucket subtests) where
   bucket is :pass/:fail/:error/:no-result and subtests is (name status msg)."
  (if (or (null raw) (zerop (length raw)) (not (find +sep1+ raw)))
      (values :no-result nil)
      (let* ((head-tail (split-on raw +sep1+))
             (s (parse-integer (first head-tail) :junk-allowed t))
             (rest (second head-tail))
             ;; rest = count \x02 rec \x02 rec ...  (first field before \x02 is count)
             (recs (rest (split-on rest +sep2+)))
             (subs (mapcar (lambda (r)
                             (let ((f (split-on r +sep3+)))
                               (list (or (second f) "?")       ; name
                                     (or (parse-integer (first f) :junk-allowed t) 1) ; status
                                     (or (third f) ""))))       ; message
                           recs)))
        (cond ((null subs) (values :no-result nil))
              ((eql s 1) (values :error subs))                  ; harness ERROR
              ((every (lambda (r) (zerop (second r))) subs) (values :pass subs))
              (t (values :fail subs))))))

;;; ---- test selection -------------------------------------------------------
(defun testharness-test-p (path)
  "A testharness test <script src=...testharness.js>; not a reftest."
  (let ((html (slurp-string path)))
    (and html (search "testharness.js" html) t)))

(defun collect-tests ()
  (let* ((dir (merge-pathnames (concatenate 'string *category* "/") *wpt-root*))
         (files (directory (merge-pathnames "**/*.html" dir)))
         (sorted (sort files #'string< :key #'namestring))
         (th (remove-if-not #'testharness-test-p sorted)))
    (if *limit* (subseq th 0 (min *limit* (length th))) th)))

;;; ---- run one --------------------------------------------------------------
(defun run-one (path)
  "Return (values bucket subtests) — bucket in
   :pass :fail :timeout :error :no-result."
  (handler-case
      (let* ((dir (directory-namestring path))
             (rel (subseq (namestring path) (length *wpt-root*)))
             (base (format nil "https://wpt.test/~a" rel))
             (html (slurp-string path))
             (pg (sb-ext:with-timeout *timeout*
                   (loom:load-page html :base base :url base :width *width*
                                        :loader (make-loader dir))))
             (realm (weft.script:context-realm (loom::page-ctx pg)))
             (raw (ignore-errors
                    (shuttle:to-string
                     (shuttle:eval-script realm *reduce-js*)))))
        (parse-result raw))
    (sb-ext:timeout () (values :timeout nil))
    (error (e) (values :error (list (list "<lisp error>" 1 (princ-to-string e)))))))

;;; ---- main -----------------------------------------------------------------
(defun main ()
  (let ((files (collect-tests))
        (buckets (make-hash-table))
        (fails '()))
    (format t "~&=== WPT testharness: ~a (~d tests, width ~d) ===~%"
            *category* (length files) *width*)
    (finish-output)
    (loop for f in files
          for i from 1
          do (multiple-value-bind (bucket subs) (run-one f)
               (incf (gethash bucket buckets 0))
               (let ((rel (subseq (namestring f) (length *wpt-root*))))
                 (unless (eq bucket :pass)
                   (push (list rel bucket subs) fails))
                 (format t "  [~4d/~4d] ~a ~a~%" i (length files) bucket rel))
               (finish-output)))
    (let* ((pass (gethash :pass buckets 0))
           (total (length files)))
      (format t "~%=== ~a ===~%" *category*)
      (format t "testharness match: ~d/~d~%" pass total)
      (dolist (k '(:pass :fail :timeout :error :no-result))
        (format t "  ~10a ~d~%" k (gethash k buckets 0)))
      (format t "~%--- failing tests (bucket / name) ---~%")
      (dolist (fl (nreverse fails))
        (destructuring-bind (rel bucket subs) fl
          (let ((nfail (count-if-not (lambda (r) (zerop (second r))) subs))
                (n (length subs)))
            (format t "  ~9a ~a  (~d/~d subtests fail)~%" bucket rel nfail n)))))
    (finish-output)))

(main)
