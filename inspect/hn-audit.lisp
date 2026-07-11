;;;; hn-audit.lisp — render-quality audit of the Hacker News front page, in Common
;;;; Lisp.  For every external story link it lays the page out in weft (JS on, as
;;;; loom's service does) and diffs its element geometry against Chromium (the
;;;; reference, JS off) keyed by structural DOM path; the divergences that survive a
;;;; systematic offset give a badness score, and the worst offenders are written to
;;;; loom-errors.db (kind='render-audit').  Chromium is driven by hn-audit-chrome.js
;;;; (Playwright) — the one external edge; everything else is Lisp.
;;;;
;;;; No LLM, no interactivity — safe to run from cron:
;;;;   sbcl --dynamic-space-size 4096 --script inspect/hn-audit.lisp
;;;; Env: HN_WIDTH (1024), HN_DB (../loom-errors.db), HN_LOG_TOP (15),
;;;;      HN_MIN_SCORE (8), HN_TIMEOUT (60), HN_WORK (/tmp/hn-audit).
(require :asdf)
(defparameter *loom-dir* (truename "/home/claude/loom/"))
(push *loom-dir* asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "loom"))

(defpackage #:hn-audit (:use #:cl))
(in-package #:hn-audit)

(defun envi (name default)
  (let ((v (sb-ext:posix-getenv name))) (or (and v (parse-integer v :junk-allowed t)) default)))
(defun envs (name default)
  (let ((v (sb-ext:posix-getenv name))) (if (and v (plusp (length v))) v default)))

(defparameter *width* (envi "HN_WIDTH" 1024))
(defparameter *timeout* (envi "HN_TIMEOUT" 60))
(defparameter *log-top* (envi "HN_LOG_TOP" 15))
(defparameter *min-score* (envi "HN_MIN_SCORE" 8))
(defparameter *work* (envs "HN_WORK" "/tmp/hn-audit"))
(defparameter *db-path* (envs "HN_DB" (namestring (merge-pathnames "loom-errors.db" cl-user::*loom-dir*))))
(defparameter *chrome-js* (namestring (merge-pathnames "inspect/hn-audit-chrome.js" cl-user::*loom-dir*)))
(defparameter *fail* 1000.0)

;;; ---- error db (libsqlite3 over sb-alien; the binding loom's service uses) ----
(ignore-errors (sb-alien:load-shared-object "libsqlite3.so.0"))
(sb-alien:define-alien-routine ("sqlite3_open" %sqlite-open) sb-alien:int
  (filename sb-alien:c-string) (ppdb (* sb-alien:system-area-pointer)))
(sb-alien:define-alien-routine ("sqlite3_exec" %sqlite-exec) sb-alien:int
  (db sb-alien:system-area-pointer) (sql sb-alien:c-string)
  (cb sb-alien:system-area-pointer) (arg sb-alien:system-area-pointer)
  (errmsg sb-alien:system-area-pointer))
(defvar *db* nil)
(defun %nsap () (sb-sys:int-sap 0))
(defun db-exec (sql) (when *db* (ignore-errors (%sqlite-exec *db* sql (%nsap) (%nsap) (%nsap)))))
(defun db-open (path)
  (handler-case
      (sb-alien:with-alien ((h sb-alien:system-area-pointer))
        (when (zerop (%sqlite-open path (sb-alien:addr h)))
          (setf *db* h)
          (db-exec "CREATE TABLE IF NOT EXISTS errors (id INTEGER PRIMARY KEY,
                    ts TEXT DEFAULT CURRENT_TIMESTAMP, kind TEXT, url TEXT, detail TEXT)")
          (db-exec "PRAGMA busy_timeout=30000")
          t))
    (error () nil)))
(defun sqlq (s)
  (with-output-to-string (o)
    (write-char #\' o)
    (loop for c across (or s "") do (when (char= c #\') (write-char #\' o)) (write-char c o))
    (write-char #\' o)))
(defun db-log (url detail)
  (db-exec (format nil "INSERT INTO errors (kind,url,detail) VALUES ('render-audit',~a,~a)"
                   (sqlq url) (sqlq detail))))

;;; ---- string helpers -------------------------------------------------------
(defun split-tab (s)
  (loop with start = 0 with out = '()
        for i = (position #\Tab s :start start)
        do (push (subseq s start (or i (length s))) out)
           (if i (setf start (1+ i)) (return (nreverse out)))))
(defun pint (s) (or (parse-integer (or s "") :junk-allowed t) 0))
(defun jstr (s)
  "JSON string literal of S."
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across (or s "") do (case c ((#\" #\\) (write-char #\\ o) (write-char c o))
                                             ((#\Newline #\Return #\Tab) (write-char #\Space o))
                                             (t (write-char c o))))
    (write-char #\" o)))
(defun short-path (p)
  (let ((segs (loop with start = 0 for i = (position #\/ p :start start)
                    collect (subseq p start (or i (length p)))
                    do (if i (setf start (1+ i)) (loop-finish)))))
    (format nil "~{~a~^/~}" (last segs 2))))
(defun median (nums)
  (let* ((v (sort (copy-list nums) #'<)) (n (length v)))
    (if (zerop n) 0 (nth (floor n 2) v))))

;;; ---- HN front-page links --------------------------------------------------
(defun hn-links ()
  "External story links on the HN front page, in order, deduped."
  (let ((html (weft.fetch:fetch-text "https://news.ycombinator.com/"))
        (marker "class=\"titleline\"><a href=\"") (out '()) (i 0))
    (loop for p = (search marker html :start2 i) while p do
      (let* ((s (+ p (length marker))) (e (or (position #\" html :start s) (length html)))
             (u (subseq html s e)))
        (setf i e)
        (when (and (>= (length u) 4) (string= (subseq u 0 4) "http")
                   (not (search "news.ycombinator.com" u))
                   (not (member u out :test #'string=)))
          (push u out))))
    (nreverse out)))

;;; ---- structural DOM path (matches hn-audit-chrome.js's pathOf) -------------
(defun path-of (n)
  (let ((parts '()))
    (loop for e = n then (weft.html:dnode-parent e)
          while (and e (eq (weft.html:dnode-kind e) :element)
                     (not (string-equal (weft.html:dnode-name e) "html")))
          do (let ((p (weft.html:dnode-parent e))
                   (tag (string-downcase (weft.html:dnode-name e))))
               (when p
                 (let ((nth 0))
                   (loop for c across (weft.html:dnode-children p)
                         when (and (eq (weft.html:dnode-kind c) :element)
                                   (string-equal (string-downcase (weft.html:dnode-name c)) tag))
                           do (incf nth) (when (eq c e) (return)))
                   (push (format nil "~a:~a" tag nth) parts)))))
    (format nil "~{~a~^/~}" parts)))

;;; ---- geometry dumps -------------------------------------------------------
(defun weft-geom (url width)
  "Lay URL out in weft (JS on) and return a hash path -> (:x :y :w :h), or the
keyword :error / :timeout on a failed render."
  (handler-case
      (sb-ext:with-timeout *timeout*
        (let* ((pg (loom:load-url url :width width :viewport-height 100000))
               (doc (loom:page-doc pg))
               (boxes (make-hash-table)) (out (make-hash-table :test 'equal)))
          (labels ((collect (lb)
                     (when (typep lb 'weft.render::lbox)
                       (let ((n (weft.render::lbox-node lb)))
                         (when (and n (eq (weft.html:dnode-kind n) :element) (not (gethash n boxes)))
                           (setf (gethash n boxes) lb)))
                       (dolist (c (weft.render::lbox-children lb))
                         (when (typep c 'weft.render::lbox) (collect c))))))
            (collect (loom:page-root pg)))
          (labels ((walk (n)
                     (when (eq (weft.html:dnode-kind n) :element)
                       (let ((b (gethash n boxes)))
                         (when b
                           (setf (gethash (path-of n) out)
                                 (list :x (round (weft.render::lbox-x b)) :y (round (weft.render::lbox-y b))
                                       :w (round (weft.render::lbox-w b)) :h (round (weft.render::lbox-h b)))))))
                     (loop for c across (weft.html:dnode-children n) do (walk c))))
            (walk doc))
          out))
    (sb-ext:timeout () :timeout)
    (error () :error)))

(defun load-chrome-tsv (path)
  "Parse a Chromium geometry TSV into a hash path -> (:x :y :w :h :tx)."
  (let ((h (make-hash-table :test 'equal)))
    (with-open-file (s path :if-does-not-exist nil)
      (when s
        (loop for line = (read-line s nil) while line do
          (let ((f (split-tab line)))
            (when (>= (length f) 6)
              (setf (gethash (first f) h)
                    (list :x (pint (nth 1 f)) :y (pint (nth 2 f))
                          :w (pint (nth 3 f)) :h (pint (nth 4 f)) :tx (nth 5 f))))))))
    h))

;;; ---- geometry diff + score ------------------------------------------------
(defun round-1 (x) (/ (fround (* x 10)) 10.0))

(defun diff-score (chrome weft)
  "Score weft's geometry against Chrome's.  Returns (values score info-plist)."
  (let ((common '()))
    (maphash (lambda (p b) (declare (ignore b)) (when (gethash p chrome) (push p common))) weft)
    (if (null common)
        (values *fail* (list :note "no DOM elements matched Chrome" :matched 0))
        (let* ((mdx (median (mapcar (lambda (p) (- (getf (gethash p weft) :x) (getf (gethash p chrome) :x))) common)))
               (mdy (median (mapcar (lambda (p) (- (getf (gethash p weft) :y) (getf (gethash p chrome) :y))) common)))
               (matched (length common)) (width-bad 0) (pos-bad 0) (worst '()))
          (dolist (p common)
            (let* ((c (gethash p chrome)) (w (gethash p weft))
                   (dw (- (getf w :w) (getf c :w)))
                   (rdx (- (getf w :x) (getf c :x) mdx))
                   (rdy (- (getf w :y) (getf c :y) mdy)))
              (cond ((> (abs dw) (max 8 (* 0.12 (max (getf c :w) 1))))
                     (incf width-bad) (push (list (abs dw) (short-path p) dw) worst))
                    ((or (> (abs rdx) 24) (> (abs rdy) 64)) (incf pos-bad)))))
          (setf worst (subseq (sort worst #'> :key #'first) 0 (min 5 (length worst))))
          ;; width divergences (collapsed columns, boxes that fail to fill a parent)
          ;; are the strong structural signal; position drift compounds down long
          ;; pages so it is weighted lightly.
          (values (round-1 (+ (* 100.0 (/ width-bad matched)) (* 25.0 (/ pos-bad matched))))
                  (list :matched matched :width-bad width-bad :pos-bad pos-bad
                        :sys-dx mdx :sys-dy mdy :worst worst))))))

(defun detail-json (status score rank info)
  (with-output-to-string (o)
    (format o "{\"score\":~a,\"rank\":~a,\"status\":\"~a\",\"width\":~a" score rank status *width*)
    (when info
      (when (getf info :matched)
        (format o ",\"matched\":~a,\"width_bad\":~a,\"pos_bad\":~a,\"sys_dy\":~a"
                (getf info :matched) (getf info :width-bad 0) (getf info :pos-bad 0) (getf info :sys-dy 0)))
      (let ((worst (getf info :worst)))
        (when worst
          (format o ",\"worst\":[~{~a~^,~}]"
                  (mapcar (lambda (wd) (destructuring-bind (adw p dw) wd (declare (ignore adw))
                                         (format nil "{\"path\":~a,\"dw\":~a}" (jstr p) dw)))
                          worst))))
      (when (getf info :note) (format o ",\"note\":~a" (jstr (getf info :note)))))
    (format o "}")))

;;; ---- Chromium reference (Playwright, the one external edge) ----------------
(defun run-chrome (links-file workdir width n)
  (format t "~&rendering ~a pages in Chromium (reference)…~%" n)
  (handler-case
      (sb-ext:run-program "node" (list *chrome-js* links-file workdir (princ-to-string width))
                          :search t :output *error-output* :error *error-output* :wait t)
    (error (e) (format t "~&chrome reference failed to launch: ~a~%" e))))

;;; ---- run ------------------------------------------------------------------
(defun ensure-dir (path)
  (sb-ext:run-program "/bin/rm" (list "-rf" path) :wait t)
  (sb-ext:run-program "/bin/mkdir" (list "-p" path) :wait t))

(defun main ()
  (let ((links (handler-case (hn-links) (error (e) (format t "~&could not fetch HN: ~a~%" e) nil))))
    (unless links (format t "~&no links; aborting~%") (return-from main))
    (ensure-dir *work*)
    (let ((lf (format nil "~a/links.txt" *work*)))
      (with-open-file (s lf :direction :output :if-exists :supersede :if-does-not-exist :create)
        (dolist (u links) (write-line u s)))
      (format t "~&auditing ~a HN front-page links @ ~apx~%" (length links) *width*)
      (run-chrome lf *work* *width* (length links))
      (let ((rows '()))
        (loop for url in links for i from 0 do
          (let* ((wg (weft-geom url *width*))
                 (chrome-path (format nil "~a/~a.chrome.tsv" *work* i)))
            (multiple-value-bind (score status info)
                (cond
                  ((eq wg :timeout) (values *fail* "timeout" (list :note "weft render timeout")))
                  ((eq wg :error)   (values *fail* "error"   (list :note "weft render error")))
                  ((not (probe-file chrome-path)) (values -1.0 "no-ref" (list :note "chrome reference unavailable")))
                  (t (multiple-value-bind (sc inf) (diff-score (load-chrome-tsv chrome-path) wg)
                       (values sc "ok" inf))))
              (push (list score url status info) rows)
              (format t "  [~2d/~2d] ~7,1f  ~7a  ~a~%"
                      (1+ i) (length links) score status (subseq url 0 (min 60 (length url)))))
            (sb-ext:gc :full t)))
        (setf rows (sort rows #'> :key #'first))
        (db-open *db-path*)
        (let ((logged 0))
          (loop for (score url status info) in rows for rank from 1
                while (<= rank *log-top*)
                when (>= score *min-score*)
                  do (db-log url (detail-json status score rank info)) (incf logged))
          (format t "~&~%worst offenders (top ~a):~%" (min *log-top* (length rows)))
          (loop for (score url status info) in rows for rank from 1 while (<= rank *log-top*) do
            (let ((w (getf info :worst)))
              (format t "  ~2d. ~7,1f  ~7a  ~a~@[  [~a]~]~%" rank score status
                      (subseq url 0 (min 60 (length url)))
                      (and w (format nil "~{~a~^, ~}"
                                     (mapcar (lambda (wd) (format nil "~a(~@d)" (second wd) (third wd)))
                                             (subseq w 0 (min 2 (length w)))))))))
          (format t "~&logged ~a offenders (score >= ~a) to ~a~%" logged *min-score* *db-path*))))))

(main)
