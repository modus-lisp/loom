;;;; hn-audit-weft.lisp — batch element-geometry dumper for the HN render audit.
;;;; Loads loom once and, for each URL in the links file, renders the page the way
;;;; the service does (JS on) and writes its block-element box geometry as TSV to
;;;; <outdir>/<index>.weft.tsv (path<TAB>x<TAB>y<TAB>w<TAB>h<TAB>text).  A render
;;;; that errors writes <index>.weft.err instead, so one bad page never aborts the
;;;; batch.  Args: <links-file> <outdir> <width>.  No LLM, cron-safe.
(require :asdf)
(push (truename "/home/claude/loom/") asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "loom"))

(defun path-of (n)
  "Structural tag:nth-of-type chain from the root to element N (HTML excluded) —
identical to cmp-chrome.js's pathOf, so the two dumps line up per element."
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

(defun clean (s)
  (with-output-to-string (o)
    (loop for c across (or s "")
          do (write-char (if (member c '(#\Tab #\Newline #\Return)) #\Space c) o))))

(defun dump-page (url w stream)
  "Render URL at width W the way loom's service does (JS on) and write one TSV row
per laid-out block element to STREAM."
  (let* ((pg (loom:load-url url :width w :viewport-height 100000))
         (doc (loom::page-doc pg))
         (boxes (make-hash-table)))
    (labels ((collect (lb)
               (when (typep lb 'weft.render::lbox)
                 (let ((n (weft.render::lbox-node lb)))
                   (when (and n (eq (weft.html:dnode-kind n) :element) (not (gethash n boxes)))
                     (setf (gethash n boxes) lb)))
                 (dolist (c (weft.render::lbox-children lb)) (collect c)))))
      (collect (loom::page-root pg)))
    (labels ((walk (n)
               (when (eq (weft.html:dnode-kind n) :element)
                 (let ((b (gethash n boxes)))
                   (when b
                     (let* ((full (string-trim '(#\Space #\Newline #\Tab) (or (weft.dom:text-content n) "")))
                            (tx (clean (subseq full 0 (min 25 (length full))))))
                       (format stream "~a~c~a~c~a~c~a~c~a~c~a~%"
                               (path-of n) #\Tab
                               (round (weft.render::lbox-x b)) #\Tab (round (weft.render::lbox-y b)) #\Tab
                               (round (weft.render::lbox-w b)) #\Tab (round (weft.render::lbox-h b)) #\Tab tx)))))
               (loop for c across (weft.html:dnode-children n) do (walk c))))
      (walk doc))
    ;; also emit the total content height as a trailing comment for the scorer
    (format stream "#~c~a~c~a~%" #\Tab "content-height" #\Tab (round (loom::page-content-height pg)))))

(defun read-lines (path)
  (with-open-file (s path)
    (loop for line = (read-line s nil) while line
          for u = (string-trim '(#\Space #\Tab #\Return) line)
          when (plusp (length u)) collect u)))

(let* ((args (cdr sb-ext:*posix-argv*))
       (links-file (first args))
       (outdir (second args))
       (w (parse-integer (or (third args) "1024")))
       (urls (read-lines links-file)))
  (loop for url in urls for i from 0 do
    (let ((tsv (format nil "~a/~a.weft.tsv" outdir i))
          (err (format nil "~a/~a.weft.err" outdir i)))
      (handler-case
          (sb-ext:with-timeout 60
            (with-open-file (s tsv :direction :output :if-exists :supersede :if-does-not-exist :create)
              (dump-page url w s))
            (format *error-output* "~&[weft] ~a ok~%" i))
        (error (e)
          (ignore-errors (delete-file tsv))
          (with-open-file (s err :direction :output :if-exists :supersede :if-does-not-exist :create)
            (format s "~a~%" e))
          (format *error-output* "~&[weft] ~a FAIL ~a~%" i e))
        (sb-ext:timeout ()
          (ignore-errors (delete-file tsv))
          (with-open-file (s err :direction :output :if-exists :supersede :if-does-not-exist :create)
            (format s "render timeout (>60s)~%"))
          (format *error-output* "~&[weft] ~a TIMEOUT~%" i)))
      ;; drop references so each page's canvas can be collected before the next
      (sb-ext:gc :full t))))
