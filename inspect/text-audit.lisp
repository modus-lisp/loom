;;;; text-audit.lisp — dump weft's text-layout geometry for the text-layout audit.
;;;; For each local test HTML file (args after --), lay it out in weft at a fixed
;;;; width and emit, keyed by element id: the element box (x,y,w,h) AND the per-LINE
;;;; geometry of its inline content (x,y,ink-width,h + the line's text) drawn from
;;;; the :line lboxes and their frags — the per-line signal that reveals
;;;; line-breaking / white-space / justify / rtl behaviour.  Output: one
;;;; <outdir>/<basename>.weft.json per file.  The GEOMETRY DIFF against Chromium's
;;;; dump (text-audit-chrome.js) is done by text-audit-diff.py with font tolerance.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --script inspect/text-audit.lisp <outdir> <width> <file>...
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
(require :asdf)
(let ((home (truename "/home/claude/")))
  (dolist (d '("loom/" "weft/" "shuttle/" "pigment/" "cram/" "scribe/" "gesso/"
               "stencil/" "webp-pure/" "seal/" "glass/" "brotli-pure/" "zstd-pure/"))
    (let ((p (merge-pathnames d home))) (when (probe-file p) (push (truename p) asdf:*central-registry*)))))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "loom"))

(defpackage #:text-audit (:use #:cl))
(in-package #:text-audit)

(defun eid (n)
  (and (eq (weft.html:dnode-kind n) :element)
       (cdr (assoc "id" (weft.html:dnode-attrs n) :test #'string-equal))))

(defun jstr (s)
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across (or s "")
          do (case c ((#\" #\\) (write-char #\\ o) (write-char c o))
                     ((#\Newline #\Return #\Tab) (write-char #\Space o))
                     (t (write-char c o))))
    (write-char #\" o)))

;;; Collect the principal (first/outer) lbox per DOM node.
(defun node-box-table (root)
  (let ((boxes (make-hash-table)))
    (labels ((collect (lb)
               (when (typep lb 'weft.render::lbox)
                 (let ((n (weft.render::lbox-node lb)))
                   (when (and n (eq (weft.html:dnode-kind n) :element) (not (gethash n boxes)))
                     (setf (gethash n boxes) lb)))
                 (dolist (c (weft.render::lbox-children lb))
                   (when (typep c 'weft.render::lbox) (collect c))))))
      (collect root))
    boxes))

;;; Per-line geometry for an element's principal box: walk the box subtree and
;;; gather every :line lbox that belongs to THIS element's flow — i.e. lines
;;; reached without descending through a nested element's own principal box
;;; (so a <p>'s lines are counted, but a nested block's lines belong to it).
;;; For each line: x/y, ink width + start from its frags, and the line text.
(defun frag-p (x) (weft.render::frag-p x))

(defun element-lines (lb node)
  "List of (:x :y :w :h :text) for the inline lines directly composing NODE."
  (let ((out '()))
    (labels ((line-info (line)
               (let ((frags (remove-if-not #'frag-p (weft.render::lbox-children line)))
                     (atomics (remove-if #'frag-p (weft.render::lbox-children line))))
                 (let* ((xs (append (mapcar #'weft.render::frag-x frags)
                                    (mapcar #'weft.render::lbox-x atomics)))
                        (rs (append (mapcar (lambda (f) (+ (weft.render::frag-x f) (weft.render::frag-w f))) frags)
                                    (mapcar (lambda (a) (+ (weft.render::lbox-x a) (weft.render::lbox-w a))) atomics)))
                        (lx (if xs (reduce #'min xs) (weft.render::lbox-x line)))
                        (rx (if rs (reduce #'max rs) (weft.render::lbox-x line)))
                        (txt (with-output-to-string (o)
                               (dolist (f (sort (copy-list frags) #'< :key #'weft.render::frag-x))
                                 (write-string (or (weft.render::frag-text f) "") o)))))
                   (list :x (round lx) :y (round (weft.render::lbox-y line))
                         :w (round (- rx lx)) :h (round (weft.render::lbox-h line))
                         :text txt))))
             (walk (box owner)
               (dolist (c (weft.render::lbox-children box))
                 (when (typep c 'weft.render::lbox)
                   (cond
                     ((eq (weft.render::lbox-kind c) :line)
                      (when owner (push (line-info c) out)))
                     (t
                      ;; a nested block whose node is a different element starts a new
                      ;; owner (its lines are its own); anonymous/same-node blocks keep owner.
                      (let* ((cn (weft.render::lbox-node c))
                             (still (or (null cn) (eq cn node))))
                        (walk c (and owner still)))))))))
      (walk lb t))
    (nreverse out)))

(defun dump-file (file outdir width)
  (let* ((base (pathname-name (pathname file)))
         (outpath (format nil "~a/~a.weft.json" outdir base)))
    (handler-case
        (let* ((pg (loom:load-file file :width width :viewport-height 100000))
               (doc (loom::page-doc pg))
               (root (loom:page-root pg))
               (boxes (node-box-table root))
               (recs '()))
          (labels ((walk (n)
                     (when (eq (weft.html:dnode-kind n) :element)
                       (let ((id (eid n)) (b (gethash n boxes)))
                         (when (and id b)
                           (push (cons id b) recs))))
                     (loop for c across (weft.html:dnode-children n) do (walk c))))
            (walk doc))
          (setf recs (nreverse recs))
          (with-open-file (s outpath :direction :output :if-exists :supersede :if-does-not-exist :create)
            (format s "{")
            (loop for (id . b) in recs for first = t then nil do
              (unless first (format s ","))
              (let ((lines (element-lines b (weft.render::lbox-node b))))
                (format s "~a:{\"x\":~a,\"y\":~a,\"w\":~a,\"h\":~a"
                        (jstr id) (round (weft.render::lbox-x b)) (round (weft.render::lbox-y b))
                        (round (weft.render::lbox-w b)) (round (weft.render::lbox-h b)))
                (if lines
                    (progn
                      (format s ",\"lines\":[")
                      (loop for ln in lines for lf = t then nil do
                        (unless lf (format s ","))
                        (format s "{\"x\":~a,\"y\":~a,\"w\":~a,\"h\":~a,\"text\":~a}"
                                (getf ln :x) (getf ln :y) (getf ln :w) (getf ln :h)
                                (jstr (getf ln :text))))
                      (format s "]"))
                    (format s ",\"lines\":null"))
                (format s "}")))
            (format s "}~%"))
          (format *error-output* "[weft] ~a ok~%" base))
      (error (e)
        (with-open-file (s (format nil "~a/~a.weft.err" outdir base)
                           :direction :output :if-exists :supersede :if-does-not-exist :create)
          (format s "~a~%" e))
        (format *error-output* "[weft] ~a FAIL ~a~%" base e)))))

(let* ((args sb-ext:*posix-argv*)
       (rest (cdr args))
       (outdir (first rest))
       (width (or (parse-integer (or (second rest) "400") :junk-allowed t) 400))
       (files (cddr rest)))
  (dolist (f files) (dump-file f outdir width))
  (sb-ext:exit :code 0))
