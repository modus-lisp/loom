;;;; cmp-weft.lisp — dump every laid-out element's box geometry from weft as TSV:
;;;; path<TAB>x<TAB>y<TAB>w<TAB>h<TAB>text.  The path is the same structural
;;;; tag:nth-of-type chain cmp-chrome.js emits, so the two dumps line up per element.
;;;; Reads CMP_URL and CMP_W from the environment.
(require :asdf)
(push (truename "/home/claude/loom/") asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "loom"))

(defun path-of (n)
  "Structural tag:nth-of-type chain from the root to element N (HTML excluded)."
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

(defun run (url w)
  (let* ((html (weft.fetch:fetch-text url))
         (pg (loom:load-page html :base url :url url :width w :viewport-height 100000
                             :loader (loom::make-http-loader url)))
         (doc (loom::page-doc pg))
         (boxes (make-hash-table)))
    ;; first lbox per node = its principal (outer) box
    (labels ((collect (lb)
               (when (typep lb 'weft.render::lbox)
                 (let ((n (weft.render::lbox-node lb)))
                   (when (and n (eq (weft.html:dnode-kind n) :element) (not (gethash n boxes)))
                     (setf (gethash n boxes) lb)))
                 (dolist (c (weft.render::lbox-children lb)) (collect c)))))
      (collect (loom::page-root pg)))
    ;; walk the document (the root is a :document node, not an element), emitting a
    ;; row for every element that got a box.
    (labels ((walk (n)
               (when (eq (weft.html:dnode-kind n) :element)
                 (let ((b (gethash n boxes)))
                   (when b
                     (let* ((full (string-trim '(#\Space #\Newline #\Tab) (or (weft.dom:text-content n) "")))
                            (tx (clean (subseq full 0 (min 25 (length full))))))
                       (format t "~a~c~a~c~a~c~a~c~a~c~a~%"
                               (path-of n) #\Tab
                               (round (weft.render::lbox-x b)) #\Tab (round (weft.render::lbox-y b)) #\Tab
                               (round (weft.render::lbox-w b)) #\Tab (round (weft.render::lbox-h b)) #\Tab tx)))))
               (loop for c across (weft.html:dnode-children n) do (walk c))))
      (walk doc))
    (finish-output)))

(run (uiop:getenv "CMP_URL") (parse-integer (uiop:getenv "CMP_W")))
