;;;; src/main.lisp — start-page and file-URL helpers.
;;;;
;;;; loom drives weft through a display backend (glass, over VNC — see
;;;; src/glass-shell.lisp).  These are the small backend-agnostic helpers a
;;;; driver needs to open its first page: the bundled home page and the
;;;; file:// -> path resolution a followed link uses.
(in-package #:loom)

(defun default-home ()
  "The bundled offline start page shipped in loom's assets/."
  (let ((p (merge-pathnames "assets/home.html"
                            (asdf:system-source-directory "loom"))))
    (and (probe-file p) p)))

(defun url->path (file-url)
  "The filesystem path of a file:// URL (or the string unchanged)."
  (if (url-prefix-p "file://" file-url) (subseq file-url 7) file-url))
